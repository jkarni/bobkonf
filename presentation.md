%title: verdict - Type, Validate, Refine
%author: Julian K. Arni
%date: 19 Feb 2016

-> verdict <-
==============


> I always assumed gradual types were to help those poor schmucks using untyped
> languages to migrate to typed languages. I now realize that I am one of the
> poor schmucks.
>   - Philip Wadler, _A Completement to Blame_

------------------------------------------------------------------------------

-> # The barbarians at the gate


   +---------+          +-----------------+ <----+ +-----------+
   |   CLI   | +------> |                 |        |   DB      |
   +---------+          |   Your code     | +----> +-----------+
                        |                 |
    +----------+ +----> |                 |
    | Browser  |        +-----------------+
    +----------+                      ^         +---------+
                                      |         | Other   |
                                      +-------+ | service |
                                                +---------+

^
All of these may be trying to give you a *User*.
^

    data User = User {
        name :: String,
        age :: Int
    }

------------------------------------------------------------------------------

-> # The barbarians at the gate

    data User = User {
        name :: String,
        age :: Int                        -- (1)
    }
^
      deriving (Read, FromJSON, FromRow)  -- (2)

^

- Deserialization (2) involves validation
^
- Convenient (generic)
^
- But deserialization /= validation
^
- Inconvenient (manual, repetitive)

------------------------------------------------------------------------------

-> # The barbarians at the gate

   +---------+          +-----------------+ <----+ +-----------+
   |   CLI   | +------> |                 |        |   DB      |
   +---------+          |   Your code     | +----> +-----------+
                        |                 |
    +----------+ +----> |                 |
    | Browser  |        +-----------------+
    +----------+                      ^         +---------+
                                      |         | Other   |
                                      +-------+ | service |
                                                +---------+
^

- Let's focus on the browser

------------------------------------------------------------------------------

-> # The barbarians at the gate


+---Browswer------------------+
|                             |
|                             |
|                             |
|   +-User form-------+       |
|   |                 |       |
|   |                 |       |
|   |  Name: ----     |       |
|   |                 |       |
|   |  Age:  ----     |       |
|   |                 |       |
|   +-----------------+       |
|                             |
+-----------------------------+

^
- Validate first in the browser
^
- Then send it as form-url-encoded
^
- Then validate again in your code
^
- If the browser validation is more lax, bad UX
^
- If server validation is more lax, either too restricting or invalid.

------------------------------------------------------------------------------

-> # The barbarians at the gate



+---Browser-------------------+
|                             |
|                             |                              +-DB------------+
|                             |                              |               |
|   +-User form-------+       |                              | User          |
|   |                 |       |                              |---------------|
|   |                 |       | +----- ... --------------->  ||name|age|     |
|   |  Name: ----     |       |                              ||... |...|     |
|   |                 |       |                              ||... |...|     |
|   |  Age:  ----     |       |                              |               |
|   |                 |       |                              +---------------+
|   +-----------------+       |
|                             |
+-----------------------------+

^
- If DB schema is too lax, bad data may enter
^
- If DB schema is too strict, you won't be able to store good data

------------------------------------------------------------------------------

-> # The barbarians at the gate




-> *Do it once!* <-


------------------------------------------------------------------------------

-> # Do it once

^

    data User = User {
        name :: String,
        age :: Int
    }

------------------------------------------------------------------------------

-> # Do it once


    type Age = Validated (Minimum 0 :&& Maximum 200) Int   -- (1)
    data User = User {
        name :: String,
        age :: Age
    }
^
      deriving (Read, FromJSON, FromRow)                   -- (2)

^

* Validation becomes part of the type (1)
^
    - Introspectable
^
    - _Like_ types
^
    - Keep the convenience and safety of generics (2)

------------------------------------------------------------------------------

-> # Do it once (Examples)

-> ## Read

[TODO]

------------------------------------------------------------------------------

-> # Do it once (Examples)

-> ## JSON

    test :: ByteString
    test = [str|
       { "name": "Julian K. Arni",
         "age":  -20
       }
    |]
    eitherDecode test :: Either String User

[TODO]



------------------------------------------------------------------------------

-> # Do it once (Examples)

-> ## JSON Schema

[TODO]

------------------------------------------------------------------------------

-> # Do it once (Examples)

-> ## Databases

^
The same language maps well to relational algebras

^
     result :: [Validation (Maximum 100) Price] <- tbl
       == SELECT price FROM tbl WHERE price <= 100

^
     result :: [Validation (Maximum 100) Price] <- tbl
       == SELECT price FROM tbl WHERE price <= 100

^

More ambitiously:

^

    data Person c1 c2 c3 = Person
      { firstName :: Validated c1 String
      , lastName  :: Validated c2 String
      , age       :: Validated c3 String
      }

    data Country c1 c2 = Country
      { name      :: Validated c1 String
      , continent :: Validated c2 String
      }

^
    peopleWithAfricanCountryLastName
       :: [(Person Any (Equals p) Any, Country (Equals p) (Equals "Africa")] <- tbl

------------------------------------------------------------------------------

-> # Do it once (Examples)

-> ## QuickCheck

^
- Writing *Arbitrary* instances for opaque types is annoying.
^
  + Can't use existing Template Haskell or Generic solutions.
^
- But with *verdict* the constraints are reified.
^
  + Can deduce more complex constraints from simpler ones.
^
  + Can use validation instance for new constraints
^
+ But efficiently?
^
  - *Validated (x :|| y) val* is easy
^
    - E.g. alternatingly generate a *Validated x val* and a *Validated y val*.
^
  - What about *Validated (x :&& y) val*?
^
    - Generating a *Validated x val* and validating it as *y* may not be great.
^
    - E.g. *Validated (Even :&& Equals 500) Int*
^
    - Alternating may also not be great.
^
    - No obvious way to go from an efficient *Validated x val* to an efficient
      *Validated (Not x) val*.
^
    - (Though for certain predicates a technique by Martin Escardo may help.)

------------------------------------------------------------------------------

-> # As a type

^
.                          This part
.                             vvvv
   +---------+          +-----------------+ <----+ +-----------+
   |   CLI   | +------> |                 |        |   DB      |
   +---------+          |   Your code     | +----> +-----------+
                        |                 |
    +----------+ +----> |                 |
    | Browser  |        +-----------------+
    +----------+                      ^         +---------+
                                      |         | Other   |
                                      +-------+ | service |
                                                +---------+

------------------------------------------------------------------------------

-> # As a type

^

Types are not just for tagging behaviour (such as deserialization).

^

- They also help avoid programmer mistakes
^
- And they can often be inferred by the compiler.

------------------------------------------------------------------------------

-> # As a type

^
We of course get some typechecking.

    div :: Int -> Validated (Not (Equals 0)) -> Int
    div n d = n / getVal d
    zero :: Validated (Equals 0) Int
    zero = ...
    -- Won't typecheck
    -- 10 / zero

^
Not good enough...
^

    positive :: Validated (Minimum 1) Int
    positive = ...
    -- Also won't typecheck
    -- 10 / positive

^
- Here we expect the argument's type to be *exactly* what we mentioned.
^
- But what we really mean is that the argument's type should *imply* the
constraints.
^

    div2 :: (c `Implies` (Not (Equals 0))) => Int -> Validated c -> Int
    div2 = ...
^

- Now dividing by *zero* won't work, but by *positive* will.
^
- As would:
^
  + *Minimum 1 :&& Maximum 10*,
^
  + *Not (Equals 0) :&& Anything*,
^
  + *Equals 10 :|| Equals 100*,
^
  + etc.

------------------------------------------------------------------------------

-> # As a type

How does this work?
^
- We describe a type family for implication:
^
    type family Implies (a :: k) (b :: k) :: Constraint where
        Implies a a = ()
        Implies (a :&& b) c = (a `Implies` c) `Or`  (b `Implies` c)
        ...
        Implies a (Not (Not a)) = ()
        Implies a 'True = ()
        Implies a b = Implies' a b
^

    type family Implies' a b :: Constraint
    type instance Implies' (Length a) (MaxLength b) = a <= b
    type instance Implies' (Length a) (MinLength b) = b <= b
    type instance Implies' (MaxLength a) (MaxLength b) = a <= b
    type instance Implies' (MinLength a) (MinLength b) = b <= a
    ...

^
- Decidable
^
- But unaware of semantics
  + So no checks that *Implies'* type instances are correct.


------------------------------------------------------------------------------

-> # As a type

Note that information is lost:
^

    plusOne :: (c `Implies` Maximum n) => Validated c Int -> Validated (Maximum (n + 1)) Int
    plusOne = ...

^
- If *c* also implies the value is odd, then the returned value should be even.
^
  - But we would have to explicitly mention that
^
  - And then if the value is not known to be odd or even, the program would not
  typecheck (or would require additional runtime validation).

-> # As a type

Type inference - getting your compiler to do your job.
