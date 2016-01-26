%title: verdict - Type, Validate, Refine
%author: Julian K. Arni
%date: 19 Feb 2016

-> verdict <-
==============
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

-> # Do it once

## Examples

### JSON
