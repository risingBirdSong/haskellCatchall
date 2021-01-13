import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- *Main> :i monoid
-- monoid ::
--   (Monoid a, Show a, Arbitrary a, EqProp a) => a -> TestBatch
--         -- Defined in `Test.QuickCheck.Classes'

-- *Main> quickBatch (monoid ("abc", "def"))
-- monoid:
--   left  identity: +++ OK, passed 500 tests.
--   right identity: +++ OK, passed 500 tests.
--   associativity:  +++ OK, passed 500 tests.
--   mappend = (<>): +++ OK, passed 500 tests.
--   mconcat:        +++ OK, passed 500 tests.



-- *Main> quickBatch (monoid ("abc"))
-- monoid:
--   left  identity: +++ OK, passed 500 tests.
--   right identity: +++ OK, passed 500 tests.
--   associativity:  +++ OK, passed 500 tests.
--   mappend = (<>): +++ OK, passed 500 tests.
--   mconcat:        +++ OK, passed 500 tests.

-- *Main> quickBatch (monoid ([1,2,3]))
-- monoid:
--   left  identity: +++ OK, passed 500 tests.
--   right identity: +++ OK, passed 500 tests.
--   associativity:  +++ OK, passed 500 tests.
--   mappend = (<>): +++ OK, passed 500 tests.
--   mconcat:        +++ OK, passed 500 tests.

-- *Main> quickBatch (monoid (Sum 1))
-- monoid:
--   left  identity: +++ OK, passed 500 tests.
--   right identity: +++ OK, passed 500 tests.
--   associativity:  +++ OK, passed 500 tests.
--   mappend = (<>): +++ OK, passed 500 tests.
--   mconcat:        +++ OK, passed 500 tests.

-- *Main> quickBatch (semigroup ("something", 1))
-- semigroup:
--   associativity: +++ OK, passed 500 tests.
--   sconcat:       +++ OK, passed 500 tests.
--   stimes:        +++ OK, passed 500 tests.