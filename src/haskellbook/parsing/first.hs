module LearnParsers where
import Text.Trifecta
import Control.Monad.State.Lazy


stop :: Parser a
stop = unexpected "stop"

one = char '1'
-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop
testing = char '1' >> stop

