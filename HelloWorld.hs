import Data.Foldable
import Data.String

type Unop a = a -> a
type Biop a = a -> Unop a

data RingF a
    = RConstF a
    | RAddF a a
    | RMultF a a

main :: IO ()
main = do
    print 1
