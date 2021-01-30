import Data.Stack

main :: IO ()
main = do
    let st = push 15 $ push 10 $ push 5 $ push 0 empty
    let st' = pop $ pop st
    let st'' = push 100 st'
    let shouldBeTrue = [ top st' == 5,
                     top st'' == 100,
                     isEmpty $ pop $ pop st']
    print $ and shouldBeTrue
