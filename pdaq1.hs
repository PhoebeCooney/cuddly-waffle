type PDA = (Int, [Int], [Transition])
type Transition = ((Int, String, String), (Int, String))
type Configuration = (Int, String, String)
data Result = Accept | Reject deriving Show

validTransitions :: Configuration -> [Transition] -> [Transition]
validTransitions cfg [] = []
validTransitions (cstate, str, z) (((tstate, currentc, pop), (nextstate, push)):xs)
 | cstate == tstate && (take 1 str == currentc || currentc == "") && (pop == take 1 z || pop == "") = [((tstate, currentc, pop),(nextstate, push))] ++ (validTransitions (cstate, str, z) xs)
 | otherwise = validTransitions (cstate, str, z) xs

applyConfig :: PDA -> Configuration -> [Transition] -> [Configuration]
applyConfig pda cfg [] = [cfg]
applyConfig (state, astate, trans) (cstate, str, z) (((tstate, currentc, pop), (nextstate, push)):xs)
 | pop == "" && push /= "" = let nstr = drop 1 str
                                 nz = push ++ z
                             in  applyConfig (state, astate, trans) (nextstate, nstr, nz) (validTransitions (nextstate, nstr, nz) trans) ++ (applyConfig (state, astate, trans) (nextstate, str, z) xs)
 | pop /= "" && push /= "" = let nstr = drop 1 str
                                 nz =  push ++ (drop 1 z)
                             in  applyConfig (state, astate, trans) (nextstate, nstr, nz) (validTransitions (nextstate, nstr, nz) trans) ++ (applyConfig (state, astate, trans) (nextstate, str, z) xs)
 | pop /= "" && push == "" = let nstr = drop 1 str
                                 nz = drop 1 z
                             in applyConfig (state, astate, trans) (nextstate, nstr, nz) (validTransitions (nextstate, nstr, nz) trans) ++ (applyConfig (state, astate, trans) (nextstate, str, z) xs)
 | pop == "" && push == "" && currentc == "" = [(nextstate, str, z)] ++ (applyConfig (state, astate, trans) (cstate, str, z) xs)
 | otherwise =  (applyConfig (state, astate, trans) (cstate, str, z) xs)

run :: PDA -> String -> Result
run (state, astate, trans) str = check (state, astate, trans) (applyConfig (state, astate, trans) (state, str, "") (validTransitions (state, str, "") trans))

check :: PDA -> [Configuration] -> Result
check pda [] = Reject
check (istate, astate, trans) ((cstate, str, z):xs)
 | str == "" && z == "" && elem cstate astate = Accept
 | otherwise = check (istate, astate, trans) xs