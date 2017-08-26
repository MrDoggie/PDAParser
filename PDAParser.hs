--- The idea is to use the PDA as a parser to parse a simple English grammar
module PDAParser where 
--- I use this library so I could easy transfer "I have a cat" into ["I", "have", "a", "cat"]
--- To install:         cabal install split
import Data.List.Split

---x stands for bar
data Cat = S | NP | VP | V | D | N | PP | P | EOF |
           Sx | NPx | VPx | Vx | Dx | Nx | PPx | Px deriving (Eq,Show)

--- transfer from "I left" into ["I", "left"]. It is handy to use.
sentenceToList :: String -> [String]
sentenceToList s = splitOn " " s

---trivial function to get the type of a word
typeOf :: String -> Cat
typeOf "" = error "Input needed"
typeOf s = 
    case s of 
        "the" -> D
        "a" -> D
        "cat" -> N
        "dog" -> N
        "ling209" -> NP
        "CSSA" -> NP
        "John" -> NP
        "Mary" -> NP
        "Junyi" -> NP
        "Everyone" -> NP
        "left" -> V
        "thinks" -> V
        "likes" -> V
        "with" -> P

typeNeg :: Cat -> Cat
typeNeg s =
    case s of
        S -> Sx
        NP -> NPx
        VP -> VPx
        V -> Vx
        D -> Dx
        N -> Nx
        PP -> PPx 
        P -> Px 

--- push the category onto the stack, remove the corresponding word.
shift :: ([Cat], [String]) -> ([Cat], [String])
shift (cList, s:sr) = (((typeOf s) : cList), sr)
shift (cList, []) = (cList, [])

--- pop the category bar from the stack, remove the corresponding word.
match :: ([Cat], [String]) -> ([Cat], [String])
match (cx:cr, s:sr) = if (cx == (typeNeg (typeOf s))) then (cr,sr)
                      else (cx:cr, s:sr)
match ([], sl) = ([], sl)
match (cl, []) = (cl, [])

--- Produce a list of possible results if multiple results exist.
lcPredict :: ([Cat], [String]) -> [([Cat], [String])]
lcPredict (cl, []) = [(cl,[])]  -- if the remaining input is empty, no matter how you work on it, it will never succeed. This line largely optimize the program
lcPredict (c1:cr, sl) = 
    case c1 of
        D -> [((Nx:NP:cr,sl))]
        NP -> [((VPx:S:cr),sl)]
        VP -> [((PPx:VP:cr),sl)]
        P -> [((NPx:PP:cr),sl)]
        V -> [((NPx:VP:cr),sl),((Sx:VP:cr),sl),((VP:cr),sl)]  
        otherwise -> [(c1:cr, sl)]
lcPredict ([],sl) = [([],sl)]

---Produce a list of possible results if multiple results exist,
lcConnect :: ([Cat],[String]) ->[([Cat], [String])]
lcConnect (c1:c2:cr, sl) = 
    case c1 of
        D -> if (c2 == NPx) then [((Nx:cr),sl)] else [(c1:c2:cr,sl)]
        NP -> if (c2 == Sx) then [((VPx:cr),sl)] else [(c1:c2:cr,sl)]
        VP -> if (c2 == VPx) then [((PPx:cr),sl)] else [(c1:c2:cr,sl)]
        P -> if (c2 == PPx) then [((NPx:cr),sl)] else [(c1:c2:cr,sl)]
        V -> if (c2 == VPx) then [((NPx:cr),sl),((Sx:cr),sl),(cr,sl)] else [(c1:c2:cr,sl)]
        otherwise -> [(c1:c2:cr,sl)]
lcConnect ([],sl) = [([],sl)]
lcConnect ([c1], sl) = [([c1],sl)]

--- if the operation does not change anything, this operation is uselss. This function makes sure that the program does not keep doing useless thing.
removeDup :: ([Cat], [String]) -> [([Cat],[String])] -> [([Cat],[String])]
removeDup m (x:xs) = if (m == x) then removeDup m xs
                            else x:(removeDup m xs)
removeDup m [] = []


--- conduct all four operations on the input, recursively. Once the input is the form ([],[]), the operation ends. 
parser :: ([Cat],[String]) -> [([Cat], [String])]
parser ([],[]) = [([EOF],[])]
parser (cl, sl) = let result = (removeDup (cl, sl) ((shift (cl, sl) : match (cl, sl): lcPredict (cl, sl))++lcConnect (cl, sl))) in
                if (result == [([],[])]) then [([],[])]
                else (flatten (map (\e-> parser e) result))

--- user interface. the format is: parsable "XXX XX XXX"
parsable :: String -> Bool
parsable s = elem ([EOF],[]) (parser ([Sx], sentenceToList s))
 
flatten [] = []
flatten (firstList:otherLists) = firstList ++ flatten otherLists

{-
Test cases:
---Predicted to be true
    parsable "John likes Mary"
    parsable "John left"
    parsable "John left the dog with Mary"
    parsable "Junyi likes ling209"
    parsable "John thinks Mary likes ling209"
    parsable "John thinks Mary likes Junyi likes John likes ling209"
    parsable "Everyone thinks Junyi thinks the cat thinks John left the dog"

---Predicted to be false
    parsable "John"
    parsable "John likes dog"
    parsable ""
    parsable "John thinks Mary with"

---Limit of the model
    "Everyone left John likes"
    "Everyone likes ling209 thinks Junyi left"
-}


