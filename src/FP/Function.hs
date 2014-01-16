module FP.Function where

import FP.Value

tl :: FunValue
tl (SequenceObject [_]) = emptySeq
tl (SequenceObject (_:os)) = SequenceObject os
tl _ = Bottom

tlr :: FunValue
tlr (SequenceObject [_]) = emptySeq
tlr (SequenceObject os) = SequenceObject $ init os
tlr _ = Bottom

id :: FunValue
id = Prelude.id

atom :: FunValue
atom (AtomObject _) = makeBool True
atom _ = makeBool False

eq :: FunValue
eq (SequenceObject [x,y]) = makeBool $ x == y
eq _ = makeBool False

null :: FunValue
null = AtomObject . BoolAtom . (emptySeq==)

reverse :: FunValue
reverse (SequenceObject os) = SequenceObject $ Prelude.reverse os
reverse _ = Bottom

distl :: FunValue
distl = undefined

distr :: FunValue
distr = undefined

length :: FunValue
length (SequenceObject os) = makeNumber $ fromIntegral $ Prelude.length os
length _ = Bottom

trans :: FunValue
trans = undefined

and :: FunValue
and (SequenceObject [AtomObject (BoolAtom x), AtomObject (BoolAtom y)]) = makeBool $ x && y
and _ = Bottom

or :: FunValue
or (SequenceObject [AtomObject (BoolAtom x), AtomObject (BoolAtom y)]) = makeBool $ x || y
or _ = Bottom

not :: FunValue
not (AtomObject (BoolAtom x)) = makeBool $ Prelude.not x
not _ = Bottom

apndl :: FunValue
apndl (SequenceObject [x, SequenceObject os]) = SequenceObject $ x:os
apndl _ = Bottom

apndr :: FunValue
apndr (SequenceObject [SequenceObject os, x]) = SequenceObject $ os ++ [x]
apndr _ = Bottom

rotl :: FunValue
rotl (SequenceObject (o:os)) = SequenceObject $ os ++ [o]
rotl _ = Bottom

rotr :: FunValue
rotr (SequenceObject os) =
    let lastIndex = Prelude.length os - 1
        (xs, ys) = splitAt lastIndex os
    in SequenceObject $ ys ++ xs
rotr _ = Bottom


s :: Object -> Object -> Object
s (AtomObject (NumberAtom index)) (SequenceObject os) =
    let seqLength = Prelude.length os
        intIndex = fromIntegral index
    in  if intIndex <= seqLength
            then os !! (intIndex - 1)
            else Bottom
s _ _ = Bottom

sr :: Object -> Object -> Object
sr (AtomObject (NumberAtom index)) (SequenceObject os) =
    let seqLength = Prelude.length os
        intIndex = fromIntegral index
    in  if intIndex <= seqLength
            then os !! (seqLength - intIndex)
            else Bottom
sr _ _ = Bottom

add :: FunValue
add (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) = makeNumber $ x + y
add _ = Bottom

subtract :: FunValue
subtract (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) = makeNumber $ x - y
subtract _ = Bottom

multiply :: FunValue
multiply (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) = makeNumber $ x * y
multiply _ = Bottom

divide :: FunValue
divide (SequenceObject [AtomObject (NumberAtom x), AtomObject (NumberAtom y)]) =
    if y == 0 then
        Bottom
    else
        makeNumber $ x `div` y
divide _ = Bottom