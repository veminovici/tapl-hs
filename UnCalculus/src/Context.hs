module Context (
    DBIndex,
    Hint,
    Context,
    getVarName,
    freshVarName,
) where

type DBIndex = Int
type Hint = String
type Context = [String]

getVarName :: DBIndex -> Context -> String
getVarName ndx ctx
    | length ctx > ndx = ctx !! ndx
    | otherwise = error ("Requested index " ++ show ndx ++ " of context of length " ++ show (length ctx))

bindVarName :: String -> Context -> Context
bindVarName = (:)

mkFreshVarName :: Hint -> Context -> String
mkFreshVarName hint [] = hint
mkFreshVarName hint ctx@(b:bs)
    | hint == b = mkFreshVarName (hint ++ "'") ctx
    | otherwise = mkFreshVarName hint bs

freshVarName :: Hint -> Context -> (String, Context)
freshVarName hint ctx =
    let n = mkFreshVarName hint ctx in (n, bindVarName n ctx)

