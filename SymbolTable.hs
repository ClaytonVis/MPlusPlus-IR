module SymbolTable (ST, empty, new_scope, insert, lookupST, returnST) where
import SymbolTypes
import AST

empty :: ST
empty = []

new_scope :: ScopeType -> ST -> ST
new_scope type_ s = (Symbol_table(0, 0,[])):s

insert :: Int -> ST -> SYM_DESC -> ST
insert n [] d = error "Symbol table error: insertion before defining scope."
insert n ((Symbol_table(nL, nA, sL)):rest) desc = case desc of
    ARGUMENT(str, t, dim) -> if (in_index_list str sL)
        then error ("Symbol table error: " ++ str ++ " is already defined.")
        else (Symbol_table(nL, (nA + 1), ((str, Var_attr((nA + 4), t, dim)):sL)):rest)
    VARIABLE(str, t, dim) -> if (in_index_list str sL)
        then error ("Symbol table error: " ++ str ++ " is already defined.")
        else (Symbol_table((nL + 1), nA, ((str, Var_attr((nL + 1), t, dim)):sL)):rest)
    FUNCTION(str, ts, t) -> if (in_index_list str sL)
        then error ("Symbol table error: " ++ str ++ " is already defined.")
        else (Symbol_table(nL, nA, ((str, Fun_attr(getlabel str, ts, t)):sL)):rest)
    where
        --getlabel :: String -> String
        getlabel = ("fun_" ++)
        --in_index_list str sL
        in_index_list str [] = False
        in_index_list str ((x,_):xs) | str == x = True
                                     | otherwise = in_index_list str xs



lookupST :: ST -> String -> SYM_I_DESC
lookupST s x = find 0 s 
        where
    found level (Var_attr(offset, type_, dim)) =  I_VARIABLE(level, offset, type_, dim)
    found level (Fun_attr(label, arg_Type, type_)) = I_FUNCTION(level, label, arg_Type, type_)

    find_level :: [(String, SYM_VALUE)] -> Maybe SYM_VALUE
    find_level ((str, v):r)
        | x == str = Just v
        | otherwise =  find_level r
    find_level [] = Nothing

    find :: Int -> ST -> SYM_I_DESC
    find n [] = error ("Could not find " ++ x)
    find n (Symbol_table(_,_,vs):r) = case find_level vs of 
        Just v -> found n v
        Nothing -> find (n+1) r

returnST :: ST -> M_type
returnST = returnST
