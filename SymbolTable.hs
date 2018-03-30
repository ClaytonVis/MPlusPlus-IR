module SymbolTable (ST, empty, new_scope, insert, lookupST, returnST) where
import SymbolTypes
import AST

empty :: ST
empty = []

new_scope :: ScopeType -> ST -> ST
new_scope = new_scope

insert :: Int -> ST -> SYM_DESC -> (Int, ST)
insert = insert

lookupST :: ST -> String -> SYM_I_DESC
lookupST = lookupST

returnST :: ST -> M_type
returnST = returnST
