module SymbolTypes where

import AST

-----------------------------------------------------------------------------
-- Symbol Description
-----------------------------------------------------------------------------
-- ARGUMENT (name?, type, dimensions)
-- VARIABLE (name?, type, dimensions)
-- FUNCTION (name?, argument_types, output_type)
-----------------------------------------------------------------------------
data SYM_DESC = ARGUMENT (String, M_type, Int)
        |VARIABLE (String, M_type, Int)
        |FUNCTION (String, [(M_type, Int)], M_type)
    deriving (Show)

-----------------------------------------------------------------------------
-- Symbol Information Description
-----------------------------------------------------------------------------
-- I_VARIABLE (level, offset, type, dimensions)
-- I_FUNCTION (level, label, argument_types, output_type)
-----------------------------------------------------------------------------
data SYM_I_DESC = I_VARIABLE (Int, Int, M_type, Int)
        |I_FUNCTION (Int, String, [(M_type, Int)], M_type)
    deriving (Show)

-----------------------------------------------------------------------------
-- Symbol Values
-----------------------------------------------------------------------------
-- Var_attr (offset, type, dimension)
-- Fun_atrr (label, arg_types, type)
-----------------------------------------------------------------------------
data SYM_VALUE = Var_attr (Int, M_type, Int)
        |Fun_attr (String, [(M_type, Int)], M_type)
    deriving (Show)

-----------------------------------------------------------------------------
-- Scope Type
-----------------------------------------------------------------------------
-- L_FUN (return_type)
-----------------------------------------------------------------------------
data ScopeType = L_PROG 
        | L_FUN M_type 
        | L_BLK 
        | L_CASE
    
-----------------------------------------------------------------------------
-- Symbol Table
-----------------------------------------------------------------------------
-- Symbol_table (Type, Size of Local Var Pool, Number of Args, (Name var/fun, values)
-----------------------------------------------------------------------------
data SYM_TABLE = Symbol_table (ScopeType, Int, Int, [(String, SYM_VALUE)])

type ST = [SYM_TABLE]

data SYM_ERROR = Sym_error String
