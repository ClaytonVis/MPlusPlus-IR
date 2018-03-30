module SymbolTypes where

import AST

data SYM_DESC = ARGUMENT (String, M_type, Int)
        |VARIABLE (String, M_type, Int)
        |FUNCTION (String, M_type, Int)
        |DATATYPE String
        |CONSTRUCTOR (String, [M_type], String)
    deriving (Show)

data SYM_I_DESC = I_VARIABLE (Int, Int, M_type, Int)
        |I_FUNCTION (Int, String, [(M_type, Int)], M_type)
        |I_CONSTRUCTOR (Int, [M_type], String)
        |I_TYPE [String]
    deriving (Show)

data ScopeType = L_PROG
        |L_FUN M_type
        |L_BLK
        |L_CASE
    deriving (Show)

data SYM_VALUE = Var_attr (Int, M_type, Int)
        |Fun_attr (String, [(M_type, Int)], M_type)
        |Con_attr (Int, [M_type], String)
        |Typ_attr [String]
    deriving (Show)

data SYM_TABLE = Symbol_table (Int, Int, [(String, SYM_VALUE)])

type ST = [SYM_TABLE]
