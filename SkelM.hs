module SkelM where

-- Haskell module generated by the BNF converter

import AbsM
import ErrM
import AST
type Result = Err String

--failure :: Show a => a -> Result
--failure x = Bad $ "Undefined case: " ++ show x

transID :: ID -> String
transID x = case x of
  ID string -> string

transIVAL :: IVAL -> Int
transIVAL x = case x of
  IVAL string -> read string

transRVAL :: RVAL -> Float
transRVAL x = case x of
  RVAL string -> read string

transBVAL :: BVAL -> Bool
transBVAL x = case x of
  BVAL "true" -> True
  BVAL "false" -> False

transProg :: Prog -> M_prog
transProg x = case x of
  ProgBlock block -> M_prog (ds, ss)
    where
      M_block (ds, ss) = transBlock block

transBlock :: Block -> M_stmt
transBlock x = case x of
  Block1 declarations programbody -> M_block (transDeclarations declarations, transProgram_Body programbody)

transDeclarations :: Declarations -> [M_decl]
transDeclarations x = case x of
  Declarations1 declaration declarations -> (transDeclaration declaration) : (transDeclarations declarations)
  Declarations2 -> []

transDeclaration :: Declaration -> M_decl
transDeclaration x = case x of
  DeclarationVar_Declaration vardeclaration -> transVar_Declaration vardeclaration
  DeclarationFun_Declaration fundeclaration -> transFun_Declaration fundeclaration

transVar_Declaration :: Var_Declaration -> M_decl
transVar_Declaration x = case x of
  Var_Declaration1 id arraydimensions type_ -> M_var (transID id, transArray_Dimensions arraydimensions, transType type_)

transType :: Type -> M_type
transType x = case x of
  Type_int -> M_int
  Type_real -> M_real
  Type_bool -> M_bool

transArray_Dimensions :: Array_Dimensions -> [M_expr]
transArray_Dimensions x = case x of
  Array_Dimensions1 expr arraydimensions -> (transExpr expr) : (transArray_Dimensions arraydimensions)
  Array_Dimensions2 -> []

transFun_Declaration :: Fun_Declaration -> M_decl
transFun_Declaration x = case x of
  Fun_Declaration1 id paramlist type_ funblock -> M_fun (transID id, transParam_List paramlist, transType type_, d, s)
    where
      M_block (d, s) = transFun_Block funblock

transFun_Block :: Fun_Block -> M_stmt
transFun_Block x = case x of
  Fun_Block1 declarations funbody -> M_block (transDeclarations declarations, transFun_Body funbody)

transParam_List :: Param_List -> [(String, Int, M_type)]
transParam_List x = case x of
  Param_List1 parameters -> transParameters parameters

transParameters :: Parameters -> [(String, Int, M_type)]
transParameters x = case x of
  Parameters1 basicdeclaration moreparameters -> (transBasic_Declaration basicdeclaration) : (transMore_Parameters moreparameters)
  Parameters2 -> []

transMore_Parameters :: More_Parameters -> [(String, Int, M_type)]
transMore_Parameters x = case x of
  More_Parameters1 basicdeclaration moreparameters -> (transBasic_Declaration basicdeclaration) : (transMore_Parameters moreparameters)
  More_Parameters2 -> []

transBasic_Declaration :: Basic_Declaration -> (String, Int, M_type)
transBasic_Declaration x = case x of
  Basic_Declaration1 id basicarraydimensions type_ -> (transID id, transBasic_Array_Dimensions basicarraydimensions, transType type_)

transBasic_Array_Dimensions :: Basic_Array_Dimensions -> Int
transBasic_Array_Dimensions x = case x of
  Basic_Array_Dimensions1 basicarraydimensions -> (transBasic_Array_Dimensions basicarraydimensions) + 1
  Basic_Array_Dimensions2 -> 0

transProgram_Body :: Program_Body -> [M_stmt]
transProgram_Body x = case x of
  Program_Body1 progstmts -> transProg_Stmts progstmts

transFun_Body :: Fun_Body -> [M_stmt]
transFun_Body x = case x of
  Fun_Body1 progstmts expr -> (transProg_Stmts progstmts) ++ [M_return $ transExpr expr]

transProg_Stmts :: Prog_Stmts -> [M_stmt]
transProg_Stmts x = case x of
  Prog_Stmts1 progstmt progstmts -> (transProg_Stmt progstmt) : (transProg_Stmts progstmts)
  Prog_Stmts2 -> []

transProg_Stmt :: Prog_Stmt -> M_stmt
transProg_Stmt x = case x of
  Prog_Stmt1 expr progstmt1 progstmt2 -> M_cond (transExpr expr, transProg_Stmt progstmt1, transProg_Stmt progstmt2)
  Prog_Stmt2 expr progstmt -> M_while (transExpr expr, transProg_Stmt progstmt)
  Prog_Stmt3 identifier -> M_read $ transIdentifier identifier
  Prog_Stmt4 identifier expr -> M_ass (v, exps, transExpr expr)
    where
      (v, exps) = transIdentifier identifier
  Prog_Stmt5 expr -> M_print $ transExpr expr
  Prog_Stmt6 block -> transBlock block

transIdentifier :: Identifier -> (String, [M_expr])
transIdentifier x = case x of
  Identifier1 id arraydimensions -> (transID id, transArray_Dimensions arraydimensions)

transExpr :: Expr -> M_expr
transExpr x = case x of
  Expr1 expr bintterm -> M_app (M_or, (transExpr expr) : [transBInt_Term bintterm])
  ExprBInt_Term bintterm -> transBInt_Term bintterm

transBInt_Term :: BInt_Term -> M_expr
transBInt_Term x = case x of
  BInt_Term1 bintterm bintfactor -> M_app (M_and, (transBInt_Term bintterm) : [transBInt_Factor bintfactor])
  BInt_TermBInt_Factor bintfactor -> transBInt_Factor bintfactor

transBInt_Factor :: BInt_Factor -> M_expr
transBInt_Factor x = case x of
  BInt_Factor1 bintfactor -> M_app (M_not, [transBInt_Factor bintfactor])
  BInt_Factor2 intexpr1 compareop intexpr2 -> M_app (transCompare_Op compareop, (transInt_Expr intexpr1) : [transInt_Expr intexpr2])
  BInt_FactorInt_Expr intexpr -> transInt_Expr intexpr

transCompare_Op :: Compare_Op -> M_operation
transCompare_Op x = case x of
  Compare_Op1 -> M_eq
  Compare_Op2 -> M_lt
  Compare_Op3 -> M_gt
  Compare_Op4 -> M_le
  Compare_Op5 -> M_ge

transInt_Expr :: Int_Expr -> M_expr
transInt_Expr x = case x of
  Int_Expr1 intexpr addop intterm -> M_app (transAddop addop, (transInt_Expr intexpr) : [transInt_Term intterm])
  Int_ExprInt_Term intterm -> transInt_Term intterm

transAddop :: Addop -> M_operation
transAddop x = case x of
  Addop1 -> M_add
  Addop2 -> M_sub

transInt_Term :: Int_Term -> M_expr
transInt_Term x = case x of
  Int_Term1 intterm mulop intfactor -> M_app (transMulop mulop, (transInt_Term intterm) : [transInt_Factor intfactor])
  Int_TermInt_Factor intfactor -> transInt_Factor intfactor

transMulop :: Mulop -> M_operation
transMulop x = case x of
  Mulop1 -> M_mul
  Mulop2 -> M_div

transInt_Factor :: Int_Factor -> M_expr
transInt_Factor x = case x of
  Int_Factor1 expr -> transExpr expr
  Int_Factor2 id basicarraydimensions -> M_size (transID id, transBasic_Array_Dimensions basicarraydimensions)
  Int_Factor3 expr -> M_app (M_float, [transExpr expr])
  Int_Factor4 expr -> M_app (M_floor, [transExpr expr])
  Int_Factor5 expr -> M_app (M_ceil, [transExpr expr])
  Int_Factor6 id modifierlist -> case modifierlist of
    Modifier_List1 args -> M_app (M_fn (transID id), (transModifier_List modifierlist))
    Modifier_ListArray_Dimensions arrDim -> M_id (transID id, (transModifier_List modifierlist))
  Int_FactorIVAL ival -> M_ival $ transIVAL ival
  Int_FactorRVAL rval -> M_rval $ transRVAL rval
  Int_FactorBVAL bval -> M_bval $ transBVAL bval
  Int_Factor7 intfactor -> M_app (M_sub, [transInt_Factor intfactor])

transModifier_List :: Modifier_List -> [M_expr]
transModifier_List x = case x of
  Modifier_List1 arguments -> transArguments arguments
  Modifier_ListArray_Dimensions arraydimensions -> transArray_Dimensions arraydimensions

transArguments :: Arguments -> [M_expr]
transArguments x = case x of
  Arguments1 expr morearguments -> (transExpr expr) : (transMore_Arguments morearguments)
  Arguments2 -> []

transMore_Arguments :: More_Arguments -> [M_expr]
transMore_Arguments x = case x of
  More_Arguments1 expr morearguments -> (transExpr expr) : (transMore_Arguments morearguments)
  More_Arguments2 -> []

