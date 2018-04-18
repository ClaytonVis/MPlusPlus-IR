module SemIR where

data I_prog  = IPROG    ([I_fbody],Int,[(Int,Int)],[I_stmt])
data I_fbody = IFUN     (string,[I_fbody],Int,[I_array],[I_stmt])
data I_array = IALLOC   (Int,[I_expr])
data I_stmt = IASS_V    (Int,Int,I_expr)
            | IASS_A    (Int,Int,I_expr,Iexpr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | iREAD_F   (Int,Int)
            | iREAD_I   (Int,Int)
            | iREAD_B   (Int,Int)
            | iREAD_AF  (Int,Int,I_expr)
            | iREAD_AI  (Int,Int,I_expr)
            | iREAD_AB  (Int,Int,I_expr)
            | iPRINT_F  I_expr
            | iPRINT_I  I_expr
            | iPRINT_B  I_expr
            | iRETURN I_expr
            | iBLOCK    ([I_fbody],Int,[I_array],[I_stmt])
data I_expr = IINT      Int
            | IREAL     Real
            | IBOOL     Bool
            | IID_V     (Int,Int)
            | IID_A     (Int,Int,I_expr)
            | IAPP      (I_opn,[I_expr])
data I_opn = ICALL      (String,Int)
           | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
           | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ
           | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR
