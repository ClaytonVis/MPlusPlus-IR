module GenIR where

import SemIR
import AST
import SymbolTable
import SymbolTypes


convIR = convIR


convProg :: M_prog -> I_prog
convProg (M_prog (decls, stmts)) = IPROG (fbody, locvars, arryspecs, bodystmts)
    where
        st = new_scope L_PROG empty
        (st', arryspecs) = var_pass decls st
        fbody = fun_pass decls st'
        locvars = (\((Symbol_table (_, n, _, _)) : rest) -> n) st'
        bodystmts = stmts_ir n stmts st'

var_pass :: [M_decl] -> (Int, ST) -> ((Int, ST), [(Int,[I_expr])])
var_pass [] (n, st) = ((n, st), [])
var_pass (d:ds) (n, st) = ((n'', st''), arraySpec)
    where
        ((n', st'), arrspec) = var_p d (n, st)
        ((n'', st''), arrspec') = var_pass ds (n, st')
        arraySpec = arrspec ++ arrspec'
        

fun_pass :: [M_decl] -> (Int, ST) -> [I_fbody]
fun_pass [] st = []
fun_pass (d:ds) st = fir
    where
        f_ir = fun_p d st
        f_ir' = fun_pass ds st
        fir = f_ir ++ f_ir'

var_p :: M_decl -> (Int, ST) -> ((Int, ST), [(Int,[I_expr])])
var_p (M_var (id, exprs, ty)) (n, st) = ((n' ,st'), arrSpec)
    where
        expIr = exprs_ir exprs st
        symdesc = VARIABLE(id, ty, (checkDim expIr 0))
        st' = insert n st symdesc
        arrSpec = offset st expIr

checkDim :: [(I_expr, M_type)] -> Int -> Int
checkDim [] n = n
checkDim (((IINT _),M_int):exs) n = checkDim exs (n + 1)
checkDim ((_,_):exs) _ = error "Non-Integer Index Used!"

offset :: (Int, ST) -> [(I_expr, M_type)] -> [(Int, [I_expr])]
offset = offset

fun_p :: M_decl -> (Int, ST) -> [I_fbody]
fun_p (M_var (_,_,_)) _ = []

stmts_ir :: [M_stmt] -> (Int, ST) -> [I_stmt]
stmts_ir = stmts_ir

exprs_ir :: [M_expr] -> (Int, ST) -> [(I_expr, M_type)]
exprs_ir = exprs_ir 



-- stmt_ir (M_block (ds, stmts)) st = ir
--  where
--      (st', arraySpec) = var_pass ds st
--      f_ir = fun_pass ds st'
--      s_ir = stmt_ir stmts st'
--simpT = "var x[2]:int;\n fun exp(b:int):int\n { var z:int;\n begin if b=0 then z:= 1\n else z:= x[1] * exp(b-1);\n return z;\n end};\n begin\n read x[0];\n read x[1];\n print exp(x[0]);\n end"

--tokT = myLexer simpT
--parT = pProg tokT
--astT = transProg $ (\(Ok t) -> t) parT
--decT = (\(M_prog (d, s)) -> d) astT
--stmT = (\(M_prog (d, s)) -> s) astT


