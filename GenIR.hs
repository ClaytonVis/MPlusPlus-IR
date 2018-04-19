module GenIR where

import SemIR
import AST
import SymbolTable
import SymbolTypes


convProg :: M_prog -> I_prog
convProg (M_prog (decls, stmts)) = IPROG (fbody, locvars, arryspecs, bodystmts)
    where
        st = new_scope L_PROG empty
        cst = (0, st)
        (cst', arryspecs) = var_pass decls cst
        fbody = fun_pass decls cst'
        (n, st') = cst'
        locvars = (\((Symbol_table (_, n, _, _)) : rest) -> n) st'
        bodystmts = stmts_ir stmts cst'


var_pass :: [M_decl] -> CST -> (CST, [(Int,[I_expr])])
var_pass [] cst = (cst, [])
var_pass (d:ds) cst = (cst'', arraySpec)
    where
        (cst', arrspec) = var_p d cst
        (cst'', arrspec') = var_pass ds cst'
        arraySpec = arrspec ++ arrspec'
        

fun_pass :: [M_decl] -> CST -> [I_fbody]
fun_pass [] cst = []
fun_pass (d:ds) cst = fir
    where
        f_ir = fun_p d cst
        f_ir' = fun_pass ds cst
        fir = f_ir ++ f_ir'


var_p :: M_decl -> CST -> (CST, [(Int,[I_expr])])
var_p (M_var (id, exprs, ty)) cst = (cst', arrSpec)
    where
        expIr = exprs_ir exprs cst
        symdesc = VARIABLE(id, ty, (checkDim expIr 0))
        (n,st) = cst
        st' = insert 0 st symdesc
        cst' = (n, st')
        arrSpec = offset cst expIr
var_p (M_fun (id, params, ty, decls, stmts)) cst = (cst', arrSpec)
    where
        (n, st) = cst
        symdesc = FUNCTION (id, loseNames params, ty)
        st' = insert n st symdesc
        cst' = ((n + 1), st')
        arrSpec = []

loseNames :: [(String, Int, M_type)] -> [(M_type, Int)]
loseNames [] = []
loseNames ((_, n, ty):rest) = (ty, n) : (loseNames rest)


checkDim :: [(I_expr, M_type)] -> Int -> Int
checkDim [] n = n
checkDim (((IINT _),M_int):exs) n = checkDim exs (n + 1)
checkDim ((_,_):exs) _ = error "Non-Integer Index Used!"

offset :: CST -> [(I_expr, M_type)] -> [(Int, [I_expr])]
offset = offset

fun_p :: M_decl -> CST -> [I_fbody]
fun_p (M_var (_,_,_)) _ = []
fun_p (M_fun (id, params, ty, decls, stmts)) (n, st) = [fun]
    where
        (label, type_) = (\(I_FUNCTION(_,l,_,t)) -> (l, t)) $ lookupST st id 
        st' = new_scope (L_FUN type_) st 
        st'' = loadArgs st' params
        (cst'', ar) = var_pass decls (n, st'') 
        stmIR = stmts_ir stmts cst''
        ifbod = fun_pass decls cst''
        (vn, vst) = cst''
        (Symbol_table(_, lv, la, _):rest) = vst
        fun = IFUN (label, ifbod, lv, la, ar, stmIR)


loadArgs :: ST -> [(String, Int, M_type)] -> ST
loadArgs st [] = st
loadArgs st ((id, n, type_):rest) = loadArgs (insert 0 st (ARGUMENT(id, type_, n))) rest

stmts_ir :: [M_stmt] -> CST -> [I_stmt]
stmts_ir [] cst = []
stmts_ir (s:rest) cst = (stmt_ir s cst) ++ (stmts_ir rest cst)

stmt_ir :: M_stmt -> CST -> [I_stmt]
stmt_ir stmt (n, st) = case stmt of
    M_ass (id, dims, asigndExp) -> res
        where
            I_VARIABLE(level, offset, _, _) = lookupST st id
            dimsExpr = validDim $ exprs_ir dims (n, st)
            ((expBody, ty):[]) = expr_ir asigndExp (n, st)
            res = IASS(level, offset, dimsExpr, expBody):[]
    M_while (exp, stmt) -> res
        where
            (ixp, t):[] = expr_ir exp (n, st)
            istm:[] = stmt_ir stmt (n, st)
            res = IWHILE(ixp, istm):[]
    M_cond (exp, stm, stm') -> res
        where
            (ixp, t):[] = expr_ir exp (n,st)
            istm1:[] = stmt_ir stm (n, st)
            istm2:[] = stmt_ir stm' (n, st)
            res = ICOND(ixp, istm1, istm2):[]
    M_read (id, exprs) -> res
        where
            I_VARIABLE(level,offset,t,dim) = lookupST st id
            dimsExpr = validDim $ exprs_ir exprs (n, st)
            res = case t of
                M_int -> IREAD_I (level, offset, dimsExpr):[]
                M_bool -> IREAD_B (level, offset, dimsExpr):[]
                M_real -> IREAD_F (level, offset, dimsExpr):[]
    M_print (expr) -> res
        where
            (iexp, t):[] = expr_ir expr (n,st)
            res = case t of
                M_int -> IPRINT_I iexp:[]
                M_bool -> IPRINT_B iexp:[]
                M_real -> IPRINT_F iexp:[]
    M_return (expr) -> res
        where
            (iexp, t):[] = expr_ir expr (n,st)
            res = IRETURN iexp:[]
    M_block (decls, stmts) -> res
        where
            (cst', arraySpec) = var_pass decls (n, st)
            f_ir = fun_pass decls cst'
            istm = stmts_ir stmts cst'
            (num', (Symbol_table(_,n,_,_):_)) = cst'
            res = (IBLOCK(f_ir, n, arraySpec, istm)):[]


validDim :: [(I_expr, M_type)] -> [I_expr]
validDim [] = []
validDim ((e, M_int):rest) = e:(validDim rest)
validDim _ = error "Invalid Array Index"

exprs_ir :: [M_expr] -> CST -> [(I_expr, M_type)]
exprs_ir = exprs_ir 

expr_ir :: M_expr -> CST -> [(I_expr, M_type)]
expr_ir = expr_ir

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


