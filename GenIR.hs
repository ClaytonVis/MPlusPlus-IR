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
        arrSpec = calcOffset cst expIr
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
checkDim ((_,M_int):exs) n = checkDim exs (n + 1)
checkDim ((_,_):exs) _ = error "Non-Integer Index Used!"

calcOffset :: CST -> [(I_expr, M_type)] -> [(Int, [I_expr])]
calcOffset cst _ = []

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
            st' = new_scope L_BLK st
            (cst', arraySpec) = var_pass decls (n, st')
            f_ir = fun_pass decls cst'
            istm = stmts_ir stmts cst'
            (num', (Symbol_table(_,n,_,_):_)) = cst'
            res = (IBLOCK(f_ir, n, arraySpec, istm)):[]


validDim :: [(I_expr, M_type)] -> [I_expr]
validDim [] = []
validDim ((e, M_int):rest) = e:(validDim rest)
validDim _ = error "Invalid Array Index"

exprs_ir :: [M_expr] -> CST -> [(I_expr, M_type)]
exprs_ir [] cst = []
exprs_ir (e:rest) cst = (expr_ir e cst) ++ (exprs_ir rest cst)

expr_ir :: M_expr -> CST -> [(I_expr, M_type)]
expr_ir exp (n,st) = case exp of
    M_ival int -> [(IINT int, M_int)]
    M_rval num -> [(IREAL num, M_real)]
    M_bval bl -> [(IBOOL bl, M_bool)]
    M_size (name, dimInt) -> [(dimsize, M_int)]
        where
            I_VARIABLE(level, offset, ty, dims) = lookupST st name
            dimsize = case (dimInt > dims) of
                True -> error "Size of invalid dimension"
                False -> ISIZE(level, offset, dimInt)
    M_id (name, dimexprs) -> [(id, ty)]
        where
            I_VARIABLE(level, offset, ty, dims) = lookupST st name
            indicExprs = validDim $ exprs_ir dimexprs (n,st)
            id = IID(level, offset, indicExprs)
            -- TODO: Make sure every expr has return type, M_int (do a lookup)
    M_app (mop, exprs) -> [(app, ty)]
        where
            iexp = exprs_ir exprs (n,st)
            (iop, ty) = mop_ir mop iexp (n,st)
            sexp = expStripped iexp
            app = IAPP(iop, sexp)

compareArgs :: [(I_expr, M_type)] -> [(M_type, Int)] -> Bool
compareArgs [] [] = True
compareArgs _ [] = False
compareArgs [] _ = False
compareArgs ((inExpr, inType):inArgs) ((reqType, reqSize):reqArgs) = case inType == reqType of
    True -> case inExpr of
        IID (_, _, dims) -> case (length dims) == reqSize of
            True -> compareArgs inArgs reqArgs
            False -> False
        _ -> compareArgs inArgs reqArgs
    False -> False 
 
mop_ir :: M_operation -> [(I_expr, M_type)] -> CST -> (I_opn, M_type)
mop_ir op args (n, st) = case op of
    M_fn name -> res
        where
            I_FUNCTION(level, label, argsReq, ty) = lookupST st name
            res = case compareArgs args argsReq of
                False -> error ("Invalid parameters for: " ++ name)
                True -> (ICALL(label, level), ty)
            -- I_FUNCTION (level, label, argument_types, output_type)
            -- arg_types :: [(M_type, Int)]
    M_add -> case check1DArgs args of
        (M_int):(M_int):[] -> (IADD, M_int)
        (M_int):(M_real):[] -> (IADD_F, M_real)
        (M_real):(M_int):[] -> (IADD_F, M_real)
        (M_real):(M_real):[] -> (IADD_F, M_real)
        _ -> error "Invalid ADD operation"
    M_mul -> case check1DArgs args of
        (M_int):(M_int):[] -> (IMUL, M_int)
        (M_int):(M_real):[] -> (IMUL_F, M_real)
        (M_real):(M_int):[] -> (IMUL_F, M_real)
        (M_real):(M_real):[] -> (IMUL_F, M_real)
        _ -> error "Invalid MUL operation"
    M_sub -> case check1DArgs args of
        (M_int):(M_int):[] -> (ISUB, M_int)
        (M_int):(M_real):[] -> (ISUB_F, M_real)
        (M_real):(M_int):[] -> (ISUB_F, M_real)
        (M_real):(M_real):[] -> (ISUB_F, M_real)
        _ -> error "Invalid SUB operation"
    M_div -> case check1DArgs args of
        (M_int):(M_int):[] -> (IDIV, M_int)
        (M_int):(M_real):[] -> (IDIV_F, M_real)
        (M_real):(M_int):[] -> (IDIV_F, M_real)
        (M_real):(M_real):[] -> (IDIV_F, M_real)
        _ -> error "Invalid DIV operation"
    M_neg -> case check1DArgs args of
        (M_int):[] -> (INEG, M_int)
        (M_real):[] -> (INEG_F, M_int)
        _ -> error "Invalid NEG operation"
    M_lt -> case check1DArgs args of
        (M_int):(M_int):[] -> (ILT, M_bool)
        (M_int):(M_real):[] -> (ILT_F, M_bool)
        (M_real):(M_int):[] -> (ILT_F, M_bool)
        (M_real):(M_real):[] -> (ILT_F, M_bool)
        _ -> error "Invalid LT operation"
    M_le -> case check1DArgs args of
        (M_int):(M_int):[] -> (ILE, M_bool)
        (M_int):(M_real):[] -> (ILE_F, M_bool)
        (M_real):(M_int):[] -> (ILE_F, M_bool)
        (M_real):(M_real):[] -> (ILE_F, M_bool)
        _ -> error "Invalid LE operation"
    M_gt -> case check1DArgs args of
        (M_int):(M_int):[] -> (IGT, M_bool)
        (M_int):(M_real):[] -> (IGT_F, M_bool)
        (M_real):(M_int):[] -> (IGT_F, M_bool)
        (M_real):(M_real):[] -> (IGT_F, M_bool)
        _ -> error "Invalid GT operation"
    M_ge -> case check1DArgs args of
        (M_int):(M_int):[] -> (IGE, M_bool)
        (M_int):(M_real):[] -> (IGE_F, M_bool)
        (M_real):(M_int):[] -> (IGE_F, M_bool)
        (M_real):(M_real):[] -> (IGE_F, M_bool)
        _ -> error "Invalid GE operation"
    M_eq -> case check1DArgs args of
        (M_int):(M_int):[] -> (IEQ, M_bool)
        (M_int):(M_real):[] -> (IEQ_F, M_bool)
        (M_real):(M_int):[] -> (IEQ_F, M_bool)
        (M_real):(M_real):[] -> (IEQ_F, M_bool)
        _ -> error "Invalid EQ operation"
    M_not -> case check1DArgs args of
        (M_bool):(M_bool):[] -> (INOT, M_bool)
        _ -> error "Invalid NOT operation"
    M_and -> case check1DArgs args of
        (M_bool):(M_bool):[] -> (IAND, M_bool)
        _ -> error "Invalid AND operation"
    M_or -> case check1DArgs args of
        (M_bool):(M_bool):[] -> (IOR, M_bool)
        _ -> error "Invalid OR operation"
    M_float -> case check1DArgs args of
        (M_int):[] -> (IFLOAT, M_real)
        (M_real):[] -> (IFLOAT, M_real)
        _ -> error "Invalid FLOAT operation"
    M_floor -> case check1DArgs args of
        (M_real):[] -> (IFLOOR, M_int)
        (M_int):[] -> (IFLOOR, M_int)
        _ -> error "Invalid FLOOR operation"
    M_ceil -> case check1DArgs args of
        (M_real):[] -> (ICEIL, M_int)
        (M_int):[] -> (ICEIL, M_int)
        _ -> error "Invalid CEIL operation"



check1DArgs :: [(I_expr, M_type)] -> [M_type]
check1DArgs [] = []
check1DArgs ((IID(_, _,dims),ty):rest) = case dims of
    [] -> ty:(check1DArgs rest)
    _ -> error "Expected variable, not array"
check1DArgs ((_,ty):rest) = ty:(check1DArgs rest)



expStripped :: [(I_expr, M_type)] -> [I_expr]
expStripped [] = []
expStripped ((ex, _):rest) = ex:(expStripped rest)

typeStripped :: [(I_expr, M_type)] -> [M_type]
typeStripped [] = []
typeStripped ((_, ty):rest) = ty : (typeStripped rest)

