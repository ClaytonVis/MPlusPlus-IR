module AST where
{-
The AST was laid out by Dr. Cockett in his documentation for the M+ specs
-}

-----------------------------------------------------------------------------
-- 
-----------------------------------------------------------------------------
data M_prog = M_prog ([M_decl],[M_stmt])
  deriving (Eq, Ord, Show, Read)

foldProg :: (([M_decl], [M_stmt]) -> r) -> M_prog -> r
foldProg f (M_prog (decls, stmts)) = f (decls, stmts)
-----------------------------------------------------------------------------
-- Declarations:
--  M_var (id, array_dimensions, type)
--  M_fun (id, parameterlist, type, d, s) where are taken from a M_block (d, s)
-----------------------------------------------------------------------------
data M_decl = M_var (String,[M_expr],M_type)
    | M_fun (String,[(String,Int,M_type)],M_type,[M_decl],[M_stmt])
  deriving (Eq, Ord, Show, Read)

foldDecl  :: ((String, [M_expr], M_type) -> r)
        -> ((String, [(String, Int, M_type)], M_type, [r], [M_stmt]) -> r)
        -> M_decl -> r
foldDecl v f dec = case dec of
    M_var (name, dim, t) -> v (name, dim, t)
    M_fun (name, params, t, decs, stmts) -> f (name, params, t, (map repeat decs), stmts)
        where
            repeat = foldDecl v f

-----------------------------------------------------------------------------
-- 
-----------------------------------------------------------------------------
data M_stmt = M_ass (String,[M_expr],M_expr)
    | M_while (M_expr,M_stmt)
    | M_cond (M_expr,M_stmt,M_stmt)
    | M_read (String,[M_expr])
    | M_print M_expr
    | M_return M_expr
    | M_block ([M_decl],[M_stmt])
  deriving (Eq, Ord, Show, Read)

foldStmt :: ((String, [M_expr], M_expr) -> r) -> (M_expr -> [M_stmt] -> r)
        -> (M_expr -> [M_stmt] -> [M_stmt] -> r) -> ((String, [M_expr]) -> r)
        -> (M_expr -> r) -> (M_expr -> r) -> (([M_decl], [r]) -> r)
        -> M_stmt -> r
foldStmt a w c r p ret b stmt = case stmt of
    M_ass (name, arry_dim, t) -> a (name, arry_dim, t)
    M_while (expr, stmt') -> w expr (repeat stmt')
    M_cond (expr, s1, s2) -> c expr (repeat s1) (repeat s2)
    M_read (name, dim) -> r (name, dim)
    M_print exp -> p exp
    M_return exp -> ret exp
    M_block (decls, stmts) -> b (decls, (map repeat stmts))
        where
            repeat = foldStmt a w c r p ret b

data M_type = M_int | M_bool | M_real 
  deriving (Eq, Ord, Show, Read)

foldType :: r -> r -> r -> M_type -> r
foldType i b r t = case t of
    M_int -> i
    M_bool -> b
    M_real -> r

data M_expr = M_ival Int
    | M_rval Float
    | M_bval Bool
    | M_size (String,Int)
    | M_id (String,[M_expr])
    | M_app (M_operation,[M_expr])
  deriving (Eq, Ord, Show, Read)

foldExp i r b s id a e = case e of
    M_ival num -> i num
    M_rval flo -> r flo
    M_bval boo -> b boo
    M_size (name, num) -> s (name, num)
    M_id (name, exps) -> id name (map (foldExp i r b s id a) exps)
    M_app (op, exps) -> a op (map (foldExp i r b s id a) exps)

data M_operation = M_fn String
    | M_add | M_mul | M_sub | M_div | M_neg
    | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
    | M_float | M_floor | M_ceil
  deriving (Eq, Ord, Show, Read)

foldOp :: (String -> r) -> r -> r -> r -> r -> r -> r -> r -> r
          -> r -> r -> r -> r -> r -> r -> r -> r -> M_operation -> r 
foldOp f a m s d n lt le gt ge e nt an or float flo ceil op = case op of
    M_fn name -> f name
    M_add -> a
    M_mul -> m
    M_sub -> s
    M_div -> d
    M_neg -> n
    M_lt -> lt
    M_le -> le
    M_gt -> gt
    M_ge -> ge
    M_eq -> e
    M_not -> nt
    M_and -> an
    M_or -> or
    M_float -> float
    M_floor -> flo
    M_ceil -> ceil





ind :: Int -> String
ind 0 = ""
ind n = ". " ++ (ind $ n - 1)

ppProg :: M_prog -> String
ppProg (M_prog (decls, stmts)) = "M_prog(\n" ++ (ind 1) ++ "[\n" ++ (concat (map (ppDec 2) decls)) ++ (ind 1) ++ "],\n" ++ (ind 1) ++ "[\n" ++ (concat (map (ppStmt 2) stmts)) ++ (ind 1) ++ "]\n)"

ppDec :: Int -> M_decl -> String
ppDec n (M_var (str, exprs, type_)) = (ind n) ++ "M_var(" ++ str ++ ",\n" ++ (ind (n + 1)) ++ "[\n"  ++ (concat (map (ppExpr (n + 2)) exprs)) ++ (ind (n + 1)) ++ "],\n" ++ (ppType (n + 1) type_) ++ "\n"  ++ (ind n) ++ ")\n"
ppDec n (M_fun (str, trip, type_, decs, stmts)) = (ind n) ++ "M_fun(" ++ str ++ ",\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppTrip (n + 2)) trip)) ++ (ind (n + 1)) ++ "],\n" ++ (ppType (n + 1) type_) ++ ",\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppDec (n + 2)) decs)) ++ (ind (n + 1)) ++ "],\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppStmt (n + 2)) stmts)) ++ (ind (n + 1)) ++ "]\n" ++ (ind n) ++ ")\n"

ppTrip :: Int -> (String, Int, M_type) -> String
ppTrip n (str, num, type_) = (ind n) ++ "(" ++ str ++ ", " ++ (show num) ++ ", " ++ (ppType 0 type_) ++ ")\n"

ppStmt :: Int -> M_stmt -> String
ppStmt n st = case st of
  M_ass (str, exprs, expr) -> (ind n) ++ "M_ass(" ++ str ++ ",\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppExpr (n + 2)) exprs)) ++ (ind (n + 1)) ++ "],\n" ++ (ppExpr (n + 1) expr) ++ (ind n) ++ ")\n"
  M_while (expr, stmt) -> (ind n) ++ "M_while(\n" ++ (ppExpr (n + 1) expr) ++ (ind (n + 1)) ++ ",\n" ++ (ppStmt (n + 1) stmt) ++ (ind n) ++ ")\n"
  M_cond (expr, stmt1, stmt2) -> (ind n) ++ "M_cond(\n" ++ (ppExpr (n + 1) expr) ++ (ind (n + 1)) ++ ",\n" ++ (ppStmt (n + 1) stmt1) ++ (ind (n + 1)) ++ ",\n" ++ (ppStmt (n + 1) stmt2) ++ (ind n) ++ ")\n"
  M_read (str, exprs) -> (ind n) ++ "M_read(" ++ str ++ ",\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppExpr (n + 2)) exprs)) ++ (ind (n + 1)) ++ "]\n" ++ (ind n) ++ ")\n"
  M_print expr -> (ind n) ++ "M_print(\n" ++ (ppExpr (n + 1) expr) ++ (ind n) ++ ")\n"
  M_return expr -> (ind n) ++ "M_return(\n" ++ (ppExpr (n + 1) expr) ++ (ind n) ++ ")\n"
  M_block (decs, stmts) -> (ind n) ++ "M_block(\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppDec (n + 2)) decs)) ++ "],\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppStmt (n + 2)) stmts)) ++ (ind (n + 1)) ++ "]\n" ++ (ind n) ++ ")\n"

ppType :: Int -> M_type -> String
ppType n ty = case ty of
  M_int -> (ind n) ++ "M_int"
  M_bool -> (ind n) ++ "M_bool"
  M_real -> (ind n) ++ "M_real"

ppExpr :: Int -> M_expr -> String
ppExpr n exp = case exp of
  M_ival num -> (ind n) ++ "M_ival(" ++ (show num) ++ ")\n"
  M_rval flo -> (ind n) ++ "M_rval(" ++ (show flo) ++ ")\n"
  M_bval bl -> (ind n) ++ "M_bval(" ++ (show bl) ++ ")\n"
  M_size (str, num) -> (ind n) ++ "M_size(" ++ str ++ ", " ++ (show num) ++ ")\n"
  M_id (str, exprs) -> (ind n) ++ "M_id(" ++ str ++ ",\n" ++ (ind (n + 1)) ++  "[\n" ++ (concat (map (ppExpr (n + 2)) exprs)) ++ (ind (n + 1)) ++ "]\n" ++ (ind n) ++ ")\n"
  M_app (op, exprs) -> (ind n) ++ "M_app(" ++ (ppOp (n + 1) op) ++ ",\n" ++ (ind (n + 1)) ++ "[\n" ++ (concat (map (ppExpr (n + 2)) exprs)) ++ (ind (n + 1)) ++ "]\n" ++ (ind n) ++ ")\n"

ppOp :: Int -> M_operation -> String
ppOp n op = case op of
  M_fn str -> "M_fn(" ++ str ++ ")"
  M_add -> "M_add"
  M_sub -> "M_sub"
  M_mul -> "M_mul"
  M_div -> "M_div"
  M_neg -> "M_neg"
  M_lt -> "M_lt"
  M_gt -> "M_gt"
  M_le -> "M_le"
  M_ge -> "M_ge"
  M_eq -> "M_eq"
  M_not -> "M_not"
  M_and -> "M_and" 
  M_or -> "M_or"
  M_float -> "M_float"
  M_floor -> "M_floor"
  M_ceil -> "M_ceil"

