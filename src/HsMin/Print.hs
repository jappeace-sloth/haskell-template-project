{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module HsMin.Print
  ( printModule
  , PrintOpts(..)
  , defaultPrintOpts
  ) where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Hs
import GHC.Types.Basic (OverlapMode(..))
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (Located, GenLocated(..), unLoc)
import GHC.Types.SourceText (IntegralLit(..))
import GHC.Utils.Outputable (Outputable, showPprUnsafe)
import Language.Haskell.Syntax.Basic (Boxity(..))

-- | Options controlling compact printing
data PrintOpts = PrintOpts
  { poUseBraces :: Bool  -- ^ Use {;} syntax instead of layout
  } deriving (Show, Eq)

-- | Default print options: use braces
defaultPrintOpts :: PrintOpts
defaultPrintOpts = PrintOpts
  { poUseBraces = True
  }

-- | Print a parsed module in compact form
printModule :: PrintOpts -> Located (HsModule GhcPs) -> String
printModule opts (L _ modl) = case modl of
  HsModule _ext mname mexports imports decls ->
    header ++ body
    where
      header :: String
      header = printModuleHeader opts mname mexports
      bodyItems :: [String]
      bodyItems = filter (not . null)
        ( map (printImport opts) imports
       ++ map (printDecl opts) decls
        )
      body :: String
      body
        | null bodyItems = ""
        | poUseBraces opts = "{" ++ intercalate ";" bodyItems ++ "}"
        | otherwise = intercalate ";" bodyItems
  XModule x -> absurd' x

-- | Print module header: module Name (exports) where
printModuleHeader :: PrintOpts -> Maybe (XRec GhcPs ModuleName) -> Maybe (XRec GhcPs [LIE GhcPs]) -> String
printModuleHeader _opts Nothing _ = ""
printModuleHeader opts (Just (L _ mname)) mexports =
  "module " ++ moduleNameString mname ++ exportList ++ " where"
  where
    exportList = case mexports of
      Nothing -> ""
      Just (L _ ies) -> "(" ++ intercalate "," (map (printIE opts) ies) ++ ")"

-- | Print an import/export item
printIE :: PrintOpts -> LIE GhcPs -> String
printIE _opts (L _ ie) = case ie of
  IEVar _ lname _ -> ppr lname
  IEThingAbs _ lname _ -> ppr lname
  IEThingAll _ lname _ -> ppr lname ++ "(..)"
  IEThingWith _ lname _ pieces _ ->
    ppr lname ++ "(" ++ intercalate "," (map ppr pieces) ++ ")"
  IEModuleContents _ (L _ mn) -> "module " ++ moduleNameString mn
  IEGroup {} -> ""
  IEDoc {} -> ""
  IEDocNamed {} -> ""

-- | Print a single import declaration
printImport :: PrintOpts -> LImportDecl GhcPs -> String
printImport _opts (L _ decl) =
  "import " ++ qual ++ src ++ moduleNameString (unLoc (ideclName decl)) ++ alias ++ hiding ++ impList
  where
    qual = if ideclQualified decl /= NotQualified then "qualified " else ""
    src = case ideclSource decl of
      IsBoot -> "{-# SOURCE #-} "
      NotBoot -> ""
    alias = case ideclAs decl of
      Nothing -> ""
      Just (L _ mn) -> " as " ++ moduleNameString mn
    (hiding, impList) = case ideclImportList decl of
      Nothing -> ("", "")
      Just (EverythingBut, L _ ies) ->
        (" hiding", "(" ++ intercalate "," (map (printIE defaultPrintOpts) ies) ++ ")")
      Just (Exactly, L _ ies) ->
        ("", "(" ++ intercalate "," (map (printIE defaultPrintOpts) ies) ++ ")")

-- | Print a single declaration
printDecl :: PrintOpts -> LHsDecl GhcPs -> String
printDecl opts (L _ decl) = case decl of
  TyClD _ tycl   -> printTyClDecl opts tycl
  InstD _ inst    -> printInstDecl opts inst
  ValD _ bind     -> printBind opts bind
  SigD _ sig      -> printSig opts sig
  WarningD {} -> ""
  DocD {} -> ""
  _ -> ppr decl

-- | Print type class and data declarations
printTyClDecl :: PrintOpts -> TyClDecl GhcPs -> String
printTyClDecl opts decl = case decl of
  SynDecl _ lname tvs _fix rhs ->
    "type " ++ pn lname ++ printTyVarBndrs opts tvs ++ "=" ++ printType opts (unLoc rhs)
  DataDecl _ lname tvs _fix defn ->
    printDataDefn opts (pn lname ++ printTyVarBndrs opts tvs) defn
  ClassDecl _ ctx lname tvs _fix _fds sigs meths ats atdefs _ ->
    "class " ++ printContext opts ctx ++ pn lname ++ printTyVarBndrs opts tvs ++ " where"
    ++ braceBlock opts (
         map (printSig opts . unLoc) sigs
      ++ map (printBind opts . unLoc) meths
      ++ map (ppr . unLoc) ats
      ++ map ppr atdefs
    )
  _ -> ppr decl

-- | Print a data/newtype definition
printDataDefn :: PrintOpts -> String -> HsDataDefn GhcPs -> String
printDataDefn opts header defn =
  keyword ++ header ++ ctx ++ eqOrWhere ++ consPart ++ derivs
  where
    keyword = case dd_cons defn of
      NewTypeCon _ -> "newtype "
      DataTypeCons False _ -> "data "
      DataTypeCons True _ -> "type data "
    ctx = printContext opts (dd_ctxt defn)
    cons = case dd_cons defn of
      NewTypeCon con -> [printConDecl opts (unLoc con)]
      DataTypeCons _ cs -> map (printConDecl opts . unLoc) cs
    (eqOrWhere, consPart) = case cons of
      [] -> ("", "")
      _  -> ("=", intercalate "|" cons)
    derivs = concatMap (printDerivingClause opts . unLoc) (dd_derivs defn)

-- | Print a constructor declaration
printConDecl :: PrintOpts -> ConDecl GhcPs -> String
printConDecl opts con = case con of
  ConDeclGADT _ names _bndrs _ctx _args res _ ->
    intercalate "," (map pn (toList' names)) ++ "::" ++ printType opts (unLoc res)
  ConDeclH98 _ lname _forall _tvs _ctx details _ ->
    pn lname ++ printConDeclDetails opts details
  XConDecl x -> absurd' x

-- | Print constructor details (arguments)
printConDeclDetails :: PrintOpts -> HsConDeclH98Details GhcPs -> String
printConDeclDetails opts details = case details of
  PrefixCon _ args -> concatMap (\a -> " " ++ printType opts (unLoc (hsScaledThing a))) args
  RecCon (L _ fields) ->
    "{" ++ intercalate "," (map (printConDeclField opts . unLoc) fields) ++ "}"
  InfixCon a1 a2 ->
    " " ++ printType opts (unLoc (hsScaledThing a1)) ++ " " ++ printType opts (unLoc (hsScaledThing a2))

-- | Print a record field
printConDeclField :: PrintOpts -> ConDeclField GhcPs -> String
printConDeclField opts (ConDeclField _ names ty _) =
  intercalate "," (map pn names) ++ "::" ++ printType opts (unLoc ty)

-- | Print instance declarations
printInstDecl :: PrintOpts -> InstDecl GhcPs -> String
printInstDecl opts inst = case inst of
  ClsInstD _ cid -> printClsInstDecl opts cid
  _ -> ppr inst

-- | Print class instance declaration
printClsInstDecl :: PrintOpts -> ClsInstDecl GhcPs -> String
printClsInstDecl opts cid =
  "instance " ++ printOverlapPragma (cid_overlap_mode cid)
  ++ printSigType opts (unLoc (cid_poly_ty cid))
  ++ " where" ++ braceBlock opts (
       map (printBind opts . unLoc) (cid_binds cid)
    ++ map (printSig opts . unLoc) (cid_sigs cid)
  )

-- | Print overlap pragma
printOverlapPragma :: Maybe (XRec GhcPs OverlapMode) -> String
printOverlapPragma Nothing = ""
printOverlapPragma (Just (L _ mode)) = case mode of
  NoOverlap _    -> ""
  Overlappable _ -> "{-# OVERLAPPABLE #-} "
  Overlapping _  -> "{-# OVERLAPPING #-} "
  Overlaps _     -> "{-# OVERLAPS #-} "
  Incoherent _   -> "{-# INCOHERENT #-} "
  NonCanonical _ -> "{-# NONCANONICAL #-} "

-- | Print a deriving clause
printDerivingClause :: PrintOpts -> HsDerivingClause GhcPs -> String
printDerivingClause opts (HsDerivingClause _ strat dcs) =
  " deriving " ++ stratBefore ++ printDerivClauseTys dcs ++ stratAfter
  where
    (stratBefore, stratAfter) = case strat of
      Nothing -> ("", "")
      Just (L _ s) -> case s of
        StockStrategy _ -> ("stock ", "")
        AnyclassStrategy _ -> ("anyclass ", "")
        NewtypeStrategy _ -> ("newtype ", "")
        ViaStrategy (XViaStrategyPs _ sigTy) -> ("", " via " ++ printSigType opts (unLoc sigTy))
    printDerivClauseTys :: LDerivClauseTys GhcPs -> String
    printDerivClauseTys (L _ dct) = case dct of
      DctSingle _ ty -> printSigType opts (unLoc ty)
      DctMulti _ tys -> "(" ++ intercalate "," (map (printSigType opts . unLoc) tys) ++ ")"
      XDerivClauseTys x -> absurd' x

-- | Print a context (class constraints)
printContext :: PrintOpts -> Maybe (LHsContext GhcPs) -> String
printContext _opts Nothing = ""
printContext _opts (Just (L _ [])) = ""
printContext opts (Just (L _ [ct])) = printType opts (unLoc ct) ++ "=>"
printContext opts (Just (L _ cts)) =
  "(" ++ intercalate "," (map (printType opts . unLoc) cts) ++ ")=>"

-- | Print a binding (function definition, pattern binding, etc.)
printBind :: PrintOpts -> HsBindLR GhcPs GhcPs -> String
printBind opts bind = case bind of
  FunBind _ lid matches ->
    printMatchGroup opts (pn lid) matches
  PatBind _ pat _mult rhs ->
    printPat opts (unLoc pat) ++ printGRHSs opts "=" rhs
  _ -> ppr bind

-- | Print a signature
printSig :: PrintOpts -> Sig GhcPs -> String
printSig opts sig = case sig of
  TypeSig _ names ty ->
    intercalate "," (map pn names) ++ "::" ++ printSigType opts (unLoc (hswc_body ty))
  PatSynSig _ names ty ->
    "pattern " ++ intercalate "," (map pn names) ++ "::" ++ printSigType opts (unLoc ty)
  ClassOpSig _ deflt names ty ->
    (if deflt then "default " else "") ++ intercalate "," (map pn names) ++ "::" ++ printSigType opts (unLoc ty)
  FixSig _ (FixitySig _ names fixity) ->
    ppr fixity ++ " " ++ intercalate "," (map pn names)
  _ -> ppr sig

-- | Print match groups (for function definitions)
printMatchGroup :: PrintOpts -> String -> MatchGroup GhcPs (LHsExpr GhcPs) -> String
printMatchGroup opts fname (MG _ (L _ matches)) =
  intercalate ";" (map (printMatch opts fname) matches)

-- | Print a single match (one equation)
printMatch :: PrintOpts -> String -> LMatch GhcPs (LHsExpr GhcPs) -> String
printMatch opts fname (L _ (Match _ ctx lpats rhs)) = case ctx of
  FunRhs {} ->
    fname ++ concatMap (\p -> " " ++ printPatPrec opts (unLoc p)) pats ++ printGRHSs opts "=" rhs
  CaseAlt ->
    intercalate " " (map (printPatTop opts . unLoc) pats) ++ printGRHSs opts "->" rhs
  LamAlt _ ->
    intercalate " " (map (printPatPrec opts . unLoc) pats) ++ printGRHSs opts "->" rhs
  _ -> ppr (Match noExtField ctx lpats rhs)
  where
    pats :: [LPat GhcPs]
    pats = unLoc lpats

-- | Print GRHSs (guarded right-hand sides + where clause)
printGRHSs :: PrintOpts -> String -> GRHSs GhcPs (LHsExpr GhcPs) -> String
printGRHSs opts sep (GRHSs _ grhss binds) =
  concatMap (printGRHS opts sep) grhss ++ printLocalBinds opts binds

-- | Print a single GRHS
printGRHS :: PrintOpts -> String -> LGRHS GhcPs (LHsExpr GhcPs) -> String
printGRHS opts sep (L _ (GRHS _ guards body)) = case guards of
  [] -> sep ++ " " ++ printExpr opts (unLoc body)
  gs -> "|" ++ intercalate "," (map (printStmt opts . unLoc) gs) ++ sep ++ " " ++ printExpr opts (unLoc body)

-- | Print local bindings (where clause)
printLocalBinds :: PrintOpts -> HsLocalBinds GhcPs -> String
printLocalBinds opts binds = case binds of
  EmptyLocalBinds _ -> ""
  HsValBinds _ vbs -> case vbs of
    ValBinds _ bs sigs ->
      " where" ++ braceBlock opts (
           map (printSig opts . unLoc) sigs
        ++ map (printBind opts . unLoc) bs
      )
    XValBindsLR _ -> ""
  HsIPBinds {} -> ppr binds

-- | Print an expression
printExpr :: PrintOpts -> HsExpr GhcPs -> String
printExpr opts expr = case expr of
  HsVar _ lid -> pn lid
  HsUnboundVar _ rn -> pn rn
  HsOverLit _ olit -> printOverLit olit
  HsLit _ lit -> printLit lit
  HsLam _ variant mg -> printLamExpr opts variant mg
  HsApp _ f x -> printExpr opts (unLoc f) ++ " " ++ printExprPrec opts (unLoc x)
  OpApp _ l op r ->
    printExpr opts (unLoc l) ++ opStr ++ printExpr opts (unLoc r)
    where
      opStr :: String
      opStr = case unLoc op of
        HsVar _ (L _ rn) ->
          let s = occNameString (rdrNameOcc rn)
          in if isOp s then s else "`" ++ s ++ "`"
        _ -> " " ++ printExpr opts (unLoc op) ++ " "
  NegApp _ e _ -> "-" ++ printExprPrec opts (unLoc e)
  HsPar _ e -> "(" ++ printExpr opts (unLoc e) ++ ")"
  SectionL _ e op -> "(" ++ printExpr opts (unLoc e) ++ " " ++ printExpr opts (unLoc op) ++ ")"
  SectionR _ op e -> "(" ++ printExpr opts (unLoc op) ++ " " ++ printExpr opts (unLoc e) ++ ")"
  ExplicitTuple _ args boxity ->
    let (open, close) = case boxity of
          Boxed -> ("(", ")")
          Unboxed -> ("(#", "#)")
    in open ++ intercalate "," (map (printTupArg opts) args) ++ close
  HsCase _ scrut mg ->
    "case " ++ printExpr opts (unLoc scrut) ++ " of" ++ braceBlock opts (printCaseAlts opts mg)
  HsIf _ cond t f ->
    "if " ++ printExpr opts (unLoc cond) ++ " then " ++ printExpr opts (unLoc t) ++ " else " ++ printExpr opts (unLoc f)
  HsMultiIf _ grhss ->
    "if" ++ braceBlock opts (map (printGRHS opts "->") grhss)
  HsLet _ lbinds body ->
    "let" ++ printLetBinds opts lbinds ++ " in " ++ printExpr opts (unLoc body)
  HsDo _ flavour (L _ stmts) ->
    printDoExpr opts flavour stmts
  ExplicitList _ exprs ->
    "[" ++ intercalate "," (map (printExpr opts . unLoc) exprs) ++ "]"
  RecordCon _ con fields ->
    pn con ++ "{" ++ ppr fields ++ "}"
  ExprWithTySig _ e ty ->
    printExpr opts (unLoc e) ++ "::" ++ printSigType opts (unLoc (hswc_body ty))
  ArithSeq _ _ info -> printArithSeq opts info
  HsTypedBracket _ e -> "[||" ++ printExpr opts (unLoc e) ++ "||]"
  HsTypedSplice _ e -> "$$(" ++ printExpr opts (unLoc e) ++ ")"
  HsStatic _ e -> "static " ++ printExpr opts (unLoc e)
  HsGetField _ e (L _ fld) -> printExpr opts (unLoc e) ++ "." ++ ppr fld
  HsPragE _ _ e -> printExpr opts (unLoc e)
  XExpr x -> absurd' x
  _ -> ppr expr

-- | Print expression with parentheses if it's a complex expression
printExprPrec :: PrintOpts -> HsExpr GhcPs -> String
printExprPrec opts e = case e of
  HsApp {} -> "(" ++ printExpr opts e ++ ")"
  OpApp {} -> "(" ++ printExpr opts e ++ ")"
  NegApp {} -> "(" ++ printExpr opts e ++ ")"
  HsLam {} -> "(" ++ printExpr opts e ++ ")"
  HsCase {} -> "(" ++ printExpr opts e ++ ")"
  HsIf {} -> "(" ++ printExpr opts e ++ ")"
  HsLet {} -> "(" ++ printExpr opts e ++ ")"
  HsDo {} -> "(" ++ printExpr opts e ++ ")"
  ExprWithTySig {} -> "(" ++ printExpr opts e ++ ")"
  _ -> printExpr opts e

-- | Print lambda expressions
printLamExpr :: PrintOpts -> HsLamVariant -> MatchGroup GhcPs (LHsExpr GhcPs) -> String
printLamExpr opts variant mg@(MG _ (L _ matches)) = case variant of
  LamSingle -> "\\" ++ concatMap printLamMatch matches
  LamCase -> "\\case" ++ braceBlock opts (printCaseAlts opts mg)
  LamCases -> "\\cases" ++ braceBlock opts (printCaseAlts opts mg)
  where
    printLamMatch :: LMatch GhcPs (LHsExpr GhcPs) -> String
    printLamMatch (L _ (Match _ _ lpats2 rhs)) =
      intercalate " " (map (printPatPrec opts . unLoc) (unLoc lpats2)) ++ printGRHSs opts " ->" rhs

-- | Print case alternatives from a MatchGroup
printCaseAlts :: PrintOpts -> MatchGroup GhcPs (LHsExpr GhcPs) -> [String]
printCaseAlts opts (MG _ (L _ matches)) =
  map printAlt matches
  where
    printAlt :: LMatch GhcPs (LHsExpr GhcPs) -> String
    printAlt (L _ (Match _ _ lpats2 rhs)) =
      intercalate " " (map (printPatTop opts . unLoc) (unLoc lpats2)) ++ printGRHSs opts "->" rhs

-- | Print do-expression
printDoExpr :: PrintOpts -> HsDoFlavour -> [ExprLStmt GhcPs] -> String
printDoExpr opts flavour stmts =
  keyword ++ braceBlock opts (map (printStmt opts . unLoc) stmts)
  where
    keyword :: String
    keyword = case flavour of
      DoExpr Nothing     -> "do"
      DoExpr (Just m)    -> moduleNameString m ++ ".do"
      MDoExpr Nothing    -> "mdo"
      MDoExpr (Just m)   -> moduleNameString m ++ ".mdo"
      ListComp           -> ""
      MonadComp          -> ""
      GhciStmtCtxt       -> "do"

-- | Print a statement
printStmt :: PrintOpts -> StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> String
printStmt opts stmt = case stmt of
  LastStmt _ body _ _ -> printExpr opts (unLoc body)
  BindStmt _ pat body -> printPat opts (unLoc pat) ++ "<-" ++ printExpr opts (unLoc body)
  BodyStmt _ body _ _ -> printExpr opts (unLoc body)
  LetStmt _ lbinds -> "let" ++ printLocalBindsInline opts lbinds
  _ -> ppr stmt

-- | Print local bindings inline (for let in do blocks)
printLocalBindsInline :: PrintOpts -> HsLocalBinds GhcPs -> String
printLocalBindsInline opts lbinds = case lbinds of
  EmptyLocalBinds _ -> ""
  HsValBinds _ vbs -> case vbs of
    ValBinds _ bs sigs ->
      braceBlock opts (
           map (printSig opts . unLoc) sigs
        ++ map (printBind opts . unLoc) bs
      )
    XValBindsLR _ -> ""
  HsIPBinds {} -> ppr lbinds

-- | Print let bindings
printLetBinds :: PrintOpts -> HsLocalBinds GhcPs -> String
printLetBinds = printLocalBindsInline

-- | Print a tuple argument
printTupArg :: PrintOpts -> HsTupArg GhcPs -> String
printTupArg opts (Present _ e) = printExpr opts (unLoc e)
printTupArg _opts (Missing _) = ""

-- | Print arithmetic sequences
printArithSeq :: PrintOpts -> ArithSeqInfo GhcPs -> String
printArithSeq opts info = case info of
  From e -> "[" ++ printExpr opts (unLoc e) ++ "..]"
  FromThen e1 e2 -> "[" ++ printExpr opts (unLoc e1) ++ "," ++ printExpr opts (unLoc e2) ++ "..]"
  FromTo e1 e2 -> "[" ++ printExpr opts (unLoc e1) ++ ".." ++ printExpr opts (unLoc e2) ++ "]"
  FromThenTo e1 e2 e3 ->
    "[" ++ printExpr opts (unLoc e1) ++ "," ++ printExpr opts (unLoc e2) ++ ".." ++ printExpr opts (unLoc e3) ++ "]"

-- | Print an overloaded literal
printOverLit :: HsOverLit GhcPs -> String
printOverLit (OverLit _ val) = case val of
  HsIntegral il -> show (il_value il)
  HsFractional fl -> ppr fl
  HsIsString _ fs -> show fs

-- | Print a literal
printLit :: HsLit GhcPs -> String
printLit lit = case lit of
  HsChar _ c -> show c
  HsCharPrim _ c -> show c ++ "#"
  HsString _ fs -> show fs
  HsInt _ il -> show (il_value il)
  HsIntPrim _ i -> show i ++ "#"
  HsWordPrim _ i -> show i ++ "##"
  XLit x -> absurd' x
  _ -> ppr lit

-- | Print a pattern (top-level, no extra parens needed)
printPatTop :: PrintOpts -> Pat GhcPs -> String
printPatTop = printPat

-- | Print a pattern
printPat :: PrintOpts -> Pat GhcPs -> String
printPat opts pat = case pat of
  WildPat _ -> "_"
  VarPat _ lid -> pn lid
  LazyPat _ p -> "~" ++ printPatPrec opts (unLoc p)
  AsPat _ lid p -> pn lid ++ "@" ++ printPatPrec opts (unLoc p)
  ParPat _ p -> "(" ++ printPat opts (unLoc p) ++ ")"
  BangPat _ p -> "!" ++ printPatPrec opts (unLoc p)
  ListPat _ ps -> "[" ++ intercalate "," (map (printPat opts . unLoc) ps) ++ "]"
  TuplePat _ ps boxity ->
    let (open, close) = case boxity of
          Boxed -> ("(", ")")
          Unboxed -> ("(#", "#)")
    in open ++ intercalate "," (map (printPat opts . unLoc) ps) ++ close
  ConPat _ con details -> printConPatDetails opts con details
  ViewPat _ e p -> "(" ++ printExpr opts (unLoc e) ++ "->" ++ printPat opts (unLoc p) ++ ")"
  LitPat _ lit -> printLit lit
  NPat _ lit _ _ -> ppr lit
  SigPat _ p ty -> printPat opts (unLoc p) ++ "::" ++ printType opts (unLoc (hsps_body ty))
  OrPat _ ps -> intercalate ";" (map (printPat opts . unLoc) (toList' ps))
  _ -> ppr pat

-- | Print a pattern with parentheses if complex
printPatPrec :: PrintOpts -> Pat GhcPs -> String
printPatPrec opts p = case p of
  ConPat _ _ (PrefixCon _ (_:_)) -> "(" ++ printPat opts p ++ ")"
  ConPat _ _ (InfixCon _ _) -> "(" ++ printPat opts p ++ ")"
  AsPat {} -> "(" ++ printPat opts p ++ ")"
  ViewPat {} -> "(" ++ printPat opts p ++ ")"
  SigPat {} -> "(" ++ printPat opts p ++ ")"
  _ -> printPat opts p

-- | Print constructor pattern details
printConPatDetails :: PrintOpts -> XRec GhcPs (ConLikeP GhcPs) -> HsConPatDetails GhcPs -> String
printConPatDetails opts con details = case details of
  PrefixCon _tys args ->
    pn (unLoc con) ++ concatMap (\a -> " " ++ printPatPrec opts (unLoc a)) args
  RecCon recFields ->
    pn (unLoc con) ++ "{" ++ ppr recFields ++ "}"
  InfixCon p1 p2 ->
    printPatPrec opts (unLoc p1) ++ " " ++ pn (unLoc con) ++ " " ++ printPatPrec opts (unLoc p2)

-- | Print a type
printType :: PrintOpts -> HsType GhcPs -> String
printType opts ty = case ty of
  HsForAllTy _ tele body ->
    printForAllTele opts tele ++ printType opts (unLoc body)
  HsQualTy _ ctx body ->
    printContext opts (Just ctx) ++ printType opts (unLoc body)
  HsTyVar _ prom lid ->
    (if isPromotedFlag prom then "'" else "") ++ pn lid
  HsAppTy _ f x -> printType opts (unLoc f) ++ " " ++ printTypePrec opts (unLoc x)
  HsAppKindTy _ f x -> printType opts (unLoc f) ++ " @" ++ printTypePrec opts (unLoc x)
  HsFunTy _ _arr l r ->
    printType opts (unLoc l) ++ "->" ++ printType opts (unLoc r)
  HsListTy _ t -> "[" ++ printType opts (unLoc t) ++ "]"
  HsTupleTy _ sort ts ->
    let (open, close) = case sort of
          HsUnboxedTuple -> ("(#", "#)")
          _ -> ("(", ")")
    in open ++ intercalate "," (map (printType opts . unLoc) ts) ++ close
  HsSumTy _ ts ->
    "(#" ++ intercalate "|" (map (printType opts . unLoc) ts) ++ "#)"
  HsOpTy _ prom l op r ->
    printType opts (unLoc l) ++ " " ++ (if isPromotedFlag prom then "'" else "") ++ pn op ++ " " ++ printType opts (unLoc r)
  HsParTy _ t -> "(" ++ printType opts (unLoc t) ++ ")"
  HsKindSig _ t k -> printType opts (unLoc t) ++ "::" ++ printType opts (unLoc k)
  HsDocTy _ t _ -> printType opts (unLoc t)
  HsBangTy _ bang t -> printHsBang bang ++ printType opts (unLoc t)
  HsRecTy _ fields ->
    "{" ++ intercalate "," (map (printConDeclField opts . unLoc) fields) ++ "}"
  HsExplicitListTy _ prom ts ->
    (if isPromotedFlag prom then "'" else "") ++ "[" ++ intercalate "," (map (printType opts . unLoc) ts) ++ "]"
  HsExplicitTupleTy _ _prom ts ->
    "'(" ++ intercalate "," (map (printType opts . unLoc) ts) ++ ")"
  HsTyLit _ tlit -> printTyLit tlit
  HsWildCardTy _ -> "_"
  _ -> ppr ty

-- | Print a HsSigType
printSigType :: PrintOpts -> HsSigType GhcPs -> String
printSigType opts (HsSig _ _bndrs body) = printType opts (unLoc body)
printSigType _opts (XHsSigType x) = absurd' x

-- | Print a type with parentheses if complex
printTypePrec :: PrintOpts -> HsType GhcPs -> String
printTypePrec opts t = case t of
  HsAppTy {} -> "(" ++ printType opts t ++ ")"
  HsFunTy {} -> "(" ++ printType opts t ++ ")"
  HsForAllTy {} -> "(" ++ printType opts t ++ ")"
  HsQualTy {} -> "(" ++ printType opts t ++ ")"
  HsOpTy {} -> "(" ++ printType opts t ++ ")"
  _ -> printType opts t

-- | Print forall telescope
printForAllTele :: PrintOpts -> HsForAllTelescope GhcPs -> String
printForAllTele opts tele = case tele of
  HsForAllVis _ bndrs ->
    "forall " ++ unwords (map (printTyVarBndr opts . unLoc) bndrs) ++ " -> "
  HsForAllInvis _ bndrs ->
    "forall " ++ unwords (map (printTyVarBndr opts . unLoc) bndrs) ++ "."

-- | Print type variable binders
printTyVarBndrs :: PrintOpts -> LHsQTyVars GhcPs -> String
printTyVarBndrs opts (HsQTvs _ tvs) =
  concatMap (\tv -> " " ++ printTyVarBndr opts (unLoc tv)) tvs

-- | Print a single type variable binder (9.12 uses HsTvb record)
printTyVarBndr :: PrintOpts -> HsTyVarBndr flag GhcPs -> String
printTyVarBndr opts tvb = case tvb of
  HsTvb _ _ bndrVar bndrKind -> printBndrVar bndrVar bndrKind
  XTyVarBndr x -> absurd' x
  where
    printBndrVar :: HsBndrVar GhcPs -> HsBndrKind GhcPs -> String
    printBndrVar (HsBndrVar _ lid) (HsBndrNoKind _) = pn lid
    printBndrVar (HsBndrVar _ lid) (HsBndrKind _ kind) =
      "(" ++ pn lid ++ "::" ++ printType opts (unLoc kind) ++ ")"
    printBndrVar (HsBndrWildCard _) _ = "_"
    printBndrVar (XBndrVar x) _ = absurd' x

-- | Print bang type annotation
printHsBang :: HsBang -> String
printHsBang (HsBang unpk strict) =
  unpackedness ++ strictness
  where
    unpackedness = case unpk of
      SrcUnpack   -> "{-# UNPACK #-}"
      SrcNoUnpack -> "{-# NOUNPACK #-}"
      NoSrcUnpack -> ""
    strictness = case strict of
      SrcLazy    -> "~"
      SrcStrict  -> "!"
      NoSrcStrict -> ""

-- | Print a type literal
printTyLit :: HsTyLit GhcPs -> String
printTyLit tlit = case tlit of
  HsNumTy _ i -> show i
  HsStrTy _ fs -> show fs
  HsCharTy _ c -> show c

-- | Check if promoted
isPromotedFlag :: PromotionFlag -> Bool
isPromotedFlag IsPromoted = True
isPromotedFlag NotPromoted = False

-- | Format a block using {;} syntax
braceBlock :: PrintOpts -> [String] -> String
braceBlock opts items
  | poUseBraces opts = "{" ++ intercalate ";" (filter (not . null) items) ++ "}"
  | otherwise = " " ++ intercalate "; " (filter (not . null) items)

-- | Check if a string represents an operator
isOp :: String -> Bool
isOp [] = False
isOp (c:_) = not (elem c (['a'..'z'] ++ ['A'..'Z'] ++ ['_']))

-- | Print name from located thing using Outputable
pn :: Outputable a => a -> String
pn = showPprUnsafe

-- | Fallback: use GHC's Outputable
ppr :: Outputable a => a -> String
ppr = showPprUnsafe

-- | Convert NonEmpty to list
toList' :: NonEmpty a -> [a]
toList' (x :| xs) = x : xs

-- | Eliminate impossible cases
absurd' :: DataConCantHappen -> a
absurd' x = case x of {}
