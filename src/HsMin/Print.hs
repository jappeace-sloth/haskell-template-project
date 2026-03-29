module HsMin.Print
  ( printModule
  , PrintOpts(..)
  , defaultPrintOpts
  ) where

import Data.List (intercalate, intersperse)
import GHC.Hs
import GHC.Hs.Binds
import GHC.Hs.Decls
import GHC.Hs.ImpExp
import GHC.Types.Basic (TopLevelFlag(..), Origin(..))
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (Located, GenLocated(..), unLoc, getLoc)
import GHC.Types.SourceText (SourceText(..), IntegralLit(..), FractionalLit(..), StringLiteral(..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Utils.Outputable (Outputable, showPprUnsafe)
import GHC.Parser.Annotation (SrcSpanAnnA, LocatedN, EpAnn(..))
import Language.Haskell.Syntax.Basic (Boxity(..))
import Language.Haskell.Syntax.Expr
import Language.Haskell.Syntax.Pat
import Language.Haskell.Syntax.Type
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Binds
import Language.Haskell.Syntax.Decls
import Language.Haskell.Syntax.ImpExp
import Language.Haskell.Syntax.Extension (IdP)

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
    unlines' $ filter (not . null)
      [ printPragmas opts modl
      , printModuleHeader opts mname mexports
      , printImports opts imports
      , printDecls opts decls
      ]
  XModule x -> absurd x
  where
    absurd :: DataConCantHappen -> a
    absurd x = case x of {}

    unlines' :: [String] -> String
    unlines' = intercalate ";"

-- | Extract and print LANGUAGE pragmas from module annotations.
--   Since ghc-lib-parser 9.12 stores pragmas in annotations which we
--   don't have easy access to, we use the parsed extension info.
--   For now, pragmas need to be preserved from source separately.
printPragmas :: PrintOpts -> HsModule GhcPs -> String
printPragmas _opts _modl = ""

-- | Print module header: module Name (exports) where
printModuleHeader :: PrintOpts -> Maybe (LocatedN ModuleName) -> Maybe (LocatedN [LIE GhcPs]) -> String
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
  IEVar _ lname _ -> printWrappedName lname
  IEThingAbs _ lname _ -> printWrappedName lname
  IEThingAll _ lname _ -> printWrappedName lname ++ "(..)"
  IEThingWith _ lname _ pieces _ ->
    printWrappedName lname ++ "(" ++ intercalate "," (map printWrappedName pieces) ++ ")"
  IEModuleContents _ (L _ mn) -> "module " ++ moduleNameString mn
  IEGroup _ _ _ -> ""
  IEDoc _ _ -> ""
  IEDocNamed _ _ -> ""

-- | Print a wrapped name (IEWrappedName)
printWrappedName :: (Outputable (GenLocated l e)) => GenLocated l e -> String
printWrappedName = showPprUnsafe

-- | Print import declarations
printImports :: PrintOpts -> [LImportDecl GhcPs] -> String
printImports opts imports =
  intercalate ";" (map (printImport opts) imports)

-- | Print a single import declaration
printImport :: PrintOpts -> LImportDecl GhcPs -> String
printImport _opts (L _ decl) =
  "import " ++ qual ++ src ++ moduleNameString (unLoc (ideclName decl)) ++ alias ++ hiding ++ impList
  where
    qual = if ideclQualified decl /= NotQualified then "qualified " else ""
    src = if ideclSource decl then "{-# SOURCE #-} " else ""
    alias = case ideclAs decl of
      Nothing -> ""
      Just (L _ mn) -> " as " ++ moduleNameString mn
    (hiding, impList) = case ideclImportList decl of
      Nothing -> ("", "")
      Just (EverythingBut, L _ ies) ->
        (" hiding", "(" ++ intercalate "," (map (printIE defaultPrintOpts) ies) ++ ")")
      Just (Exactly, L _ ies) ->
        ("", "(" ++ intercalate "," (map (printIE defaultPrintOpts) ies) ++ ")")

-- | Print declarations
printDecls :: PrintOpts -> [LHsDecl GhcPs] -> String
printDecls opts decls =
  intercalate ";" (map (printDecl opts) decls)

-- | Print a single declaration
printDecl :: PrintOpts -> LHsDecl GhcPs -> String
printDecl opts (L _ decl) = case decl of
  TyClD _ tycl   -> printTyClDecl opts tycl
  InstD _ inst    -> printInstDecl opts inst
  DerivD _ deriv  -> printDerivDecl opts deriv
  ValD _ bind     -> printBind opts bind
  SigD _ sig      -> printSig opts sig
  KindSigD _ ksd  -> printKindSigDecl opts ksd
  DefD _ dd       -> printDefaultDecl opts dd
  ForD _ fd       -> printForeignDecl opts fd
  WarningD _ _    -> ""  -- drop warning pragmas
  AnnD _ _        -> showPprUnsafe decl  -- annotations
  RuleD _ _       -> showPprUnsafe decl  -- rewrite rules
  SpliceD _ sp    -> printSpliceDecl opts sp
  DocD _ _        -> ""  -- drop doc declarations
  RoleAnnotD _ ra -> printRoleAnnotDecl opts ra
  XHsDecl x       -> case x of {}

-- | Print type class and data declarations
printTyClDecl :: PrintOpts -> TyClDecl GhcPs -> String
printTyClDecl opts decl = case decl of
  FamDecl _ fd -> printFamilyDecl opts fd
  SynDecl _ lname tvs _fix rhs ->
    "type " ++ pn lname ++ printTyVarBndrs opts tvs ++ "=" ++ printType opts (unLoc rhs)
  DataDecl _ lname tvs _fix defn ->
    printDataDefn opts (pn lname ++ printTyVarBndrs opts tvs) defn
  ClassDecl _ ctx lname tvs _fix _fds sigs meths ats atdefs _ ->
    "class " ++ printContext opts ctx ++ pn lname ++ printTyVarBndrs opts tvs ++ " where"
    ++ braceBlock opts (
         map (printSig opts . unLoc) (bagToList' sigs)
      ++ map (printBind opts . unLoc) (bagToList' meths)
      ++ map (printFamilyDecl opts . unLoc) ats
      ++ map (showPprUnsafe) atdefs
    )

  where
    bagToList' :: [a] -> [a]
    bagToList' = id

-- | Print a family declaration
printFamilyDecl :: PrintOpts -> FamilyDecl GhcPs -> String
printFamilyDecl opts fd = showPprUnsafe fd

-- | Print a data/newtype definition
printDataDefn :: PrintOpts -> String -> HsDataDefn GhcPs -> String
printDataDefn opts header defn =
  keyword ++ header ++ ctx ++ derivs ++ " where" ++ braceBlock opts cons
  where
    keyword = case dd_cons defn of
      NewTypeCon _ -> "newtype "
      DataTypeCons False _ -> "data "
      DataTypeCons True _ -> "type data "
    ctx = printContext opts (dd_ctxt defn)
    cons = case dd_cons defn of
      NewTypeCon con -> [printConDecl opts (unLoc con)]
      DataTypeCons _ cs -> map (printConDecl opts . unLoc) cs
    derivs = concatMap (printDerivingClause opts . unLoc) (dd_derivs defn)

-- | Print a constructor declaration
printConDecl :: PrintOpts -> ConDecl GhcPs -> String
printConDecl opts con = case con of
  ConDeclGADT _ names _fix bndrs ctx args res _ ->
    intercalate "," (map pn names) ++ "::" ++ printType opts (unLoc res)
  ConDeclH98 _ lname _fix _tvs ctx details _ ->
    pn lname ++ printConDeclDetails opts details

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
  TyFamInstD _ tfid -> showPprUnsafe tfid
  DataFamInstD _ dfid -> showPprUnsafe dfid

-- | Print class instance declaration
printClsInstDecl :: PrintOpts -> ClsInstDecl GhcPs -> String
printClsInstDecl opts cid =
  "instance " ++ printOverlapPragma (cid_overlap cid)
  ++ printType opts (unLoc (cid_poly_ty cid))
  ++ " where" ++ braceBlock opts (
       map (printBind opts . unLoc) binds
    ++ map (printSig opts . unLoc) sigs
  )
  where
    binds = cid_binds cid
    sigs = cid_sigs cid

-- | Print overlap pragma
printOverlapPragma :: Maybe (LocatedN OverlapMode) -> String
printOverlapPragma Nothing = ""
printOverlapPragma (Just (L _ mode)) = case mode of
  NoOverlap _    -> ""
  Overlappable _ -> "{-# OVERLAPPABLE #-} "
  Overlapping _  -> "{-# OVERLAPPING #-} "
  Overlaps _     -> "{-# OVERLAPS #-} "
  Incoherent _   -> "{-# INCOHERENT #-} "

-- | Print a deriving declaration
printDerivDecl :: PrintOpts -> DerivDecl GhcPs -> String
printDerivDecl opts dd = showPprUnsafe dd

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
        ViaStrategy ty -> ("", " via " ++ printType opts (unLoc (unLoc ty)))
    printDerivClauseTys (L _ dct) = case dct of
      DctSingle _ ty -> printType opts (unLoc (dropWildCards ty))
      DctMulti _ tys -> "(" ++ intercalate "," (map (printType opts . unLoc . dropWildCards) tys) ++ ")"

-- | Print standalone kind signatures
printKindSigDecl :: PrintOpts -> StandaloneKindSig GhcPs -> String
printKindSigDecl opts ksd = showPprUnsafe ksd

-- | Print default declarations
printDefaultDecl :: PrintOpts -> DefaultDecl GhcPs -> String
printDefaultDecl opts dd = showPprUnsafe dd

-- | Print foreign declarations
printForeignDecl :: PrintOpts -> ForeignDecl GhcPs -> String
printForeignDecl opts fd = showPprUnsafe fd

-- | Print role annotations
printRoleAnnotDecl :: PrintOpts -> RoleAnnotDecl GhcPs -> String
printRoleAnnotDecl opts ra = showPprUnsafe ra

-- | Print splice declarations
printSpliceDecl :: PrintOpts -> SpliceDecl GhcPs -> String
printSpliceDecl opts sd = showPprUnsafe sd

-- | Print a context (class constraints)
printContext :: PrintOpts -> Maybe (LHsContext GhcPs) -> String
printContext _opts Nothing = ""
printContext opts (Just (L _ [])) = ""
printContext opts (Just (L _ [ct])) = printType opts (unLoc ct) ++ "=>"
printContext opts (Just (L _ cts)) =
  "(" ++ intercalate "," (map (printType opts . unLoc) cts) ++ ")=>"

-- | Print a binding (function definition, pattern binding, etc.)
printBind :: PrintOpts -> HsBind GhcPs -> String
printBind opts bind = case bind of
  FunBind _ lid matches ->
    printMatchGroup opts (pn lid) matches
  PatBind _ pat rhs _ ->
    printPat opts (unLoc pat) ++ printGRHSs opts "=" rhs
  VarBind {} -> showPprUnsafe bind
  PatSynBind _ psb -> printPatSynBind opts psb
  XHsBindLR x -> case x of {}

-- | Print a pattern synonym binding
printPatSynBind :: PrintOpts -> PatSynBind GhcPs GhcPs -> String
printPatSynBind opts psb = showPprUnsafe psb

-- | Print a signature
printSig :: PrintOpts -> Sig GhcPs -> String
printSig opts sig = case sig of
  TypeSig _ names ty ->
    intercalate "," (map pn names) ++ "::" ++ printType opts (unLoc (dropWildCards (hswc_body ty)))
  PatSynSig _ names ty ->
    "pattern " ++ intercalate "," (map pn names) ++ "::" ++ printType opts (unLoc (sig_body ty))
  ClassOpSig _ deflt names ty ->
    (if deflt then "default " else "") ++ intercalate "," (map pn names) ++ "::" ++ printType opts (unLoc (sig_body ty))
  FixSig _ (FixitySig _ names fixity) ->
    showPprUnsafe fixity ++ " " ++ intercalate "," (map pn names)
  InlineSig _ name ipragma ->
    showPprUnsafe sig
  SpecSig _ name tys ipragma ->
    showPprUnsafe sig
  SpecInstSig _ ty ->
    showPprUnsafe sig
  MinimalSig _ bf ->
    showPprUnsafe sig
  SCCFunSig _ name mstr ->
    showPprUnsafe sig
  CompleteMatchSig _ names mty ->
    showPprUnsafe sig
  XSig x -> case x of {}

-- | Print match groups (for function definitions)
printMatchGroup :: PrintOpts -> String -> MatchGroup GhcPs (LHsExpr GhcPs) -> String
printMatchGroup opts fname (MG _ (L _ matches)) =
  intercalate ";" (map (printMatch opts fname) matches)

-- | Print a single match (one equation)
printMatch :: PrintOpts -> String -> LMatch GhcPs (LHsExpr GhcPs) -> String
printMatch opts fname (L _ (Match _ ctx pats rhs)) = case ctx of
  FunRhs _ _ _ ->
    fname ++ concatMap (\p -> " " ++ printPatPrec opts p) pats ++ printGRHSs opts "=" rhs
  CaseAlt _ ->
    intercalate " " (map (printPatTop opts) pats) ++ printGRHSs opts "->" rhs
  LamAlt _ ->
    intercalate " " (map (printPatPrec opts) pats) ++ printGRHSs opts "->" rhs
  LambdaExpr ->
    intercalate " " (map (printPatPrec opts) pats) ++ printGRHSs opts "->" rhs
  _ -> showPprUnsafe (Match noAnn ctx pats rhs)

-- | Print GRHSs (guarded right-hand sides + where clause)
printGRHSs :: PrintOpts -> String -> GRHSs GhcPs (LHsExpr GhcPs) -> String
printGRHSs opts sep (GRHSs _ grhss binds) =
  concatMap (printGRHS opts sep) grhss ++ printLocalBinds opts binds

-- | Print a single GRHS
printGRHS :: PrintOpts -> String -> LGRHS GhcPs (LHsExpr GhcPs) -> String
printGRHS opts sep (L _ (GRHS _ guards body)) = case guards of
  [] -> sep ++ printExpr opts (unLoc body)
  gs -> "|" ++ intercalate "," (map (printStmt opts . unLoc) gs) ++ sep ++ printExpr opts (unLoc body)

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
  HsIPBinds _ _ -> showPprUnsafe binds

-- | Print an expression
printExpr :: PrintOpts -> HsExpr GhcPs -> String
printExpr opts expr = case expr of
  HsVar _ lid -> pn lid
  HsUnboundVar _ rn -> pn' rn
  HsOverLit _ lit -> printOverLit opts lit
  HsLit _ lit -> printLit opts lit
  HsLam _ variant mg -> printLamExpr opts variant mg
  HsApp _ f x -> printExpr opts (unLoc f) ++ " " ++ printExprPrec opts (unLoc x)
  HsAppType _ e _ -> printExpr opts (unLoc e) ++ " @" ++ "..."  -- type application
  OpApp _ l op r ->
    printExpr opts (unLoc l) ++ opStr ++ printExpr opts (unLoc r)
    where
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
  ExplicitSum _ tag arity e ->
    "(#" ++ replicate (tag - 1) ',' ++ " " ++ printExpr opts (unLoc e) ++ " " ++ replicate (fromIntegral arity - fromIntegral tag) ',' ++ "#)"
  HsCase _ scrut mg ->
    "case " ++ printExpr opts (unLoc scrut) ++ " of" ++ braceBlock opts (printCaseAlts opts mg)
  HsIf _ cond t f ->
    "if " ++ printExpr opts (unLoc cond) ++ " then " ++ printExpr opts (unLoc t) ++ " else " ++ printExpr opts (unLoc f)
  HsMultiIf _ grhss ->
    "if" ++ braceBlock opts (map (printGRHS opts "->") grhss)
  HsLet _ binds body ->
    "let" ++ printLetBinds opts binds ++ " in " ++ printExpr opts (unLoc body)
  HsDo _ flavour (L _ stmts) ->
    printDoExpr opts flavour stmts
  ExplicitList _ exprs ->
    "[" ++ intercalate "," (map (printExpr opts . unLoc) exprs) ++ "]"
  RecordCon _ con fields ->
    pn con ++ "{" ++ printRecFields opts fields ++ "}"
  RecordUpd _ e fields ->
    printExpr opts (unLoc e) ++ "{" ++ printRecUpdFields opts fields ++ "}"
  ExprWithTySig _ e ty ->
    printExpr opts (unLoc e) ++ "::" ++ printType opts (unLoc (dropWildCards (hswc_body ty)))
  ArithSeq _ _ info -> printArithSeq opts info
  HsTypedBracket _ e -> "[||" ++ printExpr opts (unLoc e) ++ "||]"
  HsUntypedBracket _ q -> printQuote opts q
  HsTypedSplice _ e -> "$$(" ++ printExpr opts (unLoc e) ++ ")"
  HsUntypedSplice _ s -> printUntypedSplice opts s
  HsProc _ p cmd -> "proc " ++ printPat opts (unLoc p) ++ "->" ++ showPprUnsafe cmd
  HsStatic _ e -> "static " ++ printExpr opts (unLoc e)
  HsGetField _ e (L _ fld) -> printExpr opts (unLoc e) ++ "." ++ showPprUnsafe fld
  HsProjection _ flds -> "(" ++ concatMap (\f -> "." ++ showPprUnsafe f) flds ++ ")"
  HsOverLabel _ fl -> "#" ++ showPprUnsafe fl
  HsIPVar _ ip -> "?" ++ showPprUnsafe ip
  HsPragE _ _ e -> printExpr opts (unLoc e)
  HsEmbTy _ _ -> showPprUnsafe expr
  HsForAll _ _ _ -> showPprUnsafe expr
  HsQual _ _ _ -> showPprUnsafe expr
  HsFunArr _ _ _ _ -> showPprUnsafe expr
  XExpr x -> case x of {}

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
    printLamMatch (L _ (Match _ _ pats rhs)) =
      intercalate " " (map (printPatPrec opts) pats) ++ printGRHSs opts "->" rhs

-- | Print case alternatives from a MatchGroup
printCaseAlts :: PrintOpts -> MatchGroup GhcPs (LHsExpr GhcPs) -> [String]
printCaseAlts opts (MG _ (L _ matches)) =
  map printAlt matches
  where
    printAlt (L _ (Match _ _ pats rhs)) =
      intercalate " " (map (printPatTop opts) pats) ++ printGRHSs opts "->" rhs

-- | Print do-expression
printDoExpr :: PrintOpts -> HsDoFlavour -> [ExprLStmt GhcPs] -> String
printDoExpr opts flavour stmts =
  keyword ++ braceBlock opts (map (printStmt opts . unLoc) stmts)
  where
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
  LastBodyStmt _ body _ _ -> printExpr opts (unLoc body)
  BindStmt _ pat body -> printPat opts (unLoc pat) ++ "<-" ++ printExpr opts (unLoc body)
  BodyStmt _ body _ _ -> printExpr opts (unLoc body)
  LetStmt _ binds -> "let" ++ printLocalBindsInline opts binds
  ParStmt _ blocks _ _ -> showPprUnsafe stmt
  TransStmt {} -> showPprUnsafe stmt
  RecStmt {} -> showPprUnsafe stmt
  XStmtLR x -> case x of {}

-- | Print local bindings inline (for let in do blocks)
printLocalBindsInline :: PrintOpts -> HsLocalBinds GhcPs -> String
printLocalBindsInline opts binds = case binds of
  EmptyLocalBinds _ -> ""
  HsValBinds _ vbs -> case vbs of
    ValBinds _ bs sigs ->
      braceBlock opts (
           map (printSig opts . unLoc) sigs
        ++ map (printBind opts . unLoc) bs
      )
    XValBindsLR _ -> ""
  HsIPBinds _ _ -> showPprUnsafe binds

-- | Print let bindings
printLetBinds :: PrintOpts -> HsLocalBinds GhcPs -> String
printLetBinds = printLocalBindsInline

-- | Print a tuple argument
printTupArg :: PrintOpts -> HsTupArg GhcPs -> String
printTupArg opts (Present _ e) = printExpr opts (unLoc e)
printTupArg _opts (Missing _) = ""

-- | Print record fields
printRecFields :: PrintOpts -> HsRecordBinds GhcPs -> String
printRecFields opts (HsRecFields fields dotdot) =
  intercalate "," (map (printRecField opts . unLoc) fields ++ dots)
  where
    dots = case dotdot of
      Nothing -> []
      Just _ -> [".."]

-- | Print a record field binding
printRecField :: PrintOpts -> HsFieldBind (LocatedN (FieldOcc GhcPs)) (LHsExpr GhcPs) -> String
printRecField opts (HsFieldBind _ lbl arg pun)
  | pun = pn lbl
  | otherwise = pn lbl ++ "=" ++ printExpr opts (unLoc arg)

-- | Print record update fields
printRecUpdFields :: PrintOpts -> LHsRecUpdFields GhcPs -> String
printRecUpdFields opts fields = showPprUnsafe fields

-- | Print arithmetic sequences
printArithSeq :: PrintOpts -> ArithSeqInfo GhcPs -> String
printArithSeq opts info = case info of
  From e -> "[" ++ printExpr opts (unLoc e) ++ "..]"
  FromThen e1 e2 -> "[" ++ printExpr opts (unLoc e1) ++ "," ++ printExpr opts (unLoc e2) ++ "..]"
  FromTo e1 e2 -> "[" ++ printExpr opts (unLoc e1) ++ ".." ++ printExpr opts (unLoc e2) ++ "]"
  FromThenTo e1 e2 e3 ->
    "[" ++ printExpr opts (unLoc e1) ++ "," ++ printExpr opts (unLoc e2) ++ ".." ++ printExpr opts (unLoc e3) ++ "]"

-- | Print quotes
printQuote :: PrintOpts -> HsQuote GhcPs -> String
printQuote opts q = showPprUnsafe q

-- | Print untyped splices
printUntypedSplice :: PrintOpts -> HsUntypedSplice GhcPs -> String
printUntypedSplice opts s = showPprUnsafe s

-- | Print an overloaded literal
printOverLit :: PrintOpts -> HsOverLit GhcPs -> String
printOverLit _opts (OverLit _ val) = case val of
  HsIntegral il -> show (il_value il)
  HsFractional fl -> showPprUnsafe fl
  HsIsString _ fs -> show fs

-- | Print a literal
printLit :: PrintOpts -> HsLit GhcPs -> String
printLit _opts lit = case lit of
  HsChar _ c -> show c
  HsCharPrim _ c -> show c ++ "#"
  HsString _ fs -> show fs
  HsStringPrim _ _ -> showPprUnsafe lit
  HsInt _ il -> show (il_value il)
  HsIntPrim _ i -> show i ++ "#"
  HsWordPrim _ i -> show i ++ "##"
  HsInt8Prim _ i -> show i
  HsInt16Prim _ i -> show i
  HsInt32Prim _ i -> show i
  HsInt64Prim _ i -> show i
  HsWord8Prim _ i -> show i
  HsWord16Prim _ i -> show i
  HsWord32Prim _ i -> show i
  HsWord64Prim _ i -> show i
  HsFloatPrim _ fl -> showPprUnsafe fl
  HsDoublePrim _ fl -> showPprUnsafe fl
  XLit x -> case x of {}

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
  SumPat _ p tag arity ->
    "(#" ++ replicate (tag - 1) ',' ++ " " ++ printPat opts (unLoc p) ++ " " ++ replicate (fromIntegral arity - fromIntegral tag) ',' ++ "#)"
  ConPat _ con details -> printConPatDetails opts con details
  ViewPat _ e p -> "(" ++ printExpr opts (unLoc e) ++ "->" ++ printPat opts (unLoc p) ++ ")"
  SplicePat _ sp -> showPprUnsafe sp
  LitPat _ lit -> printLit opts lit
  NPat _ lit _ _ -> showPprUnsafe lit
  NPlusKPat _ n k _ _ _ -> pn n ++ "+" ++ showPprUnsafe k
  SigPat _ p ty -> printPat opts (unLoc p) ++ "::" ++ printType opts (unLoc (sig_body (unLoc ty)))
  OrPat _ ps -> intercalate ";" (map (printPat opts . unLoc) ps)
  XPat x -> showPprUnsafe pat
  InvisPat _ _ -> showPprUnsafe pat
  EmbTyPat _ _ -> showPprUnsafe pat

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
printConPatDetails :: PrintOpts -> LocatedN RdrName -> HsConPatDetails GhcPs -> String
printConPatDetails opts con details = case details of
  PrefixCon tys args ->
    pn con ++ concatMap (\a -> " " ++ printPatPrec opts (unLoc a)) args
  RecCon (HsRecFields fields dotdot) ->
    pn con ++ "{" ++ intercalate "," (map (printPatField opts . unLoc) fields ++ dots) ++ "}"
    where
      dots = case dotdot of
        Nothing -> []
        Just _ -> [".."]
  InfixCon p1 p2 ->
    printPatPrec opts (unLoc p1) ++ " " ++ pn con ++ " " ++ printPatPrec opts (unLoc p2)

-- | Print a pattern field binding
printPatField :: PrintOpts -> HsFieldBind (LocatedN (FieldOcc GhcPs)) (LPat GhcPs) -> String
printPatField opts (HsFieldBind _ lbl pat pun)
  | pun = pn lbl
  | otherwise = pn lbl ++ "=" ++ printPat opts (unLoc pat)

-- | Print a type
printType :: PrintOpts -> HsType GhcPs -> String
printType opts ty = case ty of
  HsForAllTy _ tele body ->
    printForAllTele opts tele ++ printType opts (unLoc body)
  HsQualTy _ ctx body ->
    printContext opts (Just ctx) ++ printType opts (unLoc body)
  HsTyVar _ prom lid ->
    (if isPromoted prom then "'" else "") ++ pn lid
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
    printType opts (unLoc l) ++ " " ++ (if isPromoted prom then "'" else "") ++ pn op ++ " " ++ printType opts (unLoc r)
  HsParTy _ t -> "(" ++ printType opts (unLoc t) ++ ")"
  HsIParamTy _ ip t -> "?" ++ showPprUnsafe ip ++ "::" ++ printType opts (unLoc t)
  HsKindSig _ t k -> printType opts (unLoc t) ++ "::" ++ printType opts (unLoc k)
  HsSpliceTy _ sp -> showPprUnsafe sp
  HsDocTy _ t _ -> printType opts (unLoc t)
  HsBangTy _ bang t -> printBangType bang ++ printType opts (unLoc t)
  HsRecTy _ fields ->
    "{" ++ intercalate "," (map (printConDeclField opts . unLoc) fields) ++ "}"
  HsExplicitListTy _ prom ts ->
    (if isPromoted prom then "'" else "") ++ "[" ++ intercalate "," (map (printType opts . unLoc) ts) ++ "]"
  HsExplicitTupleTy _ ts ->
    "'(" ++ intercalate "," (map (printType opts . unLoc) ts) ++ ")"
  HsTyLit _ lit -> printTyLit lit
  HsWildCardTy _ -> "_"
  HsStarTy _ _ -> "*"
  XHsType x -> showPprUnsafe ty

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

-- | Print a single type variable binder
printTyVarBndr :: PrintOpts -> HsTyVarBndr flag GhcPs -> String
printTyVarBndr opts (UserTyVar _ _ lid) = pn lid
printTyVarBndr opts (KindedTyVar _ _ lid kind) =
  "(" ++ pn lid ++ "::" ++ printType opts (unLoc kind) ++ ")"

-- | Print bang type annotation
printBangType :: HsSrcBang -> String
printBangType (HsSrcBang _ unpk strict) =
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
printTyLit lit = case lit of
  HsNumTy _ i -> show i
  HsStrTy _ fs -> show fs
  HsCharTy _ c -> show c

-- | Check if promoted
isPromoted :: PromotionFlag -> Bool
isPromoted IsPromoted = True
isPromoted NotPromoted = False

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

-- | Print a RdrName directly
pn' :: RdrName -> String
pn' = showPprUnsafe

-- | Get the annotation (placeholder)
noAnn :: EpAnn a
noAnn = EpAnnNotUsed

-- | Helper to drop wildcard wrapper
dropWildCards :: HsWildCardBndrs GhcPs (LHsType GhcPs) -> LHsType GhcPs
dropWildCards = hswc_body
