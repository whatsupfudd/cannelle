module Cannelle.Hugo.ActionMerger where

import Control.Monad (foldM)
import Cannelle.Hugo.AST


-- *** The FileUnitElement to Statement conversion part: ***
printStatements :: [RawStatement] -> IO ()
printStatements stmts = do
  putStrLn "Statements: ["
  case stmts of
    [] -> putStrLn ""
    (h : rest) -> do
      putStrLn $ "\t" <> showAStmt 1 h
      mapM_ (\s -> putStrLn $ "\t, " <> showAStmt 1 s) rest
  putStrLn "]"

showAStmt :: Int -> RawStatement -> String
showAStmt level stmt =
  let
    nLevel = level + 1
    ident = replicate (nLevel * 2) ' '
    subIdent = replicate (level * 2) ' '
  in
  case stmt of
    ListST l -> "ListST: ["
      <> foldl (\accum s -> case accum of
          "" -> "\n  " <> ident <> showAStmt nLevel s <> "\n"
          _ -> accum <> ident <> ", " <> showAStmt nLevel s <> "\n"
          ) "" l
      <> subIdent <> "]"
    BlockST name expr bStmt ->
      "BlockST: " <> show name <> ", expr: " <> show expr
      <> ", body: " <> showAStmt nLevel bStmt
    IfST expr thenSt elseSt ->
      "IfST cond: " <> show expr
      <> "\n" <> ident <> ", then: " <> showAStmt nLevel thenSt
      <> "\n" <> ident <> ", else: " <> showAStmt nLevel elseSt
    RangeST rangeVars expr loopStmt elseStmt ->
      "RangeST rangeVars: " <> show rangeVars <> ", expr: " <> show expr
      <> "\n" <> ident <> ", loop: " <> showAStmt nLevel loopStmt
      <> "\n" <> ident <> ", else: " <> showAStmt nLevel elseStmt
    DefineST name bodyStmt ->
      "DefineST: " <> show name
      <> "\n" <> ident <> ", body: " <> showAStmt nLevel bodyStmt
    WithST expr bodyStmt elseStmt ->
      "WithST: " <> show expr
      <> "\n" <> ident <> ", body: " <> showAStmt nLevel bodyStmt
      <> "\n" <> ident <> ", else: " <> showAStmt nLevel elseStmt
    _ -> show stmt


consolidateActions :: [FileUnitElement] -> Either String [RawStatement]
consolidateActions tElements=
  let
    rez = foldM (\(accum, stack) tEle ->
      case tEle of
        Verbatim text -> case stack of
          [] -> Right (accum <> [ VerbatimST text ], stack)
          h : rest -> Right (accum, h { children = VerbatimST text : h.children } : rest)
        SourceCode text -> Left $ "@[consolidateElements] source-code still present in template elements, it should have been parsed: " <> show text
        ParsedCode action -> handleAction (accum, stack) action
      ) ([], []) tElements
  in
  case rez of
    Left err -> Left err
    Right (statements, _) -> Right statements
  where
  -- TODO: figure out the merging of all statements.
  handleAction :: ([RawStatement], [NodeGast errH typeH]) -> Action -> Either String ([RawStatement], [NodeGast errH typeH])
  handleAction state action@(ExprS expr) = pushSimpleAction state action (ExpressionST expr)
  handleAction state action@(AssignmentS assignKind var expr) = pushSimpleAction state action (VarAssignST assignKind var expr)
  handleAction (accum, stack) action@(IfS expr) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(ElseIfS elseBranch) =
    if null stack then
      Left $ "@[handleAction] empty stack on ElseIfS!"
    else
      Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(RangeS maybeRangeVars expr) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(WithS expr) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(DefineS name) = Right (accum, NodeGast action [] : stack)
  handleAction (accum, stack) action@(BlockS name expr) = Right (accum, NodeGast action [] : stack)
  handleAction state action@(TemplateIncludeS name expr) = pushSimpleAction state action (IncludeST name expr)
  handleAction state action@(PartialS name expr) = pushSimpleAction state action (PartialST name expr)
  handleAction state action@(ReturnS expr) = pushSimpleAction state action (ReturnST expr)
  handleAction (accum, stack) EndS =
    if null stack then
      Left $ "@[handleAction] empty stack on EndS!"
    else
      let
        newState = unstackNodes stack
      in
      case newState of
        Left errMsg -> Left errMsg
        Right (nStmt, nStack) ->
          case nStack of
            [] -> Right (accum <> [nStmt], [])
            h : rest -> Right (accum, h { children = nStmt : h.children } : rest)
  -- ContinueS, BreakS?
  handleAction state action = Left $ "@[handleAction] unimplemented case: " <> show action


pushSimpleAction :: ([RawStatement], [NodeGast errH typeH]) -> Action -> RawStatement -> Either String ([RawStatement], [NodeGast errH typeH])
pushSimpleAction (accum, stack) action potentialStmt =
  if null stack then
    let
      eiStmt = simpleActionToStatement action
    in
      case eiStmt of
        Left err -> Left err
        Right stmt -> Right (accum <> [stmt], [])
  else
    let
      (h:rest) = stack
    in
    simpleActionToStatement action >>= \stmt ->
      Right (accum, h { children = stmt : h.children } : rest)
  where
  simpleActionToStatement :: Action -> Either String RawStatement
  simpleActionToStatement action =
    case action of
      ExprS expr -> Right $ ExpressionST expr
      AssignmentS assignKind var expr -> Right $ VarAssignST assignKind var expr
      TemplateIncludeS name expr -> Right $ IncludeST name expr
      PartialS name expr -> Right $ PartialST name expr
      ReturnS expr -> Right $ ReturnST expr
      ContinueS -> Right ContinueST
      BreakS -> Right BreakST
      -- VerbatimS text -> Right $ VerbatimST text
      _ -> Left $ "@[simpleActionToStatement] illegal action: " <> show action


isSimpleUnstack :: Action -> Bool
isSimpleUnstack action
  | RangeS _ _ <- action = True
  | WithS _ <- action = True
  | DefineS _ <- action = True
  | BlockS _ _ <- action = True
  | IfS _ <- action = True
  | otherwise = False


unstackNodes :: [NodeGast errH typeH] -> Either String (RawStatement, [NodeGast errH typeH])
unstackNodes [] = Left "@[unstackNodes] trying to unstack an empty NodeGast stack!"
unstackNodes (topNode : rest) =
  if isSimpleUnstack topNode.action then
    let
      combinedStmts = combineChildren topNode.children
      newStmt = case topNode.action of
        RangeS rangeVars expr -> RangeST rangeVars expr combinedStmts NoOpST
        WithS expr -> WithST expr combinedStmts NoOpST
        DefineS name -> DefineST name combinedStmts
        BlockS name expr -> BlockST name expr combinedStmts
        IfS expr -> IfST expr combinedStmts NoOpST
        in
        Right (newStmt, rest)
  else
    case topNode.action of
      ElseIfS elseDetails ->
          handleElseIf elseDetails topNode.children rest
      _ -> Left $ "@[unstackNodes] unexpected node for unstacking: " <> show topNode


combineChildren :: [RawStatement] -> RawStatement
combineChildren stmts =
  case stmts of
    [] -> NoOpST
    [h] -> h
    h : rest -> ListST (reverse stmts)


matchSimpleElse :: Action -> Bool
matchSimpleElse action
  | RangeS _ _ <- action = True
  | WithS _ <- action = True
  | IfS _ <- action = True
  | otherwise = False

-- ^ takes care of unrolling the stack for an else-if node; receives details, children and stack.
handleElseIf :: ElseBranch -> [RawStatement] -> [NodeGast errH typeH] -> Either String (RawStatement, [NodeGast errH typeH])
handleElseIf _ _ [] = Left "@[unstackNodes] unexpected empty stack on ElseIfS handling!"
handleElseIf ElseB children (prevNode : rest) =
  if matchSimpleElse prevNode.action then
    let
      elseB = combineChildren children
      thenB = combineChildren prevNode.children
      builder = case prevNode.action of
        RangeS mbVarDef expr -> RangeST mbVarDef expr
        WithS expr -> WithST expr
        IfS expr -> IfST expr
    in
    Right $ (builder thenB elseB, rest)
  else case prevNode.action of
    ElseIfS (ElsePlusB _ _) ->
      handleElseContChain prevNode (combineChildren children) rest
    _ -> Left $ "@[unstackNodes] previous node in stack isn't compatible with a else node: " <> show prevNode

handleElseIf (ElsePlusB kind expr) children (prevNode : rest) =
  let
    elseB = mkContStmt kind expr (combineChildren children) NoOpST
  in
  case prevNode.action of
    IfS topExpr -> Right (IfST topExpr (combineChildren prevNode.children) elseB, rest)
    WithS topExpr -> Right (WithST topExpr (combineChildren prevNode.children) elseB, rest)
    ElseIfS _ ->
      handleElseContChain prevNode elseB rest


handleElseContChain :: NodeGast errH typeH -> RawStatement -> [NodeGast errH typeH] -> Either String (RawStatement, [NodeGast errH typeH])
handleElseContChain _ _ [] = Left "@[handleElseContChain] unexpected empty stack unrolling."
handleElseContChain thenNode elseStmt (hStack : tStack) =
  case hStack.action of
    IfS expr -> if isIfKind thenNode then
        let
          ElseIfS (ElsePlusB IfK newElseExpr) = thenNode.action
          newElseStmt = IfST newElseExpr (combineChildren thenNode.children) elseStmt
        in
        Right (IfST expr (combineChildren hStack.children) newElseStmt, tStack)
      else
        Left $ "@[handleElseContChain] alternative continuation mismatch: " <> show hStack <> " vs. " <> show thenNode
    WithS expr -> if isWithKind thenNode then
        let
          ElseIfS (ElsePlusB WithK newElseExpr) = thenNode.action
          newElseStmt = WithST newElseExpr (combineChildren thenNode.children) elseStmt
        in
        Right (WithST expr (combineChildren hStack.children) newElseStmt, tStack)
      else
        Left $ "@[handleElseContChain] alternative continuation mismatch: " <> show hStack <> " vs. " <> show thenNode
    ElseIfS (ElsePlusB kind _) -> if matchElseContKind thenNode kind then
        let
          ElseIfS (ElsePlusB kind' expr) = thenNode.action
          newElseStmt = mkContStmt kind' expr (combineChildren thenNode.children) elseStmt
        in
        handleElseContChain hStack newElseStmt tStack
      else
        Left $ "@[handleElseContChain] alternative continuation mismatch: " <> show hStack <> " vs. " <> show thenNode
    _ -> Left $ "@[handleElseContChain] unimplemented case: " <> show hStack


mkContStmt kind expr thenStmt elseStmt =
  case kind of
    IfK -> IfST expr thenStmt elseStmt
    WithK -> WithST expr thenStmt elseStmt


matchElseContKind :: NodeGast errH typeH -> PlusKind -> Bool
matchElseContKind node kind =
  case node.action of
    ElseIfS (ElsePlusB kind' _) -> kind == kind'
    _ -> False

isIfKind :: NodeGast errH typeH -> Bool
isIfKind node =
  case node.action of
    IfS _ -> True
    ElseIfS (ElsePlusB IfK _) -> True
    _ -> False


isWithKind :: NodeGast errH typeH -> Bool
isWithKind node =
  case node.action of
    WithS _ -> True
    ElseIfS (ElsePlusB WithK _) -> True
    _ -> False


mkElseIfTree :: [ (Maybe Expression, [RawStatement]) ] -> RawStatement
mkElseIfTree [] = NoOpST
mkElseIfTree (hBlocks : tBlocks) =
  case hBlocks of
    (Nothing, cStmts) -> combineChildren cStmts
    (Just expr, cStmts) -> IfST expr (combineChildren cStmts) (mkElseIfTree tBlocks)


stmtListToStatement :: [RawStatement] -> RawStatement
stmtListToStatement stmts =
  case stmts of
    [] -> NoOpST
    [h] -> h
    h : rest -> ListST stmts


fuseTwoNodes :: (RawStatement -> RawStatement -> RawStatement) -> String -> (RawStatement, RawStatement) -> Either String RawStatement
fuseTwoNodes builder builderName (prevB, elseB) =
  Right $ builder prevB elseB
