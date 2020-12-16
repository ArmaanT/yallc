{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LL.Generator where

import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
  )
import Data.Map as Map (Map, empty, insert)
import Data.Set as Set (Set, empty, insert, size, toList)
import LL.Ast
  ( Block (Block),
    Bop (..),
    Cfg,
    Cnd (..),
    Fdecl (Fdecl),
    Gid,
    Insn (Binop, Icmp),
    Lbl,
    Operand (Const, Id),
    Prog (Prog),
    Terminator (Br, Ret),
    Ty (I64),
    Uid,
  )
import QuickCheck.GenT
  ( Arbitrary (..),
    GenT,
    MonadGen (..),
    elements,
    getSize,
    oneof,
    vectorOf,
  )
import Utils ()

type Memory' = Set Int

type Variables' = Set String

type Blocks' = Set String

type Funcs' = Map Gid Fdecl

type Cfg'' = Map Lbl Block

-- local vars, functions, memory
type FuncStore = (Variables', Blocks', Funcs', Memory')

type ProgStore = Funcs'

genBop :: (MonadGen m, MonadState FuncStore m) => m Bop
genBop =
  elements
    [ Add,
      Sub,
      Mul,
      And,
      Or,
      Xor
    ]

genCnd :: (MonadGen m, MonadState FuncStore m) => m Cnd
genCnd =
  elements
    [ Eq,
      Ne,
      Slt,
      Sle,
      Sgt,
      Sge
    ]

genUidChar :: (MonadGen m, MonadState FuncStore m) => m Char
genUidChar = elements $ ['a' .. 'z'] ++ "_"

genUid :: (MonadGen m, MonadState FuncStore m) => m Uid
genUid = vectorOf 5 genUidChar

genConst :: (MonadGen m, MonadState FuncStore m) => m Int
genConst = choose (-10, 10)

genTy :: (MonadGen m, MonadState FuncStore m) => m Ty
genTy = pure I64

genOperand :: (MonadGen m, MonadState FuncStore m) => m Operand
genOperand = do
  (var, _, _, mem) <- get
  let ops = [Const <$> genConst]
  let ops' =
        if Set.size var == 0
          then ops
          else
            let uids = Set.toList var
                uidOps = Prelude.map Id uids
             in elements uidOps : ops
  oneof ops'

genInsn :: (MonadGen m, MonadState FuncStore m) => m Insn
genInsn = do
  oneof
    [ Binop <$> genBop <*> genTy <*> genOperand
        <*> genOperand,
      Icmp
        <$> genCnd
        <*> genTy
        <*> genOperand
        <*> genOperand
    ]

genUidInsn :: (MonadGen m, MonadState FuncStore m) => m (Uid, Insn)
genUidInsn = do
  insn <- genInsn
  (var, block, func, mem) <- get
  uid <- genUid
  put (Set.insert uid var, block, func, mem)
  return (uid, insn)

genInsns :: (MonadGen m, MonadState FuncStore m) => m [(Uid, Insn)]
genInsns = vectorOf 1 genUidInsn

genTerminator :: (MonadGen m, MonadState FuncStore m) => m Terminator
genTerminator = do
  (_, block, _, _) <- get
  let ret = Ret <$> genTy <*> (Just <$> genOperand)
  let terms =
        if Set.size block == 0
          then [ret]
          else
            let lbls = Set.toList block
                lblBr = Prelude.map Br lbls
             in [elements lblBr, ret]
  oneof terms

genBlock :: (MonadGen m, MonadState FuncStore m) => m Block
genBlock = do
  insns <- genInsns
  term <- genTerminator
  return $ Block insns ("_", term)

genLblBlock :: (MonadGen m, MonadState FuncStore m) => m (Lbl, Block)
genLblBlock = do
  (var, block, func, mem) <- get
  lbl <- genUid
  blk <- genBlock
  put (var, Set.insert lbl block, func, mem)
  return (lbl, blk)

genLblBlocks :: (MonadGen m, MonadState FuncStore m) => m [(Lbl, Block)]
genLblBlocks = vectorOf 1 genLblBlock

genCfg :: (MonadGen m, MonadState FuncStore m) => m Cfg
genCfg = do
  insns <- genInsns
  (locals, _, _, _) <- get
  lblblks <- genLblBlocks
  (_, block, func, mem) <- get
  put (locals, block, func, mem)
  term <- genTerminator
  return (Block insns ("_", term), lblblks)

genMainFunc :: (MonadGen m, MonadState FuncStore m) => m (Gid, Fdecl)
genMainFunc = do
  (var, block, func, mem) <- get
  let fName = "main"
  cfg <- genCfg
  let fdecl = Fdecl ([], I64) [] cfg
  put (var, block, Map.insert fName fdecl func, mem)
  return (fName, fdecl)

genFunc :: (MonadGen m, MonadState FuncStore m) => m (Gid, Fdecl)
genFunc = do
  (var, block, func, mem) <- get
  fName <- genUid
  cfg <- genCfg
  let fdecl = Fdecl ([], I64) [] cfg
  put (var, block, Map.insert fName fdecl func, mem)
  return (fName, fdecl)

genFuncs :: (MonadGen m, MonadState FuncStore m) => m [(Gid, Fdecl)]
genFuncs = vectorOf 0 genFunc

genProg :: (MonadGen m, MonadState FuncStore m) => m Prog
genProg = do
  funcs <- genFuncs
  mainFunc <- genMainFunc
  return $ Prog [] [] (mainFunc : funcs)

instance Arbitrary Prog where
  arbitrary =
    let gen = runStateT genProg (Set.empty, Set.empty, Map.empty, Set.empty)
        gen' = fst <$> gen
     in gen'
