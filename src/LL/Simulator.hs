{-# LANGUAGE FlexibleContexts #-}

module LL.Simulator where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.State (MonadState (get, put), gets, runState)
import Data.Bits (Bits (shiftL, shiftR, xor, (.&.), (.|.)))
import Data.Map as Map
  ( Map,
    empty,
    findWithDefault,
    fromList,
    insert,
    (!),
  )
import LL.Ast
  ( Block (Block),
    Bop (..),
    Cnd (..),
    Fdecl (fCfg, fParam),
    Gdecl,
    Gid,
    Ginit (..),
    Insn (..),
    Lbl,
    Operand (..),
    Prog (Prog),
    Terminator (..),
    Tid,
    Ty (Array, I1, I64, Namedt, Ptr),
    Uid,
  )

-- Map of memory location to value
type Memory = Map Int Int

type Variables = Map String Int

type Funcs = Map Gid Fdecl

type Cfg' = Map Lbl Block

type Tys = Map Tid Ty

-- local vars, global vars, functions, memory, mem counter
type Store = (Tys, Variables, Variables, Funcs, Memory, Int)

-- | Get the size of a type
sizeOf :: Tys -> Ty -> Int
sizeOf _ I64 = 1
sizeOf _ (Ptr _) = 1
sizeOf _ I1 = 1
sizeOf tys (Array i ty) = i * sizeOf tys ty
sizeOf tys (Namedt tid) = sizeOf tys (tys Map.! tid)
sizeOf _ _ = 0

-- | Break a type to get the subtype
breakTy :: Tys -> Ty -> Ty
breakTy _ (Array _ t) = t
breakTy tys (Namedt tid) = breakTy tys (tys Map.! tid)
breakTy _ _ = error "bad"

-- | Lookup an operand
lookupOperand :: Operand -> Store -> Int
lookupOperand Null _ = 0
lookupOperand (Const i) _ = i
lookupOperand (Gid gid) (_, _, global, _, _, _) = global Map.! gid
lookupOperand (Id id) (_, local, _, _, _, _) = local Map.! id

-- | Insert a Ginit
insertGinit :: Tys -> Int -> Ginit -> Memory -> Memory
insertGinit _ ptr GNull mem = Map.insert ptr 0 mem
insertGinit _ ptr (GInt i) mem = Map.insert ptr i mem
insertGinit tys ptr (GArray ((ty, ginit) : xs)) mem =
  let mem' = insertGinit tys (ptr + sizeOf tys ty) (GArray xs) mem
      mem'' = insertGinit tys ptr ginit mem'
   in mem''
insertGinit _ ptr (GArray []) mem = mem

-- | Load globals
loadGlobals :: Tys -> Int -> [(Gid, Gdecl)] -> (Variables, Memory, Int)
loadGlobals _ i [] = (Map.empty, Map.empty, i)
loadGlobals tys i ((gid, (t, ginit)) : xs) =
  let ptr = i + sizeOf tys t
      (var, mem, int) = loadGlobals tys ptr xs
      var' = Map.insert gid i var
      mem' = insertGinit tys i ginit mem
   in (var', mem', int)

-- | Get the location in memory to start global definitions
globalStart :: Tys -> Int -> [(Gid, Gdecl)] -> Int
globalStart _ i [] = i
globalStart tys i ((_, (ty, _)) : xs) = globalStart tys (i - sizeOf tys ty) xs

-- | Simulate an LL program
simulate :: Prog -> Either String Int
simulate (Prog tdecls gdecls fdecls) =
  let fdecs = Map.fromList fdecls
      tys = Map.fromList tdecls
      args = Prelude.foldr (`Map.insert` 0) Map.empty ["argc", "argv"]
      sim = execFunction (fdecs Map.! "main")
      globalAddr = globalStart tys 0x7FFF gdecls
      (globals, memory, _) = loadGlobals tys globalAddr gdecls
      state = (tys, args, globals, fdecs, memory, globalAddr)
      (result, _) = runState (runExceptT sim) state
   in result

-- | Execute a function
execFunction :: (MonadError String m, MonadState Store m) => Fdecl -> m Int
execFunction fdec =
  let cfg = fCfg fdec
      cfg' = Prelude.foldr (uncurry Map.insert) Map.empty $ snd cfg
   in execBlock (fst cfg) cfg'

-- | Execute a block
execBlock :: (MonadError String m, MonadState Store m) => Block -> Cfg' -> m Int
execBlock (Block [] (_, term)) cfg = execTerminator term cfg
execBlock (Block (x : xs) term) cfg = do
  execInstruction x
  execBlock (Block xs term) cfg

-- | Translate a Bop to a function
translateBop :: Bop -> (Int -> Int -> Int)
translateBop Add = (+)
translateBop Sub = (-)
translateBop Mul = (*)
translateBop Shl = shiftL
translateBop Lshr = shiftR
translateBop Ashr = shiftR
translateBop And = (.&.)
translateBop Or = (.|.)
translateBop Xor = xor

-- | Translate a Cnd to a function
translateCnd :: Cnd -> (Int -> Int -> Bool)
translateCnd Eq = (==)
translateCnd Ne = (/=)
translateCnd Slt = (<)
translateCnd Sle = (<=)
translateCnd Sgt = (>)
translateCnd Sge = (>=)

-- | Execute an instruction
execInstruction :: (MonadError String m, MonadState Store m) => (Uid, Insn) -> m ()
execInstruction (u, Binop b _ o1 o2) = do
  s@(tys, local, global, funcs, mem, ctr) <- get
  let local' = Map.insert u (translateBop b (lookupOperand o1 s) (lookupOperand o2 s)) local
  put (tys, local', global, funcs, mem, ctr)
execInstruction (u, Alloca t) = do
  (tys, local, global, funcs, mem, ctr) <- get
  let ctr' = ctr - sizeOf tys t
  let local' = Map.insert u ctr' local
  put (tys, local', global, funcs, mem, ctr')
  return ()
execInstruction (u, Load _ (Id i)) = do
  (tys, local, global, funcs, mem, ctr) <- get
  let loc = local Map.! i
  let local' = Map.insert u (mem Map.! loc) local
  put (tys, local', global, funcs, mem, ctr)
execInstruction (u, Load _ (Gid g)) = do
  (tys, local, global, funcs, mem, ctr) <- get
  let loc = global Map.! g
  let val = Map.findWithDefault 0 loc mem
  let local' = Map.insert u val local
  put (tys, local', global, funcs, mem, ctr)
execInstruction (u, Load _ _) = throwError "bad load"
execInstruction (_, Store _ o1 (Id id)) = do
  s@(tys, local, global, funcs, mem, ctr) <- get
  let loc = local Map.! id
  let mem' = Map.insert loc (lookupOperand o1 s) mem
  put (tys, local, global, funcs, mem', ctr)
execInstruction (_, Store _ o1 (Gid gid)) = do
  s@(tys, local, global, funcs, mem, ctr) <- get
  let loc = global Map.! gid
  let mem' = Map.insert loc (lookupOperand o1 s) mem
  put (tys, local, global, funcs, mem', ctr)
execInstruction (_, Store {}) = throwError "bad store"
execInstruction (u, Icmp c _ o1 o2) = do
  s@(tys, local, global, funcs, mem, ctr) <- get
  let local' = Map.insert u (if translateCnd c (lookupOperand o1 s) (lookupOperand o2 s) then 1 else 0) local
  put (tys, local', global, funcs, mem, ctr)
execInstruction (u, Call _ (Gid name) args) = do
  s@(tys, local, global, funcs, mem, ctr) <- get
  let fdec = funcs Map.! name
  let params = fParam fdec
  let namedParams = zipWith (\x (_, o) -> (x, lookupOperand o s)) params args
  let local' = Prelude.foldr (uncurry Map.insert) Map.empty namedParams
  put (tys, local', global, funcs, mem, ctr)
  r <- execFunction fdec
  let local'' = Map.insert u r local
  put (tys, local'', global, funcs, mem, ctr)
execInstruction (u, Call _ o _) = throwError $ "invalid function call: " ++ show o
execInstruction (u, Bitcast _ o _) = do
  s@(tys, local, global, funcs, mem, ctr) <- get
  let local' = Map.insert u (lookupOperand o s) local
  put (tys, local', global, funcs, mem, ctr)
execInstruction (u, Gep t (Gid g) b) = do
  s@(tys, local, global, funcs, mem, ctr) <- get
  let loc = global Map.! g
  let loop :: Ty -> [Operand] -> Int -> Int
      loop _ [] i = i
      loop ty (o : xs) i = loop (breakTy tys ty) xs (lookupOperand o s * sizeOf tys ty + i)
  let loc' = loop t b loc
  let local' = Map.insert u loc' local
  put (tys, local', global, funcs, mem, ctr)
execInstruction (_, Gep {}) = throwError "invalid GEP"

-- | Execute a terminator
execTerminator :: (MonadError String m, MonadState Store m) => Terminator -> Cfg' -> m Int
execTerminator (Ret _ (Just o)) _ = do gets (lookupOperand o)
execTerminator (Ret _ Nothing) _ = return 0
execTerminator (Br lbl) cfg = execBlock (cfg Map.! lbl) cfg
execTerminator (Cbr o l1 l2) cfg =
  do
    state <- get
    let i = lookupOperand o state
    if i == 1 then execBlock (cfg Map.! l1) cfg else execBlock (cfg Map.! l2) cfg
