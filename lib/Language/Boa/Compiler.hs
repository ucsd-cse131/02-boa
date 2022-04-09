{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Boa.Compiler ( compiler, compile ) where

import           Text.Printf                     (printf)
import           Prelude                 hiding (compare)
import           Control.Arrow           ((>>>))
import           Control.Monad           (void)
import           Data.Maybe
import           Language.Boa.Types      hiding (Tag)
import           Language.Boa.Parser
import           Language.Boa.Normalizer (anormal)
import           Language.Boa.Asm        (asm)

--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> anormal >>> tag >>> compile >>> asm

-- | to test your compiler with code that is ALREADY in ANF comment out
--   the above definition and instead use the below:

-- compiler f = parse f >>> tag >>> compile >>> asm


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
type ABind = Bind    Tag

instance Located Tag where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: Bare -> AExp
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
-- | @compile@ a (tagged-ANF) expr into assembly
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = header
         ++ compileEnv emptyEnv e
         ++ footer

header :: [Instruction]
header = [ IPush (Reg RBP)
         , IMov  (Reg RBP) (Reg RSP)
         ]

footer :: [Instruction]
footer = [ IPop (Reg RBP)
         , IRet
         ]

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@(Number {})     = [ compileImm env v  ]

compileEnv env v@(Id {})         = [ compileImm env v  ]

compileEnv env e@(Let {})        = error "fill this in"

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 l)    = error "fill this in"

compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)

immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg env e@(Id x _)    = error "fill this in"
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " ++ show (void e)

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l


--------------------------------------------------------------------------------
-- | Compiling Primitive Operations
--------------------------------------------------------------------------------
compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1   v = error "fill this in"
compilePrim1 l env Sub1   v = error "fill this in"

compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 _ env Plus    = error "fill this in"
compilePrim2 _ env Minus   = error "fill this in"
compilePrim2 _ env Times   = error "fill this in"

--------------------------------------------------------------------------------
-- | Arithmetic
--------------------------------------------------------------------------------
arith :: Env -> AOp -> IExp -> IExp  -> [Instruction]
--------------------------------------------------------------------------------
arith env instr v1 v2
  = [ IMov  (Reg EAX) (immArg env v1)
    , instr (Reg EAX) (immArg env v2)
    ]

type AOp = Arg -> Arg -> Instruction


--------------------------------------------------------------------------------
-- | Local Variables
--------------------------------------------------------------------------------
stackVar :: Int -> Arg
--------------------------------------------------------------------------------
stackVar i = RegOffset (-8 * i) RBP

--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Int where
  repr n = Const (fromIntegral n)

instance Repr Integer where
  repr n = Const (fromIntegral n)
