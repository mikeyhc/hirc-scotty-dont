{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses
           , FlexibleInstances #-}

module Main where

import Control.Arrow (second)
import Control.Monad.State
import Control.Monad.Trans
import Data.List (isPrefixOf)
import HarkerIRC.Client
import HarkerIRC.Types
import qualified Data.Map as M

newtype SDMonadT m a = SDMonad (StateT (M.Map String String) 
                                (HarkerClientT m) a)
    deriving (Monad, MonadIO, Functor)
type SDMonad a = SDMonadT IO a

instance (Monad m) => MonadState (M.Map String String) (SDMonadT m) where
    get   = SDMonad $ get
    put   = SDMonad . put
    state = SDMonad . state

instance MonadTrans SDMonadT where
    lift = SDMonad . lift . lift

instance  HarkerClientMonad (SDMonadT IO) where
    clientLift = SDMonad . lift

runSDMonad :: SDMonad () -> IO ()
runSDMonad (SDMonad s) = runHarkerClient (evalStateT s M.empty)

main = runPlugin "scotty-dont" "0.1.0.0" scottyDont runSDMonad

scottyDont :: SDMonad ()
scottyDont = do
    msg   <- getMsg
    if msg == "!help"                 then help
    else if "!dont " `isPrefixOf` msg then ifauth (remember msg)
    else if "!do " `isPrefixOf` msg   then ifauth (forget msg)
                                      else checkDont

help :: SDMonad ()
help = sendReply "!dont user message: respond with message when user "
    >> sendReply "                    talks"
    >> sendReply "!do user:           remove the rule about user"

remember :: String -> SDMonad ()
remember msg = let (u, r) = second tail . break (==' ') $ splitmsg msg
                in if null r then sendReply "no message provided"
                             else modify (M.insert u r) 
                                   >> sendReply "response stored"

splitmsg = tail . dropWhile(/= ' ')

forget :: String -> SDMonad ()
forget msg = let u = splitmsg msg
             in  modify (M.delete u) >> sendReply (u ++ "forgotten")

checkDont :: SDMonad ()
checkDont = do
    nick <- getNick
    chan <- getChan
    mrep <- gets (M.lookup nick)
    -- bots name shoud be sent with in message
    case mrep of
        Just rep -> if chan == "harker" then sendReply rep
                    else sendReply $ nick ++ ": " ++ rep
        _        -> return ()
