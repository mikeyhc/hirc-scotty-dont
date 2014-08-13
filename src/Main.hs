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

instance (Monad m) => MonadState (M.Map String String) (SDMonadT m) where
    get   = SDMonad $ get
    put   = SDMonad . put
    state = SDMonad . state

instance MonadTrans SDMonadT where
    lift = SDMonad . lift . lift

instance (Functor m, Monad m, MonadIO m) 
         => HarkerClientMonad (SDMonadT m) where
    getSocket = SDMonad . lift $ getSocket
    getHandle = SDMonad . lift $ getHandle
    getIRCMsg = SDMonad . lift $ getIRCMsg
    getUser   = SDMonad . lift $ getUser
    getNick   = SDMonad . lift $ getNick
    getChan   = SDMonad . lift $ getChan
    getMMsg   = SDMonad . lift $ getMMsg
    getMsg    = SDMonad . lift $ getMsg
    getMAuth  = SDMonad . lift $ getMAuth
    getAuth   = SDMonad . lift $ getAuth
    setSocket = SDMonad . lift . setSocket
    setHandle = SDMonad . lift . setHandle
    setIRCMsg = SDMonad . lift . setIRCMsg

type SDMonad a = SDMonadT IO a

runSDMonad :: SDMonad () -> IO ()
runSDMonad (SDMonad s) = runHarkerClient (evalStateT s M.empty)

main = runPlugin "scotty-dont" "0.1.0.0" scottyDont runSDMonad

scottyDont :: SDMonad ()
scottyDont = do
    msg   <- getMsg
    if "!dont " `isPrefixOf` msg    then ifauth (remember msg)
    else if "!do " `isPrefixOf` msg then ifauth (forget msg)
                                    else checkDont

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
    mnick <- getNick
    mchan <- getChan
    case (mnick, mchan) of
        (Just nick, Just chan) -> do
            mrep <- gets (M.lookup nick)
            case mrep of
                -- bots name shoud be sent with in message
                Just rep -> if chan == "harker" then sendReply rep
                            else sendReply $ nick ++ ": " ++ rep
                _        -> return ()
        _                      -> return ()
