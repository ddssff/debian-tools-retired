module System.Posix.EnvPlus
    ( module System.Posix.Env
    , modifyEnv
    ) where

import System.Posix.Env

-- | Generalization of Posix setEnv/unSetEnv.
modifyEnv :: String -> (Maybe String -> Maybe String) -> IO ()
modifyEnv name f =
    getEnv name >>= maybe (unsetEnv name) (\ x -> setEnv name x True) . f
