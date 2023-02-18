module App where

import Control.Monad.Reader qualified as Reader
import Database.PostgreSQL.Simple qualified as Simple
import UnliftIO (MonadIO)
import UnliftIO qualified

newtype App a = App {unApp :: Reader.ReaderT Environment IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFail,
      MonadIO,
      Reader.MonadReader Environment,
      UnliftIO.MonadUnliftIO
    )

data Environment = Environment
  { connection :: Simple.Connection
  }
