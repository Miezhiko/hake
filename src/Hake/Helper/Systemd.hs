module Hake.Helper.Systemd
  ( systemctl
  ) where

import           Hake.Common

systemctl ∷ [String] -> IO ()
systemctl = raw "systemctl"
