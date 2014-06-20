module Paths_roguenads where

--This module is a hack so that it is possible to use ghci on separate files.
--See neilmitchell.blogspot.com/2008/02/adding-data-files-using-cabal.html
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
