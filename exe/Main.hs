{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Options.Applicative

import BCMtools.Types
import BCMtools.Convert (convert, convertOptions)
import BCMtools.View (view, viewOptions)

globalOptions :: Parser Command -> Parser BCMtoolsOptions
globalOptions cmd = BCMtoolsOptions 
             <$> argument str (metavar "INPUT") 
             <*> strOption
                   ( long "output"
                  <> short 'o'
                  <> metavar "OUTPUT" )
             <*> cmd

bcmtoolsOptions :: Parser BCMtoolsOptions
bcmtoolsOptions = subparser
    ( command "convert" ( info (globalOptions convertOptions)
                               (progDesc "file conversion") )
   <> command "view" ( info (globalOptions viewOptions)
                            (progDesc "view file") )
    )

runBCMtools :: BCMtoolsOptions -> IO () 
runBCMtools (BCMtoolsOptions input output bcmopt) = case bcmopt of
    Convert opt -> convert input output opt
    View opt -> view input output opt

main :: IO ()
main = execParser opts >>= runBCMtools
  where
    opts = info (helper <*> bcmtoolsOptions)
         ( fullDesc
        <> progDesc ""
        <> header "Big Contact Map (BCM) tools" )