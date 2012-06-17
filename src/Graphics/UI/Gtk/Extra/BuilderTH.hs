-- | This module allows you to access Glade objects from
-- a Gtk Builder by providing the builder, the type of the
-- object and the name (the cast operation is deduced
-- automatically from the type name).
--
-- It uses Graphics.UI.Gtk.Builder.onBuilder to access a
-- glade object using TH.

module Graphics.UI.Gtk.Extra.BuilderTH
  ( gtkBuilderAccessor
  , fromBuilder
  )
 where

import Graphics.UI.Gtk.Extra.Builder (fromBuilder)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

-- | Accessor for Glade objects from Gtk Builders by name and
-- type.
gtkBuilderAccessor :: String -> String -> Q [Dec]
gtkBuilderAccessor name kind = sequenceQ
  -- Declaration
  [ sigD funcName
         -- Builder -> IO Kind
         (appT (appT arrowT (conT (mkName "Builder")))           
               (appT (conT (mkName "IO")) (conT (mkName kind))))
  -- Implementation
  , funD funcName                                                 
         -- castedOnBuilder objectName
         [clause [] (normalB (appE castedAccess                   
                                   (litE (stringL name)))) []]
  ]

  where castedAccess = appE (varE (mkName "fromBuilder")) casting
        casting      = varE (mkName ("castTo" ++ kind))
        funcName     = mkName name
