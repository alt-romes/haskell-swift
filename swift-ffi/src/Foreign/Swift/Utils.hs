module Foreign.Swift.Utils where

import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH

-- | Derive JSON instances if they don't exist yet
mayDeriveJSON :: Name -> Q [Dec]
mayDeriveJSON name = do
  typ <- getSaturatedType name

  hasToJSON <- isInstance ''ToJSON [typ]
  hasFromJSON <- isInstance ''FromJSON [typ]

  dtoJSON   <- if hasToJSON then pure []
              else deriveToJSON defaultOptions name
  dfromJSON <- if hasFromJSON then pure []
              else deriveFromJSON defaultOptions name

  return (dtoJSON ++ dfromJSON)

-- | Return the given type constructor name saturated by applying to it as many
-- new type variables as necessary.
getSaturatedType :: Name -> Q Type
getSaturatedType name = do
  kind <- reifyType name
  let tyVars (AppT a x) = a:tyVars x
      tyVars _ret = []
  tyNames <- mapM (\_ -> VarT <$> newName "a") (tyVars kind)
  let typ = foldl' AppT (ConT name) tyNames
  return typ
