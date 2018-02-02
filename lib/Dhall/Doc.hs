{-# language OverloadedStrings #-}

module Dhall.Doc where

import Control.Exception ( throwIO )
import Data.Functor ( void )
import Data.Monoid ( (<>) )

import qualified Data.Text.Lazy.Builder as TextBuilder
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Dhall hiding ( Type )
import qualified Dhall.Core as Dhall ( Const( Type ), Expr, normalize, pretty )
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Import as Dhall
import qualified Dhall.Parser as Dhall ( exprFromText )
import qualified Dhall.TypeCheck as Dhall
import qualified Text.Pandoc as Pandoc
import qualified Text.Trifecta.Delta as Trifecta


data TypeError
  = NotDocumentable
  | UnexpectedField Dhall.Text
  | UndocumentedField Dhall.Text
  deriving (Eq, Show)



typeCheckDoc
  :: Eq s => Dhall.Expr s Dhall.X -- ^ The type to document
  -> Dhall.Expr s Dhall.X -- ^ The documentation itself
  -> Either TypeError ()
typeCheckDoc t doc | documentable t =
  case (t, doc) of
    ( Expr.Record fields, Expr.RecordLit doc ) -> 
      void
        ( Map.mergeA
            ( Map.traverseMissing ( \k _ -> Left ( UnexpectedField k ) ) )
            ( Map.traverseMissing ( \k _ -> Left ( UndocumentedField k ) ) )
            ( Map.zipWithAMatched ( \_k -> typeCheckDoc ) )
            fields
            doc
        )

    _ ->
      error "typeCheckDoc was given a type that is apparently documentable, but I couldn't find a way to type check the documentation. This is a bug, please report it!"

typeCheckDoc _ _ =
  Left NotDocumentable



-- | Check if a given 'Expr' by documented.
--
-- @dhall-doc@ allows documenting any expression that normalizes to a Dhall
-- type.

documentable :: Dhall.Expr s Dhall.X -> Bool
documentable e =
  case Dhall.typeOf e of
    Right ( Expr.Const Dhall.Type ) ->
      True

    _ ->
      False



load :: FilePath -> IO (Dhall.Expr Dhall.X Dhall.X)
load filePath = do
  text <-
    LazyText.readFile filePath

  parsed <-
    case Dhall.exprFromText (Trifecta.Directed "(stdin)" 0 0 0 0) text of
      Left err ->
        throwIO err

      Right ok ->
        return ok

  loaded <-
    Dhall.load parsed

  return ( Dhall.normalize loaded )



document
  :: ( Eq s, Show s )
  => Dhall.Expr s Dhall.X
  -> Dhall.Expr s Dhall.X
  -> [ Pandoc.Block ]
document t doc =
  case (t, doc) of
    ( Expr.Record fields, Expr.RecordLit docs ) ->
      [ Pandoc.DefinitionList
          ( Map.elems ( Map.intersectionWithKey documentField fields docs ) )
      ]

    _ ->
      error $ "Could not document " <> show t <> show doc



documentField
  :: ( Eq s, Show s )
  => Dhall.Text
  -> Dhall.Expr s Dhall.X
  -> Dhall.Expr s Dhall.X
  -> ( [ Pandoc.Inline ], [ [ Pandoc.Block ] ] )
documentField name t ( Expr.TextLit doc ) | isPrimType t =
  ( [ Pandoc.Str ( LazyText.unpack name )
    , Pandoc.Str " : "
    , Pandoc.Code Pandoc.nullAttr ( LazyText.unpack ( Dhall.pretty t ) )
    ]
  , [ [ Pandoc.Para
          [ Pandoc.Str ( LazyText.unpack ( TextBuilder.toLazyText doc ) ) ]
      ]
    ]
  )
    


isPrimType :: Eq s => Dhall.Expr s Dhall.X -> Bool
isPrimType e =
  e `elem` primTy

  where

    primTy = [ Expr.Text ]
