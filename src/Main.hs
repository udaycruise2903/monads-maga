{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Default (def)
import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Ema.CLI qualified
import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Ema.Route.Lib.Extra.PandocRoute qualified as PR
import Optics.Core (Prism', (%))
import Options.Applicative
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Model = Model
  { modelBaseUrl :: Text
  , modelStatic :: SR.Model
  , modelMarkdown :: PR.Model
  }
  deriving stock (Generic)

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_Blog
  | HtmlRoute_Notes
  | HtmlRoute_CV
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

deriveGeneric ''HtmlRoute
deriveIsRoute ''HtmlRoute 
  [t|
    '[ WithSubRoutes
        '[ FileRoute "index.html"
         , FileRoute "blog.html"
         , FileRoute "notes.html"
         , FileRoute "cv.html"
        ]
    ]
  |]

type StaticRoute = SR.StaticRoute "static"

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    [ -- To render a `Route` we need `Model`
      WithModel Model
    , -- Override default sub-route encoding (to avoid the folder prefix in encoded URLs)
      WithSubRoutes [HtmlRoute, StaticRoute]
    ]
    |]

instance EmaSite Route where
  type SiteArg Route = CliArgs
  siteInput cliAct args = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    markdownDyn <- 
      siteInput @PR.PandocRoute cliAct $ 
        def 
          { PR.argBaseDir = "markdown"
          , PR.argFormats = one ".md"
          --, PR.argFormats = one ".org"
          }
    pure $ Model (cliArgsBaseUrl args) <$> staticRouteDyn <*> markdownDyn
  siteOutput rp m = \case
    Route_Html r ->
      pure $ Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    H.html ! A.lang "en" $ do
      H.head $ do
        renderHead rp m r
      H.body ! A.class_ "text-stone-900 antialiased" $ do
        renderBody rp m r

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ routeTitle r <> " - Monads Maga"
  H.base ! A.href (H.toValue $ modelBaseUrl model)
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

renderNavbar :: Prism' FilePath Route -> HtmlRoute -> H.Html
renderNavbar rp currentRoute =
  H.nav ! A.class_ "w-full text-xl font-bold flex space-x-4  mb-4" $ do
    forM_ (universe @HtmlRoute) $ \r ->
      let extraClass = if r == currentRoute then "bg-rose-400 text-white" else "text-gray-700"
       in H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
            ! A.class_ ("rounded p-2 " <> extraClass)
            $ H.toHtml $ routeTitle r

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r = do
  H.div ! A.class_ "container mx-auto mt-8 p-2" $ do
    renderNavbar rp r
    H.h1 ! A.class_ "text-3xl font-bold" $ H.toHtml $ routeTitle r
    case r of
      HtmlRoute_Index -> do
        renderMarkdown model "index.md"
      HtmlRoute_Blog -> do
        renderMarkdown model "blog.md"
      HtmlRoute_Notes -> do
        renderMarkdown model "notes.md"
      HtmlRoute_CV -> do
        renderMarkdown model "cv.md"
    H.img ! A.src (staticRouteUrl rp model "logo.svg") ! A.class_ "py-4 w-32" ! A.alt "Ema Logo"
    renderFooter rp model

renderFooter :: Prism' FilePath Route -> Model -> H.Html
renderFooter rp model = do
  H.footer $
    H.div ! A.class_ "mt-16 flex flex-col items-center" $ do
      H.div ! A.class_ "mb-3 flex space-x-4" $ do
        footerLink "mail.svg" "Mail" "mailto:udaycruise2903@gmail.com"
        footerLink "github.svg" "Github" "https://github.com/udaycruise2903/"
        footerLink "twitter.svg" "Youtube" "https://twitter.com/neoatnebula"
      H.div ! A.class_ "mb-2 flex space-x-2 text-sm text-gray-500 dark:text-gray-400" $ do
        H.div "Monads Maga © 2022"
      H.div ! A.class_ "mb-8 text-sm text-gray-500 dark:text-gray-400" $ do
        "Made with "
        H.a ! A.class_ "text-rose-700" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.href "https://www.haskell.org/" $ "Haskell"
        " and "
        H.a ! A.class_ "text-rose-700" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.href "https://ema.srid.ca/" $ "Ema"
  where
    footerLink image altText url =
      H.a ! A.class_ "text-sm text-gray-500 transition h-8 v-8" ! A.target "_blank" ! A.rel "noopener noreferrer" ! A.href url $ do
        H.span ! A.class_ "sr-only" $ altText
        H.img ! A.class_ "fill-current text-gray-700 hover:border-b-2 hover:text-blue-500 dark:text-gray-200 dark:hover:text-blue-400 h-6 w-6" ! A.src (staticRouteUrl rp model image)

routeHref :: Prism' FilePath Route -> HtmlRoute -> H.AttributeValue
routeHref rp r = fromString . toString $ Ema.routeUrlWith Ema.UrlPretty rp (Route_Html r)

routeTitle :: HtmlRoute -> Text
routeTitle r = case r of
  HtmlRoute_Index -> "Home"
  HtmlRoute_Blog -> "Blog"
  HtmlRoute_Notes -> "Notes"
  HtmlRoute_CV -> "CV"

routeLink :: Prism' FilePath Route -> HtmlRoute -> H.Html -> H.Html
routeLink rp r =
  H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
    ! A.class_ "text-rose-400"

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)

-- | Render a Markdown file inside ./markdown directory
renderMarkdown :: Model -> String -> H.Html
renderMarkdown m fp =
  H.div ! A.class_ ("prose max-w-none " <> proseStyle) $ do
    renderMarkdown' m fp
  where
    -- Style Pandoc generated HTML en masse here.
    -- See https://tailwindcss.com/docs/typography-plugin
    proseStyle = "rounded p-4 text-stone-800 prose-a:underline prose-a:decoration-rose-600 prose-a:decoration-solid prose-a:decoration-1 hover:prose-a:decoration-2"

-- | Like `renderMarkdown` but without the prose styling
renderMarkdown' :: HasCallStack => Model -> String -> H.Html
renderMarkdown' m fp =
  case PR.lookupPandocRoute (modelMarkdown m) (fromString fp) of
    Nothing -> error $ "renderMarkdown: not a Pandoc ext: " <> toText fp
    Just (pandoc, render) ->
      renderRawHtml $ PR.unPandocHtml $ render pandoc
  where
    renderRawHtml =
      H.unsafeLazyByteString . encodeUtf8

-- CLI argument handling
-- ---------------------

data CliArgs = CliArgs
  { cliArgsBaseUrl :: Text
  , cliArgsEmaCli :: Ema.CLI.Cli
  }
  deriving stock (Eq, Show)

parseCliArgs :: IO CliArgs
parseCliArgs =
  execParser $ parserInfo cliParser
  where
    cliParser :: Parser CliArgs
    cliParser =
      CliArgs
        <$> (option str $ long "base-url" <> metavar "BASE_URL" <> help "Base URL to use in <base>" <> value "/")
        <*> Ema.CLI.cliParser
    parserInfo :: Parser a -> ParserInfo a
    parserInfo p =
      info
        (versionOption <*> p <**> helper)
        ( fullDesc
            <> progDesc "monads-maga: TODO"
            <> header "monads-maga"
        )
      where
        versionOption = infoOption "0.1" (long "version" <> help "Show version")

-- Main entrypoint
-- ---------------

main :: IO ()
main = do
  cliArgs <- parseCliArgs
  void $ Ema.runSiteWithCli @Route (cliArgsEmaCli cliArgs) cliArgs
