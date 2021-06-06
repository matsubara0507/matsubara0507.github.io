{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Orphan                     ()

import           Control.Monad
import           Data.Extensible
import           Data.Functor.Identity      (Identity)
import           Data.Time
import           Development.Shake
import           Development.Shake.Classes  ()
import           Development.Shake.FilePath
import           Development.Shake.Forward
import           Lens.Micro
import           Skylighting                (pygments, styleToCss)
import           Slick
import           Text.Mustache              (Template, ToMustache)

import qualified Data.HashMap.Lazy          as HML
import qualified Data.List                  as L
import qualified Data.Text                  as T

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta
    = #siteTitle   @= "ひげメモ"
   <: #domain      @= "https://matsubara0507.github.io"
   <: #author      @= "MATSUBARA Nobutada"
   <: #description @= "自分用のメモ書きだったり，イロイロといじって遊ぶようだったり"
   <: #twitter     @= Nothing
   <: #github      @= Just "matsubara0507"
   <: nil

outputFolder :: FilePath
outputFolder = "docs/"

type SiteMeta = Record
  '[ "siteTitle"   >: String
   , "domain"      >: String
   , "author"      >: String
   , "description" >: String
   , "twitter"     >: Maybe String
   , "github"      >: Maybe String
   ]

type IndexInfo = Record
  '[ "tags"  >: [Record '[ "name" >: Tag, "size" >: Int]]
   , "posts" >: [Post]
   ]

type PagenationInfoParams =
  '[ "posts"       >: [Post]
   , "prevPageNum" >: Maybe Int
   , "nextPageNum" >: Maybe Int
   ]

type Tag = String

type Post = Record ('[ "date" >: String, "url" >: String ] ++ FrontMatterParams)

type FrontMatterParams =
  '[ "title"   >: String
   , "tags"    >: [Tag]
   , "image"   >: Maybe String
   , "content" >: String
   ]

type AtomData = Record
  '[ "siteTitle"   >: String
   , "domain"      >: String
   , "author"      >: String
   , "posts"       >: [Post]
   , "currentTime" >: String
   , "atomUrl"     >: String
   ]

buildIndex :: [(Tag, Int)] -> [Post] -> Action ()
buildIndex tags posts = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexHTML = T.unpack $ substitute indexT (happend siteMeta indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML
  where
    indexInfo = #tags @= tagsInfo <: #posts @= take 4 (reverse posts) <: nil :: IndexInfo

    tagsInfo = map (uncurry toTagInfo) (L.sortOn fst tags)
    minCnt = maximum $ map snd tags
    maxCnt = minimum $ map snd tags
    toTagInfo tag n
       = #name @= tag
      <: #size @= calcSize 120.0 80.0 n minCnt maxCnt
      <: nil

    calcSize :: Double -> Double -> Int -> Int -> Int -> Int
    calcSize minSize maxSize cnt min' max' =
      let diff = 1 + fromIntegral max' - fromIntegral min'
          relative = (fromIntegral cnt - fromIntegral min') / diff
      in floor $ minSize + relative * (maxSize - minSize)

buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  postData <- markdownToHTML' . T.pack $ postContent :: Action (Record FrontMatterParams)
  let srcDir   = takeDirectory srcPath
      fileName = takeFileName srcPath
      postUrl  = T.pack . dropDirectory1 $ srcDir <> "-" <> fileName -<.> "html"
      fullPostData = happend siteMeta
          $ #url  @= postUrl
         <: #date @= toPostDate srcPath
         <: postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  convert fullPostData

-- expect: path/to/YYYY/MM-DD-filename.md
toPostDate :: FilePath -> String
toPostDate p = year <> "-" <> date
  where
    date = take 5 $ takeFileName p
    year = takeFileName $ takeDirectory p

buildAbout :: Action ()
buildAbout = do
  aboutT <- compileTemplate' "site/templates/about.html"
  content <- markdownToHTML' =<< T.pack <$> readFile' "site/about.md" :: Action (Record '["content" >: String])
  writeFile' (outputFolder </> "about.html") . T.unpack $ substitute aboutT (happend siteMeta content)

buildArchive :: [Post] -> Action ()
buildArchive posts = do
  archiveT <- compileTemplate' "site/templates/archive.html"
  buildWithPagenation archiveT siteMeta (reverse posts) (outputFolder </> "archive")

buildTagPages :: [Post] -> Action [(Tag, Int)]
buildTagPages posts = do
  tagT <- compileTemplate' "site/templates/tags.html"
  forM (groupByTag posts) $ \(tag, posts') -> do
    buildWithPagenation tagT (#tag @= tag <: siteMeta) posts' (outputFolder </> "tags" </> tag)
    pure (tag, length posts')

groupByTag :: [Post] -> [(Tag, [Post])]
groupByTag = HML.toList . foldl go mempty
  where
    go :: HML.HashMap Tag [Post] -> Post -> HML.HashMap Tag [Post]
    go acc post =
      foldl (\acc' tag -> HML.insertWith (++) tag [post] acc') acc (post ^. #tags)

copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["assets//*", "css//*.css", "js//*.js"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      parseTimeOrError True defaultTimeLocale "%Y-%m-%d" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- liftIO getCurrentTime
  let atomData = shrinkAssoc
          $ #posts       @= mkAtomPost <$> (take 10 $ reverse posts)
         <: #currentTime @= toIsoDate now
         <: #atomUrl     @= "/feed.xml"
         <: siteMeta :: AtomData
  atomTempl <- compileTemplate' "site/templates/feed.xml"
  writeFile' (outputFolder </> "feed.xml") . T.unpack $ substitute atomTempl atomData
    where
      mkAtomPost :: Post -> Post
      mkAtomPost p = p & #date .~ formatDate (p ^. #date)

buildSitemap :: [Post] -> Action ()
buildSitemap posts = do
  sitemapTempl <- compileTemplate' "site/templates/sitemap.xml"
  writeFile' (outputFolder </> "sitemap.xml") . T.unpack $
    substitute sitemapTempl (happend siteMeta $ #posts @= posts <: nil)

buildWithPagenation
  :: Forall (KeyTargetAre KnownSymbol (Instance1 ToMustache Identity)) (xs ++ PagenationInfoParams)
  => Template
  -> (xs :& Field Identity)
  -> [Post]
  -> FilePath
  -> Action ()
buildWithPagenation t r posts dir =
  mapM_ (uncurry buildWithPageNum) $ zip (zip3 prevPageNums [1..] nextPageNums) chunk
  where
    chunk = chunkOf 10 posts
    prevPageNums = Nothing : map Just [1..]
    nextPageNums = map Just [2..(length chunk)] ++ [Nothing]

    buildWithPageNum :: (Maybe Int, Int, Maybe Int) -> [Post] -> Action ()
    buildWithPageNum (prev, current, next) posts' =
      let path = dir </> show current -<.> "html"
          info = #posts @= posts' <: #prevPageNum @= prev <: #nextPageNum @= next <: nil
      in writeFile' path . T.unpack $ substitute t (happend r info)

chunkOf :: Int -> [a] -> [[a]]
chunkOf _ [] = []
chunkOf n xs = let (hs, ts) = splitAt n xs in hs : chunkOf n ts

buildHighlightCss :: Action ()
buildHighlightCss =
  writeFile' (outputFolder </> "css" </> "highlight.css") $ styleToCss pygments

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildArchive allPosts
  tags <- buildTagPages allPosts
  buildIndex tags allPosts
  buildFeed allPosts
  buildSitemap allPosts
  buildAbout
  copyStaticFiles
  buildHighlightCss

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
