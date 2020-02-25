{-# LANGUAGE
    BangPatterns
  , GADTs
  , DeriveGeneric
  , StandaloneDeriving
  , MagicHash
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

import Gauge.Main (defaultMain, bench, nf)

import Data.Bytes.Types
import Control.DeepSeq
import GHC.Generics
import Data.Primitive.ByteArray
import Control.Applicative (ZipList(..))
import qualified Data.Bytes as Bytes
import qualified Url
import qualified Url.Unsafe
import qualified Data.ByteString.Char8 as BS
import qualified URI.ByteString as URI

instance NFData (URI.URIRef a) where
  rnf (URI.URI a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e
  rnf (URI.RelativeRef b c d e) = rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance NFData URI.Authority
instance NFData URI.Host
instance NFData URI.UserInfo
instance NFData URI.SchemaError
instance NFData URI.URIParseError
instance NFData URI.Scheme
instance NFData URI.Port
instance NFData URI.Query

deriving instance Generic Url.ParseError
deriving instance Generic Url.Url
deriving instance Generic Bytes
instance NFData Url.ParseError
instance NFData ByteArray where
  rnf !b = b `seq` ()
instance NFData Bytes
instance NFData Url.Unsafe.Url where
  rnf (Url.Unsafe.Url a _ _ _ _ _ _ _ _) = rnf a

main :: IO ()
main = defaultMain
  [ bench "url-bytes 1000" $ nf (fmap Url.decodeUrl) bytesUrls
  , bench "uri-bytestring strict 1000" $ nf (fmap $ URI.parseURI URI.strictURIParserOptions) bsUrls
  , bench "uri-bytestring lax 1000" $ nf (fmap $ URI.parseURI URI.laxURIParserOptions) bsUrls
  ]
  where
  !permUrls = take 1000 $ getZipList $ 
    (\a b c d -> a <> b <> c <> d)
    <$> ZipList (cycle schemes) 
    <*> ZipList (cycle urls)
    <*> ZipList (cycle paths)
    <*> ZipList (cycle queryParams)
  !bsUrls = BS.pack <$> permUrls
  !bytesUrls = Bytes.fromAsciiString <$> permUrls

paths :: [String]
paths =
  [ "/example"
  , "//foo/bar@:"
  , ""
  , "/"
  , "/en/wikipedia/Foo.txt"
  ]

queryParams :: [String]
queryParams =
  [ "?foo=bar&qux=quine"
  , ""
  , "?"
  ]

schemes :: [String]
schemes =
  [ "http://"
  , "https://"
  ]

urls :: [String]
urls =
  [ "ggoogle.com"
  , "guser:password@facebook.org:322"
  , "en.wikipedia.org"
  , "github.com"
  , "www.youtube.com"
  , "www.facebook.com"
  , "www.baidu.com"
  , "www.yahoo.com"
  , "www.amazon.com"
  , "www.wikipedia.org"
  , "www.qq.com"
  , "www.google.co.in"
  , "www.twitter.com"
  , "www.live.com"
  , "www.taobao.com"
  , "www.bing.com#foo"
  , "www.instagram.com"
  , "www.weibo.com"
  , "www.sina.com.cn"
  , "www.linkedin.com"
  , "www.yahoo.co.jp"
  , "www.msn.com"
  , "www.vk.com"
  , "www.google.de"
  , "www.yandex.ru"
  , "www.hao123.com"
  , "www.google.co.uk"
  , "www.reddit.com"
  , "www.ebay.com"
  , "www.google.fr"
  , "www.t.co"
  , "www.tmall.com"
  , "www.google.com.br"
  , "www.360.cn"
  , "www.sohu.com"
  , "www.amazon.co.jp"
  , "www.pinterest.com"
  , "www.netflix.com"
  , "www.google.it"
  , "www.google.ru"
  , "www.microsoft.com"
  , "www.google.es"
  , "www.wordpress.com"
  , "www.gmw.cn"
  , "www.tumblr.com"
  , "www.paypal.com"
  , "www.blogspot.com"
  , "www.imgur.com"
  , "www.stackoverflow.com"
  , "www.aliexpress.com"
  , "www.naver.com"
  , "www.ok.ru"
  , "www.apple.com"
  , "www.github.com"
  , "www.chinadaily.com.cn"
  , "www.imdb.com"
  , "www.google.co.kr"
  , "www.fc2.com"
  , "www.jd.com"
  , "www.blogger.com"
  , "www.163.com"
  , "www.google.ca"
  , "www.whatsapp.com"
  , "www.amazon.in"
  , "www.office.com"
  , "www.tianya.cn"
  , "www.google.co.id"
  , "www.youku.com"
  , "www.rakuten.co.jp"
  , "www.craigslist.org"
  , "www.amazon.de"
  , "www.nicovideo.jp"
  , "www.google.pl"
  , "www.soso.com"
  , "www.bilibili.com"
  , "www.dropbox.com"
  , "www.xinhuanet.com"
  , "www.outbrain.com"
  , "www.pixnet.net"
  , "www.alibaba.com"
  , "www.alipay.com"
  , "www.microsoftonline.com"
  , "www.booking.com"
  , "www.googleusercontent.com"
  , "www.google.com.au"
  , "www.popads.net"
  , "www.cntv.cn"
  , "www.zhihu.com"
  , "www.amazon.co.uk"
  , "www.diply.com"
  , "www.coccoc.com"
  , "www.cnn.com"
  , "www.bbc.co.uk"
  , "www.twitch.tv"
  , "www.wikia.com"
  , "www.google.co.th"
  , "www.go.com"
  , "www.google.com.ph"
  , "www.doubleclick.net"
  , "www.onet.pl"
  , "www.googleadservices.com"
  , "www.accuweather.com"
  , "www.googleweblight.com"
  , "www.answers.yahoo.com"
  ]
