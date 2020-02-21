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
import qualified Data.Bytes as Bytes
import qualified Url
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
instance NFData Url.Url where
  rnf (Url.Url a b c d e f g h i) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i

main :: IO ()
main = defaultMain
  [ bench "url-bytes" $ nf (fmap Url.decodeUrl) bytesUrls
  , bench "uri-bytestring strict" $ nf (fmap $ URI.parseURI URI.strictURIParserOptions) bsUrls
  , bench "uri-bytestring lax" $ nf (fmap $ URI.parseURI URI.laxURIParserOptions) bsUrls
  ]
  where
  !bsUrls = BS.pack <$> urls
  !bytesUrls = Bytes.fromAsciiString <$> urls

urls :: [String]
urls =
  [ "http://google.com/example?params=youbetcha"
  , "http://user:password@facebook.org:322/"
  , "https://en.wikipedia.org/wiki/Main_Page"
  , "https://github.com/goolord/url-bytes/blob/master/src/Url.hs"
  , "http://www.youtube.com"
  , "http://www.facebook.com"
  , "http://www.baidu.com"
  , "http://www.yahoo.com"
  , "http://www.amazon.com"
  , "http://www.wikipedia.org"
  , "http://www.qq.com"
  , "http://www.google.co.in"
  , "http://www.twitter.com"
  , "http://www.live.com"
  , "http://www.taobao.com"
  , "http://www.bing.com#foo"
  , "http://www.instagram.com"
  , "http://www.weibo.com"
  , "http://www.sina.com.cn"
  , "http://www.linkedin.com"
  , "http://www.yahoo.co.jp"
  , "http://www.msn.com"
  , "http://www.vk.com"
  , "http://www.google.de"
  , "http://www.yandex.ru"
  , "http://www.hao123.com"
  , "http://www.google.co.uk"
  , "http://www.reddit.com"
  , "http://www.ebay.com"
  , "http://www.google.fr"
  , "http://www.t.co"
  , "http://www.tmall.com"
  , "http://www.google.com.br"
  , "http://www.360.cn"
  , "http://www.sohu.com"
  , "http://www.amazon.co.jp"
  , "http://www.pinterest.com"
  , "http://www.netflix.com"
  , "http://www.google.it"
  , "http://www.google.ru"
  , "http://www.microsoft.com"
  , "http://www.google.es"
  , "http://www.wordpress.com"
  , "http://www.gmw.cn"
  , "http://www.tumblr.com"
  , "http://www.paypal.com"
  , "http://www.blogspot.com"
  , "http://www.imgur.com"
  , "http://www.stackoverflow.com"
  , "http://www.aliexpress.com"
  , "http://www.naver.com"
  , "http://www.ok.ru"
  , "http://www.apple.com"
  , "http://www.github.com"
  , "http://www.chinadaily.com.cn"
  , "http://www.imdb.com"
  , "http://www.google.co.kr"
  , "http://www.fc2.com"
  , "http://www.jd.com"
  , "http://www.blogger.com"
  , "http://www.163.com"
  , "http://www.google.ca"
  , "http://www.whatsapp.com"
  , "http://www.amazon.in"
  , "http://www.office.com"
  , "http://www.tianya.cn"
  , "http://www.google.co.id"
  , "http://www.youku.com"
  , "http://www.rakuten.co.jp"
  , "http://www.craigslist.org"
  , "http://www.amazon.de"
  , "http://www.nicovideo.jp"
  , "http://www.google.pl"
  , "http://www.soso.com"
  , "http://www.bilibili.com"
  , "http://www.dropbox.com"
  , "http://www.xinhuanet.com"
  , "http://www.outbrain.com"
  , "http://www.pixnet.net"
  , "http://www.alibaba.com"
  , "http://www.alipay.com"
  , "http://www.microsoftonline.com"
  , "http://www.booking.com"
  , "http://www.googleusercontent.com"
  , "http://www.google.com.au"
  , "http://www.popads.net"
  , "http://www.cntv.cn"
  , "http://www.zhihu.com"
  , "http://www.amazon.co.uk"
  , "http://www.diply.com"
  , "http://www.coccoc.com"
  , "http://www.cnn.com"
  , "http://www.bbc.co.uk"
  , "http://www.twitch.tv"
  , "http://www.wikia.com"
  , "http://www.google.co.th"
  , "http://www.go.com"
  , "http://www.google.com.ph"
  , "http://www.doubleclick.net"
  , "http://www.onet.pl"
  , "http://www.googleadservices.com"
  , "http://www.accuweather.com"
  , "http://www.googleweblight.com"
  , "http://www.answers.yahoo.com"
  ]
