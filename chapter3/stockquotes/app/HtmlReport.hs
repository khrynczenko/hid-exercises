module HtmlReport where

import QuoteData
import StatReport

data Html = Html

htmlReport :: QuoteDataCollection -> StatInfo -> Html
htmlReport quotes info = undefined

saveHtml :: FilePath -> Html -> IO ()
saveHtml filePath html = undefined

