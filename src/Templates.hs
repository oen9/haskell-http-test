{-# LANGUAGE OverloadedStrings #-}

module Templates (
    homePageTemplate
  , queryParamsTemplate
  , echoTemplate
  , formPageGetTemplate
  , formPagePostTemplate
) where

import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import Data.Text.Lazy

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body ! A.style "background-color: CornflowerBlue;" $ do
      body
      p $ a ! href "/" $ "back home"

homePageTemplate :: Response
homePageTemplate =
  template "home page" $ do
          H.h1 "Hello haskell!"
          H.p $ a ! href "/echo/secret%20message"  $ "echo"
          H.p $ a ! href "/query?foo=bar" $ "query parameters"
          H.p $ a ! href "/form" $ "form processing"

queryParamsTemplate :: Maybe Text -> Response
queryParamsTemplate mFoo =
  template "query params" $ do
        p $ "foo is set to: " >> toHtml (show mFoo)
        p $ "change the url to set it to something else."

echoTemplate :: Text -> Response
echoTemplate msg =
  template "echo" $ do
    p $ "echo says: " >> toHtml msg
    p "Change the url to echo something else."

formPageGetTemplate :: Response
formPageGetTemplate =
  template "form" $
    form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
      label ! A.for "msg" $ "Say something clever "
      input ! type_ "text" ! A.id "msg" ! name "msg"
      input ! type_ "submit" ! value "Say it!"

formPagePostTemplate :: Text -> Response
formPagePostTemplate msg =
  template "form" $ do
    H.p "You said:"
    H.p (toHtml msg)
