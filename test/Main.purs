module Test.Main
where

import Prelude

import Bonsai.DOM (Document(Document), ElementId(ElementId), addEventListener, affF, elementById, innerHTML, locationHash, ownerDocument, querySelector, querySelectorAll, textContent)
import Bonsai.JSDOM (fireClick, jsdomDocument, setValue)
import Effect.Aff (try)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Control.Monad.Free (Free)
import Data.Array as Array
import Data.Either (isLeft)
import Foreign (isNull, isUndefined, readString)
import Foreign.Index ((!))
import Data.Newtype (unwrap)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest $
  tests


mainHtml :: String
mainHtml = """<html><body id="main">Hello, world!</body></html>"""

tests :: Free TestF Unit
tests =

  suite "Bonsai.DOM" do
    test "document" do
      Document doc <- affF $ jsdomDocument mainHtml
      Assert.assertFalse "document null" $ isNull doc
      Assert.assertFalse "document undefined" $ isUndefined doc

    test "elementById/textContent" do
      doc <- affF $ jsdomDocument mainHtml
      found <- affF $ elementById (ElementId "main") doc
      text  <- affF $ textContent found
      Assert.equal "Hello, world!" text

    test "elementById #doesNotExist" do
      doc <- affF $ jsdomDocument mainHtml
      err <- try $ affF $ elementById (ElementId "doesNotExist") doc
      Assert.assert "should have errored" $ isLeft err

    test "querySelector/innerHTML/ownerDocument" do
      doc <- affF $ jsdomDocument mainHtml
      f1 <- affF $ elementById (ElementId "main") doc
      i1 <- affF $ innerHTML f1
      odoc <- affF $ ownerDocument f1
      f2 <- affF $ querySelector "#main" (unwrap odoc)
      i2 <- affF $ innerHTML f2
      Assert.equal i1 i2

    test "querySelectorAll" do
      doc <- affF $ jsdomDocument mainHtml
      arr <- affF $ querySelectorAll "div" (unwrap doc)
      Assert.equal 0 (Array.length arr)
      arr2 <- affF $ querySelectorAll "body" (unwrap doc)
      Assert.equal 1 (Array.length arr2)

    test "locationHash" do
      hash <- affF $ jsdomDocument mainHtml >>= locationHash
      Assert.equal "" hash

    test "addEventListener" do
      doc <- affF $ jsdomDocument """<html>
<head><title>Test</title></head>
<body>
  <input id="I1" type="text"/>
  <button id="B1" type="button">CLICK ME</button>
</body>
</html>
"""
      i1 <- affF $ elementById (ElementId "I1") doc
      b1 <- affF $ elementById (ElementId "B1") doc

      fired <- liftEffect $ Ref.new false
      affF $
        addEventListener
          { capture: false, once:false, passive: false }
          "click"
          (\_ -> Ref.write true fired)
          b1

      affF $ setValue "Hello, world!" i1
      v <- affF $ (unwrap i1) ! "value" >>= readString
      Assert.equal v "Hello, world!"


      affF $ fireClick b1
      isFired <- liftEffect $ Ref.read fired
      Assert.assert "isFired" isFired


    -- the other DOM helpers can't really be tested with our limited
    -- dom vocabulary.  they will be exercised by the full virtualdom/bonsai tests
