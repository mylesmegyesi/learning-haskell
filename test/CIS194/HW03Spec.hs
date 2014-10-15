module CIS194.HW03Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import CIS194.HW03
    ( compareMsgs
    , parse
    , parseMessage
    , sortMessages
    , validMessagesOnly
    , whatWentWrong
    )
import CIS194.Log
    ( MaybeLogMessage(ValidLM, InvalidLM)
    , MessageType(Info, Warning, Error)
    , LogMessage(LogMessage)

    , testWhatWentWrong
    )

spec :: Spec
spec =
  describe "CIS 194 Homework 3" $ do
    describe "Exercise 1" $ do
      describe "parseMessage" $ do
        it "parses a valid info log" $ do
          parseMessage "I 29 la la la" `shouldBe` ValidLM (LogMessage Info 29 "la la la")

        it "parses a valid warning log" $ do
          parseMessage "W 29 la la la" `shouldBe` ValidLM (LogMessage Warning 29 "la la la")

        it "parses a valid error log" $ do
          parseMessage "E 2 562 help help" `shouldBe` ValidLM (LogMessage (Error 2) 562 "help help")

        it "handles an invalid log" $ do
          parseMessage "This is not in the right format" `shouldBe` InvalidLM "This is not in the right format"

    describe "Exercise 2" $ do
      describe "validMessagesOnly" $ do
        it "filters invalid messages" $ do
          let validMessages = [ LogMessage Info 29 "la la la"
                              , LogMessage Warning 29 "la la la"
                              ]
              invalidMessages = [InvalidLM "This is not in the right format"]
              allMessages = (map ValidLM validMessages) ++ invalidMessages in
              validMessagesOnly allMessages `shouldBe` validMessages

    describe "Exercise 3" $ do
      describe "parse" $ do
        it "parses a string into log messages" $ do
          let logfile = "I 6 Completed armadillo processing\nW notannumber Nothing to report\nE 2 4 Everything normal"
              expectedLogs = [ LogMessage Info 6 "Completed armadillo processing"
                             , LogMessage (Error 2) 4 "Everything normal"
                             ] in
              parse logfile `shouldBe` expectedLogs

    describe "Exercise 4" $ do
      describe "compareMsgs" $ do
        it "returns LT when the first message happened before the second" $ do
          let firstMessage = (LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing...")
              secondMessage = (LogMessage Info 208 "the Weighted Companion Cube cannot talk") in
              compareMsgs firstMessage secondMessage `shouldBe` LT

        it "returns EQ when both messages happened at the same time" $ do
          let firstMessage = (LogMessage Warning 208 "Not a speck of light is showing, so the danger must be growing...")
              secondMessage = (LogMessage Info 208 "the Weighted Companion Cube cannot talk") in
              compareMsgs firstMessage secondMessage `shouldBe` EQ

        it "returns GT when the second message happened before the first" $ do
          let firstMessage = (LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing...")
              secondMessage = (LogMessage Info 208 "the Weighted Companion Cube cannot talk") in
              compareMsgs secondMessage firstMessage `shouldBe` GT

    describe "Exercise 5" $ do
      describe "sortMessages" $ do
        it "orders the messages by timestamp" $ do
          let firstMessage = (LogMessage Warning 153 "test")
              secondMessage1 = (LogMessage Info 208 "message 1")
              secondMessage2 = (LogMessage Info 208 "message 2")
              thirdMessage = (LogMessage Info 500 "message 3")
              unorderedMessages = [ secondMessage1
                                  , thirdMessage
                                  , secondMessage2
                                  , firstMessage
                                  ] in
              sortMessages unorderedMessages `shouldBe` [ firstMessage
                                                        , secondMessage1
                                                        , secondMessage2
                                                        , thirdMessage
                                                        ]
    describe "Exercise 6" $ do
      describe "whatWentWrong" $ do
        it "returns errors with severity greater that 50" $ do
          results <- testWhatWentWrong parse whatWentWrong "src/CIS194/sample.log"
          results `shouldBe` [ "Way too many pickles"
                             , "Bad pickle-flange interaction detected"
                             , "Flange failed!"
                             ]
