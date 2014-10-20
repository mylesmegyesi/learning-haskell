module CIS194.HW03Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import CIS194.HW03
    ( compareMsgs
    , messagesAbout
    , parse
    , parseMessage
    , sortMessages
    , validMessagesOnly
    , whatWentWrong
    , whatWentWrongEnhanced
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

    describe "Exercise 7" $ do
      describe "messagesAbout" $ do
        it "filters logs that contain the given phrase in the message" $ do
          let message1 = (LogMessage Warning 153 "message number one")
              message2 = (LogMessage Info 208 "message number two") in
              messagesAbout "one" [message1, message2] `shouldBe` [message1]

        it "matches logs case insenstive on the match to string" $ do
          let message1 = (LogMessage Warning 153 "message number one")
              message2 = (LogMessage Info 208 "message number two") in
              messagesAbout "OnE" [message1, message2] `shouldBe` [message1]

        it "matches logs case insenstive on the log string" $ do
          let message1 = (LogMessage Warning 153 "message number ONE")
              message2 = (LogMessage Info 208 "message number two") in
              messagesAbout "one" [message1, message2] `shouldBe` [message1]

        it "returns nothing if there are no matches" $ do
          let message1 = (LogMessage Warning 153 "message number ONE")
              message2 = (LogMessage Info 208 "message number two") in
              messagesAbout "something" [message1, message2] `shouldBe` []

        it "matches on multiple" $ do
          let message1 = (LogMessage Warning 153 "message number ONE")
              message2 = (LogMessage Info 208 "message number two") in
              messagesAbout "message" [message1, message2] `shouldBe` [message1, message2]

    describe "Exercise 8" $ do
      describe "whatWentWrongEnhanced" $ do
        it "returns high severity messages and messages containing a certain keyword" $ do
          results <- testWhatWentWrong parse (whatWentWrongEnhanced "relish") "src/CIS194/error.log"
          results `shouldBe` [ "Twenty seconds remaining until out-of-mustard condition"
                             , "Hard drive failure: insufficient mustard"
                             , "Empty mustard reservoir! Attempting to recover..."
                             , "Depletion of mustard stores detected!"
                             , "Recovery failed! Initiating shutdown sequence"
                             , "All backup mustardwatches are busy"
                             , "All backup mustardwatches are busy"
                             , "Mustardwatch opened, please close for proper functioning!"
                             , "Ten seconds remaining until out-of-mustard condition"
                             ]
