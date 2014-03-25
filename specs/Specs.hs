{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit          as HU
-- import Test.Tasty.Golden as TG

import qualified Data.Text as T
import           Control.Error
import qualified Data.List                 as L
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           Phaedrus.Text.BetaCode
import           Phaedrus.Types
import           Phaedrus.XML

import Debug.Trace


testFirst :: FilePath -> (TextLoc -> Assertion) -> Assertion
testFirst fp f = fileToTextLoc fp >>= headZ >>= f

testN :: Int -> FilePath -> (TextLoc -> Assertion) -> Assertion
testN n fp f = fileToTextLoc fp >>= (`atZ` n) >>= f

trace' :: Show a => a -> a
trace' a = traceShow a a


tests :: TestTree
tests = testGroup "phaedrus"
    [ testCase "tlTitle" $
        testFirst "data/Epin_.xml" $ ("Ἐπίνομις" @=?) . _tlTitle
    , testCase "tlFile" $
        testFirst "data/Epin_.xml" $ ("Epin_.xml" @=?) . _tlFile
    , testGroup "tlSpeech"
        [ testCase "speaker" $ do
            testFirst "data/Epin_.xml" $
                (Just "Κλεινίας" @=?) . _speaker . _tlSpeech
            testFirst "data/Apol_.xml" $
                (Nothing @=?) . _speaker . _tlSpeech
        , testCase "speechN" $ do
            testFirst "data/Apol_.xml" $ (1 @=?) . _speechN . _tlSpeech
            testN 152 "data/Apol_.xml" $ (2 @=?) . _speechN . _tlSpeech
        ]
    , testCase "tlPage" $ do
        testFirst "data/Apol_.xml" $ (17 @=?) . _tlPage
        testN 4 "data/Apol_.xml" $ (18 @=?) . _tlPage
        testFirst "data/Charm_.xml" $ (153 @=?) . _tlPage
    , testCase "tlSection" $ do
        testFirst "data/Apol_.xml" $ ("17a" @=?). _tlSection
        testN 1 "data/Apol_.xml" $ ("17b" @=?). _tlSection
        testFirst "data/Charm_.xml" $ ("153a" @=?) . _tlSection
        testN 1 "data/Charm_.xml" $ ("153b" @=?) . _tlSection
    , testCase "tlEvidence" $ do
        tls <- fileToTextLoc "data/Charm_.xml"
        assertBool "Empty list" (not $ L.null tls)
        True @=? not (any _tlEvidence tls)
    , testCase "tlText" $ do
        testFirst "data/Apol_.xml" $ (text1 @=?) . _tlText
        testN 1 "data/Apol_.xml" $ (text2 @=?) . _tlText
        testFirst "data/Charm_.xml" $ (text3 @=?) . _tlText
    , testCase "title normalization and comparison" $
        testFirst "data/Alc__2.xml" $
            (betanorm alc2 @=?) . betanorm . _tlTitle
    ]

main :: IO ()
main = defaultMain tests

text1, text2, text3, alc2 :: T.Text

text1 = "ὅτι μὲν ὑμεῖς, ὦ ἄνδρες Ἀθηναῖοι, πεπόνθατε ὑπὸ τῶν ἐμῶν κατηγόρων, \
\οὐκ οἶδα: ἐγὼ δ᾽ οὖν καὶ αὐτὸς ὑπ᾽ αὐτῶν ὀλίγου ἐμαυτοῦ ἐπελαθόμην, οὕτω \
\πιθανῶς ἔλεγον. καίτοι ἀληθές γε ὡς ἔπος εἰπεῖν οὐδὲν εἰρήκασιν. μάλιστα δὲ \
\αὐτῶν ἓν ἐθαύμασα τῶν πολλῶν ὧν ἐψεύσαντο, τοῦτο ἐν ᾧ ἔλεγον ὡς χρῆν ὑμᾶς \
\εὐλαβεῖσθαι μὴ ὑπ᾽ ἐμοῦ ἐξαπατηθῆτε"

text2 = "ὡς δεινοῦ ὄντος λέγειν. τὸ γὰρ μὴ αἰσχυνθῆναι ὅτι αὐτίκα ὑπ᾽ ἐμοῦ \
\ἐξελεγχθήσονται ἔργῳ, ἐπειδὰν μηδ᾽ ὁπωστιοῦν φαίνωμαι δεινὸς λέγειν, τοῦτό \
\μοι ἔδοξεν αὐτῶν ἀναισχυντότατον εἶναι, εἰ μὴ ἄρα δεινὸν καλοῦσιν οὗτοι \
\λέγειν τὸν τἀληθῆ λέγοντα: εἰ μὲν γὰρ τοῦτο λέγουσιν, ὁμολογοίην ἂν ἔγωγε οὐ \
\κατὰ τούτους εἶναι ῥήτωρ. οὗτοι μὲν οὖν, ὥσπερ ἐγὼ λέγω, ἤ τι ἢ οὐδὲν ἀληθὲς \
\εἰρήκασιν, ὑμεῖς δέ μου ἀκούσεσθε πᾶσαν τὴν ἀλήθειαν—οὐ μέντοι μὰ Δία, ὦ \
\ἄνδρες Ἀθηναῖοι, κεκαλλιεπημένους γε λόγους, ὥσπερ οἱ τούτων,"

text3 = "ἥκομεν τῇ προτεραίᾳ ἑσπέρας ἐκ Ποτειδαίας ἀπὸ τοῦ στρατοπέδου, οἷον \
\δὲ διὰ χρόνου ἀφιγμένος ἁσμένως ᾖα ἐπὶ τὰς συνήθεις διατριβάς. καὶ δὴ καὶ εἰς \
\τὴν Ταυρέου παλαίστραν τὴν καταντικρὺ τοῦ τῆς Βασίλης ἱεροῦ εἰσῆλθον, καὶ \
\αὐτόθι κατέλαβον πάνυ πολλούς, τοὺς μὲν καὶ ἀγνῶτας ἐμοί, τοὺς δὲ πλείστους \
\γνωρίμους. καί με ὡς"

alc2 = "\7944\955\954\953\946\953\940\948\951\962 \946"

