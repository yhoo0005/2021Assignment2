-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where
import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game
import Control.Concurrent.Chan (Chan)
import Cards (Card(Card))
import TwentyOne.Play (playDealerHand, combinePoints)
import Data.Maybe (fromJust)
import Text.Printf (IsChar(toChar))
import GHC.StableName (StableName(StableName))
import TwentyOne.Types (Action(DoubleDown))
import Debug.Trace
import Data.Fixed (div')

-- You can add more imports if you need them
-- Get the card ranks
rank :: Card -> Rank
rank (Card _ rk) = rk

-- | Select suit.
suit :: Card -> Suit
suit (Card s _) = s

initialbid :: Points 
initialbid = 10
 
-- | This function is called once it's your turn, and keeps getting called until your turn ends.
-- | If hand value is 0 means start of the round, I will place a bid
-- | If hand value is 21, I will Stand
-- | If my current hand value is a Combo or Charlie, I will stand 
-- | If my current card point on hand is between 1 and 20, I will call the function decision which takes in
-- | my current hand and dealer upcard to decide my next action

playCard :: PlayFunc
playCard dealer pPoints pinfo pid mem f
                    | trace (show mem) False = undefined 
                    | handCalc f == 0 = placeInitialBid
                    | take 1 (fromJust mem) == "P" = insurance f dealer
                    | take 1 (fromJust mem) == "D" = hitDoubleDown
                    | take 1 (fromJust mem) == "X" = stand
                    | handCalc f == 21 = (Stand, "")
                    | handValue f == Combo || handValue f == Charlie  = stand
                    | handCalc f >= 1 && handCalc f <= 20 = decision f dealer
                    | otherwise = (Stand, "S")


placeInitialBid :: (Action, [Char])
placeInitialBid = (Bid initialbid, "P")

hitDoubleDown :: (Action, [Char])
hitDoubleDown = (Hit, "X")

stand :: (Action, [Char])
stand = (Stand, "S")


-- | If the total card point is <= 21 after adding together with the dealer's upcard, then we will hit 
-- | If the current hand point is between 18 and 20, we will stand
-- | else we will doubledown
decision :: Hand -> Maybe Card -> (Action, [Char])
decision pHand dCard
    | handCalc pHand + toPoints(fromJust dCard) <= 21 = (Hit, "H")
    | handCalc pHand >= 12 && handCalc pHand <= 20 = (Stand, "S") 
    | otherwise = doubledown pHand


doubledown :: Hand -> (Action, [Char])
doubledown pHand
    | length pHand == 2 && handCalc pHand > initialbid * 2 = (DoubleDown initialbid, "D")
    | otherwise = (Stand, "S")



insurance :: Hand -> Maybe Card -> (Action, [Char])
insurance pHand dCard
    | getRank (fromJust dCard) == Ace = (Insurance (initialbid `div` 2), "I")
    | otherwise = (Stand, "S")















