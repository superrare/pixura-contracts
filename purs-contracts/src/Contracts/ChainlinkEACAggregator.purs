--------------------------------------------------------------------------------
-- | ChainlinkEACAggregator
--------------------------------------------------------------------------------

module Contracts.ChainlinkEACAggregator where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D0, D1, D2, D5, D6, D8, DOne, IntN, Tuple0(..), Tuple1(..), Tuple2(..), Tuple5, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,address)") (Tuple2 (Tagged (SProxy "_aggregator") Address) (Tagged (SProxy "_accessController") Address))

constructor :: TransactionOptions NoPay -> HexString -> { _aggregator :: Address, _accessController :: Address } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_aggregator") Address) -> (Tagged (SProxy "_accessController") Address) -> Web3 HexString
    constructor' y0 bc' y2 y3 = deployContract y0 bc' ((tagged $ Tuple2 y2 y3) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | AnswerUpdated
--------------------------------------------------------------------------------


newtype AnswerUpdated = AnswerUpdated {current :: (IntN (D2 :& D5 :& DOne D6)),roundId :: (UIntN (D2 :& D5 :& DOne D6)),updatedAt :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAnswerUpdated :: Newtype AnswerUpdated _

instance eventFilterAnswerUpdated :: EventFilter AnswerUpdated where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0559884fd3a460db3073b7fc896cc77986f16e378210ded43186175bf646fc5f"),Nothing,Nothing]

instance indexedEventAnswerUpdated :: IndexedEvent (Tuple2 (Tagged (SProxy "current") (IntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "roundId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple1 (Tagged (SProxy "updatedAt") (UIntN (D2 :& D5 :& DOne D6)))) AnswerUpdated where
  isAnonymous _ = false

derive instance genericAnswerUpdated :: Generic AnswerUpdated _

instance eventGenericAnswerUpdatedShow :: Show AnswerUpdated where
  show = genericShow

instance eventGenericAnswerUpdatedeq :: Eq AnswerUpdated where
  eq = genericEq

--------------------------------------------------------------------------------
-- | NewRound
--------------------------------------------------------------------------------


newtype NewRound = NewRound {roundId :: (UIntN (D2 :& D5 :& DOne D6)),startedBy :: Address,startedAt :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeNewRound :: Newtype NewRound _

instance eventFilterNewRound :: EventFilter NewRound where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0109fc6f55cf40689f02fbaad7af7fe7bbac8a3d2186600afc7d3e10cac60271"),Nothing,Nothing]

instance indexedEventNewRound :: IndexedEvent (Tuple2 (Tagged (SProxy "roundId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "startedBy") Address)) (Tuple1 (Tagged (SProxy "startedAt") (UIntN (D2 :& D5 :& DOne D6)))) NewRound where
  isAnonymous _ = false

derive instance genericNewRound :: Generic NewRound _

instance eventGenericNewRoundShow :: Show NewRound where
  show = genericShow

instance eventGenericNewRoundeq :: Eq NewRound where
  eq = genericEq

--------------------------------------------------------------------------------
-- | OwnershipTransferRequested
--------------------------------------------------------------------------------


newtype OwnershipTransferRequested = OwnershipTransferRequested {from :: Address,to :: Address}

derive instance newtypeOwnershipTransferRequested :: Newtype OwnershipTransferRequested _

instance eventFilterOwnershipTransferRequested :: EventFilter OwnershipTransferRequested where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ed8889f560326eb138920d842192f0eb3dd22b4f139c87a2c57538e05bae1278"),Nothing,Nothing]

instance indexedEventOwnershipTransferRequested :: IndexedEvent (Tuple2 (Tagged (SProxy "from") Address) (Tagged (SProxy "to") Address)) (Tuple0 ) OwnershipTransferRequested where
  isAnonymous _ = false

derive instance genericOwnershipTransferRequested :: Generic OwnershipTransferRequested _

instance eventGenericOwnershipTransferRequestedShow :: Show OwnershipTransferRequested where
  show = genericShow

instance eventGenericOwnershipTransferRequestedeq :: Eq OwnershipTransferRequested where
  eq = genericEq

--------------------------------------------------------------------------------
-- | OwnershipTransferred
--------------------------------------------------------------------------------


newtype OwnershipTransferred = OwnershipTransferred {from :: Address,to :: Address}

derive instance newtypeOwnershipTransferred :: Newtype OwnershipTransferred _

instance eventFilterOwnershipTransferred :: EventFilter OwnershipTransferred where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8be0079c531659141344cd1fd0a4f28419497f9722a3daafe3b4186f6b6457e0"),Nothing,Nothing]

instance indexedEventOwnershipTransferred :: IndexedEvent (Tuple2 (Tagged (SProxy "from") Address) (Tagged (SProxy "to") Address)) (Tuple0 ) OwnershipTransferred where
  isAnonymous _ = false

derive instance genericOwnershipTransferred :: Generic OwnershipTransferred _

instance eventGenericOwnershipTransferredShow :: Show OwnershipTransferred where
  show = genericShow

instance eventGenericOwnershipTransferredeq :: Eq OwnershipTransferred where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AcceptOwnershipFn
--------------------------------------------------------------------------------


type AcceptOwnershipFn = Tagged (SProxy "acceptOwnership()") (Tuple0 )

acceptOwnership :: TransactionOptions NoPay -> Web3 HexString
acceptOwnership x0 = sendTx x0 ((tagged $ Tuple0 ) :: AcceptOwnershipFn)

--------------------------------------------------------------------------------
-- | AccessControllerFn
--------------------------------------------------------------------------------


type AccessControllerFn = Tagged (SProxy "accessController()") (Tuple0 )

accessController :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
accessController x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AccessControllerFn)

--------------------------------------------------------------------------------
-- | AggregatorFn
--------------------------------------------------------------------------------


type AggregatorFn = Tagged (SProxy "aggregator()") (Tuple0 )

aggregator :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
aggregator x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AggregatorFn)

--------------------------------------------------------------------------------
-- | ConfirmAggregatorFn
--------------------------------------------------------------------------------


type ConfirmAggregatorFn = Tagged (SProxy "confirmAggregator(address)") (Tuple1 (Tagged (SProxy "_aggregator") Address))

confirmAggregator :: TransactionOptions NoPay -> { _aggregator :: Address } -> Web3 HexString
confirmAggregator x0 r = uncurryFields  r $ confirmAggregator' x0
   where
    confirmAggregator' :: TransactionOptions NoPay -> (Tagged (SProxy "_aggregator") Address) -> Web3 HexString
    confirmAggregator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: ConfirmAggregatorFn)

--------------------------------------------------------------------------------
-- | DecimalsFn
--------------------------------------------------------------------------------


type DecimalsFn = Tagged (SProxy "decimals()") (Tuple0 )

decimals :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
decimals x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DecimalsFn)

--------------------------------------------------------------------------------
-- | DescriptionFn
--------------------------------------------------------------------------------


type DescriptionFn = Tagged (SProxy "description()") (Tuple0 )

description :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
description x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DescriptionFn)

--------------------------------------------------------------------------------
-- | GetAnswerFn
--------------------------------------------------------------------------------


type GetAnswerFn = Tagged (SProxy "getAnswer(uint256)") (Tuple1 (Tagged (SProxy "_roundId") (UIntN (D2 :& D5 :& DOne D6))))

getAnswer :: TransactionOptions NoPay -> ChainCursor -> { _roundId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (IntN (D2 :& D5 :& DOne D6)))
getAnswer x0 cm r = uncurryFields  r $ getAnswer' x0 cm
   where
    getAnswer' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_roundId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (IntN (D2 :& D5 :& DOne D6)))
    getAnswer' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetAnswerFn)

--------------------------------------------------------------------------------
-- | GetRoundDataFn
--------------------------------------------------------------------------------


type GetRoundDataFn = Tagged (SProxy "getRoundData(uint80)") (Tuple1 (Tagged (SProxy "_roundId") (UIntN (D8 :& DOne D0))))

getRoundData :: TransactionOptions NoPay -> ChainCursor -> { _roundId :: (UIntN (D8 :& DOne D0)) } -> Web3 (Either CallError (Tuple5 (UIntN (D8 :& DOne D0)) (IntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D8 :& DOne D0))))
getRoundData x0 cm r = uncurryFields  r $ getRoundData' x0 cm
   where
    getRoundData' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_roundId") (UIntN (D8 :& DOne D0))) -> Web3 (Either CallError (Tuple5 (UIntN (D8 :& DOne D0)) (IntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D8 :& DOne D0))))
    getRoundData' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 y2) :: GetRoundDataFn)

--------------------------------------------------------------------------------
-- | GetTimestampFn
--------------------------------------------------------------------------------


type GetTimestampFn = Tagged (SProxy "getTimestamp(uint256)") (Tuple1 (Tagged (SProxy "_roundId") (UIntN (D2 :& D5 :& DOne D6))))

getTimestamp :: TransactionOptions NoPay -> ChainCursor -> { _roundId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getTimestamp x0 cm r = uncurryFields  r $ getTimestamp' x0 cm
   where
    getTimestamp' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_roundId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getTimestamp' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetTimestampFn)

--------------------------------------------------------------------------------
-- | LatestAnswerFn
--------------------------------------------------------------------------------


type LatestAnswerFn = Tagged (SProxy "latestAnswer()") (Tuple0 )

latestAnswer :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (IntN (D2 :& D5 :& DOne D6)))
latestAnswer x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: LatestAnswerFn)

--------------------------------------------------------------------------------
-- | LatestRoundFn
--------------------------------------------------------------------------------


type LatestRoundFn = Tagged (SProxy "latestRound()") (Tuple0 )

latestRound :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
latestRound x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: LatestRoundFn)

--------------------------------------------------------------------------------
-- | LatestRoundDataFn
--------------------------------------------------------------------------------


type LatestRoundDataFn = Tagged (SProxy "latestRoundData()") (Tuple0 )

latestRoundData :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Tuple5 (UIntN (D8 :& DOne D0)) (IntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D8 :& DOne D0))))
latestRoundData x0 cm = call x0 cm ((tagged $ Tuple0 ) :: LatestRoundDataFn)

--------------------------------------------------------------------------------
-- | LatestTimestampFn
--------------------------------------------------------------------------------


type LatestTimestampFn = Tagged (SProxy "latestTimestamp()") (Tuple0 )

latestTimestamp :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
latestTimestamp x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: LatestTimestampFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | PhaseAggregatorsFn
--------------------------------------------------------------------------------


type PhaseAggregatorsFn = Tagged (SProxy "phaseAggregators(uint16)") (Tuple1 (UIntN (D1 :& DOne D6)))

phaseAggregators :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D1 :& DOne D6)) -> Web3 (Either CallError Address)
phaseAggregators x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: PhaseAggregatorsFn)

--------------------------------------------------------------------------------
-- | PhaseIdFn
--------------------------------------------------------------------------------


type PhaseIdFn = Tagged (SProxy "phaseId()") (Tuple0 )

phaseId :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D1 :& DOne D6)))
phaseId x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PhaseIdFn)

--------------------------------------------------------------------------------
-- | ProposeAggregatorFn
--------------------------------------------------------------------------------


type ProposeAggregatorFn = Tagged (SProxy "proposeAggregator(address)") (Tuple1 (Tagged (SProxy "_aggregator") Address))

proposeAggregator :: TransactionOptions NoPay -> { _aggregator :: Address } -> Web3 HexString
proposeAggregator x0 r = uncurryFields  r $ proposeAggregator' x0
   where
    proposeAggregator' :: TransactionOptions NoPay -> (Tagged (SProxy "_aggregator") Address) -> Web3 HexString
    proposeAggregator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: ProposeAggregatorFn)

--------------------------------------------------------------------------------
-- | ProposedAggregatorFn
--------------------------------------------------------------------------------


type ProposedAggregatorFn = Tagged (SProxy "proposedAggregator()") (Tuple0 )

proposedAggregator :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
proposedAggregator x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ProposedAggregatorFn)

--------------------------------------------------------------------------------
-- | ProposedGetRoundDataFn
--------------------------------------------------------------------------------


type ProposedGetRoundDataFn = Tagged (SProxy "proposedGetRoundData(uint80)") (Tuple1 (Tagged (SProxy "_roundId") (UIntN (D8 :& DOne D0))))

proposedGetRoundData :: TransactionOptions NoPay -> ChainCursor -> { _roundId :: (UIntN (D8 :& DOne D0)) } -> Web3 (Either CallError (Tuple5 (UIntN (D8 :& DOne D0)) (IntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D8 :& DOne D0))))
proposedGetRoundData x0 cm r = uncurryFields  r $ proposedGetRoundData' x0 cm
   where
    proposedGetRoundData' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_roundId") (UIntN (D8 :& DOne D0))) -> Web3 (Either CallError (Tuple5 (UIntN (D8 :& DOne D0)) (IntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D8 :& DOne D0))))
    proposedGetRoundData' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 y2) :: ProposedGetRoundDataFn)

--------------------------------------------------------------------------------
-- | ProposedLatestRoundDataFn
--------------------------------------------------------------------------------


type ProposedLatestRoundDataFn = Tagged (SProxy "proposedLatestRoundData()") (Tuple0 )

proposedLatestRoundData :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Tuple5 (UIntN (D8 :& DOne D0)) (IntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D8 :& DOne D0))))
proposedLatestRoundData x0 cm = call x0 cm ((tagged $ Tuple0 ) :: ProposedLatestRoundDataFn)

--------------------------------------------------------------------------------
-- | SetControllerFn
--------------------------------------------------------------------------------


type SetControllerFn = Tagged (SProxy "setController(address)") (Tuple1 (Tagged (SProxy "_accessController") Address))

setController :: TransactionOptions NoPay -> { _accessController :: Address } -> Web3 HexString
setController x0 r = uncurryFields  r $ setController' x0
   where
    setController' :: TransactionOptions NoPay -> (Tagged (SProxy "_accessController") Address) -> Web3 HexString
    setController' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetControllerFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "_to") Address))

transferOwnership :: TransactionOptions NoPay -> { _to :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)

--------------------------------------------------------------------------------
-- | VersionFn
--------------------------------------------------------------------------------


type VersionFn = Tagged (SProxy "version()") (Tuple0 )

version :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
version x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: VersionFn)