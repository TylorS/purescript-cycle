module Cycle (run) where

import Almost (Stream, Promise, Subject, observe, holdSubject, next, complete, thenp)
import Data.StrMap (fromFoldable, StrMap, keys)
import Data.StrMap.Unsafe (unsafeIndex)
import Data.Tuple (Tuple(..))
import Prelude (map, ($))

type Drivers a b = StrMap (Subject a -> b)
type Sinks a = StrMap (Stream a)
type SinkProxies a = StrMap (Subject a)
type Sources b = StrMap b
type Subscriptions a = StrMap a

makeSinkProxies :: forall a b. Drivers a b -> SinkProxies a
makeSinkProxies drivers =
  map (\_ -> holdSubject 1) drivers

callDriver :: forall a b. Drivers a b -> SinkProxies a -> String -> b
callDriver drivers proxies name = driver proxy
  where
    driver :: Subject a -> b
    driver = unsafeIndex drivers name

    proxy :: Subject a
    proxy = unsafeIndex proxies name

callDrivers :: forall a b. Drivers a b -> SinkProxies a -> Sources b
callDrivers drivers sinkProxies =
  fromFoldable $ map (\name -> (Tuple name (callDriver' name))) names
  where
    names :: Array String
    names = keys drivers

    callDriver' :: String -> b
    callDriver' = callDriver drivers sinkProxies

replicateOne :: forall a. Sinks a -> SinkProxies a -> String -> Promise (Subject a)
replicateOne sinks proxies name =
  thenp (\x -> complete x subject) ( observe (\x -> next x subject) stream )
  where
    subject :: Subject a
    subject = unsafeIndex proxies name

    stream :: Stream a
    stream = unsafeIndex sinks name

replicateMany :: forall a. Sinks a -> SinkProxies a -> Subscriptions (Promise (Subject a))
replicateMany sinks proxies =
  fromFoldable $ map (\name -> (Tuple name (replicateOne' name))) names
  where
    names :: Array String
    names = keys proxies

    replicateOne' :: String -> Promise (Subject a)
    replicateOne' = replicateOne sinks proxies

run :: forall a b.
  (Sources b -> Sinks a) ->
  Drivers a b ->
  Subscriptions (Promise (Subject a))
run main drivers = replicateMany (main sources) sinkProxies
  where
    sinkProxies :: SinkProxies a
    sinkProxies = makeSinkProxies drivers

    sources :: Sources b
    sources = callDrivers drivers sinkProxies
