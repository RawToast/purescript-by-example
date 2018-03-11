module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
-- same as:
-- findEntry firstName lastName = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

filterEntries :: String -> String -> AddressBook ->  AddressBook
filterEntries firstName lastName = filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- 2
findEntryByAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findEntryByAddress street city state = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street && entry.address.city == city && entry.address.state == state

-- 3
hasName :: String -> String -> AddressBook -> Boolean
hasName firstname lastname = filter filterEntry >>> null >>> eq false
  where
  filterEntry entry = entry.firstName == firstname && entry.lastName == lastname

-- 4
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy filterEntries
  where
  filterEntries e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName