module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(Cons), filter, head)
import Data.Maybe (Maybe)


type AddressBook = List Entry

type Entry =
    { firstName :: String
    , lastName  :: String
    , address   :: Address
    }

type Address =
    { street    :: String
    , city      :: String
    , state     :: String
    }

{- This would be nice to make work, but right now gives us an
   InvalidInstanceHead error. Not sure how to handle it yet.
instance showAddressInstance
    :: Show { street::String, city::String, state::String }
    where
        show = showAddress
-}

showEntry :: Entry -> String
showEntry e = e.lastName
    <> ", " <> e.firstName
    <> ": " <> showAddress e.address

showAddress :: Address -> String
showAddress a = a.street <> ", " <> a.city <> ", " <> a.state


emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry fname lname = head <<< filter filterEntry
                 -- or  filter filterEntry >>> head
    where
        filterEntry :: Entry -> Boolean
        filterEntry e = e.firstName == fname && e.lastName == lname

