module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), head, filter, null, nubBy)
import Data.Maybe (Maybe)

type Entry = { firstName :: String
             , lastName  :: String
             , address   :: Address
             }

type Address = { street :: String
               , city   :: String
               , state  :: String
               }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city   <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
{-- insertEntry = entry book = Cons entry book --} -- eta conversion
insertEntry = Cons

{-- $, alias for apply, is right associative, low precedence. its use is called 'infix function
  application' --}
findEntry :: String -> String -> AddressBook -> Maybe Entry
{-- findEntry firstName lastName book = head $ filter filterEntry book --}
{-- <<< operator is called backwards composition, >>> is called forward composition --}
{-- `head <<< filter filterEntry` and `filter filterEntry >>> head` are isomorphic --}
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName = map showEntry <<< findEntry firstName lastName

{-- exercises --}
findByStreet :: String -> AddressBook -> Maybe Entry
findByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

haveName :: String -> AddressBook -> Boolean
haveName name = not null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == name || entry.lastName == name

removeDup :: AddressBook -> AddressBook
removeDup = nubBy predicate
  where
    predicate e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName

address = { street: "123", city: "FakeTown", state: "CA" }
entry1 = { firstName: "John", lastName: "Doe", address: address }
entry2 = { firstName: "Mary", lastName: "Doe", address: address }
book1 = insertEntry entry1 $ insertEntry entry2 emptyBook
book2 = insertEntry entry1 book1
