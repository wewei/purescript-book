module Test.MySolutions where

import Prelude ((<<<), (==), ($))


import Data.List (head, filter, null, nubByEq)
import Data.Maybe (Maybe)
import Data.AddressBook (AddressBook, Entry)
import Data.HeytingAlgebra (not, (&&))

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter ((_ == street) <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not <<< null <<< filter match
  where
    match e = e.firstName == firstName && e.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq entryEq
  where
    entryEq x y = x.firstName == y.firstName && x.lastName == y.lastName
