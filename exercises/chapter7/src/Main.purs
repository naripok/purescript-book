module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Control.Apply (lift2)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)
import Data.Validation.Semigroup (V)
import Data.AddressBook.Validation (matches, Errors, nonEmpty, arrayNonEmpty, validatePhoneNumber)
import Data.AddressBook (Address(..), address, PhoneNumber, PhoneType(HomePhone), phoneNumber)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Traversable (class Traversable)

--

liftedPlus :: Maybe Int -> Maybe Int -> Maybe Int
liftedPlus = lift2 (+)


combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just f) = Just <$> f
combineMaybe Nothing = pure Nothing

--

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[a-zA-Z]{2}$" noFlags of
         Right r -> r


validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (matches "State" stateRegex o.state *> pure o.state)

--

whitespaceRegex :: Regex
whitespaceRegex =
  unsafePartial
    case regex "^\\S+$" noFlags of
         Right r -> r


newValidateAddress :: Address -> V Errors Address
newValidateAddress (Address o) =
  address <$> (matches "Street" whitespaceRegex o.street  *> pure o.street)
          <*> (matches "City"   whitespaceRegex o.city    *> pure o.city)
          <*> (matches "State"  stateRegex      o.state   *> pure o.state)

--

{-- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b) --}
{-- sequence :: forall a m. Applicative m => t (m a) -> m (t a) --}

{-- listTraverse --}
{-- traverseList :: forall a b f. Applicative f => (a -> f b) -> List a -> f List b --}
{-- traverse _ Nil = pure Nil --}
{-- traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs --}


data Tree a = Leaf a | Branch (Tree a) a (Tree a)


instance functorTree :: Functor Tree where
  map :: forall a b. (a -> b) -> Tree a -> Tree b
  map f (Leaf a) = Leaf (f a)
  map f (Branch x y z) = Branch (f <$> x) (f y) (f <$> z)


instance foldableTree :: Foldable Tree where
  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr f x (Branch l y r) = foldr f (f y (foldr f x r)) l
  foldr f x (Leaf a) = f a x

  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl f x (Branch l y r) = foldl f (f (foldl f x l) y) r
  foldl f x (Leaf a) = f x a

  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r
  foldMap f (Leaf a) = mempty


instance inOrderTraversableTree :: Traversable Tree where

  traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse f (Leaf y) = Leaf <$> (f y)
  traverse f (Branch r y l) = Branch <$> traverse f r <*> f y <*> traverse f l

  sequence :: forall a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence (Leaf y) = Leaf <$> y
  sequence (Branch l y r) = Branch <$> (traverse identity l) <*> y <*> (traverse identity r)

--

newtype Person = Person
  { firstName   :: String
  , lastName    :: String
  , homeAddress :: Maybe Address
  , phones      :: Array PhoneNumber
  }

person :: String -> String -> Maybe Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones =
  Person { firstName, lastName, homeAddress, phones }

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> traverse validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePersonAdo :: Person -> V Errors Person
validatePersonAdo (Person o) = ado
  firstName   <- (nonEmpty "First Name" o.firstName *> pure o.firstName)
  lastName    <- (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
  address     <- traverse validateAddress o.homeAddress
  numbers     <- (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)
  in person firstName lastName address numbers


instance showPerson :: Show Person where
  show (Person o) = "Person " <> show o

--

sequence :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequence = traverse identity

traverse :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverse f t = sequence (f <$> t)

--

main :: Effect Unit
main = do
  logShow $ (Just 1) `liftedPlus` (Just 2)
  logShow $ (Just 1) `liftedPlus` (Just 2)
  logShow $ combineMaybe (Just [1, 2, 3])
  logShow $ person "Some" "LName" Nothing [phoneNumber HomePhone "555-555-5555"]
