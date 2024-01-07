{- Normally this would be split into multiple modules as there are cross-cutting
concerns. However, for ease of review, I've placed it all in one.

Also note there are many more comments here than I would usually put in code,
attempting to explain the exact reason for each and every choice. I tend to
restrict comments down to how one should best use a type or function when
it is non-obvious. -}
module Main where

import Prelude (Unit)

import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

{- A very basic validation error type, perhaps a bit too focused at the moment.
Some of these don't make sense for certain validations, like TooShort or TooLong don't
make sense when validating numerical import. However, redefining validation errors for
every single field could be considered overkill.

So I've gone with a middle-ground. A type that takes a parameter allowing for extension.
I'ved attempted to cover the most common cases while requiring only the minimum amount
of thought for the coder who will end up using this.

This would normally be in it's own module, or module with other form / validation logic.
It would then be extended as needed in other modules dealing with more complex types. -}
data ValidationError custom_error
  = EmptyValueDisallowed 
  {- A whitelist -}
  | OnlyTheseCharactersAllowed String
  {- a blacklist -}
  | FoundDisallowedCharacters String
  {- a human readable pattern. eg:
  * "phone number like (111) 222-3333"
  * "ssn formatted as XXX-XX-XXXX or XXXXXXXXX"
  -}
  | PatternMismatch String
  | InvalidLength { min :: Int, max :: Int}
  {- for when the input doesn't have a max length -}
  | TooShort Int
  {- for when the input doesn't have a minimum length -}
  | TooLong Int
  {- for any errors not covered above -}
  | Other custom_error

{- A social security number represented only by the digits.
This would be in it's own module with the constructor hidden -}
newtype SocialSecurityNumber = SocialSecurityNumber NonEmptyString

{- Takes a string and returns a SocialSecurityNumber if it is valid.
A valid ssn string is 10 digits, '-', and whitespace. Specifics of
whitespace and '-' don't matter, so long as after those are removed
exactly 10 digits remain. This way, we can be reasonably certain that
if we're dealing the a SocialSecurityNumber type, it's at least
structurally valid. -}
ssn_from_string :: String -> Either (ValidationError String) SocialSecurityNumber
ssn_from_string _ = Left (Other "not implemented")

{- Formats a social security number to XXX-XX-XXXX. This may not exist at all
in the same module as ssn_from_string as it's a purely display function. -}
format_ssn :: SocialSecurityNumber -> String
format_ssn _ = "000-11-2222"

{- This would exist in a module / package focused on phone number validation.
It would be updated periodically as country codes or formats might change. It's
following the same strategy as the social security number.-}
newtype PhoneNumber = PhoneNumber
  { country_code :: Int
  , digits :: String
  , extension :: String
  }

{- Takes a string and does it's best guess on the phone number.
If it starts with +, attempts to extract the country code. This can be done unambigously
( https://en.wikipedia.org/wiki/List_of_country_calling_codes ). If there is no country code,
US is assumed. Next, based on the country code it will extract as many digits as it needs.
This means for a US number it will extract exactly 10 digits. Non-digit characters are dropped.
Finally, after an optional '#', any remaining digits are used as the extention. Again,
non-digits are dropped. -}
phone_number_from_string :: String -> Either (ValidationError String) PhoneNumber
phone_number_from_string _ = Left (Other "not implemented")

{- Takes a string and tries to get a us phone number out of it. This does not support
extensions.

Take note of this edge case:
The string "+1223334444" will fail to validate becuase the inital "+1"
is stripped off, leaving only 22-333-4444. 

Function would exist in a very focused UI front end.  It should call into the
phone validation module passing the us country code "+1" if
needed. The result would check if there is an extension, and if there is one,
fail.
-}
us_phone_number_from_string :: String -> Either (ValidationError String) PhoneNumber
us_phone_number_from_string string = phone_number_from_string string

{- This would be in the UI module for presenting US specific information. While the
phone number validation module would likely contain 'phone_number_to_string', I'm not
going to assume it would allow for variations like optional parenthesis or dropping the
country code. Thus, a bespoke function for US numbers. -}
format_us_phone_number :: PhoneNumber -> String
format_us_phone_number _ = "(111) 222-3333"

{- The remaining types and functions would all reside in a 'Person' module.
At least initially. As the 'Person' module grows, it may need to be broken out
into it's own module, with the validation remainging focused on backing form
data and validating to a person. It's a pattern likely to repeat for many business
data objects: start with a single simple module and refactor out as it grows. -}

{- Primary use case is to be an unvalidated data store for a form
a user may fill out for their own info. While a user is filling out
a form, nearly anything goes: data could be empty, the SSN could contain
non-numbers, and so forth. Even the married part allows for no entry to better
enforce "must select one" and not make any assumptions.

There is little in the way of helper functions for this type as the
existing purescript record support should be enough.  One could even
make an argument that the record below should allow for extra fields.
For simplicities sake, I kept it as specified. This means if we need
to extend the type later with new fields, we are unlikely to
have unexpected conflicts.
-}
type PersonFormBacker = 
  { first_name :: String
  , last_name :: String
  , social_security_number :: String
  , married :: Maybe Boolean
  , us_phone_number :: String
  }

{- A fully validated person. Database_id may be blank so we can use this
to add a person to the database if needed. If this is a person pulled from
a database, we should have the id already.

Depending on the backing datastore, this type could be parameterized for
different database_id types. String is rather common, so again, for simplicity,
I went with that route.

This type and those functions that use only this time are the first candiates
to be refactored out to their own module
-}
type Person =
  { database_id :: Maybe String
  , first_name :: NonEmptyString
  , last_name :: NonEmptyString
  , married :: Boolean
  , social_security_number :: SocialSecurityNumber
  , us_phone_number :: PhoneNumber
  }

{- This allows for a developer to easily gather all possible errors and attach
them to a form for the correct field. The validation module will likely have
easy to_string functions for the common errors. -}
type PersonValidationFailure =
  { first_name :: Array (ValidationError Unit)
  , last_name :: Array (ValidationError Unit)
  , social_security_number :: Array (ValidationError String)
  , us_phone_number :: Array (ValidationError String)
  , married :: Array (ValidationError Unit)
  }

validate_person :: PersonFormBacker -> Either PersonValidationFailure Person
validate_person _ = Left { first_name : [], last_name : [], social_security_number : [], us_phone_number : [], married : []}

{- This basically cannot fail. -}
person_to_form_backing :: Person -> PersonFormBacker
person_to_form_backing {first_name, last_name, social_security_number, us_phone_number, married} =
  { first_name : toString first_name
  , last_name : toString last_name
  , social_security_number : format_ssn social_security_number
  , us_phone_number : format_us_phone_number us_phone_number
  , married : Just married
  }
