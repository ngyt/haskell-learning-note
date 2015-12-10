--
-- Getting information in Person data type with functions.

--data Person = Person String String Int Float String String deriving (Show)

--firstName :: Person -> String
--firstName (Person firstName _ _ _ _ _) = firstName

--lastName :: Person -> String
--lastName (Person _ lastName _ _ _ _) = lastName

--age :: Person -> Int
--age (Person _ _ age _ _ _) = age

--height :: Person -> Float
--height (Person _ _ _ height _ _) = height

--phoneNumber :: Person -> String
--phoneNumber (Person _ _ _ _  phoneNumber _) = phoneNumber

--flavor :: Person -> String
--flavor (Person _ _ _ _ _ flavor) = flavor

-- 
-- Using record syntax
data Person = Person { firstName :: String,
						lastName  ::String,
						age :: Int,
						height :: Float,
						phoneNumber :: String,
						flavor :: String 
						} deriving (Show)

-- Another example
data Car = Car { brand :: String, model :: String, year :: Int } deriving (Show)