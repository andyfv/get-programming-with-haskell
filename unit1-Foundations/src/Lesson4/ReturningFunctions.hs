module ReturningFunctions where


addressLetter :: (String, String) -> String -> String
addressLetter name location = locationFunction name
    where locationFunction = getLocation location

sfOffice :: (String, String) -> String
sfOffice name = 
    if    lastName < "L"
    then  nameText name ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else  nameText name ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
        
nyOffice :: (String, String) -> String
nyOffice name = nameText name ++ ": PO Box 789 - New York, NY, 10013"

renoOffice :: (String, String) -> String
renoOffice name = nameText name ++ " - PO Box 456 - Reno, NV 89523"

washOffice :: (String, String) -> String
washOffice name = nameText name ++ " - Esq"

nameText :: (String, String) -> String
nameText name = (fst name) ++ " " ++ (snd name)

getLocation :: String -> (String, String) -> String
getLocation location =
    case location of 
        "ny"   -> nyOffice
        "sf"   -> sfOffice
        "reno" -> renoOffice
        "wash" -> washOffice
        _      -> (\name -> nameText name)

compareLastName :: (String, String) -> (String, String) -> Ordering
compareLastName name1 name2 = 
    case compare lastName1 lastName2 of
        GT -> GT
        LT -> LT
        EQ -> 
            case compare firstName1 firstName2 of
                GT -> GT
                LT -> LT
                EQ -> EQ
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2


names :: [(String, String)]
names = 
    [ ("Ian", "Curtis")
    , ("Bernard","Sumner")
    , ("Peter", "Hook")
    , ("Stephen","Morris")
    ]