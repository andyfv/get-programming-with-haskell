import qualified Data.Map as Map

type UserId      = Int
type UserName    = String
type UserCredits = Int
-- 
type WillCoId    = Int


userNameDB :: Map.Map UserId UserName
userNameDB = Map.fromList [ (1,"nYarlathoTep") 
                          , (2,"KINGinYELLOW") 
                          , (3,"dagon1997") 
                          , (4,"rcarter1919") 
                          , (5,"xCTHULHUx") 
                          , (6,"yogSOThoth")
                          ]


userCreditsDB :: Map.Map UserName UserCredits
userCreditsDB = Map.fromList [ ("nYarlathoTep",2000) 
                             , ("KINGinYELLOW",15000) 
                             , ("dagon1997",300) 
                             , ("rcarter1919",12) 
                             , ("xCTHULHUx",50000) 
                             , ("yogSOThoth",150000)
                             ]


creditsFromId :: UserId -> Maybe UserCredits
creditsFromId id = lookupUserCreditsAlt (lookupUserName id)

lookupUserName :: UserId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupUserCredits :: UserName -> Maybe UserCredits
lookupUserCredits username = Map.lookup username userCreditsDB

lookupUserCreditsAlt :: Maybe UserName -> Maybe UserCredits
lookupUserCreditsAlt Nothing         = Nothing
lookupUserCreditsAlt (Just username) = Map.lookup username userCreditsDB

--  f       a     -> (   a     ->    f        b    ) ->   f        b
-- Maybe UserName -> (UserName -> Maybe UserCredits) -> Maybe UserCredits
-- f a -> (a -> f b) -> f b


creditsFromIdMonad :: UserId -> Maybe UserCredits
creditsFromIdMonad id = lookupUserName id >>= lookupUserCredits

-- 

userIdDB :: Map.Map WillCoId UserId
userIdDB = Map.fromList [(1001,1)
                        ,(1002,2)
                        ,(1003,3)
                        ,(1004,4)
                        ,(1005,5)
                        ,(1006,6)
                        ]

lookupUserId :: WillCoId -> Maybe UserId
lookupUserId id = Map.lookup id userIdDB


creditsFromWCId :: WillCoId -> Maybe UserCredits
creditsFromWCId id = lookupUserId id 
                       >>= lookupUserName 
                       >>= lookupUserCredits
