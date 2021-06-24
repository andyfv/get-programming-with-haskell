module CustomTypes where

type FirstName = String
type LastName  = String
type Height    = Int
type Age       = Int


patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fName lName age height = 
    name ++ " " ++ ageHeight
    where name      = lName ++ ", " ++ fName
          ageHeight = 
              "("
              ++ show age 
              ++ "yrs. " 
              ++ show height 
              ++ "in.)"



type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient


patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' patient age height =
    name ++ " " ++ ageHeight
    where name      = firstName patient ++ ", " ++ lastName patient
          ageHeight = 
              "("
              ++ show age 
              ++ "yrs. " 
              ++ show height 
              ++ "in.)"
    


data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'



data RhType    = Pos | Neg
data ABO       = A | B | AB | O
data BloodType = BloodType ABO RhType


showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABO -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _  = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False



-- Add MiddleName
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddleName FirstName MiddleName LastName


showName :: Name -> String
showName (Name f l)                 = f ++ " " ++ l
showName (NameWithMiddleName f m l) = f ++ " " ++ m ++ " " ++ l


name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddleName "Jerome" "David" "Salinger"


data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)


janeSmith :: Patient
janeSmith = Patient 
                (NameWithMiddleName "Jane" "Elizabeth" "Smith")
                Female
                30
                74 200
                (BloodType A Pos)


getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt


-- Using Record Syntax
data PatientRec = PatientRec { name      :: Name
                             , sex       :: Sex
                             , age       :: Int
                             , height    :: Int
                             , weight    :: Int
                             , bloodType :: BloodType
                             }


jackieSmith :: PatientRec
jackieSmith = PatientRec { name      = Name "Jackie" "Smith"
                         , age       = 42
                         , sex       = Female
                         , height    = 62
                         , weight    = 115
                         , bloodType = BloodType O Neg
                         }


jackieSmithUpdated = jackieSmith {age = 44}


-- 1. canDonatoTo
canDonateTo' :: PatientRec -> PatientRec -> Bool
canDonateTo' p1 p2 = canDonateTo (bloodType p1) (bloodType p2)


-- 2. patientSummary
patientSummary :: PatientRec -> String
patientSummary p = 
    pName
    ++ newline
    ++ pSex ++ newline
    ++ pAge ++ newline
    ++ pHeight ++ newline
    ++ pWeight ++ newline
    ++ pBloodType
    ++ startEnd
    where startEnd = "****************"
          newline  = "\n"
          --   
          pName = "Patient Name : " ++ showName' p
          pSex  = "Sex: " ++ showSex' p
          pAge  = "Age: " ++ showAge p
          pHeight = "Height: " ++ showHeight p
          pWeight = "Weight: " ++ showWeight p
          pBloodType = "Blood Type:" ++ showBloodType' p 


-- Show
showName' :: PatientRec -> String
showName' p = showName $ name p

showSex' :: PatientRec -> String
showSex' p = showSex $ sex p

showSex :: Sex -> String
showSex Male   = "Male"
showSex Female = "Female"

showAge :: PatientRec -> String
showAge p = show $ age p

showHeight :: PatientRec -> String
showHeight p = show $ height p

showWeight :: PatientRec -> String
showWeight p = show $ weight p

showBloodType' :: PatientRec -> String
showBloodType' p = showBloodType (bloodType p)