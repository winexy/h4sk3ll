type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (String, String)

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
  where 
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos


showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh



canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName


showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l


name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Patient = Patient { 
  name :: Name
  , sex :: Sex
  , age :: Int
  , height :: Int
  , weight :: Int
  , bloodType :: BloodType 
}

jackieSmith :: Patient
jackieSmith = Patient { 
  name = Name "Jackie" "Smith"
  , age = 43
  , sex = Female
  , height = 62
  , weight = 115
  , bloodType = BloodType O Neg 
}

johnDoe :: Patient 
johnDoe = Patient { 
  name = Name "John" "Doe"
  , age = 43
  , sex = Male
  , height = 72
  , weight = 124
  , bloodType = BloodType A Pos 
}

jackieSmithUpdated = jackieSmith { age = 44 }


canDonateTo2 :: Patient -> Patient -> Bool
canDonateTo2 patient1 patient2 = 
  canDonateTo (bloodType patient1) (bloodType patient2)

patientSummary :: Patient -> String
patientSummary patient = 
  "********************" ++ "\n" ++
  "Patient Name: " ++ fullName (name patient) ++ "\n" ++ 
  "Sex: " ++ gender (sex patient) ++ "\n" ++
  "Age: " ++ show (age patient) ++ "\n" ++
  "Height: " ++ show (height patient) ++ " in." ++ "\n" ++
  "Weight: " ++ show (weight patient) ++ " lbs." ++ "\n" ++
  "Blood Type: " ++ showBloodType (bloodType patient) ++ "\n" ++
  "********************"
  where 
    fullName (Name fname lname) = fname ++ ", " ++ lname
    gender Male = "Male"
    gender Female = "Female"
