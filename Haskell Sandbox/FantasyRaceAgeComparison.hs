type Name = String
type Age = Float

data Humanoid = Human Name Age | Elf Name Age | Dwarf Name Age deriving (Eq, Ord, Show)
--- Use Type Humanoid = (Name, Age, Race)

isOlder :: Humanoid -> Humanoid -> Bool

isOlder (Human _ human_age) (Elf _ elf_age)
  | human_age / 90 > elf_age / 750 = True
  | otherwise = False

isOlder (Human _ human_age) (Dwarf _ dwarf_age)
  | human_age / 90 > dwarf_age / 350 = True
  | otherwise = False

isOlder (Elf _ elf_age) (Dwarf _ dwarf_age)
  | elf_age / 350 > dwarf_age / 750 = True
  | otherwise = False

  
isOlder (Elf a b) (Human c d) = not (isOlder (Human c d) (Elf a b))
isOlder (Dwarf a b) (Human c d) = not (isOlder (Dwarf c d) (Elf a b))
isOlder (Dwarf a b) (Elf c d) = not (isOlder (Elf c d) (Dwarf a b))


-- Is there a way to reduce the repetitions? (Probably could ask the DnD group)
isOlder (Human _ a_age) (Human _ b_age)
  | a_age > b_age = True
  | otherwise = False
  
isOlder (Elf _ a_age) (Elf _ b_age)
  | a_age > b_age = True
  | otherwise = False
  
isOlder (Dwarf _ a_age) (Dwarf _ b_age)
  | a_age > b_age = True
  | otherwise = False
  
isOlder h1 h2 =
  case (h1, h2) of
    (Human _ a_age, Human _ b_age) -> (a_age > b_age)
    (Elf _ a_age, Elf _ b_age) -> (a_age > b_age)
  
getAge (Human _ a_age) = a_age
getAge (Elf _ a_age) = a_age

isOlder h1 h2 = (getAge h1) > (getAge h2)

  
  