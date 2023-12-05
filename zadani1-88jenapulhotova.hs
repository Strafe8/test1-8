import Data.Text (Text)

data Company = Company {name::String, employees::Int, ownerOf::[Company]}deriving(Show)
c1::Company
c1 = Company "Lethal Company" 20 []

c2 :: Company
c2=Company "Unlethal Company" 10 []

c3::Company 
c3= Company "Bogos binted" 30 [c1,c2]

data Component = TextBox {namef :: String, text :: String}
               | Button {namef :: String, value :: String}
               | Container {namef :: String, children :: [Component]}deriving(Show)

gui :: Component
gui = Container "My App" [
    Container "Menu" [
        Button "btn_new" "New",
        Button "btn_open" "Open",
        Button "btn_close" "Close"],
    Container "Body" [TextBox "textbox_1" "Some text goes here"],
    Container "Footer" []]


countAllComponents::Component -> Int 
countAllComponents (TextBox _ _) = 1
countAllComponents (Button _ _) = 1
countAllComponents(Container _ children) = 1 + sum(map (countAllComponents) (children))

isEmpty:: Component->Bool 
isEmpty (Container _ []) = True 
isEmpty _ = False

removeEmptyContainers :: Component-> Component
removeEmptyContainers(TextBox namef text) = TextBox namef text
removeEmptyContainers (Button namef value) =  Button namef value
removeEmptyContainers (Container namef children) = Container namef (filter (\x->not (isEmpty x)) (map removeEmptyContainers children))

--  Zadání 2
data Entity = Point{x::Double, y::Double}|Circle {stred::(Double,Double), r::Double}|Box {ents::[Entity]} deriving(Show)

ent1 :: Entity
ent1 = Box [(Point 2 5),(Box[Circle (1,5) 4])]

countButtons::Component->Int 
countButtons (TextBox _ _) = 0
countButtons (Button _ _) = 1
countButtons(Container _ children) = sum(map countButtons children)

copyElement::Component-> String->String->Component
copyElement gui "" _ = gui
copyElement (TextBox namef value) _ _ = TextBox namef value
copyElement (Button namef value) _ _ = Button namef value
copyElement (Container name children) strin strinx = if or(map (\x->repeatElement x strin strinx) children) then Container name ((map (\x->copyElement x strin strinx) children) ++[(Button (strin ++"_copy") strinx)])  else Container name (map (\x->copyElement x strin strinx) children)

repeatElement::Component->String->String->Bool
repeatElement (Button namef value) x y = if namef==x then True else False
repeatElement _ _ _ = False

-- Zadani 3

data TernaryTree a= Leaf a | Branch (TernaryTree a) (TernaryTree a) (TernaryTree a) deriving(Show)

stromek = Branch (Leaf 10) (Branch (Leaf 5) (Leaf 1) (Leaf 1)) (Leaf 5)

countEmptyContainers::Component->Int 
countEmptyContainers(TextBox _ _) = 0
countEmptyContainers (Button _ _) = 0
countEmptyContainers (Container _ []) = 1
countEmptyContainers (Container _ children) = sum (map(countEmptyContainers) children)



isTarget:: Component -> String ->Bool
isTarget (Button name _) target = if name == target then True else False 
isTarget _ _ = False

removeButton::Component->String->Component
removeButton gui "" = gui
removeButton (TextBox a b) _ = TextBox a b
removeButton (Button name b) _ = Button name b
removeButton (Container name children) target= Container name (filter (\x->not(isTarget x target )) (map (\x->removeButton x target)children))

-- Asi 4
data Attribute = Attribute { name'::String, value'::String}deriving(Show)
data Tag = Tag {name''::String, cha::[Attribute], cht::[Tag]}deriving(Show)
data HTML = HTML {childrentag::[Tag]}deriving(Show)


stranka = HTML [(Tag "body" [(Attribute "class" "heavy")] [])]

listAllButtons::Component->[String]
listAllButtons (TextBox _ _) = []
listAllButtons (Button name value) = [name]
listAllButtons (Container _ children) = concatMap (listAllButtons) children

changeText::Component->String ->String ->Component
changeText (Button name value) nazev novyvalue = if name==nazev then Button nazev novyvalue else Button name value
changeText (TextBox name value) nazev novyvalue = if name==nazev then Button nazev novyvalue else TextBox name value
changeText (Container name children) nazev novyvalue = Container name (map (\x->changeText x nazev novyvalue) children)

-- Zadani 5
data Element = Button' {name'::String, text'::String} |Text {name'::String,text'::String} | Panel [Element] deriving (Show)

prikladelement :: Element
prikladelement = Panel [(Text "Uvod" "Toto je uvod"),(Button' "Odejit" "ze stranky"),(Panel [(Text "text tady" "text tu")])]


data Component = 
    TextBox {name::String,text::String}
    |Button {name::String, value::String}
    |Container {name::String, children ::[Component]} deriving(Show)


gui::Component
gui = Container "My App" [
        Container "Menu" [
            Button "btn_new" "New",
            Button "btn_open" "Open",
            Button "btn_close" "Close"],
        
        Container "Body" [TextBox "textbox_1" "Some text goes here"],
        Container "Footer" []] 

listAllButtons ::Component -> [Component]
listAllButtons (Button name value) = [(Button name value)]
listAllButtons (TextBox name value) = []
listAllButtons (Container _ children) = concatMap (listAllButtons) (children)

isButton::Component -> Bool 
isButton (Button _ _) = True
isButton _ = False

removeAllButtons :: Component ->Component
removeAllButtons (Button name value) = Button name value
removeAllButtons (TextBox name value) = TextBox name value
removeAllButtons (Container name children) = Container name (filter (\x->not(isButton x)) (map (removeAllButtons) children))

-- Zadani 6
data Article = Text String|Section String [Article] deriving(Show)

clanek :: Article
clanek= Section "clanek1" [Text "tVOJE MAMAMAAMAMAMAMAAM"]
listAllNames::Component->[String]

listAllNames (TextBox name _) = [name]
listAllNames (Button name _) = [name]
listAllNames (Container name children) = name:(concatMap (listAllNames) children)

isTarget'::Component ->[String] ->Bool 
isTarget' (Container name _) x = if elem name x  then True else False 
isTarget' (TextBox name _) x = if elem name x then True else False 
isTarget' (Button name _) x = if elem name x then True else False 

removeAllElements::Component-> [String] ->Component
removeAllElements gui [] = gui
removeAllElements (TextBox name value) _ = TextBox name value
removeAllElements (Button name value) _ = Button name value
removeAllElements (Container name children) target = Container name (filter (\x-> not(isTarget' x target)) (map (\x-> removeAllElements x target) children) )

data Attribute' = Attribute' { name''''::String, value''::String}deriving(Show)
data Tag' = Tag' {name'''::String, cha'::[Attribute], cht'::[Tag]}deriving(Show)
data HTML' = HTML' {childrentag'::[Tag]}deriving(Show)


isTarget''::Component ->String ->Bool 
isTarget'' (Container name _) x = if name == x  then True else False 
isTarget'' (TextBox name _) x = if name == x then True else False 
isTarget'' (Button name _) x = if name == x then True else False 



printPath::Component->String->String
printPath gui "" = []
printPath (TextBox name _) x = if name == x then name else [] 
printPath (Button name _) x = if name == x then name else [] 
printPath (Container name children) target = if any (\x -> isTarget'' x target) children then name ++ "/" ++ target else concatMap (\x -> printPath x target) children



data DynamicTree a = Leaf' a | Branch' [DynamicTree a]deriving(Show)



tree1::DynamicTree Int 
tree1 = Branch' [Leaf' 10,Branch'[Leaf' 5, Leaf' 3]]

data FileType = Image|Executable|SourceCode|TextFile
data Entry = File {namef'::String,size'::Int,ftype'::FileType} 
    |Directory'  {namef'::String,entries'::[Entry]}
root::Entry
root = Directory' "root" [File "logo.jpg" 5000 Image, Directory' "classes" [File "notes-fpr.txt" 200 TextFile,File "presentation.jpg" 150 Image,File "first_test.hs" 20 SourceCode]]

countFiles::Entry->Int 
countFiles(File _ _ _) = 1
countFiles(Directory' _ entries) = sum(map countFiles entries ) 

fullNames:: Entry -> [String]
fullNames (File namef _ _) = ["/"++namef]
fullNames (Directory' namef entries) =  (map (\x->"/"++namef++x)(concatMap (fullNames) entries))