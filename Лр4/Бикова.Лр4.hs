-- Лабораторна робота №4
-- студентки групи КН-31 підгрупи 1
-- Бикова Валентина
-- Варіант №1

-- Мета: ознайомитись з системою типів та класів типів. Набути досвіду визначення
-- нових типів та класів типів і їх використання.

-- 4.1. Фігури на площині: Визначити функцію для отримання прямокутника, який містить
-- усі фігури із задоного списку.
-- 4.2. Переміщення фігури на вказаний вектор

data Font=Consolas | SourceCode | Lucida deriving (Eq, Show)
data Figure=Circle Float Float Float | Rectangle Float Float Float Float | Triangle Float Float Float Float Float Float
              | TextBox  Float Float Font String deriving (Eq, Show)

getLet :: Font->Float
getLet Consolas=8
getLet SourceCode=10
getLet Lucida=12

getRects :: [Figure]->[Figure]
getRects []=[]
getRects ((Rectangle x1 y1 x2 y2):xs)=Rectangle x1 y1 x2 y2 : getRects xs
getRects ((Circle {}):xs)=getRects xs
getRects ((Triangle {}):xs)=getRects xs
getRects ((TextBox {}):xs)=getRects xs

displacement :: Figure->Float->Float->Figure
displacement (Rectangle x1 y1 x2 y2) qx qy=Rectangle(x1+qx)(y1+qy)(x2+qx)(y2+qy)
displacement (Circle x y r) qx qy=Circle(x+qx)(y+qy)r
displacement (TextBox x y w e) qx qy=TextBox(x+qx)(y+qy)w e
displacement (Triangle x1 y1 x2 y2 x3 y3) qx qy=Triangle(x1+qx)(y1+qy)(x2+qx)(y2+qy)(x3+qx)(y3+qy)

conjunct :: Figure->Figure
conjunct (Rectangle x1 y1 x2 y2)=Rectangle x1 y1 x2 y2
conjunct (Circle x y r)=Rectangle(x-r)(y-r)(x+r)(y+r)
conjunct (TextBox x y w e)=Rectangle x(y-getLet w)(x+fromIntegral(length e)*getLet w)y
conjunct (Triangle x1 y1 x2 y2 x3 y3)=Rectangle(min x1(min x2 x3))(max y1(max y2 y3))(max x1(max x2 x3))(min y1(min y2 y3))

conjuncts :: [Figure]->[Figure]
conjuncts=map conjunct

gMinx1 :: [Figure]->Float
gMinx1 []=0
gMinx1 [Rectangle x1 y1 x2 y2]=x1
gMinx1 ((Rectangle x1 y1 x2 y2):xs)=min x1(gMinx1 xs)

gMaxx2 :: [Figure]->Float
gMaxx2 []=0
gMaxx2 [Rectangle x1 y1 x2 y2]=x2
gMaxx2 ((Rectangle x1 y1 x2 y2):xs)=max x2(gMaxx2 xs)

gMiny1 :: [Figure]->Float
gMiny1 []=0
gMiny1 [Rectangle x1 y1 x2 y2]=y1
gMiny1 ((Rectangle x1 y1 x2 y2):xs)=min y1(gMiny1 xs)

gMaxy2 :: [Figure]->Float
gMaxy2 []=0
gMaxy2 [Rectangle x1 y1 x2 y2]=y2
gMaxy2 ((Rectangle x1 y1 x2 y2):xs)=max y2(gMaxy2 xs)

getRect :: [Figure]->Figure
getRect x=Rectangle(gMinx1(conjuncts x))(gMiny1(conjuncts x))(gMaxx2(conjuncts x))(gMaxy2(conjuncts x))

-- Результат тестування:
-- 4.1
-- Prelude> getRect [Circle 2 2 5, Restangle 0 0 5 5, Triangle 0 0 5 5 0]
-- > Rectangle (-3.0) (-3.0) 7.0 7.0
-- 4.2
-- Prelude> displacement (Rectagle 0 0 6 6) 3 3
-- > Rectangle 3.0 3.0 9.0 9.0

-- Висновок: під час даної лабораторної робооти я дізналась про нові типи та 
-- класи типів, нові зазнання використала на практиці.