-- Лабораторна робота №5
-- студентки групи КН-31 підгрупи 1
-- Бикова Валентина
-- Варіант №1

-- Мета: Ознайомитись з модульною організацією програм та засобами
-- введення-виведення. Набути досвіду компіляції Haskell-програм.

-- Завдання 5.1. Реалiзувати та скомпiлювати: Переписати список справа наліво
-- 5.1.a: введення з клавіатури і результат в консолі

inConsoletoConsole :: IO()
inConsoletoConsole=do
    putStrLn "Input:"
    str<-getLine
    putStrLn(reverse str)

-- 5.1.b: дані з файлу і результат в консолі

inFiletoConsole :: IO()
inFiletoConsole=do
    str<-readFile "input.txt"
    putStrLn(reverse str)

-- 5.1.c: введення з клавіатури і результат у файлі

inConsoletoFile :: IO()
inConsoletoFile=do
    putStrLn "Input:"
    str<-getLine
    writeFile "output.txt" (reverse str)

-- 5.1.d: дані з файлу і результат у файлі

inFiletoFile :: IO()
inFiletoFile=do
    str<-readFile "input.txt"
    writeFile "output.txt" (reverse str)


-- Висновок: Ознайомились з модульною органiзацiєю програм та засобами введення-
-- виведення.