
{-# LANGUAGE ImplicitParams #-}
import Data.List

-- Решение контрольной нужно написать в отдельном файле midterm.hs и
-- загрузить по ссылке
-- https://www.dropbox.com/request/4iXot3SndQT4fJdcKhn7 не позже
-- 25.04.2021. При загрузке на сайте Dropbox нужно указать сначала
-- фамилию, затем имя на русском языке. Правильный адрес электронной
-- почты на сайте указывать не обязательно, так как Dropbox не делает
-- его доступным автору запроса.

-- При написании требуемых функций можно использовать вспомогательные
-- функции.  Для каждой функции нужно написать ее тип. Если для
-- понимания кода нужно думать хотя бы 10 секунд, нужно также написать
-- небольшой комментарий, говорящий, что делает функция и как. С
-- использованием комментария человек, знающий Haskell, должен иметь
-- возможность разобраться в работе функции не более, чем за две
-- минуты. Но это не значит, что код нужно упрощать в ущерб краткости
-- и элегантности.

-- Использование подходящих функций из модулей Prelude и Data.List
-- стандартной библиотеки ожидается и приветствуется. Также
-- приветствуется разумное использование функций высших порядков,
-- генераторов списков, бесточечной записи и других возможностей
-- Haskell.  Код будет оцениваться в первую очередь по элегантности и
-- соответствию стилю Haskell и во вторую очередь по эффективности.

-- Каждое из пяти заданий оценивается максимум в 6 баллов.
-- Оценка за контрольную будет составлять 30% итоговой оценки за курс.

-- Можно пользоваться документацией Haskell и литературой, указанной
-- на сайте.  Нельзя обсуждать решения с другими и нельзя искать
-- решения в Интернете.

-- 1. Прочитайте определение совершенного числа в статье
-- https://ru.wikipedia.org/wiki/Совершенное_число.


-- Напишите следующие функции.

{-------------------------------------------------------------------------
                                ЗАДАЧА 1
    Обе функции на аргументе n > 1 должны возвращает список натуральных
    делителей n (в любом порядке), отличных от самого n. Функция
    properDivisors1 не должна явно использовать рекурсию. Функция
    properDivisors2, наоборот, должна быть рекурсивной. Она должна
    перебирать возможные делители от 1 до sqrt(n), причем этот интервал
    следует проходить только один раз. Можно (но не обязательно)
    использовать функцию isqrt ниже, которая возвращает квадратный
    корень из n, округленный вниз.

    С помощью properDivisors2 напишите предикат perfect :: Integer ->
    Bool, который проверяет, является ли число n > 1 совершенным, и
    функцию findPerfects :: Integer -> [Integer], которая возвращает
    список всех совершенных чисел до n включительно. Функции perfect и
    findPerfects не должны использовать рекурсию. Найдите все
    совершенные числа от 1 до 10000.
-------------------------------------------------------------------------
    Для properDivisors1 будем использовать List comprehension 
    https://wiki.haskell.org/List_comprehension Также внутри 
    helper используется механизм сопоставления шаблону, оператор 
    аппликации '$' нулевого приоритета и оператор свертки '.'
--------------------------------------------------------------------------}

properDivisors1 :: Integer -> [Integer]
properDivisors1 n = [x | x <- [1..(n-1)], n `rem` x == 0]

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

properDivisors2 :: Integer -> [Integer]
properDivisors2 n = sort . helper $ candidates
  where
    candidates = [x | x <- [2..(isqrt n)]]
    helper :: [Integer] -> [Integer]
    helper [] = [1]
    helper (candidate:xs)
      | n `rem` candidate == 0 = candidate:(n `div` candidate):(helper xs)
      | otherwise = helper xs

perfect :: Integer -> Bool
perfect n
  | sumOfDeviders n == n = True
  | otherwise = False
  where
    sumOfDeviders :: Integer -> Integer
    sumOfDeviders = sum . properDivisors2

findPerfects :: Integer -> [Integer]
findPerfects n = [x | x <- [1..n], perfect x]

-- findPerfects 10000
-- [1,6,28,496,8128]

{-------------------------------------------------------------------------
                                ЗАДАЧА 2
    2. Без использования рекурсии напишите функцию
    abbrev :: String -> String, которая в заданном списке имен людей
    выполняет сокращение всех имен, кроме фамилии, до инициалов.
    Фамилией считается последнее слово. Например:

    > abbrev "Синицин"
    "Синицин"
    > abbrev "Сергей Есенин"
    "С.Есенин"
    abbrev "Игорь Федорович Поддубный"
    "И.Ф.Поддубный"
    abbrev "Иоганн Хризостом Вольфганг Амадей Моцарт"
    "И.Х.В.А.Моцарт"
    Можно использовать функцию words из Prelude.
-------------------------------------------------------------------------
    Для abbrev будем использовать левую свертку foldl
--------------------------------------------------------------------------}
abbrev :: String -> String
abbrev input = helper . words $ input
  where
    helper :: [String] -> String
    helper list = foldl (++) "" (makeWordsShort firstWords) ++ lastWord
      where
        lastWord = last list
        firstWords = init list
        makeWordsShort :: [String] -> [String]
        makeWordsShort = map (\word -> [head word] ++ ".")

-- 3. См. файл midterm-problem3.pdf в source.unn.ru.

makeAlterSigned :: [Double] -> [Double]
makeAlterSigned = zipWith (*) $ cycle [1, -1]

oddSeries :: [Double]
oddSeries = [1,3..]

anSeries :: [Double]
anSeries = map ((/) 1) oddSeries

approxPi :: [Double]
approxPi = map ((*) 4) partSumPi
  where
    partSumPi = scanl1 (+) (makeAlterSigned anSeries)

delta :: [Double] -> [Double]
delta an = zipWith (-) (tail an) an

firstDiffSeries :: [Double] -> [Double]
firstDiffSeries = takeFirst . diffComposition
  where
    diffComposition :: [Double] -> [[Double]]
    diffComposition = iterate delta
    takeFirst :: [[Double]] -> [Double]
    takeFirst = map head

euler :: [Double] -> [Double]
euler an = unfoldr func (1, firstDiffSeries an)
  where
    func (den,(fd:fdx)) = Just (den / 2 * fd, (den / 2, fdx))

fastApproxPi :: [Double]
fastApproxPi = map ((*) 4) partSumPi
  where
    partSumPi = scanl1 (+) (makeAlterSigned . euler $ anSeries)

-- 4. Сделайте упражнение 6 из домашнего задания в lec07.hs
-- (написать функцию applyDistr :: Exp -> Exp). Включать
-- в решение функцию eval не обязательно.

-- 5. Функцию foldl можно выразить через foldr следующим образом.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (\x g y -> g (f y x)) id xs z

-- Считая типы a и b фиксированными, выпишите типы всех подвыражений
-- правой части myFoldl. Типы должны быть такими, какие
-- соответствующие выражения имеют именно в данном определении. Так,
-- недостаточно просто узнать тип foldr в описании Prelude или в
-- интерпретаторе, поскольку там указан наиболее общий тип.

-- 6. Напишите функцию concatS :: [ShowS] -> String, которая возвращает
-- конкатенацию всех строк, представленных разностными списками.
-- Определение должно использовать foldMap и моноид эндоморфизмов.
