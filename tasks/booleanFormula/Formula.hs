module Formula where

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import BooleanSyntax
--  (Op, AssocType(FA, LA, RA, NA), Domain, arity, prec, noOp, opText, assoc, evalOp)

-- Ограничения на импорт описаны в лекции 3. Данный модуль
-- предназначен для работы с функциями любой сигнатуры, то есть с
-- любым набором связок, а не только булевыми формулами. Это
-- достигается тем, что из модуля BooleanSyntax импортируется сам тип
-- Op, но не его конструкторы T, F, Neg, And и т.д. Чтобы
-- импортировались также конструкторы Op, нужно добавить их в скобках
-- после типа, как в случае AssocType, или вообще убрать ограничение
-- на импорт.

-- Пока ограничения временно закомментированны, но поскольку этот
-- модуль предназначен для формул любой сигнатуры, его функции должны
-- работать с ограничениями на импорт из BooleanSyntax.

-- C означает "compound", т.е. "составная".

data Formula a = V a | C Op [Formula a]

-- Тип a в Formula a есть тип имен переменных. Это может быть,
-- например, Char, String или Int.

-------------------------------------------------
-- 1. Примеры формул
-------------------------------------------------

-- Функции ниже, которые отображают формулы в строки, предполагают,
-- что тип a является элементом класса Show. Таким образом, Haskell
-- знает, как печатать имена переменных. Однако show, примененная к
-- символам или строчкам возвращает строки с одинарными или двойными
-- кавычками. Например, show 'c' возвращает "'c'", а show "abc" —
-- "\"abc\"" (строка из 5 символов). Вообще show x обычно возвращает
-- такую строку, что при вводе ее в интерпретаторе снова получается x.

-- Чтобы формула печаталась без кавычек, создадим новый тип и
-- определим его членство в Show по-другому.

newtype Var = Var { varName :: Char } deriving Eq

instance Show Var where
  show (Var c) = [c]

-- Примеры формул ниже принимаются интерпретатором, если не
-- ограничивать импорт из модуля BooleanSyntax.

-- form1 = x v y -> ~z

form1 :: Formula Var
form1 = C If [C Or [V (Var 'x'), V (Var 'y')], C Neg [V (Var 'z')]]

-- form2 = x y + z <-> x v z

form2 :: Formula Var
form2 = C Iff [C Xor [C And [V (Var 'x'), V (Var 'y')], C Neg [V (Var 'z')]],
               C Or [V (Var 'x'), V (Var 'z')]]

form3 :: Formula Var
form3 = C And [V (Var 'x'), C Or [V (Var 'y'), V (Var 'z')]]

-- Задание 1. Напишите функцию correctArity, которая проверяет, что
-- арность каждого оператора, объявленная в модуле BooleanSyntax,
-- совпадает с действительным количеством его аргументов в формуле.

-- Обратите внимание, что correctArity не следует включать в другие
-- функции, описанные ниже, тем более в рекурсивные. Эти другие функции
-- работают в предположении, что формула составлена правильно.

correctArity :: Formula a -> Bool
correctArity (C op args) = and (isCorrecrArityNow:otherChecks)
  where
    isCorrecrArityNow = arity op == length args
    otherChecks = map correctArity args
correctArity _ = True

-------------------------------------------------
-- 2. Текстовое представление формул
-------------------------------------------------

-- Вспомогательная функция, которую можно вызывать в функциях
-- fullParen и showFormula ниже, если встречаются операции с арностью,
-- отличной от 0, 1 или 2.

arityError = error "Arity other than 0, 1 or 2"

-- Задание 2. Напишите функцию fullParen, которая возвращает текстовое
-- представление формулы, где каждая состовная подформула с
-- положительным числом аргументов окружена скобками. Переменные и
-- константы (то есть нульарные функции) окружать скобками не нужно.

wrapOnQuotes :: ShowS -> ShowS
wrapOnQuotes input = showChar '(' . input . showChar ')'

fullParen :: Show a => Formula a -> ShowS
fullParen (V a) = shows a
fullParen (C op [arg])
  | arity op < 1 = opText op . fullParen arg
  | otherwise = wrapOnQuotes $ opText op . fullParen arg
fullParen (C op [left, right]) = wrapOnQuotes $ fullParen left . opText op . fullParen right
fullParen _ = arityError

-- Вариант, учитывающий приоритет и ассоциативность операций

-- Скобки вокруг констант (операций арности 0) не ставятся.
-- Операции арности 1 являются префиксными или отображаются
-- специальным образом. Например, C Neg [f] отображается как ~f
-- в тексте и \overline{f} в LaTeX.

-- Инфиксные операции

-- Пусть данная формула (второй аргумент функции ниже) является левым
-- аргументом операции opExt, а главная операция формулы есть opInt.
-- Скобки вокруг формулы ставятся тогда и только тогда, когда
-- 1) приоритет opExt строго больше приоритета opInt, или
-- 2) приоритет opExt равен приоритету opInt и
-- 2а) opExt <> opInt, или
-- 2б) opExt = opInt имеет ассоциативность RA или NA.

-- ( ... opInt ... ) opExt ...

-- Если данная формула является правым аргументом opExt, то в пункте 2б)
-- нужно заменить RA на LA.

-- Задание 3. Напишите функцию showFormula, которая возвращает текстовое
-- представление формулы, где вставляются только необходимые скобки
-- согласно описанию выше.
-- Первый аргумент: оператор, находящийся непосредственно снаружи формулы
--   (внешний оператор)
-- Второй аргумент: является ли формула левым (True) или правым (False)
--   аргументом внешнего оператора
-- Третий аргумент: формула, которую нужно напечатать

-- [( ... opInt ... ) opExt ( ... opInt ... )]

showFormula :: Show a => Op -> Bool -> Formula a -> ShowS
showFormula opExt isLeft (V a) = shows a
showFormula opExt isLeft (C opInt [arg]) = opText opInt . showFormula opInt False arg
showFormula opExt isLeft (C opInt [left, right])
  | condOne || condTwo = wrapOnQuotes formulaShows
  | otherwise = formulaShows
  where
    raLaAssoc = if isLeft then RA else LA
    condOne = prec opExt > prec opInt
    condTwo = (prec opExt == prec opInt) && (condTwoA || condTwoB)
    condTwoA = opExt /= opInt
    condTwoB = opExt == opInt && (assoc opExt == raLaAssoc || assoc opExt == NA)
    formulaShows = showFormula opInt True left . opText opInt . showFormula opInt False right

-- После написания fullParen или showFormula раскоментируйте соответствующий
-- вариант объявления членства типа Formula в классе Show

instance Show a => Show (Formula a) where
--  show f = fullParen f ""
  show f = showFormula noOp True f ""

-- Например, примеры формул form1 и form2 выше должны печататься так,
-- как они записаны в комментариях перед определением.

-------------------------------------------------
-- 3. Значение формулы типа Formula Int
-------------------------------------------------

-- Значения переменных берутся из окружения (environment). Окружение
-- можно рассматривать как набор значений аргументов в одной строке
-- табличного определения функции (таблицы истинности в случае
-- двузначной логики).

-- Если f :: Formula Int, то переменные кодируются целыми числами. В
-- этом случае значения переменной с номером i (i >= 0) есть i-й
-- элемент окружения.

type Environment = [Domain]

-- Задание 4. Напишите функцию lookupVar, возвращающую значение переменной
-- с данным номером в окружении.

lookupVar :: Environment -> Int -> Domain
lookupVar e index = e !! index

-- Задание 5. Напишите функцию eval, возвращающую значение формулы
-- типа Formula Int в окружении. Значение операций определяются функцией
-- evalOp, определенной в модуле BooleanSyntax.

-- V a | C Op [Formula a]
eval :: Environment -> Formula Int -> Domain
eval e (V index) = lookupVar e index
eval e (C op args) = evalOp op $ map (eval e) args

form4 :: Formula Int
form4 = C And [V 0, C Or [V 1, V 2]]
-------------------------------------------------
-- 4. Компиляция Formula a в Formula Int
-------------------------------------------------

-- Чтобы получить значения формулы на всех возможных окружениях,
-- поступим следующим образом.
-- 1. Найдем список всех переменных, встречающихся в формуле. Каждая
-- переменная должна входить в список по одному разу. Обозначим длину
-- этого списка через n.
-- 2. Преобразуем формулу в Formula Int, заменив каждую переменную на
-- ее индекс в списке из п. 1.
-- 3. Составим список всех окружений длины n.
-- 4. Вычислим значение формулы в каждом окружении с помощью функции
-- eval.

-- Задание 6. Напишите функцию collectVars1, которая возвращает список
-- переменных, входящих в формулу. Каждая переменная входит в список
-- не более одного раза. Можно использовать функцию nub из Data.List.

collectVars1 :: Eq a => Formula a -> [a]
collectVars1 = nub . helper
  where
    helper (V a) = [a]
    helper (C _ args) = concatMap helper args

-- Задание 7. Напишите функцию varsToInt, которая принимает список
-- переменных и формулу и возвращает формулу типа Formula Int, где
-- каждая переменная заменена на ее индекс в списке. Если переменной
-- из формулы нет в списке, нужно вызывать функцию error с сообщением
-- "varsToInt: Variable occurs in the formula but not in the list".
-- Можно использовать функции из Data.List для поиска в списке.

varsToInt :: Eq a => [a] -> Formula a -> Formula Int
varsToInt vars (C op args) = C op $ map (varsToInt vars) args
varsToInt vars (V a) = V $ fromMaybe e $ elemIndex a vars
  where
    e = error "varsToInt: Variable occurs in the formula but not in the list"

-- Задание 8. Напишите функцию compileFormula с аргументом f :: Formula a.
-- Пусть vars есть список всех переменных f без повторений. Тогда
-- compileFormula возвращает пару (length vars, varsToInt vars f).

compileFormula :: Eq a => Formula a -> (Int, Formula Int)
compileFormula f = (length vars, varsToInt vars f)
  where
    vars = collectVars1 f
-------------------------------------------------
-- 5. Значения формулы на всевозможных окружениях
-------------------------------------------------

-- Задание 9. В предположении, что тип Domain является членом классов
-- Enum и Bounded, определите список domain всех элементов типа
-- Domain. Следует использовать синтаксис [x..y] для определения
-- последовательностей. Может оказаться полезным описание классов
-- Enum и Bounded в Prelude.

domain :: [Domain]
domain = [minBound..maxBound]

-- Задание 10. Напишите функцию allEnvs, которая принимает число n
-- и возвращает список всех окружений длины n в лексикографическом
-- порядке. Порядок на компонентах окружения определяется списком
-- domain.

allEnvs :: Int -> [Environment]
allEnvs n = sequence $ replicate n domain

-- Задание 11. Напишите функцию formulaValues, которая возвращает
-- значения формулы на всех наборах аргументов. В случае двузначной
-- логики это последний столбец таблицы истинности.

formulaValues :: Eq a => Formula a -> [Domain]
formulaValues f = helper $ compileFormula f
  where
    helper (nVars, fInt) = map evalByEnv $ allEnvs nVars
      where
        evalByEnv = (`eval` fInt)

-- Задание 12. Напишите функцию isConstant c f, которая определяет, является
-- ли формула f константой, принимающей значение c на всех окружениях

isConstant :: Eq a => Domain -> Formula a -> Bool
isConstant c f = allEqualC $ formulaValues f
  where
    allEqualC xs = all (== c) xs

form5 :: Formula Var
form5 = C Or [V (Var 'x'), C Neg [V (Var 'x')]]
-------------------------------------------------
-- 6. Варианты collectVars
-------------------------------------------------

-- Задание 13. Напишите функцию collectVars2, аналогичную
-- collectVars1, но использующую списочную монаду. Список уже
-- определен в Prelude как член класса Monad, поэтому можно
-- использовать функции return и >>=.

collectVars2 :: Eq a => Formula a -> [a]
collectVars2 = nub . helper
  where
    helper (V a) = return a
    helper (C _ args) = args >>= helper

-- Задание 14. Разностные списки описаны в лекции 8. Напишите функцию
-- collectVars3, аналогичную collectVars1, но использующую разностные
-- списки.

collectVars3 :: Eq a => Formula a -> [a]
collectVars3 f = nub . helper f $ []
  where
    helper (V a) = ([a] ++)
    helper (C _ args) = foldl1 (.) $ map helper args

-- Задание 15. Сделайте конструктор типов Formula членом класса Foldable,
-- определив функцию foldMap.

instance Foldable Formula where
  foldMap f (V a) = f a
  foldMap f (C _ args) = foldl1 (<>) $ map (foldMap f) args

-- Задание 16. Напишите функцию collectVars4, аналогичную collectVars1,
-- которая явно не использует рекурсию, а вместо этого использует
-- foldMap с моноидом эндоморфизмов на типе списков переменных.

collectVars4 :: Eq a => Formula a -> [a]
collectVars4 f = nub $ appEndo (foldMap (\a -> Endo ([a] ++)) f) []
