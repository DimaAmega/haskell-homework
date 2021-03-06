module BooleanFormula where

import BooleanSyntax
import Formula
import Data.List

-- Этот модуль посвящен работе с булевыми формулами в отличие от
-- Formula.hs, который работает с формулами общего вида.

-- Литература

-- Гаврилов Г.П., Сапоженко А.А. Задачи и упражнения по дискретной
-- математике. М.: Физматлит, 2005.

-- Проверка на сохранение констант: с. 72.

-- Проверка на самодвойственность: с. 64.

-- Проверка на монотонность: с. 76.

-- Проверка на линейность: с. 53, п. 2: "Метод [построения полинома
-- Жегалкина], базирующийся на преобразовании вектора значений функции".
-- Альтернативно см. https://ru.wikipedia.org/wiki/Полином_Жегалкина.
-- Раздел "Метод БПФ" описывает тот же метод, что и в задачнике.
-- Можно также использовать метод треугольника.

-- Примеры формул form1 и form2, определенные в модуле Formula,
-- доступны и здесь при условии, что импорт из BooleanSyntax в модуле
-- Formula не ограничен, то есть строка после "import BooleanSyntax" в
-- Formula.hs остается закомментированной.

-- Задание 1. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- сохраняет значение False. Обратите внимание, что сохранять False не
-- то же самое, что принимать False на всех аргументах.

preservesFalse :: Eq a => Formula a -> Bool
preservesFalse f = not $ eval allFalse fInt
  where
    allFalse = replicate varsCount False
    (varsCount, fInt) = compileFormula f

-- Задание 2. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- сохраняет значение True. Обратите внимание, что сохранять True не
-- то же самое, что принимать True на всех аргументах.

preservesTrue :: Eq a => Formula a -> Bool
preservesTrue f = eval allTrue fInt
  where
    allTrue = replicate varsCount True
    (varsCount, fInt) = compileFormula f

-- Задание 3. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- самодвойственна.

selfDual :: Eq a => Formula a -> Bool
selfDual f = outputs == (map not $ reverse outputs)
  where
    outputs = formulaValues f

-- Задание 4. Напишите функцию, которая возвращает True тогда и только
-- тогда, когда булева функция, определяемая формулой-аргументом,
-- монотонна.

monotone :: Eq a => Formula a -> Bool
monotone f = check outputs
  where
    check [] = True
    check [one] = True
    check arr = checkTwoArrays $ splitByTwo arr
    checkTwoArrays (a,b) = a `isLess` b && check a && check b
    isLess a b = and $ zipWith (<=) a b
    splitByTwo l = splitAt ((length l + 1) `div` 2) l
    outputs = formulaValues f

-- Задание 5. Это задание является необязательным. Напишите функцию,
-- которая возвращает True тогда и только тогда, когда булева функция,
-- определяемая формулой-аргументом, линейна.

linear :: Eq a => Formula a -> Bool
linear = undefined
