module Tests where

import Test.HUnit

import Tarefa1_2021li1g038_Spec
import Tarefa2_2021li1g038_Spec
import Tarefa3_2021li1g038_Spec
import Tarefa4_2021li1g038_Spec
import Tarefa6_2021li1g038_Spec

runTestsT1 = runTestTT testsT1
runTestsT2 = runTestTT testsT2
runTestsT3 = runTestTT testsT3
runTestsT4 = runTestTT testsT4
runTestsT6 = runTestTT testsT6

runAllTests = runTestTT $ TestList [testsT1, testsT2, testsT3, testsT4, testsT6]