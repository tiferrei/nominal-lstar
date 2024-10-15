module Examples
    ( module Examples
    , module Examples.Contrived
    , module Examples.ContrivedNFAs
    , module Examples.Fifo
    , module Examples.NonResidual
    , module Examples.Residual
    , module Examples.RunningExample
    , module Examples.Adversarial
    , module Examples.Stack
    , module Examples.Mealy
    ) where

import Examples.Contrived
import Examples.ContrivedNFAs
import Examples.Fifo
import Examples.NonResidual
import Examples.Residual
import Examples.RunningExample
import Examples.Adversarial
import Examples.Stack
import Examples.Mealy
import NLambda (Atom)
import Teacher (Teacher, teacherWithTarget)

-- Wrapping it in a teacher
exampleTeacher :: Teacher Atom
exampleTeacher = teacherWithTarget example4
