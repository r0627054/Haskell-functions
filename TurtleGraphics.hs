----------
--DEEL 1--
----------

--Taak 1a.
data Turtle = Done | Turn Double Turtle | Step Double Turtle
               deriving (Show,Eq)
--Taak 1b.
done::Turtle
done = Done

turn:: Double -> Turtle
turn x = Turn x Done

step:: Double -> Turtle
step x = Step x Done

--taak 1c.
(>>>)::Turtle ->Turtle -> Turtle
(>>>) Done t2 = t2
(>>>) (Turn x Done) t2 = Turn x t2
(>>>) (Step x Done) t2 = Step x t2
(>>>) (Turn x ts) t2 = Turn x  (ts >>> t2)
(>>>) (Step x ts) t2 = Step x (ts >>> t2)

square:: Turtle
square = stepTurn >>> stepTurn >>> stepTurn >>> step
        where stepTurn = Step 50 (Turn 90 Done)
              step = Step 50 Done

----------
--DEEL 2--
----------

type Point = (Double,Double)
type Line  = (Point,Point)

--taak 2a.
turtleToLines:: Turtle -> [Line]
turtleToLines x =ç turtleToLinesHelper (500,500) x 0 []

turtleToLinesHelper:: Point -> Turtle -> Double-> [Line] -> [Line]
turtleToLinesHelper _ Done _ ls = ls
turtleToLinesHelper point (Turn ang ts) currentAngle ls= turtleToLinesHelper point ts (currentAngle+ang) ls
turtleToLinesHelper (x,y) (Step distance ts) currentAngle ls= turtleToLinesHelper (newX,newY) ts currentAngle (ls ++ [((x,y),(newX,newY))])
                   where newX = x + (distance * (sin ra))
                         newY = y + (distance * (cos ra))
                         ra   = currentAngle* ((2*pi)/360)


--taak 2b.

linesToSVG:: [Line] -> String
linesToSVG lines = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
                   ++ linesToSVGHelper lines
                   ++ "</svg>"


linesToSVGHelper:: [Line] -> String
linesToSVGHelper [] = ""
linesToSVGHelper ( ( (x1,y1),(x2,y2) ) :xs) = "<line x1=\"" ++ (show x1) ++ "\" y1=\"" ++ (show y1)
                                              ++ "\" x2=\"" ++ (show x2) ++ "\" y2=\"" ++ (show y2)
                                              ++ "\" stroke=\"blue\" stroke-width=\"4\"\n" ++ (linesToSVGHelper xs)
--taak 2c
writeSVG:: FilePath -> Turtle -> IO()
writeSVG path t = writeFile path (linesToSVG $ turtleToLines t)

----------
--DEEL 3--
----------

--taak 3a
data Fractal  = D | T Double Fractal | S Fractal
     deriving (Show, Eq)
--taak 3b
textttfdone::Fractal
textttfdone = D

fturn:: Double -> Fractal
fturn x = T x D

fstep:: Fractal
fstep = S D

(>->)::Fractal -> Fractal -> Fractal
(>->) D t2 = t2
(>->) (T x D) t2 = T x t2
(>->) (S D) t2 = S t2
(>->) (T x ts) t2 = T x  (ts >-> t2)
(>->) (S ts) t2 = S  (ts >-> t2)

--taak 3c
concretize::Double -> Fractal -> Turtle
concretize x D =  Done
concretize x (T d f) = Turn d (concretize x f)
concretize x (S f) = Step x (concretize x f)

--taak 3d.
expansion =
  fstep >-> fturn (60)   >->
  fstep >-> fturn (-120) >->
  fstep >-> fturn (60)   >->
  fstep

program =
  fstep >-> fturn (-120) >->
  fstep >-> fturn (-120) >->
  fstep

myResult = expansion >-> fturn (-120) >->
           expansion >-> fturn (-120) >->
           expansion

--taak 3d.
refine:: Fractal -> Fractal -> Fractal
refine expansion D           = D
refine expansion (T x fract) = T x (refine expansion fract)
refine expansion (S x)  = expansion >-> (refine expansion x)

--taak 3e.
times:: Int -> (a->a) -> (a->a)
times 0 f x = x
times n f x = times (n-1) f (f x)

--taak 3f.
exam:: Fractal -> Fractal -> Int -> Double -> FilePath -> IO ()
exam program expansion n d filename = writeSVG filename (concretize d (times n (refine expansion) program))
