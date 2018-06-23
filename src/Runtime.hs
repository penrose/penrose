-- | "Runtime" contains the optimization algorithm for diagram layout
--  and also the Gloss GUI, now functional bot deprecated.

{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- for autodiff, requires passing in a polymorphic fn

module Runtime where
import Utils
import Shapes
import Functions
import Computation
import Data.Set (fromList)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Aeson
import Data.Function
import Numeric.AD
import GHC.Float -- float <-> double conversions
import System.IO
import System.Exit
import System.Environment
import System.Random
import Debug.Trace
import Data.Dynamic
import Data.Typeable
import Data.Data -- TODO remove extra dynamic/typeable/data imports and deriving everywhere if not used
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import qualified Substance as C
import qualified Style as S
       -- (subPrettyPrint, styPrettyPrint, subParse, styParse)
       -- TODO limit export/import
import qualified Text.Megaparsec as MP (runParser, parseErrorPretty)

------ Types and type synonyms

data LastEPstate = EPstate [Obj] deriving (Eq, Show, Typeable)

data OptStatus = NewIter -- TODO should this be init with a state?
               | UnconstrainedRunning LastEPstate -- [Obj] stores last EP state
               | UnconstrainedConverged LastEPstate -- [Obj] stores last EP state
               | EPConverged
               deriving (Eq, Show, Typeable)

data Params = Params { weight :: Double,
                       optStatus :: OptStatus,
                       objFn :: forall a. ObjFnPenaltyState a,
                       annotations :: [[Annotation]]
                     } -- deriving (Eq, Show) -- TODO derive Show instance

-- State of the runtime system
data State = State { objs :: [Obj]
                   , constrs :: [C.SubConstr]
                   , comps :: forall a. (Autofloat a) => [ObjComp a]
                   , rng :: StdGen -- random number generator
                   , autostep :: Bool -- automatically step optimization or not
                   , params :: Params
                   }  deriving (Typeable)

-- | Datatypes for computation. ObjComp is gathered in pre-compilation and passed to functions that evaluate the computation.
-- | object name, function name, list of args
data ObjComp a = ObjComp {
    oName    :: Name,      -- "A"
    oProp    :: Name,      -- "radius"
    fnName   :: Name,      -- computeRadius
    fnParams :: [TypeIn a] -- (1.2, B)
} deriving (Show, Typeable)

------

calcTimestep :: Float -- for use in forcing stepping in handler
calcTimestep = 1 / int2Float stepsPerSecond

initRng :: StdGen
initRng = mkStdGen seed
    where seed = 16 -- deterministic RNG with seed

objFnNone :: ObjFnPenaltyState a
objFnNone objs w f v = 0

initParams = Params { weight = initWeight, optStatus = NewIter, objFn = objFnNone, annotations = [] }

----------------------- Unpacking
data Annotation = Fix | Vary deriving (Eq, Show, Typeable)
type Fixed a = [a]
type Varying a = [a]

-- make sure the unpacking matches the object packing in terms of number and order of parameters
-- annotations are specified inline here. this is per type, not per value (i.e. all circles have the
-- same fixed parameters). but you could generalize it to per-value by adding or overriding
--annotations globally after the unpacking does not unpack names
unpackObj :: (Autofloat a) => Obj' a -> [(a, Annotation)]
-- the location of a circle and square can vary
unpackObj (C' c) = [(xc' c, Vary), (yc' c, Vary), (r' c, Vary)]
unpackObj (E' e) = [(xe' e, Vary), (ye' e, Vary), (rx' e, Vary), (ry' e, Vary)]
unpackObj (S' s) = [(xs' s, Vary), (ys' s, Vary), (side' s, Vary)]
unpackObj (AR' ar) = [(xar' ar, Vary), (yar' ar, Vary) , (sizear' ar, Vary),
                      (radiusar' ar, Vary), (rotationar' ar, Vary), (anglear' ar, Fix)]
unpackObj (R' r) = [(xr' r, Vary), (yr' r, Vary), (sizeX' r, Vary), (sizeY' r, Vary)]
unpackObj (PA' pa) = [(xpa' pa, Vary), (ypa' pa, Vary), (sizeXpa' pa, Vary), (sizeYpa' pa, Vary)]
-- the location of a label can vary, but not its width or height (or other attributes)
unpackObj (L' l) = [(xl' l, Vary), (yl' l, Vary), (wl' l, Fix), (hl' l, Fix)]
-- the location of a point varies
unpackObj (P' p) = [(xp' p, Vary), (yp' p, Vary)]
-- TODO revert this!! Hack just for surjection program
unpackObj (A' a) = [(startx' a, Vary), (starty' a, Vary),
                    (endx' a, Vary), (endy' a, Vary), (thickness' a, Vary)]
-- unpackObj (CB' c) = [(pathcb' cb, Fix)]
unpackObj (CB' c) = concatMap (\(x, y) -> [(x, Fix), (y, Fix)]) $ pathcb' c
unpackObj (LN' a) = [(startx_l' a, Fix), (starty_l' a, Fix),
                    (endx_l' a, Fix), (endy_l' a, Fix), (thickness_l' a, Fix)]
unpackObj (IM' im) = [(xim' im, Vary), (yim' im, Vary), (sizeXim' im, Vary), (sizeYim' im, Vary)]
-- split out because pack needs this annotated list of lists
unpackAnnotate :: (Autofloat a) => [Obj' a] -> [[(a, Annotation)]]
unpackAnnotate objs = map unpackObj objs

-- TODO check it preserves order
splitFV :: (Autofloat a) => [(a, Annotation)] -> (Fixed a, Varying a)
splitFV annotated = foldr chooseList ([], []) annotated
        where chooseList :: (a, Annotation) -> (Fixed a, Varying a) -> (Fixed a, Varying a)
              chooseList (x, Fix) (f, v) = (x : f, v)
              chooseList (x, Vary) (f, v) = (f, x : v)

-- optimizer should use this unpack function
-- preserves the order of the objects’ parameters
-- e.g. unpackSplit [Circ {xc varying, r fixed}, Label {xl varying, h fixed} ] = ( [r, h], [xc, xl] )
-- crucially, this does NOT depend on the annotations, it can be used on any list of objects
unpackSplit :: (Autofloat a) => [Obj' a] -> (Fixed a, Varying a)
unpackSplit objs = let annotatedList = concat $ unpackAnnotate objs in
                   splitFV annotatedList

---------------------- Packing

-- We put `Floating a` into polymorphic objects for the autodiff.
-- (Maybe port all objects to polymorphic at some point, but would need to zero the gradient information.)
-- Can't use realToFrac here because it will zero the gradient information.
-- TODO use DuplicateRecordFields (also use `stack` and fix GLUT error)--need to upgrade GHC and gloss

-- TODO comment packing these functions defining conventions
curvePack :: (Autofloat a) => CubicBezier -> [a] -> CubicBezier' a
-- param is an ordered list of control point coordinates: [x1, y1, x2, y2 ...]
curvePack c params = CubicBezier' { pathcb' = path, namecb' = namecb c, colorcb' = colorcb c, stylecb' = stylecb c }
         where path = map tuplify2 $ chunksOf 2 params

solidArrowPack :: (Autofloat a) => SolidArrow -> [a] -> SolidArrow' a
solidArrowPack arr params = SolidArrow' { startx' = sx, starty' = sy, endx' = ex, endy' = ey, thickness' = t,
                namesa' = namesa arr, selsa' = selsa arr, colorsa' = colorsa arr, stylesa' = stylesa arr }
         where (sx, sy, ex, ey, t) = if not $ length params == 5 then error "wrong # params to pack solid arrow"
                            else (params !! 0, params !! 1, params !! 2, params !! 3, params !! 4)

linePack :: (Autofloat a) => Line -> [a] -> Line' a
linePack ln params = Line' { startx_l' = sx, starty_l' = sy, endx_l' = ex, endy_l' = ey, thickness_l' = t,
                name_l' = name_l ln, color_l' = color_l ln, style_l' = style_l ln }
         where (sx, sy, ex, ey, t) = if not $ length params == 5 then error "wrong # params to pack line"
                            else (params !! 0, params !! 1, params !! 2, params !! 3, params !! 4)

circPack :: (Autofloat a) => Circ -> [a] -> Circ' a
circPack cir params = Circ' { xc' = xc1, yc' = yc1, r' = r1, namec' = namec cir, colorc' = colorc cir, stylec' = stylec cir, strokec' = r2f $ strokec cir }
         where (xc1, yc1, r1) = if not $ length params == 3
                                then error $ "wrong # params to pack circle: expected 3, got " ++ show (length params)
                                else (params !! 0, params !! 1, params !! 2)

ellipsePack :: (Autofloat a) => Ellipse -> [a] -> Ellipse' a
ellipsePack e params = Ellipse' { xe' = xe1, ye' = ye1, rx' = rx1, ry' = ry1, namee' = namee e,
                                  colore' = colore e }
         where (xe1, ye1, rx1, ry1) = if not $ length params == 4 then error "wrong # params to pack circle"
                                else (params !! 0, params !! 1, params !! 2, params !! 3)

sqPack :: (Autofloat a) => Square -> [a] -> Square' a
sqPack sq params = Square' { xs' = xs1, ys' = ys1, side' = side1, names' = names sq, colors' = colors sq, ang' = ang sq, styles' = styles sq, strokes' = r2f $ strokes sq}
         where (xs1, ys1, side1) = if not $ length params == 3 then error "wrong # params to pack square"
                                else (params !! 0, params !! 1, params !! 2)

arPack :: (Autofloat a) => Arc -> [a] -> Arc' a
arPack ar params = Arc' { xar' = xar1, yar' = yar1, sizear' = sizear1,
                                radiusar' = radiusar1, rotationar' = rotationar1,
                                 anglear' = anglear1, namear' = namear ar, selar' = selar ar, colorar' = colorar ar,
                                 isRightar' = isRightar ar, stylear' = stylear ar}
         where (xar1, yar1, sizear1, radiusar1, rotationar1, anglear1) = if not $ length params == 6 then error "wrong # params to pack angleMark"
                                else (params !! 0, params !! 1, params !! 2, params !! 3, params !! 4, params !! 5)

rectPack :: (Autofloat a) => Rect -> [a] -> Rect' a
rectPack rct params = Rect' { xr' = xs1, yr' = ys1, sizeX' = len, sizeY' = wid, namer' = namer rct,
                              selr' = selr rct, colorr' = colorr rct, angr' = angr rct}
         where (xs1, ys1, len, wid) = if not $ length params == 4 then error "wrong # params to pack rect"
                                else (params !! 0, params !! 1, params !! 2, params !! 3)

parallelogramPack :: (Autofloat a) => Parallelogram -> [a] -> Parallelogram' a
parallelogramPack pa params = Parallelogram' { xpa' = xpa1, ypa' = ypa1, sizeXpa' = len, sizeYpa' = wid, namepa' = namepa pa,
                              selpa' = selpa pa, colorpa' = colorpa pa, anglepa' = anglepa pa, rotationpa' = rotationpa pa}
         where (xpa1, ypa1, len, wid) = if not $ length params == 4 then error "wrong # params to pack parallelogram"
                                else (params !! 0, params !! 1, params !! 2, params !! 3)

ptPack :: (Autofloat a) => Pt -> [a] -> Pt' a
ptPack pt params = Pt' { xp' = xp1, yp' = yp1, namep' = namep pt, selp' = selp pt }
        where (xp1, yp1) = if not $ length params == 2 then error "Wrong # of params to pack point"
                           else (params !! 0, params !! 1)

labelPack :: (Autofloat a) => Label -> [a] -> Label' a
labelPack lab params = Label' { xl' = xl1, yl' = yl1, wl' = wl1, hl' = hl1,
                             textl' = textl lab, sell' = sell lab, namel' = namel lab }
          where (xl1, yl1, wl1, hl1) = if not $ length params == 4 then error "wrong # params to pack label"
                                   else (params !! 0, params !! 1, params !! 2, params !! 3)

imgPack :: (Autofloat x) => Img -> [a] -> Img' a
imgPack im params = Img' { xim' = xi1, yim' = yi1, sizeXim' = len, sizeYim' = wid,
                           nameim' = nameim im, selim' = selim im, angim' = angim im,
                          path' = path im}
          where (xi1, yi1, len, wid) = if not $ length params == 4 then error "wrong # of params to pack image"
                                      else (params !! 0, params !! 1, params !! 2, params !! 3)
-- does a right fold on `annotations` to preserve order of output list
-- returns remaining (fixed, varying) that were not part of the object
-- e.g. yoink [Fixed, Varying, Fixed] [1, 2, 3] [4, 5] = ([1, 4, 2], [3], [5])
yoink :: (Show a) => [Annotation] -> Fixed a -> Varying a -> ([a], Fixed a, Varying a)
yoink annotations fixed varying = --trace ("yoink " ++ (show annotations) ++ (show fixed) ++ (show varying)) $
      case annotations of
       [] -> ([], fixed, varying)
       (Fix : annotations') -> let (params, fixed', varying') = yoink annotations' (tail fixed) varying in
                               (head fixed : params, fixed', varying')
       (Vary : annotations') -> let (params, fixed', varying') = yoink annotations' fixed (tail varying) in
                                (head varying : params, fixed', varying')

-- used inside overall objective fn to turn (fixed, varying) back into a list of objects
-- for inner objective fns to operate on
-- pack is partially applied with the annotations, which never change
-- (the annotations assume the state never changes size or order)
pack :: (Autofloat a) => [[Annotation]] -> [Obj] -> Fixed a -> Varying a -> [Obj' a]
pack annotations objs = pack' (zip objs annotations)

pack' :: (Autofloat a) => [(Obj, [Annotation])] -> Fixed a -> Varying a -> [Obj' a]
pack' zipped fixed varying =
     case zipped of
      [] -> []
      ((obj, annotations) : zips) -> res : pack' zips fixed' varying' -- preserve order
     -- use obj, annotations, fixed, and varying to create the object
     -- by yoinking the right params out of f/v in the right order
        where (flatParams, fixed', varying') = yoink annotations fixed varying
           -- is flatParams in the right order?
              res = case obj of
                 -- pack objects using the names, text params carried from initial state
                 -- assuming names do not change during opt
                    C circ  -> C' $ circPack circ flatParams
                    E elli  -> E' $ ellipsePack elli flatParams
                    L label -> L' $ labelPack label flatParams
                    P pt    -> P' $ ptPack pt flatParams
                    S sq    -> S' $ sqPack sq flatParams
                    AR ar    -> AR' $ arPack ar flatParams
                    R rect  -> R' $ rectPack rect flatParams
                    PA parallelogram  -> PA' $ parallelogramPack parallelogram flatParams
                    A ar    -> A' $ solidArrowPack ar flatParams
                    CB c    -> CB' $ curvePack c flatParams
                    LN c    -> LN' $ linePack c flatParams
                    IM im   -> IM' $ imgPack im flatParams

------- Computation related functions

mapVals :: M.Map a b -> [b]
mapVals = map snd . M.toList

computeOnObjs :: (Autofloat a) => [Obj' a] -> [ObjComp a] -> [Obj' a]
computeOnObjs objs comps = mapVals $ foldl computeOn (dictOf objs) comps

computeOnObjs_noGrad :: (Autofloat a) => [Obj] -> [ObjComp a] -> [Obj]
computeOnObjs_noGrad objs comps = let objsG = addGrads objs in
                                 let objsComputed = mapVals $ foldl computeOn (dictOf objsG) comps in
                                 zeroGrads objsComputed

-- | Apply a computation to the relevant object in the dictionary.
-- | This computation model assumes that the point of all computations is to set an attribute in an object.
-- | This helper function first catches errors on the function name, object name, and object type.
computeOn :: (Autofloat a) => M.Map Name (Obj' a) -> ObjComp a -> M.Map Name (Obj' a)
computeOn objDict comp =
          let (objName, objProperty, fname, args) = (oName comp, oProp comp, fnName comp, fnParams comp) in
          case fname of
          "None" -> objDict -- Style like "shape = None"
          _ -> case M.lookup objName objDict of
               Nothing -> error $ "compute: no object named " ++ objName
               Just obj -> case M.lookup fname computationDict of
                           Nothing -> error $ "compute: no computation named " ++ fname
                           Just function -> let objRes = applyAndSet objDict comp function obj in
                                            M.insert objName objRes objDict

-- | Look up the arguments to a computation, apply the computation,
-- | and set the property in the object to the result. The ordering of
-- | arguments is preserved by 'partitionEithers' and 'map' onto 'resolveObjs'
applyAndSet :: (Autofloat a) => M.Map Name (Obj' a) -> ObjComp a -> CompFn a -> Obj' a -> Obj' a
applyAndSet objDict comp function obj =
          let (objName, objProperty, fname, args) = (oName comp, oProp comp, fnName comp, fnParams comp) in
          -- Style computations rely on the order of object inputs being the same as program arguments.
          let (constArgs, objectArgs) = partitionEithers $ map (resolveObjs objDict) args in
          let res = function constArgs (concat objectArgs) in
          -- TODO: for multiple objects, might not be in right order. alphabetize?
          set objProperty obj res

-- | Look up either a property of a GPI, or the GPI itself by its unique identifier. Constant arguments such as TFloat are ignored and passed through.
resolveObjs :: (Autofloat a) => M.Map Name (Obj' a) -> TypeIn a
                             -> Either (TypeIn a) [Obj' a]
resolveObjs objs e = case e of
    -- TODO: type synonyms logic here
    TAllShapes v   -> case lookupAll v objs of
                      [] -> error ("id '" ++ v ++ "' /and subobjects do(es) not exist in obj dict")
                      xs -> Right xs
    TShape     v   -> case M.lookup v objs of
                      Nothing -> error ("id '" ++ v ++ "' does not exist in obj dict: " ++ show e)
                      Just x -> Right [x]
    TProp i prop   -> case M.lookup i objs of
                      Nothing  -> error ("id '" ++ i ++ "' does not exist in obj dict")
                      Just obj -> Left $ get prop obj
    TCall _ _      -> error "computations don't support nested computations"
    constantArg    -> Left constantArg

-- e.g. for an object named "domain", returns "domain" as well as secondary shapes "domain shape", "domain xaxis", etc.
   -- but not "domain shape label"
-- Maybe add the ability to pass in "expected" types, or to synthesize types and then check if they match?
lookupAll :: Name -> M.Map Name (Obj' a) -> [Obj' a]
lookupAll name objs = map snd $ M.toList $ M.filterWithKey (\inName _ -> objOrSecondaryShape name inName) objs

nameParts :: String -> [String]
nameParts = splitOn nameSep

-- A name is three parts: [subobjname, possibly styshapename, possibly label]
-- TODO rewrite, it's hacky to do name resolution in lookup vs. in procExpr
objOrSecondaryShape :: Name -> Name -> Bool
objOrSecondaryShape name inName =
    let (names, inNames) = (nameParts name, nameParts inName) in
    case (names, inNames) of
        -- Resolve e.g. "A" to "A xaxis", "A yaxis"
        ([subObjName], [inSubObjName, inStyShapeName]) ->
            subObjName == inSubObjName
        -- excludes labels of shapes
        _ -> name == inName -- Resolve e.g. "A xaxis", "A xaxis label"

------- Style related functions

defName = "default"

-- default shapes at base types (not obj)
defSolidArrow = SolidArrow { startx = 100, starty = 100, endx = 200, endy = 200,
                                thickness = 10, selsa = False, namesa = defName, colorsa = black, stylesa = "straight" }
defPt = Pt { xp = 100, yp = 100, selp = False, namep = defName }
defSquare = Square { xs = 100, ys = 100, side = defaultRad,
                          names = defName, colors = black, ang = 0.0, strokes = 0.0, styles = "solid"}
defArc = Arc { xar = 100, yar = 100, sizear = defaultRad/6, namear = defName, colorar = black
                          , isRightar = "false", selar = False, rotationar = 0.0, anglear = 60.0, radiusar = 12.0, stylear = "line" }
defRect = Rect { xr = 100, yr = 100, sizeX = defaultRad, sizeY = defaultRad + 200,
                          selr = False, namer = defName, colorr = black, angr = 0.0}
defParellelogram = Parallelogram { xpa = 100, ypa = 100, sizeXpa = defaultRad, sizeYpa = defaultRad + 200,
                          selpa = False, namepa = defName, colorpa = black, anglepa = 30.0, rotationpa = 0.0}
defText = Label { xl = -100, yl = -100, wl = 0, hl = 0, textl = defName, sell = False, namel = defName }
defLabel = Label { xl = -100, yl = -100, wl = 0, hl = 0, textl = defName, sell = False,
                        namel = labelName defName }
defCirc = Circ { xc = 100, yc = 100, r = defaultRad, namec = defName, colorc = black, strokec = 0.0, stylec = "solid" }
defEllipse = Ellipse { xe = 100, ye = 100, rx = defaultRad, ry = defaultRad,
                            namee = defName, colore = black }
defCurve = CubicBezier { colorcb = black, pathcb = path, namecb = defName, stylecb = "solid" }
    where path = [(10, 100), (300, 100)]
defLine = Line { startx_l = -100, starty_l = -100, endx_l = 300, endy_l = 300,
                                thickness_l = 2, name_l = defName, color_l = black, style_l = "solid" }
defImg = Img {xim = 100, yim = 100, sizeXim = defaultRad, sizeYim = defaultRad + 200, selim = False, nameim = defName, angim = 0.0, path = ""}
-- default shapes
defaultSolidArrow, defaultPt, defaultSquare, defaultArc, defaultRect, defaultParellelogram,
                   defaultCirc, defaultEllipse :: String -> Obj
defaultSolidArrow name = A $ setName name defSolidArrow
defaultLine name = LN $ setName name defLine
defaultPt name = P $ setName name defPt
defaultSquare name = S $ setName name defSquare
defaultArc name = AR $ setName name defArc
defaultRect name = R $ setName name defRect
defaultParellelogram name = PA $ setName name defParellelogram

defaultCirc name = C $ setName name defCirc
defaultEllipse name = E $ setName name defEllipse
defaultCurve name = CB $ setName name defCurve
defaultImg name = IM $ setName name defImg

-- Set the text field to just the Substance object name (e.g. "A")
-- and name to the internal name, e.g. "A shape"
defaultText :: String -> String -> Obj
defaultText text name = L $ setName name defText { textl = text }

-- If an object's name is X and it is labeled "Set X", the label name is "Label_X" and the text is "Set X"
-- "Auto" is reserved text that labels the object with the Substance name; in this case it would be "X"
defaultLabel :: String -> String -> Obj
defaultLabel objName labelText =
             L $ setName (labelName objName) defLabel { textl = checkAuto objName labelText }

checkAuto :: String -> String -> String
checkAuto objName labelText =  -- TODO: should this use labelSetting?
          if labelText == autoWord then
             let subObjName = (nameParts objName) !! 0
             in subObjName
          else labelText

shapeAndFn :: (Autofloat a) => S.StyDict a -> String ->
                               ([Obj], [ObjFnInfo a], [ConstrFnInfo a], [ObjComp a])
shapeAndFn dict subObjName = case M.lookup subObjName dict of
    Nothing   -> error ("Cannot find style info for " ++ subObjName)
    Just spec ->
        let names = map (uniqueShapeName subObjName) $ M.keys (S.spShpMap spec)
            styobjs = M.elems (S.spShpMap spec)
            labelText = S.spLabel spec
        in concat4 $ map (\(n,o) -> getShape n labelText o) $ zip names styobjs

getShape :: (Autofloat a) => String -> Maybe String -> S.StyObj a ->
                             ([Obj], [ObjFnInfo a], [ConstrFnInfo a], [ObjComp a])

getShape oName labelText (objType, properties) =
         -- We don't need the object type to typecheck the computation, because we have the object's name and
         -- it's stored as an Obj (can pattern-match)
         let (computations, properties_nocomps) = compsAndVars oName properties in
         let objInfo = case objType of

              S.Text    -> initText oName properties_nocomps
              S.Arrow   -> initArrow oName properties_nocomps
              S.Circle  -> initCircle oName properties_nocomps
              S.Ellip   -> initEllipse oName properties_nocomps
              S.Box     -> initSquare oName properties_nocomps
              S.Arc2 -> initArc oName properties_nocomps
              S.Rectangle -> initRect oName properties_nocomps
              S.Dot     -> initDot oName properties_nocomps
              S.Curve   -> initCurve oName properties_nocomps
              S.Line2    -> initLine oName properties_nocomps
              S.Image      -> initImg oName properties_nocomps
              S.Parallel -> initParallelogram oName properties_nocomps
              S.NoShape -> ([], [], []) in
         let res = tupAppend objInfo computations in

         -- TODO factor out label logic?
         let labelRes = M.lookup labelTextWord properties in -- assuming one label per shape
         let labelSet = labelSetting labelRes objType labelText in
         case labelSet of
         -- By default, if unspecified, an object is labeled with "Auto" setting, unless it has no shape
         Nothing    -> res
         Just label -> addObject [defaultLabel oName label] res

         where tupAppend (a, b, c) d = (a, b, c, d)
               addObject l (a, b, c, d) = (a ++ l, b, c, d)

-- TODO: what if a property (e.g. "r") can take either a computation or an input expr??
-- that should be done via getters and setters (for both base and derived properties)

-- Given a config, separates the computations and the vars and returns both
compsAndVars :: (Autofloat a) =>  Name -> S.Properties a -> ([ObjComp a], S.Properties a)
compsAndVars n props =
    let (props_comps, props_nocomps) = M.partition isComp props
        comps = mapVals $ M.mapWithKey packComp props_comps
    in
    (comps, props_nocomps)
    where
        packComp :: Property -> TypeIn a -> ObjComp a
        packComp prop (TCall f args) = ObjComp { oName = n, oProp = prop, fnName = f, fnParams = args }
        packComp prop (TProp i p) = ObjComp { oName = n, oProp = prop, fnName = "_get", fnParams = [TStr p, TShape i]} -- see `_get` in Computation
        packComp p e = error $ "packComp: there are only two types of computation - (1) computation function; (2) property access -- " ++ show p

isComp :: (Autofloat a) => TypeIn a -> Bool
isComp expr = case expr of
    TCall f args -> True
    TProp i p    -> True
    _            -> False

-- | Reserved words or special demarcators in the system
noneWord, autoWord, labelTextWord :: String
noneWord = "None"
autoWord = "Auto"
labelTextWord = "text"

labelSetting :: (Autofloat a) =>
    Maybe (TypeIn a) -> S.StyType -> Maybe String -> Maybe String
labelSetting s_expr objType labelText = case objType of
    S.NoShape -> Nothing
    S.Text    -> Nothing -- for shape = Text { }
    _ -> case s_expr of
            Nothing -> labelText -- TODO: should be Nothing?
            Just (TStr "None") -> Nothing
            Just (TStr "Auto") -> labelText
            Just (TStr text)   -> Just text     -- overrides Substance labels
            Just res -> error ("invalid label setting:\n" ++ show res)

-- | Given a name and context (?), the initObject functions return a 3-tuple of objects, objectives (with info), and constraints (with info) (NOT labels or computations; those are found in `getShape`)`
initCurve, initDot, initText, initArrow, initCircle, initSquare, initEllipse, initLine ::
    (Autofloat a) => Name -> S.Properties a -> ([Obj], [ObjFnInfo a], [ConstrFnInfo a])

initText n config = ([defaultText text n], [], [])
         where text = checkAuto n (fromMaybe n $ lookupStr "text" config)
               -- For text objects, use the normal label "text" attribute to set the text
               -- of the text object (handling Auto in the same way, not allowing None)

initSquare n config = ([sq], [], sizeFuncs n)
    where
        style = fromMaybe "solid" $ lookupStr "style" config
        stroke = fromMaybe 0.0 $ lookupFloat "stroke" config
        setStyle (S s) sty str = S $ s { styles = sty, strokes = str }
        sq = setStyle (defaultSquare n) style stroke



initArc n config = (objs, [],sizeFuncs n)
    where right = fromMaybe "true" $ lookupStr "isRight" config
          radius =  fromMaybe 10.0 $ lookupFloat "radius" config
          angle =  fromMaybe 30.0 $ lookupFloat "angle" config
          rotation =  fromMaybe 0.0 $ lookupFloat "rotation" config
          style = fromMaybe "line" $ lookupStr "style" config
          setStyle (AR ar) s ra a ro st = AR $ ar { isRightar =  s, radiusar = ra, rotationar = ro, anglear = a, stylear = st}
          objs = [setStyle (defaultArc n) right radius angle rotation style]

initRect n config = ([defaultRect n], [], sizeFuncs n)

initParallelogram n config = (objs, [],sizeFuncs n)
     where
           angle =  fromMaybe 30.0 $ lookupFloat "angle" config
           rotation =  fromMaybe 0.0 $ lookupFloat "rotation" config
           setStyle (PA pa) a ro = PA $ pa {rotationpa = ro, anglepa = a}
           objs = [setStyle (defaultParellelogram n) angle rotation]


initDot n config = ([defaultPt n], [], [])

initEllipse n config = ([defaultEllipse n], [],
                         (penalty `compose2` ellipseRatio, defaultWeight, [TShape n]) : sizeFuncs n)

initArrow n config = (objs, oFns, [])
    where from = lookupId "start" config
          to   = lookupId "end" config
          style = fromMaybe "straight" $ lookupStr "style" config
          setStyle (A a) s = A $ a { stylesa = s } -- TODO: refactor defaultX vs defX
          objs = [setStyle (defaultSolidArrow n) style]
          betweenObjFn = case (from, to) of
                         (Nothing, Nothing) -> []
                         (Just fromName, Just toName) ->
                            [(centerMap, defaultWeight, [TShape n, TShape fromName, TShape toName])]
          oFns = betweenObjFn

-- very similar to arrow and curve
initLine n config = (objs, oFns, [])
    where from = lookupId "start" config
          to   = lookupId "end" config
          style = fromMaybe "solid" $ lookupStr "style" config
          setStyle (LN l) s = LN $ l { style_l = s }
          objs = [setStyle (defaultLine n) style]
          betweenObjFn = case (from, to) of
                         (Nothing, Nothing) -> []
                         (Just fromNm, Just toNm) -> [(centerLine, defaultWeight, [TShape n, TShape fromNm, TShape toNm])]
          oFns = betweenObjFn

initCircle n config = (objs, oFns, constrs)
    where
        style = fromMaybe "solid" $ lookupStr "style" config
        stroke = fromMaybe 0.0 $ lookupFloat "stroke" config
        setStyle (C c) sty str = C $ c { stylec = sty, strokec = str }
        circObj = setStyle (defaultCirc n) style stroke
        objs = [circObj]
        oFns = []
        constrs = sizeFuncs n


initCurve n config = (objs, [], [])
        where defaultPath = [(10, 100), (50, 0)] -- (60, 0), (100, 100), (250, 250), (300, 100)]
              style = fromMaybe "solid" $ lookupStr "style" config
              curve = CB CubicBezier { colorcb = black, pathcb = defaultPath, namecb = n, stylecb = style }
              objs = [curve]

initImg n config = (objs, [], sizeFuncs n)
        where pth = fromMaybe "http://www.penrose.ink/resources/hollow-pentagon.svg" $ lookupStr "path" config
              setPath (IM im) pth = IM $ im { path = pth } -- TODO: refactor defaultX vs defX
              objs = [setPath (defaultImg n) pth]



sizeFuncs :: (Autofloat a) => Name -> [ConstrFnInfo a]
sizeFuncs n = [(penalty `compose2` maxSize, defaultWeight, [TShape n]),
               (penalty `compose2` minSize, defaultWeight, [TShape n])]

tupCons :: a -> (b, c) -> (a, b, c)
tupCons a (b, c) = (a, b, c)

-- TODO: deal with pattern-matching on computation anywhere
lookupId :: (Autofloat a) =>
    String -> S.Properties a ->  Maybe String
lookupId key dict = case M.lookup key dict of
    Just (TShape i)     -> Just i -- objects are looked up later
    Just (TAllShapes i) -> Just i -- objects are looked up later
    Just res -> error ("expecting id, got:\n" ++ show res)
    Nothing -> Nothing

lookupStr :: (Autofloat a) =>
    String -> S.Properties a ->  Maybe String
lookupStr key dict = case M.lookup key dict of
    Just (TStr i) -> Just i
    Just res -> error ("expecting str, got:\n" ++ show res)
    Nothing -> Nothing

lookupFloat :: (Autofloat a) =>
    String -> S.Properties a ->  Maybe Float
lookupFloat key dict = case M.lookup key dict of
     Just (TFloat i) -> Just i -- objects are looked up later
     Just (TNum i) -> Just $ r2f i -- HACK: separate function
     Just res -> error ("expecting float, got:\n" ++ show res)
     Nothing -> Nothing

    -- Just (S.CompArgs fn params) -> error "not expecting a computed property"
    -- FIXME: get dot access to work for arbitrary input

------- Generate objective functions

-- defaultWeight :: Floating a => a
-- defaultWeight = 1

defaultRad :: Floating a => a
defaultRad = 100

objFnOnNone :: ObjFn
objFnOnNone _ _ = 0

-- Parameters to change
declSetObjfn :: ObjFn
declSetObjfn = objFnOnNone -- centerCirc

declPtObjfn :: ObjFn
declPtObjfn = objFnOnNone -- centerCirc

declLabelObjfn :: ObjFn
declLabelObjfn = centerLabel -- objFnOnNone

declMapObjfn :: ObjFn
declMapObjfn = centerMap

map4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4 f (w, x, y, z) = (f w, f x, f y, f z)

genAllObjs :: (Autofloat a) => ([C.SubDecl], [C.SubConstr]) -> S.StyDict a ->
    ([Obj], [ObjFnInfo a], [ConstrFnInfo a], [ObjComp a])
-- TODO figure out how the types work. also add weights
genAllObjs (decls, constrs) stys = (concat objss, concat objFnss, concat constrFnss, concat compss)
    where
        (objss, objFnss, constrFnss, compss) = unzip4 $ map (shapeAndFn stys) $ C.getAllIds (decls, constrs)

dictOf :: (Autofloat a) => [Obj' a] -> M.Map Name (Obj' a)
dictOf = foldr addObj M.empty
       where addObj o dict = M.insert (getName o) o dict

dictOfObjs :: Named a => [a] -> M.Map Name a
dictOfObjs = foldr addObj M.empty
       where addObj o dict = M.insert (getName o) o dict

-- constant b/c ambient fn value seems to be 10^4 and constr value seems to reach only 10, 10^2
constrWeight :: Floating a => a
constrWeight = 10 ^ 4

-- Preserve failed keys for reporting errors
lookupWithFail :: M.Map Name (Obj' a) -> Name -> Either Name [Obj' a]
lookupWithFail dict key = case lookupAll key dict of -- accounts for X resolving to "X shape1", "X shape2", etc
                             []   -> Left key
                             vals -> Right vals

lookupNames :: (Autofloat a) => M.Map Name (Obj' a) -> [Name] -> [Obj' a]
lookupNames dict ns = concatMap check res
    where
        res = map (lookupWithFail dict) ns
        check x = case x of
            Right obj -> obj
            Left key -> error ("lookupNames: key `" ++ key ++ "` does not exist in dict:\n\n" ++ show dict)

-- takes list of current objects as a parameter, and is later partially applied with that in optimization
-- first param: list of parameter annotations for each object in the state
-- assumes that the INPUT state's SIZE and ORDER never change (their size and order can change inside the fn)
-- note: CANNOT do dict -> list because that destroys the order
genObjFn :: (Autofloat a) =>
         [[Annotation]]
         -> [ObjComp a]
         -> [ObjFnInfo a]
         -> [(M.Map Name (Obj' a) -> a, Weight a)]
         -> [ConstrFnInfo a]
         -> [Obj] -> a -> [a] -> [a] -> a
genObjFn annotations computations objFns ambientObjFns constrObjFns =
         \currObjs penaltyWeight fixed varying ->
         let newObjs = pack annotations currObjs fixed varying in
         -- Construct implicit computation graph (second stage), including computations as intermediate vars
         let objsComputed = computeOnObjs newObjs computations in
         let objDict = dictOf objsComputed in -- TODO revert
         sumMap (applyFn objDict) objFns
            + (tr "ambient fn value: " (sumMap (\(f, w) -> w * f objDict) ambientObjFns))
            + (tr "constr fn value: "
                (constrWeight * penaltyWeight
                              * sumMap (applyFn objDict) constrObjFns))
    where
        applyFn d (f, w, e) =
            let (args, objs) = partitionEithers $ map (resolveObjs d) e
            in w * f (concat objs) args

-- TODO: In principle, the exterior point method requires the initial state to start in the exterior
-- of the feasible region; that is, at least one of the constraints should be violated.
-- In practice, it doesn't seem to matter for our small examples.
constraint :: [C.SubConstr] -> [Obj] -> Bool
constraint constrs = if constraintFlag then \x ->
                        let res = [consistentSizes constrs x] in and res
                     else const True

-- generate all objects and the overall objective function
-- TODO adjust weights of all functions
genInitState :: C.SubObjects -> S.StyProg -> State
genInitState objs stys =
             -- objects and objectives (without ambient objfns or constrs)
             let (decls, constrs) = C.subSeparate $ C.subObjs objs in
             let (dict, userObjFns, userConstrFns) = S.getDictAndFns objs stys in
             let (initObjs, initObjFns, initConstrFns, computations) = genAllObjs (decls, constrs) dict in
             let objFns = userObjFns ++ initObjFns in
            --  let objFns = [] in -- TODO removed only for debugging constraints
             -- ambient objectives
             -- be careful with how the ambient objectives interact with the per-declaration objectives!
             -- e.g. the repel objective conflicts with a subset/intersect constraint -> nonconvergence!
            --  let ambientObjFns = [(circlesCenter, defaultWeight)] in
             let ambientObjFns = [] in

             -- constraints
            --  let constrFns = genConstrFns constrs in
            --  let constrFns = [] in
             let ambientConstrFns = [] in -- TODO add
             let constrObjFns = initConstrFns ++ userConstrFns ++ ambientConstrFns in

             -- resample state w/ constrs. TODO how to deal with `Subset A B` -> `r A < r B`?
             -- let boolConstr = \x -> True in // TODO needs to take this as a param
             let (initStateConstr, initRng') = sampleConstrainedState initRng initObjs constrs in

             -- Apply each computation to the object in the (dictionary of) state, updating the state each time.
             -- Gradients are added here because applying computations and generating annotations
                -- are polymorphic, but gradients are removed before they go into the state.
             let initStateComputed = computeOnObjs (addGrads initStateConstr) computations in
             let objsInit = zeroGrads initStateComputed in

             -- Note: after creating these annotations, we can no longer change the size or order of the state.
             -- unpackAnnotate :: [Obj] -> [ [(Float, Annotation)] ]
             let flatObjsAnnotated = unpackAnnotate initStateComputed in
             let annotationsCalc = map (map snd) flatObjsAnnotated in -- `map snd` throws away initial floats

             -- overall objective function
             let objFnOverall = genObjFn annotationsCalc computations objFns ambientObjFns constrObjFns in
             -- trace ("initial constrained, computed state: \n" ++ show objsInit ++ "\n and Constraints: \n " ++ show constrs) $
             State { objs = objsInit,
                     constrs = constrs,
                     comps = computations,
                     params = initParams { objFn = objFnOverall, annotations = annotationsCalc },
                     rng = initRng', autostep = False }

--------------- end object / objfn generation

rad :: Floating a => a
rad = 200 -- TODO don't hardcode into constant
clamp1D y = if clampflag then 0 else y

rad1 :: Floating a => a
rad1 = rad-100

rad2 :: Floating a => a
rad2 = rad+50

-- Initial state of the world, before including Substance/Style input
initState :: State
initState = State { objs = objsInit,
                    constrs = [],
                    comps = [],
                    rng = initRng,
                    autostep = False,
                    params = initParams }

-- divide two integers to obtain a float
divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

pw2 :: Float
pw2 = picWidth `divf` 2

pw2' :: Floating a => a
pw2' = realToFrac pw2

ph2 :: Float
ph2 = picHeight `divf` 2

ph2' :: Floating a => a
ph2' = realToFrac ph2

-- avoid having black and white to ensure the visibility of objects
opacity, cmax, cmin :: Float
opacity = 0.5
cmax = 0.1
cmin = 0.9

-- radiusRange, sideRange :: Floating a => (a, a    )
sizeYange  = (-pw2, pw2)
heightRange = (-ph2, ph2)
radiusRange = (20, picWidth `divf` 6)
sideRange = (20, picWidth `divf` 3)
colorRange  = (cmin, cmax)

------- Sampling the state subject to a constraint. Currently not used since we are doing unconstrained optimization.

-- generate an infinite list of sampled elements
-- keep the last generator for the "good" element
genMany :: RandomGen g => g -> (g -> (a, g)) -> [(a, g)]
genMany gen genOne = iterate (\(c, g) -> genOne g) (genOne gen)

-- take the first element that satisfies the condition
-- not the most efficient impl. also assumes infinite list s.t. head always exists
crop :: RandomGen g => (a -> Bool) -> [(a, g)] -> (a, g)
crop cond xs = --(takeWhile (not . cond) (map fst xs), -- drops gens
                head $ dropWhile (\(x, _) -> not $ cond x) xs -- drops while top-level condition true. keeps good's gen

-- randomly sample location (for circles and labels) and radius (for circles)
sampleCoord :: RandomGen g => g -> Obj -> (Obj, g)
sampleCoord gen o = case o of
                    C circ -> let (r',  gen3) = randomR radiusRange gen2
                                  (cr', gen4) = randomR colorRange  gen3
                                  (cg', gen5) = randomR colorRange  gen4
                                  (cb', gen6) = randomR colorRange  gen5
                                  in
                              (C $ circ { r = r', colorc = makeColor cr' cg' cb' opacity }, gen6)
                    E elli -> let (rx', gen3) = randomR radiusRange gen2
                                  (cr', gen4) = randomR colorRange  gen3
                                  (cg', gen5) = randomR colorRange  gen4
                                  (cb', gen6) = randomR colorRange  gen5
                                  (ry', gen7) = randomR radiusRange gen6
                                  in
                              (E $ elli { rx = rx', ry = ry', colore = makeColor cr' cg' cb' opacity }, gen7)
                    S sq   -> let (side', gen3) = randomR sideRange gen2
                                  (cr', gen4) = randomR colorRange  gen3
                                  (cg', gen5) = randomR colorRange  gen4
                                  (cb', gen6) = randomR colorRange  gen5
                                  in
                              (S $ sq { side = side', colors = makeColor cr' cg' cb' opacity }, gen6)
                    R rt   -> let (len', gen3) = randomR sideRange gen2
                                  (wid', gen4) = randomR sideRange gen3
                                  (cr', gen5) = randomR colorRange  gen4
                                  (cg', gen6) = randomR colorRange  gen5
                                  (cb', gen7) = randomR colorRange  gen6
                                  in
                              (R $ rt { sizeX = len', sizeY = wid',
                                        colorr = makeColor cr' cg' cb' opacity }, gen7)
                    PA pa   -> let
                                  (cr', gen3) = randomR colorRange  gen2
                                  (cg', gen4) = randomR colorRange  gen3
                                  (cb', gen5) = randomR colorRange  gen4
                                  in
                              (PA $ pa { colorpa = makeColor cr' cg' cb' opacity }, gen5)
                    L lab -> (o_loc, gen2) -- only sample location
                    P pt  -> (o_loc, gen2)
                    A a   -> (o_loc, gen2) -- TODO
                    AR ar -> (o_loc, gen2)
                    LN a   -> (o_loc, gen2) -- TODO
                    CB c  -> (o, gen2) -- TODO: fall through
                    IM im -> let (len', gen3) = randomR sideRange gen2
                                 (wid', gen4) = randomR sideRange gen3
                                  in
                              (IM $ im { sizeXim = len', sizeYim = wid'}, gen4)

        where (x', gen1) = randomR sizeYange  gen
              (y', gen2) = randomR heightRange gen1
              o_loc      = setX x' $ setY (clamp1D y') o

-- sample each object independently, threading thru gen
stateMap :: RandomGen g => g -> (g -> a -> (b, g)) -> [a] -> ([b], g)
stateMap gen f [] = ([], gen)
stateMap gen f (x:xs) = let (x', gen') = f gen x in
                        let (xs', gen'') = stateMap gen' f xs in
                        (x' : xs', gen'')

-- sample a state
genState :: RandomGen g => [Obj] -> g -> ([Obj], g)
genState shapes gen = stateMap gen sampleCoord shapes

-- sample entire state at once until constraint is satisfied
-- TODO doesn't take into account pairwise constraints or results from objects sampled first, sequentially
sampleConstrainedState :: RandomGen g => g -> [Obj] -> [C.SubConstr] -> ([Obj], g)
sampleConstrainedState gen shapes constrs = (state', gen')
       where (state', gen') = crop (constraint constrs) states
             states = genMany gen (genState shapes)
             -- init state params are ignored; we just need to know what kinds of objects are in it

--------------- Handle user input. "handler" is the main function here.
-- Whenever the library receives an input event, it calls "handler" with that event
-- and the current state of the world to handle it.

bbox = 60 -- TODO put all flags and consts together
-- hacky bounding box of label

-- Hardcode bbox of label at the center
-- TODO properly get bbox; rn text is centered at bottom left
inObj :: (Float, Float) -> Obj -> Bool

-- TODO: this is NOT an accurate BBox at all. Good for selection though
inObj (xm, ym) (L o) =
    abs (xm - (xl o)) <= 0.1 * (wl o) &&
    abs (ym - (yl o)) <= 0.1 * (hl o) -- is label
    -- abs (xm - (label_offset_x (textl o) (xl o))) <= 0.25 * (wl o) &&
    -- abs (ym - (label_offset_y (textl o) (yl o))) <= 0.25 * (hl o) -- is label
inObj (xm, ym) (C o) = dist (xm, ym) (xc o, yc o) <= r o -- is circle
inObj (xm, ym) (S o) = abs (xm - xs o) <= 0.5 * side o && abs (ym - ys o) <= 0.5 * side o -- is square
inObj (xm, ym) (AR o) = abs (xm - xar o) <= 0.5 * sizear o && abs (ym - yar o) <= 0.5 * sizear o -- is Arc
inObj (xm, ym) (R o) = let (bl_x, bl_y) = (xr o - 0.5 * sizeX o, yr o - 0.5 * sizeY o) in -- bottom left
                       let (tr_x, tr_y) = (xr o + 0.5 * sizeX o, yr o + 0.5 * sizeY o) in -- top right
                       bl_x < xm && xm < tr_x && bl_y < ym && ym < tr_y
inObj (xm, ym) (P o) = dist (xm, ym) (xp o, yp o) <= ptRadius -- is Point, where we arbitrarily define the "radius" of a point
-- TODO: due to the way Located is defined, we can only drag the starting pt here
inObj (xm, ym) (A a) =
    let (sx, sy, ex, ey, t) = (startx a, starty a, endx a, endy a, thickness a)
        (x, y) = midpoint (sx, sy) (ex, ey)
        len = 0.5 * dist (sx, sy) (ex, ey)
    in abs (x - xm) <= len && abs (y - ym) <= t

-- check convergence of EP method
epDone :: State -> Bool
epDone s = ((optStatus $ params s) == EPConverged) || ((optStatus $ params s) == NewIter)

----------- Stepping the state the world via gradient descent.
 -- First, miscellaneous helper functions.

-- Clamp objects' positions so they don't go offscreen.
-- TODO clamp needs to take into account bbox of object
clampX :: Float -> Float
clampX x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

clampY :: Float -> Float
clampY y = if y < -ph2 then -ph2 else if y > ph2 then ph2 else y

noOverlapPair :: Obj -> Obj -> Bool
noOverlapPair (C c1) (C c) = dist (xc c1, yc c1) (xc c, yc c) > r c1 + r c
noOverlapPair (S s1) (S s2) = dist (xs s1, ys s1) (xs s2, ys s2) > side s1 + side s2
-- TODO: factor out this
noOverlapPair (C c) (S s) = dist (xc c, yc c) (xs s, ys s) > (halfDiagonal .  side) s + r c
noOverlapPair (S s) (C c) = dist (xs s, ys s) (xc c, yc c) > (halfDiagonal . side) s + r c
noOverlapPair _ _ = error "no overlap case not handled"

-- return true iff satisfied
-- TODO deal with labels and more than two objects
noneOverlap :: [Obj] -> Bool
noneOverlap objs = let allPairs = filter (\x -> length x == 2) $ subsequences objs in -- TODO factor out
                 all id $ map (\[o1, o2] -> noOverlapPair o1 o2) allPairs
-- noOverlap (c1 : c : []) = noOverlapPair c1 c
-- noOverlap (c1 : c : c3 : _) = noOverlapPair c1 c && noOverlapPair c c3 && noOverlapPair c1 c3 -- TODO
-- noOverlap _ _ = True

-- allOverlap vs. not noOverlap--they're different!
allOverlap :: [Obj] -> Bool
allOverlap objs = let allPairs = filter (\x -> length x == 2) $ subsequences objs in -- TODO factor out
                 all id $ map (\[o1, o2] -> not $ noOverlapPair o1 o2) allPairs

-- -- used when sampling the inital state, make sure sizes satisfy subset constraints
-- subsetSizeDiff :: Floating a => a
-- subsetSizeDiff = 10.0

-- halfDiagonal :: (Floating a) => a -> a
-- halfDiagonal side = 0.5 * dist (0, 0) (side, side)

-- TODO: do we want strict subset or loose subset here? Now it is strict
consistentSizes :: [C.SubConstr] -> [Obj] -> Bool
consistentSizes constrs objs = True
--   all id $ map (checkSubsetSize dict) constrs
--                             where dict = dictOfObjs objs
-- checkSubsetSize dict constr@((C.PredicateConst "Subset") inName outName) =
--     case (M.lookup inName dict, M.lookup outName dict) of
--         (Just (C inc), Just (C outc)) ->
--             -- (r outc) (r inc) > subsetSizeDiff -- TODO: taking this as a parameter?
--             0.7 * (r outc) > (r inc)
--         (Just (S inc), Just (S outc)) -> (side outc) - (side inc) > subsetSizeDiff
--         -- TODO: this does not scale, general way?
--         (Just (C c), Just (S s)) -> r c < 0.5 * side s
--         (Just (S s), Just (C c)) -> (halfDiagonal . side) s < r c
--         (_, _) -> True -- error "objects don't exist in check subset size"
checkSubsetSize _ _ = True -- error "object subset sizes not handled"


-- Type aliases for shorter type signatures.
type TimeInit = Float
type Time = Double
type ObjFn1 a = forall a . (Autofloat a) => [a] -> a
type GradFn a = forall a . (Autofloat a) => [a] -> [a]
type Constraints = [(Int, (Double, Double))]
     -- TODO: convert lists to lists of type-level length, and define an interface for object state (pos, size)
     -- also need to check the input length matches obj fn lengths, e.g. in awlinesearch

-- old code for bound constraints
-- does not project onto an arbitrary set, only intervals
-- projCoordInterval :: (Double, Double) -> Double -> Double
-- projCoordInterval (lower, upper) x = (sort [lower, upper, x]) !! 1 -- median of the list

-- for each element, if there's a constraint on it (by index), project it onto the interval
-- lookInAndProj :: Constraints -> (Int, Double) -> [Double] -> [Double]
-- lookInAndProj constraints (index, x) acc =
--               case (Map.lookup index constraintsMap) of
--               Just bounds -> projCoordInterval bounds x : acc
--               Nothing     -> x : acc
--               where constraintsMap = Map.fromList constraints

-- don't change the order of elements in the state!! use foldr, not foldl
-- projectOnto :: Constraints -> [Double] -> [Double]
-- projectOnto constraints state =
--             let indexedState = zip [0..] state in
--             foldr (lookInAndProj constraints) [] indexedState

-------- Step the world by one timestep (provided by the library).
-- this function actually ignores the input timestep, because line search calculates the appropriate timestep to use,
-- but it's left in, in case we want to debug the line search.
-- gloss operates on floats, but the optimization code should be done with doubles, so we
-- convert float to double for the input and convert double to float for the output.
step :: TimeInit -> State -> State
step t s = -- if down s then s -- don't step when dragging
           if autostep s then s { objs = objs', params = params' } else s
           where (objs', params') = stepObjs (float2Double t) (params s) (objs s)

stateSize :: Int
stateSize = 3

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

objsInfo :: [a] -> [[a]]
objsInfo = chunksOf stateSize

-- from [x,y,s] over all objs, return [x,y] over all
objsCoords :: [a] -> [a]
objsCoords = concatMap (\[x, y, s] -> [x, y]) . objsInfo -- from [x,y,s] over all objs, return [s] over all
objsSizes :: [a] -> [a]
objsSizes = map (\[x, y, s] -> s) . objsInfo
----

-- convergence criterion for EP
-- if you want to use it for UO, needs a different epsilon
epStopCond :: (Autofloat a) => [a] -> [a] -> a -> a -> Bool
epStopCond x x' fx fx' =
           trStr ("EP: \n||x' - x||: " ++ (show $ norm (x -. x'))
           ++ "\n|f(x') - f(x)|: " ++ (show $ abs (fx - fx'))) $
           (norm (x -. x') <= epStop) || (abs (fx - fx') <= epStop)

-- just for unconstrained opt, not EP
-- stopEps large bc UO doesn't seem to strongly converge...
optStopCond :: (Autofloat a) => [a] -> Bool
optStopCond gradEval = trStr ("||gradEval||: " ++ (show $ norm gradEval)
                       ++ "\nstopEps: " ++ (show stopEps)) $
            (norm gradEval <= stopEps)

-- unpacks all objects into a big state vector, steps that state, and repacks the new state into the objects
-- NOTE: all downstream functions (objective functions, line search, etc.) expect a state in the form of
-- a big list of floats with the object parameters grouped together: [x1, y1, size1, ... xn, yn, sizen]


-- Going from `Floating a` to Float discards the autodiff dual gradient info (I think)
zeroGrad :: (Autofloat a) => Obj' a -> Obj
zeroGrad (C' c) = C $ Circ { xc = r2f $ xc' c, yc = r2f $ yc' c, r = r2f $ r' c, namec = namec' c, colorc = colorc' c, stylec = stylec' c, strokec = r2f $ strokec' c }
zeroGrad (E' e) = E $ Ellipse { xe = r2f $ xe' e, ye = r2f $ ye' e, rx = r2f $ rx' e, ry = r2f $ ry' e,
                              namee = namee' e, colore = colore' e }
zeroGrad (S' s) = S $ Square { xs = r2f $ xs' s, ys = r2f $ ys' s, side = r2f $ side' s, names = names' s, colors = colors' s, ang = ang' s, styles = styles' s, strokes = r2f $ strokes' s }
zeroGrad (AR' ar) = AR $ Arc { xar = r2f $ xar' ar, yar = r2f $ yar' ar, sizear = r2f $ sizear' ar,
                                      isRightar = isRightar' ar, radiusar = r2f $ radiusar' ar,
                                      selar = selar' ar, rotationar = r2f $ rotationar' ar, anglear = r2f $ anglear' ar
                                      ,namear = namear' ar, colorar = colorar' ar, stylear = stylear' ar}
zeroGrad (R' r) = R $ Rect { xr = r2f $ xr' r, yr = r2f $ yr' r, sizeX = r2f $ sizeX' r, sizeY = r2f $ sizeY' r,
                           selr = selr' r, namer = namer' r, colorr = colorr' r, angr = angr' r }
zeroGrad (PA' pa) = PA $ Parallelogram { xpa = r2f $ xpa' pa, ypa = r2f $ ypa' pa, sizeXpa = r2f $ sizeXpa' pa, sizeYpa = r2f $ sizeYpa' pa,
                           selpa = selpa' pa, namepa = namepa' pa, colorpa = colorpa' pa, anglepa = anglepa' pa, rotationpa = rotationpa' pa}
zeroGrad (L' l) = L $ Label { xl = r2f $ xl' l, yl = r2f $ yl' l, wl = r2f $ wl' l, hl = r2f $ hl' l,
                              textl = textl' l, sell = sell' l, namel = namel' l }
zeroGrad (P' p) = P $ Pt { xp = r2f $ xp' p, yp = r2f $ yp' p, selp = selp' p,
                           namep = namep' p }
zeroGrad (A' a) = A $ SolidArrow { startx = r2f $ startx' a, starty = r2f $ starty' a,
                            endx = r2f $ endx' a, endy = r2f $ endy' a, thickness = r2f $ thickness' a,
                            selsa = selsa' a, namesa = namesa' a, colorsa = colorsa' a, stylesa = stylesa' a }
zeroGrad (LN' a) = LN $ Line { startx_l = r2f $ startx_l' a, starty_l = r2f $ starty_l' a,
                            endx_l = r2f $ endx_l' a, endy_l = r2f $ endy_l' a,
                            thickness_l = r2f $ thickness_l' a, name_l = name_l' a, color_l = color_l' a,
                            style_l = style_l' a }
zeroGrad (CB' c) = CB $ CubicBezier { pathcb = path, colorcb = colorcb' c, namecb = namecb' c, stylecb = stylecb' c }
    where path_flat = concatMap (\(x, y) -> [r2f x, r2f y]) $ pathcb' c
          path      = map tuplify2 $ chunksOf 2 path_flat
zeroGrad (IM' im) = IM $ Img { xim = r2f $ xim' im, yim = r2f $ yim' im, sizeXim = r2f $ sizeXim' im, sizeYim = r2f $ sizeYim' im,
                           selim = selim' im, nameim = nameim' im, angim = angim' im , path = path' im}

zeroGrads :: (Autofloat a) => [Obj' a] -> [Obj]
zeroGrads = map zeroGrad

-- Add the grad info by generalizing Obj (on Floats) to polymorphic objects (for autodiff to use)
addGrad :: (Autofloat a) => Obj -> Obj' a
addGrad (C c) = C' $ Circ' { xc' = r2f $ xc c, yc' = r2f $ yc c, r' = r2f $ r c, namec' = namec c, colorc' = colorc c, stylec' = stylec c, strokec' = r2f $ strokec c }
addGrad (E e) = E' $ Ellipse' { xe' = r2f $ xe e, ye' = r2f $ ye e, rx' = r2f $ rx e, ry' = r2f $ ry e,
                             namee' = namee e, colore' = colore e }

addGrad (S s) = S' $ Square' { xs' = r2f $ xs s, ys' = r2f $ ys s, side' = r2f $ side s, names' = names s, colors' = colors s, ang' = ang s, styles' = styles s, strokes' = r2f $ strokes s }

addGrad (AR ar) = AR' $ Arc' { xar' = r2f $ xar ar, yar' = r2f $ yar ar, sizear' = r2f $ sizear ar,
                                      isRightar' = isRightar ar, radiusar' = r2f $ radiusar ar,
                                      selar' = selar ar, rotationar' = r2f $ rotationar ar, anglear' = r2f $ anglear ar
                                      ,namear' = namear ar, colorar' = colorar ar , stylear' = stylear ar}

addGrad (R r) = R' $ Rect' { xr' = r2f $ xr r, yr' = r2f $ yr r, sizeX' = r2f $ sizeX r,
                           sizeY' = r2f $ sizeY r, selr' = selr r, namer' = namer r,
                           colorr' = colorr r, angr' = angr r }
addGrad (PA pa) = PA' $ Parallelogram' { xpa' = r2f $ xpa pa, ypa' = r2f $ ypa pa, sizeXpa' = r2f $ sizeXpa pa,
                           sizeYpa' = r2f $ sizeYpa pa, selpa' = selpa pa, namepa' = namepa pa,
                           colorpa' = colorpa pa, anglepa' = anglepa pa, rotationpa' = rotationpa pa }
addGrad (L l) = L' $ Label' { xl' = r2f $ xl l, yl' = r2f $ yl l, wl' = r2f $ wl l, hl' = r2f $ hl l,
                              textl' = textl l, sell' = sell l, namel' = namel l }
addGrad (P p) = P' $ Pt' { xp' = r2f $ xp p, yp' = r2f $ yp p, selp' = selp p,
                           namep' = namep p }
addGrad (A a) = A' $ SolidArrow' { startx' = r2f $ startx a, starty' = r2f $ starty a,
                            endx' = r2f $ endx a, endy' = r2f $ endy a, thickness' = r2f $ thickness a,
                            selsa' = selsa a, namesa' = namesa a, colorsa' = colorsa a, stylesa' = stylesa a}
addGrad (LN a) = LN' $ Line' { startx_l' = r2f $ startx_l a, starty_l' = r2f $ starty_l a,
                            endx_l' = r2f $ endx_l a, endy_l' = r2f $ endy_l a, style_l' = style_l a,
                            thickness_l' = r2f $ thickness_l a, name_l' = name_l a, color_l' = color_l a }
addGrad (CB c) = CB' $ CubicBezier' { pathcb' = path, colorcb' = colorcb c, namecb' = namecb c, stylecb' = stylecb c }
    where path_flat = concatMap (\(x, y) -> [r2f x, r2f y]) $ pathcb c
          path      = map tuplify2 $ chunksOf 2 path_flat
addGrad (IM im) = IM' $ Img' { xim' = r2f $ xim im, yim' = r2f $ yim im, sizeXim' = r2f $ sizeXim im,
                           sizeYim' = r2f $ sizeYim im, selim' = selim im, nameim' = nameim im,
                           angim' = angim im , path' = path im}

addGrads :: (Autofloat a) => [Obj] -> [Obj' a]
addGrads = map addGrad

-- implements exterior point algo as described on page 6 here:
-- https://www.me.utexas.edu/~jensen/ORMM/supplements/units/nlp_methods/const_opt.pdf
-- the initial state (WRT violating constraints), initial weight, params, constraint normalization, etc.
-- have all been initialized or set earlier
stepObjs :: (Autofloat a) => a -> Params -> [Obj] -> ([Obj], Params)
stepObjs t sParams objs =
         let (epWeight, epStatus) = (weight sParams, optStatus sParams) in
         case epStatus of

         -- start the outer EP optimization and the inner unconstrained optimization, recording initial EPstate
         NewIter -> let status' = UnconstrainedRunning $ EPstate objs in
                    (objs', sParams { weight = initWeight, optStatus = status'} )

         -- check *weak* convergence of inner unconstrained opt.
         -- if UO converged, set opt state to converged and update UO state (NOT EP state)
         -- if not, keep running UO (inner state implicitly stored)
         -- note convergence checks are only on the varying part of the state
         UnconstrainedRunning lastEPstate ->  -- doesn't use last EP state
        --    let unconstrConverged = optStopCond gradEval in
           let unconstrConverged = epStopCond stateVarying stateVarying'
                                   (objFnApplied stateVarying) (objFnApplied stateVarying') in
           if unconstrConverged then
              let status' = UnconstrainedConverged lastEPstate in -- update UO state only!
              (objs', sParams { optStatus = status'}) -- note objs' (UO converged), not objs
           else (objs', sParams) -- update UO state but not EP state; UO still running

         -- check EP convergence. if converged then stop, else increase weight, update states, and run UO again
         -- TODO some trickiness about whether unconstrained-converged has updated the correct state
         -- and whether i should check WRT the updated state or not
         UnconstrainedConverged (EPstate lastEPstate) ->
           let (_, epStateVarying) = tupMap (map float2Double) $ unpackSplit
                                     $ addGrads lastEPstate in -- TODO factor out
           let epConverged = epStopCond epStateVarying stateVarying -- stateV is last state for converged UO
                                   (objFnApplied epStateVarying) (objFnApplied stateVarying) in
           if epConverged then
              let status' = EPConverged in -- no more EP state
              (objs, sParams { optStatus = status'}) -- do not update UO state
           -- update EP state: to be the converged state from the most recent UO
           else let status' = UnconstrainedRunning $ EPstate objs in -- increase weight
                (objs, sParams { weight = weightGrowth * epWeight, optStatus = status' })

         -- done; don't update obj state or params; user can now manipulate
         EPConverged -> (objs, sParams)

         -- TODO: implement EPConvergedOverride (for when the magnitude of the gradient is still large)

         -- TODO factor out--only unconstrainedRunning needs to run stepObjective, but EPconverged needs objfn
        where (fixed, stateVarying) = tupMap (map float2Double) $ unpackSplit $ addGrads objs
                      -- realToFrac used because `t` output is a Float? I don't really know why this works
              (stateVarying', objFnApplied, gradEval) = stepWithObjective objs fixed sParams
                                                             (realToFrac t) stateVarying
              -- re-pack each object's state list into object
              objs' = zeroGrads $ pack (annotations sParams) objs fixed stateVarying'

-- Given the time, state, and evaluated gradient (or other search direction) at the point,
-- return the new state. Note that the time is treated as `Floating a` (which is internally a Double)
-- not gloss's `Float`
stepT :: Floating a => a -> a -> a -> a
stepT dt x dfdx = x - dt * dfdx

-- Calculates the new state by calculating the directional derivatives (via autodiff)
-- and timestep (via line search), then using them to step the current state.
-- Also partially applies the objective function.
stepWithObjective :: (Autofloat a) =>
                  [Obj] -> [a] -> Params -> a -> [a] -> ([a], [a] -> a, [a])
stepWithObjective objs fixed stateParams t state = (steppedState, objFnApplied, gradEval)
                  where (t', gradEval) = timeAndGrad objFnApplied t state
                        -- get timestep via line search, and evaluated gradient at the state
                        -- step each parameter of the state with the time and gradient
                        -- gradEval :: [Double]; gradEval = [dfdx1, dfdy1, dfdsize1, ...]
                        steppedState = let state' = map (\(v, dfdv) -> stepT t' v dfdv) (zip state gradEval) in
                                       trStr ("||x' - x||: " ++ (show $ norm (state -. state'))
                                              ++ "\n|f(x') - f(x)|: " ++
                                             (show $ abs (objFnApplied state - objFnApplied state'))
                                              ++ "\ngradEval: \n" ++ (show gradEval)
                                              ++ "\nstate: \n" ++ (show objs) )
                                       state'
                        objFnApplied :: ObjFn1 a -- i'm not clear on why realToFrac is needed here either
                                     -- since everything should already be polymorphic
                        -- here, objFn is a function that gets the objective function from stateParams
                        -- note that the objective function is partially applied w/ current list of objects
                        objFnApplied = (objFn stateParams) objs (realToFrac cWeight) (map realToFrac fixed)
                        cWeight = weight stateParams

-- a version of grad with a clearer type signature
appGrad :: (Autofloat a) => (forall b . (Autofloat b) => [b] -> b) -> [a] -> [a]
appGrad f l = grad f l

appGrad' :: (Autofloat' a) =>
         (forall b . (Autofloat b) => [b] -> b) -> [a] -> [a]
appGrad' f l = grad f l

-- TODO: Autofloat these?
nanSub :: (RealFloat a, Floating a) => a
nanSub = 0

removeNaN' :: (RealFloat a, Floating a) => a -> a
removeNaN' x = if isNaN x then nanSub else x

removeNaN :: (RealFloat a, Floating a) => [a] -> [a]
removeNaN = map removeNaN'

removeInf' :: (RealFloat a, Floating a) => a -> a
removeInf' x = if isInfinity x then bignum else if isNegInfinity x then (-bignum) else x
           where bignum = 10**10

removeInf :: (RealFloat a, Floating a) => [a] -> [a]
removeInf = map removeInf'

tupMap :: (a -> b) -> (a, a) -> (b, b)
tupMap f (a, b) = (f a, f b)

-------------------

-- Given the objective function, gradient function, timestep, and current state,
-- return the timestep (found via line search) and evaluated gradient at the current state.
-- TODO change stepWithGradFn(s) to use this fn and its type
-- note: continue to use floats throughout the code, since gloss uses floats
-- the autodiff library requires that objective functions be polymorphic with Floating a
timeAndGrad :: (Autofloat b) => ObjFn1 a -> b -> [b] -> (b, [b])
timeAndGrad f t state = tr "timeAndGrad: " (timestep, gradEval)
            where gradF :: GradFn a
                  gradF = appGrad f
                  gradEval = gradF (tr "STATE: " state)
                  -- Use line search to find a good timestep.
                  -- Redo if it's NaN, defaulting to 0 if all NaNs. TODO
                  descentDir = negL gradEval
                  -- timestep :: Floating c => c
                  timestep = if not linesearch then t else -- use a fixed timestep for debugging
                             let resT = awLineSearch f duf descentDir state in
                             if isNaN resT then tr "returned timestep is NaN" nanSub else resT
                  -- directional derivative at u, where u is the negated gradient in awLineSearch
                  -- descent direction need not have unit norm
                  -- we could also use a different descent direction if desired
                  duf :: (Autofloat a) => [a] -> [a] -> a
                  duf u x = gradF x `dotL` u

-- Parameters for Armijo-Wolfe line search
-- NOTE: must maintain 0 < c1 < c2 < 1
c1 :: Floating a => a
c1 = 0.4 -- for Armijo, corresponds to alpha in backtracking line search (see below for explanation)
-- smaller c1 = shallower slope = less of a decrease in fn value needed = easier to satisfy
-- turn Armijo off: c1 = 0

c2 :: Floating a => a
c2 = 0.2 -- for Wolfe, is the factor decrease needed in derivative value
-- new directional derivative value / old DD value <= c2
-- smaller c2 = smaller new derivative value = harder to satisfy
-- turn Wolfe off: c1 = 1 (basically backatracking line search only)

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)
-- all numbers are smaller than infinity except infinity, to which it's equal

negInfinity :: Floating a => a
negInfinity = -infinity

isInfinity x = (x == infinity)
isNegInfinity x = (x == negInfinity)

-- Implements Armijo-Wolfe line search as specified in Keenan's notes, converges on nonconvex fns as well
-- based off Lewis & Overton, "Nonsmooth optimization via quasi-Newton methods", page TODO
-- duf = D_u(f), the directional derivative of f at descent direction u
-- D_u(x) = <gradF(x), u>. If u = -gradF(x) (as it is here), then D_u(x) = -||gradF(x)||^2
-- TODO summarize algorithm
-- TODO what happens if there are NaNs in awLineSearch? or infinities
awLineSearch :: (Autofloat b) => ObjFn1 a -> ObjFn2 a -> [b] -> [b] -> b
awLineSearch f duf_noU descentDir x0 =
             -- results after a&w are satisfied are junk and can be discarded
             -- drop while a&w are not satisfied OR the interval is large enough
    --  let (af, bf, tf) = head $ dropWhile intervalOK_or_notArmijoAndWolfe
    --                           $ iterate update (a0, b0, t0) in tf
     let (numUpdatas, (af, bf, tf)) = head $ dropWhile (intervalOK_or_notArmijoAndWolfe . snd)
                              $ zip [0..] $ iterate update (a0, b0, t0) in
                            --   trRaw ("Linear search update count: " ++ show numUpdatas) $
                              tf
          where (a0, b0, t0) = (0, infinity, 1)
                duf = duf_noU descentDir
                update (a, b, t) =
                       let (a', b', sat) = if not $ armijo t then tr' "not armijo" (a, t, False)
                                           else if not $ weakWolfe t then tr' "not wolfe" (t, b, False)
                                           -- remember to change both wolfes
                                           else (a, b, True) in
                       if sat then (a, b, t) -- if armijo and wolfe, then we use (a, b, t) as-is
                       else if b' < infinity then tr' "b' < infinity" (a', b', (a' + b') / 2)
                       else tr' "b' = infinity" (a', b', 2 * a')
                intervalOK_or_notArmijoAndWolfe (a, b, t) = not $
                      if armijo t && weakWolfe t then -- takes precedence
                           tr ("stop: both sat. |-gradf(x0)| = " ++ show (norm descentDir)) True
                      else if abs (b - a) < minInterval then
                           tr ("stop: interval too small. |-gradf(x0)| = " ++ show (norm descentDir)) True
                      else False -- could be shorter; long for debugging purposes
                armijo t = (f ((tr' "** x0" x0) +. t *. (tr' "descentDir" descentDir))) <= ((tr' "fAtX0"fAtx0) + c1 * t * (tr' "dufAtX0" dufAtx0))
                strongWolfe t = abs (duf (x0 +. t *. descentDir)) <= c2 * abs dufAtx0
                weakWolfe t = duf_x_tu >= (c2 * dufAtx0) -- split up for debugging purposes
                          where duf_x_tu = tr' "Duf(x + tu)" (duf (x0 +. t' *. descentDir'))
                                t' = tr' "t" t
                                descentDir' = descentDir --tr' "descentDir" descentDir
                dufAtx0 = duf x0 -- cache some results, can cache more if needed
                fAtx0 = f x0 -- TODO debug why NaN. even using removeNaN' didn't help
                minInterval = if intervalMin then 10 ** (-10) else 0
                -- stop if the interval gets too small; might not terminate

------------------------ ### frequently-changed params for debugging

objsInit = []

-- Flags for debugging the surrounding functions.
clampflag = False
-- debug = True
-- debug = False
-- debugLineSearch = False
-- debugObj = False -- turn on/off output in obj fn or constraint
constraintFlag = True
objFnOn = True -- turns obj function on or off in exterior pt method (for debugging constraints only)
constraintFnOn = True -- TODO need to implement constraint fn synthesis

type ObjFnPenalty a = forall a . (Autofloat a) => a -> [a] -> [a] -> a
-- needs to be partially applied with the current list of objects
-- this type is only for the TOP-LEVEL synthesized objective function, not for any of the ones that people write
type ObjFnPenaltyState a = forall a . (Autofloat a) => [Obj] -> a -> [a] -> [a] -> a

-- TODO should use objFn as a parameter
objFnPenalty :: ObjFnPenalty a
objFnPenalty weight = combineObjfns objFnUnconstrained weight
             where objFnUnconstrained :: Floating a => ObjFn2 a
                --    objFnUnconstrained = centerObjs -- centerAndRepel
                   objFnUnconstrained = centerAndRepel

-- if the list of constraints is empty, it behaves as unconstrained optimization
boundConstraints :: Constraints
boundConstraints = [] -- first_two_objs_box

weightGrowth :: Floating a => a -- for EP weight
weightGrowth = 10

epStop :: Floating a => a -- for EP diff
epStop = 10 ** (-3)
-- epStop = 60 ** (-3)
-- epStop = 10 ** (-1)
-- epStop = 0.05

-- for use in barrier/penalty method (interior/exterior point method)
-- seems if the point starts in interior + weight starts v small and increases, then it converges
-- not quite... if the weight is too small then the constraint will be violated
initWeight :: Floating a => a
initWeight = 10 ** (-5)
-- initWeight = 10 ** (-3)


stopEps :: Floating a => a
stopEps = 10 ** (-1)

------------ Various constants and helper functions related to objective functions

-- epsd :: Floating a => a -- to prevent 1/0 (infinity). put it in the denominator
-- epsd = 10 ** (-10)

objText = "objective: center all sets; center all labels in set"
constrText = "constraint: satisfy constraints specified in Substance program"

-- separates fixed parameters (here, size) from varying parameters (here, location)
-- ObjFn2 has two parameters, ObjFn1 has one (partially applied)
type ObjFn2 a = forall a . (Autofloat a) => [a] -> [a] -> a

linesearch = True -- TODO move these parameters back
intervalMin = True -- true = force linesearch halt if interval gets too small; false = no forced halt

sumMap :: Floating b => (a -> b) -> [a] -> b -- common pattern in objective functions
sumMap f l = sum $ map f l

-------------- Sample bound constraints

-- TODO test bound constraints with EP, keep separate and formally build in if it doesn't work
-- TODO add more constraints for testing

-- x-coord of first object's center in [-300,-200], y-coord of first object's center in [0, 200]
first_two_objs_box :: Constraints
first_two_objs_box = [(0, (-300, -100)), (1, (0, 200)), (4, (100, 300)), (5, (-100, -400))]

-------------- Objective functions

-- simple test function
minx1 :: ObjFn2 a -- timestep t
minx1 _ xs = if length xs == 0 then error "minx1 empty list" else (head xs)^2

-- only center the first object (for debugging). NOTE: need to pass in parameters in the right order
centerObjNoSqrt :: ObjFn2 a
centerObjNoSqrt _ (x1 : y1 : _) = x1^2 + y1^2 -- sum $

-- center both objects without sqrt
centerObjsNoSqrt :: ObjFn2 a
centerObjsNoSqrt _ = sumMap (^2)

centerx1Sqrt :: ObjFn2 a -- discontinuous, timestep = 100 * t. autodiff behaves differently for this vs abs
centerx1Sqrt _ (x1 : _) = sqrt $ x1^2

-- lot of "interval too small"s happening with the objfns on lists now
centerObjs :: ObjFn2 a -- with sqrt
centerObjs fixed = sqrt . (centerObjsNoSqrt fixed)

-- Repel two objects
repel2 :: ObjFn2 a
repel2 _ [x1, y1, x2, y2] = 1 / ((x1 - x2)^2 + (y1 - y2)^2 + epsd)

-- pairwise repel on a list of objects (by distance b/t their centers)
repelCenter :: ObjFn2 a
repelCenter _ locs = sumMap (\x -> 1 / (x + epsd)) denoms
                 where denoms = map diffSq allPairs
                       diffSq [[x1, y1], [x2, y2]] = (x1 - x2)^2 + (y1 - y2)^2
                       allPairs = filter (\x -> length x == 2) $ subsequences objs
                       -- TODO implement more efficient version. also, subseq only returns *unique* subseqs
                       objs = chunksOf 2 locs

-- does not deal with labels
centerAndRepel :: ObjFn2 a -- timestep t
centerAndRepel fixed varying = centerObjsNoSqrt fixed varying + weight * repelCenter fixed varying
                   where weight = 10 ** (9.8) -- TODO calculate this weight as a function of radii and bbox

-- pairwise repel on a list of objects (by distance b/t their centers)
-- TODO: version of above function that separates fixed parameters (size) from varying parameters (location)
-- assuming 1 size for each two locs, and s1 corresponds to x1, y1 (and so on)
repelDist :: ObjFn2 a
repelDist sizes locs = sumMap (\x -> 1 / (x + epsd)) denoms
                 where denoms = map diffSq allPairs
                       diffSq [[x1, y1, s1], [x2, y2, s2]] = (x1 - x2)^2 + (y1 - y2)^2 - s1 - s2
                       allPairs = filter (\x -> length x == 2) $ subsequences objs
                       objs = zipWith (++) locPairs sizes'
                       (sizes', locPairs) = (map (\x -> [x]) sizes, chunksOf 2 locs)

-- attempts to account for the radii of the objects
-- currently, they repel each other "too much"--want them to be as centered as possible
-- not sure whether to use sqrt or not
-- try multiple objects?
centerAndRepel_dist :: ObjFn2 a
centerAndRepel_dist fixed varying = centerObjsNoSqrt fixed varying + weight * (repelDist fixed varying)
       where weight = 10 ** 10

-----

doNothing :: ObjFn2 a -- for debugging
doNothing _ _ = 0

nonDifferentiable :: ObjFn2 a
nonDifferentiable sizes locs = let q = head locs in
                  -- max q 0
                  abs q -- actually works fine with the line search

-- TODO these need separate pack/unpack functions because they change the sizes. these don't currently work
grow2 :: ObjFn2 a
grow2 _ [_, _, s1, _, _, s2] = 1 / (s1 + epsd) + 1 / (s2 + epsd)

grow :: ObjFn2 a
grow _ varying = sumMap (\x -> 1 / (x + epsd)) $ varying

-- TODO this needs to use the set size info. hardcode radii for now
-- TODO to use "min rad rad1" we need to add "Ord a" to the type signatures everywhere
-- we want the distance between the sets to be <overlap> less than having them just touch
-- this isn't working? and isn't resampling?
-- also i'm getting interval shrinking problems just with this function (using 'distance' only)
setsIntersect2 :: ObjFn2 a
setsIntersect2 sizes [x1, y1, x2, y2] = (dist (x1, y1) (x2, y2) - overlap)^2
              where overlap = rad + rad1 - 0.5 * rad1 -- should be "min rad rad1"

------ Objective function to place a label either inside of or right outside of a set

eps' :: Floating a => a
eps' = 60 -- why is this 100??

-- two parabolas, one at f(d) = d^2 and one at f(d) = (d-c)^2, intersecting at c/2
-- (i could try making the first one bigger and solving for the new intersection pt if i want the threshold
-- to be greater than (r + margin)/2

-- note: whenever an objective function has a partial derivative that might be fractional,
-- and vary in the denominator, need to add epsilon to denominator to avoid 1/0, or avoid it altogether
-- e.g. f(x) = sqrt(x) -> f'(x) = 1/(2sqrt(x))
-- in the first branch, we square the distance, because the objective there is to minimize the distance (resulting in 1/0).
-- in the second branch, the objective is to keep the distance at (r_set + margin), not at 0--so there’s no NaN in the denominator
centerOrRadParabola2 :: Bool -> ObjFn2 a
centerOrRadParabola2 inSet [r_set, _] [x1, y1, x2, y2] =
                     if dsq <= r_set^2 then dsq
                     else (if inSet then dsq else coeff * (d - const)^2) -- false -> can lay it outside as well
                     where d = dist (x1, y1) (x2, y2) -- + epsd
                           dsq = distsq (x1, y1) (x2, y2) -- + epsd
                           coeff = r_set^2 / (r_set - const)^2 -- chosen s.t. parabolas intersect at r
                           const = r_set + margin -- second parabola's zero
                           margin = if r_set <= 30 then 30 else 60  -- distance from edge of set (as a fn of r)
                           -- we want r to be close to r_set+margin, otherwise if r is small it converges slowly?

-- NOTE: assumes that object and label are exactly contiguous in list: sizes of [o1, l1, o2, l2...]
-- and locs: [x_o1, y_o1, x_l1, y_l1, x_o2, y_o2, x_l2, y_l2...]
-- TODO abstract out repelDist / labelSum pattern
labelSum :: Bool -> ObjFn2 a
labelSum inSet objLabelSizes objLabelLocs =
               let objLabelSizes' = chunksOf 2 objLabelSizes in
               let objLabelLocs' = chunksOf 4 objLabelLocs in
               sumMap (\(sizes, locs) -> centerOrRadParabola2 inSet sizes locs) (zip objLabelSizes' objLabelLocs')

------

-- TODO: label-only obj fns don't work out-of-the-box with set-only obj fns since they do unpacking differently

-- Start composing set-wise functions (centerAndRepel) with set-label functions (labelSum)
-- Sets repel each other, labels repel each other, and sets are labeled
-- TODO non-label sets should repel those labels
-- TODO resample initial state s.t. labels start inside the set (the centerOrRad is mostly useful if there are other objects inside the set that might repel the label)
-- TODO abstract out the unpacking functions here and factor out the weights
centerRepelLabel :: ObjFn2 a
centerRepelLabel olSizes olLocs =
                 centerAndRepel oSizes oLocs + weight * repelCenter lSizes lLocs + labelSum inSet olSizes olLocs
                 where (oSizes, lSizes) = (map fst zippedSizes, map snd zippedSizes)
                       zippedSizes = map (\[obj, lab] -> (obj, lab)) $ chunksOf 2 olSizes
                       (oLocs, lLocs) = (concatMap fst zippedLocs, concatMap snd zippedLocs)
                       zippedLocs = map (\[xo, yo, xl, yl] -> ([xo, yo], [xl, yl])) $ chunksOf 4 olLocs
                       weight = 10 ** 6
                       inSet = True -- label only in set vs. in or at radius


---------------- Exterior point method functions
-- Given an objective function and a list of constraints (phrased in terms of violations on a list of floats),
-- combines them using the penalty method (parametrized by a constraint weight over the sum of the constraints,
-- & individual normalizing weights on each constraint). Returns the corresponding unconstrained objective fn,
-- for use in unconstrained opt with line search.

-- PAIRWISE constraint functions that return the magnitude of violation
-- same type as ObjFn2; more general than PairConstrV
type StateConstrV a = forall a . (Floating a, Ord a, Show a) => [a] -> [a] -> a
-- type PairConstrV a = forall a . (Floating a, Ord a, Show a) => [[a]] -> a -- takes pairs of "packed" objs

-- NOTE: some old code from Katherine here. Keeping them for reference just in case. Consider removal in the future
-- noConstraint :: PairConstrV a
-- noConstraint _ = 0
--
-- -- To convert your inequality constraint into a violation to be penalized:
-- -- it needs to be in the form "c < 0" and c is the violation penalized if > 0
-- -- so e.g. if you want "x < -100" then you would convert it to "x + 100 < 0" with c = x + 100
-- -- if you want "f x > -100" then you would convert it to "-(f x + 100) < 0" with c = -(f x + 100)"
--
-- -- all sets must pairwise-strict-intersect
-- -- plus an offset so they overlap by a visible amount (perhaps this should be an optimization parameter?)
-- looseIntersect :: PairConstrV a
-- looseIntersect [[x1, y1, s1], [x2, y2, s2]] = let offset = 10 in
--         -- if s1 + s2 < offset then error "radii too small"  --TODO: make it const
--         -- else
--             dist (x1, y1) (x2, y2) - (s1 + s2 - offset)
--
-- -- the energy actually increases so it always settles around the offset
-- -- that's because i am centering all of them--test w/objective off
-- -- TODO flatten energy afterward, or get it to be *far* from the other set
-- -- offset so the sets differ by a visible amount
-- noSubset :: PairConstrV a
-- noSubset [[x1, y1, s1], [x2, y2, s2]] = let offset = 10 in -- max/min dealing with s1 > s2 or s2 < s1
--          -(dist (x1, y1) (x2, y2)) + max s2 s1 - min s2 s1 + offset
--
-- -- the first set is the subset of the second, and thus smaller than the second in size.
-- -- TODO: test for equal sets
-- -- TODO: for two primitives we have 4 functions, which is not sustainable. NOT NEEDED, remove them.
-- strictSubset :: PairConstrV a
-- strictSubset [[x1, y1, s1], [x2, y2, s2]] = dist (x1, y1) (x2, y2) - (s2 - s1)
--
-- -- exterior point method constraint: no intersection (meaning also no subset)
-- noIntersectExt :: PairConstrV a
-- noIntersectExt [[x1, y1, s1], [x2, y2, s2]] = -(dist (x1, y1) (x2, y2)) + s1 + s2 + offset where offset = 10
--
-- pointInExt :: PairConstrV a
-- pointInExt [[x1, y1], [x2, y2, r]] = dist (x1, y1) (x2, y2) - 0.5 * r
--
-- pointNotInExt :: PairConstrV a
-- pointNotInExt [[x1, y1], [x2, y2, r]] = - dist (x1, y1) (x2, y2) + r
--
-- -- exterior point method: penalty function
-- penalty :: (Ord a, Floating a, Show a) => a -> a
-- penalty x = (max x 0) ^ q -- weights should get progressively larger in cr_dist
--             where  q = 2 -- also, may need to sample OUTSIDE feasible set
--             -- where q = 3 -- also, may need to sample OUTSIDE feasible set

-- for each pair, for each constraint on that pair, compose w/ penalty function and sum
-- TODO add vector of normalization constants for each constraint
pairToPenalties :: PairConstrV a
pairToPenalties pair = sum $ map (\((f, w), p) -> w * (penalty $ f p)) $ zip pairConstrVs (repeat pair)

-- sum penalized violations of each constraint on the whole state
stateConstrsToObjfn :: ObjFn2 a
stateConstrsToObjfn fixed varying = sum $ map (\((f, w), (fix, vary)) -> w * (penalty $ f fix vary))
                    $ zip stateConstrVs (repeat (fixed, varying))

-- the overall penalty function is the sum m (unweighted)
-- generate all unique pairs of objs and sum the penalized violation on each pair
pairConstrsToObjfn :: ObjFn2 a
pairConstrsToObjfn sizes locs = sumMap pairToPenalties allPairs
                 where -- generates all *unique* pairs (does not generate e.g. (o1, o2) and (o2, o1))
                       allPairs = filter (\x -> length x == 2) $ subsequences objs
                       objs = zipWith (++) locPairs sizes'
                       (sizes', locPairs) = (map (\x -> [x]) sizes, chunksOf 2 locs)

-- add the obj fn value to all penalized violations of constraints
-- note that a high weight may result in an "ill-conditioned hessian" with high differences b/t eigenvalues
-- with which the line search and stopping conditions may have trouble
-- https://www.researchgate.net/post/What_is_stopping_criteria_of_any_optimization_algorithm
combineObjfns :: ObjFn2 a -> ObjFnPenalty a
combineObjfns objfn weight fixed varying = -- input objfn is unconstrained
             (if objFnOn then tro "obj val" $ objWeight * objfn fixed varying else 0)
             + (if constraintFnOn then tro "penalty val" $
                   weight * (pairConstrsToObjfn fixed varying + stateConstrsToObjfn fixed varying)
                else 0)
             where objWeight = 1

-- constraint functions that act on the entire state
-- this one just acts on the first object and ignores the fixed params
firstObjInBbox :: (Floating a, Ord a, Show a) => (a, a, a, a) -> [([a] -> [a] -> a, a)]
firstObjInBbox (l, r, b, t) = [(leftBound, 1), (rightBound, 1), (botBound, 1), (topBound, 1)]
               where leftBound fixed (x1 : _ : _) = -(x1 - l)
                     leftBound _ _ = error "not enough floats in state to apply constr function firstObjInBbox"
                     rightBound fixed (x1 : _ : _) = x1 - r
                     botBound fixed (_ : y1 : _) = -(y1 - b)
                     topBound fixed (_ : y1 : _) = y1 - t

stateConstrVs :: (Floating a, Ord a, Show a) => [([a] -> [a] -> a, a)] -- constr, constr weight
stateConstrVs = -- firstObjInBbox (leftb, rightb, botb, topb)
                -- ++ firstObjInBbox (-pw2', pw2', -ph2', ph2') -- first object in viewport, TODO for all objs
                [] -- TODO add more

-- Parameter to modify (TODO move it to other section)
-- [PairConstrV a] is not allowed b/c impredicative types
pairConstrVs :: (RealFloat a, Floating a, Ord a, Show a) => [([[a]] -> a, a)] -- constr, constr weight
pairConstrVs = [(noSubset, 1)]

-- It's not clear what happens with contradictory constraints like these:
-- It looks like one pair satisfies strict subset, and the other pairs all intersect
-- pairConstrVs = [(strictSubset, 1), (noIntersectExt, 1)]

-- Corners for hard-coded bounding box constraint.
leftb :: Floating a => a
leftb = -200

rightb :: Floating a => a
rightb = 100

botb :: Floating a => a
botb = 0

topb :: Floating a => a
topb = 200
