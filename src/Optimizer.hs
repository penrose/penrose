{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction #-}

module Optimizer where

import Utils
import NewStyle
import Numeric.AD

------ Opt types, util functions, and params

type ObjFn1 a = forall a . (Autofloat a) => [a] -> a

-- used for duf
type ObjFn2 a = forall a . (Autofloat a) => [a] -> [a] -> a

type GradFn a = forall a . (Autofloat a) => [a] -> [a]

weightGrowthFactor :: (Autofloat a) => a -- for EP weight
weightGrowthFactor = 10

epStop :: Floating a => a -- for EP diff
-- epStop = 10 ** (-3)
-- epStop = 60 ** (-3)
epStop = 10 ** (-1)
-- epStop = 0.05

-- convergence criterion for EP
-- if you want to use it for UO, needs a different epsilon
epStopCond :: (Autofloat a) => [a] -> [a] -> a -> a -> Bool
epStopCond x x' fx fx' =
           trStr ("EP: \n||x' - x||: " ++ (show $ norm (x -. x'))
           ++ "\n|f(x') - f(x)|: " ++ (show $ abs (fx - fx'))) $
           (norm (x -. x') <= epStop) || (abs (fx - fx') <= epStop)

nanSub :: (Autofloat a) => a
nanSub = 0

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

-- true = force linesearch halt if interval gets too small; false = no forced halt
intervalMin = True

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)
-- all numbers are smaller than infinity except infinity, to which it's equal

---------------------------------------

-- Main optimization functions

step :: RState -> RState
step s = let (state', params') = stepShapes (paramsr s)  (varyingState s) in
         let shapes' = evalTranslation s in
         s { varyingState = state', shapesr = shapes', paramsr = params' }
         -- note: trans is not updated in rstate


-- Note use of realToFrac to generalize type variables (on the weight and on the varying state)

-- implements exterior point algo as described on page 6 here:
-- https://www.me.utexas.edu/~jensen/ORMM/supplements/units/nlp_methods/const_opt.pdf
stepShapes :: (Autofloat a) => Params -> [a] -> ([a], Params)
stepShapes params vstate = -- varying state
         let (epWeight, epStatus) = (weight params, optStatus params) in
         case epStatus of

         -- start the outer EP optimization and the inner unconstrained optimization, recording initial EPstate
         NewIter -> let status' = UnconstrainedRunning (map realToFrac vstate) in
                    (vstate', params { weight = initWeight, optStatus = status'} )
         -- check *weak* convergence of inner unconstrained opt.
         -- if UO converged, set opt state to converged and update UO state (NOT EP state)
         -- if not, keep running UO (inner state implicitly stored)
         -- note convergence checks are only on the varying part of the state
         UnconstrainedRunning lastEPstate ->  -- doesn't use last EP state
           -- let unconstrConverged = optStopCond gradEval in
           let unconstrConverged = epStopCond vstate vstate'
                                   (objFnApplied vstate) (objFnApplied vstate') in
           if unconstrConverged then
              let status' = UnconstrainedConverged lastEPstate in -- update UO state only!
              (vstate', params { optStatus = status'}) -- note vstate' (UO converged), not vstate
           else (vstate', params) -- update UO state but not EP state; UO still running

         -- check EP convergence. if converged then stop, else increase weight, update states, and run UO again
         -- TODO some trickiness about whether unconstrained-converged has updated the correct state
         -- and whether to check WRT the updated state or not
         UnconstrainedConverged lastEPstate ->
           let epConverged = epStopCond lastEPstate vstate -- lastEPstate is last state for converged UO
                                   (objFnApplied lastEPstate) (objFnApplied vstate) in
           if epConverged then
              let status' = EPConverged in -- no more EP state
              (vstate, params { optStatus = status'}) -- do not update UO state
           -- update EP state: to be the converged state from the most recent UO
           else let status' = UnconstrainedRunning (map realToFrac vstate) in -- increase weight
                (vstate, params { weight = weightGrowthFactor * epWeight, optStatus = status' })

         -- done; don't update obj state or params; user can now manipulate
         EPConverged -> (vstate, params)

         -- TODO: implement EPConvergedOverride (for when the magnitude of the gradient is still large)

         -- TODO factor out--only unconstrainedRunning needs to run stepObjective, but EPconverged needs objfn
        where (vstate', gradEval) = stepWithObjective params vstate
              objFnApplied = (overallObjFn params) (weight params)

-- Given the time, state, and evaluated gradient (or other search direction) at the point,
-- return the new state. Note that the time is treated as `Floating a` (which is internally a Double)
-- not gloss's `Float`
stepT :: Floating a => a -> a -> a -> a
stepT dt x dfdx = x - dt * dfdx

-- Calculates the new state by calculating the directional derivatives (via autodiff)
-- and timestep (via line search), then using them to step the current state.
-- Also partially applies the objective function.
stepWithObjective :: (Autofloat a) => Params -> [a] -> ([a], [a])
stepWithObjective params state = (steppedState, gradEval)
    where (t', gradEval) = timeAndGrad objFnApplied state
          -- get timestep via line search, and evaluated gradient at the state
          -- step each parameter of the state with the time and gradient
          -- gradEval :: (Autofloat) a => [a]; gradEval = [dfdx1, dfdy1, dfdsize1, ...]
          steppedState =
              let state' = map (\(v, dfdv) -> stepT t' v dfdv) (zip state gradEval) in
                         trStr ("||x' - x||: " ++ (show $ norm (state -. state'))
                                ++ "\n|f(x') - f(x)|: " ++
                               (show $ abs (objFnApplied state - objFnApplied state'))
                                ++ "\ngradEval: \n" ++ (show gradEval)
                                ++ "\nstate: \n" ++ (show state') )
                         state'

          objFnApplied :: ObjFn1 b
          objFnApplied = (overallObjFn params) cWeight
          cWeight = realToFrac $ weight params
          -- realToFrac generalizes the type variable `a` to the type variable `b`, which timeAndGrad expects

-- a version of grad with a clearer type signature
appGrad :: (Autofloat a) => (forall b . (Autofloat b) => [b] -> b) -> [a] -> [a]
appGrad f l = grad f l

-- Given the objective function, gradient function, timestep, and current state,
-- return the timestep (found via line search) and evaluated gradient at the current state.
-- the autodiff library requires that objective functions be polymorphic with Floating a
timeAndGrad :: (Autofloat b) => ObjFn1 a -> [b] -> (b, [b])
timeAndGrad f state = tr "timeAndGrad: " (timestep, gradEval)
            where gradF :: GradFn a
                  gradF = appGrad f
                  gradEval = gradF (tr "STATE: " state)
                  -- Use line search to find a good timestep.
                  -- Redo if it's NaN, defaulting to 0 if all NaNs. TODO
                  descentDir = negL gradEval
                  -- timestep :: Floating c => c
                  timestep =
                      let resT = awLineSearch f duf descentDir state in
                      -- let resT = r2f 0.001 in
                             if isNaN resT then tr "returned timestep is NaN" nanSub else resT
                  -- directional derivative at u, where u is the negated gradient in awLineSearch
                  -- descent direction need not have unit norm
                  -- we could also use a different descent direction if desired
                  duf :: (Autofloat a) => [a] -> [a] -> a
                  duf u x = gradF x `dotL` u

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
