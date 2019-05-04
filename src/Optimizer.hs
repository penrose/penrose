{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns, FlexibleInstances #-}

module Optimizer where

import Utils
import Style
import GenOptProblem
import Numeric.AD
import Numeric.AD.Internal.On
import Numeric.AD.Internal.Reverse
import Numeric.AD.Internal.Sparse
import qualified Numeric.LinearAlgebra as L
import Debug.Trace
import System.Random
import System.Console.ANSI
import Data.List (foldl')

default (Int, Float)

------ Opt types, util functions, and params

type ObjFn1 a = forall a . (Autofloat a) => [a] -> a

-- used for duf
type ObjFn2 a = forall a . (Autofloat a) => [a] -> [a] -> a

type GradFn a = forall a . (Autofloat a) => [a] -> [a]

----- Various consts

nanSub :: (Autofloat a) => a
nanSub = 0

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)
-- all numbers are smaller than infinity except infinity, to which it's equal

----- Hyperparameters

weightGrowthFactor :: (Autofloat a) => a -- for EP weight
weightGrowthFactor = 10

epsUnconstr :: Floating a => a
epsUnconstr = 10 ** (-2)

epStop :: Floating a => a -- for EP diff
epStop = 10 ** (-5)
-- epStop = 60 ** (-3)
-- epStop = 10 ** (-1)
-- epStop = 0.05

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

useLineSearch :: Bool
useLineSearch = True

useAutodiff :: Bool
useAutodiff = True

constT :: Floating a => a
constT = 0.001

debugBfgs = True

trb :: String -> a -> a
trb s x = if debugBfgs then trace "---" $ trace s x else x -- prints in left to right order

-- turn on/off output in obj fn or constraint
debugOpt = True

tro :: String -> a -> a
tro s x = if debugOpt then trace "---" $ trace s x else x -- prints in left to right order


----- Convergence criteria

-- convergence criterion for EP
-- if you want to use it for UO, needs a different epsilon
epStopCond :: (Autofloat a) => [a] -> [a] -> a -> a -> Bool
epStopCond x x' fx fx' =
           tro ("EP: \n||x' - x||: " ++ (show $ norm (x -. x'))
           ++ "\n|f(x') - f(x)|: " ++ (show $ abs (fx - fx'))) $
           (norm (x -. x') <= epStop) || (abs (fx - fx') <= epStop)

unconstrainedStopCond :: (Autofloat a) => [a] -> Bool
unconstrainedStopCond gradEval = norm gradEval < epsUnconstr

---------------------------------------

-- Policies

stepPolicy :: State -> (Params, PolicyParams)
stepPolicy s = 
    -- Check overall convergence first 
    let epStatus = optStatus $ (paramsr s) in
    let pparams = policyParams s in
    case epStatus of

    -- Generate new objective function and replace the optimization and policy params accordingly
    EPConverged -> 
                -- TODO: clean up the step incrementing
        let pparams' = pparams { policySteps = 1 + policySteps pparams } in
        let (policyRes, psNew) = (policyFn s) (objFns s) (constrFns s) pparams' in -- See what the policy function wants
        case policyRes of
            Nothing     -> (paramsr s, pparams' { policyState = psNew }) -- steps incremented, policy done

            Just newFns -> -- Policy keeps going
                let objFnNew = genObjfn (transr s) (filter isObjFn newFns) (filter isConstr newFns) (varyingPaths s) 
                    -- TODO: check that these inputs are right
                    -- Change obj function and restart optimization
                    pparamsNew = pparams' { policyState = psNew,
                                            currFns = newFns }
                    paramsNew = Params { weight = initWeight,
                                         optStatus = NewIter,
                                         overallObjFn = objFnNew,
                                         bfgsInfo = defaultBfgsParams }
                in tro ("Step policy, EP converged, new params:\n" ++ show (paramsNew, pparamsNew, newFns)) $ (paramsNew, pparamsNew)

    -- If not converged, optimize as usual, don't change policy mid-optimization
    _ -> tro ("Step policy, EP not converged, new params:\n" ++ show (paramsr s, pparams)) $ (paramsr s, pparams)

---------------------------------------

-- Main optimization functions

step :: State -> State
step s = let (state', params') = stepShapes (oConfig s) (paramsr s) (varyingState s) (rng s)
             s'                = s { varyingState = state', 
                                     paramsr = params' }
             -- NOTE: we intentionally discard the random generator here because
             -- we want to have consistent computation output in a single
             -- optimization session
             -- For the same reason, all subsequent step* functions such as
             -- stepShapes do not return the new random generator
             (!shapes', _, _)     = evalTranslation s'

             -- Check the state and see if the overall objective function should be changed
             -- The policy may change EPConverged to a new iteration before the frontend sees it
             (paramsNew, pparamsNew) = stepPolicy s'

             -- For debugging
             oldParams = paramsr s

         in tro ("Params: \n" ++ show oldParams ++ "\n:") $
            s' { shapesr = shapes',
                 paramsr = paramsNew,
                 policyParams = pparamsNew } 
            -- note: trans is not updated in state

-- Note use of realToFrac to generalize type variables (on the weight and on the varying state)

-- implements exterior point algo as described on page 6 here:
-- https://www.me.utexas.edu/~jensen/ORMM/supplements/units/nlp_methods/const_opt.pdf
stepShapes :: OptConfig -> Params -> [Float] -> StdGen -> ([Float], Params)
stepShapes config params vstate g = -- varying state
         -- if null vstate then error "empty state in stepshapes" else
         let (epWeight, epStatus) = (weight params, optStatus params) in
         case epStatus of

         -- start the outer EP optimization and the inner unconstrained optimization, recording initial EPstate
         NewIter -> let status' = UnconstrainedRunning (map realToFrac vstate) in
                    (vstate', params { weight = initWeight, optStatus = status', bfgsInfo = defaultBfgsParams } )
         -- check *weak* convergence of inner unconstrained opt.
         -- if UO converged, set opt state to converged and update UO state (NOT EP state)
         -- if not, keep running UO (inner state implicitly stored)
         -- note convergence checks are only on the varying part of the state
         UnconstrainedRunning lastEPstate ->  -- doesn't use last EP state
           -- let unconstrConverged = unconstrainedStopCond gradEval in
           let unconstrConverged = epStopCond vstate vstate' (objFnApplied vstate) (objFnApplied vstate') in
               -- Two stopping conditions
               -- unconstrainedStopCond gradEval in
           if unconstrConverged then
              let status' = UnconstrainedConverged lastEPstate in -- update UO state only!
              (vstate', params { optStatus = status', bfgsInfo = defaultBfgsParams }) -- note vstate' (UO converged), not vstate
           else (vstate', params { bfgsInfo = bfgs' }) -- update UO state but not EP state; UO still running

         -- check EP convergence. if converged then stop, else increase weight, update states, and run UO again
         -- TODO some trickiness about whether unconstrained-converged has updated the correct state
         -- and whether to check WRT the updated state or not
         UnconstrainedConverged lastEPstate ->
           let epConverged = epStopCond lastEPstate (map r2f vstate) -- lastEPstate is last state for converged UO
                                   (objFnApplied lastEPstate) (objFnApplied (map r2f vstate)) in
           if epConverged then
              let status' = EPConverged in -- no more EP state
              (vstate, params { optStatus = status', bfgsInfo = defaultBfgsParams }) -- do not update UO state
           -- update EP state: to be the converged state from the most recent UO
           else let status' = UnconstrainedRunning (map realToFrac vstate) in -- increase weight
                let epWeight' = weightGrowthFactor * epWeight in
                -- trace ("Unconstrained converged. New weight: " ++ show epWeight') $
                      (vstate, params { weight = epWeight', optStatus = status', bfgsInfo = defaultBfgsParams })

         -- done; don't update obj state or params; user can now manipulate
         EPConverged -> (vstate, params { bfgsInfo = defaultBfgsParams } )

         -- TODO: implement EPConvergedOverride (for when the magnitude of the gradient is still large)

         -- TODO factor out--only unconstrainedRunning needs to run stepObjective, but EPconverged needs objfn
        where (vstate', gradEval, bfgs') = stepWithObjective config g params vstate
              objFnApplied = (overallObjFn params) g (r2f $ weight params)

-- Given the time, state, and evaluated gradient (or other search direction) at the point,
-- return the new state

stepT :: Float -> Float -> Float -> Float
stepT dt x dfdx = x - dt * dfdx

-- Calculates the new state by calculating the directional derivatives (via autodiff)
-- and timestep (via line search), then using them to step the current state.
-- Also partially applies the objective function.
stepWithObjective :: OptConfig -> StdGen -> Params -> [Float] -> ([Float], [Float], BfgsParams)
stepWithObjective config g params state =
          -- get timestep via line search, and evaluated gradient at the state
          let (t', gradEval, gradToUse, bfgs') = timeAndGrad config params objFnApplied state
              -- step each parameter of the state with the time and gradient
              state' = map (\(v, dfdv) -> stepT t' v dfdv) (zip state $ gradToUse)
              (fx, fx') = (objFnApplied state, objFnApplied state') 
          in -- if fx' > fx then error ("Error: new energy is greater than old energy: " ++ show (fx', fx)) else
             tro ("\nopt params: \n" ++ (show params)
                   ++ "\n||x' - x||: " ++ (show $ norm (state -. state'))
                   ++ "\n|f(x') - f(x)|: " ++
                  (show $ abs (fx' - fx))
                   ++ "\nf(x'): \n" ++ (show fx')
                   ++ "\ngradEval: \n" ++ (show gradEval)
                   ++ "\n||gradEval||: \n" ++ (show $ norm gradEval)
                   ++ "\ngradToUse: \n" ++ (show gradToUse)
                   ++ "\n||gradToUse||: \n" ++ (show $ norm gradToUse)
                   -- ++ "\nhessian: \n" ++ (show $ h)
                   ++ "\nbfgs': \n" ++ (show bfgs') -- TODO: use trb
                   ++ "\n timestep: \n" ++ (show t')
                   ++ "\n original state: \n" ++ (show state)
                   ++ "\n new state: \n" ++ (show state')
                  )
             (state', gradEval, bfgs')

          where objFnApplied :: ObjFn1 b
                objFnApplied = (overallObjFn params) g cWeight

                cWeight = r2f $ weight params
                -- realToFrac generalizes the type variable `a` to the type variable `b`, which timeAndGrad expects

-- a version of grad with a clearer type signature
appGrad :: (Autofloat a) => (forall b . (Autofloat b) => [b] -> b) -> [a] -> [a]
appGrad f l = grad f l

instance Show (Numeric.AD.Internal.On.On
                             (Numeric.AD.Internal.Reverse.Reverse
                                s (Numeric.AD.Internal.Sparse.Sparse a))) where
         show a = "error: not sure how to derive show for hessian element"

-- appHess :: (Num a) => (forall b . (Num b) => [b] -> b) -> [a] -> [[a]]
appHess :: (Autofloat a) => (forall b . (Autofloat b) => [b] -> b) -> [a] -> [[a]]
appHess f l = hessian f l

-- Precondition the gradient
gradP :: OptConfig -> BfgsParams -> [Double] -> ObjFn1 a -> [Float] -> ([Double], BfgsParams)
gradP config bfgsParams gradEval f state =
      let x_k = L.vector $ map r2f state
          grad_fx_k = L.vector gradEval
      in case optMethod config of 
      GradientDescent -> (gradEval, bfgsParams)

      Newton -> -- Precondition gradient with the pseudoinverse of the hessian
          let h = hessian f state
              h_list = (map r2f $ concat h) :: [Double]
              hinv = L.pinv $ L.matrix (length gradEval) $ h_list
              gradPreconditioned = hinv L.#> (L.vector gradEval) in
          (L.toList gradPreconditioned, bfgsParams)

      BFGS -> -- Approximate inverse of hessian with the change in gradient (see Nocedal S9.1 p224)
           let grad_val = lastGrad bfgsParams
               h_val = invH bfgsParams
               state_val = lastState bfgsParams
           in case (h_val, grad_val, state_val) of
              (Nothing, Nothing, Nothing) -> -- First step. Initialize the approximation to the identity (not clear how else to approximate it)
                    -- k=0 steps from x_0 to x_1: so on the first step, we take a normal gradient descent step
                    let h_0 = L.ident $ length gradEval
                    in (gradEval, bfgsParams { lastState = Just x_k, lastGrad = Just grad_fx_k, invH = Just h_0 })

              (Just h_km1, Just grad_fx_km1, Just x_km1) -> -- For x_{k+1}, to compute H_k, we need the (k-1) info
                    -- Our convention is that we are always working "at" k to compute k+1
                    -- x_0 doesn't require any H; x_1 (the first step) with k = 0 requires H_0
                    -- x_2 (the NEXT step) with k=1 requires H_1. For example>
                    -- x_2 = x_1 - alpha_1 H_1 grad f(x_1)   [GD step]
                    -- H_1 = V_0 H_0 V_0 + rho_0 s_0 s_0^T   [This is confusing because the book adds an extra +1 to the H index]
                    -- V_0 = I - rho_0 y_0 s_0^T
                    -- rho_0 = 1 / y_0^T s_0
                    -- s_0 = x_1 - x_0
                    -- y_0 = grad f(x_1) - grad f(x_0)

                    let s_km1 = x_k - x_km1
                        y_km1 = grad_fx_k - grad_fx_km1
                        rho_km1 = 1 / (y_km1 `L.dot` s_km1) -- Scalar
                        v_km1 = L.ident (length gradEval) - (rho_km1 `L.scale` y_km1 `L.outer` s_km1) -- Scaling can happen before outer
                        h_k = (L.tr (v_km1) L.<> h_km1 L.<> v_km1) + (rho_km1 `L.scale` s_km1 `L.outer` s_km1)
                        gradPreconditioned = h_k L.#> grad_fx_k
                    in (L.toList gradPreconditioned, 
                        bfgsParams { lastState = Just x_k, lastGrad = Just grad_fx_k, invH = Just h_k })

              _ -> error "invalid BFGS state"

      LBFGS -> -- Approximate the inverse of the Hessian times the gradient
               -- Only using the last `m` gradient/state difference vectors, not building the full h_k matrix (Nocedal p226)
            let grad_prev = lastGrad bfgsParams
                x_prev = lastState bfgsParams
                ss_val = s_list bfgsParams
                ys_val = y_list bfgsParams
                km1 = numUnconstrSteps bfgsParams -- Our current step is k; the last step is km1 (k_minus_1)
                m = memSize bfgsParams
            in case (grad_prev, x_prev, ss_val, ys_val, km1) of

               -- Perform normal gradient descent on first step
               (Nothing, Nothing, [], [], 0) ->
                   -- Store x_k, grad f(x_k) so we can compute s_k, y_k on next step
                   let bfgsParams' = bfgsParams { lastState = Just x_k, lastGrad = Just grad_fx_k, 
                                                  s_list = [], y_list = [], numUnconstrSteps = km1 + 1 } in
                   (gradEval, bfgsParams')

               (Just grad_fx_km1, Just x_km1, ss, ys, km1) -> 
                   -- Compute s_{k-1} = x_k - x_{k-1} and y_{k-1} = (analogous with grads)
                   -- Unlike Nocedal, compute the difference vectors first instead of last (same result, just a loop rewrite)
                   -- Use the updated {s_i} and {y_i}. (If k < m, this reduces to normal BFGS, i.e. we use all the vectors so far)
                   let (s_km1, y_km1) = (x_k - x_km1, grad_fx_k - grad_fx_km1) -- Newest vectors added to front
                       (ss', ys') = (take m $ s_km1 : ss, take m $ y_km1 : ys) -- The limited-memory part: drop stale vectors
                       gradPreconditioned = lbfgs grad_fx_k ss' ys'
                       bfgsParams' = bfgsParams { lastState = Just x_k, lastGrad = Just grad_fx_k,
                                                  s_list = ss', y_list = ys', numUnconstrSteps = km1 + 1 }
                   in (L.toList gradPreconditioned, bfgsParams')

               _ -> error "invalid L-BFGS state"

type LVector = L.Vector L.R
type LMatrix = L.Matrix L.R

-- See Nocedal p225
-- expects ys and ss to be in order from most recent to oldest (k-1 ... k-m)
-- expects length ys == length ss, length ys > 0, length ys > 0
lbfgs :: LVector -> [LVector] -> [LVector] -> LVector
lbfgs grad_fx_k ss ys =
    let rhos = map calculate_rho $ zip ss ys in -- The length of any list should be the number of stored vectors
    let q_k = grad_fx_k in
    let (q_k_minus_m, alphas) = foldl' pull_q_back (q_k, []) (zip3 rhos ss ys) in -- forward: for i = k-1 ... k-m
    -- Note the order of alphas will be from k-m through k-1 for the push_r_forward loop
    let h_0_k = estimate_hess (head ys) (head ss) in
    let r_k_minus_m = h_0_k L.#> q_k_minus_m in
    let r_k = foldl' push_r_forward r_k_minus_m (zip (zip rhos alphas) (zip ss ys)) in -- backward: for i = k-m .. k-1
    r_k -- is H_k * grad f(x_k)

           where calculate_rho :: (LVector, LVector) -> L.R
                 calculate_rho (s, y) = 1 / ((y `L.dot` s) + epsd)

                 pull_q_back :: (LVector, [L.R]) -> (L.R, LVector, LVector) -> (LVector, [L.R]) -- from i+1 to i
                 pull_q_back (q_i_plus_1, alphas) (rho_i, s_i, y_i) =
                        let alpha_i = rho_i * (s_i `L.dot` q_i_plus_1) in -- scalar
                        let q_i = q_i_plus_1 - (alpha_i `L.scale` y_i) in -- scalar * vector
                        (q_i, alpha_i : alphas) -- Note the order of alphas

                 -- Scale I by an estimate of the size of the Hessian along the most recent search direction (Nocedal p226)
                 estimate_hess :: LVector -> LVector -> LMatrix
                 estimate_hess y_km1 s_km1 = 
                        let gamma_k = (s_km1 `L.dot` y_km1) / ((y_km1 `L.dot` y_km1) + epsd) in
                        gamma_k `L.scale` (L.ident (L.size y_km1))

                 push_r_forward :: LVector -> ((L.R, L.R), (LVector, LVector)) -> LVector -- from i to i+1
                 push_r_forward r_i ((rho_i, alpha_i), (s_i, y_i)) =
                        let beta_i = rho_i * (y_i `L.dot` r_i) in -- scalar
                        let r_i_plus_1 = r_i + (alpha_i - beta_i) `L.scale` s_i -- scalar * vector
                        in r_i_plus_1

estimateGradient :: ObjFn1 a -> [Float] -> [Float]
estimateGradient f state =
      let len = length state in
      let h = 0.001 in -- Choice of h really matters!!! This is not an accurate estimate.
      let fx = f state in
      -- time is O(|state|^2)
      let dfx i = ((f $ replace i ((state !! i) + h) state) - fx) / h in
      let dfxs = map dfx [0..(len-1)] in
      dfxs
      where replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

-- Given the objective function, gradient function, timestep, and current state,
-- return the timestep (found via line search) and evaluated gradient at the current state.
-- the autodiff library requires that objective functions be polymorphic with Floating a
timeAndGrad :: OptConfig -> Params -> ObjFn1 a -> [Float] -> (Float, [Float], [Float], BfgsParams)
timeAndGrad config params f state = 
            let gradEval = if useAutodiff then gradF state else estimateGradient f state
                (gradToUse_d, bfgs') = gradP config (bfgsInfo params) (map r2f gradEval :: [Double]) f state
                gradToUse = map r2f gradToUse_d
                -- Use line search to find a good timestep. If we use Newton's method, the descent direction uses the preconditioned gradient.
                descentDir = negL $ gradToUse
                timestep = let resT = if useLineSearch && useAutodiff
                                      then awLineSearch config f (duf descentDir) descentDir state
                                      else constT in -- hardcoded timestep
                           if isNaN resT then error "returned timestep is NaN" else resT
            
            in tr "timeAndGrad: " (timestep, gradEval, gradToUse, bfgs')

            where gradF :: GradFn a
                  gradF = appGrad f

                  -- directional derivative at u, where u is the negated gradient in awLineSearch
                  -- descent direction need not have unit norm
                  -- we could also use a different descent direction if desired
                  -- Note: with Newton's method, we find the directional derivative using the preconditioned gradient
                  duf :: (Autofloat a) => [a] -> [a] -> a
                  duf u x = u `dotL` gradF x

linesearch_max :: Int
linesearch_max = 100 -- TODO what's a reasonable limit (if any)?

-- Implements Armijo-Wolfe line search as specified in Keenan's notes, converges on nonconvex fns as well
-- based off Lewis & Overton, "Nonsmooth optimization via quasi-Newton methods
-- duf = D_u(f), the directional derivative of f at descent direction u
-- D_u(x) = <gradF(x), u>. If u = -gradF(x) (as it is here), then D_u(x) = -||gradF(x)||^2

awLineSearch :: OptConfig -> ObjFn1 a -> ObjFn1 a -> [Float] -> [Float] -> Float
awLineSearch config f duf descentDir x0 =

     let (a0, b0, t0) = (0, infinity, 1)

         -- drop while a&w are not satisfied OR the interval is large enough
         (numUpdates, (af, bf, tf)) = head $ dropWhile intervalOK_or_notArmijoAndWolfe
                                      $ zip [0..] $ iterate update (a0, b0, t0)

     in tro ("Line search # updates: " ++ show numUpdates) $ tf

          where update :: (Float, Float, Float) -> (Float, Float, Float)
                update (a, b, t) =
                       let (a', b', sat) | not $ armijo t    = tr' "not armijo" (a, t, False)
                                         | not $ wolfe t     = tr' "not wolfe" (t, b, False)
                                         | otherwise         = (a, b, True) in
                       if sat then (a, b, t) -- if armijo and wolfe, then we use (a, b, t) as-is
                       else if b' < infinity then tr' "b' < infinity" (a', b', (a' + b') / 2)
                       else tr' "b' = infinity" (a', b', 2 * a')

                intervalOK_or_notArmijoAndWolfe :: (Int, (Float, Float, Float)) -> Bool
                intervalOK_or_notArmijoAndWolfe (numUpdates, (a, b, t)) =
                      not $
                      if armijo t && wolfe t then
                           tr ("stop: both sat. |descentDir at x0| = " ++ show (norm descentDir)) True
                      else if abs (b - a) < minInterval then
                           tr ("stop: interval too small. |descentDir at x0| = " ++ show (norm descentDir)) True
                      else if numUpdates > linesearch_max then
                           tr ("stop: number of line search updates exceeded max") True
                      else False

                armijo :: Float -> Bool
                armijo t = (f (x0 +. t *. descentDir)) <= (fAtx0 + c1 * t * dufAtx0)

                strongWolfe :: Float -> Bool
                strongWolfe t = (abs (duf (x0 +. t *. descentDir))) <= (c2 * abs dufAtx0)

                weakWolfe :: Float -> Bool
                weakWolfe t = (duf (x0 +. t *. descentDir)) >= (c2 * dufAtx0)

                dufAtx0 :: Float
                dufAtx0 = duf x0

                fAtx0 :: Float
                fAtx0 = f x0

                minInterval :: Float -- stop if the interval gets too small; might not terminate
                minInterval = if intervalMin then 10 ** (-20) else 0
                -- TODO: the line search is very sensitive to this parameter. Blows up with 10**(-5). Why?

                wolfe :: Float -> Bool
                wolfe = weakWolfe
