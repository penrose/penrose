{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-# OPTIONS_HADDOCK prune #-}

module Penrose.Optimizer
  ( step
  ) where

import           Data.List                   (foldl')
import           Debug.Trace
import           Numeric.AD
import           Numeric.AD.Internal.On
import           Numeric.AD.Internal.Reverse
import           Numeric.AD.Internal.Sparse
import qualified Numeric.LinearAlgebra       as L
import           Penrose.GenOptProblem
import           Penrose.Style
import           Penrose.Util
import           System.Console.ANSI
import           System.Random

default (Int, Double)

------ Opt types, util functions, and params
type ObjFn1 a
   = forall a. (Autofloat a) =>
                 [a] -> a

-- used for duf
type ObjFn2 a
   = forall a. (Autofloat a) =>
                 [a] -> [a] -> a

type GradFn a
   = forall a. (Autofloat a) =>
                 [a] -> [a]

----- Various consts
nanSub :: (Autofloat a) => a
nanSub = 0

infinity :: Floating a => a
infinity = 1 / 0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)

-- all numbers are smaller than infinity except infinity, to which it's equal
----- Hyperparameters
weightGrowthFactor :: (Autofloat a) => a -- for EP weight
weightGrowthFactor = 10

epsUnconstr :: Floating a => a
epsUnconstr = 10 ** (-2)

epStop :: Floating a => a -- for EP diff
epStop = 10 ** (-3)

-- epStop = 10 ** (-5)
-- epStop = 60 ** (-3)
-- epStop = 10 ** (-1)
-- epStop = 0.05
-- Parameters for Armijo-Wolfe line search
-- NOTE: must maintain 0 < c1 < c2 < 1
c1 :: Floating a => a
c1 = 0.001 -- for Armijo, corresponds to alpha in backtracking line search (see below for explanation)

-- smaller c1 = shallower slope = less of a decrease in fn value needed = easier to satisfy
-- turn Armijo off: c1 = 0
-- Nocedal p38: "In practice, c1 is chosen to be quite small, say 10−4."
c2 :: Floating a => a
c2 = 0.9 -- for Wolfe, is the factor decrease needed in derivative value

-- new directional derivative value / old DD value <= c2
-- smaller c2 = smaller new derivative value = harder to satisfy
-- turn Wolfe off: c1 = 1 (basically backatracking line search only)
-- Nocedal p39: "Typical values of c2 are 0.9 when the search direction pk is chosen by a Newton or quasi-Newton method"
-- true = force linesearch halt if interval gets too small; false = no forced halt
intervalMin = True

useLineSearch :: Bool
useLineSearch = True

useAutodiff :: Bool
useAutodiff = True

constT :: Floating a => a
constT = 0.001

debugOpt = False

-- debugOpt = True
debugLineSearch = False

debugBfgs = False

trb :: String -> a -> a
trb s x =
  if debugBfgs
    then trace "---" $ trace s x
    else x -- prints in left to right order

tro :: String -> a -> a
tro s x =
  if debugOpt
    then trace "---" $ trace s x
    else x -- prints in left to right order

trl :: Show a => String -> a -> a
trl s x =
  if debugLineSearch
    then trace "---" $ trace s $ traceShowId x
    else x -- prints in left to right order

----- Convergence criteria
-- convergence criterion for EP
-- if you want to use it for UO, needs a different epsilon
epStopCond :: (Autofloat a) => [a] -> [a] -> a -> a -> Bool
epStopCond x x' fx fx' =
  tro
    ("EP: \n||x' - x||: " ++
     (show $ norm (x -. x')) ++ "\n|f(x') - f(x)|: " ++ (show $ abs (fx - fx'))) $
  (norm (x -. x') <= epStop) || (abs (fx - fx') <= epStop)

unconstrainedStopCond :: (Autofloat a) => [a] -> Bool
unconstrainedStopCond gradEval = norm gradEval < epsUnconstr

---------------------------------------
-- Policies
-- stepPolicy :: State -> (Params, PolicyParams)
-- stepPolicy s =
--     -- Check overall convergence first
--     let epStatus = optStatus $ (paramsr s) in
--     let pparams = policyParams s in
--     case epStatus of
--     -- Generate new objective function and replace the optimization and policy params accordingly
--     EPConverged ->
--                 -- TODO: clean up the step incrementing
--         let pparams' = pparams { policySteps = 1 + policySteps pparams } in
--         let (policyRes, psNew) = (policyFn s) (objFns s) (constrFns s) pparams' in -- See what the policy function wants
--         case policyRes of
--             Nothing     -> (paramsr s, pparams' { policyState = psNew }) -- steps incremented, policy done
--             Just newFns -> -- Policy keeps going
--                 let objFnNew = genObjfn (castTranslation $ transr s) (filter isObjFn newFns) (filter isConstr newFns) (varyingPaths s)
--                     -- TODO: check that these inputs are right
--                     -- Change obj function and restart optimization
--                     pparamsNew = pparams' { policyState = psNew,
--                                             currFns = newFns }
--                     paramsNew = Params { weight = initWeight,
--                                          optStatus = NewIter,
--                                          overallObjFn = objFnNew,
--                                          bfgsInfo = defaultBfgsParams }
--                 in tro ("Step policy, EP converged, new params:\n" ++ show (paramsNew, pparamsNew, newFns)) $ (paramsNew, pparamsNew)
--     -- If not converged, optimize as usual, don't change policy mid-optimization
--     _ -> tro ("Step policy, EP not converged, new params:\n" ++ show (paramsr s, pparams)) $ (paramsr s, pparams)
---------------------------------------
-- Main optimization functions
step :: State -> State
step s =
  let (state', params') = stepShapes s
      s' = s {varyingState = state', paramsr = params'}
             -- NOTE: we intentionally discard the random generator here because
             -- we want to have consistent computation output in a single
             -- optimization session
             -- For the same reason, all subsequent step* functions such as
             -- stepShapes do not return the new random generator
    --   (!shapes', trans, _) = evalTranslation s'
             -- Check the state and see if the overall objective function should be changed
             -- The policy may change EPConverged to a new iteration before the frontend sees it
            --  (paramsNew, pparamsNew) = stepPolicy s'
             -- For debugging
      oldParams = paramsr s
  in s' {paramsr = params'}
                -- shapesr = shapes',
                --  paramsr = paramsNew,
                --  policyParams = pparamsNew }
            -- NOTE: trans is not updated in state

-- (oConfig s) (paramsr s) (varyingState s) (rng s)
-- Note use of realToFrac to generalize type variables (on the weight and on the varying state)
-- implements exterior point algo as described on page 6 here:
-- https://www.me.utexas.edu/~jensen/ORMM/supplements/units/nlp_methods/const_opt.pdf
stepShapes :: State -> ([Double], Params)
stepShapes s -- varying state
         -- if null vstate then error "empty state in stepshapes" else
 =
  let (epWeight, epStatus) = (weight params, optStatus params)
  in case epStatus
         -- start the outer EP optimization and the inner unconstrained optimization, recording initial EPstate
           of
       NewIter ->
         let status' = UnconstrainedRunning (map realToFrac vstate)
         in ( vstate'
            , params
              { weight = initWeight
              , optStatus = status'
              , bfgsInfo = defaultBfgsParams
              })
         -- check *weak* convergence of inner unconstrained opt.
         -- if UO converged, set opt state to converged and update UO state (NOT EP state)
         -- if not, keep running UO (inner state implicitly stored)
         -- note convergence checks are only on the varying part of the state
       UnconstrainedRunning lastEPstate -- doesn't use last EP state
           -- let unconstrConverged = unconstrainedStopCond gradEval in
        ->
         let unconstrConverged =
               epStopCond
                 vstate
                 vstate'
                 (objFnApplied vstate)
                 (objFnApplied vstate')
               -- Two stopping conditions
               -- unconstrainedStopCond gradEval in
         in if unconstrConverged
              then let status' = UnconstrainedConverged lastEPstate -- update UO state only! (note this does NOT change the EP state to be vstate', which happens on the branch for UnconstrainedConverged, which is next)
                   in ( vstate'
                      , params
                        { optStatus = status'
                        , bfgsInfo = defaultBfgsParams -- note vstate' (UO converged), not vstate
                        })
              else (vstate', params {bfgsInfo = bfgs'}) -- update UO state but not EP state; UO still running
         -- check EP convergence. if converged then stop, else increase weight, update states, and run UO again
         -- TODO some trickiness about whether unconstrained-converged has updated the correct state
         -- and whether to check WRT the updated state or not
       UnconstrainedConverged lastEPstate ->
         let epConverged =
               epStopCond
                 lastEPstate
                 (map r2f vstate) -- lastEPstate is last state for converged UO
                 (objFnApplied lastEPstate)
                 (objFnApplied (map r2f vstate))
         in if epConverged
              then let status' = EPConverged -- no more EP state
                   in ( vstate
                      , params
                        { optStatus = status'
                        , bfgsInfo = defaultBfgsParams -- do not update UO state
                        })
           -- update EP state: to be the converged state from the most recent UO
              else let status' = UnconstrainedRunning (map realToFrac vstate) -- increase weight
                   in let epWeight' = weightGrowthFactor * epWeight
                -- trace ("Unconstrained converged. New weight: " ++ show epWeight') $
                      in ( vstate
                         , params
                           { weight = epWeight'
                           , optStatus = status'
                           , bfgsInfo = defaultBfgsParams
                           })
         -- done; don't update obj state or params; user can now manipulate
       EPConverged -> (vstate, params {bfgsInfo = defaultBfgsParams})
         -- TODO: implement EPConvergedOverride (for when the magnitude of the gradient is still large)
         -- TODO factor out--only unconstrainedRunning needs to run stepObjective, but EPconverged needs objfn
  where
    (config, params, vstate, g) = (oConfig s, paramsr s, varyingState s, rng s)
    (vstate', gradEval, bfgs') = stepWithObjective s
            --   objFnApplied = (overallObjFn params) g (r2f $ weight params)
    objFnApplied = evalEnergyOn s

-- Given the time, state, and evaluated gradient (or other search direction) at the point,
-- return the new state
stepT :: Double -> Double -> Double -> Double
stepT dt x dfdx = x - dt * dfdx

-- Calculates the new state by calculating the directional derivatives (via autodiff)
-- and timestep (via line search), then using them to step the current state.
-- Also partially applies the objective function.
stepWithObjective :: State -> ([Double], [Double], BfgsParams)
stepWithObjective s
          -- get timestep via line search, and evaluated gradient at the state
 =
  let (t', gradEval, gradToUse, bfgs') =
        timeAndGrad config params objFnApplied state
              -- step each parameter of the state with the time and gradient
      state' = map (\(v, dfdv) -> stepT t' v dfdv) (zip state $ gradToUse)
      (fx, fx') = (objFnApplied state, objFnApplied state')
             -- if fx' > fx then error ("Error: new energy is greater than old energy: " ++ show (fx', fx)) else
            --  tro ("\n----------------------------------------\n"
            --        ++ "\nopt params: \n" ++ (show params)
            --        ++ "\n||x' - x||: " ++ (show $ norm (state -. state'))
            --        ++ "\n|f(x') - f(x)|: " ++
            --       (show $ abs (fx' - fx))
            --        ++ "\nf(x'): \n" ++ (show fx')
            --        ++ "\ngradEval: \n" ++ (show gradEval)
            --        ++ "\n||gradEval||: \n" ++ (show $ norm gradEval)
            --        ++ "\ngradToUse: \n" ++ (show gradToUse)
            --        ++ "\n||gradToUse||: \n" ++ (show $ norm gradToUse)
            --        -- ++ "\nhessian: \n" ++ (show $ h)
            --        ++ "\nbfgs': \n" ++ (show bfgs') -- TODO: use trb
            --        ++ "\n timestep: \n" ++ (show t')
            --        ++ "\n original state: \n" ++ (show state)
            --        ++ "\n new state: \n" ++ (show state')
            --       )
  in (state', gradEval, bfgs')
  where
    (config, params, state, g) = (oConfig s, paramsr s, varyingState s, rng s)
                -- objFnApplied :: ObjFn1 b
                -- objFnApplied = (overallObjFn params) g cWeight
    objFnApplied = evalEnergyOn s
    cWeight = r2f $ weight params
                -- realToFrac generalizes the type variable `a` to the type variable `b`, which timeAndGrad expects

-- a version of grad with a clearer type signature
appGrad ::
     (Autofloat a)
  => (forall b. (Autofloat b) =>
                  [b] -> b)
  -> [a]
  -> [a]
appGrad f l = grad f l

instance Show (Numeric.AD.Internal.On.On (Numeric.AD.Internal.Reverse.Reverse s (Numeric.AD.Internal.Sparse.Sparse a))) where
  show a = "error: not sure how to derive show for hessian element"

appHess ::
     (Autofloat a)
  => (forall b. (Autofloat b) =>
                  [b] -> b)
  -> [a]
  -> [[a]]
appHess f l = hessian f l

-- Precondition the gradient
gradP ::
     OptConfig
  -> BfgsParams
  -> [Double]
  -> ObjFn1 a
  -> [Double]
  -> ([Double], BfgsParams)
gradP config bfgsParams gradEval f state =
  let x_k = L.vector $ map r2f state
      grad_fx_k = L.vector gradEval
  in case optMethod config of
       GradientDescent -> (gradEval, bfgsParams)
       Newton -- Precondition gradient with the pseudoinverse of the hessian
        ->
         let h = appHess f state
             h_list = (map r2f $ concat h) :: [Double]
             hinv = L.pinv $ L.matrix (length gradEval) $ h_list
             gradPreconditioned = hinv L.#> (L.vector gradEval)
         in (L.toList gradPreconditioned, bfgsParams)
       BFGS -- Approximate inverse of hessian with the change in gradient (see Nocedal S9.1 p224)
        ->
         let grad_val = lastGrad bfgsParams :: Maybe [Double]
             h_val = invH bfgsParams :: Maybe [[Double]]
             state_val = lastState bfgsParams :: Maybe [Double]
         in case (h_val, grad_val, state_val) of
              (Nothing, Nothing, Nothing) -- First step. Initialize the approximation to the identity (not clear how else to approximate it)
                    -- k=0 steps from x_0 to x_1: so on the first step, we take a normal gradient descent step
               ->
                let h_0 = L.ident $ length gradEval
                in ( gradEval
                   , bfgsParams
                     { lastState = Just $ L.toList x_k
                     , lastGrad = Just $ L.toList grad_fx_k
                     , invH = Just $ L.toLists h_0
                     })
              (Just h_km1L, Just grad_fx_km1L, Just x_km1L) -- For x_{k+1}, to compute H_k, we need the (k-1) info
                    -- Our convention is that we are always working "at" k to compute k+1
                    -- x_0 doesn't require any H; x_1 (the first step) with k = 0 requires H_0
                    -- x_2 (the NEXT step) with k=1 requires H_1. For example>
                    -- x_2 = x_1 - alpha_1 H_1 grad f(x_1)   [GD step]
                    -- H_1 = V_0 H_0 V_0 + rho_0 s_0 s_0^T   [This is confusing because the book adds an extra +1 to the H index]
                    -- V_0 = I - rho_0 y_0 s_0^T
                    -- rho_0 = 1 / y_0^T s_0
                    -- s_0 = x_1 - x_0
                    -- y_0 = grad f(x_1) - grad f(x_0)
               ->
                let (h_km1, grad_fx_km1, x_km1) =
                      ( L.fromLists h_km1L
                      , L.vector grad_fx_km1L
                      , L.vector x_km1L)
                in let s_km1 = x_k - x_km1
                       y_km1 = grad_fx_k - grad_fx_km1
                       rho_km1 = 1 / (y_km1 `L.dot` s_km1) -- Scalar
                       v_km1 =
                         L.ident (length gradEval) -
                         (rho_km1 `L.scale` y_km1 `L.outer` s_km1) -- Scaling can happen before outer
                       h_k =
                         (L.tr (v_km1) L.<> h_km1 L.<> v_km1) +
                         (rho_km1 `L.scale` s_km1 `L.outer` s_km1)
                       gradPreconditioned = h_k L.#> grad_fx_k
                   in ( L.toList gradPreconditioned
                      , bfgsParams
                        { lastState = Just $ L.toList x_k
                        , lastGrad = Just $ L.toList grad_fx_k
                        , invH = Just $ L.toLists h_k
                        })
              _ -> error "invalid BFGS state"
       LBFGS -- Approximate the inverse of the Hessian times the gradient
               -- Only using the last `m` gradient/state difference vectors, not building the full h_k matrix (Nocedal p226)
        ->
         let grad_prev = lastGrad bfgsParams
             x_prev = lastState bfgsParams
             ss_val = s_list bfgsParams
             ys_val = y_list bfgsParams
             km1 = numUnconstrSteps bfgsParams -- Our current step is k; the last step is km1 (k_minus_1)
             m = memSize bfgsParams
         in case (grad_prev, x_prev, ss_val, ys_val, km1)
               -- Perform normal gradient descent on first step
                  of
              (Nothing, Nothing, [], [], 0)
                   -- Store x_k, grad f(x_k) so we can compute s_k, y_k on next step
               ->
                let bfgsParams' =
                      bfgsParams
                      { lastState = Just $ L.toList x_k
                      , lastGrad = Just $ L.toList grad_fx_k
                      , s_list = []
                      , y_list = []
                      , numUnconstrSteps = km1 + 1
                      }
                in (gradEval, bfgsParams')
              (Just grad_fx_km1L, Just x_km1L, ssL, ysL, km1) ->
                let (grad_fx_km1, x_km1, ss, ys) =
                      ( L.fromList grad_fx_km1L
                      , L.fromList x_km1L
                      , map L.fromList ssL
                      , map L.fromList ysL)
                   -- Compute s_{k-1} = x_k - x_{k-1} and y_{k-1} = (analogous with grads)
                   -- Unlike Nocedal, compute the difference vectors first instead of last (same result, just a loop rewrite)
                   -- Use the updated {s_i} and {y_i}. (If k < m, this reduces to normal BFGS, i.e. we use all the vectors so far)
                in let (s_km1, y_km1) = (x_k - x_km1, grad_fx_k - grad_fx_km1) -- Newest vectors added to front
                       (ss', ys') = (take m $ s_km1 : ss, take m $ y_km1 : ys) -- The limited-memory part: drop stale vectors
                       gradPreconditioned = lbfgs grad_fx_k ss' ys'
                       descentDirCheck = -gradPreconditioned `L.dot` grad_fx_k
                   -- Reset L-BFGS if the result is not a descent direction, and use steepest descent direction
                   -- https://github.com/JuliaNLSolvers/Optim.jl/issues/143 https://github.com/JuliaNLSolvers/Optim.jl/pull/144
                   in if descentDirCheck < epsd
                        then ( L.toList gradPreconditioned
                             , bfgsParams
                               { lastState = Just $ L.toList x_k
                               , lastGrad = Just $ L.toList grad_fx_k
                               , s_list = map L.toList ss'
                               , y_list = map L.toList ys'
                               , numUnconstrSteps = km1 + 1
                               })
                        --   tro "L-BFGS did not find a descent direction. Resetting correction vectors." $
                        else ( gradEval
                             , bfgsParams
                               { lastState = Just $ L.toList x_k
                               , lastGrad = Just $ L.toList grad_fx_k
                               , s_list = []
                               , y_list = []
                               , numUnconstrSteps = km1 + 1
                               })
                   -- TODO: check the curvature condition y_k^T s_k > 0 (8.7) (Nocedal 201)
                   -- https://github.com/JuliaNLSolvers/Optim.jl/issues/26
              _ -> error "invalid L-BFGS state"

type LVector = L.Vector L.R

type LMatrix = L.Matrix L.R

-- See Nocedal p225
-- expects ys and ss to be in order from most recent to oldest (k-1 ... k-m)
-- expects length ys == length ss, length ys > 0, length ys > 0
lbfgs :: LVector -> [LVector] -> [LVector] -> LVector
lbfgs grad_fx_k ss ys =
  let rhos = map calculate_rho $ zip ss ys -- The length of any list should be the number of stored vectors
      q_k = grad_fx_k
      (q_k_minus_m, alphas) = foldl' pull_q_back (q_k, []) (zip3 rhos ss ys) -- backward: for i = k-1 ... k-m
        -- Note the order of alphas will be from k-m through k-1 for the push_r_forward loop
      h_0_k = estimate_hess (head ys) (head ss)
      r_k_minus_m = h_0_k L.#> q_k_minus_m
      r_k =
        foldl'
          push_r_forward
          r_k_minus_m
          (zip (zip (reverse rhos) alphas) (zip (reverse ss) (reverse ys)))
        -- forward: for i = k-m .. k-1 (TODO: optimize out the reverses)
  in r_k -- is H_k * grad f(x_k)
  where
    calculate_rho :: (LVector, LVector) -> L.R
    calculate_rho (s, y) = 1 / ((y `L.dot` s) + epsd)
    pull_q_back ::
         (LVector, [L.R])
      -> (L.R, LVector, LVector)
      -> (LVector, [L.R])
    pull_q_back (q_i_plus_1, alphas) (rho_i, s_i, y_i) =
      let alpha_i = rho_i * (s_i `L.dot` q_i_plus_1) -- scalar
          q_i = q_i_plus_1 - (alpha_i `L.scale` y_i) -- scalar * vector
      in (q_i, alpha_i : alphas) -- Note the order of alphas
                 -- Scale I by an estimate of the size of the Hessian along the most recent search direction (Nocedal p226)
    estimate_hess :: LVector -> LVector -> LMatrix
    estimate_hess y_km1 s_km1 =
      let gamma_k = (s_km1 `L.dot` y_km1) / ((y_km1 `L.dot` y_km1) + epsd)
      in gamma_k `L.scale` (L.ident (L.size y_km1))
    push_r_forward :: LVector -> ((L.R, L.R), (LVector, LVector)) -> LVector -- from i to i+1
    push_r_forward r_i ((rho_i, alpha_i), (s_i, y_i)) =
      let beta_i = rho_i * (y_i `L.dot` r_i) -- scalar
          r_i_plus_1 = r_i + (alpha_i - beta_i) `L.scale` s_i -- scalar * vector
      in r_i_plus_1

estimateGradient :: ObjFn1 a -> [Double] -> [Double]
estimateGradient f state =
  let len = length state
      h = 0.001 -- Choice of h really matters!!! This is not an accurate estimate.
      fx = f state
      -- time is O(|state|^2)
      dfx i = ((f $ replace i ((state !! i) + h) state) - fx) / h
      dfxs = map dfx [0 .. (len - 1)]
  in dfxs
  where
    replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

-- Given the objective function, gradient function, timestep, and current state,
-- return the timestep (found via line search) and evaluated gradient at the current state.
-- the autodiff library requires that objective functions be polymorphic with Floating a
timeAndGrad ::
     OptConfig
  -> Params
  -> ObjFn1 a
  -> [Double]
  -> (Double, [Double], [Double], BfgsParams)
timeAndGrad config params f state =
  let gradEval =
        if useAutodiff
          then gradF state
          else estimateGradient f state
      (gradToUse_d, bfgs') =
        gradP config (bfgsInfo params) (map r2f gradEval :: [Double]) f state
      gradToUse = map r2f gradToUse_d
                -- Use line search to find a good timestep. If we use Newton's method, the descent direction uses the preconditioned gradient.
      descentDir = negL $ gradToUse
      timestep =
        let resT =
              if useLineSearch && useAutodiff
                then awLineSearch config f (duf descentDir) descentDir state
                else constT -- hardcoded timestep
        in if isNaN resT
             then error "returned timestep is NaN"
             else resT
  in tr "timeAndGrad: " (timestep, gradEval, gradToUse, bfgs')
  where
    gradF :: GradFn a
    gradF = appGrad f
                  -- directional derivative of f at x in the direction of u (descent direction, which may not have unit norm)
    duf :: (Autofloat a) => [a] -> [a] -> a
    duf u x = u `dotL` gradF x

linesearch_max :: Int
linesearch_max = 100 -- TODO what's a reasonable limit (if any)?

-- Implements Armijo-Wolfe line search as specified in Keenan's notes, converges on nonconvex fns as well
-- based off Lewis & Overton, "Nonsmooth optimization via quasi-Newton methods
-- duf = D_u(f), the directional derivative of f at descent direction u
-- D_u(x) = <gradF(x), u>. If u = -gradF(x) (as it is here), then D_u(x) = -||gradF(x)||^2
awLineSearch ::
     OptConfig -> ObjFn1 a -> ObjFn1 a -> [Double] -> [Double] -> Double
awLineSearch config f duf descentDir x0 =
  let (a0, b0, t0) = (0, infinity, 1) -- Unit step length should be tried first for quasi-Newton methods. (Nocedal 201)
         -- drop while a&w are not satisfied OR the interval is large enough
      (numUpdates, (af, bf, tf)) =
        head $
        dropWhile intervalOK_or_notArmijoAndWolfe $
        zip [0 ..] $ iterate update (a0, b0, t0)
        --  tro ("Line search # updates: " ++ show numUpdates) $
  in tf
  where
    update :: (Double, Double, Double) -> (Double, Double, Double)
    update (a, b, t) =
      let (a', b', sat)
            | not $ armijo t = trl "not armijo" (a, t, False)
            | not $ wolfe t = trl "not wolfe" (t, b, False)
            | otherwise = (a, b, True)
      in if sat
           then (a, b, t) -- if armijo and wolfe, then we use (a, b, t) as-is
           else if b' < infinity
                  then trl "b' < infinity" (a', b', (a' + b') / 2)
                  else trl "b' = infinity" (a', b', 2 * a')
    intervalOK_or_notArmijoAndWolfe :: (Int, (Double, Double, Double)) -> Bool
    intervalOK_or_notArmijoAndWolfe (numUpdates, (a, b, t)) =
      not $
      if armijo t && wolfe t
        then trl
               ("stop: both sat. |descentDir at x0| = " ++
                show (norm descentDir))
               True
        else if abs (b - a) < minInterval
               then trl
                      ("stop: interval too small. |descentDir at x0| = " ++
                       show (norm descentDir))
                      True
               else if numUpdates > linesearch_max
                      then trl
                             ("stop: number of line search updates exceeded max")
                             True
                      else False
    armijo :: Double -> Bool
    armijo t = (f (x0 +. t *. descentDir)) <= (fAtx0 + c1 * t * dufAtx0)
    weakWolfe :: Double -> Bool -- Better for nonsmooth functions
    weakWolfe t = (duf (x0 +. t *. descentDir)) >= (c2 * dufAtx0)
    strongWolfe :: Double -> Bool
    strongWolfe t = (abs (duf (x0 +. t *. descentDir))) <= (c2 * abs dufAtx0)
                -- Descent direction (at x0) must have a negative dot product with the gradient of f at x0.
    dufAtx0 :: Double -- TODO: this is redundant, cache it
    dufAtx0 =
      let res = duf x0
      in trl ("<grad f(x0), descent direction at x0>: " ++ show res) $
         if res > 0
           then tro "WARNING: descent direction doesn't satisfy condition" res
           else res
    fAtx0 :: Double
    fAtx0 = f x0
    minInterval :: Double -- stop if the interval gets too small; might not terminate
    minInterval =
      if intervalMin
        then 10 ** (-10)
        else 0
                -- TODO: the line search is very sensitive to this parameter. Blows up with 10**(-5). Why?
    wolfe :: Double -> Bool
    wolfe = weakWolfe
