# 16 vertices

Over 1 trial, appears to be ~2x faster to take one step with pruned, but when you include the time-to-prune, it's slower overall...

## Baseline
Time taken for 1 call to 'minimize' with 1 steps: 13.685000012628734 ms
Optimizer.ts:466 Performance: times (ms for each 1 call to minimize) over 10 samples
Optimizer.ts:467 Mean: 17.82299999613315 ms | Median: 14.0174999833107 ms

## With pruning, without measuring the time to prune
Time taken for 1 call to 'minimize' with 1 steps: 3.710000077262521 ms
Optimizer.ts:464 Performance: times (ms for each 1 call to minimize) over 10 samples
Mean: 7.7025000005960464 ms | Median: 8.2724999519

Time to prune for 33 shapes and 256 objectives/constraints: ~17-18 ms

## 18 shapes

Slightly faster with pruned, excluding the prune time. Slightly slower if we include the prune time.

## Baseline
Time taken for 1 call to 'minimize' with 1 steps: 11.100000003352761 ms
Optimizer.ts:469 Performance: times (ms for each 1 call to minimize) over 26 samples
Mean: 26.155769229472543 ms | Median: 11.91499998094514 ms

## Minimize + prune
Time to prune for 39 shapes and 361 objectives/constraints: 13.429999933578074 ms
    (This is usually 5-15 ms)

Optimizer.ts:1041 Time taken for 1 call to 'minimize' with 1 steps: 1.71000009868294 ms
Optimizer.ts:468 Performance: times (ms for each 1 call to minimize) over 20 samples
Optimizer.ts:469 Mean: 9.810500015737489 ms | Median: 9.880000026896596 ms

## TODO

- Store averages of the prune time and the baseline minimize for comparison
- Scale up # objects to see if we hit a win - run headlessly w/ automator?
- Optimize the prune routine - see what's slow about it (probably the functinon iteration)
  - Do more correctness checking
