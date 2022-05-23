type Step -- a step in a walk
type Sample -- a sample point used somewhere along the walk

-- Next(a,b) asserts that b follows a in a walk
predicate Next( Step a, Step b )

-- y := sourceSample(x) asserts that y is a sample
-- of the source term for step x
constructor sourceSample( Step x ) -> Sample
