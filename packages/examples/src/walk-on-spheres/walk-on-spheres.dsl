type Step -- a step in a walk
type Sample -- a sample point along the walk

-- Next(a,b) asserts that b follows a in a walk
predicate Next( Step a, Step b )

-- isNested(x) asserts that step x belongs to a nested walk,
-- e.g., a walk that begins at a source sample point and then
-- continues all the way to the boundary
predicate isNested( Step x )

-- y := sourceSample(x) asserts that y is a sample
-- of the source term for step x
constructor sourceSample( Step x ) -> Sample
