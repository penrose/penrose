type Step -- a step in a walk
type Sample

-- x1 := sampleBoundary(x0) takes a step in the walk by sampling the boundary
constructor sampleBoundary( Step x0 ) -> Step x1

-- y1 := sampleInterior(x0) takes a step in the walk by sampling the interior
constructor sampleInterior( Step x0 ) -> Step y1

-- y1 := sampleSource(x0) samples a source in the interior, but does not start a new walk
constructor sampleSource( Step x0 ) -> Sample y1

-- isNested(x) asserts that step x belongs to a nested walk,
-- e.g., a walk that begins at a source sample point and then
-- continues all the way to the boundary
predicate isNested( Step x )
