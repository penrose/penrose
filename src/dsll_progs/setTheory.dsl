constructor Set: type 				-- Declaration “Set A”
constructor Map: [type,type] -> type 	-- Declaration “Map f domain codomain”
constructor Point: type			-- Declaration “Point p”

operator Intersection: [Set,Set] -> Set
operator Union: [Set,Set] -> Set
operator Subtraction: [Set,Set] -> Set
operator CartesianProduct: [Set,Set] -> Set
operator Complement: [Set,Set] -> Set
operator Difference: [Set,Set] -> Set
operator Subset: [Set,Set] -> Set
operator AddPoint: [Point,Set] -> Set
operator Apply: fortypes [A,B]: [type], [Map(A,B), A] -> B
 
predicate Empty: [Set] -> Prop
predicate NonEmpty: [Set] -> Prop
predicate Intersect: [Set,Set] -> Prop
predicate NoIntersect: [Set,Set] -> Prop
predicate IsSubset: [Set,Set] -> Prop
predicate NoSubset: [Set,Set] -> Prop
predicate PointIn: [Set,Point] -> Prop
predicate PointNotIn: [Set,Point] -> Prop
predicate Injection: fortypes [A,B]: [type], [Map(A,B)] -> Prop
predicate Surjection: fortypes [A,B]: [type], [Map(A,B)] -> Prop
predicate Bijection: fortypes [A,B]: [type], [Map(A,B)] -> Prop
