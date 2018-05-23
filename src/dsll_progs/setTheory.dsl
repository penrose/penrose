tconstructor Set: type 				
tconstructor Map([a,b] : [Set(),Set()]) : type 	
tconstructor Point: type

operator Intersection ([a,b] : [Set(),Set()]) : Set()
operator Union ([a,b] : [Set(),Set()]) : Set()
operator Substraciton ([a,b] : [Set(),Set()]) : Set()
operator CartesianProduct ([a,b] : [Set(),Set()]) : Set()
operator Difference ([a,b] : [Set(),Set()]) : Set()
operator Subset ([a,b] : [Set(),Set()]) : Set()
operator AddPoint ([p1,s1] : [Point(),Set()]) : Set()
operator Apply [['A,'B] : [type,type]] ([m1,s1] : [Map('A,'B), 'A]) : 'B
 
predicate Empty ([s] : [Set()]) : Prop
predicate NonEmpty ([s] : [Set()]) : Prop
predicate Intersect ([s1,s2] : [Set(),Set()]) : Prop
predicate NonIntersect ([s1,s2] : [Set(),Set()]) : Prop
predicate IsSubset ([s1,s2] : [Set(),Set()]) : Prop
predicate NoSubset ([s1,s2] : [Set(),Set()]) : Prop
predicate PointIn ([s1, p1] : [Set(),Point()]) : Prop
predicate PointNotIn ([s1, p1] : [Set(),Point()]) : Prop
predicate Injection [['A,'B] : [type,type]] ([m] : [Map('A,'B)]) : Prop
predicate Surjection [['A,'B] : [type,type]] ([m] : [Map('A,'B)]) : Prop
predicate Bijection [['A,'B] : [type,type]] ([m] : [Map('A,'B)]) : Prop

