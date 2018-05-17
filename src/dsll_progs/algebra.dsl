tconstructor Vector([v]:[VectorSpace]) : type                    
tconstructor Scalar : type                                               
tconstructor Map([t1,t2]:[type, type]) : type
tconstructor VectorSpace : type                                      

operator AddV[[V] : [VectorSpace]] ([v1,v2]:[Vector(V), Vector(V)]) : Vector(V)
operator AddS([s1,s2] : [Scalar, Scalar]) : Scalar
operator Norm[[V] : [VectorSpace]] ([v1] : [Vector(V)]) : Scalar
operator InnerProd[[V] : [VectorSpace]] ([v1,v2] : [Vector(V), Vector(V)]) : Scalar
operator Apply[[A,B]:[type,type]] ([m1,a1] : [Map(A,B), A]) : B

predicate LinearV [[U,V] : [VectorSpace,VectorSpace]] ([m1, v1] :[Map(Vector(U), Vector(V)), Vector(U)]) : Prop
predicate LinearS ([m1] : [Map(Scalar, Scalar)]) : Prop
predicate Not([l1] : [Prop]) : Prop
predicate And([l1,l2] : [Prop, Prop]) : Prop
