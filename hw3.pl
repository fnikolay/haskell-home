/** Problem 7: */
father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).


/** Problem 8: */
%parent defines a child of a mother or father
parent(X,Y) :- (father(X, Y) ; mother(X, Y)).
grandma(X,Y) :- mother(X, Z) , parent(Z, Y).

/** Problem 9: */
descendants(X,Y) :- parent(X, Z) , descendants(Z, Y).
descendants(X,Y) :- parent(X, Y).
