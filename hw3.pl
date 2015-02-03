/** Problem 7: */
father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).


/** Problem 8: */
%parent defines a child of a mother or father
parent(X,Y) :- (father(X, Y) ; mother(X, Y)).
grandma(X,Y) :- mother(X, Z) , parent(Z, Y),!.

/** Problem 9: */
descendants(X,Y) :- parent(X, Z) , descendants(Z, Y).
descendants(X,Y) :- parent(X, Y).

/** Problem 10: */
siblings(X,Y) :- (X\=Y) , (parent(Z,X) , parent(Z,Y)).

/** Problem 11: */
%    ASCII-ART for the NFA:
%
%    (q0)  ---a-->  (q1)  ---b-->  (q2*)
%     |
%     a
%     |
%     V  / --<-- \
%    (q3*)        a
%        \ -->-- /

%  Transition relation:
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).

%  Accepting states:
accepting(q2).
accepting(q3).

%accepts(State, _) :- accepting(State).
accepts(State, InputList) :- InputList = [Head|Tail] ->
							(transition(State, Next, Head),
							accepts(Next, Tail)) ; accepting(State).
%p([H|T], H, T).
