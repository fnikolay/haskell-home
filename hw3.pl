/**Problem 7: */
father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

/**Problem 8: */
grandma(X,Y) :-mother(X, Z),(mother(Z, Y);father(Z, Y)).