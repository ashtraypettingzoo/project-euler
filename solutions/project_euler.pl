% SWI-Prolog: https://www.swi-prolog.org/
% online: https://swish.swi-prolog.org/

p001(X) :- setof(Y, p001a(Y), Z), sumlist(Z, X).
p001a(X) :- between(0, 999, X), (X mod 3 < 1; X mod 5 < 1).
