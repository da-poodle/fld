:- use_module('../prolog/fld').

:- fld_object(tt, [a,b,c]).
:- fld_object(tt2, [x,y,c]).
:- fld_object(tt3, [x,y,d]).


:- begin_tests(flds_expand).

test(tt_expand, [true(G = (T = tt(apple,banana,_)))]) :-
    expand_goal(flds([a(apple),b(banana)], T), G).

test(tt_expand, [true(G = (T = tt(A,_,C)))]) :-
    expand_goal(flds([a(A),c(C)], T), G).

test(tt_expand, [true(G = flds([x(x),y(y)], O))]) :-
    expand_goal(flds([x(x), y(y)], O), G).

test(tt_expand, [true(G = (O = tt2(X, _, C)))]) :-
    expand_goal(flds([x(X), c(C)], O), G).

:- end_tests(flds_expand).