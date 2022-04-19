:- use_module('../prolog/fld').

:- fld_object(tt, [a,b,c]).
:- fld_object(tt2, [x,y,c]).
:- fld_object(tt3, [x,y,d]).

manual_get_test(TT, A,B,C) :-
    flds([a(A),b(B),c(C)], TT).

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

manual_set_test(T,T1,A,C) :-
    flds_set([a(A),c(C)], T, T1).

:- begin_tests(flds_set_expand).

test(tt_set_expand, [true(G = (T = tt(apple,banana, carrot), T1 = tt(apple, book, car)))]) :-
    expand_goal(flds_set([b(book), c(car)], T, T1), G).

test(tt_set_expand, [true(G = (tt(apple, banana, carrot) = tt(apple,banana, carrot), T1 = tt(apple, book, car)))]) :-
    expand_goal(flds_set([b(book), c(car)], tt(apple, banana, carrot), T1), G).

test(tt_set_expand, [true(G = flds_set([x(X), y(Y)], T, T1))]) :-
    expand_goal(flds_set([x(X), y(Y)], T, T1), G).

:- end_tests(flds_set_expand).