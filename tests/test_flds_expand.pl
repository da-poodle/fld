:- use_module('../prolog/fld').

:- fld_object(tt, [a,b,c]).
:- fld_object(tt2, [x,y,c]).
:- fld_object(tt3, [x,y,d]).

manual_get_test(TT, A,B,C) :-
    fld TT:[a(A),b(B),c(C)].

% Tests of the fld:<template> goal
:- begin_tests(fld_template).

test(template_var, [true(G = (T = tt(_,_,_)))]) :-
    expand_goal(fld T:tt, G).
test(template_nonvar, [true(G = (T = tt(apple,banana, carrot)))]) :-
    T = tt(apple,banana,carrot),
    expand_goal(fld T:tt, G).

test(template_wrong_type, [true(G = (tt(_,_,_) = tt2(_, _, _)))]) :-
    T = tt(_,_,_),
    expand_goal(fld T:tt2, G).

test(template_wrong_type, [fail]) :-
    T = tt(_,_,_),
    fld T:tt2.

:- end_tests(fld_template).


% Tests of the fld X:a(..) and fld X:[..] goals
:- begin_tests(fld_get_expand).

test(expand_single_unique, [true(G = (T = tt(apple,_,_)))]) :-
    expand_goal(fld T:a(apple), G).

test(expand_single_dup, [true(G = (flds([c(carrot)],T)))]) :-
    expand_goal(fld T:c(carrot), G).

test(expand_double_unique, [true(G = (T = tt(apple,banana,_)))]) :-
    expand_goal(fld T:[a(apple),b(banana)], G).

test(expand_double_unique, [true(G = (T = tt(A,_,C)))]) :-
    expand_goal(fld T:[a(A),c(C)], G).

test(expand_double_unique, [true(G = (O = tt2(X, _, C)))]) :-
    expand_goal(fld O:[x(X), c(C)], G).

test(expand_double_dup, [true(G = flds([x(x),y(y)], O))]) :-
    expand_goal(fld O:[x(x), y(y)], G).


:- end_tests(fld_get_expand).

manual_set_test(T,T1,A,C) :-
    fld T1:[a(A),c(C)]-T.

% Tests of the fld X:a(..)-Y and fld X:[..]-Y goals
:- begin_tests(flds_set_expand).

test(expand_set_single_unique, [true(G = (T = tt(_,_,_), T1 = tt(_,b,_)))]) :-
    expand_goal(fld T1:b(b)-T, G).

test(expand_set_single_unique, [true(G = (T = tt(apple,banand,carrot), T1 = tt(apple,book,carrot)))]) :-
    T = tt(apple,banana,carrot),
    expand_goal(fld T1:b(book)-T, G).

test(expand_set_single_dup, [true(G = (flds_set([c(carrot)], T, T1)))]) :-
    expand_goal(fld T1:c(carrot)-T, G).

test(expand_set_double_unique, [true(G = (T = tt(apple,banana, carrot), T1 = tt(apple, book, car)))]) :-
    expand_goal(fld T1:[b(book), c(car)]-T, G).

test(expand_set_double_unique, [true(G = (tt(apple, banana, carrot) = tt(apple,banana, carrot), T1 = tt(apple, book, car)))]) :-
    expand_goal(fld T1:[b(book), c(car)]-tt(apple, banana, carrot), G).

test(expand_set_double_dup, [true(G = flds_set([x(X), y(Y)], T, T1))]) :-
    expand_goal(fld T1:[x(X), y(Y)]-T, G).

:- end_tests(flds_set_expand).
