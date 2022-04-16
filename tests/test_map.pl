:- use_module('../prolog/fld').

:- fld_object(o1, [f1, f2, f3]).
:- fld_object(o2, [f1, f2]).
:- fld_object(o3, [f2, f4]).

:- fld_mapper(o2, o1).
:- fld_mapper(o3, o1).
:- fld_mapper(o3, o2).

:- begin_tests(mapper).

test(map_o1_o2, [nondet]) :-
    fld_map(o2(x, y), o1(x, y, z)).

test(map_o1_o2, [true(O2 = o2(x, y)), nondet]) :-
    fld_map(O2, o1(x, y, z)).

test(map_o3_o2, [true((O3 = o3(A, x), dif(A, a)))]) :-
    fld_map(O3, o2(a, x)).

test(map_o3_o2, [true(A = x)]) :-
    fld_template(o3, O3),
    fld_template(o2, O2),
    fld(f2(x), O2),
    fld_map(O3, O2),
    fld(f2(A), O3),
    !.

:- end_tests(mapper).