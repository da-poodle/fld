:- use_module(library(fld)).

:- fld_object(named_obj, [a,b,c,d]).

fld:fld_default(a, blah).

testx :-
    fld_template(named_obj, N),
    named_obj_flds_set([a(blue), b(forth)], N, N1),
    named_obj_flds([a(blue), b(forth)], N1),
    N1 \= N.


:- begin_tests(fld_named).

test(named_default, [fail]) :-
    fld_template(named_obj, N),
    named_obj_flds([a(blue), b(_)], N).

test(named_default, [fail]) :-
    fld_template(named_obj, N),
    flds([a(blue), b(_)], N).

test(named_default, []) :-
    fld_template(named_obj, N),
    named_obj_flds_set([a(blue), b(forth)], N, N1),
    named_obj_flds([a(blue), b(forth)], N1),
    N1 \= N.

test(named_default, []) :-
    fld_template(named_obj, N),
    flds_set([a(blue), b(forth)], N, N1),
    flds([a(blue), b(forth)], N1).


:- end_tests(fld_named).
