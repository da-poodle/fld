:- use_module('../prolog/fld').

:- fld_object(extend_obj1, [*(extend_obj1),oa,ob,oc]).

:- fld_object(extend_obj1, [oa,ob,oc]).
:- fld_object(extend_obj2, [*(extend_obj1), od, oe, of]).
:- fld_object(extend_obj3, [*(extend_obj2), ob, od, oe]).
:- fld_object(extend_obj4, [of, og, oe, *(extend_obj1), *(extend_obj2)]).
:- fld_object(extend_obj5, [oi, *(extend_obj1), *(extend_obj2), oz]).


:- begin_tests(fld_extend).

test(extend_obj2, [true(Flds = [oa, ob, oc, od, oe, of])]) :-
    fld_object(extend_obj2, Flds).

test(extend_obj3, [true(Flds = [oa, ob, oc, od, oe, of])]) :-
    fld_object(extend_obj3, Flds).

test(extend_obj4, [true(Flds = [of, og, oe, oa, ob, oc, od])]) :-
    fld_object(extend_obj4, Flds).

test(extend_obj5, [true(Flds = [oi, oa, ob, oc, od, oe, of, oz])]) :-
    fld_object(extend_obj5, Flds).

:- end_tests(fld_extend).