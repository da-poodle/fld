:- use_module('../prolog/fld').

:- fld_object(person, [name, age, gender]).

:- begin_tests(flds_get).

test(get_multi_field, [nondet]) :-
    P = person(greg,32,male),
    flds([name(greg),age(32),gender(male)], P).

test(get_multi_field_not_exists, [fail]) :-
    P = fld_template(person, P),
    flds([name(_), hat_type(_)], P).

test(create_and_set_single_field, [nondet]) :-
    flds([name(greg),age(32),gender(male)], P),
    P = person(greg,32,male).

:- end_tests(flds_get).