:- use_module('../prolog/fld').

:- fld_object(person, [name, age, gender]).

:- begin_tests(fld_get).

test(get_single_field_with_var, [nondet]) :-
    P = person(greg,_,_),
    Name = greg,
    fld(name(Name), P).

test(get_single_field_not_exists, [fail]) :-
    P = fld_template(person, P),
    fld(hat_type(_), P).

test(create_and_set_single_field, [nondet]) :-
    fld(name(greg), P),
    P = person(greg,_,_).

:- end_tests(fld_get).
