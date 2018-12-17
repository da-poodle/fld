:- use_module(library(fld)).

:- fld_object(person, [name, age, gender]).

:- begin_tests(fld_set).

test(set_field_set_exists, [nondet]) :-
    fld_set(name(frank), person(greg, 35, male), person(frank, 35, male)).

test(set_field_set_last, [nondet]) :-
    fld_set(gender(female), person(tom, 35, male), person(tom, 35, female)).

test(set_field_set_not_exists, [fail]) :-
    fld_set(feet_size(9), person(greg, 35, male), _).

test(set_field_set_dif_objects, [fail]) :-
    fld_set(name(frank), poison(greg, 35, male), person(frank, 35, male)).

:- end_tests(fld_set).

