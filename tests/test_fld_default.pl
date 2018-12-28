:- use_module(library(fld)).

:- fld_object(person, [name, age, gender]).

test_default(name, 'John Smith').
test_default(age, 25).

create_defaults :-
    assert(fld:fld_default(name, 'Max')),
    assert(fld:fld_default(gender, unspecified)).

remove_defaults :-
    retract(fld:fld_default(name, 'Max')),
    retract(fld:fld_default(gender, unspecified)).


:- begin_tests(fld_default, [setup(create_defaults), cleanup(remove_defaults)]).

test(fld_default) :-
    fld_template(person, person('Max', Age, unspecified)),
    var(Age).

test(fld_default_override) :-
    fld_template(person, person('John Smith', 25, Gender), test_default),
    var(Gender).

:- end_tests(fld_default).
