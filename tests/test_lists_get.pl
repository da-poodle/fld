:- use_module('../prolog/fld').

:- fld_object(person, [name, age, gender]).

people([
    person(mary,25,female),
    person(frank,40,male),
    person(greg,34,male),
    person(adriana,23,female)
]).

:- begin_tests(fld_list_get).

test(fld_using_include, [nondet]) :-
    people(People),
    include(fld(gender(female)), People, Girls),
    Girls = [Girl1,Girl2],
    fld(name(mary), Girl1),
    fld(name(adriana), Girl2).

test(fld_using_exclude, [nondet]) :-
    people(People),
    exclude(fld(gender(female)), People, Men),
    Men = [Man1,Man2],
    fld(name(frank), Man1),
    fld(name(greg), Man2).

test(fld_using_partition, [nondet]) :-
    people(People),
    partition(fld(gender(male)), People, Men, Girls),

    Men = [Man1,Man2],
    fld(name(frank), Man1),
    fld(name(greg), Man2),

    Girls = [Girl1,Girl2],
    fld(name(mary), Girl1),
    fld(name(adriana), Girl2).

:- end_tests(fld_list_get).





