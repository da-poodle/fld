:- use_module(library(fld)).

:- fld_object(person2, [name, age, gender, occupation, shoe_size]).

:- begin_tests(flds_set).

test(set_fields_set_exists) :-
    fld_template(person2, Frank),
    flds([name(frank),age(35),shoe_size(9)], Frank),
    flds_set([age(36), shoe_size(10)], Frank, OlderFrank),
    flds([age(36), shoe_size(10)], OlderFrank).

:- end_tests(flds_set).

