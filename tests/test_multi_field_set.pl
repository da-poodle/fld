:- begin_tests(flds_set).

test(set_fields_set_exists) :-
    fld_template(person2, Frank),
    flds([name(frank),age(35),shoe_size(9)], Frank),
    flds_set([age(36), shoe_size(10)], Frank, OlderFrank),
    flds([age(36), shoe_size(10)], OlderFrank).

:- end_tests(flds_set).

