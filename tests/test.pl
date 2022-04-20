:- use_module('../prolog/fld').

:- fld_object(tt, [a,b,c]).
:- fld_object(tt2, [x,y,c]).
:- fld_object(tt3, [x,y,d]).

:- fld_object(person, [name, age, gender]).
:- fld_object(person2, [name, age, gender, occupation, shoe_size]).

:- ensure_loaded(test_single_field_get).
:- ensure_loaded(test_multi_field_get).
:- ensure_loaded(test_single_field_set).
:- ensure_loaded(test_multi_field_set).
:- ensure_loaded(test_lists_get).
:- ensure_loaded(test_flds_expand).

:- run_tests.


:- op(800, xfy, user:fld_type).
:- op(299, xf, user:lazy).
:- op(300, xf, user:many).


% field attributes test
fld_field_attributes(Name, fld_field(Name, [])) :- atom(Name).
fld_field_attributes(fld_type(Name, Type), fld_field(Name, Attrs)) :-
    sub_attributes(Type, Attrs).

sub_attributes(A, [sub_type(A)]) :- atom(A).
sub_attributes(lazy(Attr), [lazy|T]) :- sub_attributes(Attr, T).
sub_attributes(many(Attr), [many|T]) :- sub_attributes(Attr, T).

fld_object_test(business, [
    business_name,
    business_type,
    owner fld_type 'person',
    employees fld_type 'person' many lazy,
    contact fld_type 'contact_details' lazy many
]).

attrs(Name, Attr) :-
    fld_object_test(Name, B), maplist(fld_field_attributes, B, Attr).

% fld/flds test
% fld(Obj:Fld) :-
%     fld(Fld, Obj).

% fld(fld_type(Obj:Flds, Type)) :-
%     fld_template(Type, Obj),
%     fld(Flds, Obj).

% flds(Obj:Flds) :-
%     flds(Flds, Obj).

% flds(fld_type(Obj:Flds, Type)) :-
%     fld_template(Type, Obj),
%     flds(Flds, Obj).

test_op_flds(Name, Age, Occ) :-
    fld_template(person2, P),
    flds_set([name(fred), age(32), occupation(baker)],P, P1),
    flds P1:[name(Name), age(Age), occupation(Occ)]. 

test_op_fld(Name, Age, Sex) :-
    P = person(fred, 32, male),
    fld P:name(Name),
    fld P:age(Age),
    fld P:gender(Sex).

test_op_flds_type(Name, Age, Sex) :-
    P = person(fred, 32, male),
    flds P:[name(Name), age(Age), gender(Sex)] fld_type person. 