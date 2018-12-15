:- module(fld, [
              fld_object/2,
              fld/2,
              fld_set/3,
              flds/2,
              flds_set/3,
              fld_template/2,
              fld_generate/2,
              fld_destroy/1]).

:- dynamic(fld_object/2).
:- dynamic(fld/2).
:- dynamic(fld_set/3).

flds([], _).
flds([F|T], Obj) :-  fld(F, Obj), flds(T, Obj).

flds_set([], O, O).
flds_set([F|T], Obj, Newer) :-
    fld_set(F, Obj, New),
    flds_set(T, New, Newer).

fld_template(Name, Template) :-
    fld_object(Name, Flds),

    length(Flds, Len),
    obj(Name, Len, Template, _).

fld_destroy(Name) :- \+ fld_object(Name, _).
fld_destroy(Name) :-
    atom(Name),
    fld_object(Name, Flds),

    length(Flds, Len),
    maplist(obj(Name, Len), [Obj, SetObj, NewObj], _),

    retractall(fld(_,Obj)),
    retractall(fld_set(_,SetObj,NewObj)),
    retractall(fld_object(Name,Flds)),
    !.

fld_generate(Name, Flds) :-
    atom(Name),
    is_list(Flds),
    assert(fld_object(Name, Flds)),

    length(Flds, Len),
    generate_flds(Flds, Name, Len, 0),
    !.

generate_flds([], _, _, _).
generate_flds([F|T], Name, Len, N) :-

    % the field that will be the first argument
    Fld =.. [F, X],

    % the getter
    obj(Name, Len, Obj, Flds),
    fld_arg(X, Flds, N),
    assert(fld(Fld, Obj)),

    % the setter
    obj(Name, Len, SetObj, SetObjFlds),
    obj(Name, Len, NewObj, NewObjFlds),
    fld_set_arg(X, SetObjFlds, NewObjFlds, N),
    assert(fld_set(Fld, SetObj, NewObj)),

    % next field uses the next argument
    N1 is N + 1,
    generate_flds(T, Name, Len, N1).


% helper to generate blank objects
obj(Name, Len, Obj, Flds) :-
    length(Flds, Len),
    Obj =.. [Name|Flds].


% generate the second argument of the getter
fld_arg(Val, [Val|_], 0).
fld_arg(Val, [_|T], N) :-
    dif(N,0),
    N1 is N - 1,
    fld_arg(Val, T, N1).

% generate the second and third arguments of the setter
fld_set_arg(_, [], [], _).
fld_set_arg(Val, [F|T], [F|Nt], N) :-
    dif(N,0),
    N1 is N - 1,
    fld_set_arg(Val, T, Nt, N1).
fld_set_arg(Val, [_|T], [Val|Nt], 0) :-
    fld_set_arg(Val, T, Nt, -1).

