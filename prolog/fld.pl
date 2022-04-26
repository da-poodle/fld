/** fld

MIT License

Copyright (c) 2018-2022 Neil Hoskins

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

@author Neil Hoskins

@license MIT
*/
:- module(fld,[
              (fld_object)/2,
              (fld)/1,
              op(900, fx, user:fld),
              (fld)/2,
              fld_set/3,
              flds/2,
              flds_set/3,
              fld_template/2,
              fld_fields/2
          ]).

%! fld_object(+Name:atom, +Fields:list) is det.
% If this term is used as a directive, then a new fld_object_def is created.
% In this case, Name is the name of the template that will be created, and
% Fields is the list of field names that can be used for that template.
% The fld_object_def/2 is created at compile time along with fld/2 and fld_set/3 terms
% for each fields in Feilds.
%
% Usage:
%==
% :- fld_object(person, [name,age,gender]).
%==
%
% If this term is used as a goal, then Name will match with an existing fld_object type,
% and Fields with the fields that were defined.
%
% Usage:
%==
% ?- fld_object(person, Fields).
% Fields = [name,age,gender].
%==
fld_object(Name, Fields) :- fld_object_def(Name, Fields).

:- multifile fld_object_def/2.

%! fld(?Spec:compound, ?Object:term) is semidet.
% Spec is a value that exists as an argument in Object.
% Object must be a valid fld_object type.
%
% Replaced by `fld Object:Spec.`
%
% @see fld/1.
:- multifile (fld)/2.

%! fld_set(?Spec:compound, In:term, Out:term) is semidet.
% Out is the same term as In but with the Spec value replaced.
% In and Out must be the same term type, and the type must be
% an fld_object type.
%
% Replaced by `fld Out:Spec-In.`
%
% @see fld/1
:- multifile fld_set/3.

%! fld_template(?Name:atom, ?Template:compound) is semidet.
% Template is a blank term created from the fld_object spec for Name.
% This predicate can be used to get all templates or match on a spec.
%
% Alternative to `fld Template:Name`, fld/1 matches a single template only.
%
% @see fld/1
fld_template(Name, Template) :-
    fld_object_def(Name, Flds),
    length(Flds, Len),
    length(TemplateFlds, Len),
    Template =.. [Name|TemplateFlds].

/*
    FLD_OBJECT
*/

/* Generate fld_object_def/2, fld/2 and fld_set/3 predicates */
generate_flds([], _, _, _, [], []).
generate_flds([F|T], Name, Len, N, [Getter|MoreGetters],[Setter|MoreSetters]) :-

    % the field that will be the first argument
    Fld =.. [F, X],

    % the getter
    obj(Name, Len, Obj, Flds),
    fld_arg(X, Flds, N),
    Getter = (fld):fld(Fld, Obj),

    % the setter
    obj(Name, Len, SetObj, SetObjFlds),
    obj(Name, Len, NewObj, NewObjFlds),
    fld_set_arg(X, SetObjFlds, NewObjFlds, N),
    Setter = (fld):fld_set(Fld, SetObj, NewObj),

    % next field uses the next argument
    N1 is N + 1,
    generate_flds(T, Name, Len, N1, MoreGetters, MoreSetters).


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

% expand fields so that extended objects are included.
expand_field_list([]) --> [].
% *(Obj) will add the fields from an existing object to this object
expand_field_list([*(Obj)|T]) --> extend_field_list(Obj), expand_field_list(T), !.
% default is to add the field
expand_field_list([F|T]) --> { atom(F) }, [F], expand_field_list(T).

% expand out an inheritance, get the list of fields and call expand on that list
extend_field_list(Obj) --> {
        fld_object_def(Obj, ObjFields) -> true
        ;
        '$existence_error'(fld_object, Obj)
    },
    expand_field_list(ObjFields).


% expand the directives for the the fld_object.
user:term_expansion((:- fld_object(Name, Flds)), Preds) :-
    must_be(atom, Name),
    must_be(list, Flds),

    (
        ground(Flds) -> true;
        '$domain_error'('fld list of ONLY ground values', fld_object(Name, Flds))
    ),

    phrase(expand_field_list(Flds), FldList, []),
    list_to_set(FldList, UniqueFields),

    (
        (fld_object_def(Name, Fd), dif(Fd, UniqueFields)) ->
            format(atom(Error), '~p which redefines ~p', [fld_object(Name, Flds), fld_object(Name, Fd)]),
            '$domain_error'('unique fld', Error)
        ;
        fld_object_def(Name, UniqueFields) ->
            Preds = []
        ;
        length(UniqueFields, Len),
        generate_flds(UniqueFields, Name, Len, 0, Getters, Setters),
        append(Getters, Setters, Result),
        Preds = [(fld):fld_object_def(Name, UniqueFields)|Result]
    ),
    !.

%! fld_fields(?Object:term, ?Fields:list) is semidet.
% return a list of all fields for object as terms instead of atoms.
%
% Usage:
%==
% ?- fld_fields(person(fred,32,male), Fields).
% Fields = [name(fred),age(32),gender(male)].
%==
%
fld_fields(Obj, Fields) :-
    Obj =.. [Name|Vals],
    fld_object_def(Name, Flds),

    maplist(fld_field_object,Flds,Vals,Fields).

fld_field_object(FldName,Value,Field) :- Field =.. [FldName,Value].

%! flds(?Spec:list(compound), ?Object:term) is semidet.
% Object is an fld_object type, and all items in Spec must unify
% with the coresponding arguments in Object based on the fld_object spec.
%
% This predicate can operate over multiple fld_object types for the same spec.
%
% Replaced by `fld Object:Spec.`
%
% @see fld/1
flds([], _).
flds([F|T], Obj) :-  fld(F, Obj), flds(T, Obj).

%! flds_set(?Spec:list(compound/1), ?In:term, ?Out:term) is semidet.
% Out is the same type as In but with fields in Spec replaced, according to
% the fld_object template spec.
%
% This predicate can operate over multiple fld_object types.
%
% Replaced by `fld Out:Spec-In.`
%
% @see fld/1
flds_set([], O, O).
flds_set([F|T], Obj, Newer) :-
    fld_set(F, Obj, New),
    flds_set(T, New, Newer).

%! fld(Spec) is det.
% Spec varies, see below.
%
% fld/1 is an expanded term in which Spec has a different function according to the values provided.
% The following is possible:
% - check fld type
% - extract value(s) from fld type
% - replace value(s) in fld type
%
% The examples use ``:- fld_object(t,[a,b,c]).`` as an example type specification.
%
%---
% *Templates, Type Checking*
%==
% fld T:t.
%==
% T is a template of the spec for type t. This can be used to either create a blank term or type t
% or check the a variable is of type t.
% Expands to
%==
% T = t(_,_,_).
%==
%
%---
% *Get Single Value*
%==
% fld T:a(A).
%==
% Unify A with the a argument in T.
%
% If  is unique to the type t, then expansion will be:
%==
%T = t(A,_,_).
%==
%
% If a is not unque to type t, then expansion will be:
%==
% fld([a(A)],T).
%==
%
%---
% *Get Multiple Values*
%==
% fld T:[a(A),b(B)].
%==
% Unify A and B with the a,b arguments in T.
%
% If the A,B combination is unique to the type t, then expansion will be:
%==
%T = t(A,B,_).
%==
%
% If the combination is not unque to type t, then expansion will be:
%==
% fld([a(A),b(B)],T).
%==
%
%---
% *Set Single Value*
%==
% fld T1:a(A)-T.
%==
% Unify A with the value of the a argument in T1, T is T1 without A.
% This is used for replacing fields from T in T1.
%
% If A is unique to type t, then expand to
%==
% T = t(_,B,C), T1 = (X,B,C).
%==
% Otherwise expand to
%==
% flds_set([c(X)], T, T1).
%==
%
%---
% *Set Multiple Values*
%==
% fld T1:[a(A),b(B)]-T.
%==
% Unify A,B with the value of the a,b arguments in T1, T is T1 without A,B.
% This is used for replacing fields from T in T1.
%
% If the a,b combination is unique to type t, then expand to
%==
% T = t(_,_,C), T1 = (A,B,C).
%==
% Otherwise expand to
%==
% flds_set([a(A),b(B)], T, T1).
%==
%
fld(G) :-
    throw(syntax_error(G, 'fld/1 must be in the form "fld Dest:template", "fld Dest:<getter(s)>" or "fld Dest:<setter(s)>-Src".')).

%
fld_typeof(T, Obj, Typed) :-
    var(T) -> '$type_error'(atom, T)
    ;
    T = SetRule-In -> (
        fld_typeof(SetRule, Obj, flds(SetFlds, Obj)),
        Typed = flds_set(SetFlds, In, Obj)
    )
    ;
    atom(T) -> Typed = template(T, Obj)
    ;
    is_list(T) -> Typed = flds(T, Obj)
    ;
    compound(T),
    Typed = flds([T], Obj).

% Obj = the resulting object
% T = the type that is being expanded
% Expanded = the expanded term
expand_fld(Obj, T, Expanded) :-
    fld_typeof(T, Obj, Typed),
    fld_expand_type(Typed, Expanded).

% expand the template type
fld_expand_type(template(T, Obj), (Obj = Mapped)) :-
    fld_template(T, Mapped) -> true
    ; '$existence_error'('fld template', T).

% expand the flds type
fld_expand_type(flds(T, Obj), Mapped) :-
    expand_flds(T, Obj, Mapped).

% expand the flds_set type
fld_expand_type(flds_set(FldArgs, ObjIn, ObjOut), Mapped) :-
    expand_fld_set(ObjIn, ObjOut, FldArgs, Mapped).


/*
    Helpers
*/
split_fld_args(FldArg, FldName, FldValue) :-
    FldArg =.. [FldName, FldValue].


/*
    Map Getters to Flds
*/
expand_flds(FldArgs, Obj, Result) :-
    % create a list of the fields that are being accessed
    maplist(split_fld_args, FldArgs, FldNames, FldValues),

    % find ALL objects that have the set of fields
    % only sets that have one fld_object can be processed.
    findall(Name, (fld_object_def(Name, Flds), subset(FldNames, Flds)), FldObjects),

    map_get_fields(FldObjects, Obj, FldNames, FldValues, Result).

% if not fld_object_def is found, error out
map_get_fields([], _Obj, FldNames, _FldValues, _Result) :- throw(error(existence_error('fld: no templates match query for get', FldNames))).

% a single fld_object_def means a direct unification
map_get_fields([FldObject], Obj, FldNames, FldValues, (Obj = Result)) :-
    map_getter_fields_to_value(FldObject, FldNames, FldValues, Result).

% multiple fld_object_defs, call legacy
map_get_fields([_,_|_], Obj, FldNames, FldValues, flds(FldArgs, Obj)) :-
    maplist(split_fld_args, FldArgs, FldNames, FldValues).


% expand out a a template to an object and map the desired get fields to
% the same fields in the template
map_getter_fields_to_value(FldObject, FldNames, FldValues, Template) :-
    fld_object_def(FldObject, Flds),

    % create a list of variables to substitute for the flds,
    % we don't know the order of the values yet
    maplist(value_to_fld(FldNames, FldValues), Flds, GetValues),

    % create the result from the values.
    Template =.. [FldObject | GetValues].

% set the value on an argument in the template object
value_to_fld(FldNames, FldValues, Fld, Value) :-
    memberchk(Fld, FldNames) ->
        nth0(Idx, FldNames, Fld),
        nth0(Idx, FldValues, Value)
    ;
    Value = _.


/*
    Map Setters to Flds
*/

% ObjIn = the original object
% ObjOut = the set object
% T = the type that is being expanded
expand_fld_set(ObjIn, ObjOut, FldArgs, Result) :-

    % create a list of the fields that are being accessed
    maplist(split_fld_args, FldArgs, FldNames, FldValues),

    % find ALL objects that have the set of fields
    % only sets that have one fld_object can be processed.
    findall(Name, (fld_object_def(Name, Flds), subset(FldNames, Flds)), FldObject),

    % create the objects
    map_set_fields(FldObject, ObjIn, ObjOut, FldNames, FldValues, Result).

% if not fld_object_def is found, error out
map_set_fields([], _ObjIn, _ObjOut, FldNames, _FldValues, _Result) :- throw(error(existence_error('fld: no templates match query for set', FldNames))).

% a single fld_object_def means a direct unification
map_set_fields([FldObject], ObjIn, ObjOut, FldNames, FldValues, (ObjIn = In, ObjOut = Out)) :-
    map_setter_fields_to_value(FldObject, FldNames, FldValues, In, Out).

% multiple fld_object_defs, call legacy
map_set_fields([_,_|_], ObjIn, ObjOut, FldNames, FldValues, flds_set(FldArgs, ObjIn, ObjOut)) :-
    maplist(split_fld_args, FldArgs, FldNames, FldValues).

% map the in and out terms, need two terms here.
% The In term defines all the fields that are not being set
% The Out term defines all the feilds and maps the set ones.
map_setter_fields_to_value(FldObject, FldNames, FldValues, In, Out) :-
    fld_object_def(FldObject, Flds),
    fld_template(FldObject, In),
    In =.. [_|Setters],
    fld_template(FldObject, Out),
    Out =.. [_|NewSetters],

    maplist(value_set_to_fld(FldNames, FldValues), Flds, Setters, NewSetters).


value_set_to_fld(FldNames, FldValues, Fld, Setter, NewSetter) :-
    memberchk(Fld, FldNames) ->
        nth0(Idx, FldNames, Fld),
        nth0(Idx, FldValues, NewSetter)
    ;
    NewSetter = Setter.

% Map a list of fields directly to an object so that a single operation is required
% to access all the fields
user:goal_expansion((fld Obj:FldArgs), Result) :-
    expand_fld(Obj, FldArgs, Result).
