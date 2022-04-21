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
@licence MIT
*/
:- module(fld, [
    (fld_object)/2,
    (fld)/1,
    op(900, fx, user:fld),
    
    % legacy
    (fld)/2,
    fld_set/3,
    flds/2,
    flds_set/3,
    fld_template/2,
    fld_fields/2
]).

%! fld_object(++Name:atom, ++Fields:list) is det.
% fields is a list of all fields that relate to object of name.
% this item will be created at compliation time and will generate the following:
% - fld/2 for each field in Fields.
% - fld_set/3 for each field in fields.
%
% After compilation, calling fld_object will return the list of fields for a previously
% defined object.
fld_object(Name, Fields) :- fld_object_def(Name, Fields).

:- multifile(fld_object_def/2).

%! fld(?Field:term, ?Object:term) is det.
% Field is a argument in an object.
:- multifile((fld)/2).

%! fld_set(?Field:term, ?Old:term, ?New:term) is nondet.
% New is the old term with field updated.
%:- dynamic(fld_set/3).
:- multifile(fld_set/3).

/* 
    FLD_TEMPLATE 
*/

%! fld_template(?Name:atom, ?Template:list) is nondet.
% template is an object with all fields as uninstaniated variables.
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

% expand the directives for the the fld_object.
user:term_expansion((:- fld_object(Name, Flds)), Preds) :-
    must_be(atom, Name),
    must_be(list, Flds),
    
    (fld_object_def(Name, Fd), dif(Fd, Flds)) ->
        format(atom(Error), 'redefining fld template ~p with fields ~p', [Name, Flds]),
        throw(error(type_error(fld_object/2, Error)))
    ;
    fld_object_def(Name, Flds) -> 
        Preds = []
    ;
    length(Flds, Len),
    generate_flds(Flds, Name, Len, 0, Getters, Setters),
    append(Getters, Setters, Result),
    Preds = [(fld):fld_object_def(Name, Flds)|Result],
    !.


/*
    FLD_FIELDS/2.
*/
%! fld_feilds(?Object:term, ?Fields:list) is semidet.
% return a list of all fields for object as terms instead of atoms.
fld_fields(Obj, Fields) :-
    Obj =.. [Name|Vals],
    fld_object_def(Name, Flds),

    maplist(fld_field_object,Flds,Vals,Fields).

fld_field_object(FldName,Value,Field) :- Field =.. [FldName,Value].


/*
    FLDS/2.
*/

%! flds(?Fields:list, ?Object:term) is nondet.
% Fields are a list of values that all exist in object.
flds([], _).
flds([F|T], Obj) :-  fld(F, Obj), flds(T, Obj).


/*
    FLDS_SET/3.
*/

%! flds_set(?Fields:list, ?Old:term, ?New:term) is nondet.
% new is the old term with all of fields updated.
flds_set([], O, O).
flds_set([F|T], Obj, Newer) :-
    fld_set(F, Obj, New),
    flds_set(T, New, Newer).

/*
    FLD/1
*/

fld(G) :- 
    format(atom(Error), 'Invalid fld goal ~p', G),
    throw(error(syntax_error(Error))).

% for the single named fld operation, need to know what type is being referenced
%
% fld _:<atom> is a template
% fld _:[...] is a flds reference
% fld _:<compound> is a flds reference (single field)
% fld _:[...]-_ is a set of a list of fields
% fld _:<compound>-_ is a set of a single field
% default will loop over all types
%
fld_typeof(T, Obj, Typed) :-
    var(T) -> throw(error(type_error([atom,list,compound],'fld types must be ground "fld _:<Type>".')))
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
    ; throw(error(existence_error('fld: template does not exist', T))).

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
