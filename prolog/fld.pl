/** fld

MIT License

Copyright (c) 2018 Neil Hoskins

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
    fld/2,
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
%:- dynamic(fld/2).
:- multifile(fld/2).

%! fld_set(?Field:term, ?Old:term, ?New:term) is nondet.
% New is the old term with field updated.
%:- dynamic(fld_set/3).
:- multifile(fld_set/3).

%! fld_template(?Name:atom, ?Template:list) is nondet.
% template is an object with all fields as uninstaniated variables.
% defaults are taken from the fld:fld_default/2 predicates.
fld_template(Name, Template) :-
    fld_object_def(Name, Flds),
    length(Flds, Len),
    length(TemplateFlds, Len),
    Template =.. [Name|TemplateFlds].

/* Generate fld_object_def/2, fld/2 and fld_set/3 predicates */
generate_flds([], _, _, _, [], []).
generate_flds([F|T], Name, Len, N, [Getter|MoreGetters],[Setter|MoreSetters]) :-

    % the field that will be the first argument
    Fld =.. [F, X],

    % the getter
    obj(Name, Len, Obj, Flds),
    fld_arg(X, Flds, N),
    Getter = fld:fld(Fld, Obj),

    % the setter
    obj(Name, Len, SetObj, SetObjFlds),
    obj(Name, Len, NewObj, NewObjFlds),
    fld_set_arg(X, SetObjFlds, NewObjFlds, N),
    Setter = fld:fld_set(Fld, SetObj, NewObj),

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
    
    fld_object_def(Name, Flds) -> Preds = []
    ;
    length(Flds, Len),
    generate_flds(Flds, Name, Len, 0, Getters, Setters),
    append(Getters, Setters, Result),
    Preds = [fld:fld_object_def(Name, Flds)|Result],
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

map_getter_fields_to_value(FldObject, FldNames, FldValues, Template) :-
    fld_object_def(FldObject, Flds),

    % create a list of variables to substitute for the flds, 
    % we don't know the order of the values yet
    maplist(value_to_fld(FldNames, FldValues), Flds, GetValues),

    % create the result from the values.
    Template =.. [FldObject | GetValues].

value_to_fld(FldNames, FldValues, Fld, Value) :-
    memberchk(Fld, FldNames) -> 
        nth0(Idx, FldNames, Fld),
        nth0(Idx, FldValues, Value)
    ;
    Value = _.

split_fld_args(FldArg, FldName, FldValue) :-
    FldArg =.. [FldName, FldValue].

% Map a list of fields directly to an object so that a single operation is required
% to access all the fields
user:goal_expansion(flds(FldArgs, Object), (Object = Result)) :-
    is_list(FldArgs),

    % create a list of the fields that are being accessed
    maplist(split_fld_args, FldArgs, FldNames, FldValues),

    % find ALL objects that have the set of fields
    % only sets that have one fld_object can be processed.
    findall(Name, (fld_object_def(Name, Flds), subset(FldNames, Flds)), [FldObject]),

    map_getter_fields_to_value(FldObject, FldNames, FldValues, Result).


/*
    FLDS_SET/3.
*/

%! flds_set(?Fields:list, ?Old:term, ?New:term) is nondet.
% new is the old term with all of fields updated.
flds_set([], O, O).
flds_set([F|T], Obj, Newer) :-
    fld_set(F, Obj, New),
    flds_set(T, New, Newer).

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
user:goal_expansion(flds_set(FldArgs, ObjIn, ObjOut), (ObjIn = In, ObjOut = Out)) :-
    is_list(FldArgs),

    % create a list of the fields that are being accessed
    maplist(split_fld_args, FldArgs, FldNames, FldValues),

    % find ALL objects that have the set of fields
    % only sets that have one fld_object can be processed.
    findall(Name, (fld_object_def(Name, Flds), subset(FldNames, Flds)), [FldObject]),

    map_setter_fields_to_value(FldObject, FldNames, FldValues, In, Out).
