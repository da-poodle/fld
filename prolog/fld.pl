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
    (fld)/1,
    op(900, fx, user:fld)
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

% expand the directives for the the fld_object.
user:term_expansion((:- fld_object(Name, Flds)), (fld):fld_object_def(Name, Flds)) :-
    must_be(atom, Name),
    must_be(list(atom), Flds).

fld(_) :- throw(error(syntax_error('fld goals are in the form: "fld A:{Type(s)}" or "fld B:{Type(s)}]-A"'))).


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
    fld_template(T, Mapped).

% expand the flds type
fld_expand_type(flds(T, Obj), (Obj = Mapped)) :-
    expand_flds(T, Mapped).

% expand the flds_set type
fld_expand_type(flds_set(FldArgs, ObjIn, ObjOut), Mapped) :-
    expand_fld_set(ObjIn, ObjOut, FldArgs, Mapped).

% expand the general type
fld_expand_type(fld_type(T, Obj), (Obj = T)) :-
    fld_template(T, _)
    ; 
    fld_template(_, Flds),
    member(T, Flds).

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
    Map Getters to Flds
*/
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

expand_flds(FldArgs, Result) :-
    % create a list of the fields that are being accessed
    maplist(split_fld_args, FldArgs, FldNames, FldValues),

    % find ALL objects that have the set of fields
    % only sets that have one fld_object can be processed.
    findall(Name, (fld_object_def(Name, Flds), subset(FldNames, Flds)), [FldObject]),

    map_getter_fields_to_value(FldObject, FldNames, FldValues, Result).

/*
    Map Setters to Flds
*/

% ObjIn = the original object
% ObjOut = the set object
% T = the type that is being expanded
expand_fld_set(ObjIn, ObjOut, FldArgs, (ObjIn = In, ObjOut = Out)) :-

    % create a list of the fields that are being accessed
    maplist(split_fld_args, FldArgs, FldNames, FldValues),

    % find ALL objects that have the set of fields
    % only sets that have one fld_object can be processed.
    findall(Name, (fld_object_def(Name, Flds), subset(FldNames, Flds)), [FldObject]),

    % create the objects
    map_setter_fields_to_value(FldObject, FldNames, FldValues, In, Out).


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
