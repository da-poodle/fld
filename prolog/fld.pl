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
              fld_object/2,
              fld/2,
              fld_set/3,
              flds/2,
              flds_set/3,
              fld_template/2,
              fld_fields/2,
              fld_destroy/1]).

:- dynamic(fld_object_def/2).

%! fld(?Field:term, ?Object:term) is det.
% Field is a argument in an object.
:- dynamic(fld/2).

%! fld_set(?Field:term, ?Old:term, ?New:term) is nondet.
% New is the old term with field updated.
:- dynamic(fld_set/3).

%! flds(?Fields:list, ?Object:term) is nondet.
% Fields are a list of values that all exist in object.
flds([], _).
flds([F|T], Obj) :-  fld(F, Obj), flds(T, Obj).

%! flds_set(?Fields:list, ?Old:term, ?New:term) is nondet.
% new is the old term with all of fields updated.
flds_set([], O, O).
flds_set([F|T], Obj, Newer) :-
    fld_set(F, Obj, New),
    flds_set(T, New, Newer).

%! fld_template(?Name:atom, ?Template:list) is nondet.
% template is an object with all fields as uninstaniated variables.
fld_template(Name, Template) :-
    fld_object_def(Name, Flds),

    length(Flds, Len),
    obj(Name, Len, Template, _).

%! fld_object(++Name:atom, ++Fields:list) is det.
% fields is a list of all fields that relate to object of name.
% if the name does not exist then it is created.
fld_object(Name, Flds) :- fld_object_def(Name, Flds), !.
fld_object(Name, Flds) :-
    atom(Name),
    is_list(Flds),

    % create the object only if it doesn't already exist
    \+ fld_object(Name, _),
    assert(fld_object_def(Name, Flds)),

    length(Flds, Len),
    generate_flds(Flds, Name, Len, 0),
    !.


%!  fld_destroy(++Name:atom) is det.
% The fld_object that relates to name is no longer usable.
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

%! fld_feilds(?Object:term, ?Fields:list) is semidet.
% return a list of all fields for object as terms instead of atoms.
fld_fields(Obj, Fields) :-

    Obj =.. [Name|Vals],
    fld_object(Name, Flds),

    maplist(fld_field_object,Flds,Vals,Fields).

fld_field_object(FldName,Value,Field) :- Field =.. [FldName,Value].















