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
              fld_default/2,
              fld/2,
              flds/2,
              fld_set/3,
              flds_set/3,
              fld_template/2,
              fld_template/3,
              fld_fields/2,
              fld_fields/3
          ]).

:- meta_predicate fld:fld_template(*,*,2).

:- discontiguous(fld:fld_object_def/2).
:- multifile(fld_object_def/2).

:- discontiguous(fld_object/2).
:- multifile(fld_object/2).

%! fld(?Field:term, ?Object:term) is det.
% Field is a argument in an object.
:- discontiguous(fld/2).
:- multifile(fld/2).

:- discontiguous(flds/2).
:- multifile(flds/2).

%! fld_set(?Field:term, ?Old:term, ?New:term) is nondet.
% New is the old term with field updated.
:- discontiguous(fld_set/3).
:- multifile(fld_set/3).

%! fld_default(+Field:atom, ?Default:term) is semidet.
% A default is defined by the user, if no default is used then an uninstantiated variable will be used.
:- discontiguous(fld_default/2).
:- multifile(fld_default/2).


fld_object(Name, Fields) :- fld_object_def(Name, Fields).

%! fld_template(?Name:atom, ?Template:list) is nondet.
% template is an object with all fields as uninstantiated variables.
% defaults are taken from the fld:fld_default/2 predicates.
fld_template(Name, Template) :-
    fld_template(Name, Template, fld_default).

% ! fld_template(?Name:atom, ?Template:list, ++Goal:callable) is nondet.
% template is an object with all fields as uninstantiated variables.
% Goal determines the defaults for the fields or if there is not default
% for a field then an uninstantiated variable is used.
fld_template(Name, Template, Goal) :-
    fld_object_def(Name, Flds),
    length(Flds, Len),
    length(TemplateFlds, Len),
    Template =.. [Name|TemplateFlds],
    callable(Goal) ->
    maplist(fld_add_default(Goal), Flds, TemplateFlds)
    ;
    true.
	
fld_add_default(Goal, Field, Value) :-
    call(Goal, Field, Value) -> true ; true.

blank_template(_,_).


%! fld_object(++Name:atom, ++Fields:list) is det.
% fields is a list of all fields that relate to object of name.
% if the name does not exist then it is created.
% fld_object(Name, Flds) :- fld_object_def(Name, Flds), !.

generate_flds([], _, _, _, []).
generate_flds([F|T], Name, Len, N, [fld:fld(Fld, Obj), fld:fld_set(Fld, SetObj, NewObj)|Rest]) :-

    % the field that will be the first argument
    Fld =.. [F, X],

    % the getter
    obj(Name, Len, Obj, Flds),
    fld_arg(X, Flds, N),

    % the setter
    obj(Name, Len, SetObj, SetObjFlds),
    obj(Name, Len, NewObj, NewObjFlds),
    fld_set_arg(X, SetObjFlds, NewObjFlds, N),

    % next field uses the next argument
    N1 is N + 1,
    generate_flds(T, Name, Len, N1, Rest).


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

resolve_parent_tree(Name, Flds, Name, Flds) :- Name \= _/_.
resolve_parent_tree(Child/Parent, NewFlds, Name, AllFlds) :-
	resolve_parent_tree(Child, NewFlds, Name, ChildFlds),
	fld_object_def(Parent, Flds),
	list_to_set(Flds, SetOfParentFlds),
	list_to_set(ChildFlds, SetOfChildFlds),
	union(SetOfChildFlds, SetOfParentFlds, AllFlds).
	
system:term_expansion(':-'(fld_object(Name, Flds)), [fld:fld_object_def(GenName, GenFlds)|GetSet]) :-
	resolve_parent_tree(Name, Flds, GenName, GenFlds),
    \+ fld_object_def(GenName, GenFlds),
    length(GenFlds, Len),	
    generate_flds(GenFlds, GenName, Len, 0, GetSet).


%! fld_fields(?Object:term, ?Fields:list) is semidet.
% return a list of all fields for object as terms instead of atoms.
fld_fields(Obj, Fields) :-
    fld_fields(Obj, _, Fields).

%! fld_fields(?Object:term, +Name:list, ?Fields:list) is semidet.
% return a list of all fields for object as terms instead of atoms and
% the name.
%
% Either the Name and List are ground or the Object is ground
fld_fields(Obj, Name, Fields) :-
    compound(Obj),
    Obj =.. [Name|Vals],
    fld_object_def(Name, Flds),
    maplist(fld_field_object,Flds,Vals,Fields).
fld_fields(Obj, Name, Fields) :-
    atom(Name),
    is_list(Fields),
    fld_object_def(Name, Flds),
    maplist(fld_field_object,Flds,Vals,Fields),
    Obj =.. [Name|Vals].


fld_field_object(FldName,Value,Field) :- Field =.. [FldName,Value].




% expand the type specific goals to be efficient
% to do this look for a name of <type>_flds and expand this to use the
% actual object rather than the fld lookup method
resolve_fld(Template, Getter) :-
    fld(Getter, Template)
    ;
    Template =.. [Name|_],
    throw(fld_error(Getter, Name, 'fld mapping not found for object')).

system:goal_expansion(Flds, (Object = Template)) :-
    Flds =.. [Name,List,Object],
    atom(Name),
    atom_concat(FldType, '_flds', Name),
    fld_template(FldType, Template, blank_template),
    maplist(resolve_fld(Template), List).


% expand the flds term to use the multiple fld terms instead
% this is signifiantly faster that using a list, but can fail if the field does not exist.
flds_to_fld([], _, Last, Last).
flds_to_fld([Fld|T], Object, Last, ','(Last, Result)) :-
    flds_to_fld(T, Object, fld(Fld, Object), Result).

flds_to_fld([Fld|T], Object, Result) :-
    flds_to_fld(T, Object, fld(Fld, Object), Result).

system:goal_expansion(flds(Flds, Object), Result) :-
    flds_to_fld(Flds, Object, Result).

% expand the <type>_flds_set to use two objects instead of a recursive list
% throw an error if the field is not present in the template
flds_set([], O, O).
flds_set([F|T], Obj, Newer) :-
    fld_set(F, Obj, New) -> flds_set(T, New, Newer)
    ;
    Obj =.. [Name|_],
    throw(fld_error(F, Name, 'fld mapping not found for object')).

system:goal_expansion(Flds, (Object = Template, NewObject = SetTemplate)) :-
    Flds =.. [Name,List,Object,NewObject],
    atom(Name),
    atom_concat(FldType, '_flds_set', Name),
    fld_template(FldType, Template, blank_template),
    fld_template(FldType, SetTemplate, blank_template),
    flds_set(List, Template, SetTemplate).

% views - convert an fldv goal to a lookup for the type
% this allows two things, one is to have nicer looking code 
% by allowing for the following: 
% fldv(Obj, f1(_), f2(_), ...)
%
% But also allows a single inference to lookup data for multiple flds
%
:- dynamic fld_view/3.

flds_to_list_of_names([], []).
flds_to_list_of_names([H|T], [Name|R]) :-
	H =.. [Name,_],
	flds_to_list_of_names(T, R).
	
map_fld(Obj, Fld) :- fld(Fld, Obj).	
		
convert_types_to_view(FldstoSet, ViewNum, FldName) :-
	fld_template(FldName, Template, blank_template),
	maplist(map_fld(Template), FldstoSet),
	\+ fld_view(_, Template, FldstoSet) -> 
		assert(fld_view(ViewNum, Template, FldstoSet))
	; true.

expand_fld_view(A, fld:fld_view(ViewNum, Obj, FldstoSet)) :-
	A =.. [fldv,Obj|FldstoSet],
	flds_to_list_of_names(FldstoSet, FldNames),		
	findall(Name, (fld_object_def(Name,Flds), subset(FldNames, Flds)), Types),	
	gensym(fld_view, ViewNum),
	maplist(convert_types_to_view(FldstoSet, ViewNum), Types).	
	
system:goal_expansion(A, B) :-
	expand_fld_view(A, B).
	
	
% set views - convert an fldv goal to a set lookup for the type
% this is the same as the get version, but for sets
% The first param is the original object
% The second param is the updated object
% The remaining params are the fields to set
% eg: 
% fldv_set(Obj, SetObj, f1(_), f2(_), ...)
%
:- dynamic fld_set_view/4.
		
fld_values(Fld, Val) :- Fld =..	[_,Val].
		
convert_types_to_set_view(FldstoSet, ViewNum, FldName) :-
	fld_template(FldName, Template, blank_template),
    fld_template(FldName, SetTemplate, blank_template),
	flds_set(FldstoSet, Template, SetTemplate),
	\+ fld_set_view(_, Template, SetTemplate, FldstoSet) -> 
		assert(fld_set_view(ViewNum, Template, SetTemplate, FldstoSet))
	; true.

expand_fld_view_set(A, fld:fld_set_view(ViewNum, Obj, SetObj, FldstoSet)) :-
	A =.. [fldv_set,Obj,SetObj|FldstoSet],
	flds_to_list_of_names(FldstoSet, FldNames),
	findall(Name, (fld_object_def(Name,Flds), subset(FldNames, Flds)), Types),	
	gensym(fld_view, ViewNum),
	maplist(convert_types_to_set_view(FldstoSet, ViewNum), Types).	
	
system:goal_expansion(A, B) :-
	expand_fld_view_set(A, B).	
