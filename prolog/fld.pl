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
			  fld/2,
			  fld_set/3,
              fld_template/2,
              fld_template/3,
              fld_fields/2]).

:- meta_predicate fld:fld_template(*,*,2).

:- discontiguous(fld:fld_object_def/2).
:- multifile(fld_object_def/2).

%! fld(?Field:term, ?Object:term) is det.
% Field is a argument in an object.
:- discontiguous(fld/2).
:- multifile(fld/2).

%! fld_set(?Field:term, ?Old:term, ?New:term) is nondet.
% New is the old term with field updated.
:- discontiguous(fld_set/3).
:- multifile(fld_set/3).

%! fld_default(+Field:atom, ?Default:term) is semidet.
% A default is defined by the user, if no default is used then an uninstantiated variable will be used.
:- multifile(fld_default/2).

%! fld_template(?Name:atom, ?Template:list) is nondet.
% template is an object with all fields as uninstaniated variables.
% defaults are taken from the fld:fld_default/2 predicates.
fld_template(Name, Template) :-
    fld_template(Name, Template, fld_default).

% ! fld_template(?Name:atom, ?Template:list, ++Goal:callable) is nondet.
% template is an object with all fields as uninstaniated variables.
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

system:term_expansion(':-'(fld_object(Name, Flds)), [fld:fld_object_def(Name, Flds)|GetSet]) :-
	\+ fld_object_def(Name, Flds),
	length(Flds, Len),
	generate_flds(Flds, Name, Len, 0, GetSet).
		
	
%! fld_feilds(?Object:term, ?Fields:list) is semidet.
% return a list of all fields for object as terms instead of atoms.
fld_fields(Obj, Fields) :-

    Obj =.. [Name|Vals],
    fld_object(Name, Flds),

    maplist(fld_field_object,Flds,Vals,Fields).

fld_field_object(FldName,Value,Field) :- Field =.. [FldName,Value].




% expand the type specific goals to be efficient
% to do this look for a name of <type>_flds and expand this to use the 
% actual object rather than the fld lookup method
resolve_fld(Template, Getter) :- 
	fld(Getter, Template) -> true
	;	
	Template =.. [Name|_],
	throw(fld_error(Getter, Name, 'fld mapping not found for object')).	
		
system:goal_expansion(Flds, (Object = Template)) :-
	Flds =.. [Name,List,Object],
	atom(Name),
	atom_concat(FldType, '_flds', Name),
	fld_template(FldType, Template),
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
	fld_template(FldType, Template),
	fld_template(FldType, SetTemplate),
	flds_set(List, Template, SetTemplate).	
