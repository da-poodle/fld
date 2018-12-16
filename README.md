FLD 
===

A library for accessing and updating term arguments in Prolog.

Installation
============
To install the fld library, type the following in the SWI-Prolog shell:
```prolog
  ?- pack_install('https://github.com/da-poodle/fld.git').
  true.
```

Usage
=====
The library allows the definition of data that will be used in a program. To define a 'type' then use fld_generate/2. 

eg: To create a person object, the following could be added to prolog.
```prolog
:- fld_generate(person, [name, age, gender]).
```
fld uses the concept of 'business types' so in this case there are four type:
1. person
1. name
1. age
1. gender 

a person is a composite type that contains the other three. 

This will define all the predicates that are required for fld to operate. 

Predicate Reference
===================
## fld_object/2
Define a term and the named fields that can be used with the fld library.
```prolog
:- fld_object(person, [name, age, gender]).
```
Further calls to fld_object/2 with the same parameters will unify with an existing fld_object. eg: 
```prolog
?- fld_object(person, F).
F = [name, age, gender].
```
Attempting to call fld_object/2 with a name and different parameters fails. eg:
```prolog
?- fld_object(person, [name,age,sex]).
false.
```

## fld/2
Access the value of terms argument by name.
```prolog
:- fld(name(N), person(greg, 32, male)).
N = greg.
```

## flds/2
Access several arguments by name in one call.
```prolog
:- fld([name(N),gender(G)], person(greg, 32, male)).
N = greg.
G = male.
```

## fld_set/3
Replace the value of a terms argument by name.
```prolog
:- fld_set(name(frank), person(greg, 32, male), P).
P = person(frank, 32, male).
```

## flds_set/3
Replace several term arguments by name.
```prolog
:- flds_set([name(frank), age(25)], person(greg, 32, male), P).
P = person(frank,25,male).
```
## fld_template/2
This predicate two uses.

*Use 1:* generate a blank version of a term by name:
```prolog
?- fld_template(person, P).
F = person(_944, _950, _956).
```
*Use 2:* test if a term matches the template for a named type:
```prolog
?- fld_template(person, person(mary, 25, female)).
true.
```
## fld_destroy/1
Destroy and fld object so it can no longer be used. Not highly useful as object should be created for the duration of a program, however it is needed for unit testing and maybe specific scenarios.
```prolog
?- fld(name(N), person(greg, 35, male)).
N = greg.

?- fld_destroy(person).
true.

?- fld(name(N), person(greg, 35, male)).
false.
```
fld_destroy/1 will always succeed.