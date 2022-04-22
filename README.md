FLD 
===
A library for accessing and updating term arguments in Prolog in a position independent way. 

Installation
============
To install the fld library, type the following in the SWI-Prolog shell:
```prolog
  ?- pack_install('fld').
  true.
```

Quick Start
=====
The fld library allows the definition of term templates that can be efficiently accessed at the argument level. To define a 'type' then use fld_object/2. Terms are stored as plain terms, so that calls to the CSV and ODBC APIs can use the object mechanism.  

eg: To create a person object, the following could be added to a prolog file.
```prolog
:- use_module(library(fld)).

:- fld_object(person, [name, age, gender]).
```

This will create accessors for a ``person/3`` term. 

To create a blank term, use the ``fld_template/2``.
```prolog
?- fld P:person.
P = person(_111, _112, _113).
```

To set fields:

```prolog
?- fld P1:name('Fred')-$P.
P1 = person('Fred', _112, _113).

?- fld P2:[age(32), gender(male)]-$P1.
P2 = person('Fred', 32, male). 
```

To get the current values of fields:

```prolog
?- fld $P2:name(Name).
Name = 'Fred'.

?- fld $P2:[name(Name), age(Age)].
Name = 'Fred,
Age = 32.
```

API Reference
===================

## fld_object/2
Define a term and the named fields that can be used with the fld library.

This is a directive that is specified at compile time.
```prolog
:- fld_object(person, [name, age, gender]).
```
This will create a set of fld/2 and fld_set/3 predicates for the 'person' object, that can be used to access each field. 

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

## fld/1

``fld/1`` is an expanded term in which Spec has a different function according to the values provided. 
The following is possible:
 - check fld type
 - extract value(s) from fld type
 - replace value(s) in fld type

The examples use ``:- fld_object(t,[a,b,c]).`` as an example type specification.

---
*Templates, Type Checking*

```prolog
fld T:t.
```

T is a template of the spec for type t. This can be used to either create a blank term or type t
or check the a variable is of type t.

Expands to 
```prolog
T = t(_,_,_).
```
---
*Get Single Value*
```prolog
fld T:a(A).
```
Unify A with the a argument in type T.

If  is unique to the type t, then expansion will be:
```prolog
T = t(A,_,_).
```
If a is not unque to type t, then expansion will be:
```prolog
flds([a(A)],T).
```
---
*Get Multiple Values*
```prolog
fld T:[a(A),b(B)].
```
Unify A and B with the a,b arguments in type T.

If the A,B combination is unique to the type t, then expansion will be:
```prolog
T = t(A,B,_).
```
If the combination is not unque to type t, then expansion will be:
```prolog
flds([a(A),b(B)],T).
```
---
*Set Single Value*
```prolog
fld T1:a(A)-T.
```
Unify A with the value of the a argument in T1, T is T1 without A.
This is used for replacing fields from T in T1.

If A is unique to type t, then expand to
```prolog
T = t(_,B,C), T1 = (X,B,C).
```
Otherwise expand to
```prolog
flds_set([c(X)], T, T1).
```

*Set Multiple Values*
```prolog
fld T1:[a(A),b(B)]-T.
```
Unify A,B with the value of the a,b arguments in T1, T is T1 without A,B.
This is used for replacing fields from T in T1.

If the a,b combination is unique to type t, then expand to
```prolog
T = t(_,_,C), T1 = (A,B,C).
```
Otherwise expand to
```prolog
flds_set([a(A),b(B)], T, T1).
```

## fld/2
Access a single argument in an fld term by name.

eg:
```prolog
?- fld(name(N), person(greg, 32, male)).
N = greg.
```

fld/2 (and flds/2) can be used to set the values of an object as well if the object is uninstantiated. eg:
```prolog
?- fld_template(person, P), fld(name(henry), P).
P = person(henry, _, _).
```

## flds/2
Access several arguments by name in one call.
```prolog
?- fld([name(N),gender(G)], person(greg, 32, male)).
N = greg.
G = male.
```

Flds/2 will be expanded so that only one call is made to access all the arguments, unless there are two objects that share the same set of argument names. For example, consider the following objects:

```prolog
:- fld_object(o1, [a,b,c]).
:- fld_object(o2, [b,c,d]).
```

If ``flds/2`` is called for a unique set of fields for ``o1`` then the following code is created:

```prolog
?- O1 = o1(a, b, c), expand_term(flds([a(A), b(B)], O1), ExpandedTerm).
ExpandedTerm = O1 = o1(A, B, _).
```

But if ``flds/2`` is created with a non unique set, then a more generic call is made. 

```prolog
?- O1 = a1(a, b, c), expand_term(flds([b(B), c(C)], O1), ExpandedTerm).
ExpandedTerm = flds([b(B), c(C)], O1).
```
This expansion is done at compile time, to allow a single inference to access multiple arguments, and by listing the source, the transformation can be seen.

## fld_set/3
Replace the value of a single argument in an fld object by name.
```prolog
?- fld_set(name(frank), person(greg, 32, male), P).
P = person(frank, 32, male).
```

## flds_set/3
Replace several term arguments by name.
```prolog
?- flds_set([name(frank), age(25)], person(greg, 32, male), P).
P = person(frank,25,male).
```
This has similar expansion rules as ``flds/2`` with regards to uniqueness. The code produced is different and for a unique set of fields there is a garenteed 2 inferences for all fields. 

## fld_template/2
This predicate has two uses.

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
When creating a new template a default mechanism is used to set values based on name. By defaul the fld:fld_default/2 predicate is used. For example
```prolog
?- assert(fld:fld_default(gender, unspecified)).
true.

?- fld_template(person, P).
P = person(_944, _950, unspecified).
```
This is the same call as <code>fld_template(person, P, fld:fld_default).</code>

## fld_fields/2
The first parameter is an fld object, the second is a list of named terms. 
This predicate only works one way, to convert an existing fld object to a list of terms.

For example:

```prolog
:- fld_object(person, [name, age, gender]).

?- fld_fields(person('Fred', 32, male), Fields).
Fields = [name('Fred'), age(32), gender(male)].
```

Examples
========
## Example 1 - Reading from CSV files
A CSV file for bank records needs to be read which has the following headings:
```csv
transaction_id,date,description,amount,balance
```
The task is to find any transactions that are over $1000

In this case fld can be used to map the incoming data more easily, 
especially if the number or order of the data changes. 
```prolog
:- use_module(library(fld)).

% Create an fld_object for the headers
:- fld_object(transaction, [transaction_id, date, description, amount, balance]).

% Read in the CSV file with the functor name as the fld_object name and arity of 5.
% This will get a list of data which matches the
% fld_object(transaction,_).
read_bank_records(Data) :-
  csv_read_file('bank_records.csv', Data, [functor(transaction), arity(5)]).

% create a filter that can get the records over $1000
over_one_thousand(Tran) :- fld Tran:amount(Amount), Amount > 1000.

% load and filter the data
load_and_filter :-
    read_bank_records(Data),
    include(over_one_thousand, Data, ExpensiveRecords),
    maplist(print_record, ExpensiveRecords).

print_record(Tran) :-
    fld Tran:[date(Date), description(Desc), amount(Amount)],
    format('~w $~w - ~w~n', [Date, Amount, Desc]).
```

## Example 2 - Using multiple types in the same code
It is possible to use multiple object types in the same code if they share fields.
```prolog
:- use_module(library(fld)).

:- fld_object(car, [owner, model, registration, expires, year_of_make, body_type]).
:- fld_object(bus, [company, model, registration, expires, year_of_make]).

registration_valid(RoadUser) :-
  fld RoadUser:[registration(Rego), expires(Expires)],
  get_time(Time),
  (   Time < Expires -> true
  ; format('Rego ~p is expired~n', Rego)).

test :-
    fld C:car,
    fld C:[registration('ABC-123'), expires(1544947900.570324)],
    registration_valid(C).

test :-
    fld B:bus,
    fld B:[registration('DA-BUS01'), expires(1544947900.570324)],
    registration_valid(B).
```
