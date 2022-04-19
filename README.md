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
?- fld_template(person, P).
P = person(_111, _112, _113).
```

To set a field, use ``fld_set/3``, to set multiple fields use ``flds_set/3``.

```prolog
?- fld_set(name('Fred'), $P, P1).
P1 = person('Fred', _112, _113).

?- flds_set([age(32), gender(male)], $P1, P2).
P2 = person('Fred', 32, male). 
```

To get the current value of a field, use ``fld/2``, to get multiple fields use ``flds/2``.

```prolog
?- fld(name(Name), $P2).
Name = 'Fred'.

?- flds([name(Name), age(Age)], $P2).
Name = 'Fred,
Age = 32.
```

API Reference
===================
- [fld_object/2](#fld_object/2)
- [fld/2](#fld/2)
- [flds/2](#flds/2)
- [fld_set/3](#fld_set/3)
- [flds_set/3](#flds_set/3)
- [fld_template/2](#fld_template/2)
- [fld_fields/2](#fld_fields/2)

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
over_one_thousand(Tran) :- fld(amount(Amount), Tran), Amount > 1000.

% load and filter the data
load_and_filter :-
    read_bank_records(Data),
    include(over_one_thousand, Data, ExpensiveRecords),
    maplist(print_record, ExpensiveRecords).

print_record(Tran) :-
    flds([date(Date), description(Desc), amount(Amount)], Tran),
    format('~w $~w - ~w~n', [Date, Amount, Desc]).
```

## Example 2 - Using multiple types in the same code
It is possible to use multiple object types in the same code if they share fields.
```prolog
:- use_module(library(fld)).

:- fld_object(car, [owner, model, registration, expires, year_of_make, body_type]).
:- fld_object(bus, [company, model, registration, expires, year_of_make]).

registration_valid(RoadUser) :-
  flds([registration(Rego), expires(Expires)], RoadUser),
  get_time(Time),
  (   Time < Expires -> true
  ; format('Rego ~p is expired~n', Rego)).


test :-
    fld_template(car, C),
    flds([registration('ABC-123'), expires(1544947900.570324)], C),
    registration_valid(C).

test :-
    fld_template(bus, B),
    flds([registration('DA-BUS01'), expires(1544947900.570324)], B),
    registration_valid(B).
```
