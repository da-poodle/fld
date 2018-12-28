FLD 
===

A library for accessing and updating term arguments in Prolog in a position independent way. 

Installation
============
To install the fld library, type the following in the SWI-Prolog shell:
```prolog
  ?- pack_install('https://github.com/da-poodle/fld.git').
  true.
```

Usage
=====
The library allows the definition of data that will be used in a program. To define a 'type' then use fld_object/2. 

eg: To create a person object, the following could be added to prolog.
```prolog
:- fld_object(person, [name, age, gender]).
```
fld uses the concept of 'business types' so in this case there are four types:
1. person
1. name
1. age
1. gender 

a person is a composite type that contains the other three. The names represent a specific type of data and 
should be used consistently throughout a program. Name for example might not only relate to people, but the validation etc.. should be the same for all types that use it.

Note that this is similar to library(record) except that specific names are not used for each object type. There are
a few reasons for this: 
* Specific names are hard to remember and hard to read
* Multiple types may share fields with the same names. if they do then predicates can be written to handle multiple types. 
* Validation can then be done at a type level rather than an object level which brings consistency in large programs. 


API Reference
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

## fld_template/3
As per fld_template/2 but you can specify a goal that will be used for defaults. The goal has two parameters:
 1. the name of the field that the default is for. 
 1. the default value for the field. 

> Note: If multiple defaults are specified for a field, then only the first will be used.

## fld_destroy/1
Destroy and fld object so it can no longer be used. Not highly useful as objects should be created for the duration of a program, however it is needed for unit testing and maybe specific scenarios.
```prolog
?- fld(name(N), person(greg, 35, male)).
N = greg.

?- fld_destroy(person).
true.

?- fld(name(N), person(greg, 35, male)).
false.
```
fld_destroy/1 will always succeed.

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
