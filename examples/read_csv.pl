:- use_module('../prolog/fld').

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