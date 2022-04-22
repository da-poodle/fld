:- use_module('../prolog/fld').

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
