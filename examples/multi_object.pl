:- use_module('../prolog/fld').

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