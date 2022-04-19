:- use_module('../prolog/fld').

:- fld_object(alphabetical, [
    a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
]).

run :-
    fld_template(alphabetical, Template),
    run(Template).

run(Words) :-
    write('enter a word, followed by a full stop: '),
    read(Word),
    atom_chars(Word, [C|_]),
    Setter =.. [C,Word],
    fld_set(Setter, Words, Words1),
    write_words(Words1),
    run(Words1).

write_words(Words) :-
    forall(fld(F, Words), 
        (ground(F) -> writeln(F) ; true)
    ).