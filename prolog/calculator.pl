:- use_module(library(dcg/basics)).

% Variable store
:- dynamic var/2.

% Tokenization
tokenize(Input, Tokens) :-
    string_codes(Input, Codes),
    phrase(tokens(Tokens), Codes).

tokens([]) --> [].
tokens([Token|Tokens]) -->
    white,
    (   number(Token)
    ;   identifier(Token)
    ;   operator(Token)
    ),
    !,
    tokens(Tokens).

% Tokenize a number
number(num(N)) -->
    integer(I),
    (   "."
    ->  fraction(F),
        { N is I + F }
    ;   { N = I }
    ).

fraction(F) -->
    digits(D),
    { length(D, L),
      divider(L, Div),
      numlist(D, N),
      F is N / Div
    }.

divider(L, D) :- D is 10^L.

numlist([], 0).
numlist([D|Ds], N) :-
    numlist(Ds, N1),
    N is N1 * 10 + D.

% Tokenize an identifier
identifier(id(Id)) -->
    letter(H),
    identifier_chars(T),
    { atom_codes(Id, [H|T]) }.

identifier_chars([H|T]) -->
    (   letter(H)
    ;   digit(H)
    ;   [0'_]
    ),
    !,
    identifier_chars(T).
identifier_chars([]) --> [].

letter(C) --> [C], { code_type(C, alpha) }.
digit(C) --> [C], { code_type(C, digit) }.

% Tokenize operators
operator(plus) --> "+".
operator(minus) --> "-".
operator(multiply) --> "*".
operator(divide) --> "/".
operator(equals) --> "=".
operator(lparen) --> "(".
operator(rparen) --> ")".

% Parse tokens into an expression
parse(Tokens, Expr) :-
    phrase(expression(Expr), Tokens, []).

% Grammar for expressions
expression(E) --> term(T), expression_rest(T, E).

expression_rest(T, E) -->
    (   [plus], term(T2), { E1 is T + T2 }, expression_rest(E1, E)
    ;   [minus], term(T2), { E1 is T - T2 }, expression_rest(E1, E)
    ;   { E = T }
    ).

term(T) --> factor(F), term_rest(F, T).

term_rest(F, T) -->
    (   [multiply], factor(F2), { T1 is F * F2 }, term_rest(T1, T)
    ;   [divide], factor(F2),
        (   { F2 =\= 0 }
        ->  { T1 is F / F2 }, term_rest(T1, T)
        ;   { throw(error(division_by_zero, _)) }
        )
    ;   { T = F }
    ).

factor(F) -->
    (   [num(N)], { F = N }
    ;   [id(Id)],
        (   { var(Id, Value) }
        ->  { F = Value }
        ;   { throw(error(undefined_variable(Id), _)) }
        )
    ;   [lparen], expression(E), [rparen], { F = E }
    ).

% Parsing with assignments
parse_with_assignment(Tokens, Result) :-
    (   Tokens = [id(Var), equals | Rest]
    ->  parse(Rest, Value),
        retractall(var(Var, _)),
        assertz(var(Var, Value)),
        Result = Value
    ;   parse(Tokens, Result)
    ).

% Main interpreter
run_interpreter :-
    writeln('Prolog Calculator Interpreter'),
    writeln('Enter expressions or "quit"/"exit" to quit'),
    interpreter_loop.

interpreter_loop :-
    write('> '),
    read_line_to_string(user_input, Input),
    (   member(Input, ["quit", "exit"])
    ->  writeln('Goodbye!')
    ;   Input = ""
    ->  interpreter_loop
    ;   catch(
            (   tokenize(Input, Tokens),
                parse_with_assignment(Tokens, Result),
                writeln(Result)
            ),
            error(E, _),
            handle_error(E)
        ),
        interpreter_loop
    ).

handle_error(division_by_zero) :-
    writeln('Error: Division by zero').
handle_error(undefined_variable(Var)) :-
    format('Error: Undefined variable "~w"~n', [Var]).
handle_error(E) :-
    format('Error: ~w~n', [E]).

% Start the interpreter
:- initialization(run_interpreter). 