 % Copyright (C) 2019 Andrei Olaru
 % 
 % Prolog-tester is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General
 % Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
 % 
 % Prolog-tester is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 % implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 % License for more details.
 % 
 % You should have received a copy of the GNU Lesser General Public License along with Prolog-tester.
 % If not, see <http://www.gnu.org/licenses/>.
 
 


:- discontiguous exercitiul/2.


%% -----------------------------------------------------------------------------
%% EXEMPLU
%% -----------------------------------------------------------------------------
%% Atenție la semnăturile predicatelor și la specificarea parametrilor
%% p(+Arg1, -Arg2, ?Arg3)
%%  * Arg1 va fi instanțiat atunci când se va încerca satisfacerea p/3
%%  * Arg2 se va instanția odată cu satisfacerea p/3
%%  * Arg3 va putea fi instanțiat sau nu atunci când se va satisface p/3:
%%
%% Interogați Prolog cu "checkX." pentru a verifica rezolvarea exercițiului X.
%% Pentru a vedea progresul general, trimiteți scopul "check".
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
exercitiul(1, [1, punct]).
%% myConcat/3
%% myConcat(?List1, ?List2, ?List)
%% 'List' este lista formată prin concatenarea listelor 'List1' și 'List2'.

myConcat([],L,L).
myConcat([A|B],C,[A|D]) :- myConcat(B, C, D).

check1:-
    tests([
        exp('myConcat([], [], L)', ['L', []]),
        exp('myConcat(L, [1,2], [1,2])', ['L', []]),
        exp('myConcat([1,2,3], L, [1,2,3,4,5])', ['L', [4, 5]]),
        chk(myConcat([a, b, c], [d], [a, b, c, d])),
        chk(myConcat([X1, X2], [X1, X2], [1, 2, 1, 2])),
        uck(myConcat([X1, X2], [X1, X2], [1, 2, 3, 4])),
        nsl('myConcat([_,_,_], [_], L)', 'L', 1),
        nsl('myConcat([_,_,_], L, [_,_,_,_])', 'L', 1),
        exp('myConcat([X51],[X52],[X53,X54])', [cond('X51 == X53'), v('X51'), cond('X52 == X54'), v('X54')])
    ]).

%% -----------------------------------------------------------------------------

exercitiul(2, [1, punct]).
%% myReverse/2
%% myReverse(?List, +RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă.
%% Regulile pot conține și predicatul myConcat/3.

myReverse([A,B],[B,A]).
myReverse([_, _], [1, 2]).
myReverse([X], [X]).

check2:-
    tests([
        chk(myReverse([], [])),
        chk(myReverse([1,2,3], [3,2,1])),
        exp('myReverse([1,2,3], Rev)', ['Rev', [3,2,1]]),
        exp('myReverse(List, [3,2,1])', ['List', [1,2,3]]),
        exp('myReverse([1,X2,X3], [3,2,X1])', ['X1', 1, 'X2', 2, 'X3', 3]),
        exp('myReverse([Y1,Y2], L)', [cond('L == [Y2, Y1]'), v('Y1'), v('Y2')]),
        exp('myReverse(L, [Z])', [cond('L == [Z]'), v('Z')]),
        nsl('myReverse([_,_], X)', 'X', 1)
        ]).

%% -----------------------------------------------------------------------------
exercitiul(3, [0.5, puncte]).
%% myReverseAcc/2
%% myReverseAcc(?List, ?Acc, ?RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă
%% și elementele listei 'Acc'.
%% (Indicație: 'Acc' se va comporta precum un acumulator)
%% Regulile nu trebuie să conține alte predicate (în afară de "cut" și ",").

myReverseAcc(_A, _B, []).

check3:-
    tests([
        1, chk(myReverseAcc([], [], [])),
        1, chk(myReverseAcc([1,2,3], [0], [3,2,1,0])),
        2, exp('myReverseAcc([1,2,3], [0], Rev)', ['Rev', [3,2,1,0]]),
        2, exp('myReverseAcc(List, [0], [3,2,1,0])', ['List', [1,2,3]]),
        2, exp('myReverseAcc([X2,1], [3], [X1,2,3])', ['X1', 1, 'X2', 2])
    ]).

%% -----------------------------------------------------------------------------
exercitiul(13, [1, punct, bonus]).
%% descendants/2
%% descendants(?Nod, ?N)
%% Nodul Nod are N urmași.

descendants(X, Y) :- d(X, Y).
d(a,6).
d(o, 1).
d(a, 1).
d(b, 1).
d(e, 1).
d(l, 0).
d(n, 2).

check13 :- tests([
               chk(descendants(a, 6)),
               chk(descendants(b, 1)),
               exp('descendants(a, N1)', ['N1', 6]),
               chk(descendants(c, 3)),
               chk(descendants(h, 4)),
               chk(descendants(g, 0)),
               chk(descendants(l, 0)),
               sls('descendants(X, 1)', 'X', [b,e,i,j,o]),
               sls('descendants(X, 2)', 'X', [n])
           ]).



%% ----------------------------------------
%% ----------------------------------------

testtimelimit(5). % în secunde

test_mode(quickcheck) :- \+ test_mode(vmchecker).

:-dynamic(punct/2).
:-dynamic(current/1).
%:-clean.

clean :- retractall(punct(_, _)), retractall(current(_)).

testtest(5).
testtest(6).
testtest(9).
testtest(a, 5).
testtest(b, _).
testtest(d, [2, 1, 3]).
testtest(e, [2, 1, 3, 1, 2]).
testtest(1, 2, 3).
testtest(c, X, X).
testtest(d, 5, excepting) :- X is 5 / 0, write(X).
testtest(d, 5, limitless) :-  testtest(d, 5, limitless).

testtest :- tests([
               % chk(P) ("check") verifică evaluarea cu succes a lui P
               chk(testtest(5)), % a
               chk(testtest(6)), % b % eșuează
               % uck(P) ("uncheck") verifică evaluarea cu eșec a lui P
               uck(testtest(6)), % c
               uck(testtest(5)), % d % eșuează
               % exp(P, Exps) ("expect") verifică evaluarea cu succes a lui P și a unor condiții
               % P este dat ca șir de caractere pentru o verificare și afișare mai bune.
               % Condițiile sunt evaluate pe rând și independent unele de altele.
               % Dacă în lista de condiții avem un nume de variabilă,
               % se verifică că aceasta a fost legat la valoarea care urmează imediat în listă.
               % valoarea de verificat nu poate conține variabile din interogare
               % (pentru asta folosiți cond, vezi mai jos).
               exp('testtest(X, X)', ['X', b]), % e
               exp('testtest(X, Y, Z)', ['X', 1, 'Y', 2, 'Z', 3]), % f
               exp('testtest(X, X)', ['X', a]), % g % eșuează
               % Dacă în lista de condiții avem v('Var'), se verifică că Var a rămas nelegată.
               exp('testtest(b, Y)', [v('Y')]), % h
               exp('testtest(a, Y)', [v('Y')]), % i % eșuează
               % Dacă în lista de condiții avem cond('P'), se verifică că P este adevărat.
               % Diversele condiții din structuri cond diferite se verifică independent.
               exp('testtest(c, X, X)', [v('X'), cond('X==X')]), % j
               % Dacă în lista de condiții avem set('V', Set), se verifică că V este legată la mulțimea Set.
               % Duplicatele contează.
               exp('testtest(d, L)', [set('L', [1, 2, 3]), 'L', [1, 2, 3]]), % k % eșuează pe a 2a condiție
               exp('testtest(d, L)', [set('L', [2, 3, 4, 5])]), % l
               exp('testtest(e, L)', [set('L', [1, 2, 3])]), % m
               % setU funcționează la fel, dar ignoră duplicatele.
               exp('testtest(e, L)', [setU('L', [1, 2, 3])]), % n
               % ckA(P, Argss) ("check all") verifică evaluarea cu succes a lui P pe fiecare dintre variantele de argumente din Argss.
               % e.g. dacă P este pred, iar Argss este [(1, a, []), (2, b, [5])],
               % se vor evalua atomii pred(1, a, []) și pred(2, b, [5]).
               ckA('testtest', [(a, 5), (b, 1), (b, 3), (b, 4)]), %o
               ckA('testtest', [(a, 5), (c, 1), (c, 3), (b, 4)]), %p
               % ech(P, Conds) ("each") verifică dacă condițiile țin pentru fiecare dintre soluțiile lui P
               ech('testtest(X)', ['X < 10', 'X > 3']), % q
               ech('testtest(X)', ['X > 3', 'X > 5']), % r
               % nsl(P, Template, NSols) ("N Solutions") verifică dacă lungimea lui L din findall(P, Template, L) este NSols
               2, nsl('testtest(X, Y)', '(X, Y)', 4), % s
               % testul de mai sus valorează de 2 ori mai mult decât celelalte

               % sls(P, Template, Sols) ("Solutions") verifică dacă findall(P, Template, L) leagă L la aceeași mulțime cu Sols.
               % Duplicatele contează
               2, sls('testtest(X, X)', '(X, X)', [(b, b)]), % t
               % testul de mai sus valorează de 2 ori mai mult decât celelalte

               sls('testtest(X, Y)', '(X, Y)', [(b, 5)]), % u
               % gestionare exceptii
               chk(testtest(d, 5, excepting)),
               % gestionare limita de timp
               chk(testtest(d, 5, limitless))
           ]).

% aceasta va fi ordinea de testare
vmpoints(1, 5).
vmpoints(T, 2.5) :- member(T, ['2a', '2b']).

% entry point (for users) for vm tests.
vmtest(T) :-
        vmtest(T, Score),
        format('Total: ~w.', [Score]).

% performes a vm test, outputs score.
vmtest(T, Score) :-
        once(vmpoints(T, Pts)),
        tt(T, TestList),
        tests(TestList, Pts, T, Score).

tt(1, [
       chk(testtest(5)),
       chk(testtest(6)),
       uck(testtest(6)),
       uck(testtest(5))
       ]).
tt('2a', [
       exp('testtest(X, X)', ['X', b])
   ]).
tt('2b', [
       exp('testtest(X, Y, Z)', ['X', 1, 'Y', 2, 'Z', 3]),
       exp('testtest(X, X)', ['X', a])
   ]).


% entry point for quick check; handles checking all exercises or just
% one.
tests(Tests) :- (   current(_), ! ; retractall(punct(_, _))),
        (   current(Ex), !, exercitiul(Ex, [Pts | _]), Total is Pts
        ;   Total is 100
        ),
        tests(Tests, Total, none, Score),
        (   current(Ex), assert(punct(Ex, Score)), !
        ;   format('Rezolvat ~0f%.~n', [Score])
        ), !.
tests(_) :- failure(unknown, 'INTERN: tests/1 failed').

tests(Tests, TotalPoints, Ex, Score) :-
    total_units(Tests, TF),
    Unit is TotalPoints / TF,
    tests(Tests, Ex, 1, Unit, 0, Score), !.
tests(_, _, Ex, _) :- failure(Ex, 'INTERN: tests/4 failed').

% iterates through tests, handles test index, generates test id, adds
% points
tests([], _, _, _, Points, Points) :- !.
tests([Fraction, T|R], Ex, Idx, Unit, PointsIn, PointsOut) :-
        number(Fraction), !, test(T, Ex, Idx, Fraction, Unit, PointsIn, PointsOut1),
        tests(R, Ex, Idx+1, Unit, PointsOut1, PointsOut).
tests([T|R], Ex, Idx, Unit, PointsIn, PointsOut) :-
        test(T, Ex, Idx, 1, Unit, PointsIn, PointsOut1),
        tests(R, Ex, Idx+1, Unit, PointsOut1, PointsOut).
tests(_, Ex, _, _, _, _) :- failure(Ex, 'INTERN: tests/6 failed').

total_units([], 0).
total_units([P, _|R], Tot) :- number(P), !, total_units(R, TotR), Tot is TotR + P.
total_units([_|R], Tot) :- total_units(R, TotR), Tot is TotR + 1.

test(T, NEx, Idx, Fraction, Unit, PointsIn, PointsOut) :-
        IdxI is Idx + 96, char_code(CEx, IdxI),
        (   NEx == none, !, swritef(Ex, '%w|', [CEx]) ; swritef(Ex, '[%w|%w]', [NEx, CEx])),
        testtimelimit(Time), swritef(MTime, 'limita de %w secunde depasita', [Time]),
        (   catch(
                catch(call_with_time_limit(Time, once(test(Ex, T))),
                      time_limit_exceeded,
                      except(Ex, MTime)
                     ),
                Expt,
                (   swritef(M, 'exceptie: %w', [Expt]),
                except(Ex, M))
            ),
            !, success(Ex, Fraction, Unit, Points),
            PointsOut is PointsIn + Points
        ; PointsOut = PointsIn).
test(_, Ex, Idx, _, _, _, _) :- failure(Ex/Idx, 'INTERN: test/7 failed').

success(Ex, Fraction, Unit, Score) :-
    Score is Fraction * Unit,
    (   test_mode(vmchecker), !,
        format('+~2f ~10t ~w Corect.~n', [Score, Ex])
    ;   format('~w[OK] Corect. +~2f.~n', [Ex, Score])).
failure(Ex, M) :-
        (   test_mode(vmchecker), !,
            format('+0.0 ~10t  ~w ~w~n', [Ex, M]), fail
        ;   format('~w[--] ~w~n', [Ex, M]), fail).
except(Ex, M) :-
        (   test_mode(vmchecker), !,
            format('+0.0 ~10t ~w Exception: ~w~n', [Ex, M]), fail
        ;   format('~w[/-] ~w~n', [Ex, M]), fail).

test(Ex, chk(P)) :- !, testCall(Ex, P).
test(Ex, uck(P)) :- !, testCall(Ex, \+ P).
test(Ex, exp(Text, ExpList)) :- !,
    read_term_from_atom(Text, P, [variable_names(Vars)]),
    testCall(Ex, P, Text), testExp(Ex, Text, Vars, ExpList).
test(_, ckA(_, [])) :- !.
test(Ex, ckA(Pred, [Test|Tests])) :- !,
    swritef(S, '%w(%w)', [Pred, Test]),
    read_term_from_atom(S, P, []),
    testCall(Ex, P, S), test(Ex, ckA(Pred, Tests)).
test(_, ech(_, [])) :- !.
test(Ex, ech(Text, [Cond|Conds])) :- !,
    swritef(S, '%w|%w', [Text, Cond]),
    read_term_from_atom(S, P|Q, [variable_names(Vars)]),
    forall(P, (
               swritef(Msg, '%w pentru soluția %w a predicatului %w', [Cond, Vars, Text]),
               testCall(Ex, Q, Msg))),
    test(Ex, ech(Text, Conds)).
test(Ex, nsl(Text, Tmplt, N)) :- !,
    swritef(S, 'findall(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testNSols(Ex, Text, Vars, N).
test(Ex, sls(Text, Tmplt, Sols)) :- !,
    swritef(S, 'findall(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testSols(Ex, Text, Vars, Sols).
test(Ex, sSO(Text, Tmplt, Sols)) :- !,
    swritef(S, 'setof(%w, %w, TheList)', [Tmplt, Text]),
    read_term_from_atom(S, P, [variable_names(Vars)]),
    testCall(Ex, P, S), testSols(Ex, Text, Vars, Sols).
test(Ex, _) :- failure(Ex, 'INTERN: Test necunoscut').

% Pentru exercițiul Ex, evaluează clauza Do, dată ca termen.
% Opțional, în mesajul de eroare interogarea poate fi afișată ca
% parametrul Text.
testCall(Ex, Do) :- swritef(Text, '%q', [Do]), testCall(Ex, Do, Text).
testCall(Ex, Do, Text) :-
        catch((call(Do), !
              ;   !, swritef(M, 'Interogarea %w a esuat.', [Text]), failure(Ex, M)
              ), Exc,
              (swritef(M, 'Interogarea %w a produs exceptie: %w', [Text, Exc]),
              except(Ex, M))
             ).

testExp(_, _, _, []) :- !.
testExp(Ex, Text, Vars, [v(Var) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        (   var(V), !, testExp(Ex, Text, Vars, Rest) ;
            swritef(M, 'Interogarea %w leaga %w (la valoarea %w) dar nu ar fi trebuit legata.',
                    [Text, Var, V]), failure(Ex, M)
        )
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [set(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSet(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [setU(Var, Set) | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        testSetU(Ex, Text, 'intoarce', V, Set),
        testExp(Ex, Text, Vars, Rest)
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [cond(Cond) | Rest]) :- !,
    swritef(S, "(%w, %w)", [Text, Cond]),
    read_term_from_atom(S, P, []),
    (
        call(P), !, testExp(Ex, Text, Vars, Rest)
        ;
        swritef(M, 'Dupa interogarea %w conditia %w nu este adevarata.', [Text, Cond]),
        failure(Ex, M)
    ).
testExp(Ex, Text, Vars, [Var, Val | Rest]) :- !,
    (   getVal(Var, Vars, V), !,
        (   V == Val, !, testExp(Ex, Text, Vars, Rest) ;
            swritef(M, 'Interogarea %w leaga %w la %w in loc de %w.',
                    [Text, Var, V, Val]), failure(Ex, M)
        )
    ;
    swritef(M, 'INTERN: Interogarea %w nu contine variabila %w.', [Text, Var]),
    failure(Ex, M)
    ).
testExp(Ex, _, _, [X | _]) :- !,
        swritef(M, 'INTERN: element necunoscut pentru exp: %w', [X]),
        failure(Ex, M).
testExp(Ex, _, _, X) :- !,
        swritef(M, 'INTERN: format gresit pentru exp: %w', [X]),
        failure(Ex, M).

testNSols(Ex, Text, Vars, N) :-
    (   getVal('TheList', Vars, V), length(V, NSols), !,
        (   NSols =:= N, !
        ;   swritef(M, 'Numarul de solutii pentru %w este %w in loc de %w.',
                    [Text, NSols, N]), failure(Ex, M)
        )
    ;   failure(Ex, 'INTERNAL: nu avem variabila TheList sau aceasta nu este lista.')
    ).

testSols(Ex, Text, Vars, Sols) :-
    (   getVal('TheList', Vars, V), !,
        testSet(Ex, Text, 'are ca solutii', V, Sols)
    ;   failure(Ex, 'INTERNAL: nu avem variabila TheList sau aceasta nu este lista.')
    ).

testSetU(Ex, Text, TypeText, SetG, SetE) :- sort(SetG, SetGUnique),
    testSet(Ex, Text, TypeText, SetGUnique, SetE).
testSet(Ex, Text, TypeText, SetG, SetE) :- msort(SetG, SetGSorted),
    (   SetGSorted == SetE, ! ;
        testSetMinus(SetG, SetE, TooMuch),
        testSetMinus(SetE, SetG, TooLittle),
        (   TooMuch == [], TooLittle == [], !,
            M1 = 'vezi duplicate'
        ;   swritef(M1, '%w sunt in plus, %w lipsesc', [TooMuch, TooLittle])
        ),
        swritef(M,
                'Interogarea %w %w %w dar se astepta %w (%w)',
                [Text, TypeText, SetG, SetE, M1]), failure(Ex, M)
    ).

testSetMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).

getVal(Var, [Var=Val | _], Val) :- !.
getVal(Var, [_ | Vars], Val) :- getVal(Var, Vars, Val).

check :-
        clean,
        forall(exercitiul(Ex, _),
               (   atom_concat(check, Ex, Ck),
                   retractall(current(_)), assert(current(Ex)),
                   once(call(Ck)) ; true)),
        findall(P, punct(_, P), L),
        sum_list(L, S),
        format('Punctaj total: ~f~n',[S]),
        clean.

vmtest :- checkVm.
checkVm :-
        clean,
        findall(T:Score, (tt(T, _), vmtest(T, Score)), Results),
        findall(Score, member(_:Score, Results), Scores),
        sum_list(Scores, S),
        format('Total: ~w~n', [S]),
        clean.

