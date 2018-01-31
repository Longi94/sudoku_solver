:- use_module(library(lists)).

% :- type sspec ---> s(size, board).
% :- type size  == int.
% :- type field == list(info).
% :- type info ---> e; o; s; w; v(int).
% :- type board == list(list(field)).
% :- type col  == int.
% :- type row  == int.
% :- type coords --> row-col.
% :- type matrix == list(list(any)).
% :- type parameter ---> row-col.

% :- type ssol == list(list(int)).

% sudoku(SSpec, SSol):
% SSol az SSpec feladv�nyt kiel�g�t� megold�s.
% :- pred sudoku(sspec::in, ssol::out).
sudoku(s(K, S), SSol) :- 
    values_mtx(s(K, S), VM),
    X is K * K,
    list_of_lists(X, GC),
    mtx_nth(VM, X-X, Vs), !,
    generate(s(K, S), X-X, VM, [Vs], [], GC, SSols),
    member(SSol, SSols).
    
% :- pred generate(+sspec, +coords, +list(list(list(integer))), 
%         +list(list(integer)), +list(list(integer)), -list(ssol)).
% generate(SSpec, R_C, VM, P, G, GC, SSols): SSols az SSpec feladv�nyt
% kiel�g�t� megold�sok list�ja. R_C koordin�t�j� mez� az ahol a
% gener�l�sn�l tartunk. VM egy list�k m�trixa, az SSpec szrinti mez�kbe
% �rhat� lehets�ges �rt�keket tartalmazza. P a cell�kba m�g be�rhat�
% sz�mokat, tartalmazza a keres�si f�ban val� visszal�p�shez. G a
% gener�lt sudoku megold�s aktu�lis �llapota. GC az gener�lt sudoku
% megold�s feldarabolja.

% Kil�p�si felt�tel.
generate(_, _, _, [[]], _, _, []).

% Tal�ltunk egy megold�st.
generate(s(K, S), 1-1, VM, [[PHH|PHT]|PT], [GH|GT], GC, [SSol|SSols]) :-
    unwrap([[[v(PHH)]|GH]|GT], SSol), !,
    generate(s(K, S), 1-1, VM, [PHT|PT], [GH|GT], GC, SSols).

% Visszal�p�s a keres�si f�ban.
generate(s(K, S), R-C, VM, [[],[_|PT0]|PT], [[_]|GT], GC, SSols) :-
    C1 is C + 1,
    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    block_index(K, R-C1, BI),
    remove_from_nth(GC, BI, GC1), !,
    % Eldobjuk a be�rt sz�mot (PH feje)
    generate(s(K, S), R-C1, VM, [PT0|PT], GT, GC1, SSols).

% Visszal�p�s a keres�si f�ban.
generate(s(K, S), R-C, VM, [[],[_|PT0]|PT], [[_|GHT]|GT], GC, SSols) :-
    \+ GHT = [],
    (
        C =:= K * K -> 
            R1 is R + 1, C1 = 1,
            % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
            block_index(K, R1-1, BI),
            remove_from_nth(GC, BI, GC1);
        R1 = R, C1 is C + 1,
        % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
        block_index(K, R-C1, BI),
        remove_from_nth(GC, BI, GC1)
    ), !,
    % Eldobjuk a be�rt sz�mot (PH feje)
    generate(s(K, S), R1-C1, VM, [PT0|PT], [GHT|GT], GC1, SSols).

% A keres�si fa kiindul� pontja. A G param�ter �res.
generate(s(K, S), R-C, VM, [[PHH|PHT]|PT], [], GC, SSols) :-
    C > 1, C1 is C - 1,
    
    % Eddigi gener�lt kieg�sz�t�se sudoku t�bl�v�.
    fill_board(K, R-C, [[[v(PHH)]]], CG),
    
    % SSpec alapj�n be�rhat� �rt�kek a mez�be.
    mtx_nth(VM, R-C1, SV),
    
    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    block_index(K, R-C, BI),
    add_to_nth(GC, BI, PHH, GC1),
    
    % SV �s az eddig gener�lt sudoku megold�s alapj�n be�rhat� �rt�kek
    % metszete.
    values(s(K, CG), R-C1, GC1, GV),
    intersection(SV, GV, NP),
    
    % Ha van w jobbra akkor sz�r�nk a jobbra l�v� sz�m alapj�n
    mtx_nth(S, R-C, EC),
    (
        member(w, EC) -> filter_parity(NP, PHH, WP);
        WP = NP
    ), !,
    generate(s(K, S), R-C1, VM, [WP,[PHH|PHT]|PT], [[[v(PHH)]]], GC1,
        SSols).

generate(s(K, S), R-1, VM, [[PHH|PHT]|PT], [[GHH|GHT]|GT], GC, SSols) :-
    C1 is K * K, R1 is R - 1,
    
    % Eddigi gener�lt kieg�sz�t�se sudoku t�bl�v�.
    fill_board(K, R-1, [[[v(PHH)],GHH|GHT]|GT], CG),
    
    % SSpec alapj�n be�rhat� �rt�kek a mez�be.
    mtx_nth(VM, R1-C1, SV),
    
    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    block_index(K, R-1, BI),
    add_to_nth(GC, BI, PHH, GC1),
    
    % SV �s az eddig gener�lt sudoku megold�s alapj�n be�rhat� �rt�kek
    % metszete.
    values(s(K, CG), R1-C1, GC1, GV),
    intersection(SV, GV, NP),
    
    % Ha van s itt akkor sz�r�nk az alattunk l�v� sz�m alapj�n
    mtx_nth(S, R1-C1, CC),
    (
        member(s, CC) ->
            X is K * K - 1,
            nth1(X, [GHH|GHT], [v(N)]),
            filter_parity(NP, N, SP);
        SP = NP
    ), !,
    generate(s(K, S), R1-C1, VM, [SP,[PHH|PHT]|PT], 
        [[[v(PHH)],GHH|GHT]|GT], GC1, SSols).


generate(s(K, S), R-C, VM, [[PHH|PHT]|PT], [[GHH|GHT]|GT], GC, SSols) :-
    C =:= K * K, C1 is C - 1,
    
    % Eddigi gener�lt kieg�sz�t�se sudoku t�bl�v�.
    fill_board(K, R-C, [[[v(PHH)]]|[[GHH|GHT]|GT]], CG),
    
    % SSpec alapj�n be�rhat� �rt�kek a mez�be.
    mtx_nth(VM, R-C1, SV),
    
    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    block_index(K, R-C, BI),
    add_to_nth(GC, BI, PHH, GC1),
    
    % SV �s az eddig gener�lt sudoku megold�s alapj�n be�rhat� �rt�kek
    % metszete.
    values(s(K, CG), R-C1, GC1, GV),
    intersection(SV, GV, NP),
    
    mtx_nth(S, R-C, EC),
    mtx_nth(S, R-C1, CC),
    
    % Ha van w jobbra akkor sz�r�nk a jobbra l�v� sz�m alapj�n
    (
        member(w, EC) -> filter_parity(NP, PHH, WP);
        WP = NP
    ),
    
    % Ha van s itt akkor sz�r�nk az alattunk l�v� sz�m alapj�n
    (
        member(s, CC) ->
            nth1(C1, [GHH|GHT], [v(N)]),
            filter_parity(WP, N, SP);
        SP = WP
    ), !,
    generate(s(K, S), R-C1, VM, [SP,[PHH|PHT]|PT], 
        [[[v(PHH)]]|[[GHH|GHT]|GT]], GC1, SSols).


generate(s(K, S), R-C, VM, [[PHH|PHT]|PT], [[GHH|GHT]|GT], GC, SSols) :-
    C < K * K, C > 1, C1 is C - 1,
    
    % Eddigi gener�lt kieg�sz�t�se sudoku t�bl�v�.
    fill_board(K, R-C, [[[v(PHH)],GHH|GHT]|GT], CG),
    
    % SSpec alapj�n be�rhat� �rt�kek a mez�be.
    mtx_nth(VM, R-C1, SV),
    block_index(K, R-C, BI),
    
    % SV �s az eddig gener�lt sudoku megold�s alapj�n be�rhat� �rt�kek
    % metszete.
    add_to_nth(GC, BI, PHH, GC1),
    values(s(K, CG), R-C1, GC1, GV),
    intersection(SV, GV, NP),
    
    mtx_nth(S, R-C, EC),
    mtx_nth(S, R-C1, CC),
    
    % Ha van w jobbra akkor sz�r�nk a jobbra l�v� sz�m alapj�n
    (
        member(w, EC) -> filter_parity(NP, PHH, WP);
        WP = NP
    ),
    
    % Ha van s itt akkor sz�r�nk az alattunk l�v� sz�m alapj�n
    (
        member(s, CC) ->
            [GH|_] = GT,
            nth1(C1, GH, [v(N)]),
            filter_parity(WP, N, SP);
        SP = WP
    ), !,
    generate(s(K, S), R-C1, VM, [SP,[PHH|PHT]|PT], 
        [[[v(PHH)],GHH|GHT]|GT], GC1, SSols).
    
% :- pred values_mtx(+sspec, -list(list(list(integer)))).
% values_mtx(SSpec, VM): VM az SSpec feladv�ny szerint a mez�kbe �rhat�
% sz�mok m�trixa. A be�hat� sz�mik minimaliz�lva vannak.
values_mtx(s(K, S), VM) :- 
    chop(S, K-K, SC),
    values_mtx_h(s(K, S), SC, VM0),
    values_mtx(s(K, S), VM0, VM).

% :- pred values_mtx(+sspec, +list(list(list(integer))),
%         -list(list(list(integer)))).
% values_mtx(SSpec, VM0, VM): VM az SSpec feladv�ny szerint a mez�kbe
% �rhat� sz�mok m�trixa. VM0 a kiindul� m�trix amit minimaliz�lni kell.
values_mtx(s(K, S), VM0, VM) :-
    has_single(S, VM0, 1-1, N, R-C),
    (
        N =:= 0 -> VM = VM0;
        add_to_sspec(S, N, R-C, S1),
        remove_from_mtx(VM0, N, R-C, K, VM1),
        values_mtx(s(K, S1), VM1, VM)
    ).

% :- pred values_mtx_h(+sspec, +list(list(info)),
%         -list(list(list(integer)))).
% values_mtx_h(SSpec, SC, VM): Seg�d f�ggv�ny values_mtx f�ggv�nyhez. VM
% az SSpec feladv�ny szerint a mez�kbe �rhat� sz�mok m�trixa.
values_mtx_h(s(K, S), SC, VM) :- values_mtx_h(s(K, S), SC, 1-1, VM).

% :- pred values_mtx_h(+sspec, +list(list(info)), +coords,
%         -list(list(list(integer)))).
% values_mtx_h(SSpec, SC, R_C, VM): Seg�d f�ggv�ny values_mtx_h/3
% f�ggv�nyhez. VM az SSpec feladv�ny szerint a mez�kbe �rhat� sz�mok
% m�trixa. R_C koordin�t�k jelzik, hogy hol tart az �rt�kek
% gener�l�s�ban.
values_mtx_h(s(K, S), SC, R-C, [[Vals]]) :-
    R =:= K * K, C =:= K * K,
    values(s(K, S), R-C, SC, Vals).
values_mtx_h(s(K, S), SC, R-C, [[Vals]|Mtx]) :-
    R < K * K, C =:= K * K,
    R1 is R + 1,
    values(s(K, S), R-C, SC, Vals),
    values_mtx_h(s(K, S), SC, R1-1, Mtx).
values_mtx_h(s(K, S), SC, R-C, [[Vals|MtxH]|MtxT]) :-
    C < K * K,
    C1 is C + 1,
    values(s(K, S), R-C, SC, Vals),
    values_mtx_h(s(K, S), SC, R-C1, [MtxH|MtxT]).

% :- pred add_to_sspec(+board, +info, +coords, -board).
% add_to_sspec(S0, X, R_C, S): X-et az S0 sudoku t�bla R_C koordin�t�j�
% mez�j�hez adva kapjuk az S sudoku t�bl�t.
add_to_sspec([[HH|HT]|T], N, 1-1, [[[v(N)|HH]|HT]|T]).
add_to_sspec([[HH|HT]|T], N, 1-C, [[HH|ANSH]|ANST]) :- C > 1,
    C1 is C - 1,
    add_to_sspec([HT|T], N, 1-C1, [ANSH|ANST]).
add_to_sspec([H|T], N, R-C, [H|Ans]) :- R > 1,
    R1 is R - 1,
    add_to_sspec(T, N, R1-C, Ans).
    
% :- pred remove_from_mtx(+list(list(list(integer))), +integer, +coords,
%         +size, -list(list(list(integer)))).
% remove_from_mtx(VM0, N, R_C, K, VM): A VM0 m�trix minden mez�j�b�l
% elv�ve azt a sz�mot, amely az R_C koordin�t�j� mez�ben lev� N sz�mmal
% �ssze�tk�zik K m�ret� sudoku t�bla alapj�n.
remove_from_mtx(VM, N, R-C, K, Removed) :-
    block_index(K, R-C, BI),
    remove_from_mtx(VM, N, R-C, 1-1, K, BI, Removed).
    
% :- pred remove_from_mtx(+list(list(list(integer))), +integer, +coords,
%         +coords, +size, +integer, -list(list(list(integer)))).
% remove_from_mtx(VM0, N, R_C, R_C1, K, BI, VM): Seg�d f�ggv�ny a
% remove_from_mtx/5 f�ggv�nyhez. A VM0 m�trix minden mez�j�b�l elv�ve
% azt a sz�mot, amely az R_C koordin�t�j� mez�ben lev� N sz�mmal
% �ssze�tk�zik K m�ret� sudoku t�bla alapj�n. R_C1 koordin�t�k ahol
% tartunk a rekurzi�ban. BI az R_C koordin�t�khoz tartoz� blokk index,
% hogy ne kelljen mindig �jra sz�molni.
remove_from_mtx([], _, _, _, _, _, []).
remove_from_mtx([[]|T], N, R-C, R1-_, K, BI, [[]|Mtx]) :-
    R2 is R1 + 1,
    remove_from_mtx(T, N, R-C, R2-1, K, BI, Mtx).
remove_from_mtx([[HH|HT]|T], N, R-C, R1-C1, K, BI, [[HH1|MtxH]|MtxT]) :-
    block_index(K, R1-C1, BI1),
    (
        R1 =:= R, C1 =:= C -> HH1 = HH;
        R1 =:= R -> delete(HH, N, 1, HH1);
        C1 =:= C -> delete(HH, N, 1, HH1);
        BI1 =:= BI -> delete(HH, N, 1, HH1);
        HH1 = HH
    ),
    C2 is C1 + 1,
    remove_from_mtx([HT|T], N, R-C, R1-C2, K, BI, [MtxH|MtxT]).
   
% :- pred has_single(+board, +matrix, +coords, -integer, -coords).
% has_single(S, VM, R_C, R, R_C1): Ha VM lehets�ges mez� �rt�kek 
% m�trix�ban van olyan mez�, ahol csak egy lehets�ges sz�m lehet �s a 
% sudoku specifik�ci�nak ugyanabban a mez�j�ben nincsen sz�m, akkor N az
% a sz�m, R_C1 a mez� koordin�t�ja. Ha nincs ilyen mez�, akkor N, R �s
% C1 is 0.
has_single(_, [], _, 0, 0-0).
has_single(S, [[[N]|HT]|T], R-C, N1, R1-C1) :-
    mtx_nth(S, R-C, Content),
    (
        member(v(N), Content) -> C2 is C + 1,
        has_single(S, [HT|T], R-C2, N1, R1-C1);
        N1 = N, R1 = R, C1 = C
    ).
has_single(S, [[HH|HT]|T], R-C, N1, R1-C1) :- 
    \+ HH = [_],
    C2 is C + 1,
    has_single(S, [HT|T], R-C2, N1, R1-C1).
has_single(S, [[]|T], R-_, N1, R1-C1) :-
    R2 is R + 1,
    has_single(S, T, R2-1, N1, R1-C1).

% :- pred filter_parity(+list(integer), +integer, -list(integer)).
% filter_parity(L0, N, L): Az L lista az L0 lista azon elemeit
% tartalmazza, amelyek N-t�l k�l�nb�z� parit�s��ak.
filter_parity([], _, []).
filter_parity([H|T], N, [H|L]) :-
    (H + N) mod 2 =:= 1, filter_parity(T, N, L).
filter_parity([H|T], N, L) :-
    (H + N) mod 2 =\= 1, filter_parity(T, N, L).

% :- pref fill_board(+size, +coords, +list(list(any)),
%         -list(list(any))).
% fill_board(K, R_C, G, S): S az R_C koordin�t�ig gener�lt G sudoku
% megold�s �res mez�kkel kieg�sz�tettje.
fill_board(_, 1-1, G, G).
fill_board(K, R-C, G, Gen) :- 
    R > 1, C = 1,
    R1 is R - 1,
    C1 is K * K,
    fill_board(K, R1-C1, [[[]]|G], Gen).
fill_board(K, R-C, [H|T], Gen) :-
    C > 1,
    C1 is C - 1,
    fill_board(K, R-C1, [[[]|H]|T], Gen).
    
% :- pred intersection(+list(any), +list(any), -list(any)).
% intersection(L1, L2, L): L lista az L1 �s L2 list�knak a metszete.
intersection([], _, []) :- !.
intersection([X|T], L, Intersect) :-
    member(X, L), !,
    Intersect = [X|R],
    intersection(T, L, R).
intersection([_|T], L, R) :- intersection(T, L, R).

% :- pred unwrap(+matrix, -matrix).
% unwrap(L0, L): 
unwrap([], []).
unwrap([[]|A], [[]|B]) :- unwrap(A, B).
unwrap([[[v(V)]|A0]|A1], [[V|B0]|B1]) :- unwrap([A0|A1], [B0|B1]).

% :- pred list_of_lists(+integer, -list(list(any))).
% list_of_lists(N, LL): LL lista N darab �res list�t tartalmaz� lista.
list_of_lists(1, [[]]).
list_of_lists(K, [[]|F]) :-
    K > 1, K1 is K - 1,
    list_of_lists(K1, F).
    
% :- pred add_to_nth(+list(list(any)), +integer, +any,
%         -list(list(any))).
% add_to_nth(LL0, I, X, LL): LL list�k list�ja az LL0 list�k list�ja,
% X-et az I. lista elej�hez f�zve.
add_to_nth([H|T], 1, X, [[[v(X)]|H]|T]).
add_to_nth([H|T], I, X, [H|Tail]) :- I > 1, I1 is I - 1,
    add_to_nth(T, I1, X, Tail).

% :- pred remove_from_nth(+list(list(any)), +integer, -list(list(any))).
% remove_from_nth(LL0, I, LL): LL lista az LL0 lista I. list�j�nak els�
% elem�t elv�ve kapott lista.
remove_from_nth([[_|HT]|T], 1, [HT|T]).
remove_from_nth([H|T], I, [H|Tail]) :- I > 1, I1 is I - 1,
    remove_from_nth(T, I1, Tail).

% :- pred values(+sspec, +coords, +list(list(any)), -list(int)).
% values(SSpec, R_C, SC, Vals): 
% Egy �rt�k pontosan akkor szerepel a Vals list�ban, ha:
%    (a) 1..k*k k�z�tti eg�sz, ahol k az SSpec feladv�ny cellam�rete,
%    (b) teljes�ti az adott mez�re vonatkoz� sz�m- �s parit�si inf�k
%        �ltal el��rt megszor�t�sokat, tov�bb�
%    (c) k�l�nb�zik az adott mez�t tartalmaz� sor, oszlop �s cella t�bbi
%        mez�j�ben szerepl� sz�minf�kt�l, 
% ahol
%    SSpec az sspec t�pusspecifik�ci�nak megfelel� Sudoku-feladv�ny,
%    R_C az adott feladv�ny egy mez�j�nek (sor-oszlop form�ban megadott) 
%    koordin�t�ja, Vals list(int) t�pus� mez��rt�klista, az SSpec 
%    feladv�ny R_C koordin�t�j� mez�j�ben megengedett �rt�kek list�ja. 
% SC az
% SSpec-ben l�v� sudoku t�bla {K, K} param�ter� feldaraboltja.
values(s(K, S), R-C, SC, Vals) :-
    mtx_nth(S, R-C, CC),
    Max is K * K,
    (
        member(o, CC), member(e, CC) -> P = [];
        member(o, CC) -> numlist(1, Max, 2, P);
        member(e, CC) -> numlist(2, Max, 2, P);
        numlist(1, Max, 1, P)
    ),
    (
        member(v(V), CC) -> CellN = V;
        CellN = -1
    ),
    row_numbers(S, R, CellN, RN),
    column_numbers(S, C, CellN, CN),
    block_numbers(K, SC, R-C, CellN, BN),
    findall(V, (member(V, P), \+ member(V, RN), 
        \+ member(V, CN), \+ member(V, BN)), Vals).

% :- pred mtx_nth(+list(list(any)), +coords, -any).
% mtx_nth(Mx, R_C, E): E az Mx m�trix R_C koordin�t�j� eleme.
mtx_nth(Mx, R-C, E) :- nth1(R, Mx, L), nth1(C, L, E).

% :- pred row_numbers(+list(list(any)), +int, +int, -list(int)).
% row_numbers(S, R, I, L): Az L lista azokat a sz�mokat tartalmazza,
% amelyek az S sudoku t�bla R. sor�ban vannak, az I sz�mot egyszer
% kiv�ve.
row_numbers(S, R, I, L) :-
    nth1(R, S, L0),
    extract_numbers(L0, L1),
    delete(L1, I, 1, L).

% :- pred column_numbers(+list(list(any)), +int, +int, -list(int)).
% row_numbers(S, C, I, L): Az L lista azokat a sz�mokat tartalmazza,
% amelyek az S sudoku t�bla C. oszlop�ban vannak, az I sz�mot egyszer
% kiv�ve.
column_numbers(S, C, I, L) :-
    findall(V, (member(L0, S), nth1(C, L0, V)), L1),
    extract_numbers(L1, L2),
    delete(L2, I, 1, L).

% :- pred block_numbers(+sspec, +coords, +int, -list(int)).
% block_numbers(SSpec, R_C, Vals): Az L lista azokat a sz�mokat 
% tartalmazza, amelyek az S sudoku t�bla R_C koordin�t�j� mez�h�z
% tartoz� blokkban vannak, az I sz�mot egyszer kiv�ve.
block_numbers(K, SC, R-C, I, L) :- 
    block_index(K, R-C, B),
    nth1(B, SC, L1),
    extract_numbers(L1, L2),
    delete(L2, I, 1, L).

% :- pred block_index(+size, +coords, -integer).
% block_index(K, R_C, I): I annak a K m�ret� sudoku feldaraboltj�ban 
% l�v� elemnek az indexe, amelyben a sudoku t�bla R_C koordin�t�j� eleme
% van.
block_index(K, R-C, I) :-
    X is (R - 1) // K + 1,
    Y is (C - 1) // K + 1,
    I is (X - 1) * K + Y.

% :- pred extract_numbers(+list(any), -list(int)).
% extract_numbers(L0, L): Az L lista azokat a szamokat tartalmazza 
% amelyek v(N) alakban vannak az L0 list�ban (vagy m�lyebb list�kban).
extract_numbers([], []).
extract_numbers([e|T], Nums) :- extract_numbers(T, Nums).
extract_numbers([o|T], Nums) :- extract_numbers(T, Nums).
extract_numbers([w|T], Nums) :- extract_numbers(T, Nums).
extract_numbers([s|T], Nums) :- extract_numbers(T, Nums).
extract_numbers([[]|T], Nums) :- extract_numbers(T, Nums).
extract_numbers([v(V)|T], [V|Nums]) :- extract_numbers(T, Nums).
extract_numbers([[H|T0]|T], Nums) :-
    extract_numbers([H|T0], Nums0),
    extract_numbers(T, Nums1),
    append(Nums0, Nums1, Nums).

% :- pred numlist(+integer, +integer, +integer, -list(integer)).
% numlist(L, U, S, Ns): Ns az L-t�l kezd�d�, U-ig tart�, S l�p�sk�z�
% sz�mtani sorozat. 
numlist(L, U, _, []) :- L > U.
numlist(L, U, Step, [L|Ns]) :- 
    L =< U, 
    L0 is L + Step, 
    numlist(L0, U, Step, Ns).

% :- pred chop(+matrix, +parameter, -list(list(any))).
% chop(Mx, P, LL): Az LL lista az Mx m�trix P param�ter� feldarabol�sa.
chop([], _, []).
chop(Mx, R-C, LL) :-
    take(R, Mx, R1),
    drop(R, Mx, R2),
    chop_c(R1, C, R3),
    chop(R2, R-C, LL1),
    append(R3, LL1, LL).

% :- pred chop_c(+matrix, +int, -list(list(any))).
% chop_c(Mx, C, LL): Az LL lista az Mx matrix C oszloponkenti 
% feldarabolasa. Az oszlopok elemei sorfolytonosan vannak felsorolva.
chop_c([[]|_], _, []).
chop_c(Mx, C, [C1|LL1]) :-
    takecf(Mx, C, C1),
    dropc(Mx, C, Mx1),
    length(C1, N),
    N > 0,
    chop_c(Mx1, C, LL1).

% :- pred takecf(+matrix, +int, -list(any)).
% takecf(Mx, C, L): A L lista az Mx m�trix elso C oszlopanak elemei 
% sorfolytonosan felsorolva.
takecf([], _, []).
takecf([H|T], C, L) :-
    take(C, H, X),
    takecf(T, C, L1),
    append(X, L1, L).

% :- pred dropc(+matrix, +int, -list(list(any))).
% dropc(Mx0, C, Mx): Az Mx matrix az Mx0 matrix elso C oszlopanak 
% elvetelevel kapott matrix.
dropc([], _, []).
dropc([H|T], C, [X|L1]) :- drop(C, H, X), dropc(T, C, L1).

% :- pred take(+int, +list(any), -list(any)).
% take(N, L0, L): Az L lista az L0 lista N hossz� prefixuma.
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

% :- pred drop(+int, +list(any), -list(any)).
% drop(N, L0, L): Az L0 lista olyan szuffixuma L, amely az L0 els� N
% elem�t nem tartalmazza.
drop(N, L, L) :- N =< 0, !, N =:= 0.
drop(_, [], []).
drop(N, [_|Xs], Ys) :- M is N-1, drop(M, Xs, Ys).