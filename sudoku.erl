-module(sudoku).
-author('lngtrn94@gmail.com').
-vsn('2016-11-06').
-export([sudoku/1]).
-compile(export_all).

%% @type sspec() = {size(), board()}.
%% @type size()  = integer().
%% @type field() = [info()].
%% @type info()  = e | o | s | w | integer().
%% @type board() = [[field()]].
%% @type ssol() = [[integer()]].
%% @type col() = integer().
%% @type row() = integer().
%% @type coords() = {row(),col()}.
%% @type matrix() = [[any()]].
%% @type parameter() = coords().

%% @spec sudoku:sudoku(SSpec::sspec()) -> SSols::[ssol()].
%% @doc  SSols az SSpec feladv�nyt kiel�g�t� megold�sok list�ja.
sudoku({K, S}) ->
    VM = values_mtx({K, S}),
    GC = list_of_lists(K * K),
    generate({K, S}, {K * K, K * K}, VM, [mtx_nth(VM, {K * K, K * K})],
        [], GC).

%% @spec generate(SSpec::sspec(), R_C::coords(), VM::[[integer()]],
%%       P::[[integer()]], G::[[integer()]], GC::[[integer()]]) ->
%%       SSols::[ssol()].
%% @doc  SSols az SSpec feladv�nyt kiel�g�t� megold�sok list�ja. R_C
%%       koordin�t�j� mez� az ahol a gener�l�sn�l tartunk. VM egy
%%       list�k m�trixa, az SSpec szrinti mez�kbe �rhat� lehets�ges
%%       �rt�keket tartalmazza. P a cell�kba m�g be�rhat� sz�mokat,
%%       tartalmazza a keres�si f�ban val� visszal�p�shez. G a gener�lt
%%       sudoku megold�s aktu�lis �llapota. GC az gener�lt sudoku
%%       megold�s feldarabolja.

% Kil�p�si felt�tel.
generate(_, _, _, [[]], _, _) -> [];

% Tal�ltunk egy megold�st.
generate({K, S}, {1, 1}, VM, [[P|PH]|PT], [GH|GT], GC) ->
    [[[P|GH]|GT]|generate({K, S}, {1, 1}, VM, [PH|PT], [GH|GT], GC)];

% Visszal�p�s a keres�si f�ban.
generate({K, S}, {R, C}, VM, [[],PH|PT], [[_]|GT], GC) ->
    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    GC1 = remove_from_nth(GC, block_index(K, {R, C + 1})),
    % Eldobjuk a be�rt sz�mot (PH feje)
    generate({K, S}, {R, C + 1}, VM, [tl(PH)|PT], GT, GC1);

% Visszal�p�s a keres�si f�ban.
generate({K, S}, {R, C}, VM, [[],PH|PT], [GH|GT], GC) ->
    if
        C =:= K * K ->
            % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
            GC1 = remove_from_nth(GC, block_index(K, {R + 1, 1})),
            % Eldobjuk a be�rt sz�mot (PH feje)
            generate({K, S}, {R + 1, 1}, VM, [tl(PH)|PT], [tl(GH)|GT], 
                GC1);
        true ->
            % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
            GC1 = remove_from_nth(GC, block_index(K, {R, C + 1})),
            % Eldobjuk a be�rt sz�mot (PH feje)
            generate({K, S}, {R, C + 1}, VM, [tl(PH)|PT], [tl(GH)|GT], 
                GC1)
    end;

% A keres�si fa kiindul� pontja. A G param�ter �res.
generate({K, S}, {R, C}, VM, [PH|PT], [], GC) when C > 1 ->

    % Eddigi gener�lt kieg�sz�t�se sudoku t�bl�v�.
    CG = fill_board(K, {R, C}, [[hd(PH)]]),

    % SSpec alapj�n be�rhat� �rt�kek a mez�be.
    SV = mtx_nth(VM, {R, C - 1}),

    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    GC1 = add_to_nth(GC, block_index(K, {R, C}), hd(PH)),

    % SV �s az eddig gener�lt sudoku megold�s alapj�n be�rhat� �rt�kek
    % metszete.
    NP = intersection(SV, values({K, CG}, {R, C - 1}, GC1)),

    % Ha van w jobbra akkor sz�r�nk a jobbra l�v� sz�m alapj�n
    WP = case lists:member(w, mtx_nth(S, {R, C})) of
        true -> [X || X <- NP, (X + hd(PH)) rem 2 =:= 1];
        false -> NP
    end,
    generate({K, S}, {R, C - 1}, VM, [WP,PH|PT], [[hd(PH)]], GC1);

% Tov�bb l�p�s a keres�si f�ban. Egy sor elej�n vagyunk, az el�z� sor
% v�g�re l�p�nk.
generate({K, S}, {R, 1}, VM, [PH|PT], [GH|GT], GC) ->

    % Eddigi gener�lt kieg�sz�t�se sudoku t�bl�v�.
    CG = fill_board(K, {R, 1}, [[hd(PH)|GH]|GT]),

    % SSpec alapj�n be�rhat� �rt�kek a mez�be.
    SV = mtx_nth(VM, {R - 1, K * K}),

    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    GC1 = add_to_nth(GC, block_index(K, {R, 1}), hd(PH)),

    % SV �s az eddig gener�lt sudoku megold�s alapj�n be�rhat� �rt�kek
    % metszete.
    NP = intersection(SV, values({K, CG}, {R - 1, K * K}, GC1)),

    % Ha van s itt akkor sz�r�nk az alattunk l�v� sz�m alapj�n
    SP = case lists:member(s, mtx_nth(S, {R - 1, K * K})) of
        true ->
            SN = lists:nth(K * K - 1, GH),
            [X || X <- NP, (X + SN) rem 2 =:= 1];
        false -> NP
    end,
    generate({K, S}, {R - 1, K * K}, VM, [SP,PH|PT], [[hd(PH)|GH]|GT], 
        GC1);

% Tov�bb l�p�s a keres�si f�ban. Egyel balra l�p�nk.
generate({K, S}, {R, C}, VM, [PH|PT], [GH|GT], GC) ->

    % �j sor, ha C = K * K.
    G = if
        C =:= K * K -> [[hd(PH)]|[GH|GT]];
        true -> [[hd(PH)|GH]|GT]
    end,

    % Eddigi gener�lt kieg�sz�t�se sudoku t�bl�v�.
    CG = fill_board(K, {R, C}, G),

    % SSpec alapj�n be�rhat� �rt�kek a mez�be.
    SV = mtx_nth(VM, {R, C - 1}),

    % Friss�tj�k a gener�lt megold�s feldaraboltj�t.
    GC1 = add_to_nth(GC, block_index(K, {R, C}), hd(PH)),

    % SV �s az eddig gener�lt sudoku megold�s alapj�n be�rhat� �rt�kek
    % metszete.
    NP = intersection(SV, values({K, CG}, {R, C - 1}, GC1)),

    % Ha van w jobbra akkor sz�r�nk a jobbra l�v� sz�m alapj�n
    WP = case lists:member(w, mtx_nth(S, {R, C})) of
        true -> [X || X <- NP, (X + hd(PH)) rem 2 =:= 1];
        false -> NP
    end,

    % Ha van s itt akkor sz�r�nk az alattunk l�v� sz�m alapj�n
    SP = case lists:member(s, mtx_nth(S, {R, C - 1})) of
        true ->
            SN = if
                C =:= K * K -> lists:nth(C - 1, GH);
                true -> lists:nth(C - 1, hd(GT))
            end,
            [X || X <- WP, (X + SN) rem 2 =:= 1];
        false -> WP
    end,
    generate({K, S}, {R, C - 1}, VM, [SP,PH|PT], G, GC1).

%% @spec values_mtx_h(SSpec::sspec()) -> VM::[[[integer()]]].
%% @doc  VM az SSpec feladv�ny szerint a mez�kbe �rhat� sz�mok m�trixa.
%%       A be�hat� sz�mik minimaliz�lva vannak.
values_mtx({K, S}) ->
    values_mtx({K, S}, values_mtx_h({K, S}, chop(S, {K, K}))).

%% @spec values_mtx_h(SSpec::sspec(), VM0::[[[integer()]]]) ->
%%       VM::[[[integer()]]].
%% @doc  VM az SSpec feladv�ny szerint a mez�kbe �rhat� sz�mok m�trixa.
%%       VM0 a kiindul� m�trix amit minimaliz�lni kell.
values_mtx({K, S}, VM) ->
    case has_single(S, VM, {1, 1}) of
        false -> VM;
        {N, R, C} ->
            S1 = add_to_sspec(S, N, {R, C}),
            VM1 = remove_from_mtx(VM, N, {R, C}, K),
            values_mtx({K, S1}, VM1)
    end.

%% @spec values_mtx_h(SSpec::sspec(), SC::[[info()]]) ->
%%       VM::[[[integer()]]].
%% @doc  Seg�d f�ggv�ny values_mtx f�ggv�nyhez. VM az SSpec feladv�ny
%%       szerint a mez�kbe �rhat� sz�mok m�trixa.
values_mtx_h({K, S}, SC) -> values_mtx_h({K, S}, SC, {1, 1}).

%% @spec values_mtx_h(SSpec::sspec(), SC::[[info()]], R_C::coords()) ->
%%       VM::[[[integer()]]].
%% @doc  Seg�d f�ggv�ny values_mtx_h/2 f�ggv�nyhez. VM az SSpec
%%       feladv�ny szerint a mez�kbe �rhat� sz�mok m�trixa. R_C
%%       koordin�t�k jelzik, hogy hol tart az �rt�kek gener�l�s�ban.
values_mtx_h({K, S}, SC, {X, X}) when X =:= K * K ->
    [[values({K, S}, {X, X}, SC)]];
values_mtx_h({K, S}, SC, {R, C}) when C =:= K * K ->
    Mx = values_mtx_h({K, S}, SC, {R + 1, 1}),
    [[values({K, S}, {R, C}, SC)]|Mx];
values_mtx_h({K, S}, SC, {R, C}) ->
    Mx = values_mtx_h({K, S}, SC, {R, C + 1}),
    [[values({K, S}, {R, C}, SC)|hd(Mx)]|tl(Mx)].

%% @spec add_to_sspec(S0::board(), X::info(), R_C::coords()) ->
%%       S::board().
%% @doc  X-et az S0 sudoku t�bla R_C koordin�t�j� mez�j�hez adva kapjuk
%%       az S sudoku t�bl�t.
add_to_sspec([[HH|HT]|T], X, {1, 1}) -> [[[X|HH]|HT]|T];
add_to_sspec([[HH|HT]|T], X, {1, C}) ->
    Mx = add_to_sspec([HT|T], X, {1, C - 1}),
    [[HH|hd(Mx)]|tl(Mx)];
add_to_sspec([H|T], X, {R, C}) -> [H|add_to_sspec(T, X, {R - 1, C})].

%% @spec remove_from_mtx(VM0::[[[integer()]]], N::integer(),
%%       R_C::coords(), K::size()) -> VM::[[[integer()]]].
%% @doc  A VM0 m�trix minden mez�j�b�l elv�ve azt a sz�mot, amely az R_C
%%       koordin�t�j� mez�ben lev� N sz�mmal �ssze�tk�zik K m�ret�
%%       sudoku t�bla alapj�n.
remove_from_mtx(VM, N, {R, C}, K) ->
    remove_from_mtx(VM, N, {R, C}, {1, 1}, K, block_index(K, {R, C})).

%% @spec remove_from_mtx(VM0::[[[integer()]]], N::integer(),
%%       R_C::coords(), R_C1::coords(), K::size(), BI::integer()) ->
%%       VM::[[[integer()]]].
%% @doc  Seg�d f�ggv�ny a remove_from_mtx/4 f�ggv�nyhez. A VM0 m�trix
%%       minden mez�j�b�l elv�ve azt a sz�mot, amely az R_C
%%       koordin�t�j� mez�ben lev� N sz�mmal �ssze�tk�zik K m�ret�
%%       sudoku t�bla alapj�n. R_C1 koordin�t�k ahol tartunk a
%%       rekurzi�ban. BI az R_C koordin�t�khoz tartoz� blokk index, hogy
%%       ne kelljen mindig �jra sz�molni.
remove_from_mtx([], _, _, _, _, _) -> [];
remove_from_mtx([[]|T], N, {R, C}, {R1, _}, K, BI) ->
    [[]|remove_from_mtx(T, N, {R, C}, {R1 + 1, 1}, K, BI)];
remove_from_mtx([[HH|HT]|T], N, {R, C}, {R1, C1}, K, BI) ->
    HH1 = case {R1 =:= R, C1 =:= C, BI =:= block_index(K, {R1, C1})} of
        {true, true, _} -> HH;
        {true, _, _} -> lists:delete(N, HH);
        {_, true, _} -> lists:delete(N, HH);
        {_, _, true} -> lists:delete(N, HH);
        _ -> HH
    end,
    Mx = remove_from_mtx([HT|T], N, {R, C}, {R1, C1 + 1}, K, BI),
    [[HH1|hd(Mx)]|tl(Mx)].

%% @spec has_single(S::board(), VM::matrix(), R_C::coords()) ->
%%       false | {N::integer(), R::integer(), C::integer()}.
%% @doc  Ha VM lehets�ges mez� �rt�kek m�trix�ban van olyan mez�, ahol
%%       csak egy lehets�ges sz�m lehet �s a sudoku specifik�ci�nak
%%       ugyanabban a mez�j�ben nincsen sz�m, akkor N az a sz�m, R a
%%       sor, C az oszlop. Ha nincs ilyen mez�, akkor false.
has_single(_, [], _) -> false;
has_single(S, [[[N]|HT]|T], {R, C}) ->
    case lists:member(N, mtx_nth(S, {R, C})) of
        false -> {N, R, C};
        true -> has_single(S, [HT|T], {R, C + 1})
    end;
has_single(S, [[_|HT]|T], {R, C}) -> has_single(S, [HT|T], {R, C + 1});
has_single(S, [[]|T], {R, _}) -> has_single(S, T, {R + 1, 1}).

%% @spec fill_board(K::integer(), R_C::coords(), G::[[any()]]) ->
%%       S::matrix().
%% @doc  S az R_C koordin�t�ig gener�lt G sudoku megold�s �res mez�kkel
%%       kieg�sz�tettje.
fill_board(_, {1, 1}, G) -> G;
fill_board(K, {R, 1}, G) -> fill_board(K, {R - 1, K * K}, [[[]]|G]);
fill_board(K, {R, C}, [H|T]) -> fill_board(K, {R, C - 1}, [[[]|H]|T]).

%% @spec intersection(L1::[any()], L2::[any()]) -> L::[any()].
%% @doc  L lista az L1 �s L2 list�knak a metszete.
intersection([], _) -> [];
intersection([H|T], L2) ->
    case lists:member(H, L2) of
        true -> [H|intersection(T, L2)];
        false -> intersection(T, L2)
    end.

%% @spec list_of_lists(N::integer()) -> LL::[[any()]].
%% @doc  LL lista N darab �res list�t tartalmaz� lista.
list_of_lists(1) -> [[]];
list_of_lists(K) -> [[]|list_of_lists(K - 1)].

%% @spec add_to_nth(LL0::[[any()]], I::integer(), X::any()) ->
%%       LL::[[any()]].
%% @doc  LL list�k list�ja az LL0 list�k list�ja, X-et az I. lista
%%       elej�hez f�zve.
add_to_nth([H|T], 1, X) -> [[X|H]|T];
add_to_nth([H|T], I, X) -> [H|add_to_nth(T, I - 1, X)].

%% @spec remove_from_nth(LL0::[[any()]], I::integer()) -> LL::[[any()]].
%% @doc  LL lista az LL0 lista I. list�j�nak els� elem�t elv�ve kapott
%%       lista.
remove_from_nth([[]|T], 1) -> [[]|T];
remove_from_nth([[_|HT]|T], 1) -> [HT|T];
remove_from_nth([H|T], I) -> [H|remove_from_nth(T, I - 1)].

%% @spec values(SSpec::sspec(), R_C::coords(), SC::[[integer()]]) -> 
%%       L::[integer()]
%% @doc  Az L lista az SSpec specifik�ci�val megadott Sudoku-feladv�ny
%%       R_C koordin�t�j� mez�j�ben megengedett �rt�kek list�ja. SC az
%%       SSpec-ben l�v� sudoku t�bla {K, K} param�ter� feldaraboltja.
values({K, S}, {R, C}, SC) ->
    CC = mtx_nth(S, {R, C}),
    P = case {lists:member(o, CC), lists:member(e, CC)} of
        {true, true} -> [];
        {true, _} -> lists:seq(1, K * K, 2);
        {_, true} -> lists:seq(2, K * K, 2);
        _ -> lists:seq(1, K * K)
    end,
    case has_number(CC) of
        false -> remove_conflicts({K, S}, {R, C}, SC, P, 0);
        CN ->
            P1 = remove_conflicts({K, S}, {R, C}, SC, P, CN),
            case lists:member(CN, P1) of
                true -> [CN];
                false -> []
            end
    end.

%% @spec has_number(L::[any()]) -> R:: false | integer().
%% @doc  R az L list�ban tal�lhat� els� sz�m. R false, ha nincs az L0
%%       list�ban sz�m.
has_number([]) -> false;
has_number([H|T]) ->
    if
        is_integer(H) -> H;
        true -> has_number(T)
    end.

%% @spec column(Mx::matrix(), C::integer()) -> L::[any()].
%% @doc  Az L lista az Mx m�trix C. oszlopa.
column(Mx, C) -> [lists:nth(C, X) || X <- Mx].

%% @spec mtx_nth(S::[[[any()]]], R_C::coords()) -> E::any().
%% @doc  E az Mx m�trix R_C koordin�t�j� eleme.
mtx_nth(Mx, {R, C}) -> lists:nth(C, lists:nth(R, Mx)).

%% @spec block_index(K::size(), R_C::coords()) -> I::integer().
%% @doc  I annak a K m�ret� sudoku feldaraboltj�ban l�v� elemnek az
%%       indexe, amelyben a sudoku t�bla R_C koordin�t�j� eleme van.
block_index(K, {R, C}) -> ((R - 1) div K) * K + ((C - 1) div K + 1).

%% @spec remove_conflicts(SSpec::sspec(), R_C::coords(), SC::[[any()]],
%%       P::[integer()], CN::[integer()]) -> L::[integer()].
%% @doc  L azoknak a sz�moknak a list�ja, amelyek benne vannak P
%%       list�ban �s belehet �rni a SSpec �ltal specifik�lt sudoku t�bla
%%       R_C koordin�t�j� mez�j�be. CN a SSpec R_C koordin�t�j�
%%       mez�j�ben l�v� sz�m (0, ha nincs a mez�ben sz�m).
remove_conflicts({K, S}, {R, C}, SC, P, CN) ->
    if
        CN > 0 ->
            Row = lists:delete(CN, lists:flatten(lists:nth(R, S))),
            Column = lists:delete(CN, lists:flatten(column(S, C))),
            Block = lists:delete(CN,
                lists:nth(block_index(K, {R, C}), SC));
        true ->
            Row = lists:flatten(lists:nth(R, S)),
            Column = lists:flatten(column(S, C)),
            Block = lists:nth(block_index(K, {R, C}), SC)
        end,
    [X || X <- P, not lists:member(X, Row) andalso
        not lists:member(X, Column) andalso
        not lists:member(X, Block)].

%% @spec chop(Mx::matrix(), P::parameter()) -> LL::[[any()]].
%% @doc  Az LL lista az Mx m�trix P param�ter� feldarabol�sa.
chop([], _) -> [];
chop(Mx, {R, C}) -> chop_c(take(Mx, R), C) ++ chop(drop(Mx, R), {R, C}).

%% @spec chop_c(Mx::matrix(), C::integer()) -> LL::[[any()]].
%% @doc  Az LL lista az Mx matrix C oszloponkenti feldarabolasa. Az
%%       oszlopok elemei sorfolytonosan vannak felsorolva.
chop_c([[] | _], _) -> [];
chop_c(Mx, C) -> [lists:flatten([take(L, C) || L <- Mx]) |
    chop_c([drop(L, C) || L <- Mx], C)].

%% @spec take(L0::[term()], N::integer()) -> L::[term()].
%% @doc  Az L lista az L0 lista N hossz� prefixuma.
take([], _) -> [];
take(_, 0) -> [];
take(L0, N) -> [hd(L0) | take(tl(L0), N - 1)].

%% @spec drop(L0::[term()], N::integer()) -> L::[term()].
%% @doc  Az L0 lista olyan szuffixuma L, amely az L0 els� N elem�t nem
%%       tartalmazza.
drop([], _) -> [];
drop(L0, 0) -> L0;
drop(L0, N) -> drop(tl(L0), N - 1).