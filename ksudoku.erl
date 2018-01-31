-module(ksudoku).
-author('hanak@inf.bme.hu, kapolnai@iit.bme.hu, modified by: Andras Eisenberger, Balazs Galambosi').
-vsn('$LastChangedDate: 2016-09-15 16:40:04 +0200 (cs, 15 szept 2016) $$').

% -compile(export_all).
-export([sudoku_be/1,sudoku_ki/2,megold/2,stopper/2,stopper/3, help/0,teljes_teszt/1,teszt/2]).

-import(sudoku,[sudoku/1]).

%% @type sspec() = {size(), board()}.
%% @type size()  = integer().
%% @type field() = [info()].
%% @type info()  = e | o | s | w | integer().
%% @type board() = [[field()]].

%% @type ssol() = [[integer()]].

%% @spec sudoku:sudoku(SSpec::sspec()) -> SSols::[ssol()].
%% @doc  SSols az SSpec feladványt kielégítõ megoldások listája.

%% ksudoku-specifikációk

%% @spec sudoku_be(FileIn::string()) -> SSpec::sspec().
%% @doc  SSpec a FileIn szövegfájlból beolvasott Sudoku-feladvány.
%%
sudoku_be(FileIn) ->
    case file:open(FileIn,read) of
        {ok, InputFile}    -> ok;
        {error, InputFile=R} -> throw({error,R,file:format_error(R)})
    end,
    {N,_} = case io:get_line(InputFile,'') of
		eof ->
		    throw({error,ures,lists:flatten(io_lib:format("Ures a(z) ~s fajl",[FileIn]))});
		I ->
		    string:to_integer(cleaned(I))
	     end,
    KT = tabla_be(InputFile,N),
    file:close(InputFile),
    case helyes_feladvany(KT) of
	true ->
	    KT;
	false ->
	    throw({error,hibas,lists:flatten(io_lib:format("Hibas a feladvany a(z) ~s fajlban",[FileIn]))})
    end.

tabla_be(InputFile,M) ->
    {M,tabla_be(InputFile,M*M,[])}.

tabla_be(_InputFile,0,Zss) ->
    lists:reverse(Zss);
tabla_be(InputFile,Meret,Zss) ->
    case io:get_line(InputFile,'') of
	eof ->
	    throw({error,keves,lists:flatten(io_lib:format("Tul keves sor van a input fajlban",[]))});
	Sor ->
	    Ss = string:tokens(cleaned(Sor)," "),
	    Zs = lists:map(fun str2infolist/1, Ss),
	    case Zs of
		[] ->
		    tabla_be(InputFile,Meret,Zss);
		_ ->
		    tabla_be(InputFile,Meret-1,[Zs|Zss])
	    end
    end.

cleaned(Sor) ->
    S0 = lists:filter(fun(C) -> C /= $\t end,Sor),
    string:strip(string:strip(S0),right,$\n).

str2infolist([$-]) ->
    [];
str2infolist(XXs=[X|_Xs]) when $0=<X andalso X=<$9 ->
    {I,Rs} = string:to_integer(XXs),
    [I|str2infolist(Rs)];
str2infolist([X|Xs]) ->
    [list_to_atom([X])|str2infolist(Xs)];
str2infolist([]) ->
    [].

%% @spec helyes_feladvany(SSpec::sspec()) -> B::bool().
%% @doc  B igaz, ha a feladvány megfelel a specifikációnak.
%%
helyes_feladvany({K,T}) ->
    KK = K*K,
    % a cellaméret jó
    (1 =< K) andalso (K =< 6) andalso
    % a sorok és soronként az oszlopok száma jó 
    (KK == length(T)) andalso
	(lists:all(fun(L) -> length(L) == KK end,T)) andalso
    % a tábla megfelel a sudoku-feladvány specifikációjának
%    lists:all(fun helyes_mezo/2,lists:concat(T)).
    lists:all(fun(Fs) -> helyes_mezo(KK,Fs) end,lists:concat(T)).

helyes_mezo(KK,Fs) ->
    length(lists:filter(fun erlang:is_integer/1,Fs)) < 2 andalso
        lists:all(fun(A) ->
                          length([1 || F <- Fs, F =:= A]) < 2
                  end, [s,w,e,o]) andalso
        lists:all(fun(F) ->
                          is_integer(F) andalso 1=<F andalso F=<KK
                              orelse lists:member(F, [s,w,e,o])
                              
                  end, Fs).

%% @spec ksudoku:sudoku_ki(FileOut::string(),SSols::[ssol()]) -> void()
%% @doc  Az SSols listában átadott Sudoku-megoldásokat kiírja
%%       a FileOut szövegfájlba.
%%
sudoku_ki(FileOut,SSols) ->
    case file:open(FileOut,write) of
        {ok, OutputFile}      -> ok;
        {error, OutputFile=R} -> throw({error,R,file:format_error(R)})
    end,
    tablak_ki(OutputFile,SSols),
    file:close(OutputFile).

tablak_ki(OutputFile,[SSol]) ->
    io:fwrite(OutputFile,"~s~n",[tabla_ki(SSol)]);
tablak_ki(OutputFile,[SSol|SSols]) ->
    io:fwrite(OutputFile,"~s~n~n",[tabla_ki(SSol)]),
    tablak_ki(OutputFile,SSols);
tablak_ki(OutputFile,[]) ->
    io:fwrite(OutputFile,"",[]).

tabla_ki([L]) ->
    sor_ki(L);
tabla_ki([L|Ls]) ->
    sor_ki(L) ++ "\n" ++ tabla_ki(Ls);
tabla_ki(_) ->
    [].

sor_ki([H|T]) ->
    I2L = fun(Int) -> io_lib:format(if length([H|T]) > 9 -> "~2w" ; true -> "~w" end, [Int]) end,
    I2L(H) ++ lists:foldr(fun(A,B) -> " " ++ I2L(A) ++ B end,"",T);
sor_ki([]) ->
    "".
%% @spec sudokusol_be(FileIn::string()) -> SSols::[ssol()].
%% @doc  SSpec a FileIn szövegfájlból beolvasott Sudoku-feladvány.
%%       Készítette: Eisenberger András.
sudokusol_be(FileSol) ->
    case file:open(FileSol,read) of
        {ok, InputFile}      -> ok;
        {error, InputFile=R} -> throw({error,R,file:format_error(R)})
    end,
    case io:read(InputFile, "") of
        {ok, Sol}       -> ok;
        {error, Sol=R2} -> throw({error,R2,file:format_error(R2)})
    end,
    file:close(InputFile),
    Sol.

%% @spec ksudoku:megold(FileIn::string(),FileOut::string()) -> void()
%% @doc  Beolvas egy feladványt a FileIn szövegfájlból és összes
%%       megoldását kiírja a FileOut szövegfájlba. Ehhez
%%       felhasználja a sudoku:sudoku/1 függvényt.
%%
megold(FileIn,FileOut) ->
    KT = sudoku_be(FileIn),
    SSols = sudoku:sudoku(KT), 
    sudoku_ki(FileOut,SSols).

%% @spec ksudoku:stopper(FileIn::string(),FileOut::string()) -> void()
%% @doc  Mint ksudoku:megold/2, de a végén kiírja a FileIn nevet,
%%       a megoldások számát és a futási idõt is.
stopper(FileIn,FileOut) -> stopper(FileIn,FileOut,"").

%% @spec ksudoku:stopper(FileIn::string(),FileOut::string(),FileSol::string()) -> void()
%% @doc  Mint ksudoku:megold/2, de a végén kiírja a FileIn nevet,
%%       a megoldások számát és a futási idõt is. Ha lehet, összeveti FileSol megoldással is.
stopper(FileIn,FileOut,FileSol) ->
    KT = sudoku_be(FileIn),
    erlang:garbage_collect(),timer:sleep(10),
    _T1 = statistics(runtime),
    SSols = sudoku:sudoku(KT),
    {_,T2} = statistics(runtime),
    sudoku_ki(FileOut,SSols),
    io:format("Feladvany: ~s, ", [FileIn]),
    try   % Eisenberger András kiegészítése
        case lists:sort(sudokusol_be(FileSol)) =:= lists:sort(SSols) of
            true  -> io:format("HELYES ");
            false -> io:format("ROSSZ ")
        end
    catch
        {error, _, _} -> error%io:format("NINCS MO ")
    end,
    io:format("megoldas: ~3w, futasi ido: ",
	      [length(SSols)]),
    if T2 >= 10000 -> io:format("~5w s\n", [ T2 div 1000 ]);
%       T2 >= 1000 -> io:format("~5.2f s\n", [ T2 / 1000 ]);
       true        -> io:format("~5w ms\n", [ T2 ])
    end.


%% ------- experimental -----


help() ->
    io:format(
      "teljes_teszt(Timeout): meg kiserleti stadiumban van."
      "Megoldja az osszes, tests/testXXXd.txt specifikaciot idolimittel.~n"
      "Pelda: futtatas bash-bol, 10s idolimit, megoldasok tests/testXXXt.txt fajlokba: ~n"
      "$ erl -noshell -eval 'ksudoku:teljes_teszt(10).' -s init stop~n"
     ).

%% @spec teljes_teszt(Timeout::integer()) -> [done|timeout].
%% @doc A 'tests' könyvtárban levõ összes "testXXXd.txt" tesztállomány esetén
%%  - lefuttatja a tesztet Timeout másodperces idõkorláttal,
%%  - ellenõrzi, hogy a testXXXs.txt állományban megadott megoldáshalmazt kapta,
%%  - olvasható formában (lásd megold/2) kiírja az eredményt a 'tests_out_er'
%%    könyvtár testXXXt.txt nevû állományába.
%% Az állománynevekben az XXX szám tetszõleges hosszúságú lehet.
teljes_teszt(Timeout) ->
    Dir = "tests/",
    ODir = "tests_out_er/",
    case file:make_dir(ODir) of
        ok              -> ok;
        {error, eexist} -> ok;
        {error, Rd}     -> throw({error,Rd,file:format_error(Rd)})
    end,
    case file:list_dir(Dir) of
        {error, R} ->
            throw({error,R,file:format_error(R)});
        {ok, Content} ->
            IOFiles = [{Dir++Input,ODir++Output,Dir++Solution}
                       || Input <- lists:sort(Content),
                          {match,_} <- [re:run(Input, "test[0-9]*d\.txt")],
                          Solution <- [re:replace(Input, "d\.txt\$", "s.txt", [{return, list}])],
                          Output <- [re:replace(Input, "d\.txt\$", "t.txt", [{return, list}])]
                      ],
            teszt(Timeout, IOFiles)
    end.

%% @spec teszt(Timeout::integer(),
%%            IOFiles::[{Input::string(),Output::string(),Solution::string()}]) -> [done|timeout].
%% @doc Megoldja az összes Input fájlban található specifikációt Timeout idõlimittel,
%%      megoldásokat az Output fájlokba teszi. Ha lehet, összeveti a megoldásokat Solution-nel.
teszt(Timeout, IOFiles) ->
    [begin
         process_flag(trap_exit, true),
         Worker = spawn_link(fun() -> stopper(Input, Output, Solution) end),
         Timer  = spawn_link(fun() -> timer:sleep(Timeout * 1000) end),
         receive
             {'EXIT', Worker, _Reason} -> 
                 exit(Timer, kill),
                 receive {'EXIT', Timer, killed} -> ok end, % feldolgozzuk a halalat
                 done;
             {'EXIT', Timer, _Reason} ->
                 exit(Worker, kill),
                 io:format("Feladvany: ~s -- Timeout (~p s)~n", [Input,Timeout]),
                 receive {'EXIT', Worker, killed} -> ok end, % feldolgozzuk a halalat
                 timeout
         end
     end
     || {Input, Output, Solution} <- IOFiles
    ].
