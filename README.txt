Tartalom:

ksudoku.pl	Prolog keretprogram
ksudoku.erl	Erlang keretprogram
p_idok.txt	Prolog futasi idők
e_idok.txt	Erlang futasi idők
README.txt	Leírás (ez a fájl)
tests/		Tesztesetek es megoldások

A futasi időket általunk viszonylag gyorsnak vélt algoritmusokkal mértük,
feladatonként 2 perces időkorláttal, egy alábbi tipusú gépen:
model name  : Intel(R) Core(TM) i3-2120 CPU @ 3.30GHz

A méréshez az alábbi parancsok kimenetét használtuk:

erl -noshell -eval 'ksudoku:teljes_teszt(120).' -s init stop

sicstus --nologo --noinfo -l ksudoku.pl --goal 'teljes_teszt(120).'

Ahol a teljes_teszt(Timeout) eljárás/függvény a 'tests' könyvtárban levő
összes "testXXXd.txt" tesztállomány esetén
- lefuttatja a tesztet Timeout másodperces időkorláttal,
- ellenőrzi, hogy a testXXXs.txt állományban megadott megoldáshalmazt kapta
- olvasható formában kiírja az eredményt a 'tests_out'
  könyvtár testXXXt.txt nevű állományába.
Az állománynevekben az XXX szám tetszőleges hosszúságú lehet.


$LastChangedDate: 2013-10-09 16:25:22 +0200 (sze, 09 okt 2013) $
