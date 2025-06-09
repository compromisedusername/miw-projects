:- dynamic wspolrzedne/4.
:- dynamic rozmiar/3.
:- dynamic w/2.

dodaj_obiekt(Nazwa, X, Y, Z, Szerokosc, Wysokosc) :-
  \+ czy_istnieje(Nazwa),
  \+ wspolrzedne(_, X, Y, Z).
  assertz(rozmiar(Nazwa, Szerokosc, Wysokosc)),
  assertz(w(Nazwa, pokoj)),
  assertz(wspolrzedne(Nazwa, X,Y,Z)),
  \+ czy_kolizja_xyz(Nazwa, _).

czy_istnieje(Obiekt) :-
  w(Obiekt, pokoj).




przesun_obiekt(Obiekt, DX, DY, DZ) :-
    wspolrzedne(Obiekt, X0, Y0, Z0),
    X1 is X0 + DX,
    Y1 is Y0 + DY,
    Z1 is Z0 + DZ,
    \+ (czy_kolizja_xyz(X1, Y1, Z1, Inny), Inny \= Obiekt),
    retract(wspolrzedne(Obiekt, X0, Y0, Z0)),
    assertz(wspolrzedne(Obiekt, X1, Y1, Z1)),
    czy_nie_wychodzi_za_pokoj(Obiekt).


ruch(X, Y, NX, NY) :-
  member(DX, [-1, 0, 1]),
  member(DY, [-1, 0, 1]),
  (DX =\= 0 ; DY =\= 0),
    NX is X + DX,
    NY is Y + DY.

mozna_wejsc(X,Y, Z) :-
  \+ (czy_kolizja_xyz(X,Y,Z, Inny), Inny \= mysz),
  wspolrzedne(pokoj, XP, YP, _),
  rozmiar(pokoj, DX, DY),
  X >= XP, X =< DX + XP,
  Y >= YP, Y < DY + YP.


wykonaj_ruch(1,0) :- idz_prawo.
wykonaj_ruch(-1, 0) :- idz_lewo.
wykonaj_ruch(0, 1) :- idz_gora.
wykonaj_ruch(0, -1) :- idz_dol.
wykonaj_ruch(1, 1) :- idz_prawo, idz_gora.
wykonaj_ruch(-1, 1) :- idz_lewo, idz_gora.
wykonaj_ruch(1, -1) :- idz_prawo, idz_dol.
wykonaj_ruch(-1, -1) :- idz_lewo, idz_dol.

w(lozko, pokoj).
w(monitor1, pokoj).
w(monitor2, pokoj).
w(biurko, pokoj).
w(lampa, pokoj).
w(telewizor, pokoj).
w(kredens, pokoj).
w(szafa, pokoj).
w(podkladka, pokoj).
w(myszka, pokoj).
w(stolik, pokoj).
w(drzwi, pokoj).
w(krzeszlo, pokoj).
w(poduszka, pokoj).
w(mysz, pokoj).


rozmiar(pokoj, 40,30).
rozmiar(lozko, 18,9).
rozmiar(monitor1,9,1 ).
rozmiar(monitor2,9,1 ).
rozmiar(biurko, 22,7).
rozmiar(lampa,1,1 ).
rozmiar(telewizor,1,20 ).
rozmiar(kredens,5,5 ).
rozmiar(szafa,5,12 ).
rozmiar(podkladka,7,3).
rozmiar(myszka, 1,1).
rozmiar(stolik, 3,3).
rozmiar(drzwi, 9, 1).
rozmiar(krzeszlo, 7,3).
rozmiar(poduszka, 7, 3).
rozmiar(mysz,1,1).

wspolrzedne(pokoj, 0,0,0).
wspolrzedne(lozko,22,20, 1).
wspolrzedne(monitor1,19,1,2 ).
wspolrzedne(monitor2,29,1,2 ).
wspolrzedne(biurko,18,0,1 ).
wspolrzedne(lampa,37,18,2 ).
wspolrzedne(telewizor,0,8,2 ).
wspolrzedne(kredens,16,25,1 ).
wspolrzedne(szafa,0,0,0 ).
wspolrzedne(podkladka,21,3,2 ).
wspolrzedne(myszka,21,2,3 ).
wspolrzedne(stolik,36,15,1 ).
wspolrzedne(drzwi,6,0,1 ).
wspolrzedne(krzeszlo,26,7,1 ).
wspolrzedne(mysz, 7,0,1).



czy_kolizja_xyz(Obiekt1, Obiekt2) :-
  Obiekt1 \= Obiekt2,
  czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
  wspolrzedne(Obiekt1, _,_,Z1),
  wspolrzedne(Obiekt2, _,_,Z2),
  Z1 =:= Z2,
  !.




czy_nie_wychodzi_za_pokoj(Obiekt1) :-
  wspolrzedne(pokoj, X1, Y1, _),
  wspolrzedne(Obiekt1, X2, Y2, _),
  rozmiar(pokoj, DX1, DY1),
  rozmiar(Obiekt1, DX2, DY2),
  X2+DX2 =< X1+DX1, Y2 + DY2 =< Y1 + DY1,
  X2 >= X1, Y2 >= Y1.




czy_czesc_wspolna_xy(Obiekt1, Obiekt2) :-
  wspolrzedne(Obiekt1, X1,Y1, _),
  wspolrzedne(Obiekt2, X2, Y2,_ ),
  rozmiar(Obiekt2, DX2, DY2),
  rozmiar(Obiekt1, DX1, DY1),
  X1 + DX1 >= X2, X1 =< X2 + DX2, Y1 + DY1 >= Y2, Y1 =< Y2 +DY2.


czy_czesc_wspolna_y(Obiekt1, Obiekt2) :-
  wspolrzedne(Obiekt1,_ ,Y1, _),
  wspolrzedne(Obiekt2,_ , Y2,_ ),
    rozmiar(Obiekt1,_ , DY1),
    rozmiar(Obiekt2,_ , DY2),
    Y1 < Y2 + DY2,
    Y2 < Y1 + DY1.

czy_czesc_wspolna_x(Obiekt1, Obiekt2) :-
  wspolrzedne(Obiekt1, X1,_, _),
  wspolrzedne(Obiekt2, X2, _, _),
    rozmiar(Obiekt1, DX1, _),
    rozmiar(Obiekt2, DX2, _),
    X1 < X2 + DX2,
    X2 < X1 + DX1.


lezy_na(Obiekt1, Obiekt2) :-
    czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
    wspolrzedne(Obiekt1, _,_, Z1),
    wspolrzedne(Obiekt2, _,_, Z2),
    Z1 - Z2 =:= 1.

nad(Obiekt1, Obiekt2) :-
    czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
    wspolrzedne(Obiekt1, _,_, Z1),
    wspolrzedne(Obiekt2, _,_, Z2),
    Z1 > Z2.

pod(Obiekt1, Obiekt2) :-
    czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
    wspolrzedne(Obiekt1, _,_, Z1),
    wspolrzedne(Obiekt2, _,_, Z2),
    Z1 < Z2.

na_prawo(Obiekt1, Obiekt2) :-
    \+ czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
    wspolrzedne(Obiekt1, X1,_, _),
    wspolrzedne(Obiekt2, X2,_, _),
    czy_czesc_wspolna_y(Obiekt1, Obiekt2),
    X1 > X2.

na_lewo(Obiekt1, Obiekt2) :-
  \+ czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
    wspolrzedne(Obiekt1, X1,_, _),
    wspolrzedne(Obiekt2, X2,_, _),
    czy_czesc_wspolna_y(Obiekt1, Obiekt2),
    X1 < X2.

na_dole(Obiekt1, Obiekt2) :-
    \+ czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
   \+ czy_czesc_wspolna_x(Obiekt1, Obiekt2),
   wspolrzedne(Obiekt1,  _, Y1, _),
   wspolrzedne(Obiekt2,  _, Y2, _),
   Y1 < Y2.

na_gorze(Obiekt1, Obiekt2) :-
    \+ czy_czesc_wspolna_xy(Obiekt1, Obiekt2),
   \+ czy_czesc_wspolna_x(Obiekt1, Obiekt2),
   wspolrzedne(Obiekt1,  _, Y1, _),
   wspolrzedne(Obiekt2,  _, Y2, _),
   Y1 > Y2.

pomiedzy(Obiekt1, Obiekt2, Obiekt3) :-
   (
   (
     (na_lewo(Obiekt1, Obiekt2), na_prawo(Obiekt1, Obiekt3)); (na_prawo(Obiekt1, Obiekt2), na_lewo(Obiekt1, Obiekt3))
   )
   ; (
     (na_dole(Obiekt1, Obiekt2), na_gorze(Obiekt1, Obiekt3));(na_dole(Obiekt1, Obiekt3); na_gorze(Obiekt1, Obiekt2))
   )).


zakresy_sie_pokrywaja(P1, L1, P2, L2) :-
    P1 < P2 + L2,
    P2 < P1 + L1.


obok(Obiekt1, Obiekt2) :-
    wspolrzedne(Obiekt1, X1, Y1, _),
    wspolrzedne(Obiekt2, X2, Y2, _),
    rozmiar(Obiekt1, DX1, DY1),
    rozmiar(Obiekt2, DX2, DY2),

    (
        zakresy_sie_pokrywaja(Y1, DY1, Y2, DY2),
        (X1 + DX1 =:= X2 ; X2 + DX2 =:= X1)
    ;
        zakresy_sie_pokrywaja(X1, DX1, X2, DX2),
        (Y1 + DY2 =:= Y2 ; Y2 + DY2 =:= Y1)
    ).

















