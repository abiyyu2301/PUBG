use_module(library(random)).

player(100,0,none,0,9,0).

inventory([],4).

weapon(ak47, 40).
weapon(pistol, 25).
weapon(crossbow, 69).

ammo(ak47, 10).
ammo(pistol, 6).
ammo(crossbow, 1).

armor(armor1, 20).
armor(armor2, 40).
armor(armor3, 60).
armor(helmet1, 10).
armor(helmet2, 25).
armor(helmet3, 40).

medicine(daun, 15).
medicine(perban, 30).
medicine(p3k, 50).

deadzone(1).

:- dynamic posisi/3.

initPl() :- 
    random(1,11,Xp), random(1,11,Yp), assert(posisi(player,Xy,Yp)).

randomW(1, X) :- X = ak47.
randomW(2, X) :- X = pistol.
randomW(3, X) :- X = crossbow.


initEn(X) :- 
    random(1,11,Xe), random(1,11,Ye), random(1,4,We), randomW(We, S), assert(enemy(Xe,Ye,S)),
    Y is X -1, initEn(Y).
initEn(1) :- 
    random(1,11,Xe), random(1,11,Ye), random(1,4,We), randomW(We, S), assert(enemy(Xe,Ye,S)).

lihat :-
    posisi(player,X,Y),
    L is X - 1,
    R is X + 1,
    T is Y - 1,
    B is Y + 1, 
    tulislihat(L,T), tulislihat(X,T), tulislihat(R,T), nl,
    tulislihat(L,X), tulislihat(X,X), tulislihat(R,X), nl,
    tulislihat(L,B), tulislihat(X,B), tulislihat(R,B), nl.

tulisbaris(10) :- 
    tulistitikpeta(10,1), 
    tulistitikpeta(10,2),
    tulistitikpeta(10,3), 
    tulistitikpeta(10,4),
    tulistitikpeta(10,5), 
    tulistitikpeta(10,6),
    tulistitikpeta(10,7), 
    tulistitikpeta(10,8),
    tulistitikpeta(10,9), 
    tulistitikpeta(10,10),
    nl. 

tulisbaris(X) :- 
    tulistitikpeta(X,1), 
    tulistitikpeta(X,2),
    tulistitikpeta(X,3), 
    tulistitikpeta(X,4),
    tulistitikpeta(X,5), 
    tulistitikpeta(X,6),
    tulistitikpeta(X,7), 
    tulistitikpeta(X,8),
    tulistitikpeta(X,9), 
    tulistitikpeta(X,10),
    nl,
    Y is X + 1,
    tulisbaris(Y).

tulistitikpeta(X,Y) :- deadzone(D), B is 11 - Y, K is 11 - X, (X < D ; Y < D ; B < D ; K < D), write(' X'),!.
tulistitikpeta(X,Y) :- posisi(player,X,Y), write(' P'),!.
tulistitikpeta(_,_) :- write(' _').

tulislihat(X,Y) :- deadzone(D), B is 11 - Y, K is 11 - X, (X < D ; Y < D ; B < D ; K < D), write(' X'),!.
tulislihat(X,Y) :- posisi(enemy,X,Y), write(' E'),!.
tulislihat(X,Y) :- (posisi(daun,X,Y); posisi(perban,X,Y); posisi(p3k,X,Y)), write(' M'),!.
tulislihat(X,Y) :- (posisi(ak47,X,Y); posisi(pistol,X,Y); posisi(crossbow,X,Y)), write(' W'),!.
tulislihat(X,Y) :- (posisi(armor1,X,Y); posisi(armor2,X,Y); posisi(armor3,X,Y)), write(' D'),!.
tulislihat(X,Y) :- (posisi(helmet1,X,Y); posisi(helmet2,X,Y); posisi(helmet3,X,Y)), write(' D'),!.
tulislihat(X,Y) :- (posisi(ak47Ammo,X,Y); posisi(pistolAmmo,X,Y); posisi(crossbowAmmo,X,Y)), write(' A'),!.
tulislihat(X,Y) :- posisi(player,X,Y), write(' P'),!.
tulislihat(_,_) :- write(' _').


input(help) :-
    write('drop     - menjatuhkan item dari inventory'), nl,
    write('take     - mengambil item dari map'), nl,
    write('save     - menyimpan status game'), nl,
    write('load     - melanjutkan status game yang telah disimpan sebelumnya'), nl,
    write('status   - menampilkan statu permainan'), nl,
    write('look     - menampilkan wilayah 3x3 disekitar pemain'), nl,
    write('map      - menampilkan status peta'), nl,
    write('attack   - menyerang enemy yang berada di kotak yang sama dengan pemain'), nl,
    write('n        - menggerakkan pemain ke arah Utara (atas)'), nl,
    write('e        - menggerakkan pemain ke arah Timur (kanan)'), nl,
    write('s        - menggerakkan pemain ke arah Selatan (bawah)'), nl,
    write('w        - menggerakkan pemain ke arah Barat (kiri)'), nl,
    write('quit     - menghentikan permainan (jangan lupa untuk menyimpan permainan)'),nl.
input(look) :- lihat().
input(map) :- tulisbaris(1).
input()

loop() :- 
    read(I),
    input(I),
    loop().


initial :- 
    write('Selamat datang, untuk sekarang catetannya ini dulu'), nl,
    read(I),
    input(I),
    loop().