%Cara menyatukan ke kode utama:
%1. input(save) dan input(load) ditaruh didekat semua command input
%	yang lain.
%2. Sementara deadzone dan inventory belum dapat diimplementasikan
%	save dan loadnya. Jadi nggak usah ikut di copas.
%
%=============================================================
%=========== Semua Kode Untuk Save ===========================
%=============================================================
input(save) :-
	write('Masukkan nama file: '),nl,
	read(NamaFile),
	saveFile(NamaFile),
	loop.

player(100,25,ak47,5,8,7).
posisi(player,5,5).
posisi(enemy,2,3).
posisi(enemy,4,5).
posisi(enemy,6,9).
posisi(enemy,7,3).
posisi(enemy,8,2).
posisi(daun,3,5).
posisi(ak47Ammo,2,6).
enemy(1,2,3,ak47).
enemy(4,4,5,crossbow).
enemy(6,6,9,pistol).
enemy(7,7,3,crossbow).
enemy(13,8,2,ak47).

saveFile(NamaFile) :-
	open(NamaFile,write,Stream),
	write(Stream,'statusPlayer.\n'),
	writeStatus(Stream),
	%inventory([H|T],Max),
	%write(Stream,Max),write(Stream,'.'),write(Stream,'\n'),	%AKAN DIUBAH
	%writeInventory(Stream,[H|T]),
	writePosisi(Stream,1,1),
	writeEnemy(Stream,1),
	close(Stream).

writeStatus(Stream) :-
	player(Hp,Ar,We,Am,En,Ki),
	write(Stream,Hp),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,Ar),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,We),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,Am),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,En),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,Ki),write(Stream,'.'),write(Stream,'\n').

writeInventory(Stream,[]) :- !.
	
writeInventory(Stream,[H|T]) :-
	write(Stream,H),write(Stream,'.'),write(Stream,'\n'),
	writeInventory(Stream,T,Max).
	
writePosisi(Stream,X,Y) :-
	posisi(Entity,X,Y),
	write(Stream, 'posisiGame.\n'),
	write(Stream,Entity),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,X),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,Y),write(Stream,'.'),write(Stream,'\n'),
	Yn is Y + 1,
	writePosisi(Stream,X,Yn).
	
writePosisi(Stream,X,Y) :-
	\+(posisi(Entity,X,Y)),fail.	

writePosisi(Stream,X,Y) :-
	X > 10,!.

writePosisi(Stream,X,Y) :-
	Y>10,
	Yn is 1,
	Xn is X + 1,
	writePosisi(Stream,Xn,Yn).
	
writePosisi(Stream,X,Y) :-
	Yn is Y + 1,
	writePosisi(Stream,X,Yn).

writeEnemy(Stream,X) :- 
	player(Hp,Ar,We,Am,En,Ki),
	X > En,!.

writeEnemy(Stream,Id) :-
	\+(enemy(Id,X,Y,We)),fail.

writeEnemy(Stream,Id):-
	enemy(Id,X,Y,We),
	write(Stream, 'statusEnemy.\n'),
	write(Stream,Id),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,X),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,Y),write(Stream,'.'),write(Stream,'\n'),
	write(Stream,We),write(Stream,'.'),write(Stream,'\n'),
	Nid is Id + 1,
	writeEnemy(Stream,Nid).

writeEnemy(Stream,Id) :-
	Nid is Id + 1,
	writeEnemy(Stream,Nid).
	
%===============================================================
%=============== Semua Code Untuk Input load ===================
%===============================================================

input(load) :-
	write('Masukkan nama file: '),
	read(NamaFile),
	loadFile(NamaFile),
	loop.

loadFile(NamaFile) :-
	open(NamaFile,read,Inp),
	read(Inp,X),
	loadInfo(Inp,X),
	close(Inp).

loadInfo(Inp,X):-
	at_end_of_stream(Inp),!.

loadInfo(Stream,statusPlayer) :-
	read(Stream,Hp),
	read(Stream,Ar),
	read(Stream,We),
	read(Stream,Am),
	read(Stream,En),
	read(Stream,Ki),
	assertz(player(Hp,Ar,We,Am,En,Ki)),
	read(Stream,NextInp),
	loadInfo(Stream,NextInp).
	
loadInfo(Stream, posisiGame) :-
	read(Stream,Entity),
	read(Stream,X),
	read(Stream,Y),
	assertz(posisi(Entity,X,Y)),
	read(Stream,NextInp),
	loadInfo(Stream,NextInp).

loadInfo(Stream, statusEnemy) :-
	read(Stream, Id),
	read(Stream, X),
	read(Stream, Y),
	read(Stream, We),
	assertz(enemy(Id,X,Y,We)),
	read(Stream,NextInp),
	loadInfo(Stream,NextInp).
	
%loadInfo(Stream,inventory) :-
%	loadInven(Stream).

%loadInven(Stream,Inven,Max) :-
%	read(Stream,NextInp),	%Ketika sudah habis membaca inventory
%	loadInfo(Stream,X),
%	X == NextInp,!,
%	loadInfo(Stream,Next,Inp).
	
%loadInven(Stream) :-
%	read(Stream,Item),
%	inventory(Y,_),		%BAKAl DIGANTI KAYAKNYA
%	addInventory(X,Y),  %INI JUGA
%	loadInven(Stream).
