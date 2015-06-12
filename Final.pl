
%struct de type Res|Num


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Remplace les éléments d'une liste par d'autres
remplacer([_|Q],1,Remplace,[Remplace|Q]).
remplacer([T|Q],X,Remplace,[T|Q2]):-
	Y is X -1,
	remplacer(Q,Y,Remplace,Q2).

%Supprimmer un element Supp de la liste
supprimer([Supp|Q],1,Supp,Q).
supprimer([T|Q],I,Supp,[T|Q2]):-
	N is I-1,
	supprimer(Q,N,Supp,Q2).

%Accès direct aux elem : comme en cours !!
marchandise([M,_,_,_,_],M).
bourse([_,B,_,_,_],B).
positionTrader([_,_,P,_,_],P).
reserveJoueur1([_,_,_,J1,_],J1).
reserveJoueur2([_,_,_,_,J2],J2).

%Retourne une ressource dans la pile P des marchandises
%nth1 : retour elem a l'index de list
getM(P,Ress,March,Result):-
	nth1(P,March,NP),
	nth1(Ress, NP,Result).

%retour : pile P des marchandises
getP(P,March,Result):-
	nth1(P,March,Result).

%Supprime premier elemement d'une pile, retourne l'état de la pile 
%bool pour check si encore des elem dans liste
supp1P(P,M,R,Result,Bool):-
	getP(P,M,NP),
	supprimer(NP,1,R,NP2),
	%struct if then else
	(NP2==[] ->
		supprimer(M,P,_,Result),
		Bool is 1;
		remplacer(M,P,NP2,Result),
		Bool is 0).

%Décrémentation	du nombre de ressource dans structure de type res|nombre
%retour : liste décrémentée
resMoinsMoins(List, Ress, Result):-
	nth1(A,List,[Ress,Num]),
	Num > 0,
	N is Num-1,
	remplacer(List,A,[Ress,N],Result).

%fonction non utiles
/*fixList([A, B, C, D, E, F, G, H, X, Y, Z, J], Result):-
	append([], A, Result1),
	append(Result1, [B], Result2),
	append(Result2, [C], Result3),
	append(Result3, [D], Result4),
	append(Result4, [E], Result5),
	append(Result5, [F], Result6),
	append(Result6, [G], Result7),
	append(Result7, [H], Result8),
	append([], [X], List1),
	append(List1, [Y], List2),
	append(List2, [Z], List3),
	append(List3, [J], List4),
	append(Result8, List4, Result).*/

%Récupérer la ressource dans res a partir du nom
/*getRes(Num, Res, Result):-
	(NombRes == 1 ->
			Result is ble;
			(NombRes == 2 ->
				Result is riz;
				(NombRes == 3 ->
						Result is cacao;
							(NombRes == 4 ->
								Result is cafe;
									(NombRes == 5 ->
										Result is sucre;
										Result is mais))))).*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%affiche une ressource
printR([R,V]):-
	write(R),write(' : '),write(V).
	
%affiche la bourse
printBourse([T|Q]):-
	printR(T),
	(Q == [] -> nl;
	printBourse(Q)).

%affiche les marchandises et trader
printPT(I,IMax,T,March):-
	N is I + 1,
	getM(I,1,March,R),
	write(I),write('['),write(R),
	(I==T ->
		write('  T is here !!');
		write('')),
	write(']'),write(' '),
	(I==IMax ->
		write(' ');
		printPT(N,IMax,T,March)).

%affiche la reserve d'un joueur
printReserve([]):- write(' '),!.
printReserve(Reserve):-
		[T|Q] = Reserve,
		write(T),write(' '),
		printReserve(Q).

%fonction d'affichage générale
printP([M,B,PT,J1,J2]):-
	length(M,NbP),
	nl,
	printBourse(B),
	nl,
	write('Reserve Joueur 1 :'),
	write(' [ '), printReserve(J1), write(' ] '),
	nl,
	write('Reserve Joueur 2 :'),
	nl,
	write(' [ '), printReserve(J2), write(' ] '),
	nl,
	write('Piles :'),
	nl,
	printPT(1,NbP,PT,M),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
%crée le plateau complet
createP( [M,B,PT,_,_] ):-
	random(1,10,PT),
	B = [[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]],
	Res = [[ble,6],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]],
	createM(4,9,Res,M,_).


%Crée Marchandises et sépare en pile :
createM(TailleP,1,Res,[P],ResN):-
	addRes(TailleP,Res,P,ResN).

createM(TailleP,NP,Res,[T|Q],ResN):- %on décrémente pour avoir le bon nombre de pile : modulaire ++
	NP > 1,
	N is NP - 1,
	addRes(TailleP,Res,T,NR),
	createM(TailleP,N,NR,Q,ResN).

%Ajoute les ressources aux piles
addRes(N,Res,[T|Q],ResN):-
	repeat,
	%rand 1-6
	random(1,7,R),
	nth1(R,Res,[T|_]),
	resMoinsMoins(Res,T, R2),
	%cut pour le reapeat -> red cut !!!!!!!!!
	!,
	(N > 1 ->
		X is N- 1,
		addRes(X,R2,Q,ResN);
		Q = [],
		ResN = R2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Check les déplacement du trader
checkMoveT(PDep,Move,M,PEnd):-
	Move >= 1,
	Move =< 3,
  length(M, NP),
	Res is PDep + Move,
	%struct pour if then else
	(Res =< NP ->
		PEnd is Res;
	PEnd is Res - NP).


%Retour : pile autour trader
around(P,M,I1,I2):-
length(M, NP),
	(P==NP ->
		I2 is 1;
		I2 is P + 1),
	(P==1 ->
		I1 is NP;
		I1 is P - 1).

%Partie est finie ?
gameOver(M):-
	length(M,L),
	L =< 2.

%Comptage des points
valeurReserveJoueur([],_,0).
valeurReserveJoueur([T|Q],B,O):-
	valeurReserveJoueur(Q,B,Total),
	nth1(_,B,[T,Valeur]),
	O is Valeur + Total.

%Change la bourse
changerBourse([[EJete,Val]|Q],EJete,[[EJete,NewVal]|Q]):-NewVal is Val-1,!.
changerBourse([T|Q],EJete,[T|R]):-changerBourse(Q,EJete,R).

%Change les marchandises
supprimerMarchandise(Pile,M,R,NewM,T):-
	getP(Pile,M,P),
	supprimer(P,1,R,NewP),
	(NewP==[] ->
		supprimer(M,P,_,NewM),
		T is 1;
		remplacer(M,Pile, NewP,NewM),
		T is 0).

%Joue un coup
%Choix concerne achat : est Suivante ou Precedente
/*play([M,B,PT,J1,J2],[Joueur, Choix, Move], [NM,NB,PEnd,NJ1,NJ2]):-

  checkMoveT(PT, Move, M, PBuf),
	around(PBuf, M, I1, I2),

	(Choix == Suivante ->
		Achat is I2, Vente is I1;
		Achat is I1, Vente is I2),

	getM(Achat,1, M, Achete),
	getM(Vente,1, M, Vendu),

  changerBourse(B, Vendu, NB),

  supprimerMarchandise(Achat, M, Achete, NMBuff, T1),
  (T1 = 1, I2 > I1 ->
	I2F is I2 - 1;
		I2F is I2),
  (T2 == 1, PBuf > I1 -> PBuf2 is PBuf -1;
		PBuf2 is PBuf),

  supprimerMarchandise(Vente, NMBuff, Vendu, NM, T2),
  (T2 == 1, PBuf2 > I2F  ->  PEnd is PBuf2 - 1;
		PEnd is PBuf2),

  (Joueur == 1 ->
		NJ1 = [ Achete | J1], NJ2 = J2;
		NJ2 = [ Achete | J2], NJ1 = J1).*/
		
play([M,B,PT,J1,J2],[Joueur, Move, Achat, Vente], [NM,NB,PEnd,NJ1,NJ2]):-
  checkMoveT(PT, Move, M, PBuf),
 
 around(PBuf, M, I1, I2),

  changerBourse(B, Vente, NB),

  supprimerMarchandise(I1, M, _, NMBuff, T1),
  (T1 = 1, I2 > I1 ->
	I2F is I2 - 1;
		I2F is I2),
  (T2 == 1, PBuf > I1 -> PBuf2 is PBuf -1;
		PBuf2 is PBuf),

  supprimerMarchandise(I2, NMBuff, _, NM, T2),
  (T2 == 1, PBuf2 > I2F  ->  PEnd is PBuf2 - 1;
		PEnd is PBuf2),

  (Joueur == 1 ->
		NJ1 = [ Achat | J1], NJ2 = J2;
		NJ2 = [ Achat| J2], NJ1 = J1).
		

%Boucle de jeu générale
loop(P,Joueur,TypeJ1,TypeJ2):-
	printP(P),
	/*Si c'est le tour du joueur 1*/
	(Joueur == 1 ->
		(TypeJ1 = 'h' ->
			coupH(P,Joueur,Coup);
			coupM(P,Joueur,Coup)),
		Next is 2;
	/*Si c'est le tour du joueur 2*/
		(TypeJ2 = 'h' ->
			coupH(P,Joueur,Coup);
			coupM(P,Joueur,Coup)),
		Next is 1),
		trace,
	play(P,Coup,NP),
	notrace,
	(gameOver(NP) -> printP(NP), fin(NP);
		loop(NP,Next, TypeJ1,TypeJ2)).

%Déroulement d'un coup pour joueur humain	
coupH(P,Joueur,Coup):-
	write('Tour du :'),write(Joueur),
	nl,
	repeat,
	write('Deplacement du Trader ?'),
	read_integer(Move),
	nl,
	integer(Move),
	/*P = [M,_,PT,_,_],*/
	marchandise(P, M),
	positionTrader(P, PT),
	checkMoveT(PT,Move,M,PFinal),
	nl,
	around(PFinal,M,I1,I2),
	getM(I1,1,M,R1),getM(I2,1,M,R2),
	write('Marchandises: [ '),write(R1),write(' ] et [ '),write(R2),write(' ]'),
	nl,
	write('Que gardez vous ?'),
	read(Achat),
	write('Que vendez vous ?'),
	read(Vente),
	Coup = [Joueur,Move,Achat,Vente].
	
fin([_,B,_,RJ1,RJ2]):-
	write('Fin Partie'),
	valeurReserveJoueur(RJ1,B,V1),
	valeurReserveJoueur(RJ2,B,V2),
	write('Le joueur 1 marque '), write(V1),
	write('Le joueur 2 marque '), write(V2),
	(V1 > V2 ->
		write('joueur 1 gagne');
		write('joueur 2 gagne!')).
		
coupM(P,Joueur,Coup):-
	write('coupM'),
	best(P,Joueur,Coup),
	Coup = [Joueur,Move,Achat, Vente],

	write('Le joueur '),write(Joueur),write('joue :'),
	write('Le trader est deplace de '),write(Move),
	write('La ressource Achetee est [ '),write(Achat),
	write('La ressource vendu est [ '); write(Vente).
	
coups(Plateau,Joueur,Liste):-
	cp(Plateau,Joueur,3,Liste).
	
simpleCoup([_,Move,Achat,Vente],[M,_,PT,_,_]):-
	write('simpleCoup'), nl,
	checkMoveT(PT,Move,M,PTFinal),
	around(PTFinal,M,I1,I2),
	getM(I1,1,M,R1),
	getM(I2,1,M,R2),
	(Achat = R1, Vente = R2;
	 Achat = R2, Vente = R1).
	
cp(P,Joueur,1,List):-
	write('cp1'), nl,
	Coup = [Joueur,1,R1,R2],
	simpleCoup(Coup,P),
	List = [[Joueur,1,R2,R1], Coup].
	
cp(P,Joueur,Move,List):-
    write('cp'), nl,
	Move > 1,
	NM is Move - 1,
	cp(P,Joueur,NM,PList),
	Coup = [Joueur,Move,R1,R2],
	simpleCoup(Coup,P),
	List = [Coup,[Joueur,Move,R2,R1]|PList].

meilleur(P, J, C):-
	write('best'), nl,
	coups(P, J, L),
	minmax(P, L, C, _).
	
minmax(P, [Coup|Q], Best, Value):-
	notrace,
	write('minmax'), nl,
	play(P,Coup,[_,B,_,RJ1,RJ2]),
	Coup = [Joueur|_],
	marchandise(P, NM),
	positionTrader(P, PT),
	NP = [NM,B,PT,RJ1,RJ2],
	(Joueur == 1 -> RJ = RJ1, RA = RJ2;
		RJ = RJ2, RA = RJ1),
	 printP(NP),
	 
	valeurReserveJoueur(RJ,B,SJ),
	valeurReserveJoueur(RA,B,SA),
	Val is SJ - SA,
	(Q == [] -> 
		Best = Coup, Value is Val, 
		minmax(P,Q,Coup2,Val2),
	(Val > Val2 -> 
		Best = Coup, Value is Val;
		Best = Coup2, Value is Val2)).
	
	
	





