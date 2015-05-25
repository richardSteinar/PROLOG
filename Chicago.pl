
%struct de type Res|Num


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Remplacer 
remplacer([_|Q],1,Remplace,[Remplace|Q]).
remplacer([T|Q],X,Remplace,[T|Q2]):-
	Y is X -1,
	remplacer(Q,Y,Remplace,Q2).
	
%Supprimmer
supprimer([Supp|Q],1,Supp,Q).
supprimer([T|Q],I,Supp,[T|Q2]):-
	N is I-1,
	supprimer(Q,N,Supp,Q2).	
	
%Accès aux elem : comme en cours !!
marchandise([M,_,_,_,_],M).
bourse([_,B,_,_,_],B).
positionTrader([_,_,P,_,_],P).
reserveJoueur1([_,_,_,J1,_],J1).
reserveJoueur2([_,_,_,_,J2],J2).
	
%retour : marchandise 
%nth1 : retour elem a l'index de list
getM(P,Ress,March,Result):-
	nth1(P,March,NP),
	nth1(Ress, NP,Result).

%retour : pile	
getP(P,March,Result):-
	nth1(P,March,Result).	

%Supprime premier elem pile
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
printR([R,V]):-
	write(R),write(' : '),nl,write(V).	
	
printBourse([T|Q]):-
	printR(T),
	(Q == [] -> nl;
	printBourse(Q)).
	
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
		
		
printReserve([]):- write(' '),!.
printReserve(Reserve):-
		[T|Q] = Reserve,
		write(T),write(' '),
		printReserve(Q).
	
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


%Plateau : 

createP( [M,B,PT,_,_] ):-
	random(1,10,PT),
	B = [[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]],
	Res = [[ble,6],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]],
	createM(4,9,Res,M,_).
	

%Marchandies :  
createM(TailleP,1,Res,[P],ResN):-
	addRes(TailleP,Res,P,ResN).
	
createM(TailleP,NP,Res,[T|Q],ResN):- %on décrémente pour avoir le bon nombre de pile : modulaire ++
	NP > 1,
	N is NP - 1,
	addRes(TailleP,Res,T,NR),
	createM(TailleP,N,NR,Q,ResN).

%Comme le nom l'indique :)
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

checkMoveT(PDep,Move,NP,PEnd):-
	Move >= 1,
	Move =< 3,
	Res is PDep + Move,
	%struct pour if then else
	(Res =< NP ->
		PEnd is Res;
		(Res == 10 ->
		PEnd is 1;
		(Res == 11 -> 
		PEnd is 2;
		PEnd is 3))).
	

%Retour : pile autour trader
around(P,NP,I1,I2):-
	(P==NP -> 
		I2 is 1;
		I2 is P + 1),
	(P==1 ->
		I1 is 9;
		I1 is P - 1).

	
%Choix concerne achat : est Suivante ou Precedente
play([M,B,PT,J1,J2],[Joueur1,Choix, Move], [NM,NB,PEnd,NJ1,NJ2]):-

	length(M, Nb),
    checkMoveT(PT, Move, Nb, PEnd),
	around(PEnd, NbP, I1, I2),
	
	(Choix == S ->
		Achat is I2, Vente is I1;
		Achat is I1, Vente is I2),		
	
	getM(Achat,1, M, Achete),
	getM(Vente,1, M, Vendu).
	
	%changerbourse
	%changermarchandise
	%changerjoueur.
	
changerBourse([[EJete,Val]|Q],EJete,[[EJete,NewVal]|Q]):-NewVal is Val-1,!.
changerBourse([T|Q],EJete,[T|R]):-changerBourse(Q,EJete,R).

