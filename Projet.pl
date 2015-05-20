:- dynamic(goods/1).
:- dynamic(stockExchange/1).
:- dynamic(trader/1).
:- dynamic(joueur1/1).
:- dynamic(joueur2/1).
:- dynamic(tourDeJeu/1).


%DECLARATION DES PREDICATS UTILISES POUR LE JEU
tourDeJeu([]). %défini le tour de jeu
goods([]). %défini les marchandises
stockExchange([]). %défini la bourse
trader(0). %gestion du trader
joueur1([]). %gestion du Joueur 1
joueur2([]). %gestion du Joueur 2

%Initialisation : utiliser asserta pour mettre dans une base de donnée et retract retractall pour retirer les éléments déjà présent (en cas de plusiers parties successives)

start:-
	retractall(tourDeJeu(_)), %supression des données précédentes potetentielles
	retractall(stockExchange(_)),
	retractall(trader(_)),
	retractall(goods(_)),
	
	asserta(stockExchange([[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]])),%création de la bourse
	asserta(distribution([[ble,6],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]])),%création d'une variable de stockage pour distribuer les biens

	random(1,10,RandNb), 
	asserta(positionTrader(RandNb)), %on démarre à une position aléatoire
	
	distribuer(4,9,M),
	asserta(goods(M)),
	
	retractall(distribution(_)),	
	asserta(tourDeJeu(1)),
	
	printStock.
	

%distribution des marchandises dans pile

distribuer(TaillePile,1,[H|[]]):-	%sortie de la récursivité
	addToPileMarchandises([],TaillePile,H).
	
distribuer(TaillePile,Nb,[H|T]):-	%
	Nb > 1,
	N is Nb-1,
	shuffle([],TaillePile,H),
	distribuer(TaillePile,NewN,T).

%shuffle de la pile	

del(L, R, F):- %décrémentation d'un elem dans pile
	nth1(X,L,[R,V]),
	V > 0,
	NV is V-1,
	wover(L,X,[R,NV],F).
	
wover([_|T],1,R,[R|T]). %write over liste
wover([S|T],X,R,[S|Q]):-
	K is X -1,
	remplacer(T,K,R,Q).

shuffle(L,1,[K|L]):-
	repeat, %boucle
	random(1,7,R),
	distribution(D),
	nth1(R,NbR,L), %renvoie l'elem à l'index R dans nbr
	nth1(1, L, E)
	del(D,K,ND), 
	retractall(nbRessources(_)),
	asserta(nbRessources(ND)).
	
shuffle(E,N,[R|S]):-
	N > 1,
	Z is N-1,
	repeat,
	random(1,7,R),
	distribution(D),
	nth1(R,D,[R|_]),
	del(D,R,ND),
	retractall(nbRessources(_)),
	asserta(nbRessources(ND)),
	shuffle(E,Z,S).
	
%Affichage : 
	
printStock([T|Q]):-
	printDuet(T),
	nl,
	( Q == [] -> nl; %structure if then else
	afficherBourse(Q)).
	
printFuet([M,V]):-
	write(M),write(' | '),write(V).	

printJ(R):-
	(R == [] -> write(' ');
	[H|T] = Reserve,
	write(H),write(' '),
	printJ(T)).

