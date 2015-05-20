/*############################################################*/
/*#                                                          #*/
/*#           IA02 : Chicago stock exchange                  #*/
/*#          Damien Delmas & Hachem Benyahia                 #*/
/*#                                                          #*/
/*############################################################*/


/*###############Boucle de jeu principale#####################*/

startGame:-
	writeln('Bienvenue sur le jeu Chicago Stock Exchange !'),
	nl,
	createPlateau(P),
	repeat,
	writeln('Entrez le type du joueur 1 (\'humain\' pour un humain et \'ordi\' pour un ordinateur) :'),
	read(Type1),
	(Type1 == 'humain'; Type1 == 'ordi'),
	repeat, /* ajout de ce repeat pour ne pas revenir au joueur 1 si on se trompe en tapant le joueur 2 */
	writeln('Entrez le type du joueur 2 (\'humain\' pour un humain et \'ordi\' pour un ordinateur) :'),
	read(Type2),
	(Type2 == 'humain'; Type2 == 'ordi'),
	bouclePrincipale(P,1,Type1,Type2).
	
bouclePrincipale(PI,Joueur,TypeJoueur1,TypeJoueur2):-
	affiche_plateau(PI),
	/*Si c'est le tour du joueur 1*/
	(Joueur == 1 ->
		(TypeJoueur1 = 'humain' ->
			coupHumain(PI,Joueur,Coup);
			coupMachine(PI,Joueur,Coup)),
		NextJoueur is 2;
	/*Si c'est le tour du joueur 2*/
		(TypeJoueur2 = 'humain' ->
			coupHumain(PI,Joueur,Coup);
			coupMachine(PI,Joueur,Coup)),
		NextJoueur is 1),
	jouer_coup(PI,Coup,PF),
	(isGameFinished(PF) -> affiche_plateau(PF),finishGame(PF);
		bouclePrincipale(PF,NextJoueur,TypeJoueur1,TypeJoueur2)).

coupMachine(Plateau,Joueur,Coup):-
	meilleur_coup(Plateau,Joueur,Coup),
	Coup = [Joueur,Deplacement,RGardee,RVendue],
	write('Le joueur '),write(Joueur),writeln(' (Intelligence Artificielle) a joue le coup suivant :'),
	write('		-Le trader est deplace de '),write(Deplacement),writeln(' piles.'),
	write('		-La ressource [ '),write(RGardee),writeln(' ] est gardee.'),
	write('		-La ressource [ '),write(RVendue),writeln(' ] est vendue.').
		
coupHumain(PlateauI,Joueur,Coup):-
	write('C\'est au joueur '),write(Joueur),writeln(' de jouer :'),
	repeat,
	writeln('De combien de cases deplacez vous le trader ?'),
	read(Deplacement),
	integer(Deplacement),
	PlateauI = [M,_,PT,_,_],
	moveTrader(PT,Deplacement,M,PFinal),
	nl,
	writeln('# Le trader est maintenant a cet endroit :'),
	writeln('#'),
	length(M,NbPiles),
	write('# '),afficherPilesEtTrader(1,NbPiles,PFinal,M), nl,
	writeln('#'),
	aroundTrader(PFinal,M,I1,I2),
	getMarchandise(I1,1,M,R1),getMarchandise(I2,1,M,R2),
	write('# Vous pouver acceder aux ressources suivantes : [ '),write(R1),write(' ] et [ '),write(R2),writeln(' ]'),
	nl,
	writeln('Quelle ressource gardez vous ?'),
	read(RGardee),
	writeln('Quelle ressource vendez vous ?'),
	read(RVendue),
	coup_possible([_,Deplacement,RGardee,RVendue],PlateauI),
	Coup = [Joueur,Deplacement,RGardee,RVendue].
	

finishGame([_,B,_,RJ1,RJ2]):-
	writeln('La partie est terminée'),
	valeurReserve(RJ1,B,V1),
	valeurReserve(RJ2,B,V2),
	write('Le joueur 1 marque '), write(V1), writeln(' points.'),
	write('Le joueur 2 marque '), write(V2), writeln(' points.'),
	(V1 > V2 ->
		writeln('Le joueur 1 remporte la partie !');
		writeln('Le joueur 2 remporte la partie !')).


/*############################################################*/




/*##################Fonctions de Jeu##########################*/

/*Determine si un coup est possible ou pas*/
coup_possible([_,Deplacement,RGardee,RVendue],[M,_,PTrader,_,_]):-
	moveTrader(PTrader,Deplacement,M,PTraderFinal),
	aroundTrader(PTraderFinal,M,I1,I2),
	getMarchandise(I1,1,M,R1),
	getMarchandise(I2,1,M,R2),
	(RGardee = R1, RVendue = R2;
	RGardee = R2, RVendue = R1).
	
/*Determine l'état du plateau après que l'on ait joué un coup*/
jouer_coup([M,B,PT,RJ1,RJ2],[Joueur,Deplacement,RGardee,RVendue],[NewM,NewB,NewPT,NewRJ1,NewRJ2]):-
	moveTrader(PT,Deplacement,M,PTTemp),
	aroundTrader(PTTemp,M,I1,I2),
	supprimerMarchandise(I1,M,_,MTemp,Flag1),
	(Flag1 == 1, I2 > I1 -> I2F is I2 - 1;  /*On verifier les flags pour replacer le trader et les indinces au bon endroit*/
		I2F is I2),
	(Flag1 == 1, PTTemp > I1 -> PTTemp2 is PTTemp -1;
		PTTemp2 is PTTemp),
	supprimerMarchandise(I2F,MTemp,_,NewM,Flag2),
	(Flag2 == 1, PTTemp2 > I2F  ->  NewPT is PTTemp2 - 1;    
		NewPT is PTTemp2),
	decrementRessource(B,RVendue,NewB),
	(Joueur == 1 ->
		NewRJ1 = [ RGardee | RJ1], NewRJ2 = RJ2;
		NewRJ2 = [ RGardee | RJ2], NewRJ1 = RJ1).

/*Determine si la partie est finie ou non*/
isGameFinished([M|_]):-
	length(M,L),
	L =< 2.

/*Retourne la valeur d'une réserve en fonction de la valeur de la bourse*/
valeurReserve([],_,0).
valeurReserve([R|T],B,Total):-
	valeurReserve(T,B,TotalReste),
	nth1(_,B,[R,Valeur]),
	Total is Valeur + TotalReste.

	
/*############################################################*/




/*#########Fonctions pour l'intelligence artificielle#########*/

/*Determine le meilleur coup à jouer*/
meilleur_coup(Plateau,Joueur,Coup):-
	coups_possibles(Plateau,Joueur,ListeCoups),
	meilleurCoup2(Plateau,ListeCoups,Coup,_).

/*Determine de manière récursive le meilleur coup à joueur*/
meilleurCoup2(Plateau,[Coup1|Rest],BestMove,BestVal):-
	/*Calcule la valeur d'un coup*/
	jouer_coup(Plateau,Coup1,[_,B,_,RJ1,RJ2]),
	Coup1 = [Joueur|_],
	(Joueur == 1 -> RJoueur = RJ1, RAdverse = RJ2;
		RJoueur = RJ2, RAdverse = RJ1),
	valeurReserve(RJoueur,B,ScoreJoueur),
	valeurReserve(RAdverse,B,ScoreAdverse),
	Val1 is ScoreJoueur - ScoreAdverse,
	/*Determine lequel des coups a la valeur la plus important*/
	(Rest == [] -> 	BestMove = Coup1, BestVal is Val1;
		meilleurCoup2(Plateau,Rest,Coup2,Val2),
		(Val1 > Val2 -> BestMove = Coup1, BestVal is Val1;
			BestMove = Coup2, BestVal is Val2)).	

/*Determine tous les coups possibles en fonctions de l'état du jeu*/
coups_possibles(Plateau,Joueur,Liste):-
	coupsPossibles2(Plateau,Joueur,3,Liste).

/*Determine tous les coups possibles de manière récursive*/	
coupsPossibles2(Plateau,Joueur,1,List):-
	Coup1 = [Joueur,1,R1,R2],
	coup_possible(Coup1,Plateau),
	List = [[Joueur,1,R2,R1], Coup1].
coupsPossibles2(Plateau,Joueur,Deplacement,List):-
	Deplacement > 1,
	NewD is Deplacement - 1,
	coupsPossibles2(Plateau,Joueur,NewD,PreList),
	Coup1 = [Joueur,Deplacement,R1,R2],
	coup_possible(Coup1,Plateau),
	List = [Coup1,[Joueur,Deplacement,R2,R1]|PreList].

/*############################################################*/




/*##################Fonctions de Service######################*/

/*retourne la marchandises spécifiée dans la pile spécifiée)*/
getMarchandise(I_Pile,I_Ressource,Marchandises,Ressource):-
	nth1(I_Pile,Marchandises,Pile),
	nth1(I_Ressource,Pile,Ressource).

/*Récupère la pile spécifiée*/	
getPile(I_Pile,M,Pile):-
	nth1(I_Pile,M,Pile).	
	
/*Permet de décrementer la valeur des ressources de la bourse ou bien lors de la génération des marchandises*/	
decrementRessource(List, Ressource, NewList):-
	nth1(X,List,[Ressource,Val]),
	Val > 0,
	NewVal is Val-1,
	remplacer(List,X,[Ressource,NewVal],NewList).

/*Fonctions d'acces aux éléments du plateau*/	
bourse([_,B,_,_,_],B).
marchandises([M,_,_,_,_],M).
positionTrader([_,_,PTrader,_,_],PTrader).
reserveJoueur1([_,_,_,RJ1,_],RJ1).
reserveJoueur2([_,_,_,_,RJ2],RJ2).

/*Verifie que le trader peut bien se deplacer d'autant de case et renvoi sa nouvelle position*/	
moveTrader(PInit,Deplacement,Marchandises,PFinal):-
	Deplacement =< 3,
	Deplacement >= 1,
	length(Marchandises,L),
	Res is PInit + Deplacement,
	(Res =< L ->
		PFinal is Res;
		PFinal is Res - L).
		
/*Supprime un élément du haut d'une pile (Le flag vaut un lorsqu'une pile disparait et 0 dans le cas contraire.)*/
supprimerMarchandise(IPile,M,R,NewM,Flag):-
	getPile(IPile,M,Pile),
	supprimer(Pile,1,R,NewPile),
	(NewPile==[] ->
		supprimer(M,IPile,_,NewM),
		Flag is 1;
		remplacer(M,IPile,NewPile,NewM),
		Flag is 0).

/*Renvoie l'indice des piles autour du trader*/
aroundTrader(P,M,I1,I2):-
	length(M,L),
	(P==L -> 
		I2 is 1;
		I2 is P + 1),
	(P==1 ->
		I1 is L;
		I1 is P - 1).		
 
/*############################################################*/




/*################Fonctions d'initialisation##################*/

/*Génère le plateau de départ*/
createPlateau([M,B,PTrader,[],[]]):-
	random_between(1,9,PTrader),
	B = [[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]],
	Rsrc = [[ble,6],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]],
	createListMarchandises(4,9,Rsrc,M,_).
	

/*Génère la liste des piles de marchandises*/
createListMarchandises(TaillePile,1,Rsrc,[Pile],RsrcF):-
	addRessourcesToPile(TaillePile,Rsrc,Pile,RsrcF).
createListMarchandises(TaillePile,NbPiles,Rsrc,[Pile|T],RsrcF):-
	NbPiles > 1,
	NewNbPiles is NbPiles - 1,
	addRessourcesToPile(TaillePile,Rsrc,Pile,NewRsrc),
	createListMarchandises(TaillePile,NewNbPiles,NewRsrc,T,RsrcF).

/*Créé une pile de N ressources à partir d'une liste de ressources possibles et renvoie les ressources restantes*/
addRessourcesToPile(Nb,Rsrc,[R|T],RsrcF):-
	repeat,
	random_between(1,6,Rand),
	nth1(Rand,Rsrc,[R|_]),
	decrementRessource(Rsrc,R,NewRsrc),
	!,
	(Nb > 1 ->
		NewNb is Nb - 1,
		addRessourcesToPile(NewNb,NewRsrc,T,RsrcF);
		T = [],
		RsrcF = NewRsrc).

/*############################################################*/




/*################Fonctions d'affichage#######################*/

/*Affichage du plateau de jeu*/
affiche_plateau([M,B,PTrader,RJ1,RJ2]):-
	length(M,NbPile),
	nl, 
	writeln('####################################################################################'),
	writeln('#                                                                                  #'),
	writeln('#                                                                                  #'),
	nl,
	writeln('Bourse :'),
	nl,
	afficherBourse(B),
	nl,
	writeln('Reserve Joueur 1 :'),
	nl,
	write(' [ '), afficherReserve(RJ1), write(' ] '),
	nl,
	nl, 
	writeln('Reserve Joueur 2 :'),
	nl,
	write(' [ '), afficherReserve(RJ2), write(' ] '),
	nl,
	nl,
	nl,
	writeln('Piles de Marchandises :'),
	nl,
	afficherPilesEtTrader(1,NbPile,PTrader,M),
	nl,
	nl,
	writeln('#                                                                                  #'),
	writeln('#                                                                                  #'),
	writeln('####################################################################################'),
	nl.

/*Affichage de la Bourse*/	
afficherBourse([H|T]):-
	afficherRessourceEtValeur(H),
	(T == [] -> nl;
	afficherBourse(T)).
afficherRessourceEtValeur([R,V]):-
	write(R),write(' : '),writeln(V).	

/*Affichage de la réserve d'un joueur*/
afficherReserve(Reserve):-
	(Reserve == [] ->
		write(' ');
		[H|T] = Reserve,
		write(H),write(' '),
		afficherReserve(T)
		).

/*Affichage du haut des piles de marchandises et du trader*/
afficherPilesEtTrader(Index,IndexMax,ITrader,Marchandises):-
	NewIndex is Index + 1,
	getMarchandise(Index,1,Marchandises,R),
	write(Index),write('['),write(R),
	(Index==ITrader ->
		write('|*.*|');
		write('')),
	write(']'),write(' '),
	(Index==IndexMax -> 
		write(' ');	
		afficherPilesEtTrader(NewIndex,IndexMax,ITrader,Marchandises)).
		
/*############################################################*/





/*###########Fonctions de service pour les listes#############*/

/*Remplacer un élément dans une liste*/
remplacer([_|T],1,Remplacement,[Remplacement|T]).
remplacer([H|T],X,Remplacement,[H|T2]):-
	Y is X -1,
	remplacer(T,Y,Remplacement,T2).
	
/*Supprimmer un élément d'une liste*/
supprimer([Deleted|T],1,Deleted,T).
supprimer([H|T],Index,Deleted,[H|T2]):-
	NewIndex is Index-1,
	supprimer(T,NewIndex,Deleted,T2).	
	
/*############################################################*/