
%Liste des composants à répartir : 

%Affichage d'une liste : 
affiche_liste([]).
affiche_liste([T|Q]) :- write(T), affiche_liste(Q).

%Effacement d'un seul  élément

retire_elem(X, [X|Q], Q).
retire_elem(X, [T|Q], [T,N]) :- retire_elem(X, Q, N).


concat([], L, L).
concat([T|Q], L, [T|R]):-concat(Q, L, R).	
concat(E, K, [E|K]).

%select(term, list, list) est build in, ou delete(list, term, list).

%obtenir un nombre rand : random(base, max, number).

%remplir les listes

%élement numero x
el([T|Q], 1, T).
el([T|Q], I, E):- N is I-1, el(Q, N, E), !.
%longueur d'une liste
long([], 0).
long([T|Q], L):- long(Q, X), L is X+1.

%remplissage de liste
%shuffle(?listedepart, ?listefin)
shuffle([], L).
shuffle(L, R):-
	long(L, S),
	T is S+1,
	random(1, T, K),
	el(L, K, E),
	concat(R, E, N),
	select(E, L, Nl),
	remplir(Nl, N).
	

%repartition des ressources
%get_r(?ListeRes, ?nbRes, +Result)
get_r([T|Q], 1, T).
get_r([T|Q], N, [T|R]):- 
	K is N-1,
	get_r(Q, K, R).
	
%création plateau de jeu : concat

%afficher avec affiche_liste

	
	
	







