%-----------------------------------------------------------------------------
% Auteur: Charles-Antoine Brunet
% Version: 3.0
% Date: 2015-07-29
%-----------------------------------------------------------------------------
%Equipe 12 : Afaf Taïk - Robin Cavalieri - Zakaria Belgoum
%Automne 2017 - UdeS
%-----------------------------------------------------------------------------

% Un JI doit être un module afin d'éviter les conflits de noms entre les JI.
:- module(p12,[p12_nom/1,p12_auteurs/1,p12_reset/0,p12_plan/1,p12_action/2]).

%-----------------------------------------------------------------------------
% Prédicats de jeu.
%-----------------------------------------------------------------------------

% Nom du JI: p12_nom(-Nom)
p12_nom('Madoka').

% Auteurs du JI: p12_auteurs(-Auteurs)
p12_auteurs('Afaf,Robin et Zakaria').

% Remise à zero du JI: p12_reset
p12_reset :-
    planInitial(P),
    setPlan(P).

% Plan courant du JI: p12_plan(-PlanCourant)
p12_plan(Plan) :-
    getPlan(Plan).

% Prochaine action du JI: p12_action(+Etat, -Action)
p12_action(Etat, Action) :-
    trouveAction(Etat, Action).

%-----------------------------------------------------------------------------
% Prédicats internes de plans.
%-----------------------------------------------------------------------------
% La consultation d'un plan et la modification d'un plan sont protégées par
% mutex afin d'éviter les conflits possibles d'appels en parallèle.
%
% Le prédicat planRestant est déclaré dynamique, car sa valeur change au cours
% de l'exécution.
%-----------------------------------------------------------------------------

:- dynamic([planRestant/1]).

planInitial([]).

planRestant([]).

getPlan(Plan) :-
    with_mutex(p12,planRestant(Plan)).

setPlan(Plan) :-
    with_mutex(p12,changePlan(Plan)).

changePlan(Plan) :-
    retractall(planRestant(_)),
    assert(planRestant(Plan)).

%-----------------------------------------------------------------------------
% Prédicats internes d'action
%-----------------------------------------------------------------------------
% Calcul de la prochaine action du JI. Ce JI ne fera jamais rien de bon...
%-----------------------------------------------------------------------------
%trouveAction(EtatJeu, ProchaineAction) :-
%    getPlan([ProchaineAction]), !, planInitial(P), setPlan(P).
%trouveAction(EtatJeu, ProchaineAction) :-
%    getPlan([ProchaineAction|PlanRestant]), setPlan(PlanRestant).
trouveAction(State, ProchaineAction) :-
	    \+goalState(State),
	  search(State,NewPlan),
	  changePlan(NewPlan),
	  getPlan([ProchaineAction|PlanRestant]),
	  setPlan(PlanRestant).
%trouveAction(_,none()).

% ------------------------------------------------------------------------
% Actions
move(_).
attack(_).
take(_).
drop(_).
none().
% ---------------------------------------------------------------
%Obtention des informations de mon joueur p12
%Directement du set des players
getMyPlayer(Players,MyPlayer):-
    p12_nom(Nom),
    member_set([N,Nom,X,Y,B],Players),
    append([],[N,Nom,X,Y,B],MyPlayer).
%A partir de l'etat tout entier
getMyPlayerFromState(State,MyPlayer):-
    p12_nom(Nom),
    append([],State,[_,_,_,_,Players,_]),
    member_set([N,Nom,X,Y,B],Players),
    append([],[N,Nom,X,Y,B],MyPlayer).
%---------------------------------------------------------------
%Obtenir les infos du player dans une case afin de l attaquer
%a partir de ses coordonnées
%1-tiré directement de la liste des joueurs
getAttackedPlayer(Players,AttackedPlayer,X,Y):-
    member_set([N,Nom,X,Y,B],Players),
    append([],[N,Nom,X,Y,B],AttackedPlayer).
%2-De l'etat du jeu
getAttackedPlayerFromState(State,AttackedPlayer,X,Y):-
    append([],State,[_,_,_,_,Players,_]),
    getAttackedPlayer(Players,AttackedPlayer,X,Y).
%-------------------------------------------------------------
%-------------------------------------------------------------
%Coordonnees de ou se passera l action
%usage: getPosition(+Direction,+X,+Y,-U,-V)
getPosition(1,X,Y,U,V):- U is X, V is Y+1.
getPosition(2,X,Y,U,V):- U is X+1, V is Y.
getPosition(3,X,Y,U,V):- U is X, V is Y-1.
getPosition(4,X,Y,U,V):- U is X-1, V is Y.
getPosition(5,X,Y,U,V):- U is X+1, V is Y+1.
getPosition(6,X,Y,U,V):- U is X+1, V is Y-1.
getPosition(7,X,Y,U,V):- U is X-1, V is Y-1.
getPosition(8,X,Y,U,V):- U is X-1, V is Y+1.

%verifier si la position est libre
%a utiliser avec move et drop
%positionValide(+[_,_,_,_,Players,Blocs],+X,+Y)
positionValide([_,_,C,R,Players,Blocs],X,Y):-
    X<C,0=<X,
    Y<R,0=<Y,
    \+member_set([_,_,X,Y,_],Players),
    \+member_set([_,X,Y],Blocs).
%---------------------------------------------------------------
%Verification de la possibilité d'effectuer les actions
%---------------------------------------------------------------
canMove(State,D):-
    getMyPlayerFromState(State,MyPlayer),
    append([],MyPlayer,[_,_,X,Y,_]),
    getPosition(D,X,Y,U,V),
    positionValide(State,U,V).
canDrop(State,D):-
    getMyPlayerFromState(State,MyPlayer),
    append([],MyPlayer,[_,_,X,Y,B]),
    B>0,
    getPosition(D,X,Y,U,V),
    positionValide(State,U,V).
canAttack(State,D):-
    getMyPlayerFromState(State,MyPlayer),
    append([],MyPlayer,[_,_,X,Y,_]),
    getPosition(D,X,Y,U,V),
    getAttackedPlayerFromState(State,_,U,V).
canTake(State,D):-
    getMyPlayerFromState(State,MyPlayer),
    append([],MyPlayer,[_,_,X,Y,_]),
    getPosition(D,X,Y,U,V),
    append([],State,[_,_,_,_,_,Blocs]),
    member_set([_,U,V],Blocs).


%----------------------------------------------------------------
action(State, move(D)) :- canMove(State,D).
action(State, take(D)) :- canTake(State,D).
action(State, drop(D)) :- canDrop(State,D).
action(State, attack(D)) :- canAttack(State,D).
action(_, none()).

actionsPossibles(State, Res) :-
   findall(Action, action(State, Action), Res).
%----------------------------------------------------------------
%Effet des actions sur l'etat du jeu
%----------------------------------------------------------------
%Changement d'etat après avoir effectué un move(D)
moveEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]):-
    getMyPlayer(Players,Player),
    append([],Player,[A,Nom,X,Y,B]),
    getPosition(D,X,Y,U,V),
    positionValide([_,_,C,R,Players,Blocs],U,V),
    add_in_set([A,Nom,U,V,B],Players,Rplayersaux),
    delete_in_set([A,Nom,X,Y,B],Rplayersaux,RPlayers).

%Changement d'etat après avoir effectué un take(D)
takeEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,RBlocs]):-
    getMyPlayer(Players,Player),
    append([],Player,[A,Nom,X,Y,B]),
    getPosition(D,X,Y,U,V),
    B==0,
    member_set([S,U,V],Blocs),
    add_in_set([A,Nom,X,Y,S],Players,RPlayersaux),
    delete_in_set([A,Nom,X,Y,B],RPlayersaux,RPlayers),
    delete_in_set([S,U,V],Blocs,RBlocs).
takeEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,RBlocs]):-
    getMyPlayer(Players,Player),
    append([],Player,[A,Nom,X,Y,B]),
    B>0,
    getPosition(D,X,Y,U,V),
    member_set([S,U,V],Blocs),
    add_in_set([A,Nom,X,Y,S],Players,RPlayersaux),
    delete_in_set([A,Nom,X,Y,B],RPlayersaux,RPlayers),
    delete_in_set([S,U,V],Blocs,RBlocsaux),
    add_in_set([B,U,V],RBlocsaux,RBlocs).


%Changement d'etat après avoir effectué un drop(D)
dropEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,RBlocs]):-
    getMyPlayer(Players,Player),
    append([],Player,[A,Nom,X,Y,B]),
    B>0,%si le joueur n a pas de bloc on s arrete
    getPosition(D,X,Y,U,V),
    positionValide([_,_,C,R,Players,Blocs],U,V),
    add_in_set([B,U,V],Blocs,RBlocs),
    add_in_set([A,Nom,X,Y,0],Players,Rplayersaux),
    delete_in_set([A,Nom,X,Y,B],Rplayersaux,RPlayers).
dropEffect([N,M,C,R,Players,Blocs],_,[N,M,C,R,Players,Blocs]):-none().
%Effets d'une attaque selon differents cas

%Attaque lorsque le JI n'a pas de bloc ( proba de 0.25 de réussite )
attackNoBlockEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]):-
    getMyPlayer(Players,Player),
    append([],Player,[A,Nom,X,Y,B]),
    getPosition(D,X,Y,U,V),
    getAttackedPlayer(Players,AttackedPlayer,U,V),
    append([],AttackedPlayer,[A2,Nom2,U,V,B2]),
    B2>0, B=:=0,
   % maybe(0.25),  %probabilité de 25% de réussite du coup-Utilisation cause la génération de plans non optimaux
    add_in_set([A,Nom,X,Y,B2],Players,RPlayers1),
    add_in_set([A2,Nom2,U,V,B],RPlayers1,RPlayersaux),
    delete_in_set([A,Nom,X,Y,B],RPlayersaux,RPlayersaux1),
    delete_in_set([A2,Nom2,U,V,B2],RPlayersaux1,RPlayers).

%Attaque lorsque le JI possède un bloc
attackWthBlockEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]):-
    getMyPlayer(Players,Player),
    append([],Player,[A,Nom,X,Y,B]),
    getPosition(D,X,Y,U,V),
    getAttackedPlayer(Players,AttackedPlayer,U,V),
    append([],AttackedPlayer,[A2,Nom2,U,V,B2]),
    B2>0, B>0,
    %maybe(B/(B+B2)),          %probabilité de réussir un coup lorsque le joueur possède un bloc-Utilisation expérimentale uniquement
    add_in_set([A,Nom,X,Y,B2],Players,RPlayers1),
    add_in_set([A2,Nom2,U,V,B],RPlayers1,RPlayersaux),
    delete_in_set([A,Nom,X,Y,B],RPlayersaux,RPlayersaux1),
    delete_in_set([A2,Nom2,U,V,B2],RPlayersaux1,RPlayers).

%Changement d'etat après avoir effectué une attaque(D)
attackEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]):-
  attackNoBlockEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]) .
attackEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]):-
    attackWthBlockEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]).
attackEffect([N,M,C,R,Players,Blocs],_,[N,M,C,R,Players,Blocs]):-
    none().%dans le cas d'echec on retourne quand même true- marche uniquement dans les cas non déterministes

%Appliquer les actions
%Nous en avons besoin pour l'algorithme de recherche
actionEffect(State,move(D),NewState):-
    append([],State,[N,M,C,R,Players,Blocs]),
    moveEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]),
    append([],[N,M,C,R,RPlayers,Blocs],NewState).
actionEffect(State,take(D),NewState):-
    append([],State,[N,M,C,R,Players,Blocs]),
    takeEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,RBlocs]),
    append([],[N,M,C,R,RPlayers,RBlocs],NewState).
actionEffect(State,drop(D),NewState):-
    append([],State,[N,M,C,R,Players,Blocs]),
    dropEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,RBlocs]),
    append([],[N,M,C,R,RPlayers,RBlocs],NewState).
actionEffect(State,attack(D),NewState):-
    append([],State,[N,M,C,R,Players,Blocs]),
    attackEffect([N,M,C,R,Players,Blocs],D,[N,M,C,R,RPlayers,Blocs]),
    append([],[N,M,C,R,RPlayers,Blocs],NewState).
actionEffect(State,none(),State).
% =========================================================================
%Planification
% ==========================================================================
% Le but du jeu c'est de prendre le bloc avec la plus grande valeur et
% le garder jusqu a la fin. On doit donc le localiser tout d'abord
getGoalBlocValue(State,GBloc):-
    append([],State,[_,B|_]),
    GBloc is B.
%Prédicat pour chercher le plus grand bloc libre

maxFreeBloc([[Id,_,_]|T],FBloc):-
    maxFreeBloc(T,FBloc),Id<FBloc.
maxFreeBloc([[Id,_,_]|T],Id):-
    maxFreeBloc(T,FBloc),Id>=FBloc.
maxFreeBloc([[Id,_,_]],Id).
maxFreeBloc([],0).

getMaxFreeBloc(State, [M,X,Y]):-
    append([],State,[_,_,_,_,_,Blocs]),
    maxFreeBloc(Blocs,M),
    member_set([M,X,Y],Blocs).

% L'heuristique choisie est la distance euclidienne entre le JI et le but
distanceEuclidienne(GX,GY,PX,PY,H):-
    H is sqrt((GX-PX)^2 + (GY-PY)^2).
%le cas où le meilleur bloc est libre
heuristic(State,H):-
   getGoalBlocValue(State,GM),
   append([],State,[_,_,_,_,_,Blocs]),
   member_set([GM,X,Y],Blocs),
   getMyPlayerFromState(State,MyPlayer),
   append([],MyPlayer,[_,_,PX,PY,_]),
   distanceEuclidienne(X,Y,PX,PY,H).
%le cas où le meilleur bloc est pris par un joueur
heuristic(State,H):-
   getGoalBlocValue(State,GM),
   append([],State,[_,_,_,_,Players,_]),
   getMyPlayerFromState(State,MyPlayer),
   append([],MyPlayer,[_,_,PX,PY,_]),
   member_set([_,_,U,V,GM],Players),
   distanceEuclidienne(U,V,PX,PY,H).
%au cas où le JI possede le meilleur bloc
heuristic(State,H):-goalState(State),
    H is 0.
% ------------------------------------------------------------------------
%Les couts choisis:
cost(move(_),C):- C is 1.
cost(attack(_),C):- C is 1.
cost(drop(_),C):- C is 1.
cost(take(_),C):- C is 0.5.
cost(none(),C):- C is 5.
% ------------------------------------------------------------------------
%F=H+C

f_function(Action,State,F,C):-
    cost(Action,C),
    actionEffect(State,Action,Tempstate),
    heuristic(Tempstate,H),
    F is H+C.

%Trouver la meilleure action à partir de la liste des actions possibles
compareActions(State,[Action],Action):-f_function(Action,State,_,_).
compareActions(State,[Action2|RestofFrontier],BestAction):-
       compareActions(State,RestofFrontier,BestAction),f_function(Action2,State,F,_),
       f_function(BestAction,State,BestF,_),F>BestF.

compareActions(State,[Action2|RestofFrontier],Action2):-
    compareActions(State,RestofFrontier,BestAction),
    f_function(Action2,State,F,_),
    f_function(BestAction,State,BestF,_),
    F=<BestF.
%Developpement du plan en ajoutant la meilleure action au plan

developperPlan(State,CurrentCost,NewCost,Plan,NewPlan,PossibleActions,NewState,ClosedStates,RClosedStates):-

    compareActions(State,PossibleActions,BestAction),
    cost(BestAction,C),
    NewCost is CurrentCost+C,
    actionEffect(State,BestAction,NewState),
    append(Plan,[BestAction],NewPlan),
    add_in_set(State,ClosedStates,RClosedStates).

developperPlan(_,_,_,P,P,[],_,_,_).
% Répétition de la recherche de la meilleure action jusqu'à ce qu'on
% atteint le but
explore(State,CurrentCost,ClosedStates,InitPlan,FinalPlan):-
    \+goalState(State),\+member_set(State,ClosedStates),
     actionsPossibles(State, PossibleActions),
     developperPlan(State,CurrentCost,NewCost,InitPlan,NewPlan,PossibleActions,NewState,ClosedStates,RClosedStates),
     explore(NewState,NewCost,RClosedStates,NewPlan,FinalPlan).


explore(State,_,_,P,P):-goalState(State).
explore(State,_,CS,_,_):-member_set(State,CS).
%Initialisation de la recherche avec un cout nul et un plan vide
search(State,Plan):-explore(State,0,[],[],Plan).
%Test de l'état but
goalState(State):-
    p12_nom(Nom),
    append([],State,[_,M,_,_,Players,_]),
    member_set([_,Nom,_,_,M],Players).






%------------------------------------------------------------------------------
% Structure de données de set (collection),
%   Le set est sans dupliqués et les valeurs ne sont pas ordonnées
%
% Auteur: Charles-Antoine Brunet
%------------------------------------------------------------------------------
% Version 1.0: Version initiale
% Date: 2005/04/11
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% +: paramètre en entrèe
% -: paramètre en sortie
% ?: paramètre en entrèe ou sortie
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Tester si un set est vide ou créer un set
% empty_set(?Set)
%------------------------------------------------------------------------------
empty_set([]).

%------------------------------------------------------------------------------
% Vérifier si un élément est membre d'un set
% Utilise la fonction member de la librairie standard de liste
% member_set(+Item, +Set)
%------------------------------------------------------------------------------
member_set(E, S) :- member(E, S).

%------------------------------------------------------------------------------
% Enlever un élément du set, s'il est présent
% delete_in_set(+Item, +Set, -NewSet)
% Item=item à enlever, Set = ancien set, NewSet = nouveau set
%------------------------------------------------------------------------------
delete_in_set(_, [], []) :- !.
delete_in_set(E, [E|T], T) :- !.
delete_in_set(E, [H|T], [H|Tnew]) :- delete_in_set(E,T,Tnew).

%------------------------------------------------------------------------------
% Ajouter un élément au set, s'il n'est pas présent
% add_in_set(+Item, +Set, -NewSet)
% Item=item à ajouter, Set = ancien set, NewSet = nouveau set
%------------------------------------------------------------------------------
add_in_set(E, S, S) :- member(E,S), !.
add_in_set(E, S, [E|S]).

%------------------------------------------------------------------------------
% Fusionner 2 sets
% set_union(+Set1, +Set2, -Set3)
% Set3 contient les items de Set1 et de Set2
%------------------------------------------------------------------------------
set_union([], S, S).
set_union([H|T], S, Snew) :- set_union(T, S, Tmp), add_in_set(H, Tmp, Snew).

%------------------------------------------------------------------------------
% Vérifier si un set est un sous-ensemble d'un autre
% sub_set(+Set1, +Set2) est vrai si Set1 est un sous-ensemble de Set2.
%------------------------------------------------------------------------------
sub_set([],_).
sub_set([H|T], S) :- member_set(H,S), sub_set(T,S).

%------------------------------------------------------------------------------
% Trouver les éléments communs à 2 sets
% set_intersection(+Set1, +Set2, -Intersection)
% Intersection contient les items communs à Set1 et Set2.
%------------------------------------------------------------------------------
set_intersection([], _, []).
set_intersection([H|T],S,[H|Snew]) :-
    member_set(H,S), set_intersection(T,S,Snew), !.
set_intersection([_|T],S,Snew) :- set_intersection(T, S, Snew).

%------------------------------------------------------------------------------
% Calculer la différence entre 2 sets.
% set_difference(+Set1,+Set2,-Difference)
% Différence contient les éléments qui sont dans Set1 mais pas dans Set2
%------------------------------------------------------------------------------
set_difference([], _, []).
set_difference([H|T], S, Tnew) :-
    member_set(H,S), set_difference(T, S, Tnew), !.
set_difference([H|T], S, [H|Tnew]) :- set_difference(T,S,Tnew).

%------------------------------------------------------------------------------
% Vérifier si 2 sets sont équivalents
% equal_set(+S1, +S2)
% Vrai si tous membres de Set1 sont dans Set2 et ceux de Set2 dans Set1
%------------------------------------------------------------------------------
equal_set(S1, S2) :- sub_set(S1, S2), sub_set(S2, S1).

%------------------------------------------------------------------------------
% Imprimer un set
%------------------------------------------------------------------------------
print_set([]).
print_set([H|Q]) :- write(H), nl, print_set(Q).





















