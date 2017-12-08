% Auteur: Charles-Antoine Brunet
% Date: 2010-08-31
% Données pour la carte de la Roumanie, selon le livre de
% de Russel et Norvig, 3e édition, pages 68 et 93

% Représentation de la fonction successeur S(n)
s(arad,          [sibiu, timisoara, zerind]).
s(bucharest,     [fagaras,giurgiu,pitesti,urziceni]).
s(craiova,       [dobreta,pitesti,rimnicuVilcea]).
s(dobreta,       [craiova,mehadia]).
s(eforie,        [hirsova]).
s(fagaras,       [bucharest,sibiu]).
s(giurgiu,       [bucharest]).
s(hirsova,       [eforie,urziceni]).
s(iasi,          [neamt,vaslui]).
s(lugoj,         [mehadia,timisoara]).
s(mehadia,       [dobreta,lugoj]).
s(neamt,         [iasi]).
s(oradea,        [sibiu,zerind]).
s(pitesti,       [bucharest,craiova,rimnicuVilcea]).
s(rimnicuVilcea, [craiova,pitesti,sibiu]).
s(sibiu,         [arad,fagaras,oradea,rimnicuVilcea]).
s(timisoara,     [arad,lugoj]).
s(urziceni,      [bucharest,hirsova,vaslui]).
s(vaslui,        [iasi,urziceni]).
s(zerind,        [arad,oradea]).

% Distance à vol d'oiseau de Bucharest: heuristique h(n)
h(arad, 366).      h(bucharest, 0). h(craiova, 160).       h(dobreta, 242).
h(eforie, 161).    h(fagaras, 178). h(giurgiu, 77).        h(hirsova, 151).
h(iasi, 226).      h(lugoj, 244).   h(mehadia, 241).       h(neamt, 234).
h(oradea, 380).    h(pitesti, 98).  h(rimnicuVilcea, 193). h(sibiu, 253).
h(timisoara, 329). h(urziceni, 80). h(vaslui, 199).        h(zerind, 374).


% Distance entre 2 villes
d(arad, sibiu, 140).            d(arad, timisoara, 118).
d(arad, zerind, 75).
d(bucharest, fagaras, 211).     d(bucharest, giurgiu, 90).
d(bucharest, pitesti, 101).     d(bucharest, urziceni, 85).
d(craiova,dobreta, 120).        d(craiova,pitesti, 138).
d(craiova,rimnicuVilcea, 146).
d(dobreta, craiova, 120).       d(dobreta, mehadia, 75).
d(eforie, hirsova, 86).
d(fagaras, bucharest, 211).     d(fagaras, sibiu, 99).
d(giurgiu, bucharest, 90).
d(hirsova, eforie, 86).         d(hirsova, urziceni, 98).
d(iasi, neamt, 87).             d(iasi, vaslui, 92).
d(lugoj, mehadia, 70).          d(lugoj, timisoara, 111).
d(mehadia, dobreta, 75).        d(mehadia, lugoj, 70).
d(neamt, iasi, 87).
d(oradea, sibiu, 151).          d(oradea, zerind, 71).
d(pitesti,bucharest,101).       d(pitesti,craiova,138).
d(pitesti,rimnicuVilcea,97).
d(rimnicuVilcea, craiova, 146). d(rimnicuVilcea, pitesti, 97).
d(rimnicuVilcea, sibiu, 80).
d(sibiu, arad, 140).            d(sibiu, fagaras, 99).
d(sibiu, oradea, 151).          d(sibiu, rimnicuVilcea, 80).
d(timisoara, arad, 118).        d(timisoara, lugoj, 111).
d(urziceni, bucharest, 85).     d(urziceni, hirsova, 98).
d(urziceni, vaslui, 142).
d(vaslui, iasi, 92).            d(vaslui, urziceni, 142).
d(zerind, arad, 75).            d(zerind, oradea, 71).

%============================================================
%Auteur: Afaf Taïk

%============================================================
%Using greedy :
bestGreedyNode([B|R],A):-
    bestGreedyNode(R,A),
    h(B,X),
    h(A,Y),
    X>Y.
bestGreedyNode([B|R],B):-bestGreedyNode(R,A),
    h(B,X),
    h(A,Y),
    X<Y.
bestGreedyNode([X],X).


greedyPlan(N,Best,Plan,NewPlan):-
    s(N,List),
    bestGreedyNode(List,Best),
    append(Plan,[Best],NewPlan).

greedyPlanApply(N,_,Goal,Plan,Plan):- N==Goal.
greedyPlanApply(N,B,Goal,Plan,NewPlan):-
   \+(N==Goal),
   greedyPlan(N,X,Plan,NewPlanaux),
   greedyPlanApply(X,B,Goal,NewPlanaux,NewPlan).

%==============================================================
%------------------------------------------------------
%Uniform-cost
uniform(Start,Finish,ShortestPath,Len):-
    uni([0-[Start]],Finish,Rshort,Len),
    reverse(Rshort,ShortestPath).
uni([Len-[Fin|Rpath]|_],Fin,[Fin|Rpath],Len):-!.
uni(Visited,Fin,RshortestPath,Len):-
    bestCandidate(Visited,BestCandidate),
    uni([BestCandidate|Visited],Fin,RshortestPath,Len).
bestCandidate(Paths,BestCandidate):-
    findall(NP,
            (    member(Len-[P1|Path],Paths),
                 d(P1,P2,Dist),
                 \+isVisited(Paths,P2),
                 NLen is Len+Dist,
                 NP=NLen-[P2,P1|Path]),Candidates),
            minimum(Candidates,BestCandidate).
minimum(Candidates,BestCandidate):-
    keysort(Candidates,[BestCandidate|_]).
isVisited(Paths,P):-
    memberchk(_-[P|_],Paths).

%------------------------------------------------------
%breadth first
%
%
frontier_breadth(Frontier,Successors,NewFrontier):-
    Frontier=[X|R],
    dequeue(X, [X|R],R ),
    add_list_to_queue(Successors,R,NewFrontier).

%breadth_search(Frontier,_,_,Solution):-
 %   empty_set(Frontier),
  %  Solution is [].

breadth_search(Frontier,_,Goal,Solution):-
    Frontier=[X|_],
    X==Goal,
    write(Solution).
breadth_search(Frontier,Explored,Goal,Solution):-
    Frontier=[X|_],
    \+member_set(X,Explored),
    \+(X==Goal),
    s(X,Successors),
    add_in_set(X,Explored,NewExplored),
    enqueue(X,Solution,NewSolution),
    frontier_breadth(Frontier,Successors,NewFrontier),
    breadth_search(NewFrontier,NewExplored,Goal,NewSolution).

breadth_search(Frontier,Explored,Goal,Solution):-
    Frontier=[X|_],
    member_set(X,Explored),
    \+(X==Goal),
    dequeue(X,Frontier,NewFrontier),
    breadth_search(NewFrontier,Explored,Goal,Solution).

%---------------------------------------------------------
%Depth first

frontier_depth(Frontier,Successors,NewFrontier):-
    Frontier=[X|R],
    stack(X,R,Frontier),
    add_list_to_stack(Successors,R,NewFrontier).

%depth_search(Frontier,_,_,_):-
 %   empty_set(Frontier).

depth_search(Frontier,_,Goal,Solution):-
    Frontier=[X|_],
    X==Goal,
    reverse(Solution,Path),
    write(Path).
depth_search(Frontier,Explored,Goal,Solution):-
    Frontier=[X|_],
    \+member_set(X,Explored),
    \+(X==Goal),
    s(X,Successors),
    add_in_set(X,Explored,NewExplored),
    stack(X,Solution,NewSolution),
    frontier_depth(Frontier,Successors,NewFrontier),
    depth_search(NewFrontier,NewExplored,Goal,NewSolution).

depth_search(Frontier,Explored,Goal,Solution):-
    Frontier=[X|_],
    member_set(X,Explored),
    \+(X==Goal),
    stack(X,NewFrontier,Frontier),
    depth_search(NewFrontier,Explored,Goal,Solution).



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
%------------------------------------------------------------------------------
% Structure de données de file (queue) et file (queue) avec priorité
% Auteur: Charles-Antoine Brunet
%------------------------------------------------------------------------------
% Version 1.0: Version initiale
% Date: 2005/04/11
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% +: paramètre en entrée
% -: paramètre en sortie
% ?: paramètre en entrée ou sortie
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Tester si une file est vide ou créer une file
% empty_queue(?Stack)
%------------------------------------------------------------------------------
empty_queue([]).

%------------------------------------------------------------------------------
% Ajouter un item dans la file
% enqueue(+Item, +Queue, -NewQueue)
% Item=item à ajouter, Y=ancienne file, Z=nouvelle file
%------------------------------------------------------------------------------
enqueue(E, [], [E]).
enqueue(E, [H|T], [H|Tnew]) :- enqueue(E, T, Tnew).

%------------------------------------------------------------------------------
% Elever un item de la file
% dequeue(-Item, +Queue, -NewQueue)
% Item= item enlevé, Queue=ancienne file, NewQueue=la nouvelle file
%------------------------------------------------------------------------------
dequeue(E, [E|T], T).

%------------------------------------------------------------------------------
% Consulte le premier item de la file
% peek_queue(-Item, +Queue), Item=premier item, Queue= file a consulter
%------------------------------------------------------------------------------
peek_queue(E, [E|_]).

%------------------------------------------------------------------------------
% Vérifier si un élement est membre d'une file
% Utilise la fonction member de la librairie standard de liste
%------------------------------------------------------------------------------
member_queue(E, T) :- member(E, T).

%------------------------------------------------------------------------------
% Ajoute une liste d'élements à une file
% add_list_to_queue(+List, +Queue, -NewQueue)
% List=liste à ajouter, Queue=ancienne file, NewQueue=nouvelle file
% Utilise la fonction append de la librairie standard de liste
%------------------------------------------------------------------------------
add_list_to_queue(List, T, NewT) :- append(T, List, NewT).
%------------------------------------------------------------------------------
% QUEUE AVEC PRIORITÉ
%------------------------------------------------------------------------------
% Les opérateurs empty_queue, member_queue, dequeue et peek sont les mêmes
%      que plus haut. Les 2 opérateurs qui changent sont les suivants
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Ajouter un item dans la file avec priorité
% insert_pq(+Item, +Queue, -NewQueue)
% Item=item à ajouter, Y=ancienne file, Z=nouvelle file
%------------------------------------------------------------------------------
insert_pq(E, [], [E]) :- !.
insert_pq(E, [H|T], [E, H|T]) :- precedes(E,H), !.
insert_pq(E, [H|T], [H|Tnew]) :- insert_pq(E, T, Tnew).

%------------------------------------------------------------------------------
% Ajouter une liste d'éléments (non ordonnés) à une file avec priorité
% insert_list_pq(+List, +Queue, -NewQueue)
% List=liste à ajouter, Queue=ancienne file, NewQueue=nouvelle file
%------------------------------------------------------------------------------
insert_list_pq([], L, L).
insert_list_pq([E|T], L, NewL) :-
    insert_pq(E, L, Tmp), insert_list_pq(T, Tmp, NewL).

%------------------------------------------------------------------------------
% IMPORTANT! Selon le type de données, peut-être nécessaire de changer la
%     définition du prédicat suivant.
%------------------------------------------------------------------------------
precedes(X,Y) :- X<Y .


%------------------------------------------------------------------------------
% Structure de données de pile
% Auteur: Charles-Antoine Brunet
%------------------------------------------------------------------------------
% Version 1.0: Version initiale
% Date: 2005/04/11
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% +: paramètre en entrée
% -: paramètre en sortie
% ?: paramètre en entrée ou sortie
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
% Test si une pile est vide ou créer une pile
% empty_stack(?Stack)
%------------------------------------------------------------------------------
empty_stack([]).

%------------------------------------------------------------------------------
% Pousser (push) et enlever (pop) un item sur une pile
% push : stack(+X, +Y, -Z), X=item à ajouter, Y=ancienne pile, Z=nouvelle pile
% pop: stack(-X, -Y, +Z), X=item dessus, Y=nouvelle pile, Z=ancienne pile
%------------------------------------------------------------------------------
stack(Top, Stack, [Top|Stack]).

%------------------------------------------------------------------------------
% Consulter le premier item de la pile
% Top contiendra la valeur du premier item de Stack
% peek_stack(-Top, +Stack)
%------------------------------------------------------------------------------
peek_stack(Top,[Top|_]).

%------------------------------------------------------------------------------
% Vérifier si un item est membre d'une pile
% Utilise la fonction member de la librairie standard de liste
% member_stack(+Item, +Stack)
%------------------------------------------------------------------------------
member_stack(Item, Stack) :- member(Item, Stack).

%------------------------------------------------------------------------------
% Ajouter une liste d'items à une pile
% add_list_to_stack(+List, +Stack, -NewStack)
% List=liste à ajouter, Stack=ancienne pile, NewStack=nouvelle pile
% Utilise la fonction append de la librairie standard de liste
%------------------------------------------------------------------------------
add_list_to_stack(List, Stack, NewStack) :- append(List, Stack, NewStack).











