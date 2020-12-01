%% -*- prolog -*-

location(tambi).
location(faerun).
location(norrath).
location(straylight).
location(tristram).
location(arbre).
location(alphacentauri).
location(snowdin).

distance(alphacentauri,snowdin,66).
distance(alphacentauri,tambi,28).
distance(alphacentauri,faerun,60).
distance(alphacentauri,norrath,34).
distance(alphacentauri,straylight,34).
distance(alphacentauri,tristram,3).
distance(alphacentauri,arbre,108).
distance(snowdin,tambi,22).
distance(snowdin,faerun,12).
distance(snowdin,norrath,91).
distance(snowdin,straylight,121).
distance(snowdin,tristram,111).
distance(snowdin,arbre,71).
distance(tambi,faerun,39).
distance(tambi,norrath,113).
distance(tambi,straylight,130).
distance(tambi,tristram,35).
distance(tambi,arbre,40).
distance(faerun,norrath,63).
distance(faerun,straylight,21).
distance(faerun,tristram,57).
distance(faerun,arbre,83).
distance(norrath,straylight,9).
distance(norrath,tristram,50).
distance(norrath,arbre,60).
distance(straylight,tristram,27).
distance(straylight,arbre,81).
distance(tristram,arbre,90).

direct_route(A,B,Distance) :- distance(A,B,Distance).
direct_route(A,B,Distance) :- distance(B,A,Distance).

locations(Ls) :- findall(X, location(X), Results).

permute([], []).
permute([X|Rest], L) :-
    permute(Rest, L1),
    select(X, L, L1).

travel(A,B,[B]) :- direct_route(A,B,_).

travel(A,B,[X|Visited]) :-
    direct_route(A,X,_),
    X \== B,
    travel(X,B,Visited),
    \+ member(X,Visited).


