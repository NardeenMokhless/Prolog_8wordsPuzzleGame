% takeout(X,[X|R],R).  
% takeout(X,[F |R],[F|S]) :- 
%     takeout(X,R,S).

% perm([X|Y],Z) :- 
%     perm(Y,W),
%     takeout(X,Z,W).  

% perm([],[]).

takeEightWords(Y):-
	write("Word 1 : "),nl,
	read(Input),nl,
	atom_chars(Input,L1),   
	write("Word 2 : "),nl,
	read(Input1),nl,
	atom_chars(Input1,L2),
	write("Word 3 : "),nl,
	read(Input2),nl,
	atom_chars(Input2,L3),
	write("Word 4 : "),nl,
	read(Input3),nl,
	atom_chars(Input3,L4),
	write("Word 5 : "),nl,
	read(Input4),nl,
	atom_chars(Input4,L5),
	write("Word 6 : "),nl,
	read(Input5),nl,
	atom_chars(Input5,L6),
	write("Word 7 : "),nl,
	read(Input6),nl,
	atom_chars(Input6,L7),
	write("Word 8 : "),nl,
	read(Input7),nl,
	atom_chars(Input7,L8),
	Y = [L1,L2,L3,L4,L5,L6,L7,L8].

printRow(Pos,A5,A6,A7,A8):-
	nth1(Pos, A5, PA1),
	nth1(Pos, A6, PA2),
	nth1(Pos, A7, PA3),
	nth1(Pos, A8, PA4),
	write(" "),
	write(PA1),
	write(" "),
	write(PA2),
	write(" "),
	write(PA3),
	write(" "),
	write(PA4),
	write(" "),nl.

printRow([A1,A2,A3,A4,A5,A6,A7,A8,A9]):-
	write(A1),
	write(A2),
	write(A3),
	write(A4),
	write(A5),
	write(A6),
	write(A7),
	write(A8),
	write(A9),nl.



p([A1,A2,A3,A4,A5,A6,A7,A8]):-
	printRow(1,A5,A6,A7,A8),
	printRow(A1),
	printRow(3,A5,A6,A7,A8),
	printRow(A2),
	printRow(5,A5,A6,A7,A8),
	printRow(A3),
	printRow(7,A5,A6,A7,A8),
	printRow(A4),
	printRow(9,A5,A6,A7,A8).





move(P1,P2):-
        move2(P1,P2).

move2([H1,H2,H3,H4,V5,V6,V7,V8],[H2,H1,H3,H4,V5,V6,V7,V8]).

move(P1,P2):-
        move3(P1,P2).

move3([H1,H2,H3,H4,V5,V6,V7,V8],[H3,H2,H1,H4,V5,V6,V7,V8]).

move(P1,P2):-
        move4(P1,P2).
move4([H1,H2,H3,H4,V5,V6,V7,V8],[H4,H2,H3,H1,V5,V6,V7,V8]).

move(P1,P2):-
        move5(P1,P2).
move5([H1,H2,H3,H4,V5,V6,V7,V8],[V5,H2,H3,H4,H1,V6,V7,V8]).

move(P1,P2):-
        move6(P1,P2).
move6([H1,H2,H3,H4,V5,V6,V7,V8],[V6,H2,H3,H4,V5,H1,V7,V8]).

move(P1,P2):-
        move7(P1,P2).
move7([H1,H2,H3,H4,V5,V6,V7,V8],[V7,H2,H3,H4,V5,V6,H1,V8]).

move(P1,P2):-
        move8(P1,P2).
move8([H1,H2,H3,H4,V5,V6,V7,V8],[V8,H2,H3,H4,V5,V6,V7,H1]).



goal(
[[_,H12,_,H14,_,H16,_,H18,_],[_,H22,_,H24,_,H26,_,H28,_],[_,H32,_,H34,_,H36,_,H38,_],[_,H42,_,H44,_,H46,_,H48,_],[_,H12,_,H22,_,H32,_,H42,_],[_,H14,_,H24,_,H34,_,H44,_],[_,H16,_,H26,_,H36,_,H46,_],[_,H18,_,H28,_,H38,_,H48,_]]).            

%general algorithm
run():-
	takeEightWords(Y),
	go(Y).

%query of user and takes start state and next state
go(Start):-
	path([[Start,null]],[],[[H11,H12,H13,H14,H15,H16,H17,H18,H19],[H21,H22,H23,H24,H25,H26,H27,H28,H29],[H31,H32,H33,H34,H35,H36,H37,H38,H39],[H41,H42,H43,H44,H45,H46,H47,H48,H49],[H51,H12,H53,H22,H55,H32,H57,H42,H59],[H61,H14,H63,H24,H65,H34,H67,H44,H69],[H71,H16,H73,H26,H75,H36,H77,H46,H79],[H81,H18,H83,H28,H85,H38,H87,H48,H89]]),
	p([[H11,H12,H13,H14,H15,H16,H17,H18,H19],[H21,H22,H23,H24,H25,H26,H27,H28,H29],[H31,H32,H33,H34,H35,H36,H37,H38,H39],[H41,H42,H43,H44,H45,H46,H47,H48,H49],[H51,H12,H53,H22,H55,H32,H57,H42,H59],[H61,H14,H63,H24,H65,H34,H67,H44,H69],[H71,H16,H73,H26,H75,H36,H77,H46,H79],[H81,H18,H83,H28,H85,H38,H87,H48,H89]]).

%main predicate that takes open list, closed list and goal state
path([],_,_):-
		write('No solution'),nl,!.
path([[Goal,Parent] | _], Closed, Goal):-
		write('A solution is found'), nl ,
		printsolution([Goal,Parent],Closed),!.
path(Open, Closed, Goal):-
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		getchildren(State, Open, Closed, Children),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent] | Closed], Goal).

%gets Children of State that aren't in Open or Close
getchildren(State, Open ,Closed , Children):-
		bagof(X, moves( State, Open, Closed, X), Children), ! .
getchildren(_,_,_, []).

%adds children to open list (without head child) to form new open list
%here it is like append i.e.Breadth First
addListToOpen(Children, [], Children).
addListToOpen(Children, [H|Open], [H|NewOpen]):-
		addListToOpen(Children, Open, NewOpen).

%gets head of open list to get its children later
removeFromOpen([State|RestOpen], State, RestOpen).

%gets next state given the current state
moves( State, Open, Closed,[Next,State]):-
		move(State,Next),
		\+ member([Next,_],Open),
		\+ member([Next,_],Closed).

%prints the path from start state to goal state
printsolution([State, null],_):-
		write(State),nl.
printsolution([State, Parent], Closed):-
		member([Parent, GrandParent], Closed),
		printsolution([Parent, GrandParent], Closed),
		write(State), nl.
