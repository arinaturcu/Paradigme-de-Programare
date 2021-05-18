clone([], []) :- true.
clone([H|T],[H|Z]):- clone(T,Z).