mm(x, [x|r]).
mm(x, [_|r]) :- mm(x, r).
