% This is minL2(X,y) which takes a txn matrix X and tx1 vector of y
% and returns an n x 1 vector of weights w2 corresponding to the
% minimum sum of squared errors linear function. 
function woo = minLoo (X, y)
% minimize maximum error function. argmin(max(h(xi) - yi)).
linprog();

woo = inv(X * X') * X * y; 