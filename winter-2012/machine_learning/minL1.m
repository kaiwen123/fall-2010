% This is minL2(X,y) which takes a txn matrix X and tx1 vector of y
% and returns an n x 1 vector of weights w2 corresponding to the
% minimum sum of squared errors linear function. 
function w1 = minL1 (X, y)
% minimum sum of squared error is used as error function. 
w1 = inv(X * X') * X * y; 