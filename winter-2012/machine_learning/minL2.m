% This is minL2(X,y) which takes a txn matrix X and tx1 vector of y
% and returns an n x 1 vector of weights w2 corresponding to the
% minimum sum of squared errors linear function. 
function w2 = minL2 (X, y)

% minimum sum of squared error is used as error function. 
w2 = inv(X * X') * X * y; 