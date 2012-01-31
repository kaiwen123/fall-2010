% This is minLoo(X,y) which takes a txn matrix X and tx1 vector of y
% and returns an n x 1 vector of weights w2 corresponding to the
% minimum sum of squared errors linear function. 
function woo = minLoo (X, y)
% minimize maximum error function. argmin(max(h(xi) - yi)).
n = size(X, 1); % Dimension of test data. 
t = size(X, 2); % number of samples.

Xt = X'; 
I = ones(t, 1); 

f = [zeros(n,1); 1]; 
A = [Xt, -I; -Xt, -I]; % 2t x (n+t).
B = [y; -y]; 

woo = linprog(f, A, B); % w2 now is n + 1. 
woo = woo(1:n,1);