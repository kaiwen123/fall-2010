% This is minL1(X,y) which takes a nxt matrix X and tx1 vector of y
% and returns an n x 1 vector of weights w2 corresponding to the
% minimum sum of squared errors linear function. 
function w2 = minL1 (X, y)
% minimum sum of squared error is used as error function. 
n = size(X, 1); % Dimension of test data. 
t = size(X, 2); % number of samples.

Xt = X'; 
I = eye(t); 

f = [zeros(n,1); ones(t, 1)]; 
A = [Xt, -I; -Xt, -I]; % 2t x (n+t).
B = [y; -y]; 

w2 = linprog(f, A, B); % w2 now is n + t. 
w2 = w2(1:n,1);