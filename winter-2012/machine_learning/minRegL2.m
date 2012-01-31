% regularized training model. 
% X is t x n; 
% y is t x 1; 
% lambda is regularization parameter. 
% w is weight vector corresponding to the minimum penalized sum of squared
% errors. 
function w = minRegL2 (X, y, lambda)
% t = size(X, 1); % sample size. 
n = size(X, 2); % dimension. 
w = (X' * X + lambda * eye(n)) \ X' * y; 