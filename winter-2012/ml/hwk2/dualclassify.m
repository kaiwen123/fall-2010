% dual classification for given test dataset. 
% Xtest --> the to be classified dataset. 
% lambda --> dual classification model Lagrange multiplier. 
% b --> model scalar
% X --> training dataset. 
% y --> training data label; 
% yhat --> the predicted value for given Xtest. 
function [yhat] = dualclassify(Xtest, lambda, b, X, y)
    t = size(X, 1);
    n = size(X, 2); 
    M = ones(size(X)); 
    for i = 1:n
       M(:, i) = lambda .* y;  
    end
    
    X = X .* M;
    yhat = zeros(size(Xtest, 1), 1);
    
    for i = 1:size(Xtest, 1)
        tmp = ((ones(1, t) * X) * Xtest(i, :)') - b;
        yhat(i,1) = sign(tmp); 
    end