% classification using the kernel method. 
% Xtest te x n the test data set. 
% lambda t x 1 Laglange multiplier. 
% b scalar offset. 
% X t x n matrix of training dataset. 
% y t x 1 vector of training label. 
% yhat te x 1 vector of predicted labels for the testing dataset. 
function [yhat] = kernelclassify(Xtest, lambda, b, X, y, kernfun, par)
    t1 = size(X, 1); 
    t2 = size(Xtest, 1); 
    
    yhat = zeros(t2, 1); 
    for i = 1:t2
        mm = 0;
        for j = 1:t1
            mm = mm + y(j, 1) * lambda(j, 1) * feval(kernfun, X(j,:), Xtest(i,:), par); 
        end
        yhat(i,1) = mm + b; 
    end
    
    yhat = sign(yhat); 