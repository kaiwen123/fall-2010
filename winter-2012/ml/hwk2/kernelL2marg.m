% dual kernel max margin classifier.
% X training data set. t x n. 
% y training label data. t x 1. 
% c slack parameter. 
% kernfun the name of the kernel function. 
% par parameter value for the kernel function. might be d, the degree. 
function [lambda, b] = kernelL2marg(X, y, c, kernfun, par)
    t = size(X, 1); % number of samples.
    %n = size(X, 2); % dimension. 
    
    H = y * y' .* feval(kernfun, X, X, par);
       
    f = -ones(1, t); 
    
    Aeq = y'; 
    beq = 0; 
    
    lb = zeros(t, 1);  % lower bound. 
    c = c * ones(t, 1);% upper bound.
    
    %% quadratic programming. 
    lambda = quadprog(H, f, [], [], Aeq, beq, lb, c); 

    % compute b, with the model parameter lambda. 
    [l, i] = max(lambda);
    y1 = y(i,1); 
    x1 = X(i,:); 
    b = ones(t, 1)' * (X * x1') - 1 / y1;