% The maximum "soft" margin linear classifier. 
% X --> training data t x n; 
% y --> training label t x 1; 
% c --> scalar. 
function [lambda, b] = dualsoftL2(X, y, c)
    t = size(X, 1); % number of samples.
    n = size(X, 2); % dimension. 
    
    % build the model paramters. 
    H = ones(t,t); 
    
    % xk' * xj
    for i = 1:t
        for j = 1:t
            H(i,j) = (y(i, 1) * X(i, :)) * (y(j,1) * X(j, :))'; 
        end
    end
       
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