% building dual classification model. 
% X --> t x n;
% y --> t x 1; 
% lambda --> 1 x t; 
% b 1 x 1 scalar.
function [lambda, b] = dualL2marg(X, y) 
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
    
    H = H + 1e-10 * eye(t);
    
    f = -ones(1, t); %% should be minimized. 
    
    Aeq = y'; 
    beq = 0; 
    
    lb = zeros(t, 1);  % lower bound. 
    
    %% quadratic programming. 
    lambda = quadprog(H, f, [], [], Aeq, beq, lb, []); 
    
    % compute b, with the model parameter lambda. 
    [l, i] = max(lambda);
    y1 = y(i,1); 
    x1 = X(i,:); 
    b = ones(t, 1)' * (X * x1') - 1 / y1;
    
    