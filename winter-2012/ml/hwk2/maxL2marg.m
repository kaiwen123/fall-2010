% function to compute the L2 margin linear discriminant
% classifier. 
% quadprog will be used here. 
% X --> training data with dimension t x n. 
% y --> training label with dimension t x 1. 
% w --> weight of linear discriminant. 
% b --> scalar offset.
% variable x = [w; b] (n + 1) x 1.
% we need to use the quadratic programming function. 
function [w, b] = maxL2marg (X, y)
    t = size(X, 1); % number of data. 
    n = size(X, 2); % dimension. 

    % quadprog matrices. 
    H = zeros(n + 1); 
    for i = 1:n
        H(i, i) = 1; 
    end 

    f = zeros(n+1, 1); 
    
    yy = zeros(size(X)); 
    for i = 1:n 
        yy(:,i) = y; 
    end
    % constraints.
    A = [-X .* yy, y]; 
    c = -1 * ones(t, 1); 

    % Quadratic programming. 
    %opts = optimset('Algorithm', 'active-set', 'Display', 'off');
    x = quadprog(H,f,A,c);
    
    % setup the result. 
    w = x(1:n, 1); 
    b = x(n+1, 1); 