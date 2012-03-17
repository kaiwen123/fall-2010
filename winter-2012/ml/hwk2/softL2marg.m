% maximum soft margin linear discriminant classifier. the slack variable 
% s was added into the optimization model. 
% X input training data;
% y input training label; 
% c scalar argument. 
% w weight of the model. 
% b the scalar offset of model. 
% variable is x = [w; b; s].
function [w, b] = softL2marg(X, y, c) 
    t = size(X, 1); % number of training samples. 
    n = size(X, 2); % dimension of the training data set. 
    
    % for quadratic programming.
    H = zeros(n + 1 + n); % x = [w; b; s]; 
    for i = 1:n 
        H(i, i) = 1;
    end
    
    f = zeros(n + n + 1, 1); 
    for i = n+2:n+n+1
        f(i,1) = c; 
    end 
    
    yy = zeros(size(X)); 
    for i = 1:n
        yy(:,i) = y; 
    end
    
    % constrain matrix. 
    A = [-X .* yy, y, -1 * ones(t, n)];
    d = -1 * ones(t, 1); 
    
    % Quadratic programming. 
    %opts = optimset('Algorithm', 'active-set', 'Display', 'off');
    x = quadprog(H,f,A,d);
    
    % setup the result. 
    w = x(1:n, 1); 
    b = x(n+1, 1); 