% maximum soft margin linear discriminant classifier. the slack variable 
% s was added into the optimization model. 
% X input training data;
% y input training label; 
% c scalar argument. 
% w weight of the model. 
% b the scalar offset of model. 
function [w, b] = softL2marg(X, y, c) 
    t = size(X, 1); % number of training samples. 
    n = size(X, 2); % dimension of the training data set. 
    
    % for quadratic programming.
    H = zeros(n + t + 1) % x = [w; b; s]; 
    for i = 1:n 
        H(i, i) = 1;
    end
    
    f = zeros(n + t + 1, 1); 
    for i = n+2:n+t+1
        f(i,i) = c; 
    end 
    
    % constrain matrix. 
    A = [-X * y, y, -1 * ones(t, 1)];
    d = -1 * ones(t, 1); 
    
    % Quadratic programming. 
    opts = optimset('Algorithm', 'active-set', 'Display', 'off');
    [x,fval,exitflag,output,lambda] = quadprog(H,f,A,d,[],[],lb,[],[],opts);
    
    % setup the result. 
    w = x(1:n, 1); 
    b = x(n+1, 1); 