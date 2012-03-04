% data generation function to generate data for training and testing. 
% n is dimension. 
% t size/count of the training data. 
% p_pos probability of positive example.
function [X, y] = dataGen (t, n, p_pos, model)
    u = ones(n, 1); % target weights. 
    v = 0.5 * n;    % target offset. 
    mu_pos = ones(n, 1); % mean loc for positive (model 3)
    mu_neg = zeros(n, 1); % mean loc for negative. (model 3)
    
    X = rand(t, n); 
    if model == 1
        y = sign(X * u - v); 
    elseif model == 2 
        y = sign(X .^ 2 * u - v); 
    elseif model == 3
        y = 2 * (rand(t, 1) < p_pos) - 1; 
        pos = find(y > 0); 
        neg = find(y < 0); 
        X(pos, 1) = X(pos, 1) + mu_pos(1); 
        X(pos, 2) = X(pos, 2) + mu_pos(2); 
        X(neg, 1) = X(neg, 1) + mu_neg(1); 
        X(neg, 2) = X(neg, 2) + mu_neg(2); 
    end