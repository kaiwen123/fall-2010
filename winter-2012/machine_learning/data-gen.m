% This function is used to generate test data using three
% generative models. 
function [X, y] = data-gen(n, t, sigma, model)
    u = [0; ones(n-1, 1)];
    X = [ones(t,1) rand(t, n-1)];
    if model == 1
        y = X * u + randn(t, 1) * sigma
    else if model == 2
            y = X * u + randn(t, 1) ./ randn(t, 1) * sigma
    else 
        y = X * u + randn(t, 1) .* randn(t, 1) * sigma
    end if
