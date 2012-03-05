% function to compute the L2 margin linear discriminant classifier. 
% X --> training data with dimension t x n. 
% y --> training label with dimension t x 1. 
% w --> weight of linear discriminant. 
% b --> scalar offset.
% we need to use the quadratic programming function. 
function [w, b] = maxL2marg (X, y)
    t = size(X, 1); % number of data. 
    n = size(X, 2); % dimension. 
    H = eye(n); 
    f = zeros(n, 1); 
    A = 
    