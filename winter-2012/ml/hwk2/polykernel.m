% maximum margin kernel classifier. 
% X1 -> t1 x n; 
% X2 -> t2 x n; 
% d -> degree parameter. 
% K -> t1 x t2, the polynomial kernel values. 
function [K] = polykernel(X1, X2, d)
    t1 = size(X1, 1);                   % No. of samples for X1. 
    t2 = size(X2, 1);                   % No. of samples for X2. 
    %n = size(X1, 2);                    % Dimension. 
    
    K = zeros(t1, t2);
    
    % computing the K.
    for i = 1:t1
        for j = 1:t2
            K(i, j) = (X1(i, :) * X2(j, :)' + 1) ^ d;
        end
    end