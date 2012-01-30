% This function is used to generate test data using three
% generative models. 
function [X, y] = datagen(model, n, t, u, sigma) 
X = [ones(t, 1) rand(t, n-1)]; 

if model == 1
    y = X*u + randn(t,1) * sigma; 
elseif model == 2 
    y = X*u + randn(t,1)./randn(t,1)*sigma; 
else
    y = X*u + randn(t,1).*randn(t,1)*sigma; 
endif