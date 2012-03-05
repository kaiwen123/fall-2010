% maximum soft margin linear discriminant classifier. 
% X input training data;
% y input training label; 
% c scalar argument. 
% w weight of the model. 
% b the scalar offset of model. 
function [w, b] = softL2marg(X, y, c) 
