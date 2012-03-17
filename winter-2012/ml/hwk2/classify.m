% classification of testing data. This function classify 
% given testing data and return the result with given max margin
% model. 
% Xtest the test input data. 
% w the weight of the linear discriminant model. 
% b the offset of the model. 
% yhat --> predicated label for testing data. 
function [yhat] = classify(Xtest, w, b)
    yhat = sign(Xtest * w - b * ones(size(Xtest, 1), 1)); 