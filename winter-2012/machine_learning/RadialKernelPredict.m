% a function to predict over radial kernel. 
% X is training data set with dimension : t x n; 
% u is weight vector. 
% sigma is used for radial function. this is not needed? 
% Xtest is the test data set to predict with u and X. 
function yhat = RadialKernelPredict (X, u, sigma, Xtest)
t = size(X, 1); % sample X row count, sample size. 
tt = size(Xtest, 1); % number of test cases. 
%Xt = X'; 
Xtest = Xtest'; 
yhat = zeros(tt, 1);

for i = 1:tt
    yhat(i, 1) = 0.0;
    for j = 1:t
        xtmp = (X(j, :) - Xtest(:, i)') / sigma; 
        g = exp((-1) / (2 * (xtmp * xtmp'))); % radial kernel. 
        yhat(i, 1) = yhat(i, 1) + u(j, 1) * g; 
    end 
end

% return the mean sum of errors. 
yhat = sum(abs(yhat)) / tt; 