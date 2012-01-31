% regularization with radial kernel. 
% X is t x n; 
% y is t x 1; 
% lambda is panelize parameter; 
% sigma is the kernel parameter. 
function u = minRegL2RadialKernel (X, y, sigma, lambda)
t = size(X, 1); % sample X row count, sample size. 
n = size(X, 2); % sample dimension. 
Xt = X'; 
G = zeros(t, t); 

for i = 1:t
    for j = 1:t
    xtmp = (X(i, :) - Xt(:, j)') / sigma; 
    G(i,j) = exp(-1 / (2 * (xtmp * xtmp'))); % radial kernel. 
    end 
end

% compute u. 
u = (G' * G + lambda * eye(t)) \ G' * y; 