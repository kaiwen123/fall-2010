% this function computes weights based on minimum sum of squared 
% errors, with polynomial expansion d. 
% returned X is the expanded training input data. 
function c = minL2poly (x, y, d)
t = size(x, 1); 
X = ones(t, d + 1);

%% polynomial expansion.
tmp = ones(t, 1);
for i = 1:d
   tmp = tmp .* x; 
   X(:,d + 1 - i) = tmp; 
end

%% minimum sum of least square errors. 
c = (X' * X) \ X' * y;