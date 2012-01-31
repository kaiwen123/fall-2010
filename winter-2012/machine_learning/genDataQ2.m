% t is number of data. 
% sigma is noise level. 
% model is the model to use for data. 
function [x, y] = genDataQ2 (t, sigma, model)
x = rand(t, 1); % training input data.

if model == 1
    y = x > 0.5; % step model.
else 
    y = 0.5 - 10.4 * x .* (x - 0.5) .* (x - 1) + sigma * randn(t, 1); % noisy.
end
