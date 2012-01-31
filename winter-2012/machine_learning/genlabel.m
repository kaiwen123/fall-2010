function y = genlabel (x, sigma, model)
t = size(x, 1); 
if model == 1
    y = x > 0.5; % step model.
else 
    y = 0.5 - 10.4 * x .* (x - 0.5) .* (x - 1) + sigma * randn(t, 1); % noisy.
end
