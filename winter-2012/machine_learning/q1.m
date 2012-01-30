% data parameters. 
n = 2;                                  % dimension.
t = 100;                                % number of samples.
sigma = 0.1;                            % noise level.
model = 1;                              % model.

% dA -> get random testing data.
[X ,y] = datagen(n, t, sigma, model);

w1 = minL1(X, y); 
w2 = minL2(X, y); 
woo = minLoo(X, y); 

% dB -> plot 2D graph. 
clf
plot(X(:,2)', y', 'k*')
hold on
plot([0 1], [w2(1) sum(w2)], 'k-')
plot([0 1], [w1(1) sum(w1)], 'k-.')
plot([0 1], [woo(1) sum(woo)], 'k:')
print -deps experiment.1.1.<k>.ps 

% dC

% dD -> generate new test data. 
t = 1000; 
[X, y] = datagen(n, t, sigma, model); 

