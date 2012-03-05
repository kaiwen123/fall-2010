% this is question 3 of homework 1. 

load data1.mat % load the cancer data set.
t = size(X, 1); % number of training samples.
n = size(X, 2); % dimension. 

%% build the model.
lambda = 0.5; 

w0 = minRegL2(X, y, 0); 
w1 = minRegL2(X, y, 24); 
u0 = minRegL2RadialKernel(X, y, 14, 0); 
u1 = minRegL2RadialKernel(X, y, 14, 0.002);

%% calculate the mean sum of squared errors. 
trw0error = (X * w0 - y)' * (X * w0 - y); 
trw1error = (X * w1 - y)' * (X * w1 - y); 
tru0error = RadialKernelPredict(X, u0, 14, X); 
tru1error = RadialKernelPredict(X, u1, 14, X); 

tew0error = (Xtest * w0 - ytest)' * (Xtest * w0 - ytest); 
tew1error = (Xtest * w1 - ytest)' * (Xtest * w1 - ytest); 
teu0error = RadialKernelPredict(X, u0, 14, Xtest); 
teu1error = RadialKernelPredict(X, u1, 14, Xtest); 

%% error reporting. 
% testing error reporting. 
fprintf('\n\n===========================================\n');
fprintf('Training errors for cancer data:\n'); 
fprintf('Training error with w0: %.5f\n', trw0error / t);
fprintf('Training error with w1: %.5f\n', trw1error / t);
fprintf('Training error with u0: %.5f\n', tru0error / t);
fprintf('Training error with u1: %.5f\n', tru1error / t);

% testing error reporting. 
fprintf('\n\n===========================================\n');
fprintf('Testing error for cancer data:\n'); 
fprintf('Testing error with w0: %.5f\n', tew0error / t);
fprintf('Testing error with w1: %.5f\n', tew1error / t);
fprintf('Testing error with u0: %.5f\n', teu0error / t);
fprintf('Testing error with u1: %.5f\n', teu1error / t);

%% plot the data. 
x = 1:size(Xtest,1); 
hold on;
plot(x, ytest, 'r-*');
plot(x, Xtest * w0, 'g-o');
plot(x, Xtest * w1, 'k-.');
% plot(x, Xtest * u0, 'ro');
% plot(x, Xtest * u1, 'b-.');



