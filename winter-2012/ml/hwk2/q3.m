% This is question 3 of homework 2 for machine learning course. 
% The purpose of this problem is to use maximum margin classifier
% and do the classification with the classification model.
% The data was provided in a .mat file, we need to load the data
% set from the file. 

% load the data set from file. 
load data2.mat; 

ttr = size(X, 1);
tte = size(Xtest, 1); 

% explore the training data. 
% colormap gray; 
% for i = 1:size(X, 1)
%     %figure(i);
%     imagesc(reshape(X(i,:),16,16)'); 
% end

% build the model. 
[ll bl] = kernelL2marg(X,y,100,'polykernel',1);
[lq bq] = kernelL2marg(X,y,100,'polykernel',2);
[lc bc] = kernelL2marg(X,y,100,'polykernel',3);

% training result. 
yhattrain1 = kernelclassify(X, ll, bl, X, y, 'polykernel', 1);
yhattrain2 = kernelclassify(X, lq, bq, X, y, 'polykernel', 2);
yhattrain3 = kernelclassify(X, lc, bc, X, y, 'polykernel', 3);

% testing result.
yhattest1 = kernelclassify(Xtest, ll, bl, X, y, 'polykernel', 1);
yhattest2 = kernelclassify(Xtest, lq, bq, X, y, 'polykernel', 2);
yhattest3 = kernelclassify(Xtest, lc, bc, X, y, 'polykernel', 3);

% compute the training and testing error.
err11 = sum(yhattrain1 ~= y) / ttr * 100; 
err22 = sum(yhattrain2 ~= y) / ttr * 100; 
err33 = sum(yhattrain3 ~= y) / ttr * 100; 

err1 = sum(yhattest1 ~= ytest) / tte * 100; 
err2 = sum(yhattest2 ~= ytest) / tte * 100; 
err3 = sum(yhattest3 ~= ytest) / tte * 100; 

% print the result to the screen. 
fprintf('Testing error with with polykernel 1 is %f%%.\n', err11);
fprintf('Testing error with with polykernel 1 is %f%%.\n', err22);
fprintf('Testing error with with polykernel 1 is %f%%.\n', err33);

fprintf('Testing error with with polykernel 1 is %f%%.\n', err1);
fprintf('Testing error with with polykernel 1 is %f%%.\n', err2);
fprintf('Testing error with with polykernel 1 is %f%%.\n', err3);