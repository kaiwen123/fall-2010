% data parameters. 
n = 2;                                  % dimension.
t = 100;                                % number of samples.
sigma = 0.1;                            % noise level.
model = 1;                              % moEMCdel.

% dA -> get random testing data.
for model = 1:3
    %%  get data. 
    t = 100; 
    [X, y] = datagen(n, t, sigma, model);

    %% build models. 
    w1 = minL1(X', y); 
    w2 = minL2(X', y); 
    woo = minLoo(X', y); 
    
    %% calculate errors for training data. 
    w1l1 = sum(abs(X * w1 - y)); % w1 l1 error. 
    w1l2 = (X * w1 - y)' * (X * w1 - y); % w1 l2 error. 
    w1loo = max(abs(X * w1 - y)); % w1 loo error.

    w2l1 = sum(abs(X * w2 - y)); % w1 l1 error. 
    w2l2 = (X * w2 - y)' * (X * w2 - y); % w1 l2 error. 
    w2loo = max(abs(X * w2 - y)); % w1 loo error.
    
    wool1 = sum(abs(X * woo - y)); % w1 l1 error. 
    wool2 = (X * woo - y)' * (X * woo - y); % w1 l2 error. 
    wooloo = max(abs(X * woo - y)); % w1 loo error.
    
    %% print training errors.
    fprintf('\n\n============================================\n');
    fprintf('Training errors for data model %d with linear hypothesis: \n', model); 
    fprintf('\tL1error\tL2error\tLooerror\n'); 
    fprintf('w1\t%.2f\t%.2f\t%.2f\n', w1l1, w1l2, w1loo); 
    fprintf('w2\t%.2f\t%.2f\t%.2f\n', w2l1, w2l2, w2loo);
    fprintf('woo\t%.2f\t%.2f\t%.2f\n', wool1, wool2, wooloo);    
    
    %% plot result. 
    % dB -> plot 2D graph. 
    figure(model); 
    clf
    grid on;
    plot(X(:,2)', y', 'k*')
    hold on
    plot([0 1], [w2(1) sum(w2)], 'k-'); 
    plot([0 1], [w1(1) sum(w1)], 'k-.');
    plot([0 1], [woo(1) sum(woo)], 'k:'); 
    legend('Data point', 'L2 norm.', 'L1 norm.', 'Loo norm.');
    if model == 1
        title('Linear Predictors for model 1.'); 
        print -deps experiment.1.1.1.ps 
    elseif model == 2
        title('Linear Predictors for model 2.'); 
        print -deps experiment.1.1.2.ps 
    elseif model == 3
        title('Linear Predictors for model 3.'); 
        print -deps experiment.1.1.3.ps
    end
    
    %% generate testing data. 
    t = 1000; 
    [X, y] = datagen(n, t, sigma, model);
    w1l1 = sum(abs(X * w1 - y)); % w1 l1 error. 
    w1l2 = (X * w1 - y)' * (X * w1 - y); % w1 l2 error. 
    w1loo = max(abs(X * w1 - y)); % w1 loo error.

    w2l1 = sum(abs(X * w2 - y)); % w1 l1 error. 
    w2l2 = (X * w2 - y)' * (X * w2 - y); % w1 l2 error. 
    w2loo = max(abs(X * w2 - y)); % w1 loo error.
    
    wool1 = sum(abs(X * woo - y)); % w1 l1 error. 
    wool2 = (X * woo - y)' * (X * woo - y); % w1 l2 error. 
    wooloo = max(abs(X * woo - y)); % w1 loo error.
    
    %% print testing errors. 
    fprintf('Testing errors for data model %d with linear hypothesis: \n', model); 
    fprintf('\tL1error\tL2error\tLooerror\n'); 
    fprintf('w1\t%.2f\t%.2f\t%.2f\n', w1l1, w1l2, w1loo); 
    fprintf('w2\t%.2f\t%.2f\t%.2f\n', w2l1, w2l2, w2loo);
    fprintf('woo\t%.2f\t%.2f\t%.2f\n', wool1, wool2, wooloo);    
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate average errors for both training and testing. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate average errors for the above.
round = 100; % run how many times? 
% initialize average training error to zeros. 
trw1l1 = 0.0; trw1l2 = 0.0; trw1loo = 0.0; 
trw2l1 = 0.0; trw2l2 = 0.0; trw2loo = 0.0; 
trwool1 = 0.0; trwool2 = 0.0; trwooloo = 0.0; 

% initialize average testing error to zeros. 
tew1l1 = 0.0; tew1l2 = 0.0; tew1loo = 0.0; 
tew2l1 = 0.0; tew2l2 = 0.0; tew2loo = 0.0; 
tewool1 = 0.0; tewool2 = 0.0; tewooloo = 0.0; 
for model = 1:3
    for i = 1:round
        %%  get data. 
        t = 100; 
        [X, y] = datagen(n, t, sigma, model);

        %% build models. 
        w1 = minL1(X', y); 
        w2 = minL2(X', y); 
        woo = minLoo(X', y); 

        %% calculate errors for training data. 
        w1l1 = sum(abs(X * w1 - y)); % w1 l1 error. 
        w1l2 = (X * w1 - y)' * (X * w1 - y); % w1 l2 error. 
        w1loo = max(abs(X * w1 - y)); % w1 loo error.
        trw1l1 = trw1l1 + w1l1 / round; 
        trw1l2 = trw1l2 + w1l2 / round;
        trw1loo = trw1loo + w1loo / round;

        w2l1 = sum(abs(X * w2 - y)); % w1 l1 error. 
        w2l2 = (X * w2 - y)' * (X * w2 - y); % w1 l2 error. 
        w2loo = max(abs(X * w2 - y)); % w1 loo error.
        trw2l1 = trw2l1 + w2l1 / round; 
        trw2l2 = trw2l2 + w2l2 / round;
        trw2loo = trw2loo + w2loo / round;

        wool1 = sum(abs(X * woo - y)); % w1 l1 error. 
        wool2 = (X * woo - y)' * (X * woo - y); % w1 l2 error. 
        wooloo = max(abs(X * woo - y)); % w1 loo error.
        trwool1 = trwool1 + wool1 / round; 
        trwool2 = trwool2 + wool2 / round;
        trwooloo = trwooloo + wooloo / round;

        %% generate testing data. fprintf('\nError for testing data with model %d.\n', model);
        [X, y] = datagen(n, t, sigma, model);
        w1l1 = sum(abs(X * w1 - y)); % w1 l1 error. 
        w1l2 = (X * w1 - y)' * (X * w1 - y); % w1 l2 error. 
        w1loo = max(abs(X * w1 - y)); % w1 loo error.
        tew1l1 = tew1l1 + w1l1 / round; 
        tew1l2 = tew1l2 + w1l2 / round;
        tew1loo = tew1loo + w1loo / round;

        w2l1 = sum(abs(X * w2 - y)); % w1 l1 error. 
        w2l2 = (X * w2 - y)' * (X * w2 - y); % w1 l2 error. 
        w2loo = max(abs(X * w2 - y)); % w1 loo error.
        tew2l1 = tew2l1 + w2l1 / round; 
        tew2l2 = tew2l2 + w2l2 / round;
        tew2loo = tew2loo + w2loo / round;

        wool1 = sum(abs(X * woo - y)); % w1 l1 error. 
        wool2 = (X * woo - y)' * (X * woo - y); % w1 l2 error. 
        wooloo = max(abs(X * woo - y)); % w1 loo error.
        tewool1 = tewool1 + wool1 / round; 
        tewool2 = tewool2 + wool2 / round;
        tewooloo = tewooloo + wooloo / round;


    end
    %% errors for each model.
    %% print average training errors.
    fprintf('\n\n=========================================\n'); 
    fprintf('Average Training errors for data model %d with linear hypothesis with %d runs: \n', model, round); 
    fprintf('\tL1error\tL2error\tLooerror\n'); 
    fprintf('w1\t%.2f\t%.2f\t%.2f\n', trw1l1, trw1l2, trw1loo); 
    fprintf('w2\t%.2f\t%.2f\t%.2f\n', trw2l1, trw2l2, trw2loo);
    fprintf('woo\t%.2f\t%.2f\t%.2f\n', trwool1, trwool2, trwooloo);  

    %% print average testing errors. 
    fprintf('Average testing errors for data model %d with linear hypothesis with %d runs: \n', model, round); 
    fprintf('\tL1error\tL2error\tLooerror\n'); 
    fprintf('w1\t%.2f\t%.2f\t%.2f\n', tew1l1, tew1l2, tew1loo); 
    fprintf('w2\t%.2f\t%.2f\t%.2f\n', tew2l1, tew2l2, tew2loo);
    fprintf('woo\t%.2f\t%.2f\t%.2f\n', tewool1, tewool2, tewooloo);
end
 