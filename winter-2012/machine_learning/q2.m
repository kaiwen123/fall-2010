% This file answers question 2 of homework. 
n = 1; % dimension. 
t = 100; % training sample numbers; 
sigma = 0.1; % noise level. 
model = 1; 
d = 5; % training data expansion. 

% question 2b. 
for model = 1:2
    t = 100; 
    [x, y] = genDataQ2(t, sigma, model); 
    %% generate models. X1, X3, X5 and X9 and the spanned data points.
    c1 = minL2poly(x, y, 1);
    c3 = minL2poly(x, y, 3); 
    c5 = minL2poly(x, y, 5); 
    c9 = minL2poly(x, y, 9);

    %% for each model, generate data and plot picture for models. 
    
    figure(model);
    clf
    %axis([0 1 -0.5 1.5]);
    hold on
    grid on
    %plot(x, y,'k*') % original data.
    %% plot data to show the performance. 
    xx = (0:100)/100;
    yy = genlabel(xx, sigma, model); % < mean value of generative model at xx>
    plot(xx,yy,'rx')
    plot(xx,polyval(c1,xx),'k-')
    plot(xx,polyval(c3,xx),'g-.')
    plot(xx,polyval(c5,xx),'b-o')
    plot(xx,polyval(c9,xx),'y-*')
    legend('Original data', 'With c1', 'With c3', 'With c5', 'With c9', 'Location', 'SouthEast'); 
    title('Polynomial fitting with different polynomial degrees.');
    
    xlabel('x');
    ylabel('y'); 
    
    %% save pictures into files. 
    if model == 1 
        print -deps experiment.1.2.1.ps
    elseif model == 2 
        print -deps experiment.1.2.2.ps
    else 
        fprintf('model should be 1 or 2.');
    end
    
    %% calculate errors. 
    c1l2error = (polyval(c1, x) - y)' * (polyval(c1, x) - y); 
    c3l2error = (polyval(c3, x) - y)' * (polyval(c3, x) - y); 
    c5l2error = (polyval(c5, x) - y)' * (polyval(c5, x) - y); 
    c9l2error = (polyval(c9, x) - y)' * (polyval(c9, x) - y); 
    
    %% print out errors. 
    fprintf('\n\n===============================================================\n');
    fprintf('Training errors for data model %d with polynomial bases: \n', model); 
    fprintf('L2 error for c1 is: %.5f\n', c1l2error / t);
    fprintf('L2 error for c3 is: %.5f\n', c3l2error / t);
    fprintf('L2 error for c5 is: %.5f\n', c5l2error / t);
    fprintf('L2 error for c9 is: %.5f\n', c9l2error / t);
    
    
    %% generate testing data and report the errors with given coefficients.
    t = 1000; 
    [x, y] = genDataQ2(t, sigma, model); 
    
    %% calculate testing errors. 
    c1l2error = (polyval(c1, x) - y)' * (polyval(c1, x) - y); 
    c3l2error = (polyval(c3, x) - y)' * (polyval(c3, x) - y); 
    c5l2error = (polyval(c5, x) - y)' * (polyval(c5, x) - y); 
    c9l2error = (polyval(c9, x) - y)' * (polyval(c9, x) - y); 
    
    %% print out errors. 
    fprintf('\nTesting errors for data model %d with polynomial bases: \n', model); 
    fprintf('L2 error for c1 is: %.5f\n', c1l2error / t);
    fprintf('L2 error for c3 is: %.5f\n', c3l2error / t);
    fprintf('L2 error for c5 is: %.5f\n', c5l2error / t);
    fprintf('L2 error for c9 is: %.5f\n', c9l2error / t);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Average training and testing over 100 runs. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% repeat 100 times.
round = 100; 
trc1l2error = 0.0; 
trc3l2error = 0.0; 
trc5l2error = 0.0; 
trc9l2error = 0.0;

tec1l2error = 0.0; 
tec3l2error = 0.0; 
tec5l2error = 0.0; 
tec9l2error = 0.0; 
for model = 1:2
    for i = 1:round
        %% generate data for training. 
        t = 100; 
        [x, y] = genDataQ2(t, sigma, model); 
        
        %% generate models. 
        c1 = minL2poly(x, y, 1);
        c3 = minL2poly(x, y, 3); 
        c5 = minL2poly(x, y, 5); 
        c9 = minL2poly(x, y, 9); 

        %% calculate errors. 
        trc1l2error = trc1l2error + (polyval(c1, x) - y)' * (polyval(c1, x) - y) / round; 
        trc3l2error = trc3l2error + (polyval(c3, x) - y)' * (polyval(c3, x) - y) / round; 
        trc5l2error = trc5l2error + (polyval(c5, x) - y)' * (polyval(c5, x) - y) / round;  
        trc9l2error = trc9l2error + (polyval(c9, x) - y)' * (polyval(c9, x) - y) / round;  
        %% generate testing data and report the errors with given coefficients.
        t = 1000; 
        [x, y] = genDataQ2(t, sigma, model); 

        %% calculate errors. 
        tec1l2error = tec1l2error + (polyval(c1, x) - y)' * (polyval(c1, x) - y) / round; 
        tec3l2error = tec3l2error + (polyval(c3, x) - y)' * (polyval(c3, x) - y) / round; 
        tec5l2error = tec5l2error + (polyval(c5, x) - y)' * (polyval(c5, x) - y) / round; 
        tec9l2error = tec9l2error + (polyval(c9, x) - y)' * (polyval(c9, x) - y) / round; 
    end
    %% print out average training errors.
    fprintf('\n\n+++++++++++++++++++++++++++++++++++++++\n');
    fprintf('Average training error for data model %d with polynomial bases over %d rounds. \n', model, round);
    fprintf('L2 error for c1 is: %.5f\n', trc1l2error / t);
    fprintf('L2 error for c3 is: %.5f\n', trc3l2error / t);
    fprintf('L2 error for c5 is: %.5f\n', trc5l2error / t);
    fprintf('L2 error for c9 is: %.5f\n', trc9l2error / t);

    %% print out average testing errors.
    fprintf('\nAverage testing error for data model %d with polynomial bases over %d rounds. \n', model, round);
    fprintf('L2 error for c1 is: %.5f\n', tec1l2error / t);
    fprintf('L2 error for c3 is: %.5f\n', tec3l2error / t);
    fprintf('L2 error for c5 is: %.5f\n', tec5l2error / t);
    fprintf('L2 error for c9 is: %.5f\n', tec9l2error / t);
end


