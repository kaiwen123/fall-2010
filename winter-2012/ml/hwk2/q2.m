% This is problem one of machine learning homework. 
% First, build the dual model using maximum margin classification.
% Then, compute the training and testing errors for the given
% data. 
% Last, compute the average training and testing error for 100 runs
% of training and testing. 

for model = 1:3
    t = 100; 
    n = 2;
    u = ones(n, 1); 
    v = 0.5 * n; 
    p_pos = 0.5; 
    % A. generate training data. 
    [X, y] = dataGen(t, n, p_pos, model); 

    % build the training model.;
    [lm, bm] = dualL2marg(X,y);
    [ls, bs] = dualsoftL2(X,y,3);

    % B. plot the model with genertive data. 
    clf; axis([0 1 0 1]); hold; axis('square');
    pos = find(y > 0); plot(X(pos,1)',X(pos,2)','g+');
    neg = find(y < 0); plot(X(neg,1)',X(neg,2)','rx');
    plot([0 1],[v v-u(1)]/u(2),'k:');
    Z=X; for j = 1:n, Z(:,j) = X(:,j) .* y; end
    wm = lm' * Z;
    plot([0 1],[bm bm-wm(1)]/wm(2),'b-');
    spm = find(lm > 0); plot(X(spm,1)',X(spm,2)','co');
    ws = ls' * Z;
    plot([0 1],[bs bs-ws(1)]/ws(2),'m-');
    sps = find(ls > 0); plot(X(sps,1)',X(sps,2)','mo');

    if model == 1 
        print -deps experiment.2.1.1.ps
    elseif model == 2
        print -deps experiment.2.1.2.ps
    elseif model == 3
        print -deps experiment.2.1.3.ps 
    end
    % ====================

    % C. predict the training data and get the training error. 
    [yhatm] = dualclassify(X, lm, bm, X, y); 
    [yhats] = dualclassify(X, ls, bs, X, y); 
    mtrerr = sum(yhatm ~= y) / t; 
    strerr = sum(yhats ~= y) / t; 
    fprintf('Training error for L2 margin is: %.5f. \n', mtrerr); 
    fprintf('Training error for L2 soft margin is: %.5f. \n', strerr); 
    
    % D. generate new test data and compute the test error. 
    t = 1000; 
    [Xtest, ytest] = dataGen(t, n, p_pos, model); 
    [yhattestm] = dualclassify(Xtest, lm, bm, X, y); 
    [yhattests] = dualclassify(Xtest, ls, bs, X, y); 
    mteerr = sum(yhattestm ~= ytest) / t; 
    steerr = sum(yhattests ~= ytest) / t; 
    fprintf('Training error for L2 margin is: %.5f. \n', mteerr); 
    fprintf('Training error for L2 soft margin is: %.5f. \n', steerr); 
    
    % compute the average training and testing misclassification error over
    % 100 runs.
    mtrerr = 0.0; 
    strerr = 0.0; 
    mteerr = 0.0; 
    steerr = 0.0;
    count = 100; 
    for i = 1:count
        t = 100; 
        [X, y] = dataGen(t, n, p_pos, model); 
        [lm, bm] = dualL2marg(X,y); 
        [ls, bs] = dualsoftL2(X,y,1);
        % training error.
        mtrerr = mtrerr + sum(dualclassify(X, lm, bm, X, y) ~= y) / t; 
        strerr = strerr + sum(dualclassify(X, ls, bs, X, y) ~= y) / t; 
        
        % testing error. 
        [Xtest, ytest] = dataGen(t, n, p_pos, model); 
        [yhattestm] = dualclassify(Xtest, lm, bm, X, y); 
        [yhattests] = dualclassify(Xtest, ls, bs, X, y); 
        mteerr = mteerr + sum(yhattestm ~= ytest) / t; 
        steerr = steerr + sum(yhattests ~= ytest) / t; 
    end 
    % print average training and testing error for 100 runs. 
    fprintf('Training error for L2 margin is: %.5f. \n', mtrerr / count);
    fprintf('Training error for L2 soft margin is: %.5f. \n', strerr / count); 
    fprintf('Testing error for L2 margin is: %.5f. \n', mteerr / count); 
    fprintf('Testing error for L2 soft margin is: %.5f. \n', steerr / count); 
end 
