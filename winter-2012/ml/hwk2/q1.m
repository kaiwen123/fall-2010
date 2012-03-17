% This is problem one of machine learning homework. 
wss = warning('off','all');
for model = 1:3
    t = 100; 
    n = 2; 
    u = ones(n, 1); 
    v = 0.5 * n; 
    p_pos = 0.5; 
    % A. generate training data. 
    [X, y] = dataGen(t, n, p_pos, model); 

    % build the training model.
    [wm, bm] = maxL2marg(X,y);
    [ws, bs] = softL2marg(X,y,1);

    % B. plot the model with genertive data. 
    clf; axis([0 1 0 1]); hold; axis('square');
    pos = find(y > 0); plot(X(pos,1),X(pos,2),'g+');
    neg = find(y < 0); plot(X(neg,1),X(neg,2),'rx');
    plot([0 1],[v v-u(1)]/u(2),'k:');
    plot([0 1],[bm bm-wm(1)]/wm(2),'b-');
    plot([0 1],[bs bs-ws(1)]/ws(2),'m-');
    if model == 1 
        print -deps experiment.2.1.1.ps
    elseif model == 2
        print -deps experiment.2.1.2.ps
    elseif model == 3
        print -deps experiment.2.1.3.ps 
    end

    % C. predict the training data and get the training error. 
    [yhatm] = classify(X, wm, bm); 
    [yhats] = classify(X, ws, bs); 
    mtrerr = sum(yhatm ~= y) / t; 
    strerr = sum(yhats ~= y) / t; 
    fprintf('Training error for L2 margin is: %.5f. \n', mtrerr); 
    fprintf('Training error for L2 soft margin is: %.5f. \n', strerr); 
    
    % D. generate new test data and compute the test error. 
    t = 1000; 
    [Xtest, ytest] = dataGen(t, n, p_pos, model); 
    [yhattestm] = classify(Xtest, wm, bm); 
    [yhattests] = classify(Xtest, ws, bs); 
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
        [wm, bm] = maxL2marg(X,y); 
        [ws, bs] = softL2marg(X,y,1);
        % training error.
        mtrerr = mtrerr + sum(classify(X, wm, bm) ~= y) / t; 
        strerr = strerr + sum(classify(X, ws, bs) ~= y) / t; 
        
        % testing error. 
        [Xtest, ytest] = dataGen(t, n, p_pos, model); 
        [yhattestm] = classify(Xtest, wm, bm); 
        [yhattests] = classify(Xtest, ws, bs); 
        mteerr = mteerr + sum(yhattestm ~= ytest) / t; 
        steerr = steerr + sum(yhattests ~= ytest) / t; 
    end 
    % print average training and testing error for 100 runs. 
    fprintf('Training error for L2 margin is: %.5f. \n', mtrerr / count);
    fprintf('Training error for L2 soft margin is: %.5f. \n', strerr / count); 
    fprintf('Testing error for L2 margin is: %.5f. \n', mteerr / count); 
    fprintf('Testing error for L2 soft margin is: %.5f. \n', steerr / count); 
end 
warning(wss);
