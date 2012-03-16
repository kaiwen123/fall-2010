load cancer.mat; 

num_iter=5000;
err=zeros(num_iter,1);

model=boost_ada(data.xtrain(:,1:2),data.ytrain,num_iter);

hold on;
for k=1:num_iter
  y_est=sign(eval_boost(model(1:k),data.xtest(:,1:2)));
  err(k)=sum(y_est ~= data.ytest) / size(y_est, 1);
  plot(k, err(k), 'o-');
end
plot(err,'o-');
ylabel('Number test examples misclassified');
xlabel('Number of Boosting Iterations');

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% minimum training margin.% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%
minmargin = zeros(num_iter);
meanmargin = zeros(num_iter); 
for k=1:num_iter
    [hhtrain,summtrain]=eval_boost(model(1:k),data.xtrain);
    minmargin(k)=(min(hhtrain.*data.ytrain))/summtrain;
    meanmargin(k)=(mean(hhtrain.*data.ytrain))/summtrain;
end

subplot(2,1,1);
plot(minmargin);
xlabel('Iterations');
ylabel('Mean Voting Margin');
title('Minimum Voting Margin vs. Iterations');

subplot(2,1,2);
plot(meanmargin);
xlabel('Iterations');
ylabel('Mean Voting Margin');
title('Mean Voting margin vs. Iterations');

%%%%%%%%%%%%%%%%%%%%%%%%
% plot decision region.%
%%%%%%%%%%%%%%%%%%%%%%%%
plot_decision(model(1:50), data.xtrain, data.ytrain); 
plot_decision(model(1:500), data.xtrain, data.ytrain); 
plot_decision(model(1:5000), data.xtrain, data.ytrain); 

plot_decision(model(1:50), data.xtest, data.ytest); 
plot_decision(model(1:500), data.xtest, data.ytest); 
plot_decision(model(1:5000), data.xtest, data.ytest); 

%%%%%%%%%%%%%
% problem 4.
%%%%%%%%%%%%%
% No, the underlining mechanism is different, boosting will iteratively
% update the weight of the weak learners, while in the linear optimization
% problem, everything is updated altogether. 
