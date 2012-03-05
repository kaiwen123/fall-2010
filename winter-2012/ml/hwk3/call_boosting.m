load cancer.mat; 

num_iter=50;
err=zeros(num_iter,1);

model=boost_ada(data.xtrain(:,1:2),data.ytrain,num_iter);

for k=1:num_iter
  y_est=sign(eval_boost(model(1:k),data.xtest(:,1:2)));
  err(k)=sum(y_est ~= data.ytest);
  
end

plot(err,'o-');
ylabel('Number test examples misclassified');
xlabel('Number of Boosting Iterations');

