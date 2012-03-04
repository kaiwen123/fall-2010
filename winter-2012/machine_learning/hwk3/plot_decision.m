function plot_decision(model,xtest,ytest);
%Plots decision regions for provided AdaBoost model
  
stepsize=0.02;
inputaxis=[min(xtest(:,1)) max(xtest(:,1)) min(xtest(:,2)) max(xtest(:,2))];

[x,y]=meshgrid(inputaxis(1):stepsize:inputaxis(2),inputaxis(3):stepsize: ...
               inputaxis(4));

plot_points=[x(:),y(:)];
y_est=sign(eval_boost(model,plot_points));

%plot decision regions using 2 different shades of grey
figure;
sp=pcolor(x,y,reshape(y_est,size(x,1),size(x,2)));
shading interp;
colormap([0.55,0.55,0.55;0.75,0.75,0.75]);
hold on;
xlabel('feature 1');
ylabel('feature 2');
title('AdaBoost decision regions');

%find test points corresponding to the 2 classes
indices{1}=find(ytest==1);
indices{2}=find(ytest==-1);

%plot test points
plot(xtest(indices{1},1),xtest(indices{1},2),'bx');
plot(xtest(indices{2},1),xtest(indices{2},2),'ro');
