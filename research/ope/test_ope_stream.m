% test ope

clearvars -global
X = load('~/rstree/spatialindex-1.3.2/regressiontest/rtree/testLD/orig/adult.k20.d9');
X = X(:, 4:2:end);
size(X)
A = load('~/rstree/spatialindex-1.3.2/regressiontest/rtree/testLD/trans2/A.k20.d9');

%x = rand(10000,1);
[n,d] = size(X);
Y=[];
bb ={};
for i=1:d
    x = X(:,i);
    bb{i} = ope(x, 1, 100);
end

opet=0;
raspt=0;
for j=1:n
    Y=[];
    tic;
    for i=1:d
        bb1 = bb{i};
        y =ope_enc(bb1,X(j,i), 1,0,4);
        Y = [Y y];
    end
    opet = opet+toc;
    tic;
    Z = [Y'; 1; rand];
    R = A*Z;
    raspt = raspt + toc;
end
opet
raspt
