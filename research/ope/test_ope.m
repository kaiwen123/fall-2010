% test ope

clearvars -global
X = load('~/rstree/spatialindex-1.3.2/regressiontest/rtree/testLD/orig/adult.k20.d9');
X = X(:, 4:2:end);
size(X)
A = load('~/rstree/spatialindex-1.3.2/regressiontest/rtree/testLD/trans2/A.k20.d9');

%x = rand(10000,1);
[n,d] = size(X);
tic;
Y=[];
for i=1:d
    x = X(:,i);
    bb = ope(x, 1, 100);
    y =ope_enc_array(bb,x, 1,0,4);
    Y = [Y y];
end
opet=toc

size(Y)
tic
Z = [Y'; ones(1, n); rand(1,n)];
R = A*Z;
raspt = toc
