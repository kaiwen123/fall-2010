function [bb2]=ope(x, dis, b )
% p1,p2: parameters for distribution
%b = 1000; % use 1000 buckets
if dis==1
    % to normal distribution
    %mu = p1;
    %sigma = p2;
    n = length(x);
    y = randn(n,1);
    %cover range +/-2sigma
    l = 4.0/b;
    
    bb = zeros(b+2,1);
    for i=1:n
        if y(i)<-2
            bb(1)=bb(1)+1;
        else if y(i) > 2
                bb(b+2) = bb(b+2)+1; 
            else 
                j = floor((y(i)+2)/l)+2;
                bb(j)=bb(j)+1;
            end
        end
    end
    bb2 = zeros(b+3,1);
    bb2(1) = min(x);
    x = sort(x);
    for i=1:b+2
        bb2(i+1) = x(sum(bb(1:i)));
    end
        
end
