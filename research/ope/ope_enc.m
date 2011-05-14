
function y = ope_enc(bb, x, dist, p1, p2)
    y = 0;

    if dist ==1
        l=4.0/length(bb); % 4/100
    else
        return;
    end
    
    j=1;
    
    lowerb = bb(2)-bb(1);
    upperb = bb(end)-bb(end-1);
    
    if (bb(2)> x)
        y = (-4 + (x-bb(1))/lowerb)*p2+p1;
    else
        if x >bb(end-1)
            y = (4 + (x-bb(end-1))/upperb)*p2+p1;
        else
            j = bfind(bb,x);
            % normal distribution, p1=0,. p2=1
            y=(-2+j*l+ (x-bb(j))*l/(bb(j+1)-bb(j)+0.00001))*p2+p1;            
        end
    end


function idx = bfind(bb, x) 
% binary search    
    left =1;
    right = length(bb);
    while right>left
        mid = floor((right+left)/2);
        if bb(mid)<=x & bb(mid+1)>x
            idx = mid;
            return;
        end
        if bb(mid)>x
            right = mid-1;
        else
            left =mid+1;
        end
    end
    idx = mid;
