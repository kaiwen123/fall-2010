
function y = ope_enc_array(bb, x, dist, p1, p2)
    y = zeros(length(x),1);

    if dist ==1
        l=4.0/length(bb); % 4/100
    else
        return;
    end
    
    [x1,idx] = sort(x);
    j=1;
    
    lowerb = bb(2)-bb(1);
    upperb = bb(end)-bb(end-1);
    
    for i=1:length(x)
        if (bb(2)> x1(i))
            y(idx(i)) = (-4 + (x1(i)-bb(1))/lowerb)*p2+p1;
        else
            if x1(i) >bb(end-1)
                y(idx(i)) = (4 + (x1(i)-bb(end-1))/upperb)*p2+p1;
            else
                while ~(bb(j)<=x1(i) & bb(j+1)>x1(i)) & j<length(bb)-1
                    j = j+1;
                end
                % normal distribution, p1=0,. p2=1
                y(idx(i))=(-2+j*l+ (x1(i)-bb(j))*l/(bb(j+1)-bb(j)+0.00001))*p2+p1;            
            end
        end
    end

