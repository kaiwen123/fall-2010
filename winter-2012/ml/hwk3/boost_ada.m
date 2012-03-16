
% combine at most 'ncomp' stumps

function [model] = boost_ada(X,y,ncomp)

W = ones(size(X,1),1); 
W = W/sum(W);
alpha = 1;

model = cell(ncomp,1);

H = zeros(size(X,1),1);
for i=1:ncomp,
  stump = build_stump(X,y,W);

  if (stump.werr<0.5),
    h = eval_stump(stump,X);
    
    % insert alpha calculation here
    alpha = log((1-stump.werr)/stump.werr)/2; 
    
    H = H + alpha*h; % update the combined predictions
    
    % weight update
    W=exp(-H.*y);
    W=W/sum(W);
    
    model{i} = stump;
    model{i}.alpha = alpha; 
  else  
    i = i-1; break; 
  end;

end;

model = model(1:i); % return only non-empty cells 

