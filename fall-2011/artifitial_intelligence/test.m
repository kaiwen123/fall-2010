u = 0; 
h = 20; 
b = 0;
c = 9; 
d = 10; 
i = 1:50; 

for var = i, 
  b = u*h/(1/2+u); 
  u = (b + c)/(6*(b + c + d)); 
  b
  u 
endfor; 
figure; 
hold on; 
%plot(0:50, b); 
%plot(0:50, u) 
