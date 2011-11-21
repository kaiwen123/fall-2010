h = 20; 
g = 19; 
u(1) = 10; 
p(1) = 10; 

i = 1:50; 
for x = i,
  a = (p(x) * h / u(x) ) / (1 + p(x) / u(x)); 
b = h / (1 + p(x) / u(x));
c = 2 * u(x) * g / (1 - p(x) - u(x)); 
d = (1 - p(x) - 3 * u(x)); 

u(x+1) = (b + c) / (3 * (a + b + c -d));
p(x+1) = a * (a - d) / ((a + d) * (a + b + c - d));
u, p
endfor; 
