#!/usr/bin/awk 
BEGIN {
alpha = 0.0;
collision = 0;
count = 0;
}
{
if(count < 1000) {
alpha += $1 
collision += $2
count++
} else {
print alpha / count, collision / count
count = 0
alpha = 0.0
collision = 0
}
    
}
END {
}
