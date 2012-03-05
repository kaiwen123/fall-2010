./client730 localhost b1 localhost 00ff00 & 
./client730 localhost b2 localhost 00ff00 & 
sleep 1
./wbadmin730 -t localhost 0x20007161 b1 localhost 0x20007162 
sleep 1
./wbadmin730 -t localhost 0x20007161 b2 localhost 0x20007162 
sleep 1
./wbadmin730 -q localhost 0x20007161
sleep 1
./wbadmin730 -q localhost 0x20007162
