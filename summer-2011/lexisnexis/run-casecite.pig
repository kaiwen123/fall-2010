A = load 'input2000';
D = LIMIT A 1000;
OUT = stream D through `casecitation.pl`;
dump OUT;
