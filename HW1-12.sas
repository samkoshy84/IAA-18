proc optmodel;
/*Define Variables*/
	var x11>=0 integer;
	var x12>=0 integer;
	var x13>=0 integer;
	var x21>=0 integer;
	var x22>=0 integer;
	var x23>=0 integer;    
/*Objective Function*/
	min Cost= 50*x11 + 40*x12 + 100*x13 + 75*x21 + 50*x22 + 75*x23;
/*Constraints*/
	/*Capacity_Constraint*/
	con OhioValleyPlant: x11 + x12 + x13>= 5000;
	con LakeviewPlant: x21 + x22 + x23>= 7000;
	/*Demand constraints*/
	con GrandRapids: x11 + x21 >= 3000;
	con BlueRidge: x12 + x22>=5000;
	con Sunset: x13 + x23>=4000;
/*Solve*/
	solve objective Cost;
/*Output*/
	print x11 x12 x13 x21 x22 x23 Cost;
quit;
