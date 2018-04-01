proc optmodel;
/*Define Variables*/
	var x>=0 integer;
	var y>=0 integer;
/*Objective Function*/
	max profit = 60*x+50*y;
/*Constraints*/
	con Machine_A_Capacity: 2*x + 4*y <=80;
	con Machine_B_Capacity: 3*x + 2*y <=60;
	con Demand_X: x <=16;
	con Demand_Y: y <=18;
/*Solve*/
	solve objective profit;
/*Output*/
	print x y profit;
quit;
