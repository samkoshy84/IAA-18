proc optmodel;
/*Define Variables*/
	var x1>=0;
	var x2>=0;
/*Objective Function*/
	max profit = 102*x1+387*x2;
/*Constraints*/
	con assembly_time: 3*x1+5*x2<=240;
	con shipping_time: 2*x1+3*x2<=160;
	con chair_demand: x1<=360;
	con table_demand: x2<=80;
	con x1>=4*x2;
/*Solve*/
	solve objective profit;
/*Output*/
	print x1 x2 profit;
quit;
