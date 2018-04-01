proc optmodel;
/*Define Variables*/
	var x11 >=0 integer;
	var x12 >=0 integer;
	var x21 >=0 integer;
	var x22 >=0 integer;
	var y11 >=0 integer;
	var y12 >=0 integer;
	var y21 >=0 integer;
	var y22 >=0 integer;
/*Objective Function*/
	max revenue = 60*(x11+x12) +15*(x21+x22) + 70*(y11+y12) + 35*(y21+y22);
/*Constraints*/
	/*Capacity_Constraint*/
	con Month1_Machine1: 5*x11+8*y11<=600;
	con Month2_Machine1: 5*x21+8*y21<=600;
	con Month1_Machine2: 4*x12+5*y12<=600;
	con Month2_Machine2: 4*x22+5*y22<=600;
	/*Demand Constraint*/
	con Product1_Month1: x12+x11<=120;
	con Product1_Month2: x22+x21<=200;
	con Product2_Month1: y12+y11<=150;
	con Product2_Month2: y22+y21<=130;
/*Solve*/
	solve objective revenue;
/*Output*/
	print x11 x12 x21 x22 y11 y12 y21 y22 revenue;
quit;
