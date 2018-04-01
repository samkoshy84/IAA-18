/* Create Input Data */

data prices;
	length Emp_Type $10.;
	input Emp_Type $ Project Price;
	datalines;
		Partner 1 160
		Partner 2 120
		Partner 3 110
		Senior 1 120
		Senior 2 90
		Senior 3 70
		Junior 1 0
		Junior 2 50
		Junior 3 40
		;
run;

data availability;
	length Emp_Type $10.;
	input Emp_Type $ hrs;
	datalines;
		Partner 200
		Senior 400
		Junior 400
	;
run;

data projects;
	input Project Duration;
	datalines;
		1 500
		2 300
		3 100
		;
run;


proc optmodel;

	/* DEFINE SETS */

	set PROJECTS;
	set <str> EMPTYPE;
	
	
	/* DEFINE PARAMETERS */
	
	num price{EMPTYPE,PROJECTS};
	num maxtime{EMPTYPE};
	num duration{PROJECTS};
	
	/* READ DATA */
	
	read data projects 
		into PROJECTS=[project]
		duration;
		
	read data availability 
		into EMPTYPE=[emp_type]
		maxtime=hrs;
		
	read data prices 
		into [emp_type project] 
		price;
	
	/* DEFINE VARIABLES */
	
	var Penalty >= 0;
	var HrsAssign{EMPTYPE,PROJECTS} >=0 INTEGER;
	
	
	/* DEFINE OBJECTIVE FUNCTION */
	
	min ObjFunc=Penalty;
	
	
	/* DEFINE CONSTRAINTS */
	
	con Employee_Availability{e in EMPTYPE}:
		sum {p in PROJECTS} HrsAssign[e,p] <= maxtime[e];
	
	con Project_Duration{p in PROJECTS}:
		sum {e in EMPTYPE} HrsAssign[e,p] = duration[p];
	
	con At_Least_One_Partner{e in EMPTYPE}:
		sum {p in PROJECTS} HrsAssign['Partner',p] >=40;
	
	con At_Least_Three_Seniors{e in EMPTYPE}:
		sum {p in PROJECTS} HrsAssign['Senior',p] >=120;
		
	con No_Juniors_On_Project_One:
		HrsAssign['Junior',1]=0;
		
	con Penalty_Definition_1:
		Penalty >= 68000 - sum{e in EMPTYPE, p in PROJECTS} price[e,p]*HrsAssign[e,p];
	
	con Penalty_Definition_2:
		Penalty >= sum{e in EMPTYPE, p in PROJECTS} price[e,p]*HrsAssign[e,p]-68000;
	
	
	/* DEFINE VARIABLES */
	
	solve;
	
	/* PRINT SOLUTION */
	
	print HrsAssign.sol;
	
	print Penalty.sol;
	
quit;
