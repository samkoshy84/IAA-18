libname class "C:\Users\Sam Koshy\Desktop\MSA Spring\Optimization\Data";

proc optmodel;
/* Define Sets */
	set <num> PRODUCTS;
	set <num> DEPARTMENTS;
	set <str> DEPNAME={'SHIPPING', 'ASSEMBLY'};
/* Define Parameters */
	num price{PRODUCTS};
	num demand{PRODUCTS};
	num costperhr{DEPARTMENTS};
	num maxtime{DEPARTMENTS};
	num hrs{PRODUCTS,DEPARTMENTS};
/* Read Data */
	read data class.Dsf_product_data into PRODUCTS=[product_id] price demand; /* use equal sign if names used in code is different from name in table*/
	read data class.Dsf_department_data into DEPARTMENTS=[department_id] costperhr=cost maxtime;
	read data class.Dsf_product_department_data into [product_id department_id] hrs=time;
/*Define Decision Variables */
	var ProdQty{PRODUCTS}>=0;
/*Define Implicit Variables */
	impvar ProdHrsPerDept{p in PRODUCTS, d in DEPARTMENTS}=ProdQty[p]*hrs[p,d];
	impvar ProdCost{p in PRODUCTS}=sum{d in DEPARTMENTS} ProdHrsPerDept[p,d]*costperhr[d];
	impvar ProdRev{p in PRODUCTS}=price[p]*ProdQty[p];
/* Define Objective Function */
	max Profit=sum{p in PRODUCTS}(ProdRev[p]-ProdCost[p]);
/* Define Constraints */
	con Dept_Maxtime{d in DEPARTMENTS}: sum{p in PRODUCTS} ProdHrsPerDept[p,d]<=maxtime[d];
	con Product_Demand{p in PRODUCTS}: ProdQty[p]<=demand[p];
/* Solve */
	solve objective Profit;
/* Create Data of Optimized Input */
	create data class.dsf_full_solution from [PRODUCTS] ProdQty;
	data class.dsf_solution;
	set class.dsf_full_solution;
	where ProdQty>0;
	run;
quit;
