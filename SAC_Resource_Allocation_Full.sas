libname teach "C:\Users\navikt\OneDrive\1 - Teaching\Classes\NCSU\AA503\AA503 - Spring 2018\Data";

/***********************************/
/************Alternative 1**********/
/***********************************/

/* Map resource preference by project instead of by industry */
/* Create a dummy skill parameter = 1 if resource i skilled for project j */
proc sql;
	create table teach.sac_pref_skill_map as
	select 
		a.resourceID
		,b.projectCODE
		,b.expBillHrs
		,a.pref
		,case
			when c.skillGroup=b.Skillgroup then 1
			else 0
			end as skill
	from 
		teach.sac_preference_master as a
		,teach.sac_project_master as b
		,teach.sac_resource_master as c
	where 
		a.industry=b.industry
		and a.resourceID=c.resourceID;
quit;

/* Create a dummy senior parameter = 1 if resource i is a senior */
proc sql;
	create table teach.sac_senior_map as
	select
		a.resourceID
		,case
			when a.seniority='senior' then 1
			else 0
			end as senior
	from 	
		teach.sac_resource_master as a;
quit;


proc optmodel;

	/* DEFINE SETS */
	set  PROJECTS;
	set  RESOURCES;
	set  SENIORS;

	/* DEFINE PARAMETERS */
	num pref{RESOURCES,PROJECTS};
	num skill{RESOURCES,PROJECTS};
	num senior{RESOURCES};

	/* READ DATA */
	read data teach.sac_project_master(where=(expBillHrs<=35*12)) 
		into PROJECTS=[projectCode];

	read data teach.sac_senior_map 
		into RESOURCES=[resourceID]
		senior;

	read data teach.sac_pref_skill_map(where=(expBillHrs<=35*12)) 
		into [resourceID projectCode] 
		pref
		skill;

	/* DEFINE DECISION VARS */
	var Assign{RESOURCES,PROJECTS} BINARY;

	/* DEFINE OBJECTIVE FUNCTION */
	max Total_Preference = 
		sum {r in RESOURCES, p in PROJECTS}	pref[r,p]*Assign[r,p];

	/* DEFINE CONSTRAINTS */
	con Project_Skill_Needs{p in PROJECTS}: 
		sum{r in RESOURCES} skill[r,p]*Assign[r,p]=1;

	con Must_Assign_Seniors{r in RESOURCES: senior[r]=1}:
		sum{p in PROJECTS} Assign[r,p] = 1;

	con Each_Project_Assigned_Once{p in PROJECTS}:
		sum{r in RESOURCES} Assign[r,p] = 1;

	con Each_Resource_Assigned_Max_Once{r in RESOURCES}:
		sum{p in PROJECTS} Assign[r,p] <= 1;


	/* SOLVE */
	solve;

	/* CREATE OUTPUT */
	create data teach.sac_solution 
		from [RESOURCES PROJECTS] 
		Assign;

quit;

data teach.sac_summary_solution (drop=assign);
	set teach.sac_solution;
	if assign=1;
run;

/***********************************/
/************Alternative 2**********/
/***********************************/

proc sql;
	create table teach.sac_pref_map as
	select 
		a.resourceID
		,b.projectCODE
		,b.expBillHrs
		,a.pref
	from 
		teach.sac_preference_master as a
		,teach.sac_project_master as b
	where 
		a.industry=b.industry;
quit;

proc optmodel;

	/* DEFINE SETS */
	set  ALL_PROJECTS;
	set  TECH_PROJECTS;
	set  AN_PROJECTS;
	set  ALL_RESOURCES;
	set  TECH_RESOURCES;
	set  AN_RESOURCES;
	set  SENIORS;

	/* DEFINE PARAMETERS */
	num pref{ALL_RESOURCES,ALL_PROJECTS};

	/* READ DATA */
	read data teach.sac_project_master(where=(expBillHrs<=35*12)) 
		into ALL_PROJECTS=[projectCode];

	read data teach.sac_project_master(where=(expBillHrs<=35*12 and skillGroup='Technical')) 
		into TECH_PROJECTS=[projectCode];

	read data teach.sac_project_master(where=(expBillHrs<=35*12 and skillGroup='Analytic')) 
		into AN_PROJECTS=[projectCode];

	read data teach.sac_resource_master 
		into ALL_RESOURCES=[resourceID];

	read data teach.sac_resource_master(where=(skillGroup='Technical')) 
		into TECH_RESOURCES=[resourceID];

	read data teach.sac_resource_master(where=(skillGroup='Analytic')) 
		into AN_RESOURCES=[resourceID];

	read data teach.sac_resource_master (where=(seniority='senior')) 
		into SENIORS=[resourceID];

	read data teach.sac_pref_map (where=(expBillHrs<=35*12)) 
		into [resourceID projectCode] 
		pref;

	put TECH_RESOURCES=;
	put TECH_PROJECTS=;
	put AN_RESOURCES=;
	put AN_PROJECTS=;


	/* DEFINE DECISION VARS */
	var Assign{ALL_RESOURCES,ALL_PROJECTS} BINARY;


	/* DEFINE OBJECTIVE FUNCTION */
	max Total_Preference = 
		sum {r in ALL_RESOURCES, p in ALL_PROJECTS}	pref[r,p]*Assign[r,p];

	/* DEFINE CONSTRAINTS */

	con Each_Tech_Project_Assigned_Once{p in TECH_PROJECTS}:
		sum{r in TECH_RESOURCES} Assign[r,p] = 1;

	con Each_An_Project_Assigned_Once{p in AN_PROJECTS}:
		sum{r in AN_RESOURCES} Assign[r,p] = 1;

	con Mismatched_Tech_Projects:
		sum{r in AN_RESOURCES, p in TECH_PROJECTS} Assign[r,p] = 0;

	con Mismatched_An_Projects:
		sum{r in TECH_RESOURCES, p in AN_PROJECTS} Assign[r,p] = 0;

	con Each_Resource_Assigned_Max_Once{r in ALL_RESOURCES}:
		sum{p in ALL_PROJECTS} Assign[r,p] <= 1;

	con Must_Assign_Seniors{r in SENIORS}:
		sum{p in ALL_PROJECTS} Assign[r,p]=1;

	/* SOLVE */
	solve;

	/* CREATE OUTPUT */
	create data teach.sac_solution 
		from [ALL_RESOURCES ALL_PROJECTS] 
		Assign;

quit;

data teach.sac_summary_solution (drop=assign);
	set teach.sac_solution;
	if assign=1;
run;


