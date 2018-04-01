/* Model Building & Scoring for Prediction */
/*----------------------------------------------------------------------------------*/
*ods html close;
*ods rtf file="Model Building & Scoring for Prediction SAS Report.rtf";
*ods rtf;
*options nodate nonumber ls=95 ps=80;

/*******************/
/* Variable MACROS */
/*******************/
/* Defining Categorical Variables */
%let categorical=House_Style2 Overall_Qual2 Overall_Cond2 Fireplaces 
         Season_Sold Garage_Type_2 Foundation_2 Heating_QC 
         Masonry_Veneer Lot_Shape_2 Central_Air;

/* Defining Interval Variables */
%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
         Lot_Area Age_Sold Bedroom_AbvGr;

/*******************************/
/*  Splitting Data             */
/*******************************/


data ameshousing3_train ameshousing3_valid;
set bootcamp.ameshousing3;
random=RAND("Uniform");
if random <= 0.2 then output ameshousing3_valid;
 else output ameshousing3_train;
 run;


 proc surveyselect data=bootcamp.ameshousing3 
  method=srs rate=0.2 out=ameshousing3_split outall;
  run;

  data ameshousing3_train ameshousing3_valid;
   set ameshousing3_split;
   if Selected=1 then output ameshousing3_valid;
   else output ameshousing3_train;
   run;
/*********************/
/* Model Storing     */
/*********************/

/* Creation of Models*/
ods graphics;
proc glmselect data=bootcamp.ameshousing3 plots=all
         valdata=bootcamp.ameshousing4;
class &categorical;
model SalePrice = &categorical &interval/selection=backward
  select=sbc
  choose=validate;
store out=amesstore;
title 'Selecting the best model using honest assessment';
run;
/********************************/
/*  Scoring                     */
/********************************/
proc plm restore=amesstore;
  score data=bootcamp.ameshousing4 out=scored;
  code file="C:\Users\Susan\Google Drive\Summer II - statistics Bootcamp\scoring.sas";
run;

data scored2;
 set bootcamp.ameshousing4;
 %include "C:\Users\Susan\Google Drive\Summer II - statistics Bootcamp\scoring.sas";
run;

proc compare base=scored compare=scored2 criterion=0.0001;
var predicted;
with p_saleprice;
run;
/************************/
/* Evaluation           */
/************************/

data MAPE;
	set Scored;
	AE = abs(Predicted - SalePrice);
	APE = (abs(Predicted - SalePrice) / SalePrice)*100;
run;

proc means data=MAPE mean;
	var AE APE;
run;


*ods rtf close;
*ods html;
