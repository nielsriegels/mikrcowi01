$SETGLOBAL path  "c:\mikr\mikrcowi01\"
*option nlp=IPOPT;
*$SETGLOBAL path  ""
* =============================================================================
*
* BEAM - Basin Economic Allocation Model
* Copyright Mikkel Kromann, Niels David Riegels, COWI A/S and DHI Group A/S 2011-12
* Use and distribution of the model is allowed only by permission of a copyright holder
*
* =============================================================================

* Set file for renaming solution.gdx
file sRename /%path%solutions\renameS.cmd/;

* Status text file for web server interaction
file step /%path%output\step.txt/;
PUT step;
PUT "1";
PUTCLOSE;

* Scalar for optimisation of HEPS production in baseline
SCALAR basDisOpt "Optimise HEPS production in baseline" / 0 /;

* The debugging scalar is used for testing/debugging model water balance against water flow data
* Set this scalar to 1, and all water flow constraints should be satisfied by using
* base year flow data (losses are set to zero, all water supply and demand is in January)
* In normal model operation, baseline should be set to zero!
SCALAR debugging / 0 /;

* THe baseline scalar is used for forcing the model into choosing the same land allocation
* for crops as in the base year
SCALAR baseline / 0 /;

* Scaling of equations
* Data is in mm3, but for solver reasons, we might want to scale to Gm3 (e.g. scaleM3 = 1/1000)
SCALAR scaleM3 / 1 /;

* Reservoir buildup parameter for counterfactural scenario
SCALAR ctrfBuild / 0 /;

* Scalar for returning solver status
SCALAR solvStat / 0 /;
* TO DO

* =============================================================================
* Include base sets and scalars (file not written by input Excel sheet)
* =============================================================================
$include "%path%10base.inc";

* =============================================================================
* Include data driven set definitions
* =============================================================================
$offlisting
$include "%path%20sets.inc";
$onlisting
ALIAS(b,bb,bd,bo);
ALIAS(j,jj);
SET bResBuild(b)        / Res_NUR, Res_TOK /;

* =============================================================================
* Include data driven maps (i.e. intake, reservoir and river arcs)
* =============================================================================
*$offlisting
$include "%path%30maps.inc";
*$onlisting

SETS
    bRes(b)                 "Reservoir body"
    bRiv(b)                 "River body"
    bPlz(b)                 "Planning zone body"
    bSrc(b)                 "Source body"
    bResNOP                 "Reservoirs not yet built"
    bResSto(b)              "Reservoirs in storage operation mode"
    bResEly                 "Reservoirs in operation producing electricity"
    fixDisBL(b,y0,m0)       "Break intra year reservoir volume condition"
;

* =============================================================================
* Include data tables on water flows, agriculture and economics
* =============================================================================
*$offlisting
$include "%path%40data.inc";
*$onlisting

* TEMPORARY - replace with Excel entries
SET cot(j)  / cot /;
SET wht(j)  / wht /;
SET veg(j)  / tmt, shv, pot, sbt, mng /;
SET fru(j)  / stf, tgr, mln /;
SET ric(j)  / ric, ri2 /;
SET alf(j)  / alf /;
SET oth(j)  / mzf, mzg /;
SET bNat(b) / Lak_ARS, Lak_ARN, Lak_Ayd /;
SET caps3(c) / KAZ, UZB, KYR /
SET caps5(c) / KAZ, UZB, KYR, TAD, TUR /
SET caps6(c) / KAZ, UZB, KYR, TAD, TUR /
SET bResInd(b)  / Res_TOK, Res_AND, Res_NUR /

* =============================================================================
* Include defition of scenario
* =============================================================================
$include "%path%50scen.inc";

* Assign additional sets for reservoirs based on data and maps
bRiv(b)                 = YES$SUM(bd$intk(bd,b),    1);
bRes(b)                 = YES$SUM(bd$resv(bd,b),    1);
bPlz(b)                 = YES$SUM((q,j)$lnd0(b,q,j), 1);
bSrc(b)                 = YES$SUM((s,m,y0), sup0(s,b,y0,m));

* Reservoirs not in operation
* In baseline, all new reservoirs are not in operation
bResNOP(b)              = YES$resNew(b) ;
bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
bResSto(b)$bResNOP(b)   = NO;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));

* Calculate CET coefficient
SCALAR sT "Elasticity of transformation" / 0.5 /;
rL = (sT+1)/sT;

* =============================================================================
* Declare and assign model parameters based on data from Excel sheet
* =============================================================================
$include "%path%60parm.inc";

* =============================================================================
* Declare and assign model and equations and solve
* =============================================================================
$include "%path%70eqtn.inc";

* =============================================================================
* Set levels to help solver process faster
* =============================================================================
*$include "%path%80lvls.inc";

PARAMETER checkElybal(sSce,g,c,y0,*,m0) ;
PARAMETER elyBal(sSce,c,*);
PARAMETER natBal(sSce,b);
OPTIONS checkElyBal:1:4:1;

* =============================================================================
* Define base scenario and solve
* =============================================================================
$include "%path%90loop.inc";

* =============================================================================
* Save various result data files to disk
* =============================================================================
$include "%path%99save.inc";

PUT step;
PUT "4";
PUTCLOSE;

PARAMETER rcCrop(j,c), rcLand(j,c);
rcLand(j,c) = SUM((b,q,y)$bCty(b,c), round(LND.l(b,q,j,y),2));
rcCrop(j,c)$rcLand(j,c) = SUM((b,q,y)$bCty(b,c), ROUND(CRP.l(b,q,j,y),1));

PARAMETER elyPr(c,g,y0,m);
elyPr(c,g,y,m) = ELYMKT.m(c,g,y,m)*1000000;
OPTIONS elyPr:2:3:1;


*DISPLAY rPlz, rLos, rcCrop, rcLand, elyBal;
DISPLAY elyBal, TNI.l, cTPP, elyPr;
DISPLAY checkOutput;