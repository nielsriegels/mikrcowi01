$SETGLOBAL path  "c:\mikr\beam\"
*option nlp=IPOPT;
*$SETGLOBAL path  ""
* =============================================================================
*
* BEAM - Basin Economic Allocation Model
* Copyright Mikkel Kromann, Niels David Riegels, COWI A/S and DHI Group A/S 2011-12
* Use and distribution of the model is allowed only by permission of a copyright holder
*
* =============================================================================

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
*$offlisting
$include "%path%20sets.inc";
*$onlisting
ALIAS(b,bb,bd,bo);
ALIAS(j,jj);
SET bResBuild(b)        / Res_NUR, Res_TOK /;

* =============================================================================
* Include data driven maps (i.e. intake, reservoir and river arcs)
* =============================================================================
$offlisting
$include "%path%30maps.inc";
$onlisting

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
$offlisting
$onlisting
$include "%path%40data.inc";

* =============================================================================
* Include defition of scenario
* =============================================================================
$include "%path%50scen.inc";

PARAMETER scenario(*,*) "Scenario defintion parameter";
scenario("RunOfRiver","ctrf") = 0;

y(y0)$modelYear(y0)     = YES;
bRiv(b)                 = YES$SUM(bd$intk(bd,b), 1);
bRes(b)                 = YES$SUM(bd$resv(bd,b), 1);
bPlz(b)                 = YES$SUM(j, qAWater(b,j));
bSrc(b)                 = YES$SUM((s,m,y), sup0(s,b,y,m));

* Reservoirs not in operation
* In baseline, all new reservoirs are not in operation
bResNOP(b)              = YES$resNew(b) ;
bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
bResSto(b)$bResNOP(b)   = NO;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));

* Calculate CET coefficient
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
$include "%path%80lvls.inc";


* =============================================================================
* Define base scenario and solve
* =============================================================================

SET hScen / base, OptWoRogun, OptWiRogun, wgtHarm, fixHarm /
PARAMETER amuUpsFlow(hScen,m0) "Test parameter for Amudarya upstream flow to midstream (mm3/month)";
PARAMETER cottonIRG(hScen,c)     "irrigation water use for cotton (mm3/month)";
PARAMETER agriVA(hScen,c)       "Agri value added (mUSD/year)";
PARAMETER seaSto(hScen,m)           "Storage in Aral Sea (mm3/month)";

* We make wheat the only sluggish crop in baseline, thus it will be fixed
jAF(j)      = 0;
jAF("wht")  = 1;
jAX(j) = 1$(not jAF(j));
IF(not basDisOpt,
    rFixVol(b,m)                = SUM(y0$baseyear(y0), resVolFix0(b,y0,m));
    rFixDis(bd,bo,m)            = SUM(y0$baseyear(y0), resDisFix0(bd,bo,y0,m));
    sup("W","Src_ZAR","m06")    = sup("W","Src_ZAR","m06") + 2000*0;
);

PUT step;
PUT "2";
PUTCLOSE;

*beam.solprint       = yes;
beam.limrow         = 0;
beam.limcol         = 0;
*beam.iterlim        = 0;
SOLVE beam MAXIMIZING twv USING NLP;
*abort "Test baseline only";
modlStatB = beam.modelstat;
solvStatB = beam.solvestat;

DISPLAY "Base scenario", HPP.l;
amuUpsFlow("base",m) = SUM(y, FLW.l("w","AMUMID","AMUUPS",y,m));
agriVA("base",c) = SUM((b,y)$bCty(b,c), 
   SUM(j$oCosts(b,j),     iOUTPUT.l(b,j,y)*qAQuant(b,j)*pCrop(j) )  / 1000000 
  -SUM((j,k)$oCosts(b,j), iINPUT.l(b,j,k,y)*pInput(k)*iACosts(b,k,j)$(not kLimit(k)) )  );
cottonIRG("base",c) = SUM((b,y,m)$bCty(b,c), IRG.l(b,"cot",y,m));
seaSto("base",m) = SUM(y, STO.l("w","Lak_ARN",y,m)*0+STO.l("w","Lak_ARS",y,m));

* =============================================================================
* Generate output parameter for base scenario
* =============================================================================
$include "%path%91out1.inc";

PUT step;
PUT "3";
PUTCLOSE;

* =============================================================================
* Define counterfactual scenario and solve
* =============================================================================

* Allow flexibility in land allocation
jAF(j)                  = jAF0(j);
jAF("fal")              = YES$fallowLand;
jAF("fal")              = YES;
jAX(j)                  = 1$(not jAF(j));
* Set water supply (choose between dry and normal)
sup(s,b,m)              = SUM(y0$baseyear(y0), sup0(s,b,y0,m));
* Set minimum inflows according to baseyear
minInflow0(b,m0)        = minInflowN(b,m0)$baseyear("2009") + minInflowD(b,m0)$baseyear("2001");
* Set new reservoirs in operation active in counterfactual scenario
bResNOP(b)              = YES$(resNew(b) AND not resNewOp(b));
bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
bResSto(b)$bResNOP(b)   = NO;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
HPP.up(b,g,y,m)$bResEly(b) = INF;
* Require extra nature needs for counterfactual
natExtra(b)             = natExtra0(b);
* Set counterfactual crop prices
pCrop(j)                = pCrop0(j,"ctrf");
* change input prices
pInput(k)               = pInput0(k);
* Reset fixed discharge patterns from baseline
resVolFix0(b,y,m)       = 0;
resDisFix0(bd,bo,y,m)   = 0;
* Set irrigation investment costs
iInv(sInv)              = iInv0(sInv);
* Set reservoir buildup
ctrfBuild = ctrfBuild0;
* Force run-off river for all reservoirs
*bResSto(b)$scenario("RunOfRiver","ctrf") = NO;
*bResBuild(b)$scenario("RunOfRiver","ctrf") = NO;
*irg0(b,"wht",y,m) = irg0(b,"wht",y,m)/20;
*DISPLAY minInflow0, sup0, sup, baseyear;
beam.solprint = no;
beam.limrow = 0;
SOLVE beam MAXIMIZING twv USING NLP;
DISPLAY bResEly;
amuUpsFlow("OptWoRogun",m) = SUM(y, FLW.l("w","AMUMID","AMUUPS",y,m));
agriVA("OptWoRogun",c) = SUM((b,y)$bCty(b,c), 
   SUM(j$oCosts(b,j),     iOUTPUT.l(b,j,y)*qAQuant(b,j)*pCrop(j) )  / 1000000 
  -SUM((j,k)$oCosts(b,j), iINPUT.l(b,j,k,y)*pInput(k)*iACosts(b,k,j)$(not kLimit(k)) )  );
cottonIRG("OptWoRogun",c) = SUM((b,y,m)$bCty(b,c), IRG.l(b,"cot",y,m));
seaSto("OptWoRogun",m) = SUM(y, STO.l("w","Lak_ARN",y,m)*0+STO.l("w","Lak_ARS",y,m));
modlStatS = beam.modelstat;
solvStatS = beam.solvestat;

* Solve with Rogun and optimal allocation
resNewOp("Res_ROG")     = YES;
bres("Res_ROG")         = YES;
bResNOP("Res_ROG")      = NO;
bResSto("Res_ROG")      = YES;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
HPP.up(b,g,y,m)$bResEly(b) = INF;
STO.up(s,b,y,m)$bRes(b)    = 99999;
SOLVE beam MAXIMIZING twv USING NLP;
DISPLAY bResEly;
amuUpsFlow("OptWiRogun",m) = SUM(y, FLW.l("w","AMUMID","AMUUPS",y,m));
agriVA("OptWiRogun",c) = SUM((b,y)$bCty(b,c), 
   SUM(j$oCosts(b,j),     iOUTPUT.l(b,j,y)*qAQuant(b,j)*pCrop(j) )  / 1000000 
  -SUM((j,k)$oCosts(b,j), iINPUT.l(b,j,k,y)*pInput(k)*iACosts(b,k,j)$(not kLimit(k)) )  );
cottonIRG("OptWiRogun",c) = SUM((b,y,m)$bCty(b,c), IRG.l(b,"cot",y,m));
seaSto("OptWiRogun",m) = SUM(y, STO.l("w","Lak_ARN",y,m)*0+STO.l("w","Lak_ARS",y,m));

* Solve with harms allocation (weights for discharges and flows)
objAGRweight(b)   = 0.001;
objELYweight      = 0.001;
objDISweight("AMUUPS",m) = 0.001$mSum(m) + 1000$mWin(m);
SOLVE beam MAXIMIZING twv USING NLP;
DISPLAY bResEly;
amuUpsFlow("wgtHarm",m) = SUM(y, FLW.l("w","AMUMID","AMUUPS",y,m));
agriVA("wgtHarm",c) = SUM((b,y)$bCty(b,c), 
   SUM(j$oCosts(b,j),     iOUTPUT.l(b,j,y)*qAQuant(b,j)*pCrop(j) )  / 1000000 
  -SUM((j,k)$oCosts(b,j), iINPUT.l(b,j,k,y)*pInput(k)*iACosts(b,k,j)$(not kLimit(k)) )  );
cottonIRG("wgtharm",c) = SUM((b,y,m)$bCty(b,c), IRG.l(b,"cot",y,m));
seaSto("wgtharm",m) = SUM(y, STO.l("w","Lak_ARN",y,m)*0+STO.l("w","Lak_ARS",y,m));

* Solve optimising agriculture using fixed flows from Harms scenario
resDisFix0(bd,"Res_ROG",y,m)   = DIS.l("w",bd,"Res_ROG",y,m);
resDisFix0(bd,"Res_NUR",y,m)   = DIS.l("w",bd,"Res_NUR",y,m);
objAGRweight(b)   = 1;
objELYweight      = 1;
objDISweight("AMUUPS",m) = 0;
SOLVE beam MAXIMIZING twv USING NLP;
amuUpsFlow("fixHarm",m) = SUM(y, FLW.l("w","AMUMID","AMUUPS",y,m));
agriVA("fixHarm",c) = SUM((b,y)$bCty(b,c), 
   SUM(j$oCosts(b,j),     iOUTPUT.l(b,j,y)*qAQuant(b,j)*pCrop(j) )  / 1000000 
  -SUM((j,k)$oCosts(b,j), iINPUT.l(b,j,k,y)*pInput(k)*iACosts(b,k,j)$(not kLimit(k)) )  );
cottonIRG("fixharm",c) = SUM((b,y,m)$bCty(b,c), IRG.l(b,"cot",y,m));
seaSto("fixharm",m) = SUM(y, STO.l("w","Lak_ARN",y,m)*0+STO.l("w","Lak_ARS",y,m));

amuUpsFlow(hScen,"nvg") = SUM(m$mWin(m), amuUpsFlow(hScen,m));
amuUpsFlow(hScen,"veg") = SUM(m$mSum(m), amuUpsFlow(hScen,m));
amuUpsFlow(hScen,"annual") = SUM(m, amuUpsFlow(hScen,m));

DISPLAY amuUpsFlow, agriVA, cottonIRG, seaSto, HPP.l, DIS.l, VOL.l;
DISPLAY bRes, bResEly, bResSto, bResEly, bResNOP, resNewOP;

* =============================================================================
* Generate output parameter for action scenario
* =============================================================================
$include "%path%92out2.inc";

*$ontext
IF(beam.solvestat eq 1,
*    DISPLAY rAgriBase, rFlowBase, rEconBase, rAgriCtrf, rFlowCtrf, rEconCtrf;
*    execute_unload "beamOutput.gdx" rAgriBase, rFlowBase, rEconBase, rAgriCtrf, rFlowCtrf, rEconCtrf;
*    execute "gdxxrw.exe I=beamOutput.gdx O=beamOutput.xls index=index!d10";
*    execute "pause";
);
*$offtexts

* =============================================================================
* Include loops for writing csv files to disk
* =============================================================================
$include "%path%93out3.inc";

PUT step;
PUT "4";
PUTCLOSE;
