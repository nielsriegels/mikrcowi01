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

* Include base sets and scalars (file not written by input Excel sheet)
$include "%path%base.inc";

* Include data driven set definitions
$offlisting
$include "%path%sets.inc";
$onlisting
ALIAS(b,bb,bd,bo);
ALIAS(j,jj);
SET bResBuild(b)        / Res_NUR, Res_TOK /;

* Include data driven maps (i.e. intake, reservoir and river arcs)
$offlisting
$include "%path%maps.inc";
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

* Data tables on water flows, agriculture and economics
$offlisting
$include "%path%data.inc";
$onlisting

* Include defition of scenario
$include "%path%scen.inc";

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

PARAMETERS
    sup(s,b,m)              "Base year supply of river water (mm3/month)"
    qAQuant(b,j)            "Base year production agri/industrt quantities (ton/year)"
    iACosts(b,k,j)          "Base year production agri/industrt input shares quantities (mn usd/year)"
    qValue(b,j)             "Base year production agri/industrt prod. values (mn usd/year)"
    qALand(b,j)             "Base year land use in agriculture (ha)"
    qAWater(b,j)            "Base year water in agriculture (mm3/y)"
    qmWater(b,m)            "Base year water (mm3/month)"
    qInputs(b,k,j)          "Base year physical input use (unit/year)"
    oCosts(b,j)             "Base year production agri/industrt unit cost (usd/ton)"
    iShare(b,k,j)           "Base year share of input costs (pct of total cost)"
    lShare(b,j)             "Share of land for free crops in zone (%)"
    retShare0(bd,bo,*)      "Share of return water (%)"
    jAF(j)                  "Crops with flexible land allocation 0/1"
    jAX(j)                  "Crops with fixed land allocation 0/1"
    gwCorr(b,m)             "Ground water correction (mm3/month)"
    loss(b,m)               "Water loss from river (mm3/month)"
    pEly(y0,m)              "Electricity price (USD/MWh)"
    pCrop(j)                "Crop prices (USD/ton)"
    natExtra(b)             "Extra nature demands (mm3/year)"
    pInput(k)               "Index for input prices"
    rFixVol(b,m)            "Fixed volume for reservoirs (mm3)"
    rFixDis(bd,bo,m)        "Fixed discharges for reservoirs (mm3/month)"
    iInv(sInv)              "Irrigation investment data"
    minInflow0(b,m0)        "Minimum inflow for nature (mm3/month)"
    maxDischrg(b,m0)        "Maximum reservoir discharge (mm3/month)"
;

jAF(j)                          = jAF0(j);
qValue(b,j)                     = SUM(k, iACosts(b,k,j));
oCosts(b,j)$qAQuant(b,j)        = 1000000 * qValue(b,j) / qAQuant(b,j);

* Add fallow land area to area table
qALand(b,"fal")                 = fallowLand * SUM(j$jAF(j), qAland(b,j));
iShare(b,k,j)$qValue(b,j)       = iACosts(b,k,j) / qValue(b,j);
lShare(b,j)$jAF(j)              = (qALand(b,j) / SUM(jj$jAF(jj), qALand(b,jj)))$SUM(jj$jAF(jj), qALand(b,jj));
qInputs(b,"land",j)             = qALand(b,j);
qInputs(b,"watr",j)             = qAWater(b,j);
* Set water supply to normal year (base year cannot be dry year, as base year agricultural water use is fixed at 2009 level)
sup(s,b,m)                      = sup0(s,b,"2009",m);

pEly(y,m)                       = pElyBase(y,m);
pCrop(j)                        = pCrop0(j,"base");
natExtra(b)                     = 0;
pInput(k)                       = 1;
iInv(sInv)                      = 0;
minInflow0(b,m0)                = minInflowN(b,m0)$baseyear("2009");

* Ground water correction: Data on groundwater leaves too much water in some planning zones in some months, below is corrected for that
qmWater(b,m)$bPlz(b)            = SUM(j, qAWater(b,j)*agriSeason(b,j,m)) + qIwater(b,m) + qHwater(b,m) - sup("W",b,m);
gwCorr(b,m)                     = min(0,qmWater(b,m));

* Set maximum discharge capacity
maxDischrg(b,m)$bRes(b) = min( reservoirs(b,"capacity")*3600*24*30/1000000,
                              (reservoirs(b,"effect")*24*30/reservoirs(b,"ely"))$reservoirs(b,"ely")
                             )$bResEly(b) + INF$(not bResEly(b));
flow(bd,bo)$(SUM(m, maxDischrg(bo,m)) gt 0 and SUM(m, maxDischrg(bo,m)) lt INF)  = YES$resv(bd,bo);
OPTION flow:0:0:2;

* =============================================================================
* Declare and assign model and equations and solve
* =============================================================================

POSITIVE VARIABLES
    GRW(bd,y0,m0)           "Ground water extraction (mm3)"
    ITK(s,bo,bd,y0,m0)      "Water intakes from river nodes to zones (mm3)"
    FLW(s,bo,bd,y0,m0)      "Flows between river nodes (mm3)"
    VOL(s,b,y0,m0)          "Reservoir volume levels end of season (mm3)"
    DIS(s,bd,bo,y0,m0)      "Reservoir seasonal discharge (mm3)"
    STO(s,b,y0,m0)          "Reservoir seasonal storage buildup (mm3)"
    ELY(b,y0,m0)            "Electricity generation (MWh)"
    ELYPEAK(b,y0,m0)        "Electricity peak sales (MWh)"
    ELYBASE(b,y0,m0)        "Electricity base sales (MWh)"
    iOUTPUT(b,j,y0)         "Index for production of industrial/agricultural goods (1=baseyear)"
    iINPUT(b,j,k,y0)        "Index for use of production inputs (1=baseyear)"
    iLAND(b,j,y0)           "Index for physical amount of land"
VARIABLES
    TWV                     "Total water value (USD)"
    COSTS(b,j,y0)           "Production industrial/agricultural costs (usd/ton)"
;

OPTIONS DIS:0:3:1;
OPTIONS FLW:0:3:1;
OPTIONS ITK:0:3:1;
OPTIONS STO:0:2:1;
OPTIONS iINPUT:3:2:2;
OPTIONS iOUTPUT:3:1:2;
OPTIONS iLAND:3:1:2;


* Fix hydrological variables that can never be non-zero
FLW.fx(s,bd,bo,y,m)$(not flow(bd,bo))                       = 0;
ITK.fx(s,bd,bo,y,m)$(not intk(bd,bo))                       = 0;
DIS.fx(s,bd,bo,y,m)$(not resv(bd,bo))                       = 0;
GRW.fx(b,y,m)$(SUM(ms$mSeason(m,ms), grwtrMax(b,ms)) eq 0)  = 0;
STO.fx(s,bo,y,m)                                            = 0;
STO.up(s,b,y,m)$bRes(b)                                     = 99999;
STO.up(s,b,y,m)$sea(b)                                      = INF;
iOUTPUT.fx(b,j,y)$(qInputs(b,"land",j) eq 0)                = 0;
COSTS.fx(b,j,y)$(oCosts(b,j) eq 0)                          = 0;


* Lower and upper bounds
iINPUT.lo(b,j,k,y)                                          = 0.1$(iACosts(b,k,j) AND oCosts(b,j));
iLAND.lo(b,j,y)$jAF(j)                                      = 0.1$lShare(b,j);
iOUTPUT.lo(b,j,y)$jAF(j)                                    = 0.1$lShare(b,j);

EQUATIONS
* Hydrological constraints and identities
    WBALANCE(s,b,y0,m0)         "Water balance at all nodes"
    GRNDWATER(b,y0,mS)          "Seasonal limits on groundwater extraction"
    RESDISMAX(s,b,y0,m0)        "Maximal water throughput"
    DISCHARGE(b,y0,m0)          "Reservoir minimum discharge during season"
    RESVOL(s,b,y0,m0)           "Reservoir level end of season"
    RESFIXDIS(s,bd,bo,y0,m0)    "Fix Reservoir Discharge level"
    RESFIXVOL(s,b,y0,m0)        "Fix Reservoir Volume level"
    SEAVOL(s,b,y0,m0)           "Reservoir level end of season"
    RESVOLMAX(b,y0,m0)          "Reservoir maximum level"
    RESVOLMIN(b,y0,m0)          "Reservoir minimum level"
    ELECHEAD(b,y0,m)            "Unit Electricity generation, reservoir state dependent"
    ELECSALES(b,y0,m0)          "Total electricity sales"
    ELECPEAK(b,y0,m0)           "Electricity sales to peak price"
    ELECBASE(b,y0,m0)           "Electricity sales to base price"
    MININFLOW(b,y0,m0)          "Minimum inflow to body"
    MINOUTFLOW(b,y0,m0)         "Minimum outflow from body"
    MININFLOWa(b,y0,m0)         "Minimum inflow to body, annual"
* Economical constraints and identities
    LEONTIEF(b,j,k,y0)          "Ratio between outputs and inputs"
    CETLAND(b,y0)               "Index for physical amount of land"
    FIXEDLAND(b,j,y0)           "Fixed land shares"
* Objective functions
    OBJ                         "Objective function"
;

* Water balance by nodes:
* - left hand side is demand
* - right hand side is supply
WBALANCE(s,b,y,m)..
    scalem3 * (
     SUM(bd$intk(bd,b), ITK(s,bd,b,y,m))
   + SUM(bd$flow(bd,b), FLW(s,bd,b,y,m))
   + STO(s,b,y,m)$sea(b)
   + STO(s,b,y,m)$bRes(b)
   + SUM(j, iINPUT(b,j,"watr",y)*qAWater(b,j)*agriSeason(b,j,m))*(1-iInv("improve"))
   + qIWater(b,m)*(1+demography(y,"Ind"))
   + qHWater(b,m)*(1+demography(y,"HH"))
    )
  =e=
     ( SUM(bo$intk(b,bo), ITK(s,b,bo,y,m))
      +SUM(bo$resv(b,bo), DIS(s,b,bo,y,m))
      +SUM(bo$flow(b,bo), FLW(s,b,bo,y,m))
      +sup(s,b,m)
      +rtn0(b,m)
      +GRW(b,y,m)
     ) * (1-lossSh(b,m)) * scalem3
;

* Reservoir is calculated at end of body and end of season
* In break month we introduce requirements for buildup or rundown of reservoir
RESVOL(s,b,y,m)$(bRes(b) and (not rFixVol(b,m))  )..
    VOL(s,b,y,m)$bResSto(b) * scalem3 =e= ( SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm))$bResSto(b)
                                           +STO(s,b,y,m) - SUM(bd$resv(bd,b), DIS(s,bd,b,y,m))
                                           -ctrfBuild*reservoirs(b,"max")$(mBgn(m) AND bResBuild(b))
                                          ) * scalem3;

* Maximal water flow
RESDISMAX(s,b,y,m)$(maxDischrg(b,m) gt 0 and not rFixVol(b,m))..
    SUM(bd$resv(bd,b), DIS(s,bd,b,y,m)) * scalem3 =l= scalem3 * maxDischrg(b,m);

* Discharges must be higher than a given minimun level
DISCHARGE(b,y,m)$resDisMin(b,m)..
    SUM(s$sW(s), SUM(bd, DIS(s,bd,b,y,m))) * scalem3 =g= resDisMin(b,m) * scalem3;

* Fix volumes in selected reservoirs in selected months (for baseline)
RESFIXVOL(s,b,y,m)$rFixVol(b,m)..
    VOL(s,b,y,m) * scalem3 =e= rFixVol(b,m) * scalem3;

* Fix volumes in selected reservoirs in selected months (for baseline)
RESFIXDIS(s,bd,bo,y,m)$rFixDis(bd,bo,m)..
    FLW(s,bd,bo,y,m) + DIS(s,bd,bo,y,m) * scalem3 =e= rFixDis(bd,bo,m) * scalem3;

* Sea volumes are not restricted to primo level =e= ultimo level
SEAVOL(s,b,y,m)$sea(b)..
    VOL(s,b,y,m)  * scalem3 =e= (SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm))$(ord(m) gt 1) + STO(s,b,y,m) - SUM(bd$resv(bd,b), DIS(s,bd,b,y,m))) * scalem3;

* Maximum reservoir volume
RESVOLMAX(b,y,m)$bRes(b)..
    SUM(s$sW(s), VOL(s,b,y,m)) * scalem3 =l= (reservoirs(b,"max") * scalem3)$bResSto(b);

* Minimum reservoir volume
RESVOLMIN(b,y,m)$bRes(b)..
    SUM(s$sW(s), VOL(s,b,y,m)) * scalem3 =g= (reservoirs(b,"min") * scalem3)$bResSto(b);

* Reservoir unit electricity generation
* Filled reservoirs produce more per m3 than half empty
* OBS: Check how average volume is calcuated in the start and end months!
ELECHEAD(b,y,m)$bResEly(b)..
    ELY(b,y,m) =e= SUM(s$sW(s),  gravity * reservoirs(b,"coef_eta") * SUM(bd$resv(bd,b), DIS(s,bd,b,y,m))
                      * ( reservoirs(b,"coef_a")
                         +reservoirs(b,"coef_b") *      (VOL(s,b,y,m)+SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)))/2
                         +reservoirs(b,"coef_c") *POWER((VOL(s,b,y,m)+SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)))/2,2)
                        ) / 3.6 );

*  Peak sales are 8 hours per day
ELECPEAK(b,y,m)$bResEly(b)..
    ELYPEAK(b,y,m) =l= reservoirs(b,"effect")*30*elyPeakHrs;

* Base sales are 16 hours per day
ELECBASE(b,y,m)$bResEly(b)..
    ELYBASE(b,y,m) =l= reservoirs(b,"effect")*30*(24-elyPeakHrs);

*Total sales are base plus peak
ELECSALES(b,y,m)$bResEly(b)..
    ELYBASE(b,y,m) + ELYPEAK(b,y,m) =e= ELY(b,y,m);

* Maximum seasonal ground water extraction
GRNDWATER(b,y,mS)$grwtrMax(b,ms)..
    SUM(m$mSeason(m,ms), GRW(b,y,m)) * scalem3 =e= grwtrMax(b,ms) * scalem3;

* Monthly minimum inflow requirements
MININFLOW(b,y,m)$minInFlow0(b,m)..
    (SUM((bo,s)$sW(s), DIS(s,b,bo,y,m)$resv(b,bo)+FLW(s,b,bo,y,m)$flow(b,bo)+ITK(s,b,bo,y,m)$intk(b,bo))
   +SUM(s$sW(s), sup(s,b,m)) + rtn0(b,m)  ) * scalem3 =g= minInFlow0(b,m) * scalem3;

* Monthly minimum outflow requirements (only river nodes)
MINOUTFLOW(b,y,m)$(minInFlow0(b,m) AND bRiv(b))..
    SUM((bd,s)$sW(s), FLW(s,bd,b,y,m)$flow(bd,b)) * scalem3 =g= minInFlow0(b,m) * scalem3;

* Annual minimum flow requirements
MININFLOWa(b,y,mA)$(minInFlow0(b,mA) + natExtra(b))..
    (SUM((m,s)$sW(s), SUM(bo, DIS(s,b,bo,y,m)$resv(b,bo)+FLW(s,b,bo,y,m)$flow(b,bo)+ITK(s,b,bo,y,m)$intk(b,bo)) + sup(s,b,m))
   +SUM(m, rtn0(b,m))) * scalem3 =g= (minInFlow0(b,mA) + natExtra(b)) * scalem3;

* Leontief production technology (perfect complements)
* Also correct for the fall in yield caused by the CET function.
LEONTIEF(b,j,k,y)$iShare(b,k,j)..
    iOUTPUT(b,j,y) =e= iINPUT(b,j,k,y) * (1$jAX(j) + SUM(jj$lShare(b,jj), lShare(b,jj)*iOUTPUT(b,jj,y))$jAF(j) ) ;

* Enforce decreasing returns to scale from deviating away from base year land use patterns
* iINPUT(j,JA,"land",j) is effective land use, iLAND(b,j,y) is physical land use
CETLAND(b,y)$SUM(j$jAF(j), qInputs(b,"land",j))..
    1 =e=  ( SUM(j$lShare(b,j), lShare(b,j)*iOUTPUT(b,j,y)**rL )**(1/rL) ) ;

* Some crops have a fixed share of land
FIXEDLAND(b,j,y)$(qInputs(b,"land",j) AND jAX(j))..
    iOUTPUT(b,j,y)$jAX(j)   =e= 1;

* Objective is the total value of water from electricity, industry and agriculture
OBJ..    TWV  =e=
                    SUM((b,j,y)$oCosts(b,j), iOUTPUT(b,j,y)*qAQuant(b,j)*pCrop(j)/1000000 - SUM(k$(not kLimit(k)), iACosts(b,k,j)*iINPUT(b,j,k,y)*pInput(k)) )
                   +SUM((b,y,m)$bResEly(b), ELYBASE(b,y,m) * pEly(y,m) / 1000000 )
                   +SUM((b,y,m)$bResEly(b), ELYPEAK(b,y,m) * pEly(y,m)*elyPeakFac / 1000000 )
                   -SUM((b,y,m)$bRes(b), reservoirs(b,"cost")$(not bResNOP(b)))
                   -SUM((b,j,y), iInv("usdPerHa")*iINPUT(b,j,"land",y)*qALand(b,j) + (1-iInv0("improve"))*iInv("usdPerMM3")*iINPUT(b,j,"watr",y)*qAWater(b,j))/1000000 ;


MODEL beam  /   WBALANCE, RESVOL, RESFIXVOL, RESFIXDIS, SEAVOL, RESDISMAX, ELECHEAD, RESVOLMAX, RESVOLMIN,
                MINOUTFLOW, MININFLOW, MININFLOWa, 
                ELECBASE, ELECPEAK, ELECSALES, 
                GRNDWATER, LEONTIEF, CETLAND, FIXEDLAND, OBJ /;


* Baseline Levels
iINPUT.l(b,j,k,y)               = 1$iACosts(b,k,j);
iLAND.l(b,j,y)                  = 1$qALand(b,j);
iINPUT.l(b,j,"land",y)          = 1$qALand(b,j);
iINPUT.l(b,j,"watr",y)          = 1$qAWater(b,j);
iOUTPUT.l(b,j,y)                = 1$qAQuant(b,j);
COSTS.l(b,j,y)                  = SUM(k$(not kLimit(k)), iACosts(b,k,j));

* For testing water balance, all water in first month.
* 0 = no test, just normal run
IF(debugging,
    loss(b,m)                   = SUM((mm,s), lossSh(b,mm)*sup(s,b,mm))$(ord(m) eq 1);
    sup(s,b,m)                  = SUM(mm, sup(s,b,mm))$(ord(m) eq 1);
    lossSh(b,m)                 = (loss(b,m)/SUM(s,sup(s,b,m)))$SUM(s,sup(s,b,m));
    agriSeason(b,j,m)           = 1$(ord(m) eq 1);
    ITK.l(s,bd,bo,y,m)          = itk0(bd,bo)$(ord(m) eq 1);
    FLW.l(s,bd,bo,y,m)          = flw0(bd,bo)$(ord(m) eq 1);
    DIS.l(s,bd,bo,y,m)          = dis0(bd,bo)$(ord(m) eq 1);
    GRW.l(b,y,m)                = SUM(mS, grwtrMax(b,mS))$(ord(m) eq 1);
    qIWater(b,m)                = SUM(mm, qIWater(b,mm))$(ord(m) eq 1);
    qHWater(b,m)                = SUM(mm, qHWater(b,mm))$(ord(m) eq 1);
    rtn0(b,m)                   = SUM(mm, rtn0(b,mm))$(ord(m) eq 1);
    demography(y,"Ind")         = 0;
    demography(y,"HH")          = 0;
ELSE
    ITK.l(s,bd,bo,y,m)          = itk0(bd,bo)/12;
    FLW.l(s,bd,bo,y,m)          = flw0(bd,bo)/12;
    DIS.l(s,bd,bo,y,m)          = dis0(bd,bo)/12;
);


STO.l(s,b,y,m)$(not sea(b)) = SUM(bd, DIS.l(s,bd,b,y,m));
STO.l(s,b,y,m)$sea(b)       = SUM(bo, DIS.l(s,b,bo,y,m)+ITK.l(s,b,bo,y,m)+FLW.l(s,b,bo,y,m)) + rtn0(b,m);
VOL.l(s,b,y,m)              = ((reservoirs(b,"max") - reservoirs(b,"min"))/ 2 + reservoirs(b,"min") )$bResSto(b);
LOOP(m,
    VOL.l(s,b,y,m)          = SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) + STO.l(s,b,y,m) - SUM(bd$resv(bd,b), DIS.l(s,bd,b,y,m));
);
VOL.l(s,b,y,m)$(not bResSto(b)) = 0;
ELY.l(b,y,m)$bResEly(b)     = SUM((s,bd)$(resv(bd,b) AND sW(s)),  gravity * reservoirs(b,"coef_eta") * DIS.l(s,bd,b,y,m)
                                  * ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*VOL.l(s,b,y,m) + reservoirs(b,"coef_c")*POWER(VOL.l(s,b,y,m),2))
                              ) / 3600;


* Adjust initial variable levels for KELES which is a special case for debugging purposes
* (this does not influence results, only initial solver point)
ITK.l(s,"CHI_KAZ","KELES",y,m) = qIWater("CHI_KAZ",m) + qHWater("CHI_KAZ",m)
                                 + SUM(j, qAWater("CHI_KAZ",j)*agriSeason("CHI_KAZ",j,m)) - rtn0("CHI_KAZ",m);
FLW.l(s,"KELES","SRC_KEL",y,m) = sup(s,"SRC_KEL",m);
FLW.l(s,"SYRMID","KELES",y,m) = sup(s,"SRC_KEL",m) - ITK.l(s,"CHI_KAZ","KELES",y,m);

PARAMETER balCheck(b,*);

balCheck(bd,"inSrc")    =  SUM((s,y,m), sup(s,bd,m)) + SUM(mS, grwtrMax(bd,mS));
balCheck(bd,"inFlw")    =  SUM((s,bo,y,m)$flow(bd,bo), FLW.l(s,bd,bo,y,m));
balCheck(bd,"inDis")    =  SUM((s,bo,y,m)$resv(bd,bo), DIS.l(s,bd,bo,y,m));
balCheck(bd,"inItk")    =  SUM((s,bo,y,m)$intk(bd,bo), ITK.l(s,bd,bo,y,m));
balCheck(bd,"inRtn")    =  SUM(m, rtn0(bd,m));
balCheck(bo,"outAgr")   = -SUM((s,y,m), SUM(j, iINPUT.l(bo,j,"watr",y)*qAWater(bo,j)*agriSeason(bo,j,m))) ;
balCheck(bo,"outHHI")   = -SUM((y,m)$modelYear(y), qIWater(bo,m)*(1+demography(y,"Ind"))  + qHWater(bo,m)*(1+demography(y,"HH"))) ;
balCheck(bo,"outFlw")   = -SUM((s,bd,y,m)$flow(bd,bo), FLW.l(s,bd,bo,y,m));
balCheck(bo,"outDis")   = -SUM((s,bd,y,m)$resv(bd,bo), DIS.l(s,bd,bo,y,m));
balCheck(bo,"outItk")   = -SUM((s,bd,y,m)$intk(bd,bo), ITK.l(s,bd,bo,y,m));
balCheck(bo,"outSto")   = -SUM((s,y,m),                STO.l(s,bo,y,m))$sea(bo);
* Determine losses on inflow basis
balCheck(b,"outLos")    = -SUM((s,y,m), (SUM(bo, -FLW.l(s,b,bo,y,m) - ITK.l(s,b,bo,y,m) - DIS.l(s,b,bo,y,m)) + sup(s,b,m) + 0*gwCorr(b,m) + rtn0(b,m))*lossSh(b,m)) ;
*balCheck(bo,"outLos")$bSrc(bo)   = -SUM((s,m), sup(s,bo,m)*lossSh(bo,m));

balCheck(b,"Check")     = balCheck(b,"inSrc")  + balCheck(b,"inFlw")  + balCheck(b,"inDis")  + balCheck(b,"inItk")  + balCheck(b,"inRtn")
                             +balCheck(b,"outAgr") + balCheck(b,"outHHI") + balCheck(b,"outFlw") + balCheck(b,"outDis") + balCheck(b,"outItk") + balCheck(b,"outSto") + balCheck(b,"outLos");
balCheck(b,"Check")     = round (balCheck(b,"Check"), 3);



OPTION itk0:0:0:2;
OPTION flw0:0:0:2;
OPTION dis0:0:0:2;
DISPLAY balCheck;

beam.optfile        = 1;
*beam.holdfixed     = yes;
*beam.solprint       = yes;
beam.solprint       = no;
beam.tolinfrep      = 0.001;
beam.limrow         = 5000;
beam.limcol         = 5000;
beam.limrow         = 0;
beam.limcol         = 0;
beam.iterlim        = 50000;

*beam.iterlim        = 0;
IF(debugging,
*    beam.iterlim        = 0;
    beam.limrow         = 5000;
    beam.limcol         = 0;
)
;
*beam.iterlim        = 5000;


* We make wheat the only sluggish crop in baseline, thus it will be fixed
* (model cannot handle no fixed crops right now)

jAF(j)      = 0;
jAF("wht")  = 1;
jAX(j) = 1$(not jAF(j));
IF(not basDisOpt,
    rFixVol(b,m)                = SUM(y0$baseyear(y0), rVolBas0(b,y0,m));
    rFixDis(bd,bo,m)            = SUM(y0$baseyear(y0), rDisBas0(bd,bo,y0,m));
    sup("W","Src_ZAR","m06")    = sup("W","Src_ZAR","m06") + 2000*0;
);

PUT step;
PUT "2";
PUTCLOSE;

*beam.solprint = yes;
SOLVE beam MAXIMIZING twv USING NLP;
*abort "Test baseline only";
modlStatB = beam.modelstat;
solvStatB = beam.solvestat;
WBALANCE.M("w",b,y,m)$(WBALANCE.M("w",b,y,m) eq eps) = 0;

PUT step;
PUT "3";
PUTCLOSE;

PARAMETERS
    rFlowBase(b,e_resFlow,y0,m0)        "Flow of water through bodies, baseline"
    rFlowCtrf(b,e_resFlow,y0,m0)        "Flow of water through bodies, counterfactual"
    rAgriBase(b,e_resAgri,j,y0)         "Agricultural results for bodies, baseline"
    rAgriCtrf(b,e_resAgri,j,y0)         "Agricultural results for bodies, counterfactual"
    rEconBase(b,e_resEcon,y0,m0)        "Economical results for bodies, baseline"
    rEconCtrf(b,e_resEcon,y0,m0)        "Economical results for bodies, counterfactual"
;

OPTIONS rFlowBase:1:3:1;
OPTIONS rEconBase:1:2:1;
OPTIONS rAgriBase:1:2:2;
OPTIONS rFlowCtrf:1:3:1;
OPTIONS rEconCtrf:1:2:2;
OPTIONS rAgriCtrf:1:2:2;

* Flows by seasons and years, baseline
rFlowBase(b,"ResVol",y,m)                 = SUM(s$sW(s),      VOL.l(s,b,y,m));
rFlowBase(b,"ResVolAvg",y,m)              = SUM(s$sW(s),      VOL.l(s,b,y,m) + SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) ) / 2 ;
rFlowBase(b,"ResChg",y,m)                 = SUM(s$sW(s),      VOL.l(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) );

rFlowBase(b,"ResChg",y,m)$bResBuild(b)    = -SUM(s$sW(s),      -STO.l(s,b,y,m) + SUM(bd, DIS.l(s,bd,b,y,m) + FLW.l(s,bd,b,y,m)) );

rFlowBase(b,"ResChg",y,m)$(sea(b) and mBgn(m))          = SUM(s$sW(s),     VOL.l(s,b,y,m));
rFlowBase(b,"InSrc",y,m)                  = SUM(s$sW(s),      sup(s,b,m));
rFlowBase(b,"InGrw",y,m)                  = GRW.l(b,y,m);
rFlowBase(b,"InDis",y,m)                  = SUM((s,bo),       DIS.l(s,b,bo,y,m));
rFlowBase(b,"InFlw",y,m)                  = SUM((s,bo),       FLW.l(s,b,bo,y,m));
rFlowBase(b,"InItk",y,m)                  = SUM((s,bo),       ITK.l(s,b,bo,y,m));
rFlowBase(b,"InRtn",y,m)                  = rtn0(b,m);
rFlowBase(b,"InTot",y,m)                  = rFlowBase(b,"InSrc",y,m) + rFlowBase(b,"InDis",y,m) + rFlowBase(b,"InFlw",y,m) + rFlowBase(b,"InItk",y,m) + rFlowBase(b,"InRtn",y,m) + rFlowBase(b,"InGrw",y,m);
rFlowBase(b,"OutDem",y,m)                 = SUM(j,            -iINPUT.l(b,j,"watr",y)*qAWater(b,j)*agriSeason(b,j,m))*(1-iInv0("improve")) - qIwater(b,m) - qHwater(b,m);
rFlowBase(b,"OutSto",y,m)                 = -SUM(s$sW(s),     STO.l(s,b,y,m));
rFlowBase(b,"OutDis",y,m)                 = -SUM((s,bd)$(not sea(bd)), DIS.l(s,bd,b,y,m));
rFlowBase(b,"OutFlw",y,m)                 = -SUM((s,bd)$(not sea(bd)), FLW.l(s,bd,b,y,m));
rFlowBase(b,"OutInt",y,m)                 = -SUM((s,bd)$(not sea(bd)), ITK.l(s,bd,b,y,m));
rFlowBase(b,"OutLak",y,m)                 = -SUM((s,bd)$(    sea(bd)), ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)+DIS.l(s,bd,b,y,m));
rFlowBase(b,"OutLos",y,m)                 = -rFlowBase(b,"InTot",y,m) * lossSh(b,m);
rFlowBase(b,"OutTot",y,m)                 = rFlowBase(b,"OutDem",y,m) + rFlowBase(b,"OutDis",y,m) + rFlowBase(b,"OutFlw",y,m) + rFlowBase(b,"OutInt",y,m) + rFlowBase(b,"OutLos",y,m) + rFlowBase(b,"OutLak",y,m);
rFlowBase(b,"Balance",y,m)                = round(rFlowBase(b,"InTot",y,m)+rFlowBase(b,"OutTot",y,m)-rFlowBase(b,"ResChg",y,m)$bRes(b), 3) ;
rFlowBase(b,e_resFlow,y,"annual")         = SUM(m,            rFlowBase(b,e_resFlow,y,m));
rFlowBase(b,"ResVol",y,"annual")          = rFlowBase(b,"ResVol",y,"annual") + 0.000001;
rFlowBase(b,"Balance",y,"annual")         = rFlowBase(b,"Balance",y,"annual") + 0.000001;

* Agricultural baseline results
rAgriBase(b,"WaterUse",j,y)                   = iINPUT.l(b,j,"watr",y)*qAWater(b,j)*(1-iInv("improve"));
rAgriBase(b,"LandEff",j,y)$bPlz(b)            = 1000*SUM(jj$lShare(b,jj),  iINPUT.l(b,jj,"land",y)*qALand(b,jj)) /  SUM(jj$lShare(b,jj), qALand(b,jj)) - 1;
rAgriBase(b,"LandEff",j,y)$(not jAF(j))       = 0;
*rAgriBase(b,"LandUse",j,y)                    = qALand(b,j) * iINPUT.l(b,j,"land",y) / (1+rAgriBase(b,"LandEff",j,y)/1000) ;
rAgriBase(b,"LandUse",j,y)                    = qALand(b,j) * iINPUT.l(b,j,"land",y);
rAgriBase(b,"AgriSales",j,y)                  = 1000*iOUTPUT.l(b,j,y)*qAQuant(b,j)*pCrop(j) / 1000000;
rAgriBase(b,"AgriCosts",j,y)                  = -1000*SUM(k$(not kLimit(k)), iACosts(b,k,j)*iINPUT.l(b,j,k,y)*pInput(k));
rAgriBase(b,"IrrInvCost",j,y)                 = -1000*(iInv("usdPerHa")*iINPUT.l(b,j,"land",y)*qALand(b,j) + (1-iInv("improve"))*iInv("usdPerMM3")*iINPUT.l(b,j,"watr",y)*qAWater(b,j))/1000000 ;
rAgriBase(b,"AgriIncome",j,y)                 = rAgriBase(b,"AgriSales",j,y) + rAgriBase(b,"AgriCosts",j,y) + rAgriBase(b,"IrrInvCost",j,y);
rAgriBase(b,"AgriProd",j,y)                   = iOUTPUT.l(b,j,y)*qAQuant(b,j);
rAgriBase(b,"LandIncome",j,y)                 = ( 1000000*rAgriBase(b,"AgriIncome",j,y) / rAgriBase(b,"LandUse",j,y) )$rAgriBase(b,"LandUse",j,y);
rAgriBase(b,"WatrIncome",j,y)                 = ( 1000000*rAgriBase(b,"AgriIncome",j,y) / rAgriBase(b,"WaterUse",j,y) )$rAgriBase(b,"WaterUse",j,y);
rAgriBase(b,"LabrUse",j,y)                    = 1000*iINPUT.l(b,j,"labr",y)*iACosts(b,"labr",j);

* Economic results, baseline
rEconBase(b,"AgriSales",y,m)              = SUM(j, rAgriBase(b,"AgriSales",j,y) ) / 12;
rEconBase(b,"AgriInc",y,m)                = SUM(j, rAgriBase(b,"AgriIncome",j,y)) / 12;
rEconBase(b,"IrrInvCost",y,m)             = SUM(j, rAgriBase(b,"IrrInvCost",j,y)) / 12;
rEconBase(b,"AgriCost",y,m)               = SUM(j, rAgriBase(b,"AgriCosts",j,y) ) / 12;
rEconBase(b,"HydroCost",y,m)              = -1000*reservoirs(b,"cost")$(not bResNOP(b));
rEconBase(b,"HydroInc",y,m)               = 1000*(ELYBASE.l(b,y,m) *  pEly(y,m) +ELYPEAK.l(b,y,m) *  pEly(y,m)*elyPeakFac) / 1000000 + rEconBase(b,"HydroCost",y,m);
rEconBase(b,"HydroProd",y,m)              = ELY.l(b,y,m);
rEconBase(b,"HydroHead",y,m)              = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlowBase(b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlowBase(b,"ResVolAvg",y,m),2));
rEconBase(b,"HydroGain",y,m)              = -( rEconBase(b,"HydroProd",y,m) / rFlowBase(b,"OutDis",y,m) )$rFlowBase(b,"OutDis",y,m);
rEconBase(b,"TotInc",y,m)                 = rEconBase(b,"AgriInc",y,m) + rEconBase(b,"HydroInc",y,m);
rEconBase(b,"DischVal",y,m)$bResEly(b)    = -1000000*( rEconBase(b,"HydroInc",y,m) / rFlowBase(b,"OutDis",y,m) )$rFlowBase(b,"OutDis",y,m);
rEconBase(b,"WaterVal",y,m)               = WBALANCE.M("w",b,y,m)*1000000;
rEconBase(b,"WaterVal",y,"annual")        = ( SUM(m, rEconBase(b,"WaterVal",y,m) * rFlowBase(b,"InTot",y,m) )  / rFlowBase(b,"InTot",y,"annual")
                                                  )$rFlowBase(b,"InTot",y,"annual") + 0.000001;


* Allow flexibility in land allocation
jAF(j)                  = jAF0(j);
jAF("fal")              = YES$fallowLand;
jAX(j)                  = 1$(not jAF(j));
* Set water supply (choose between dry and normal)
sup(s,b,m)              = SUM(y0$baseyear(y0), sup0(s,b,y0,m));
* Set minimum inflows according to baseyear
minInflow0(b,m0)                = minInflowN(b,m0)$baseyear("2009") + minInflowD(b,m0)$baseyear("2001");
* Set counterfactual electricity prices
pEly(y,m)               = pElyCtrf(y,m);
* Set new reservoirs in operation active in counterfactual scenario
bResNOP(b)              = YES$(resNew(b) AND not resNewOp(b));
bResSto(b)              = YES$(reservoirs(b,"max") gt reservoirs(b,"min"));
bResSto(b)$bResNOP(b)   = NO;
bResEly(b)              = YES$(reservoirs(b,"Ely") > 0 AND not bResNOP(b));
* Require extra nature needs for counterfactual
natExtra(b)             = natExtra0(b);
* Set counterfactual crop prices
pCrop(j)                = pCrop0(j,"ctrf");
* change input prices
pInput(k)               = pInput0(k);
* Reset fixed discharge patterns from baseline
rFixVol(b,m)            = 0;
rFixDis(bd,bo,m)        = 0;
* Set irrigation investment costs
iInv(sInv)              = iInv0(sInv);
* Set reservoir buildup
ctrfBuild = ctrfBuild0;


*DISPLAY minInflow0, sup0, sup, baseyear;
*beam.solprint = yes;
*beam.limrow = 5000;
SOLVE beam MAXIMIZING twv USING NLP;
modlStatS = beam.modelstat;
solvStatS = beam.solvestat;

WBALANCE.M("w",b,y,m)$(WBALANCE.M("w",b,y,m) eq eps) = 0;


* Flows by seasons and years counterfactual
rFlowCtrf(b,"ResVol",y,m)                 = SUM(s$sW(s),      VOL.l(s,b,y,m));
rFlowCtrf(b,"ResVolAvg",y,m)              = SUM(s$sW(s),      VOL.l(s,b,y,m) + SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) ) / 2 ;
rFlowCtrf(b,"ResChg",y,m)                 = SUM(s$sW(s),      VOL.l(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) );

rFlowCtrf(b,"ResChg",y,m)$bResBuild(b)    = -SUM(s$sW(s),      -STO.l(s,b,y,m) + SUM(bd, DIS.l(s,bd,b,y,m) + FLW.l(s,bd,b,y,m)) );


rFlowCtrf(b,"ResChg",y,m)$(sea(b) and mBgn(m))          = SUM(s$sW(s),     VOL.l(s,b,y,m));
rFlowCtrf(b,"InSrc",y,m)                  = SUM(s$sW(s),      sup(s,b,m));
rFlowCtrf(b,"InGrw",y,m)                  = GRW.l(b,y,m);
rFlowCtrf(b,"InDis",y,m)                  = SUM((s,bo),       DIS.l(s,b,bo,y,m));
rFlowCtrf(b,"InFlw",y,m)                  = SUM((s,bo),       FLW.l(s,b,bo,y,m));
rFlowCtrf(b,"InItk",y,m)                  = SUM((s,bo),       ITK.l(s,b,bo,y,m));
rFlowCtrf(b,"InRtn",y,m)                  = rtn0(b,m);
rFlowCtrf(b,"InTot",y,m)                  = rFlowCtrf(b,"InSrc",y,m) + rFlowCtrf(b,"InDis",y,m) + rFlowCtrf(b,"InFlw",y,m) + rFlowCtrf(b,"InItk",y,m) + rFlowCtrf(b,"InRtn",y,m) + rFlowCtrf(b,"InGrw",y,m);
rFlowCtrf(b,"OutDem",y,m)                 = SUM(j,            -iINPUT.l(b,j,"watr",y)*qAWater(b,j)*agriSeason(b,j,m))*(1-iInv0("improve")) - qIwater(b,m) - qHwater(b,m);
rFlowCtrf(b,"OutSto",y,m)                 = -SUM(s$sW(s),     STO.l(s,b,y,m));
rFlowCtrf(b,"OutDis",y,m)                 = -SUM((s,bd)$(not sea(bd)),      DIS.l(s,bd,b,y,m));
rFlowCtrf(b,"OutFlw",y,m)                 = -SUM((s,bd)$(not sea(bd)),      FLW.l(s,bd,b,y,m));
rFlowCtrf(b,"OutInt",y,m)                 = -SUM((s,bd)$(not sea(bd)),      ITK.l(s,bd,b,y,m));
rFlowCtrf(b,"OutLak",y,m)                 = -SUM((s,bd)$(    sea(bd)),      ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)+DIS.l(s,bd,b,y,m));
rFlowCtrf(b,"OutLos",y,m)                 = -rFlowCtrf(b,"InTot",y,m) * lossSh(b,m);
rFlowCtrf(b,"OutTot",y,m)                 = rFlowCtrf(b,"OutDem",y,m) + rFlowCtrf(b,"OutDis",y,m) + rFlowCtrf(b,"OutFlw",y,m) + rFlowCtrf(b,"OutInt",y,m) + rFlowCtrf(b,"OutLos",y,m) + rFlowCtrf(b,"OutLak",y,m);
rFlowCtrf(b,"Balance",y,m)                = round(rFlowCtrf(b,"InTot",y,m)+rFlowCtrf(b,"OutTot",y,m)-rFlowCtrf(b,"ResChg",y,m)$bRes(b), 3);
* Add a tiny bit in order for GDX to show variable
rFlowCtrf(b,e_resFlow,y,"annual")         = SUM(m,            rFlowCtrf(b,e_resFlow,y,m));
rFlowCtrf(b,"ResVol",y,"annual")          = rFlowCtrf(b,"ResVol",y,"annual") + 0.000001;
rFlowCtrf(b,"Balance",y,"annual")         = rFlowCtrf(b,"Balance",y,"annual") + 0.000001;


* Agricultural counterfactual results
rAgriCtrf(b,"WaterUse",j,y)                   = iINPUT.l(b,j,"watr",y)*qAWater(b,j)*(1-iInv0("improve"));
rAgriCtrf(b,"WaterSave",j,y)                  = iINPUT.l(b,j,"watr",y)*qAWater(b,j)*iInv0("improve");
rAgriCtrf(b,"LandEff",j,y)$bPlz(b)            = 1000*SUM(jj$lShare(b,jj),  iINPUT.l(b,jj,"land",y)*qALand(b,jj)) /  SUM(jj$lShare(b,jj), qALand(b,jj)) - 1;
rAgriCtrf(b,"LandEff",j,y)$(not jAF(j))       = 0.001;
*rAgriCtrf(b,"LandUse",j,y)                    = qALand(b,j) * iINPUT.l(b,j,"land",y) / (1+rAgriBase(b,"LandEff",j,y)/1000) ;
rAgriCtrf(b,"LandUse",j,y)                    = qALand(b,j) * iINPUT.l(b,j,"land",y) ;
rAgriCtrf(b,"AgriSales",j,y)                  = 1000*iOUTPUT.l(b,j,y)*qAQuant(b,j)*pCrop(j) / 1000000;
rAgriCtrf(b,"AgriCosts",j,y)                  = -1000*SUM(k$(not kLimit(k)), iACosts(b,k,j)*iINPUT.l(b,j,k,y)*pInput(k));
rAgriCtrf(b,"IrrInvCost",j,y)                 = -1000*(iInv("usdPerHa")*iINPUT.l(b,j,"land",y)*qALand(b,j) + (1-iInv("improve"))*iInv("usdPerMM3")*iINPUT.l(b,j,"watr",y)*qAWater(b,j))/1000000 ;
rAgriCtrf(b,"AgriIncome",j,y)                 = rAgriCtrf(b,"AgriSales",j,y) + rAgriCtrf(b,"AgriCosts",j,y) + rAgriCtrf(b,"IrrInvCost",j,y);
rAgriCtrf(b,"AgriProd",j,y)                   = iOUTPUT.l(b,j,y)*qAQuant(b,j);
rAgriCtrf(b,"LandIncome",j,y)                 = ( 1000000*rAgriCtrf(b,"AgriIncome",j,y) / rAgriCtrf(b,"LandUse",j,y) )$rAgriCtrf(b,"LandUse",j,y);
rAgriCtrf(b,"WatrIncome",j,y)                 = ( 1000000*rAgriCtrf(b,"AgriIncome",j,y) / rAgriCtrf(b,"WaterUse",j,y) )$rAgriCtrf(b,"WaterUse",j,y);
rAgriCtrf(b,"LabrUse",j,y)                    = 1000*iINPUT.l(b,j,"labr",y)*iACosts(b,"labr",j);


* Economic results, counterfactual
rEconCtrf(b,"AgriSales",y,m)              = SUM(j, rAgriCtrf(b,"AgriSales",j,y) ) / 12;
rEconCtrf(b,"AgriInc",y,m)                = SUM(j, rAgriCtrf(b,"AgriIncome",j,y)) / 12;
rEconCtrf(b,"IrrInvCost",y,m)             = SUM(j, rAgriCtrf(b,"IrrInvCost",j,y)) / 12;
rEconCtrf(b,"AgriCost",y,m)               = SUM(j, rAgriCtrf(b,"AgriCosts",j,y) ) / 12;
rEconCtrf(b,"HydroCost",y,m)              = -1000*reservoirs(b,"cost")$(not bResNOP(b));
rEconCtrf(b,"HydroInc",y,m)               = 1000*(ELYBASE.l(b,y,m) *  pEly(y,m) +ELYPEAK.l(b,y,m) *  pEly(y,m)*elyPeakFac) / 1000000 + rEconBase(b,"HydroCost",y,m);;
rEconCtrf(b,"HydroProd",y,m)              = ELY.l(b,y,m);
rEconCtrf(b,"HydroHead",y,m)              = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlowCtrf(b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlowCtrf(b,"ResVolAvg",y,m),2));
rEconCtrf(b,"HydroGain",y,m)              = -( rEconCtrf(b,"HydroProd",y,m) / rFlowCtrf(b,"OutDis",y,m) )$rFlowCtrf(b,"OutDis",y,m) ;
rEconCtrf(b,"TotInc",y,m)                 = rEconCtrf(b,"AgriInc",y,m) + rEconCtrf(b,"HydroInc",y,m);
rEconCtrf(b,"WaterVal",y,m)               = WBALANCE.M("w",b,y,m)*1000000;
rEconCtrf(b,"WaterVal",y,"annual")        = ( SUM(m, rEconCtrf(b,"WaterVal",y,m) * rFlowCtrf(b,"InTot",y,m) )  / rFlowCtrf(b,"InTot",y,"annual")
                                                  )$rFlowCtrf(b,"InTot",y,"annual") + 0.000001;


*$ontext
IF(beam.solvestat eq 1,
*    DISPLAY rAgriBase, rFlowBase, rEconBase, rAgriCtrf, rFlowCtrf, rEconCtrf;
*    execute_unload "beamOutput.gdx" rAgriBase, rFlowBase, rEconBase, rAgriCtrf, rFlowCtrf, rEconCtrf;
*    execute "gdxxrw.exe I=beamOutput.gdx O=beamOutput.xls index=index!d10";
*    execute "pause";
);
*$offtexts

file fFlowBase /%path%output\FlowBase.csv/;
fFLowBase.pw = 170;
PUT fFlowBase;
PUT "                        "; LOOP((y,mN,m)$mX(mN,m), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN,m)$mX(mN,m), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resFlow),
    PUT b.tl:12; PUT e_resFlow.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rFlowBase(b,e_resFlow,y,m)):12:0; ); PUT /;
);

file fFlowCtrf /%path%output\FlowCtrf.csv/;
fFLowCtrf.pw = 170;
PUT fFlowCtrf;
PUT "                        "; LOOP((y,mN), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resFlow),
    PUT b.tl:12; PUT e_resFlow.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rFlowCtrf(b,e_resFlow,y,m)):12:0; ); PUT /;
);

file fAgriBase /%path%output\AgriBase.csv/;
fAgriBase.pw = 120;
PUT fAgriBase;
PUT "                        "; LOOP((y,j), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,j), PUT j.tl:>12 ); PUT /;
LOOP((b,e_resAgri)$bPlz(b),
    PUT b.tl:12; PUT e_resAgri.tl:12;
    LOOP((y,j), PUT round(rAgriBase(b,e_resAgri,j,y)):12:0; ); PUT /;
);

file fAgriCtrf /%path%output\AgriCtrf.csv/;
fAgriCtrf.pw = 120;
PUT fAgriCtrf;
PUT "                        "; LOOP((y,j), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,j), PUT j.tl:>12 ); PUT /;
LOOP((b,e_resAgri)$bPlz(b),
    PUT b.tl:12; PUT e_resAgri.tl:12;
    LOOP((y,j), PUT round(rAgriCtrf(b,e_resAgri,j,y)):12:0; ); PUT /;
);

file fEconBase /%path%output\EconBase.csv/;
fEconBase.pw = 170;
PUT fEconBase;
PUT "                        "; LOOP((y,mN), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resEcon)$(bPlz(b) or bRes(b)),
    PUT b.tl:12; PUT e_resEcon.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rEconBase(b,e_resEcon,y,m)):12:0; ); PUT /;
);

file fEconCtrf /%path%output\EconCtrf.csv/;
fEconCtrf.pw = 170;
PUT fEconCtrf;
PUT "                        "; LOOP((y,mN), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resEcon)$(bPlz(b) or bRes(b)),
    PUT b.tl:12; PUT e_resEcon.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rEconCtrf(b,e_resEcon,y,m)):12:0; ); PUT /;
);

PUTCLOSE;

SET sAss /
    sSta    "Solver status"
    mSta    "Model status"
    pCot    "Cotton price USD/ton"
    pWht    "Wheat price USD/ton"
    pRic    "Rice price USD/ton"
    pAlf    "Alfalfa price USD/ton"
    pVeg    "Vegetable price USD/ton"
    pFru    "Fruit price USD/ton"
    pOth    "Other crops price USD/ton"
    pLab    "Labor price USD/ha"
    pCap    "Capital price USD/ha"
    pInp    "Diesel/fertilizer price USD/ha"
    eta     "Crop change transformation elasticity"
    xARN    "Extra nature water need, Aral Sea North, mm3/year"
    xARS    "Extra nature water need, Aral Sea South, mm3/year"
    xGOL    "Extra nature water need, Golden Lake, mm3/year"
    fCot    "Cotton flexible (0/1)?"
    fWht    "Wheat flexible (0/1)?"
    fRic    "Rice flexible (0/1)?"
    fAlf    "Alfalfa flexible (0/1)?"
    rain    "Rainfall year"
    year    "Modelled years"
    iIha    "Irrigation efficiency investment USD/ha"
    iIm3    "Irrigation efficiency investment USD/m3"
    iImp    "Irrigatopn efficiency improvement (%)"
    iDAS    "Investments in Dashtijum"
    iROG    "Investments in Rogun"
    iKAM    "Investments in Kambarata-1"
    iNAR    "Investments in Naryn cascade"
    iVAH    "Investments in Vakhsh cascade"
    iZAR    "Investments in Yavan"
    pE01    "Electricity price m01 USD/MWh"
    pE02    "Electricity price m02 USD/MWh"
    pE03    "Electricity price m03 USD/MWh"
    pE04    "Electricity price m04 USD/MWh"
    pE05    "Electricity price m05 USD/MWh"
    pE06    "Electricity price m06 USD/MWh"
    pE07    "Electricity price m07 USD/MWh"
    pE08    "Electricity price m08 USD/MWh"
    pE09    "Electricity price m09 USD/MWh"
    pE10    "Electricity price m10 USD/MWh"
    pE11    "Electricity price m11 USD/MWh"
    pE12    "Electricity price m12 USD/MWh"
    rBld    "Reservoir buildup"
    cDHH    "Demographic change households"
    cDIn    "Demographic change industry"
    aFal    "Fallow land as percent of flexible crop area"
/;



PARAMETER assumptn(sAss,sSce);

assumptn("pCot",sSce)   = pCrop0("cot",sSce);
assumptn("pWht",sSce)   = pCrop0("wht",sSce);
assumptn("pRic",sSce)   = pCrop0("ric",sSce);
assumptn("pAlf",sSce)   = pCrop0("alf",sSce);
assumptn("pVeg",sSce)   = pCrop0("veg",sSce);
assumptn("pFru",sSce)   = pCrop0("fru",sSce);
assumptn("pOth",sSce)   = pCrop0("oth",sSce);
assumptn("eta ",sSce)   = sT*1000;

assumptn("fCot",sSce)   = jAF("cot");
assumptn("fWht",sSce)   = jAF("wht");
assumptn("fRic",sSce)   = jAF("ric");
assumptn("fAlf",sSce)   = jAF("alf");

assumptn("iIha",sSce)   = 0;
assumptn("iIm3",sSce)   = 0;
assumptn("iImp",sSce)   = 0;
assumptn("iDAS",sSce)   = 0;
assumptn("iROG",sSce)   = 0;
assumptn("iKAM",sSce)   = 0;
assumptn("iNAR",sSce)   = 0;
assumptn("iVAH",sSce)   = 0;
assumptn("iZAR",sSce)   = 0;

assumptn("pE01","base")   = SUM(y$modelYear(y), pElyBase(y,"m01"));
assumptn("pE02","base")   = SUM(y$modelYear(y), pElyBase(y,"m02"));
assumptn("pE03","base")   = SUM(y$modelYear(y), pElyBase(y,"m03"));
assumptn("pE04","base")   = SUM(y$modelYear(y), pElyBase(y,"m04"));
assumptn("pE05","base")   = SUM(y$modelYear(y), pElyBase(y,"m05"));
assumptn("pE06","base")   = SUM(y$modelYear(y), pElyBase(y,"m06"));
assumptn("pE07","base")   = SUM(y$modelYear(y), pElyBase(y,"m07"));
assumptn("pE08","base")   = SUM(y$modelYear(y), pElyBase(y,"m08"));
assumptn("pE09","base")   = SUM(y$modelYear(y), pElyBase(y,"m09"));
assumptn("pE10","base")   = SUM(y$modelYear(y), pElyBase(y,"m10"));
assumptn("pE11","base")   = SUM(y$modelYear(y), pElyBase(y,"m11"));
assumptn("pE12","base")   = SUM(y$modelYear(y), pElyBase(y,"m12"));

assumptn("rain","base")   = 2009;
assumptn("rain","ctrf")   = 2001$baseYear("2001") + 2009$baseYear("2009") ;

assumptn("year","base")   = 2009$modelYear("2009") ;
assumptn("year","ctrf")   = 2009$modelYear("2009") + 2020$modelYear("2020") + 2050$modelYear("2050");

assumptn("pLab","base")   = 1000;
assumptn("pCap","base")   = 1000;
assumptn("pInp","base")   = 1000;

assumptn("pLab","ctrf")   = pInput("labr")*1000;
assumptn("pCap","ctrf")   = pInput("cptl")*1000;
assumptn("pInp","ctrf")   = pInput("inpt")*1000;

assumptn("xARN","ctrf")   = natExtra("LAK_ARN");
assumptn("xARS","ctrf")   = natExtra("LAK_ARS");
assumptn("xGOL","ctrf")   = natExtra("LAK_GOL");


assumptn("iIha","ctrf")   = iInv0("usdPerMm3")*1000;
assumptn("iIm3","ctrf")   = iInv0("usdPerHa")*1000;
assumptn("iImp","ctrf")   = iInv0("improve")*1000;
assumptn("iDAS","ctrf")   = resNewOp("res_DAS");
assumptn("iROG","ctrf")   = resNewOp("res_ROG");
assumptn("iKAM","ctrf")   = resNewOp("res_KAM");
assumptn("iNAR","ctrf")   = resNewOp("res_NAR");
assumptn("iVAH","ctrf")   = resNewOp("res_VAH");
assumptn("iZAR","ctrf")   = resNewOp("res_ZAR");

assumptn("pE01","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m01"));
assumptn("pE02","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m02"));
assumptn("pE03","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m03"));
assumptn("pE04","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m04"));
assumptn("pE05","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m05"));
assumptn("pE06","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m06"));
assumptn("pE07","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m07"));
assumptn("pE08","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m08"));
assumptn("pE09","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m09"));
assumptn("pE10","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m10"));
assumptn("pE11","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m11"));
assumptn("pE12","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m12"));

assumptn("mSta","base") = modlStatB;
assumptn("mSta","ctrf") = modlStatS;
assumptn("sSta","base") = solvStatB;
assumptn("sSta","ctrf") = solvStatS;

assumptn("rBld","base") = SUM((b,m,y,s)$(sW(s) AND bResBuild(b)), STO.l(s,b,y,m) - SUM(bd$resv(bd,b), DIS.l(s,bd,b,y,m)+ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)) )
                          / SUM(b$bResBuild(b), reservoirs(b,"max"))$SUM(b$bResBuild(b), reservoirs(b,"max"));
assumptn("rBld","ctrf") = ctrfBuild0*1000;

assumptn("cDHH","ctrf") = SUM(y$modelYear(y), demography(y,"HH"))*1000;
assumptn("cDIn","ctrf") = SUM(y$modelYear(y), demography(y,"Ind"))*1000;

assumptn("aFal","base") = 0;
assumptn("aFal","ctrf") = fallowLand*1000;


file fAssumptn /%path%output\assumptn.csv/;
fAssumptn.pw = 170;
PUT fAssumptn;
PUT "            "; LOOP(sSce, PUT sSce.tl:>12 ); PUT /;
LOOP(sAss,
    PUT sAss.tl:12;
    LOOP(sSce, PUT assumptn(sAss,sSce):12:0; ); PUT /;
);
PUTCLOSE;

PUT step;
PUT "4";
PUTCLOSE;


file fObjValues /%path%output\objValues.txt/
PUT fObjValues;
PUT "Base total income: ";
PUT SUM((b,y,m), rEconBase(b,"TotInc",y,m)); PUT /;
PUT "Ctrf total income: ";
PUT SUM((b,y,m), rEconCtrf(b,"TotInc",y,m)); PUT /;
PUTCLOSE;

DISPLAY minInflow0, natExtra;