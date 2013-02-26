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


MODEL beam  /   WBALANCE, RESVOL, RESFIXVOL, RESFIXDIS, SEAVOL, 
                RESDISMAX, ELECHEAD, RESVOLMAX, RESVOLMIN,
                MINOUTFLOW, MININFLOW, MININFLOWa, 
                ELECBASE, ELECPEAK, ELECSALES, 
                GRNDWATER, LEONTIEF, CETLAND, FIXEDLAND, 
                OBJ /;


 
beam.optfile        = 1;
*beam.holdfixed     = yes;
*beam.solprint       = yes;
beam.solprint       = no;
beam.tolinfrep      = 0.001;
beam.limrow         = 5000;
beam.limcol         = 5000;
beam.iterlim        = 50000;

*beam.iterlim        = 0;
IF(debugging,
*    beam.iterlim        = 0;
    beam.limrow         = 5000;
    beam.limcol         = 0;
)
;
*beam.iterlim        = 5000;

* =============================================================================
* Fix unused variables and set levels
* =============================================================================


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
