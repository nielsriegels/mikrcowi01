POSITIVE VARIABLES
    GRW(bd,y0,m0)           "Ground water extraction (mm3)"
    IRG(b,j,y0,m0)          "Actual irrigation (mm3)"
    HHI(s,sSec,b,y0,m0)     "Water use by households and industries (mm3)"
    LOS(s,b,l,y0,m0)        "Losses to seepage and evaporation (mm3)"
    ITK(s,bo,bd,y0,m0)      "Water intakes from river nodes to zones (mm3)"
    FLW(s,bo,bd,y0,m0)      "Flows between river nodes (mm3)"
    VOL(s,b,y0,m0)          "Reservoir volume levels end of season (mm3)"
    DIS(s,bd,bo,y0,m0)      "Reservoir seasonal discharge (mm3)"
    STO(s,b,y0,m0)          "Reservoir seasonal storage buildup (mm3)"
    HPP(b,g0,y0,m0)         "Hydro power production (MWh)"
    TPP(u,g0,y0,m0)         "Thermal power production (MWh)"
    CRP(b,j,y0)             "Crop production (ton/year)"
    LND(b,j,y0)             "Land use by crop (ha)"
VARIABLES
    TWV                     "Total water value (USD)"
;

OPTIONS DIS:0:3:1;
OPTIONS FLW:0:3:1;
OPTIONS ITK:0:3:1;
OPTIONS IRG:0:3:1;
OPTIONS LOS:0:4:1;
OPTIONS STO:0:2:1;
OPTIONS HPP:0:3:1;
OPTIONS TPP:0:3:1;


EQUATIONS
* Hydrological constraints and identities
    WBALANCE(s,b,y0,m0)         "Water balance at all nodes"
    GRNDWATER(b,y0,mS)          "Seasonal limits on groundwater extraction"
    LOSSES(s,b,l,y0,m0)         "Losses to seepage and evaporation"
    IRGACT(b,j,y0,m0)           "Actual irrigation need by zone"
    HHIUSES(s,sSec,b,y0,m0)     "Water use by households and industries"
    SEAVOL(s,b,y0,m0)           "Reservoir level end of season"
    RESVOL(s,b,y0,m0)           "Reservoir level end of season"
    RESVOLMAX(b,y0,m0)          "Reservoir maximum level"
    RESVOLMIN(b,y0,m0)          "Reservoir minimum level"
    RESDISMAX(s,b,y0,m0)        "Maximal water throughput"
    RESDISMIN(b,y0,m0)          "Reservoir minimum discharge during season"
    RESFIXDIS(s,bd,bo,y0,m0)    "Fix Reservoir Discharge level"
    RESFIXVOL(s,b,y0,m0)        "Fix Reservoir Volume level"
    ELYHEAD(b,y0,m)             "Unit Electricity generation, reservoir state dependent"
    ELYHPP(b,g0,y0,m0)          "Hydro power production"
    ELYTPP(u,g0,y0,m0)          "Thermal power production"
    ELYMKT(g,y0,m0)             "Power market balance"
    MININFLOW(b,y0,m0)          "Minimum inflow to body"
    MINOUTFLOW(b,y0,m0)         "Minimum outflow from body"
    MININFLOWa(b,y0,m0)         "Minimum inflow to body, annual"
    INDEXCALCULATOR(b,j,k,y0)     "Index for the relation between new crop output and the old crop output"
* Economical constraints and identities
    LEONTIEF(b,j,y0)            "Ratio between outputs and inputs"
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
   + SUM(j$irg0(b,j,y,m), IRG(b,j,y,m))
   + SUM(sSec,            HHI(s,sSec,b,y,m))
   + LOS(s,b,"seep",y,m)

   ) =e= (
       SUM(bo$intk(b,bo), ITK(s,b,bo,y,m))
      +SUM(bo$resv(b,bo), DIS(s,b,bo,y,m))
      +SUM(bo$flow(b,bo), FLW(s,b,bo,y,m))
      +sup(s,b,m)
      +rtn0(b,m)
      +GRW(b,y,m)
     ) * scalem3
;

* Account for losses to seepage and evaporation
LOSSES(s,b,l,y,m)$(lossRivers(b,l,m))..
    LOS(s,b,l,y,m) =e= lossRivers(b,l,m) * (
                          SUM(bo$flow(b,bo), FLW(s,b,bo,y,m))
                        + SUM(bo$resv(b,bo), DIS(s,b,bo,y,m))
                        + sup(s,b,m)
                        + rtn0(b,m) )
                      +lossChannels(b,l,m) * (
                          SUM(bo$intk(b,bo), ITK(s,b,bo,y,m)) )
;

* Irrigation needs - irg0 is base year irrigation, 
IRGACT(b,j,y,m)$irg0(b,j,y,m)..
    IRG(b,j,y,m) =e= (LND(b,j,y)/lnd0(b,j))*irg0(b,j,y,m) * (1-iInv("improve"));

* Use of water by households and industries
HHIUSES(s,sSec,b,y,m)$hhiUse(sSec,b,m)..
    HHI(s,sSec,b,y,m) =e= hhiUse(sSec,b,m);

* Sea volumes are not restricted to primo level =e= ultimo level
SEAVOL(s,b,y,m)$sea(b)..
    VOL(s,b,y,m)  * scalem3 =e= (SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm))$(ord(m) gt 1) + STO(s,b,y,m) - SUM(bd$resv(bd,b), DIS(s,bd,b,y,m))) * scalem3;

* Reservoir is calculated at end of body and end of season
* In break month we introduce requirements for buildup or rundown of reservoir
RESVOL(s,b,y,m)$(bRes(b) and (not resVolFix(b,y,m))  )..
    ( VOL(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)) ) * scalem3$bResSto(b)
        =e=   (
                                           +STO(s,b,y,m) - SUM(bd$resv(bd,b), DIS(s,bd,b,y,m))
                                           -ctrfBuild*reservoirs(b,"max")$(mBgn(m) AND bResBuild(b))
                                          ) * scalem3;

* Maximum reservoir volume
RESVOLMAX(b,y,m)$bRes(b)..
    SUM(s$sW(s), VOL(s,b,y,m)) * scalem3 =l= (reservoirs(b,"max") * scalem3)$bResSto(b);

* Minimum reservoir volume
RESVOLMIN(b,y,m)$bRes(b)..
    SUM(s$sW(s), VOL(s,b,y,m)) * scalem3 =g= (reservoirs(b,"min") * scalem3)$bResSto(b);

* Maximal water flow
RESDISMAX(s,b,y,m)$(resDisMax0(b,m) gt 0 and not resVolFix(b,y,m))..
    SUM(bd$resv(bd,b), DIS(s,bd,b,y,m)) * scalem3 =l= scalem3 * resDisMax0(b,m);

* Discharges must be higher than a given minimun level
RESDISMIN(b,y,m)$resDisMin0(b,m)..
    SUM(s$sW(s), SUM(bd, DIS(s,bd,b,y,m))) * scalem3 =g= resDisMin0(b,m) * scalem3;

* Fix volumes in selected reservoirs in selected months (for baseline)
RESFIXVOL(s,b,y,m)$resVolFix(b,y,m)..
    VOL(s,b,y,m) * scalem3 =e= resVolFix(b,y,m) * scalem3;

* Fix discharges in selected reservoirs in selected months (for baseline)
* ### NB: Need to take account for fixed zero discharges - revert to scenario parameter for deciding whether to fix or not
RESFIXDIS(s,bd,bo,y,m)$resDisFix(bd,bo,y,m)..
    FLW(s,bd,bo,y,m) + DIS(s,bd,bo,y,m) * scalem3 =e= resDisFix(bd,bo,y,m) * scalem3;

* Reservoir unit electricity generation
* Filled reservoirs produce more per m3 than half empty
* OBS: Check how average volume is calcuated in the start and end months!
ELYHEAD(b,y,m)$bResEly(b)..
    SUM(g, HPP(b,g,y,m))
                =e= SUM(s$sW(s),  gravity * reservoirs(b,"coef_eta") * SUM(bd$resv(bd,b), DIS(s,bd,b,y,m))
                      * ( reservoirs(b,"coef_a")
                         +reservoirs(b,"coef_b") *      (VOL(s,b,y,m)+SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)))/2
                         +reservoirs(b,"coef_c") *POWER((VOL(s,b,y,m)+SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)))/2,2)
                        ) / 3.6 );

* Hydro power production (in MWh) by time segment is capped by hours in each segment according to maximum effect (in MW)
ELYHPP(b,g,y,m)$bResEly(b)..
    0.001*HPP(b,g,y,m) =l= 0.001 * 30 * reservoirs(b,"effect") * segmentHours(g);

* Thermal power production (in MWh) is also capped by hours in each segment according to maximum effect (in MW)
ELYTPP(u,g,y,m)$elyThermal("elyCap",u)..
    0.001 * TPP(u,g,y,m) =l= 0.001 * 30 * elyThermal("elyCap",u) * segmentHours(g);

* Themal and hydro power production should cover the energy demand in every segment
ELYMKT(g,y,m)..
    0.001 * ( SUM(u$elyThermal("elyCap",u), TPP(u,g,y,m)) + SUM(b$bResEly(b), HPP(b,g,y,m)) )
    =e=    0.001 * 30 * segmentHours(g) * SUM(c, elyDemand0(c,g,m));

* Maximum seasonal ground water extraction
GRNDWATER(b,y,mS)$grwtrMax(b,ms)..
    SUM(m$mSeason(m,ms), GRW(b,y,m)) * scalem3 =e= grwtrMax(b,ms) * scalem3;

* Monthly minimum inflow requirements
MININFLOW(b,y,m)$minInFlow0(b,m)..
    (SUM((bo,s)$sW(s), DIS(s,b,bo,y,m)$resv(b,bo)+FLW(s,b,bo,y,m)$flow(b,bo)+ITK(s,b,bo,y,m)$intk(b,bo))
   +SUM(s$sW(s), sup(s,b,m)) + rtn0(b,m)  ) * scalem3 =g= minInFlow0(b,m) * scalem3;

* Monthly minimum outflow requirements (only river nodes)
MINOUTFLOW(b,y,m)$(minInFlow0(b,m) AND bRiv(b))..
    SUM((bd,s)$sW(s), (ITK(s,bd,b,y,m)+FLW(s,bd,b,y,m))$flow(bd,b)) * scalem3 =g= minInFlow0(b,m) * scalem3;

* Annual minimum flow requirements
MININFLOWa(b,y,mA)$(minInFlow0(b,mA) + natExtra(b))..
    (SUM((m,s)$sW(s), SUM(bo, DIS(s,b,bo,y,m)$resv(b,bo)+FLW(s,b,bo,y,m)$flow(b,bo)+ITK(s,b,bo,y,m)$intk(b,bo)) + sup(s,b,m))
   +SUM(m, rtn0(b,m))) * scalem3 =g= (minInFlow0(b,mA) + natExtra(b)) * scalem3;

* Leontief production technology (perfect complements)
* Also correct for the fall in yield caused by the CET function.
LEONTIEF(b,j,y)$lnd0(b,j)..
    ( CRP(b,j,y) / crp0(b,j) ) =e= (LND(b,j,y)/lnd0(b,j)) * (1$jAX(j) + SUM(jj$lShare(b,jj), lShare(b,jj)*( CRP(b,jj,y) / crp0(b,jj) ) )$jAF(j) ) ;

* Enforce decreasing returns to scale from deviating away from base year land use patterns
CETLAND(b,y)$SUM(j$jAF(j), lnd0(b,j))..
    1 =e=  ( SUM(j$lShare(b,j), lShare(b,j)*( CRP(b,j,y) / crp0(b,j) )**rL )**(1/rL) ) ;
 
* Some crops have a fixed share of land
FIXEDLAND(b,j,y)$(lnd0(b,j) AND jAX(j))..
    ( CRP(b,j,y) / crp0(b,j) )$jAX(j)   =e= 1;

* Objective is the total value of water from electricity, industry and agriculture scaled to million USD
OBJ..   TWV =e=
                    SUM((b,j,y)$crp0(b,j),                      CRP(b,j,y) * pCrop(j)                                       * objAGRweight(b)    ) / 1000000
                   -SUM((b,j,y)$lnd0(b,j),                      LND(b,j,y) * cCrop(b,j)                                     * objAGRweight(b)    ) / 1000000
* CAUTION: The row below are operating costs depending on water use, e.g. power consumption - correct specification when receiving inv. data from Bekzhod
                   -SUM((b,j,y,m)$irg0(b,j,y,m),                IRG(b,j,y,m)*iInv("usdPerMM3")                              * objAGRweight(b)    ) / 1000000
* CAUTION: The irrigation investments are probably misspecified: they depend on exogenous land use, not actual - check this!!!
                   -SUM((b,j,y)$lnd0(b,j),                      LND(b,j,y) * iInv("usdPerHa")                               * objAGRweight(b)    ) / 1000000
                   -SUM((u,g,y,m)$elyThermal("elyCap",u),       TPP(u,g,y,m)*elyThermal("uFUELEX",u)                        * objELYweight       ) / 1000000
                   -SUM((b,y)$bRes(b),                          reservoirs(b,"cost")$(not bResNOP(b))                       * objELYweight       )
                   +SUM((b,g,y,m)$objHPPweight(b,m),            HPP(b,g,y,m)                                                * objHPPweight(b,m)  ) / 1000000
                   +SUM((s,bd,bo,j,y,m)$objDISweight(bo,m),     (DIS(s,bd,bo,y,m)+FLW(s,bd,bo,y,m))                         * objDISweight(bo,m) ) / 1000000
* Subtract a tiny amount from spill flows in reservoirs, so that DIS is preferred above FLW 
                   -SUM((s,bd,bo,y,m)$resv(bd,bo),              FLW(s,bd,bo,y,m) * 0.001                                                         ) / 1000000

;


MODEL beam  /   WBALANCE, SEAVOL, LOSSES, HHIUSES, GRNDWATER,
                RESVOL, RESFIXVOL, RESFIXDIS, RESDISMAX, RESVOLMAX, RESVOLMIN,
                MINOUTFLOW, MININFLOW, MININFLOWa,
                ELYHEAD, ELYHPP, ELYTPP, ELYMKT,
                LEONTIEF, CETLAND, FIXEDLAND, IRGACT, 
                OBJ
                /;



beam.optfile        = 1;
*beam.holdfixed     = yes;
beam.solprint       = no;
beam.tolinfrep      = 0.001;
beam.limrow         = 0;
beam.limcol         = 0;
beam.iterlim        = 50000;

IF(debugging,
    beam.limrow         = 5000;
    beam.limcol         = 0;
)
;

$ontext
* =============================================================================
* Fix unused variables and set levels
* =============================================================================


* Fix hydrological variables that can never be non-zero
FLW.fx(s,bd,bo,y,m)$(not flow(bd,bo))                       = 0;
ITK.fx(s,bd,bo,y,m)$(not intk(bd,bo))                       = 0;
IRG.fx(b,j,y,m)$(qAWater(b,j)*agriSeason(b,j,m) eq 0)       = 0;
DIS.fx(s,bd,bo,y,m)$(not resv(bd,bo))                       = 0;
GRW.fx(b,y,m)$(SUM(ms$mSeason(m,ms), grwtrMax(b,ms)) eq 0)  = 0;
HPP.fx(b,g,y,m)$(not bResEly(b))                            = 0;
STO.fx(s,bo,y,m)                                            = 0;
STO.up(s,b,y,m)$bRes(b)                                     = 99999;
STO.up(s,b,y,m)$sea(b)                                      = INF;

CRP.fx(b,j,y)$(qInputs(b,"land",j) eq 0)                    = 0;

* Lower and upper bounds
CRP.lo(b,j,y)$jAF(j)                                        = 0.01$lShare(b,j) * crp0(b,j);


TPP.up(u,g,y,m) = elyThermal("elyCap",u)*30*segmentHours(g);
$offtext