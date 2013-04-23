POSITIVE VARIABLES
    GRW(bd,y0,m0)           "Ground water extraction (mm3)"
    IRG(b,q0,j,y0,m0)       "Actual irrigation (mm3)"
    YWR(b,q0,j,y0)          "Yield-water response function (index: 1=optimal)"
    iET(b,q0,j,p,y0)        "Index for actual ET relative to optimal ET (index: 1=optimal)"
    CMX(b,q0,x,y0)          "Crop mix land use (ha)"
    CRP(b,q0,j,y0)          "Crop production (ton/year)"
    LND(b,q0,j,y0)          "Land use by crop and suitability class (ha)"
    HHI(s,sSec,b,y0,m0)     "Water use by households and industries (mm3)"
    LOS(s,b,l,y0,m0)        "Losses to seepage and evaporation (mm3)"
    ITK(s,bd,bo,y0,m0)      "Water intakes from river nodes to zones (mm3)"
    FLW(s,bd,bo,y0,m0)      "Flows between river nodes (mm3)"
    VOL(s,b,y0,m0)          "Reservoir volume levels end of season (mm3)"
    DIS(s,bd,bo,y0,m0)      "Reservoir seasonal discharge (mm3)"
    STO(s,b,y0,m0)          "Reservoir seasonal storage buildup (mm3)"
    HHD(b,y0,m0)            "Hydro power reservoir head (m)"
    HPP(b,g0,y0,m0)         "Hydro power production (GWh)"
    TPP(c,u,g0,y0,m0)       "Thermal power production (GWh)"
    USD(g0,c,d0,y0,m0)      "Unserved demand (GWh)"
    DEM(g0,c,y0,m0)         "Served demand (GWh)"
    TRX(c,cc,g0,y0,m0)      "Export of electricity from c to cc (GWh)"
    RTN(s,bd,bo,y0,m0)      "Return of water from irrigating the fields (mm3)"
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
OPTIONS TPP:0:4:1;
OPTIONS USD:0:4:1;
OPTIONS LND:0:2:2;
OPTIONS CRP:0:2:2;
OPTIONS CMX:0:2:2;
OPTIONS YWR:4:2:2;
OPTIONS iET:4:3:2;

EQUATIONS
* Hydrological constraints and identities
    WBALANCE(s,b,y0,m0)         "Water balance at all nodes"
    GRNDWATER(b,y0,mS)          "Seasonal limits on groundwater extraction"
    LOSSES(s,b,l,y0,m0)         "Losses to seepage and evaporation"
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
    ELYHPP(b,y0,m0)             "Hydro power production"
    ELYSEG(b,g0,y0,m0)          "Hydro power production by segment"
    ELYTPP(c,u,g0,y0,m0)        "Thermal power production"
    ELYDEM(c,g0,y0,m0)          "Electricity demand"
    ELYUSD(g0,c,d0,y0,m0)       "Unserved demand steps"
    ELYTRX(c,cc,g0,y0,m0)       "Export of electricity from c to cc"
    ELYMKT(c,g,y0,m0)           "Power market balance"
    MININFLOW(b,y0,m0)          "Minimum inflow to body"
    MINOUTFLOW(b,y0,m0)         "Minimum outflow from body"
    MININFLOWa(b,y0,m0)         "Minimum inflow to body, annual"
    MINLANDUSE(b,j,y0)          "Annual minimum land use requirements"
    RETURNWATER(s,b,y0,m0)      "Calculate the returnwater"
* Economical constraints and identities
    IRGACT(b,q0,j,y0,m0)        "Actual irrigation need by zone"
    YIELDWATER1(b,q0,j,y0)      "Yield water response function"
    YIELDWATER2(b,q0,j,y0,m0)   "Yield water response function upper limit"
    iEVAPOTRANS(b,q0,j,p,y0)    "Calculate index for actual ET ratio to optimal ET"
    LEONTIEF(b,q0,j,y0)         "Ratio between outputs and inputs"
    CETLAND(b,y0)               "Index for physical amount of land"
    FIXEDLAND(b,q0,j,y0)        "Fixed land shares"
    LANDLIMIT(b,y0)             "Maximum land use"
    AGRCMX(b,q0,j,y0)           "Convert crop mix land use to crop land use"
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
   + SUM((q,j)$irg0(b,q,j,y,m), IRG(b,q,j,y,m))
   + SUM(sSec$HHIuse(sSec,b,m), HHI(s,sSec,b,y,m))
   + LOS(s,b,"seep",y,m)

   ) =e= (
       SUM(bo$intk(b,bo), ITK(s,b,bo,y,m))
      +SUM(bo$resv(b,bo), DIS(s,b,bo,y,m))
      +SUM(bo$flow(b,bo), FLW(s,b,bo,y,m))
      +sup(s,b,m)
      +rtn0(b,m)
      +GRW(b,y,m)$SUM(ms, grwtrMax(b,ms))
     ) * scalem3
;

* Account for losses to seepage and evaporation
LOSSES(s,b,l,y,m)$(lossRivers(b,l,m) + lossChannels(b,l,m))..
    LOS(s,b,l,y,m) =e= lossRivers(b,l,m) * (
                          SUM(bo$flow(b,bo), FLW(s,b,bo,y,m))
                        + SUM(bo$resv(b,bo), DIS(s,b,bo,y,m))
                        + sup(s,b,m)
                        + rtn0(b,m) )
                        + lossChannels(b,l,m)* (
                          SUM(bo$intk(b,bo), ITK(s,b,bo,y,m)) )
;

* Account for return of water due to drainage
RETURNWATER(s,b,y,m)..
    SUM(bo$(intk(b,bo)+resv(b,bo)+flow(b,bo)),RTN(s,bo,b,y,m))
                 =e= 0.0*SUM((q,j),IRG(b,q,j,y,m));

* Irrigation needs - irg0 is base year irrigation,
* Not used when we enable Yield-Water response
IRGACT(b,q,j,y,m)$(irg0(b,q,j,y,m) AND noYWR and useCET)..
    IRG(b,q,j,y,m) =e= (LND(b,q,j,y)/lnd0(b,q,j))*irg0(b,q,j,y,m) * (1-iInv("improve"));

* Yield water response function
* Summed over growth phases with appropriate phase weights and phase specific crop yield
YIELDWATER1(b,q,j,y)$((not noYWR) AND lnd0(b,q,j))..
*    YWR(b,q,j,y) =e= 1 - SUM(p$etcP(b,j,p),  wPhase(b,j,p) * (1 - kYield(p,j) * iET(b,q,j,p,y))  );
    YWR(b,q,j,y) =e=     SUM(p$etcP(b,j,p),  wPhase(b,j,p) * (1 - kYield(p,j)*(1-iET(b,q,j,p,y)))  );

* TEMPORARY equation for eva / evc index
iEVAPOTRANS(b,q,j,p,y)$(lnd0(b,q,j) AND etcP(b,j,p) AND (not noYWR) )..
    iET(b,q,j,p,y) =e= SUM(m, etcM2P(b,j,p,m)*IRG(b,q,j,y,m)) / (etcP(b,j,p)*LND(b,q,j,y))  ;




* Ensure that there is no excess watering (to not boost YWR above 1)
YIELDWATER2(b,q,j,y,m)$((not noYWR) AND lnd0(b,q,j))..
    IRG(b,q,j,y,m) =l= etcM(b,j,m) * LND(b,q,j,y) ;

* Use of water by households and industries
HHIUSES(s,sSec,b,y,m)$hhiUse(sSec,b,m)..
    HHI(s,sSec,b,y,m) =e= hhiUse(sSec,b,m);

* Sea volumes are not restricted to primo level =e= ultimo level
SEAVOL(s,b,y,m)$sea(b)..
    VOL(s,b,y,m)  * scalem3 =e= (SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm))$(not mBgn(m)) + STO(s,b,y,m) - SUM(bd$resv(bd,b), DIS(s,bd,b,y,m))) * scalem3;

* Reservoir is calculated at end of body and end of season
* In break month we introduce requirements for buildup or rundown of reservoir
RESVOL(s,b,y,m)$(bRes(b) and (not resVolFix(b,y,m))  )..
    ( VOL(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)) ) * scalem3$bResSto(b)
        =e=   (
                                           +STO(s,b,y,m) - SUM(bd$resv(bd,b), DIS(s,bd,b,y,m)+FLW(s,bd,b,y,m))
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

* Reservoir unit electricity generation - transform from MWh to GWh
* Filled reservoirs produce more per m3 than half empty
* OBS: Check how average volume is calcuated in the start and end months!
ELYHEAD(b,y,m)$bResEly(b)..
    HHD(b,y,m)  =e= SUM(s$sW(s),
                          reservoirs(b,"coef_a")
                         +reservoirs(b,"coef_b") *      (VOL(s,b,y,m)+SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)))/2
                         +reservoirs(b,"coef_c") *POWER((VOL(s,b,y,m)+SUM((yy,mm)$mLast(y,m,yy,mm), VOL(s,b,yy,mm)))/2,2)
                        )  ;

* Hydro power production (in GWh) by time segment is capped by hours in each segment according to maximum effect (in MW)
ELYSEG(b,g,y,m)$bResEly(b)..
    HPP(b,g,y,m) =l= 0.001 * 30 * reservoirs(b,"effect") * SUM(c$bCty(b,c), elyHours(c,g,m)) ;

* HPP production is 1 kg * m * m/s^2 = 1 Joule
* Since we use million m3 (=billion kg) as weight unit, the energy unit is GJ
* We convert to GWh using 1 GWh = 3.6 TJ
ELYHPP(b,y,m)$bResEly(b)..
    SUM(g, HPP(b,g,y,m)) =e= 0.001 * gravity * reservoirs(b,"coef_eta") * SUM((s,bd)$resv(bd,b), DIS(s,bd,b,y,m)$sW(s)) * HHD(b,y,m) / 3.6;

* Thermal power production (in MWh) is also capped by hours in each segment according to maximum effect (in MW)
ELYTPP(c,u,g,y,m)$(elyThermal("elyCap",u) and tppMap(c,u))..
    TPP(c,u,g,y,m) =l= 0.001 * 30 * elyThermal("elyCap",u) * elyHours(c,g,m);

* Thermal power production (in MWh) is also capped by hours in each segment according to maximum effect (in MW)
ELYUSD(g,c,d,y,m)$usd0(c,g,d)..
    USD(g,c,d,y,m) =l= 0.001 * 30 * elyHours(c,g,m) * elyDemand0(c,g,m) * usdSh(d);

* Electricity demand
ELYDEM(c,g,y,m)$elyDemand0(c,g,m)..
    DEM(g,c,y,m)   =e= 0.001 * 30 * elyHours(c,g,m) * elyDemand0(c,g,m) ;

* Themal and hydro power production should cover the energy demand in every segment
* Note: We might want to enter availability of TPP as a parameter which can increase
*       if trade possibilities makes the plant more attractive
* RHS is supply, LHS is demand, unit is GWh
ELYMKT(c,g,y,m)..
           SUM(cc$trx0(cc,c),       TRX(cc,c,g,y,m) )
         + SUM(u$tppMap(c,u),       TPP(c,u,g,y,m))
         + SUM(b$bResEly(b),        HPP(b,g,y,m)$bCty(b,c))
         + SUM(d$usd0(c,g,d),       USD(g,c,d,y,m))
    =e=
           DEM(g,c,y,m)
         + SUM((s,bd,bo),           0.001*ITK(s,bd,bo,y,m)* elyPump(bd,bo))
         + SUM(cc$trx0(c,cc),       TRX(c,cc,g,y,m) );

* Transmission constraints between countries (trx0 in MW, TRX in GW)
ELYTRX(c,cc,g,y,m)$trx0(c,cc)..
    TRX(c,cc,g,y,m) =l= 0 * 0.001 * 30 *trx0(c,cc) * elyHours(c,g,m)$transx;

* Maximum seasonal ground water extraction
GRNDWATER(b,y,mS)$grwtrMax(b,ms)..
    SUM(m$mSeason(m,ms), GRW(b,y,m)) * scalem3 =e= grwtrMax(b,ms) * scalem3;

* Monthly minimum inflow requirements
MININFLOW(b,y,m)$minInFlow0(b,y,m)..
    (SUM((bo,s)$sW(s), DIS(s,b,bo,y,m)$resv(b,bo)+FLW(s,b,bo,y,m)$flow(b,bo)+ITK(s,b,bo,y,m)$intk(b,bo))
   +SUM(s$sW(s), sup(s,b,m)) + rtn0(b,m)  ) * scalem3 =g= minInFlow0(b,y,m) * scalem3;

* Monthly minimum outflow requirements (only river nodes)
MINOUTFLOW(b,y,m)$(minInFlow0(b,y,m) AND bRiv(b))..
    SUM((bd,s)$sW(s), (ITK(s,bd,b,y,m)+FLW(s,bd,b,y,m))$flow(bd,b)) * scalem3 =g= minInFlow0(b,y,m) * scalem3;

* Annual minimum flow requirements
MININFLOWa(b,y,mA)$(minInFlow0(b,y,mA) + natExtra(b))..
    (SUM((m,s)$sW(s), SUM(bo, DIS(s,b,bo,y,m)$resv(b,bo)+FLW(s,b,bo,y,m)$flow(b,bo)+ITK(s,b,bo,y,m)$intk(b,bo)) + sup(s,b,m))
   +SUM(m, rtn0(b,m))) * scalem3 =g= (minInFlow0(b,y,mA) + natExtra(b)) * scalem3;

* Annual minimum land use requirements
MINLANDUSE(b,j,y)..
SUM(q, LND(b,q,j,y)) =g= SUM(q, lnd0(b,q,j)* iMinLnd(b,j));

* Leontief production technology (perfect complements)
* Also correct for the fall in yield caused by the CET function.
LEONTIEF(b,q,j,y)$lnd0(b,q,j)..
    ( CRP(b,q,j,y) / crp0(b,q,j) ) =e=
        YWR(b,q,j,y) * (LND(b,q,j,y)/lnd0(b,q,j)) * (1$jAX(j)
                                                     +useCET*SUM(jj$lShare(b,q,jj), lShare(b,q,jj)*( CRP(b,q,jj,y) / crp0(b,q,jj) ) )$jAF(j)
                                                     +1$((not useCET) AND jAF(j))
                                                     ) ;

* Enforce decreasing returns to scale from deviating away from base year land use patterns
CETLAND(b,y)$(SUM((q,j)$jAF(j), lnd0(b,q,j)) AND useCET)..
    1$useCET =e=  ( SUM((j,q)$lShare(b,q,j), lShare(b,q,j)*( CRP(b,q,j,y) / crp0(b,q,j) )**rL )**(1/rL) ) ;

* Some fixed crops have a fixed share of land
FIXEDLAND(b,q,j,y)$(lnd0(b,q,j) AND jAX(j))..
    ( LND(b,q,j,y) / lnd0(b,q,j) )$jAX(j)   =e= 1;

* Land allocation from crop mix to land use
* (crop mix covers a whole year, while crop mix may have 2nd crops
*  so land use can exceed available land, while crop mix use cannot)
AGRCMX(b,q,j,y)$(lnd0(b,q,j))..
    LND(b,q,j,y) =e= SUM(x$cmx2lnd(b,j,x), cmx2lnd(b,j,x)*CMX(b,q,x,y));

* Total land use for crop mixes need to stay at or below available land use
LANDLIMIT(b,y)$SUM((q,j), lnd0(b,q,j))..
    SUM((q,x), CMX(b,q,x,y))$useCMX
   +SUM((q,j), LND(b,q,j,y))$(not useCMX)
    =l= SUM((q,j), lnd0(b,q,j));

* Objective is the total value of water from electricity, industry and agriculture scaled to million USD
OBJ..   TWV =e=
                SUM(b$objNODEweight(b),
                    SUM((q,j,y)$crp0(b,q,j),                  CRP(b,q,j,y) * pCrop(j)                                     * objAGRweight(b)    ) / 1000000
                   -SUM((q,j,y)$lnd0(b,q,j),                  LND(b,q,j,y) * cCrop(b,j)                                   * objAGRweight(b)    ) / 1000000
* CAUTION: The row below are operating costs depending on water use, e.g. power consumption - correct specification when receiving inv. data from Bekzod
                   -SUM((q,j,y,m)$irg0(b,q,j,y,m),            IRG(b,q,j,y,m)*iInv("usdPerMM3")                            * objAGRweight(b)    ) / 1000000
* CAUTION: The irrigation investments are probably misspecified: they depend on exogenous land use, not actual - check this!!!
                   -SUM((q,j,y)$lnd0(b,q,j),                  LND(b,q,j,y) * iInv("usdPerHa")                             * objAGRweight(b)    ) / 1000000
                   -SUM((y)$bRes(b),                          reservoirs(b,"cost")$(not bResNOP(b))                       * objELYweight       )
              )+SUM(c$objCTYweight(c),
                   -SUM((u,g,y,m)$tppMap(c,u),                TPP(c,u,g,y,m)*elyThermal("uFUELEX",u)*1000                 * objELYweight       ) / 1000000
                   -SUM((d,g,y,m)$usd0(c,g,d),                USD(g,c,d,y,m)*usd0(c,g,d)*1000                             * objELYweight       ) / 1000000
* CAPEX and OPEX expense due to conveyance system
                   -conveyanceCost(c)
              )
* Constrained optimisation for HPP and discharges
                   +SUM((b,g,y,m)$objHPPweight(b,m),            HPP(b,g,y,m)                                                * objHPPweight(b,m)  ) / 1000000
                   +SUM((s,bd,bo,j,y,m)$objDISweight(bo,m),     (DIS(s,bd,bo,y,m)+FLW(s,bd,bo,y,m))                         * objDISweight(bo,m) ) / 1000000
* Subtract a tiny amount from spill flows in reservoirs, so that DIS is preferred above FLW
                   -SUM((s,bd,bo,y,m)$resv(bd,bo),              FLW(s,bd,bo,y,m) * 0.001                                                         ) / 1000000
;


MODEL beam  /   WBALANCE, SEAVOL, LOSSES, HHIUSES, GRNDWATER,
                RESVOL, RESFIXVOL, RESFIXDIS, RESDISMAX, RESVOLMAX, RESVOLMIN,
                MINOUTFLOW, MININFLOW, MININFLOWa, MINLANDUSE,
                ELYHEAD, ELYHPP, ELYSEG, ELYTPP, ELYUSD, ELYDEM, ELYMKT, ELYTRX
                LEONTIEF, CETLAND, FIXEDLAND, LANDLIMIT, AGRCMX, IRGACT, YIELDWATER1, YIELDWATER2, iEVAPOTRANS, RETURNWATER
                OBJ
                /;

* Set model options
beam.solprint       = no;
beam.tolinfrep      = 0.001;
beam.limrow         = 0;
beam.limcol         = 0;
beam.iterlim        = 50000;
