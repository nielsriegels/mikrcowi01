
PARAMETERS
    sup(s,b,m)                      "Base year supply of river water (mm3/month)"
    crp0(b,j)                       "Base year production agri/industrt quantities (ton/year)"
    iACosts(b,k,j)                  "Base year production agri/industrt input shares quantities (mn usd/year)"
    qValue(b,j)                     "Base year production agri/industrt prod. values (mn usd/year)"
    qALand(b,j)                     "Base year land use in agriculture (ha)"
    qAWater(b,j)                    "Base year water in agriculture (mm3/y)"
    qmWater(b,m)                    "Base year water (mm3/month)"
    irg0(b,j,y0,m0)                 "Base year water for irrigation (mm3/month)"
    hhiUse(sSec,b,m0)               "Base year water use for households and industries (mm3)"
    qInputs(b,k,j)                  "Base year physical input use (unit/year)"
    oCosts(b,j)                     "Base year production agri/industrt unit cost (usd/ton)"
    iShare(b,k,j)                   "Base year share of input costs (pct of total cost)"
    lShare(b,j)                     "Share of land for free crops in zone (%)"
    retShare0(bd,bo,*)              "Share of return water (%)"
    jAF(j)                          "Crops with flexible land allocation 0/1"
    jAX(j)                          "Crops with fixed land allocation 0/1"
    gwCorr(b,m)                     "Ground water correction (mm3/month)"
    loss(b,m)                       "Water loss from river (mm3/month)"
    pEly(g,y0,m0)                   "Electricity price (USD/MWh)"
    pCrop(j)                        "Crop prices (USD/ton)"
    pInput(k)                       "Index for input prices"
    resVolFix(b,y0,m)               "Fixed volume for reservoirs (mm3)"
    resDisFix(bd,bo,y0,m)           "Fixed discharges for reservoirs (mm3/month)"
    iInv(sInv)                      "Irrigation investment data"
    minInflow0(b,m0)                "Minimum inflow for nature (mm3/month)"
    resDisMax0(b,m0)                "Maximum reservoir discharge (mm3/month)"
    crp0(b,j)                       "Crop production base year (ton/year)"
    lnd0(b,j)                       "Land use in base year (ha)"
    cCrop(b,j)                      "Crop costs by land use (USD/ha)"
    checkoutput(*,sSce)             "Check of output values for no-change control"
    natExtra(b)                     "Nature extra water (mm3/year)"
    rFlow(sSce,b,e_resFlow,y0,m0)   "Flow of water through bodies"
    rAgri(sSce,b,e_resAgri,j,y0)    "Agricultural results for bodies"
    rEcon(sSce,b,e_resEcon,y0,m0)   "Economical results for bodies"
    rsFlow(sSce,bs,y0,e_resFlow,m0) "Flow of water through basin sections"
    rsAgri(sSce,bs,y0,e_resAgri,j)  "Agricultural results for basin sections"
    rcAgri(sSce,c,y0,e_resAgri,j)   "Agricultural results for countries"
    rcEcon(sSce,y0,e_resEcon,c)     "Economical results for countries"
    iMinLnd(b,j)                    "Index for minimum land use requirements for crops in procent of base"
    conveyanceCost(c)               "Cost of conveyance system"
    elyPump(bd,bo)                  "Energy required for conveyance (KWh/m^3)"
    wPhase(b,j,p0)                  "Weight for days in growth phase (% of growth days)"
    etcM2P(b,j,p0,m)                "Map for water use in phases and months (% of monthly water use in phase)"
    etcP(b,j,p0)                    "Optimal evapotranspiration by phase (mm3/ha)"
    etcM(b,j,m0)                    "Optimal evapotranspiration by month (mm3/ha)"
;

OPTIONS rEcon:2:2:2;
OPTIONS rFlow:2:2:2;
OPTIONS rAgri:2:2:2;
OPTIONS rsFlow:2:4:1;
OPTIONS rsAgri:2:4:1;
OPTIONS rcAgri:2:4:1;
OPTIONS rcEcon:2:3:1;
OPTIONS checkoutput:2:1:1;

* Provide baseline for check against former model runs - UPDATE WHEN CHANGE IN totinc IS WARRANTED!
checkoutput("totinc0","s01")   = 9890671.66 ;
checkoutput("totinc0","s02")   = 10164741.19;
checkoutput("obj0","s01")      = -20157.33;
checkoutput("obj0","s02")      = -19883.26;

* Yield response parameters (etc0 is in mm/day, so scale with x 10 / 1000000 to get to mm3/ha)
wPhase(b,j,p)       = ( phaseDays(b,p,j) / SUM(pp, phaseDays(b,pp,j)) )$phaseDays(b,p,j); 
etcP(b,j,p)         = 0.000001 * SUM(m, 10*etc0(b,p,m,j));
etcM(b,j,m)         = 0.000001 * SUM(p, 10*etc0(b,p,m,j))/1000000;
etcM2P(b,j,p,m)     = (etc0(b,p,m,j)/SUM(pp, etc0(b,pp,m,j)) )$SUM(pp, etc0(b,pp,m,j));

* TEMPORARY - replace Excel names to crp0, lnd0, irg0, cCrop0 and pCrop0
irg0(b,j,y0,m)                  = qAWater(b,j)*agriSeason(b,j,m);

* Temporary replacement for etc0
etcM(b,j,m)         = ( qAWater(b,j)*agriSeason(b,j,m) / qAland(b,j) )$qAland(b,j);
etcP(b,j,p)         = SUM(m, etcM2P(b,j,p,m) * etcM(b,j,m));
*etcM2P(b,j,p,m)     = ( etc0(b,p,m,j)/etcP(b,j,p) )$etcP(b,j,p);



* TEMPOARAY - refine YRW weights - fetch from Excel
crp0(b,j)                       = qAquant(b,j);
lnd0(b,j)                       = qAland(b,j);
cCrop(b,j)$lnd0(b,j)            = 1000000*SUM(k$(not kLimit(k)), iACosts(b,k,j))/lnd0(b,j);
*qValue(b,j)                     = SUM(k, iACosts(b,k,j));
*oCosts(b,j)$crp0(b,j)           = 1000000 * qValue(b,j) / crp0(b,j);

*iShare(b,k,j)$qValue(b,j)       = iACosts(b,k,j) / qValue(b,j);
*qInputs(b,"land",j)             = qALand(b,j);
*qInputs(b,"watr",j)             = qAWater(b,j);
* Set water supply to normal year (base year cannot be dry year, as base year agricultural water use is fixed at 2009 level)
*sup(s,b,m)                      = sup0(s,b,"2009",m);
pEly("base",y0,m)               = pEly0(y0,m);
pEly("peak",y0,m)               = pEly0(y0,m)*elyPeakFac;

* Ground water correction: Data on groundwater leaves too much water in some planning zones in some months, below is corrected for that
*qmWater(b,m)$bPlz(b)            = SUM(j, qAWater(b,j)*agriSeason(b,j,m)) + qIwater(b,m) + qHwater(b,m) - sup("W",b,m);
*gwCorr(b,m)                     = min(0,qmWater(b,m));

* Set maximum discharge capacity
resDisMax0(b,m)$bRes(b) = min( reservoirs(b,"capacity")*3600*24*30/1000000,
                              (reservoirs(b,"effect")*24*30/reservoirs(b,"ely"))$reservoirs(b,"ely")
                             )$bResEly(b) + INF$(not bResEly(b));
* Enable spillways in reservoirs (any overflow is found in variable FLW)
flow(bd,bo)$(SUM(m, resDisMax0(bo,m)) gt 0 and SUM(m, resDisMax0(bo,m)) lt INF)  = YES$resv(bd,bo);



* Temporary data for hours in each electricity market segment
PARAMETER segmentHours(g) / base 16, peak 8 /;

* Temporary data for objective weights
PARAMETER objDISweight(b,m)       "Constrained optimisation objective weights for discharges";
PARAMETER objHPPweight(b,m)       "Constrained optimisation objective weights for hydro power";
PARAMETER objELYweight            "Constrained optimisation objective weights for electricity";
PARAMETER objAGRweight(b)         "Constrained optimisation objective weights for agriculture";
objDISweight(b,m)   = 0;
objHPPweight(b,m)   = 0;
objELYweight        = 1;
objAGRweight(b)     = 1;