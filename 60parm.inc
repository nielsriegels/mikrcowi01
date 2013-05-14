PARAMETERS
    sup(s,b,m)                      "Base year supply of river water (mm3/month)"
    irg0(b,q0,j,y0,m0)              "Base year water for irrigation (mm3/month)"
    itk1(s,bd,y0,m0)             "Base year water for irrigation (mm3/month)"
    itk2(s,bd,y0,m0)             "Base year water for irrigation (mm3/month)"
    qLch(b)                         "Leaching requirement (m3/ha)"
    hhiUse(sSec,b,m0)               "Base year water use for households and industries (mm3)"
    lShare(b,q0,j)                  "Share of land for free crops in zone (%)"
    retShare0(bd,bo,*)              "Share of return water (%)"
    pEly(g,y0,m0)                   "Electricity price (USD/MWh)"
    pCrop(b,j)                      "Crop prices (USD/ton)"
    pInput(k)                       "Index for input prices"
    cCrop(b,j)                      "Crop costs by land use (USD/ha)"
    cmx2lnd(b,q0,j,x)               "Map between crop mix land use and crop land use"
* Irrigation system costs
    iInv(sInv)                      "Irrigation investment data"
    conveyanceCost(c)               "Cost of conveyance system"
    elyPump(bd,bo)                  "Energy required for conveyance (KWh/m^3)"
* Irrigation system physical assumptions
    resDisMax0(b,m0)                "Maximum reservoir discharge (mm3/month)"
    natExtra(b)                     "Nature extra water (mm3/year)"
    lndMin(b,j)                     "Minimum land use requirements for crops (1000 ha)"
    crpMax(c,j)                     "Maximum crop production due to demand constraints (1000 ton/year)"
    wPhase(b,j,p0)                  "Weight for days in growth phase (% of growth days)"
    etcM2P(b,j,p0,m)                "Map for water use in phases and months (% of monthly water use in phase)"
    etcP(b,j,p0)                    "Optimal evapotranspiration by phase (1000 m3/ha)"
    etcM(b,j,m0)                    "Optimal evapotranspiration by month (1000 m3/ha)"
    resVolFix(b,y0,m)               "Fixed volume for reservoirs (mm3)"
    resDisFix(bd,bo,y0,m)           "Fixed discharges for reservoirs (mm3/month)"
    nuk0(rb0,c,y0,m0)               "Nukus Allocation of water (mm3/bi-season)"
* Parameters for control of model scenarios
    jAF(j)                          "Crops with flexible land allocation 0/1"
    jAX(j)                          "Crops with fixed land allocation 0/1"
    maxHead(b)                      "Impose max head limits on selected reservoirs (meter)"
    objDISweight(b,m)               "Constrained optimisation objective weights for discharges"
    objHPPweight(b,m)               "Constrained optimisation objective weights for hydro power"
    objELYweight                    "Constrained optimisation objective weights for electricity"
    objAGRweight(b)                 "Constrained optimisation objective weights for agriculture"
    objNODEweight(b)                "Constrained optimisation objective weights for specific nodes (agri+resv. only)"
    objCTYweight(c)                 "Constrained optimisation objective weights for specific countries (tpp only)"
    noYWR                           "Disable yield-water response function (0/1)"
    useCET                          "Enable CET function for land yield response"
    useCMX                          "Enable Crop mix method"
    useNukus                        "Allocate water in accoradance with Nukus agreement"
    pXpt(c,m0)                      "Electricity export price extra CAPS (USD/MWh)"
    qXpt(c,m0)                      "Electricity export quantity extra CAPS (MW)"

* Parameters for reporting of results to Excel
    rFlow(sSce,b,e_resFlow,y0,m0)   "Flow of water through bodies"
    rcFlow(sSce,c,y0,e_resFlow,m0)  "Flow of water by country"
    rsFlow(sSce,bs,y0,e_resFlow,m0) "Flow of water through basin sections"
    rRbFlow(sSce,rb0,c,y0,*)        "Flow of water by basin in country" 
    rAgri(sSce,b,e_resAgri,y0,j,q0) "Agricultural results for zone parttions"
    rAgriNode(sSce,b,e_resAgri,y0)  "Agricultural results for planning zones"
    rAgriCrop(sSce,b,e_resAgri,y0,j) "Agricultural results for planning zones by crop type"
    rIrg(sSce,b,e_resIrg,y0,p,j,q0) "Irrigation results by zone, partition and growth phase"
    rEcon(sSce,b,e_resEcon,y0,m0)   "Economical results for zone"
    rsAgri(sSce,bs,y0,e_resAgri,j)  "Agricultural results for basin sections"
    rcAgri(sSce,c,y0,e_resAgri,j)   "Agricultural results for countries"
    rcEcon(sSce,y0,e_resEcon,c)     "Economical results for countries"
    rcView(sSce,y0,e_resView,c)     "Overview results summarised by scenario and country"
    rcRbView(sSce,y0,e_resView,rb0,c) "Overview results summarised by scenario, basin and country"
    rSView(y0,e_resView,sSce)          "Overview results summarised by scenario"       
    rcElec(sSce,e_resElec,y0,c,m0)  "Electricity market results"
    rcElecBal(sSce,y0,c,m0,e_resElec) "Electricity market balance bi-season"
    rBorder(sSce,y0,e_resBorder,m0) "Border flows (mm3)"
    checkoutput(*,sSce)             "Check of output values for no-change control"

    rRbAgri(sSce,y0,e_resAgri,rb0,c) "Agricultural results"
    rRbEcon(sSce,y0,*,rb0,c)         "Economic results"
    rResv(sSce,y0,*,b,m)            "Reservoir information"
    rRbResv(sSce,y0,*,rb0,c)       "Description of reservoirs on basin level"
    rcPublic(sSce,y0,e_resPublic,c,e_resPubItems)   "Public finance output"

* Temporary - to be replaced or deleted
    trx0(c,cc)
    transx(c,cc)

rItk(b,sSce,*)
rPlz(b,sSce,*)
rLos(b,sSce);
;

OPTIONS itk1:0:2:2;
OPTIONS itk2:0:2:2;

OPTIONS crp0:0:2:1;
OPTIONS lnd0:0:2:1;
OPTIONS nuk0:0:2:2;
OPTIONS etcM2P:2:3:1;
OPTIONS etc0:2:3:1;
OPTIONS rEcon:2:2:2;
OPTIONS rFlow:2:2:2;
OPTIONS rAgri:2:2:2;
OPTIONS rsFlow:2:4:1;
OPTIONS rsAgri:2:4:1;
OPTIONS rcAgri:2:4:1;
OPTIONS rcEcon:2:3:1;
OPTIONS rcRbView:0:3:2;
OPTIONS rcPublic:0:3:2;
OPTIONS checkoutput:2:1:1;

* Provide baseline for check against former model runs - UPDATE WHEN CHANGE IN totinc IS WARRANTED!
checkoutput("totinc0","s01")   = 9890671.66 ;
checkoutput("totinc0","s02")   = 10164741.19;
checkoutput("obj0","s01")      = -20157.33;
checkoutput("obj0","s02")      = -19883.26;

* Yield response parameters (etc0 is in mm/month = 10m3/ha/day, so scale with x 10 / 1000 to get to 1000 m3/ha/month) 
* etcP and etcM is in 1000 m3/ha/month
* irg0 is in mm3/month 
wPhase(b,j,p)       = ( phaseDays(b,p,j) / SUM(pp, phaseDays(b,pp,j)) )$phaseDays(b,p,j);
etcP(b,j,p)         = SUM(m, 10*etc0(b,p,m,j))/1000;
etcM(b,j,m)         = SUM(p, 10*etc0(b,p,m,j))/1000;
etcM2P(b,j,p,m)     = (etc0(b,p,m,j)/SUM(pp, etc0(b,pp,m,j)) )$SUM(pp, etc0(b,pp,m,j));
irg0(b,q0,j,y0,m)   = etcM(b,j,m) * lnd0(b,q0,j) / 1000;

itk1(s,bd,y0,m0) = 0;
itk2(s,bd,y0,m0) = 0;

* TEMPORARY map between crops and crop mixes, move to Excel when we implement 2nd crops
SET mapCMX(x,j) "Map between crops and crop mixes" / cotn.cot, whet.wht, rice.ric, ric2.ri2, alfa.alf, stfr.stf, tgrp.tgr, meln.mln, tomt.tmt, shav.shv, pota.pot, mung.mng, subt.sbt, mazf.mzf, mazg.mzg / ;
cmx2lnd(b,q0,j,x)$mapCMX(x,j) = ( lnd0(b,q0,j) / SUM(jj$mapCMX(x,jj), lnd0(b,q0,jj)) )$SUM(jj$mapCMX(x,jj), lnd0(b,q0,jj)) ;

* Set all nodes to be member of All basins
bBas(b,"All") = 1;

* set all crops free
jAF0(j) = 1;
jAF0("stf") = 0;
JAX(j) = 0;
resVolFix(b,y0,m) = 0;
resDisFix(bd,bo,y0,m) = 0;

nuk0(rb,c,y0,mS) = SUM((s,b)$bBas(b,rb), SUM(m$mSm(m,mS), sup0(s,b,y0,m))) * sNukus(c,rb);