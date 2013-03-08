
PARAMETERS
    sup(s,b,m)              "Base year supply of river water (mm3/month)"
    qAQuant(b,j)            "Base year production agri/industrt quantities (ton/year)"
    iACosts(b,k,j)          "Base year production agri/industrt input shares quantities (mn usd/year)"
    qValue(b,j)             "Base year production agri/industrt prod. values (mn usd/year)"
    qALand(b,j)             "Base year land use in agriculture (ha)"
    qAWater(b,j)            "Base year water in agriculture (mm3/y)"
    qmWater(b,m)            "Base year water (mm3/month)"
    irg0(b,j,y0,m0)         "Base year water for irrigation (mm3/month)"
    hhiUse0(sSec,b,m0)      "Base year water use for households and industries (mm3)"
    qInputs(b,k,j)          "Base year physical input use (unit/year)"
    oCosts(b,j)             "Base year production agri/industrt unit cost (usd/ton)"
    iShare(b,k,j)           "Base year share of input costs (pct of total cost)"
    lShare(b,j)             "Share of land for free crops in zone (%)"
    retShare0(bd,bo,*)      "Share of return water (%)"
    jAF(j)                  "Crops with flexible land allocation 0/1"
    jAX(j)                  "Crops with fixed land allocation 0/1"
    gwCorr(b,m)             "Ground water correction (mm3/month)"
    loss(b,m)               "Water loss from river (mm3/month)"
    pEly(g,y0,m0)           "Electricity price (USD/MWh)"
    pCrop(j)                "Crop prices (USD/ton)"
    natExtra(b)             "Extra nature demands (mm3/year)"
    pInput(k)               "Index for input prices"
    rFixVol(b,m)            "Fixed volume for reservoirs (mm3)"
    rFixDis(bd,bo,m)        "Fixed discharges for reservoirs (mm3/month)"
    iInv(sInv)              "Irrigation investment data"
    minInflow0(b,m0)        "Minimum inflow for nature (mm3/month)"
    resDisMax0(b,m0)        "Maximum reservoir discharge (mm3/month)"
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
irg0(b,j,y,m)                   = qAWater(b,j)*agriSeason(b,j,m);
* Set water supply to normal year (base year cannot be dry year, as base year agricultural water use is fixed at 2009 level)
sup(s,b,m)                      = sup0(s,b,"2009",m);

hhiUse0("HH",b,m)               = qHwater(b,m);
hhiUse0("ind",b,m)              = qIwater(b,m);

pCrop(j)                        = pCrop0(j,"base");
natExtra(b)                     = 0;
pInput(k)                       = 1;
iInv(sInv)                      = 0;
minInflow0(b,m0)                = minInflowN(b,m0)$baseyear("2009");

* Ground water correction: Data on groundwater leaves too much water in some planning zones in some months, below is corrected for that
qmWater(b,m)$bPlz(b)            = SUM(j, qAWater(b,j)*agriSeason(b,j,m)) + qIwater(b,m) + qHwater(b,m) - sup("W",b,m);
gwCorr(b,m)                     = min(0,qmWater(b,m));

* Set maximum discharge capacity
resDisMax0(b,m)$bRes(b) = min( reservoirs(b,"capacity")*3600*24*30/1000000,
                              (reservoirs(b,"effect")*24*30/reservoirs(b,"ely"))$reservoirs(b,"ely")
                             )$bResEly(b) + INF$(not bResEly(b));
flow(bd,bo)$(SUM(m, resDisMax0(bo,m)) gt 0 and SUM(m, resDisMax0(bo,m)) lt INF)  = YES$resv(bd,bo);
OPTION flow:0:0:2;


* Temporary data for hours in each electricity market segment
PARAMETER segmentHours(g) / base 16, peak 8 /;