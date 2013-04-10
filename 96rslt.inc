* Reservoir flows
    rFlow(sSce,b,"ResVol",y,m)                       =  SUM(s$sW(s),     VOL.l(s,b,y,m));
    rFlow(sSce,b,"ResVolAvg",y,m)                    =  SUM(s$sW(s),     VOL.l(s,b,y,m) + SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) ) / 2 ;
    rFlow(sSce,b,"ResChg",y,m)                       =  SUM(s$sW(s),     VOL.l(s,b,y,m) - SUM((yy,mm)$mLast(y,m,yy,mm), VOL.l(s,b,yy,mm)) );
    rFlow(sSce,b,"ResChg",y,m)$bResBuild(b)          = -SUM(s$sW(s),    -STO.l(s,b,y,m) + SUM(bd, DIS.l(s,bd,b,y,m) + FLW.l(s,bd,b,y,m)) );
    rFlow(sSce,b,"ResChg",y,m)$(sea(b) and mBgn(m))  =  SUM(s$sW(s),     VOL.l(s,b,y,m));

* Inflows
    rFlow(sSce,b,"InSrc",y,m)                        =  SUM(s$sW(s),         sup(s,b,m));
    rFlow(sSce,b,"InGrw",y,m)                        =                       GRW.l(b,y,m);
    rFlow(sSce,b,"InDis",y,m)                        =  SUM((s,bo)$sW(s),    DIS.l(s,b,bo,y,m));
    rFlow(sSce,b,"InFlw",y,m)                        =  SUM((s,bo)$sW(s),    FLW.l(s,b,bo,y,m));
    rFlow(sSce,b,"InItk",y,m)                        =  SUM((s,bo)$sW(s),    ITK.l(s,b,bo,y,m));
    rFlow(sSce,b,"InRtn",y,m)                        = rtn0(b,m);
    rFlow(sSce,b,"InTot",y,m)                        = rFlow(sSce,b,"InSrc",y,m) + rFlow(sSce,b,"InDis",y,m) + rFlow(sSce,b,"InFlw",y,m) 
                                                      +rFlow(sSce,b,"InItk",y,m) + rFlow(sSce,b,"InRtn",y,m) + rFlow(sSce,b,"InGrw",y,m);
* Outflows
    rFlow(sSce,b,"OutHHI",y,m)                       = -SUM((s,sSec)$sW(s),  HHI.l(s,sSec,b,y,m));
    rFlow(sSce,b,"OutIrg",y,m)                       = -SUM(j,               IRG.l(b,j,y,m));
    rFlow(sSce,b,"OutDem",y,m)                       = -SUM(j,               IRG.l(b,j,y,m)) - SUM((s,sSec)$sW(s), HHI.l(s,sSec,b,y,m));
    rFlow(sSce,b,"OutSto",y,m)                       = -SUM(s$sW(s),         STO.l(s,b,y,m));
    rFlow(sSce,b,"OutDis",y,m)                       = -SUM((s,bd)$sW(s),    DIS.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(sSce,b,"OutFlw",y,m)                       = -SUM((s,bd)$sW(s),    FLW.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(sSce,b,"OutInt",y,m)                       = -SUM((s,bd)$sW(s),    ITK.l(s,bd,b,y,m)$(not sea(bd)) );
    rFlow(sSce,b,"OutLak",y,m)                       = -SUM((s,bd)$sW(s),    (ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)+DIS.l(s,bd,b,y,m))$sea(bd)  );
    rFlow(sSce,b,"OutLos",y,m)                       = -SUM((s,l)$sW(s),                LOS.l(s,b,l,y,m));
    rFlow(sSce,b,"OutTot",y,m)                       =  rFlow(sSce,b,"OutDem",y,m) + rFlow(sSce,b,"OutDis",y,m) + rFlow(sSce,b,"OutFlw",y,m) 
                                                       +rFlow(sSce,b,"OutInt",y,m) + rFlow(sSce,b,"OutLos",y,m) + rFlow(sSce,b,"OutLak",y,m);
* Balance
    rFlow(sSce,b,"Balance",y,m)                      =  round(rFlow(sSce,b,"InTot",y,m)+rFlow(sSce,b,"OutTot",y,m)-rFlow(sSce,b,"ResChg",y,m)$bRes(b), 3) ;
    rFlow(sSce,b,e_resFlow,y,"annual")               =  SUM(m,            rFlow(sSce,b,e_resFlow,y,m));
    rFlow(sSce,b,"ResVol",y,"annual")                =  rFlow(sSce,b,"ResVol",y,"annual")  + 0.000001;
    rFlow(sSce,b,"Balance",y,"annual")               =  rFlow(sSce,b,"Balance",y,"annual") + 0.000001;

* Sum up for basin sections 
* - zero out internal flows within the basin section, 
* - calculate cross basin section flows
    rsFlow(sSce,bs,y,e_resFlow,m)                   = SUM(b$bSec(b,bs),     rFlow(sSce,b,e_resFlow,y,m));
    rsFlow(sSce,bs,y,"ResVol",m)                    = SUM((b,s)$sW(s),      VOL.l(s,b,y,m)$(bSec(b,bs) and not sea(b)) );
    rsFlow(sSce,bs,y,"ResChg",m)                    = SUM(b$bSec(b,bs),     rFlow(sSce,b,"ResChg",y,m)$(not sea(b)) );
    rsFlow(sSce,bs,y,"InDis",m)                     = SUM((s,bd,bo)$sW(s),  DIS.l(s,bd,bo,y,m)$sXsecIn(bs,bd,bo));
    rsFlow(sSce,bs,y,"InFlw",m)                     = SUM((s,bd,bo)$sW(s),  FLW.l(s,bd,bo,y,m)$sXsecIn(bs,bd,bo));
    rsFlow(sSce,bs,y,"InItk",m)                     = SUM((s,bd,bo)$sW(s),  ITK.l(s,bd,bo,y,m)$sXsecIn(bs,bd,bo));
    rsFlow(sSce,bs,y,"InTot",m)                     = rsFlow(sSce,bs,y,"InSrc",m) + rsFlow(sSce,bs,y,"InDis",m) + rsFlow(sSce,bs,y,"InFlw",m) 
                                                     +rsFlow(sSce,bs,y,"InItk",m) + rsFlow(sSce,bs,y,"InRtn",m) + rsFlow(sSce,bs,y,"InGrw",m);

    rsFlow(sSce,bs,y,"OutSto",m)                    = -SUM((s,bo)$sW(s),     STO.l(s,bo,y,m)$SUM(bd, sXsecOut(bs,bd,bo)) );
    rsFlow(sSce,bs,y,"OutDis",m)                    = -SUM((s,bd,bo)$sW(s),  DIS.l(s,bd,bo,y,m)$sXsecOut(bs,bd,bo));
    rsFlow(sSce,bs,y,"OutFlw",m)                    = -SUM((s,bd,bo)$sW(s),  FLW.l(s,bd,bo,y,m)$sXsecOut(bs,bd,bo));
    rsFlow(sSce,bs,y,"OutLak",m)                    = -SUM((s,bo)$sW(s),     STO.l(s,bo,y,m)$(bSec(bo,bs) AND sea(bo)) );
    rsFlow(sSce,bs,y,"OutInt",m)                    = -SUM((s,bd,bo)$sW(s),  ITK.l(s,bd,bo,y,m)$sXsecOut(bs,bd,bo));
    rsFlow(sSce,bs,y,"OutTot",m)                    =  rsFlow(sSce,bs,y,"OutDem",m) + rsFlow(sSce,bs,y,"OutDis",m) + rsFlow(sSce,bs,y,"OutFlw",m) 
                                                      +rsFlow(sSce,bs,y,"OutInt",m) + rsFlow(sSce,bs,y,"OutLos",m) + rsFlow(sSce,bs,y,"OutLak",m);

    rsFlow(sSce,bs,y,"TotDis",m)                    = -SUM((s,bd,bo)$(sW(s) and bSec(bo,bs)),  DIS.l(s,bd,bo,y,m)$(not bResNOP(bo)));

    
    rsFlow(sSce,bs,y,"Balance",m)                   =  round(rsFlow(sSce,bs,y,"InTot",m)+rsFlow(sSce,bs,y,"OutTot",m)-rsFlow(sSce,bs,y,"ResChg",m), 3) ;
    rsFlow(sSce,bs,y,e_resFlow,"annual")            =  SUM(m,            rsFlow(sSce,bs,y,e_resFlow,m));
    rsFlow(sSce,bs,y,"ResVol","annual")             =  rsFlow(sSce,bs,y,"ResVol","annual")  + 0.000001;
    rsFlow(sSce,bs,y,"Balance","annual")            =  rsFlow(sSce,bs,y,"Balance","annual") + 0.000001;
    
* Sum up for countries
* - zero out internal flows within the basin section, 
* - calculate cross basin section flows
    rcFlow(sSce,c,y,e_resFlow,m)                    = SUM(b$bCty(b,c),     rFlow(sSce,b,e_resFlow,y,m));
    rcFlow(sSce,c,y,"TotDis",m)                     = -SUM((s,bd,bo)$(sW(s) and bCty(bo,c)),  DIS.l(s,bd,bo,y,m)$(not bResNOP(bo)));
    rcFlow(sSce,c,y,"Balance",m)                    =  round(rcFlow(sSce,c,y,"InTot",m)+rcFlow(sSce,c,y,"OutTot",m)-rcFlow(sSce,c,y,"ResChg",m), 3) ;
    rcFlow(sSce,c,y,e_resFlow,"annual")             =  SUM(m,            rcFlow(sSce,c,y,e_resFlow,m));
    rcFlow(sSce,c,y,"ResVol","annual")              =  rcFlow(sSce,c,y,"ResVol","annual")  + 0.000001;
    rcFlow(sSce,c,y,"Balance","annual")             =  rcFlow(sSce,c,y,"Balance","annual") + 0.000001;
    



* Agricultural line results
    rAgri(sSce,b,"WaterUse",j,y)                     = SUM(m, IRG.l(b,j,y,m));
    rAgri(sSce,b,"LandEff",j,y)$bPlz(b)              = 0;
    rAgri(sSce,b,"LandEff",j,y)$(not jAF(j))         = 0;
    rAgri(sSce,b,"LandUse",j,y)                      = LND.l(b,j,y);
    rAgri(sSce,b,"AgriSales",j,y)                    =  1000 * CRP.l(b,j,y) * pCrop(j)   / 1000000;
    rAgri(sSce,b,"AgriCosts",j,y)                    = -1000 * LND.l(b,j,y) * cCrop(b,j) / 1000000;
    rAgri(sSce,b,"IrrInvCost",j,y)                   = -1000 * ( iInv("usdPerHa")*LND.l(b,j,y) + iInv("usdPerMM3")*SUM(m, IRG.l(b,j,y,m)) )/1000000 ;
    rAgri(sSce,b,"AgriIncome",j,y)                   = rAgri(sSce,b,"AgriSales",j,y) + rAgri(sSce,b,"AgriCosts",j,y) + rAgri(sSce,b,"IrrInvCost",j,y);
    rAgri(sSce,b,"AgriProd",j,y)                     = CRP.l(b,j,y);
    rAgri(sSce,b,"LandIncome",j,y)                   = ( 1000000*rAgri(sSce,b,"AgriIncome",j,y) / rAgri(sSce,b,"LandUse",j,y) )$rAgri(sSce,b,"LandUse",j,y);
    rAgri(sSce,b,"WatrIncome",j,y)                   = ( 1000000*rAgri(sSce,b,"AgriIncome",j,y) / rAgri(sSce,b,"WaterUse",j,y))$rAgri(sSce,b,"WaterUse",j,y);
    rAgri(sSce,b,"LabrUse",j,y)$lnd0(b,j)            = 1000*LND.l(b,j,y)/lnd0(b,j);

* Economic results, line
    rEcon(sSce,b,"AgriSales",y,m)                    = SUM(j, rAgri(sSce,b,"AgriSales",j,y) ) / 12;
    rEcon(sSce,b,"AgriInc",y,m)                      = SUM(j, rAgri(sSce,b,"AgriIncome",j,y)) / 12;
    rEcon(sSce,b,"IrrInvCost",y,m)                   = SUM(j, rAgri(sSce,b,"IrrInvCost",j,y)) / 12;
    rEcon(sSce,b,"AgriCost",y,m)                     = SUM(j, rAgri(sSce,b,"AgriCosts",j,y) ) / 12;
    rEcon(sSce,b,"HydroCost",y,m)                    = -1000*reservoirs(b,"cost")$(not bResNOP(b));
    rEcon(sSce,b,"HydroInc",y,m)                     = 1000 * SUM(g, HPP.l(b,g,y,m) *  pEly(g,y,m) ) / 1000000 + rEcon(sSce,b,"HydroCost",y,m);
    rEcon(sSce,b,"HydroProd",y,m)                    = SUM(g, HPP.l(b,g,y,m));
    rEcon(sSce,b,"HydroHead",y,m)                    = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlow(sSce,b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlow(sSce,b,"ResVolAvg",y,m),2));
    rEcon(sSce,b,"HydroGain",y,m)                    = -( rEcon(sSce,b,"HydroProd",y,m) / rFlow(sSce,b,"OutDis",y,m) )$rFlow(sSce,b,"OutDis",y,m);
    rEcon(sSce,b,"TotInc",y,m)                       = rEcon(sSce,b,"AgriInc",y,m) + rEcon(sSce,b,"HydroInc",y,m);
    rEcon(sSce,b,"DischVal",y,m)$bResEly(b)          = -1000000*( rEcon(sSce,b,"HydroInc",y,m) / rFlow(sSce,b,"OutDis",y,m) )$rFlow(sSce,b,"OutDis",y,m);
    rEcon(sSce,b,"WaterVal",y,m)                     = WBALANCE.M("w",b,y,m)*1000000;
    rEcon(sSce,b,"WaterVal",y,"annual")              = ( SUM(m, rEcon(sSce,b,"WaterVal",y,m) * rFlow(sSce,b,"InTot",y,m) )  / rFlow(sSce,b,"InTot",y,"annual")
                                                      )$rFlow(sSce,b,"InTot",y,"annual") + 0.000001;

* Aggregate results into basin sections and countries
    rsAgri(sSce,bs,y0,e_resAgri,j)                  = SUM(b$bSec(b,bs),     rAgri(sSce,b,e_resAgri,j,y0));
    rcAgri(sSce,c,y0,e_resAgri,j)                   = SUM(b$bCty(b,c),      rAgri(sSce,b,e_resAgri,j,y0));
    rcEcon(sSce,y0,e_resEcon,c)                     = SUM((m,b)$bCty(b,c),  rEcon(sSce,b,e_resEcon,y0,m));
    rcEcon(sSce,y,"WaterVal",c)                  = 0;
    rcEcon(sSce,y,"HydroHead",c)                 = 0;
    rcEcon(sSce,y,"HydroGain",c)                 = 0;
    rcEcon(sSce,y,"DischVal",c)                  = 0;
 
DISPLAY LOS.l;