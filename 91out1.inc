* out1.inc - output data parameter generation for base scenario

WBALANCE.M("w",b,y,m)$(WBALANCE.M("w",b,y,m) eq eps) = 0;
pEly(g,y,m)     = -ELYMKT.M(g,y,m)*1000;

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
rFlowBase(b,"OutLos",y,m)                 = -SUM((s,l),                LOS.l(s,b,l,y,m));
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
rEconBase(b,"HydroInc",y,m)               = 1000 * SUM(g, HPP.l(b,g,y,m) *  pEly(g,y,m)) / 1000000 + rEconBase(b,"HydroCost",y,m);
rEconBase(b,"HydroProd",y,m)              = SUM(g, HPP.l(b,g,y,m));
rEconBase(b,"HydroHead",y,m)              = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlowBase(b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlowBase(b,"ResVolAvg",y,m),2));
rEconBase(b,"HydroGain",y,m)              = -( rEconBase(b,"HydroProd",y,m) / rFlowBase(b,"OutDis",y,m) )$rFlowBase(b,"OutDis",y,m);
rEconBase(b,"TotInc",y,m)                 = rEconBase(b,"AgriInc",y,m) + rEconBase(b,"HydroInc",y,m);
rEconBase(b,"DischVal",y,m)$bResEly(b)    = -1000000*( rEconBase(b,"HydroInc",y,m) / rFlowBase(b,"OutDis",y,m) )$rFlowBase(b,"OutDis",y,m);
rEconBase(b,"WaterVal",y,m)               = WBALANCE.M("w",b,y,m)*1000000;
rEconBase(b,"WaterVal",y,"annual")        = ( SUM(m, rEconBase(b,"WaterVal",y,m) * rFlowBase(b,"InTot",y,m) )  / rFlowBase(b,"InTot",y,"annual")
                                                  )$rFlowBase(b,"InTot",y,"annual") + 0.000001;

 
