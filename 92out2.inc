* out2.inc - generation of scenario output data parameter

WBALANCE.M("w",b,y,m)$(WBALANCE.M("w",b,y,m) eq eps) = 0;
pEly(g,y,m)     = -ELYMKT.M(g,y,m)*1000;

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
rFlowCtrf(b,"OutLos",y,m)                 = -SUM((s,l),                     LOS.l(s,b,l,y,m));
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
rEconCtrf(b,"HydroInc",y,m)               = 1000 * SUM(g, HPP.l(b,g,y,m) *  pEly(g,y,m)) / 1000000 + rEconCtrf(b,"HydroCost",y,m);
rEconCtrf(b,"HydroProd",y,m)              = SUM(g, HPP.l(b,g,y,m));
rEconCtrf(b,"HydroHead",y,m)              = ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*rFlowCtrf(b,"ResVolAvg",y,m) + reservoirs(b,"coef_c")*POWER(rFlowCtrf(b,"ResVolAvg",y,m),2));
rEconCtrf(b,"HydroGain",y,m)              = -( rEconCtrf(b,"HydroProd",y,m) / rFlowCtrf(b,"OutDis",y,m) )$rFlowCtrf(b,"OutDis",y,m) ;
rEconCtrf(b,"TotInc",y,m)                 = rEconCtrf(b,"AgriInc",y,m) + rEconCtrf(b,"HydroInc",y,m);
rEconCtrf(b,"WaterVal",y,m)               = WBALANCE.M("w",b,y,m)*1000000;
rEconCtrf(b,"WaterVal",y,"annual")        = ( SUM(m, rEconCtrf(b,"WaterVal",y,m) * rFlowCtrf(b,"InTot",y,m) )  / rFlowCtrf(b,"InTot",y,"annual")
                                                  )$rFlowCtrf(b,"InTot",y,"annual") + 0.000001;


 
