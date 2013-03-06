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
*    loss(b,m)                   = SUM((mm,l,s), lossRivers(b,l,mm)*sup(s,b,mm))$(ord(m) eq 1);
    sup(s,b,m)                  = SUM(mm, sup(s,b,mm))$(ord(m) eq 1);
*    lossRivers(b,m)             = (loss(b,m)/SUM(s,sup(s,b,m)))$SUM(s,sup(s,b,m));
    agriSeason(b,j,m)           = 1$(ord(m) eq 1);
    qIWater(b,m)                = SUM(mm, qIWater(b,mm))$(ord(m) eq 1);
    qHWater(b,m)                = SUM(mm, qHWater(b,mm))$(ord(m) eq 1);
    rtn0(b,m)                   = SUM(mm, rtn0(b,mm))$(ord(m) eq 1);
    ITK.l(s,bd,bo,y,m)          = itk0(bd,bo)$(ord(m) eq 1);
    FLW.l(s,bd,bo,y,m)          = flw0(bd,bo)$(ord(m) eq 1);
    DIS.l(s,bd,bo,y,m)          = dis0(bd,bo)$(ord(m) eq 1);
    LOS.l(s,b,l,y,m)            = lossRivers(b,l,m)*(rtn0(b,m) + sup(s,b,m) + SUM(bo, FLW.l(s,b,bo,y,m)+DIS.l(s,b,bo,y,m)));
    GRW.l(b,y,m)                = SUM(mS, grwtrMax(b,mS))$(ord(m) eq 1);
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
HPP.l(b,g,y,m)$bResEly(b)   = 0.5*SUM((s,bd)$(resv(bd,b) AND sW(s)),  gravity * reservoirs(b,"coef_eta") * DIS.l(s,bd,b,y,m)
                                  * ( reservoirs(b,"coef_a") + reservoirs(b,"coef_b")*VOL.l(s,b,y,m) + reservoirs(b,"coef_c")*POWER(VOL.l(s,b,y,m),2))
                              ) / 3600;

TPP.l(u,g,y,m)              = elyThermal("elyCap",u) / 2;

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
balCheck(b,"outLos")    = -SUM((s,l,y,m), (SUM(bo, -FLW.l(s,b,bo,y,m) - ITK.l(s,b,bo,y,m) - DIS.l(s,b,bo,y,m)) + sup(s,b,m) + 0*gwCorr(b,m) + rtn0(b,m))*lossRivers(b,l,m)) ;
*balCheck(bo,"outLos")$bSrc(bo)   = -SUM((s,m), sup(s,bo,m)*lossRivers(bo,m));

balCheck(b,"Check")     = balCheck(b,"inSrc")  + balCheck(b,"inFlw")  + balCheck(b,"inDis")  + balCheck(b,"inItk")  + balCheck(b,"inRtn")
                             +balCheck(b,"outAgr") + balCheck(b,"outHHI") + balCheck(b,"outFlw") + balCheck(b,"outDis") + balCheck(b,"outItk") + balCheck(b,"outSto") + balCheck(b,"outLos");
balCheck(b,"Check")     = round (balCheck(b,"Check"), 3);



OPTION itk0:0:0:2;
OPTION flw0:0:0:2;
OPTION dis0:0:0:2;
DISPLAY balCheck;

 
