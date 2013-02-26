file fFlowBase /%path%output\FlowBase.csv/;
fFLowBase.pw = 170;
PUT fFlowBase;
PUT "                        "; LOOP((y,mN,m)$mX(mN,m), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN,m)$mX(mN,m), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resFlow),
    PUT b.tl:12; PUT e_resFlow.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rFlowBase(b,e_resFlow,y,m)):12:0; ); PUT /;
);

file fFlowCtrf /%path%output\FlowCtrf.csv/;
fFLowCtrf.pw = 170;
PUT fFlowCtrf;
PUT "                        "; LOOP((y,mN), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resFlow),
    PUT b.tl:12; PUT e_resFlow.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rFlowCtrf(b,e_resFlow,y,m)):12:0; ); PUT /;
);

file fAgriBase /%path%output\AgriBase.csv/;
fAgriBase.pw = 120;
PUT fAgriBase;
PUT "                        "; LOOP((y,j), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,j), PUT j.tl:>12 ); PUT /;
LOOP((b,e_resAgri)$bPlz(b),
    PUT b.tl:12; PUT e_resAgri.tl:12;
    LOOP((y,j), PUT round(rAgriBase(b,e_resAgri,j,y)):12:0; ); PUT /;
);

file fAgriCtrf /%path%output\AgriCtrf.csv/;
fAgriCtrf.pw = 120;
PUT fAgriCtrf;
PUT "                        "; LOOP((y,j), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,j), PUT j.tl:>12 ); PUT /;
LOOP((b,e_resAgri)$bPlz(b),
    PUT b.tl:12; PUT e_resAgri.tl:12;
    LOOP((y,j), PUT round(rAgriCtrf(b,e_resAgri,j,y)):12:0; ); PUT /;
);

file fEconBase /%path%output\EconBase.csv/;
fEconBase.pw = 170;
PUT fEconBase;
PUT "                        "; LOOP((y,mN), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resEcon)$(bPlz(b) or bRes(b)),
    PUT b.tl:12; PUT e_resEcon.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rEconBase(b,e_resEcon,y,m)):12:0; ); PUT /;
);

file fEconCtrf /%path%output\EconCtrf.csv/;
fEconCtrf.pw = 170;
PUT fEconCtrf;
PUT "                        "; LOOP((y,mN), PUT y.tl:>12 ); PUT /;
PUT "                        "; LOOP((y,mN), PUT mN.tl:>12 ); PUT /;
LOOP((b,e_resEcon)$(bPlz(b) or bRes(b)),
    PUT b.tl:12; PUT e_resEcon.tl:12;
    LOOP((y,mN,m)$mX(mN,m), PUT round(rEconCtrf(b,e_resEcon,y,m)):12:0; ); PUT /;
);

PUTCLOSE;

SET sAss /
    sSta    "Solver status"
    mSta    "Model status"
    pCot    "Cotton price USD/ton"
    pWht    "Wheat price USD/ton"
    pRic    "Rice price USD/ton"
    pAlf    "Alfalfa price USD/ton"
    pVeg    "Vegetable price USD/ton"
    pFru    "Fruit price USD/ton"
    pOth    "Other crops price USD/ton"
    pLab    "Labor price USD/ha"
    pCap    "Capital price USD/ha"
    pInp    "Diesel/fertilizer price USD/ha"
    eta     "Crop change transformation elasticity"
    xARN    "Extra nature water need, Aral Sea North, mm3/year"
    xARS    "Extra nature water need, Aral Sea South, mm3/year"
    xGOL    "Extra nature water need, Golden Lake, mm3/year"
    fCot    "Cotton flexible (0/1)?"
    fWht    "Wheat flexible (0/1)?"
    fRic    "Rice flexible (0/1)?"
    fAlf    "Alfalfa flexible (0/1)?"
    rain    "Rainfall year"
    year    "Modelled years"
    iIha    "Irrigation efficiency investment USD/ha"
    iIm3    "Irrigation efficiency investment USD/m3"
    iImp    "Irrigatopn efficiency improvement (%)"
    iDAS    "Investments in Dashtijum"
    iROG    "Investments in Rogun"
    iKAM    "Investments in Kambarata-1"
    iNAR    "Investments in Naryn cascade"
    iVAH    "Investments in Vakhsh cascade"
    iZAR    "Investments in Yavan"
    pE01    "Electricity price m01 USD/MWh"
    pE02    "Electricity price m02 USD/MWh"
    pE03    "Electricity price m03 USD/MWh"
    pE04    "Electricity price m04 USD/MWh"
    pE05    "Electricity price m05 USD/MWh"
    pE06    "Electricity price m06 USD/MWh"
    pE07    "Electricity price m07 USD/MWh"
    pE08    "Electricity price m08 USD/MWh"
    pE09    "Electricity price m09 USD/MWh"
    pE10    "Electricity price m10 USD/MWh"
    pE11    "Electricity price m11 USD/MWh"
    pE12    "Electricity price m12 USD/MWh"
    rBld    "Reservoir buildup"
    cDHH    "Demographic change households"
    cDIn    "Demographic change industry"
    aFal    "Fallow land as percent of flexible crop area"
/;



PARAMETER assumptn(sAss,sSce);

assumptn("pCot",sSce)   = pCrop0("cot",sSce);
assumptn("pWht",sSce)   = pCrop0("wht",sSce);
assumptn("pRic",sSce)   = pCrop0("ric",sSce);
assumptn("pAlf",sSce)   = pCrop0("alf",sSce);
assumptn("pVeg",sSce)   = pCrop0("veg",sSce);
assumptn("pFru",sSce)   = pCrop0("fru",sSce);
assumptn("pOth",sSce)   = pCrop0("oth",sSce);
assumptn("eta ",sSce)   = sT*1000;

assumptn("fCot",sSce)   = jAF("cot");
assumptn("fWht",sSce)   = jAF("wht");
assumptn("fRic",sSce)   = jAF("ric");
assumptn("fAlf",sSce)   = jAF("alf");

assumptn("iIha",sSce)   = 0;
assumptn("iIm3",sSce)   = 0;
assumptn("iImp",sSce)   = 0;
assumptn("iDAS",sSce)   = 0;
assumptn("iROG",sSce)   = 0;
assumptn("iKAM",sSce)   = 0;
assumptn("iNAR",sSce)   = 0;
assumptn("iVAH",sSce)   = 0;
assumptn("iZAR",sSce)   = 0;

assumptn("pE01","base")   = SUM(y$modelYear(y), pElyBase(y,"m01"));
assumptn("pE02","base")   = SUM(y$modelYear(y), pElyBase(y,"m02"));
assumptn("pE03","base")   = SUM(y$modelYear(y), pElyBase(y,"m03"));
assumptn("pE04","base")   = SUM(y$modelYear(y), pElyBase(y,"m04"));
assumptn("pE05","base")   = SUM(y$modelYear(y), pElyBase(y,"m05"));
assumptn("pE06","base")   = SUM(y$modelYear(y), pElyBase(y,"m06"));
assumptn("pE07","base")   = SUM(y$modelYear(y), pElyBase(y,"m07"));
assumptn("pE08","base")   = SUM(y$modelYear(y), pElyBase(y,"m08"));
assumptn("pE09","base")   = SUM(y$modelYear(y), pElyBase(y,"m09"));
assumptn("pE10","base")   = SUM(y$modelYear(y), pElyBase(y,"m10"));
assumptn("pE11","base")   = SUM(y$modelYear(y), pElyBase(y,"m11"));
assumptn("pE12","base")   = SUM(y$modelYear(y), pElyBase(y,"m12"));

assumptn("rain","base")   = 2009;
assumptn("rain","ctrf")   = 2001$baseYear("2001") + 2009$baseYear("2009") ;

assumptn("year","base")   = 2009$modelYear("2009") ;
assumptn("year","ctrf")   = 2009$modelYear("2009") + 2020$modelYear("2020") + 2050$modelYear("2050");

assumptn("pLab","base")   = 1000;
assumptn("pCap","base")   = 1000;
assumptn("pInp","base")   = 1000;

assumptn("pLab","ctrf")   = pInput("labr")*1000;
assumptn("pCap","ctrf")   = pInput("cptl")*1000;
assumptn("pInp","ctrf")   = pInput("inpt")*1000;

assumptn("xARN","ctrf")   = natExtra("LAK_ARN");
assumptn("xARS","ctrf")   = natExtra("LAK_ARS");
assumptn("xGOL","ctrf")   = natExtra("LAK_GOL");


assumptn("iIha","ctrf")   = iInv0("usdPerMm3")*1000;
assumptn("iIm3","ctrf")   = iInv0("usdPerHa")*1000;
assumptn("iImp","ctrf")   = iInv0("improve")*1000;
assumptn("iDAS","ctrf")   = resNewOp("res_DAS");
assumptn("iROG","ctrf")   = resNewOp("res_ROG");
assumptn("iKAM","ctrf")   = resNewOp("res_KAM");
assumptn("iNAR","ctrf")   = resNewOp("res_NAR");
assumptn("iVAH","ctrf")   = resNewOp("res_VAH");
assumptn("iZAR","ctrf")   = resNewOp("res_ZAR");

assumptn("pE01","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m01"));
assumptn("pE02","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m02"));
assumptn("pE03","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m03"));
assumptn("pE04","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m04"));
assumptn("pE05","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m05"));
assumptn("pE06","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m06"));
assumptn("pE07","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m07"));
assumptn("pE08","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m08"));
assumptn("pE09","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m09"));
assumptn("pE10","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m10"));
assumptn("pE11","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m11"));
assumptn("pE12","ctrf")   = SUM(y$modelYear(y), pElyctrf(y,"m12"));

assumptn("mSta","base") = modlStatB;
assumptn("mSta","ctrf") = modlStatS;
assumptn("sSta","base") = solvStatB;
assumptn("sSta","ctrf") = solvStatS;

assumptn("rBld","base") = SUM((b,m,y,s)$(sW(s) AND bResBuild(b)), STO.l(s,b,y,m) - SUM(bd$resv(bd,b), DIS.l(s,bd,b,y,m)+ITK.l(s,bd,b,y,m)+FLW.l(s,bd,b,y,m)) )
                          / SUM(b$bResBuild(b), reservoirs(b,"max"))$SUM(b$bResBuild(b), reservoirs(b,"max"));
assumptn("rBld","ctrf") = ctrfBuild0*1000;

assumptn("cDHH","ctrf") = SUM(y$modelYear(y), demography(y,"HH"))*1000;
assumptn("cDIn","ctrf") = SUM(y$modelYear(y), demography(y,"Ind"))*1000;

assumptn("aFal","base") = 0;
assumptn("aFal","ctrf") = fallowLand*1000;


file fAssumptn /%path%output\assumptn.csv/;
fAssumptn.pw = 170;
PUT fAssumptn;
PUT "            "; LOOP(sSce, PUT sSce.tl:>12 ); PUT /;
LOOP(sAss,
    PUT sAss.tl:12;
    LOOP(sSce, PUT assumptn(sAss,sSce):12:0; ); PUT /;
);
PUTCLOSE;

file fObjValues /%path%output\objValues.txt/
PUT fObjValues;
PUT "Base total income: ";
PUT SUM((b,y,m), rEconBase(b,"TotInc",y,m)); PUT /;
PUT "Ctrf total income: ";
PUT SUM((b,y,m), rEconCtrf(b,"TotInc",y,m)); PUT /;
PUTCLOSE;
 
