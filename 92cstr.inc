

* Constrained optimisation - maximise winter discharges
    IF(scenarios("wDISwin",sSce),
        objAGRweight(b)             = 0.001;
        objELYweight                = 0.001;
        objHPPweight(b,m)           = 0.001;
        objDISweight("Res_ROG",m)   = -1000$mSum(m) + 1000$mWin(m); 
        objDISweight("Res_TOK",m)   = -1000$mSum(m) + 1000$mWin(m); 
    );

* Constrained optimisation - maximise summer discharges
    IF(scenarios("wDISsum",sSce),
        objAGRweight(b)             = 0.001;
        objELYweight                = 0.001;
        objHPPweight(b,m)           = 0.001;
        objDISweight("Res_ROG",m)   = -1000$mWin(m) + 1000$mSum(m); 
        objDISweight("Res_TOK",m)   = -1000$mWin(m) + 1000$mSum(m); 
    );

* Constrained optimisation - maximise total HPP electricity
    IF(scenarios("wHPPall",sSce),
        objAGRweight(b)             = 0.001;
        objHPPweight(b,m)           = 1000$( bCty(b,"KYR") or bCty(b,"TAD"));
        objELYweight                = 0.001;
        objDISweight(b,m)           = 0; 
    );

* Constrained optimisation - maximise winter HPP electricity
    IF(scenarios("wHPPwin",sSce),
        objAGRweight(b)             = 0.001;
        objHPPweight(b,m)           = 1000$( (bCty(b,"KYR") or bCty(b,"TAD")) AND mWin(m) );
        objELYweight                = 0.001;
        objDISweight(b,m)           = 0; 
    );


    IF(scenarios("wHPPwin",sSce)+scenarios("wHPPall",sSce)+scenarios("wDISwin",sSce)+scenarios("wDISsum",sSce) gt 0, 
* Run model with altered weighting of objective parts
        SOLVE beam MAXIMIZING twv USING NLP;

* Reset objective weights
        objAGRweight(b)             = 1;
        objELYweight                = 1;
        objHPPweight(b,m)           = 0;
        objDISweight(b,m)           = 0; 
 
* Set fixes for 2nd optimisation run, but only for reservoirs in operation
        resDisFix(bd,"Res_ROG",y,m)$bResEly("Res_ROG")  = DIS.l("w",bd,"Res_ROG",y,m)+eps;
        resDisFix(bd,"Res_TOK",y,m)$bResEly("Res_TOK")  = DIS.l("w",bd,"Res_TOK",y,m)+eps; 
        resVolFix("Res_ROG",y,m)$bResEly("Res_ROG")     = VOL.l("w","Res_ROG",y,m);
        resVolFix("Res_TOK",y,m)$bResEly("Res_TOK")     = VOL.l("w","Res_TOK",y,m); 
    );

* Constrained optimisation - single country sequential optimisation
* (KYR-TAD)-TUR-UZB-KAZ priority
    IF(scenarios("wSingle",sSce),
* Disable reservoir spillways (they cause troubles) and clear all countries from obj.
        flow(bd,bo)$resv(bd,bo)                 = NO;
        objCTYweight(c)                         = 0;
        objNODEweight(b)                        = 0;
        
* Optimise for KYR+TAD (they interfere with each other only little)
        objCTYweight("KYR")                     = 1;
        objNODEweight(b)$bCty(b,"KYR")          = 1;
        objCTYweight("TAD")                     = 1;
        objNODEweight(b)$bCty(b,"TAD")          = 1;
        objNODEweight("FER_UZB")                = 1;
        SOLVE beam MAXIMIZING twv USING NLP;
        DISPLAY "Solve FER+TAD", TWV.l;
        LOOP(b$objNODEweight(b),
            resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+eps; 
            resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
            ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
        );

DISPLAY DIS.l;
        
* Assuming TUR (and not UZB) have control over intake to Res_ZD and Kairakum canal
* given fixed intakes upstream
        objCTYweight("TUR")                     = 1;
        objNODEweight(b)$bCty(b,"TUR")          = 1;
beam.solprint = yes;
        SOLVE beam MAXIMIZING twv USING NLP;
        DISPLAY "Solve TUR", TWV.l;
        LOOP(b$objNODEweight(b),
            resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+eps; 
            resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
            ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
        );
beam.solprint = no;

* Optimise for UZB given fixed intakes upstream
        objCTYweight("UZB")                     = 1;
        objNODEweight(b)$bCty(b,"UZB")          = 1;
        SOLVE beam MAXIMIZING twv USING NLP;
        DISPLAY "Solve UZB", TWV.l;
        LOOP(b$objNODEweight(b),
            resDisFix(bd,b,y,m)$bResEly(b)      = DIS.l("w",bd,b,y,m)+eps; 
            resVolFix(b,y,m)$bResEly(b)         = VOL.l("w",b,y,m);
            ITK.fx(s,b,bo,y,m)                  = ITK.l(s,b,bo,y,m);
        );

* Optimise for all region given fixed flows
        objCTYweight(c)                         = 1;
        objNODEweight(b)                        = 1;
*        SOLVE beam MAXIMIZING twv USING NLP;
        DISPLAY "Solve KAZ", TWV.l;

    );