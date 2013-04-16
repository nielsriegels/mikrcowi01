

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
        objHPPweight(b,m)           = 1;
        objDISweight(b,m)           = 0; 
 
* Set fixes for 2nd optimisation run, but only for reservoirs in operation
        resDisFix(bd,"Res_ROG",y,m)$bResEly("Res_ROG")  = DIS.l("w",bd,"Res_ROG",y,m)+eps;
        resDisFix(bd,"Res_TOK",y,m)$bResEly("Res_TOK")  = DIS.l("w",bd,"Res_TOK",y,m)+eps; 
        resVolFix("Res_ROG",y,m)$bResEly("Res_ROG")     = VOL.l("w","Res_ROG",y,m);
        resVolFix("Res_TOK",y,m)$bResEly("Res_TOK")     = VOL.l("w","Res_TOK",y,m); 
    );
