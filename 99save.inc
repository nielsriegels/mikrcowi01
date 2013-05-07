* Unload results to GDX files
execute_unload "%path%output\BEAM-flows.gdx"    rFlow, rsFlow, rcFlow;
execute_unload "%path%output\BEAM-overview.gdx" rcView;
execute_unload "%path%output\BEAM-agri.gdx"     rAgri;
execute_unload "%path%output\BEAM-elec.gdx"     rcElec;
execute_unload "%path%output\BEAM-details.gdx"  rAgri, rcElec, rcElecBal, rRbAgri, rRbEcon, rResv, rRbResv, rBorder, rcView, rcRbView, rSView, rIrg, rRbFlow;

* Display checks that results have not changed
file objvalues /%path%output\objvalues.txt/;
PUT objvalues;
PUT "Sc.        TotInc         Obj  Model Status"; PUT /;
PUT "-------------------------------------------"; PUT /;
LOOP(sSce$scenarios("run",sSce), 
    PUT sSce.tl:<5;
    PUT round(checkOutput("TotInc" ,sSce),2):12:2; 
    PUT round(checkOutput("Obj"    ,sSce),2):12:2;
    PUT round(checkOutput("mstat"  ,sSce),0):14:0;
    PUT /;
);
PUTCLOSE;
