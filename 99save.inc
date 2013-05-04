IF(beam.modelstat eq 1 or beam.modelstat eq 2 ,
    execute_unload "%path%output\BEAM-flows.gdx"    rFlow, rsFlow, rcFlow;
    execute_unload "%path%output\BEAM-overview.gdx" rcView;
    execute_unload "%path%output\BEAM-agri.gdx"     rAgri;
    execute_unload "%path%output\BEAM-elec.gdx"     rcElec;
    execute_unload "%path%output\BEAM-details.gdx"  rAgri, rcElec, rRbAgri, rRbEcon, rResv, rRbResv, rBorder, rcView, rIrg;
    

*    execute "gdxxrw.exe I=%path%output\BEAM-flows.gdx    O=%path%output\BEAM-flows.xls    par=rFlow  rng=rFlow!B10 par=rsFlow rdim=4 rng=rsFlow!B10 rdim=4 par=rcFlow rdim=4 rng=rcFlow!B10 rdim=4 runMacros=3";
*    execute "gdxxrw.exe I=%path%output\BEAM-agri.gdx     O=%path%output\BEAM-agri.xls     par=rAgri  rng=rAgri!E7   rdim=3       runMacros=3";
*    execute "gdxxrw.exe I=%path%output\BEAM-overview.gdx O=%path%output\BEAM-overview.xls par=rcView rng=rcView!D10 rdim =3          runMacros=3";
*    execute "pause";
);




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
