SET scOptions /
    objvar      "Variables entering into objective function"
    seqnce      "Sequencing used"
    nukus       "Use Nukus agreement"
    clmChg      "Climate change"
    prLevl      "Price level"
    aPrice      "Agricultural prices (financial/economic)"
    ePrice      "Energy prices (financial/economic)"
    lndRfm      "Land reform"
    iCanal      "Improvements in canals"
    iField      "Improvements in field level irrigation"
    iWSS        "Investments in WSS"
    trCAPS      "CAPS trade"
    SAexpt      "South Asia exports"
    cCanal      "Canal investment costs (% of max)"
    cField      "Field irrigation investment costs (% of max)"
    cElect      "Investments in thermal power"
/;

TABLE scenCalc(*,sSce) "Control calculation flow"
    s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32 s33 s34 s35 s36 s37 s38 s39 s40 s41 s42 s43 s44 s45 s46 s47 s48 s49 s50 s51 s52 s53 s54 s55 s56 s57 s58 s59 s60 s61 s62 s63 s64 s65 s66 s67 s68 s69 s70 s71 s72 s73 s74 s75 s76 s77 s78 s79 s80 s81 s82 s83 s84 
run 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   
out 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   
*    s01 s02 s03 s04 s05 s06 s07 s08 s09 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32 s33 s34 s35 s36 s37 s38 s39 s40 s41 s42 s43 s44 s45 s46 s47 s48 s49 s50 s51 s53 
*run 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0                       0   0   0
*out 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0                       1   1   1
*out 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0
*run 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0
;

TABLE scenarios(sSce,scOptions) "Scenario definition table"
           objvar   seqnce   nukus    clmChg   prLevl   aPrice   ePrice   lndRfm   iCanal   iField   iWSS     trCAPS   SAexpt   cCanal   cField   cElect 
s01        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s02        0        3        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s03        0        3        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s04        0        3        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s05        0        3        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s06        0        3        0        0        0        0        0        0        0        0        0        5        1        0        0        1
s07        0        3        0        0        0        0        0        0        0        0        0        0        1        0        0        1
s08        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s09        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s10        0        3        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s11        0        3        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s12        0        3        1        0        0        0        0        0        0        0        0        5        1        0        0        1
s13        0        3        1        0        0        0        0        0        0        0        0        0        1        0        0        1
s14        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s15        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s16        0        3        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s17        0        3        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s18        0        3        1        0        0        0        0        0        0        0        0        5        1        0        0        1
s19        0        3        1        0        0        0        0        0        0        0        0        0        1        0        0        1
s20        0        0        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s21        0        0        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s22        0        0        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s23        0        0        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s24        0        0        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s25        0        0        1        0        0        0        0        0        0        0        0        5        1        0        0        1
s26        0        0        1        0        0        0        0        0        0        0        0        0        1        0        0        1
s27        0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s28        0        0        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s29        0        0        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s30        0        0        0        0        0        0        0        0        0        0        0        5        1        0        0        1
s31        0        0        0        0        0        0        0        0        0        0        0        0        1        0        0        1
s32        0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s33        0        0        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s34        0        0        0        0        0        0        0        0        100      100      0        5        0        0        0        1
s35        0        0        0        0        0        0        0        1        100      100      0        5        0        0        0        1
s36        1        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s37        2        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s38        3        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s39        4        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s40        1        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s41        2        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s42        3        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s43        4        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s44        0        0        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s45        0        3        1        0        1        0        0        0        0        0        0        0        0        0        0        1
s46        0        3        1        0        0        1        1        0        0        0        0        0        0        0        0        1
s47        0        3        1        1        0        0        0        0        0        0        0        0        0        0        0        1
s48        0        3        1        0        0        1        0        1        100      100      0        0        0        100      100      1
s49        0        3        1        1        0        1        0        1        100      100      0        0        0        100      100      1
s50        0        3        1        0        0        0        0        0        100      100      0        0        0        50       50       1
s51        0        3        1        0        0        0        0        0        100      0        0        0        0        100      0        1
s52        0        3        1        1        0        0        0        0        100      0        0        0        0        100      0        1
s53        0        3        1        0        0        0        0        0        0        0        1        0        0        0        0        1
s54        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s55        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s56        0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s57        0        3        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s58        0        3        1        1        0        0        0        0        0        0        0        5        0        0        0        1
s59        0        3        1        0        0        0        0        0        0        0        0        0        1        0        0        1
s60        0        3        1        0        0        0        0        0        0        0        0        3        0        0        0        1
s61        0        3        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s62        0        3        1        0        0        0        0        0        0        0        0        5        0        0        0        1
s63        0        3        1        1        0        0        0        0        0        0        0        5        0        0        0        1
s64        0        3        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s65        1        3        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s66        4        3        0        0        0        0        0        0        0        0        0        5        0        0        0        1
s67        0        3        1        0        0        0        0        0        0        0        0        5        1        0        0        1
s68        0        3        1        0        0        0        0        0        0        0        0        5        1        0        0        1
s69        0        3        1        1        0        0        0        0        0        0        0        5        1        0        0        1
s70        1        3        0        0        0        0        0        0        0        0        0        5        1        0        0        1
s71        4        3        0        0        0        0        0        0        0        0        0        5        1        0        0        1
s72        0        3        0        0        0        0        0        0        0        0        0        5        1        0        0        1
s73        0        0        1        0        0        0        0        0        0        0        0        5        1        0        0        1
s74        0        0        0        0        0        0        0        0        0        0        0        5        1        0        0        1
s75        0        3        0        0        0        1        1        1        100      100      1        5        0        100      100      1
s76        0        3        1        0        0        1        1        1        100      100      1        6        0        100      100      1
s77        0        3        0        1        0        1        1        1        100      100      1        5        0        100      100      1
s78        0        3        0        0        0        1        1        1        100      100      1        5        1        100      100      1
s79        0        3        0        1        0        1        1        1        100      100      1        5        1        100      100      1
s80        1        3        0        0        0        1        1        1        100      100      1        5        1        100      100      1
s81        4        3        0        0        0        1        1        1        100      100      1        5        1        100      100      1
s82        0        0        1        0        0        1        1        1        100      100      1        5        1        100      100      1
s83        0        0        0        0        0        1        1        1        100      100      1        5        1        100      100      1
s84        0        3        0        0        0        0        0        0        100      0        1        5        1        100      0        1
s100       0        1        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s101       0        2        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s102       0        3        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s103       0        1        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s104       0        2        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s105       0        3        1        0        0        0        0        0        0        0        0        0        0        0        0        1
s106       0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s107       0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s108       0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s109       0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s110       0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
s111       0        0        0        0        0        0        0        0        0        0        0        0        0        0        0        1
;

TABLE resNewOp(sSce,b) "New reservoir in operation"
          Res_ROG Res_KA1 Res_DAS Res_ZAR Res_VA2 Res_NA1 Res_KA2 Res_PYD 
s01       0       0       0       0       0       0       0       0
s02       0       0       0       0       0       0       0       0
s03       0       0       0       1       0       0       1       1
s04       0       0       0       0       0       0       0       0
s05       0       0       0       1       0       0       1       1
s06       0       0       0       0       0       0       0       0
s07       0       0       0       0       0       0       0       0
s08       0       0       0       0       0       0       0       0
s09       0       0       0       1       0       0       1       1
s10       0       0       0       0       0       0       0       0
s11       0       0       0       1       0       0       1       1
s12       0       0       0       0       0       0       0       0
s13       0       0       0       0       0       0       0       0
s14       1       1       0       0       0       0       0       0
s15       1       1       0       1       0       0       1       1
s16       1       1       0       0       0       0       0       0
s17       1       1       0       1       0       0       1       1
s18       1       1       0       0       0       0       0       0
s19       1       1       0       0       0       0       0       0
s20       0       0       0       0       0       0       0       0
s21       1       1       0       0       0       0       0       0
s22       0       0       0       1       0       0       1       1
s23       0       0       0       0       0       0       0       0
s24       0       0       0       1       0       0       1       1
s25       0       0       0       0       0       0       0       0
s26       0       0       0       0       0       0       0       0
s27       1       1       0       0       0       0       0       0
s28       1       1       0       0       0       0       0       0
s29       1       1       0       0       0       0       0       0
s30       1       1       0       0       0       0       0       0
s31       0       0       0       0       0       0       0       0
s100      0       0       0       0       0       0       0       0
s101      0       0       0       0       0       0       0       0
s102      0       0       0       0       0       0       0       0
s103      0       0       0       0       0       0       0       0
s104      0       0       0       0       0       0       0       0
s105      0       0       0       0       0       0       0       0
s32       0       0       0       0       0       0       0       0
s33       0       0       0       0       0       0       0       0
s34       0       0       0       0       0       0       0       0
s35       0       0       0       0       0       0       0       0
s36       1       1       0       0       0       0       0       0
s37       1       1       0       0       0       0       0       0
s38       1       1       0       0       0       0       0       0
s39       1       1       0       0       0       0       0       0
s40       1       1       0       0       0       0       0       0
s41       1       1       0       0       0       0       0       0
s42       1       1       0       0       0       0       0       0
s43       1       1       0       0       0       0       0       0
s44       0       0       0       0       0       0       0       0
s45       0       0       0       0       0       0       0       0
s46       0       0       0       0       0       0       0       0
s47       0       0       0       0       0       0       0       0
s48       0       0       0       0       0       0       0       0
s49       0       0       0       0       0       0       0       0
s50       0       0       0       0       0       0       0       0
s51       0       0       0       0       0       0       0       0
s52       0       0       0       0       0       0       0       0
s53       0       0       0       0       0       0       0       0
s54       0       0       0       0       0       0       0       0
s55       0       0       0       1       0       0       1       1
s56       1       1       0       1       0       0       1       1
s57       0       0       0       0       0       0       0       0
s58       0       0       0       0       0       0       0       0
s59       0       0       0       0       0       0       0       0
s60       1       1       0       0       0       0       0       0
s61       1       1       0       0       0       0       0       0
s62       1       1       0       1       0       0       1       1
s63       1       1       0       0       0       0       0       0
s64       1       1       0       0       0       0       0       0
s65       1       1       0       0       0       0       0       0
s66       1       1       0       0       0       0       0       0
s67       1       1       0       0       0       0       0       0
s68       1       1       0       1       0       0       1       1
s69       1       1       0       0       0       0       0       0
s70       1       1       0       0       0       0       0       0
s71       1       1       0       0       0       0       0       0
s72       1       1       0       0       0       0       0       0
s73       1       1       0       0       0       0       0       0
s74       1       1       0       0       0       0       0       0
s75       1       1       0       0       0       0       0       0
s76       1       1       0       0       0       0       0       0
s77       1       1       0       0       0       0       0       0
s78       1       1       0       0       0       0       0       0
s79       1       1       0       0       0       0       0       0
s80       1       1       0       0       0       0       0       0
s81       1       1       0       0       0       0       0       0
s82       1       1       0       0       0       0       0       0
s83       1       1       0       0       0       0       0       0
s84       1       1       0       0       0       0       0       0
;


TABLE maxHead0(sSce,b) "Maximum limit head for selected reservoirs (meter)"
          Res_ROG
s01       335
s02       335
s03       335
s04       335
s05       335
s06       335
s07       335
s08       335
s09       335
s10       335
s11       335
s12       335
s13       335
s14       335
s15       335
s16       335
s17       335
s18       335
s19       335
s20       335
s21       335
s22       335
s23       335
s24       335
s25       335
s26       335
s27       335
s28       335
s29       265
s30       335
s31       335
s100      335
s101      335
s102      335
s103      335
s104      335
s105      335
s32       335
s33       335
s34       335
s35       335
s36       335
s37       335
s38       335
s39       335
s40       265
s41       265
s42       265
s43       265
s44       335
s45       335
s46       335
s47       335
s48       335
s49       335
s50       335
s51       335
s52       335
s53       335
s54       335
s55       335
s56       335
s57       335
s58       335
s59       335
s60       335
s61       335
s62       335
s63       335
s64       335
s65       335
s66       335
s67       335
s68       335
s69       335
s70       335
s71       335
s72       335
s73       335
s74       335
s75       335
s76       335
s77       335
s78       335
s79       335
s80       335
s81       335
s82       335
s83       335
s84       335
;

TABLE icCrop(k,sSce) "Indices for crop costs"                                                               
         s01
labr     0
inpt     0
cptl     0
;

TABLE ipCrop(j,sSce) "Indices for crop prices"
          s45      
cot       0.9        
wht       0.9        
ric       0.9        
ri2       0.9        
alf       0.9        
tmt       0.9        
shv       0.9        
pot       0.9        
mng       0.9        
sbt       0.9        
stf       0.9        
tgr       0.9        
mln       0.9        
mzf       0.9        
mzg       0.9        
;

TABLE natExtra0(b,sSce) "Extra nature needs (mm3/year)"
        s01 
Lak_ARN 0   
Lak_ARS 0   
Lak_GOL 0   
;


PARAMETER minIntake0(sSce) / s99 0 /;


TABLE sNukus(c,rb) "Country share of river basin runoff"
        AMU     SYR
KAZ     0       0.42
UZB     0.42    0.50
TUR     0.42    0 
KYR     0       0.01
TAD     0.15    0.07




TABLE elyEta(c,g,m0) "Semilog demand price elasticity"
            m01     m02     m03     m04     m05     m06     m07     m08     m09     m10     m11     m12     annual
KAZ.base    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
KAZ.peak    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
UZB.base    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
UZB.peak    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
KYR.base    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
KYR.peak    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
TAD.base    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
TAD.peak    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2    -0.2
;

TABLE elyTariff(c,y0) "Consumer tariff for electricity (USD/MWh)"
        2009    2020    2050
KAZ     72.4    72.4    72.4
UZB     41.15   41.15   50
TAD     20.12   20.12   50
KYR     20.4    20.4    50
;

PARAMETER elyWTP0(c,g,d0) "Willingness to pay for electricity";
* (ln(Ds)-(ln(Dn)-eta))/(eta/tariff)
*elyWTP0(c,g,d) = ( log(usdLvl(d)) - log(1) - elyEta(c,g,"annual") ) / (elyEta(c,g,"annual")/elyTariff(c,"2009"));
*DISPLAY elyWTP0;
*abort "Hej";

SET scenModelYear(sSce,y0) "Model year for demography and depreciation" /
s01.2009, s02.2009, s03.2009, s04.2009, s05.2009, s06.2009, s07.2009, s08.2009, s09.2009, s10.2009,
s11.2009, s12.2009, s13.2009, s14.2009, s15.2009, s16.2009, s17.2009, s18.2009, s19.2009, s20.2009,
s21.2009, s22.2009, s23.2009, s24.2009, s25.2009, s26.2009, s27.2009, s28.2009, s29.2009, s30.2009,
s31.2009, s32.2009, s33.2009, s34.2009, s35.2009, s36.2009, s37.2009, s38.2009, s39.2009, s40.2009,
s41.2009, s42.2009, s43.2009, s44.2020, s45.2020, s46.2020, s47.2020, s48.2020, s49.2020, s50.2020,
s51.2020, s52.2020, s53.2020, s54.2020, s55.2020, s56.2020, s57.2020, s58.2020, s59.2020, s60.2020,
s61.2020, s62.2020, s63.2020, s64.2020, s65.2020, s66.2020, s67.2020, s68.2020, s69.2020, s70.2020,
s71.2020, s72.2020, s73.2020, s74.2020, s75.2020, s76.2020, s77.2020, s78.2020, s79.2020, s80.2020,
s81.2020, s82.2020, s83.2020, s84.2020, s85.2020, s86.2020, s87.2020, s88.2020, s89.2020, s90.2020
/;

SET scenRainYear(sSce,y0) "Rainfall year" /
s01.2009, s02.2009, s03.2009, s04.2009, s05.2009, s06.2009, s07.2009, s08.2009, s09.2009, s10.2009,
s11.2009, s12.2009, s13.2009, s14.2009, s15.2009, s16.2009, s17.2009, s18.2009, s19.2009, s20.2009,
s21.2009, s22.2009, s23.2009, s24.2009, s25.2009, s26.2009, s27.2009, s28.2009, s29.2009, s30.2009,
s31.2009, s32.2009, s33.2009, s34.2009, s35.2009, s36.2009, s37.2009, s38.2009, s39.2009, s40.2009,
s41.2009, s42.2009, s43.2009, s44.2009, s45.2009, s46.2009, s47.2009, s48.2009, s49.2009, s50.2009,
s51.2009, s52.2009, s53.2009, s54.2009, s55.2009, s56.2009, s57.2009, s58.2009, s59.2009, s60.2009,
s61.2009, s62.2009, s63.2009, s64.2009, s65.2009, s66.2009, s67.2009, s68.2009, s69.2009, s70.2009,
s71.2009, s72.2009, s73.2009, s74.2009, s75.2009, s76.2009, s77.2009, s78.2009, s79.2009, s80.2009,
s81.2009, s82.2009, s83.2009, s84.2009, s85.2009, s86.2009, s87.2009, s88.2009, s89.2009, s90.2009
/;

TABLE pEly0(y0,m) "Electricity prices USD/MWh"
                m01             m02             m03             m04             m05             m06             m07             m08             m09             m10             m11             m12
2009            7.000000E+01    7.000000E+01    6.000000E+01    5.000000E+01    4.000000E+01    3.000000E+01    3.000000E+01    4.000000E+01    5.000000E+01    5.000000E+01    6.000000E+01    6.500000E+01
2020            7.000000E+01    7.000000E+01    6.000000E+01    5.000000E+01    4.000000E+01    3.000000E+01    3.000000E+01    4.000000E+01    5.000000E+01    5.000000E+01    6.000000E+01    6.500000E+01
2050            7.000000E+01    7.000000E+01    6.000000E+01    5.000000E+01    4.000000E+01    3.000000E+01    3.000000E+01    4.000000E+01    5.000000E+01    5.000000E+01    6.000000E+01    6.500000E+01
;


* CASA-1000 (1,300 MW) with a maximum annual energy export of 4,000 GWh (shared by the Kyrgyz Republic [40%] 
* and Tajikistan [60% = 2,400 GWh] from 2017 onwards; 
* CASA Phase 2 (1,300 MW) with a maximum annual energy export from Tajikistan of 4,000 GWh, from 2023 onwards; 
* Assuming an energy tariff of 3.5 cents/kWh, based on the tariff agreed with Afghanistan and competitive 
* pricing for summer surpluses, the total export tariffs (including recovery of estimated transmission costs) 
* will be 4.4 cents/kWh for CASA-1000 and 6.2 cents/kWh for CASA Phase 2. 
TABLE elyExp0(*,c,m0) "Electricity export to South Asia (qExp in MW; pExp in USD/MWh)"
            m01     m02     m03     m04     m05     m06     m07     m08     m09     m10     m11     m12
qExp.KYR    0       0       0       0       520     520     520     520     0       0       0       0   
pExp.KYR    0       0       0       0       53      53      53      53      0       0       0       0   
qExp.TAD    0       0       0       0       2080    2080    2080    2080    0       0       0       0   
pExp.TAD    0       0       0       0       53      53      53      53      0       0       0       0   
;


PARAMETER  elyPump0(bd,bo)    "Electricy required to pump water through conveyance (kWh per m3)"    /

FER_KYR        .NARYN            0.000
FER_KYR        .KRDRYA           0.000
FER_UZB        .NARYN            0.000
FER_UZB        .KRDRYA           0.000
FER_TAD        .SYRFER           0.000
FER_UZB        .SYRFER           0.000
FER_KYR        .SYRFER           0.000
SYR_UZB        .SYRMID           0.000
SYR_KAZ        .SYRMID           0.000
CHI_KAZ        .KELES            0.000
NOR_KAZ        .SYRDW1           0.000
NOR_KAZ        .SYRDW2           0.000
UPA_TAD        .VAKHSH           0.000
UPA_TAD        .PYANDJ           0.000
UPA_TAD        .KARFIR           0.000
UPA_AFG        .KUNDUZ           0.000
SOU_UZB        .AMUDWN           0.000
SOU_TUR        .AMUDWN           0.000
SUR_UZB        .AMUMID           0.000
LAK_GOL        .AMUMID           0.000
LAK_SYR        .SYRDW2           0.000
LAK_AMU        .AMUMID           0.000
LAK_ARN        .SYRDW2           0.000
LAK_ARS        .AMUDWN           0.000
Res_ZD         .AMUMID           0.000
Res_BUK        .AMUMID           0.000
Res_TAL        .AMUMID           0.000
Res_ARN        .SYRMID           0.000
Res_TMR        .AMUMID           0.000
/;

PARAMETER  elyPump1(bd,bo)    "Electricy required to pump water through conveyance (kWh per m3)"    /

FER_KYR        .NARYN            0.000
FER_KYR        .KRDRYA           0.000
FER_UZB        .NARYN            0.000
FER_UZB        .KRDRYA           0.000
FER_TAD        .SYRFER           0.000
FER_UZB        .SYRFER           0.000
FER_KYR        .SYRFER           0.000
SYR_UZB        .SYRMID           0.000
SYR_KAZ        .SYRMID           0.000
CHI_KAZ        .KELES            0.000
NOR_KAZ        .SYRDW1           0.000
NOR_KAZ        .SYRDW2           0.000
UPA_TAD        .VAKHSH           0.000
UPA_TAD        .PYANDJ           0.000
UPA_TAD        .KARFIR           0.000
UPA_AFG        .KUNDUZ           0.000
SOU_UZB        .AMUDWN           0.000
SOU_TUR        .AMUDWN           0.000
SUR_UZB        .AMUMID           0.000
LAK_GOL        .AMUMID           0.000
LAK_SYR        .SYRDW2           0.000
LAK_AMU        .AMUMID           0.000
LAK_ARN        .SYRDW2           0.000
LAK_ARS        .AMUDWN           0.000
Res_ZD         .AMUMID           0.000
Res_BUK        .AMUMID           0.000
Res_TAL        .AMUMID           0.000
Res_ARN        .SYRMID           0.000
Res_TMR        .AMUMID           0.000
/;




PARAMETERS conveyCost0(c) "Conveyance cost of system for each country (mUSD/year)" /

UZB     0
KAZ     0
TUR     0
TAD     0
KYR     0
AFG     0
/;


PARAMETERS conveyCost1(c) "Conveyance cost of improved system for each country (mUSD/year)" /

UZB     0
KAZ     0
TUR     0
TAD     0
KYR     0
AFG     0
/;

SET nukusIn0(rb,bd,bo) "Inflows into Nukus areas" /
    AMU. AMUUPS .VAKHSH    YES
    AMU. AMUUPS .PYANDJ    YES
    AMU. AMUUPS .KUNDUZ    YES
    AMU. AMUUPS .KARFIR    YES
    AMU. AMUMID .Res_SUR   YES
    SYR. NARYN  .Res_NA2   YES
    SYR. NARYN  .Src_NAR   YES
    SYR. SYRFER .Res_FER   YES
    SYR. SYRFER .KRDRYA    YES
    SYR. SYRMID .Res_CHA   YES
    SYR. SYRMID .Res_AHA   YES
    SYR. SYRMID .KELES     YES
    SYR. SYRDW2 .Res_ARY   YES
/ ;

SET nukusOut0(rb,bd,bo) "Outflows from Nukus areas" /
    AMU. SUR_UZB.AMUMID    YES
    AMU. SOU_UZB.AMUDWN    YES
    AMU. SOU_TUR.AMUDWN    YES
    AMU. Res_ZD .AMUMID    YES
    AMU. Res_TAL.AMUMID    YES
    AMU. Res_BUK.AMUMID    YES
    SYR. FER_UZB.KRDRYA    YES
    SYR. FER_UZB.NARYN     YES
    SYR. FER_UZB.SYRFER    YES
    SYR. SYR_UZB.SYRMID    YES
    SYR. SYR_KAZ.SYRMID    YES
    SYR. NOR_KAZ.SYRDW1    YES
    SYR. NOR_KAZ.SYRDW2    YES
    SYR. FER_KYR.NARYN     YES
    SYR. FER_KYR.SYRFER    YES
    SYR. FER_TAD.SYRFER    YES
    SYR. Res_ARN.SYRMID    YES
/;

