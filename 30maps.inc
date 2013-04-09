* Automatically created file, please enter changes into beam.xls
SET bCty(b,c) "Country-node map" / 
                                 
FER_UZB         .UZB             
SYR_UZB         .UZB             
CHI_UZB         .UZB             
SUR_UZB         .UZB             
KAS_UZB         .UZB             
ZAR_UZB         .UZB             
SOU_UZB         .UZB             
SYR_KAZ         .KAZ             
CHI_KAZ         .KAZ             
NOR_KAZ         .KAZ             
AMU_TUR         .TUR             
SOU_TUR         .TUR             
FER_KYR         .KYR             
FER_TAD         .TAD             
UPA_TAD         .TAD             
ZAR_TAD         .TAD             
UPA_AFG         .AFG             
Res_KAM         .KYR             
Res_TOK         .KYR             
Res_AND         .TAD             
Res_KAR         .TAD             
Res_CHA         .UZB             
Res_SHA         .KAZ             
Res_ROG         .TAD             
Res_NUR         .TAD             
Res_TMP         .UZB             
Res_DAS         .TAD             
Res_ZAR         .TAD             
Res_FER         .UZB             
Res_TMR         .UZB             
Res_AHA         .UZB             
Res_ARN         .UZB             
Res_KOK         .KAZ             
Res_ARY         .KAZ             
Res_KAF         .TAD             
Res_ZD          .TUR             
Res_TUR         .TUR             
Res_SUR         .UZB             
Res_KAS         .UZB             
Res_TAL         .UZB             
Res_BUK         .UZB             
Res_NAR         .KYR             
Res_FAR         .UZB             
Res_VAH         .TAD             
Res_CHI         .UZB             
Src_TOK         .KYR             
Src_NAR         .TAD             
Src_AND         .KYR             
Src_KAR         .KYR             
Src_FER         .UZB             
Src_SYR         .UZB             
Src_AHA         .UZB             
Src_CHI         .KAZ             
Src_KEL         .KAZ             
Src_ARY         .KAZ             
Src_ROG         .TAD             
Src_VAH         .TAD             
Src_PYD         .TAD             
Src_KAF         .AFG             
Src_ZAR         .UZB             
Src_SUR         .UZB             
Src_KAS         .UZB             
Src_AFG         .UZB             
Src_TUR         .TUR             
NARYN           .KYR             
KRDRYA          .KYR             
SYRFER          .UZB             
SYRMID          .UZB             
KELES           .KAZ             
SYRDW1          .KAZ             
SYRDW2          .KAZ             
VAKHSH          .TAD             
PYANDJ          .TAD             
KUNDUZ          .AFG             
KARFIR          .UZB             
AMUUPS          .UZB             
AMUMID          .UZB             
AMUDWN          .UZB             
Lak_AYD         .UZB             
Lak_SYR         .KAZ             
Lak_GOL         .UZB             
Lak_AMU         .TUR             
Lak_ARS         .UZB             
Lak_ARN         .KAZ             
/;

SET flow(bd,bo) "River flow arcs" / 
                                 
Res_KAM         .Src_TOK         
Res_AND         .Src_AND         
NARYN           .Src_NAR         
KRDRYA          .Src_KAR         
Res_FER         .Src_FER         
SYR_UZB         .Src_SYR         
Res_AHA         .Src_AHA         
KELES           .Src_KEL         
Res_CHA         .Src_CHI         
Res_ARY         .Src_ARY         
Res_ROG         .Src_ROG         
Res_NUR         .Src_VAH         
Res_DAS         .Src_PYD         
KUNDUZ          .Src_AFG         
Res_KAF         .Src_KAF         
Res_SUR         .Src_SUR         
Res_KAS         .Src_KAS         
Res_ZAR         .Src_ZAR         
Res_TUR         .Src_TUR         
SYRFER          .NARYN           
SYRFER          .KRDRYA          
Res_KAR         .SYRFER          
SYRMID          .KELES           
Res_SHA         .SYRMID          
SYRDW2          .SYRDW1          
Lak_ARN         .SYRDW2          
AMUUPS          .VAKHSH          
AMUUPS          .PYANDJ          
AMUUPS          .KUNDUZ          
AMUUPS          .KARFIR          
AMUMID          .AMUUPS          
Res_TMR         .AMUMID          
Lak_ARS         .AMUDWN          
Res_KOK         .SYRDW1          
/;

SET resv(bd,bo) "Arcs from reservoirs" / 
                                 
Res_TOK         .Res_KAM         
Res_NAR         .Res_TOK         
NARYN           .Res_NAR         
KRDRYA          .Res_AND         
SYRFER          .Res_FER         
FER_UZB         .Res_FER         
FER_TAD         .Res_FER         
CHI_UZB         .Res_AHA         
Res_CHI         .Res_CHA         
Res_FAR         .Res_KAR         
SYRMID          .Res_FAR         
SYRMID          .Res_AHA         
SYRMID          .Res_CHA         
SYR_UZB         .Res_ARN         
Lak_AYD         .Res_ARN         
SYRDW1          .Res_SHA         
SYRDW2          .Res_KOK         
SYRDW2          .Res_ARY         
Res_NUR         .Res_ROG         
Res_VAH         .Res_NUR         
VAKHSH          .Res_VAH         
PYANDJ          .Res_DAS         
AMUMID          .Res_SUR         
KAS_UZB         .Res_KAS         
ZAR_TAD         .Res_ZAR         
ZAR_UZB         .Res_ZAR         
KAS_UZB         .Res_TAL         
ZAR_UZB         .Res_BUK         
AMU_TUR         .Res_TUR         
AMU_TUR         .Res_ZD          
Res_TMR         .Res_TMP         
AMUDWN          .Res_TMR         
KARFIR          .Res_KAF         
Lak_AMU         .Res_ZAR         
CHI_UZB         .Res_CHI         
/;

SET intk(bd,bo) "Intake arcs" / 
                                 
FER_KYR         .NARYN           
FER_KYR         .KRDRYA          
FER_UZB         .NARYN           
FER_UZB         .KRDRYA          
FER_TAD         .SYRFER          
FER_UZB         .SYRFER          
FER_KYR         .SYRFER          
SYR_UZB         .SYRMID          
SYR_KAZ         .SYRMID          
CHI_KAZ         .KELES           
NOR_KAZ         .SYRDW1          
NOR_KAZ         .SYRDW2          
UPA_TAD         .VAKHSH          
UPA_TAD         .PYANDJ          
UPA_TAD         .KARFIR          
UPA_AFG         .KUNDUZ          
SOU_UZB         .AMUDWN          
SOU_TUR         .AMUDWN          
SUR_UZB         .AMUMID          
LAK_GOL         .AMUMID          
LAK_SYR         .SYRDW2          
LAK_AMU         .AMUMID          
Res_ZD          .AMUMID          
Res_BUK         .AMUMID          
Res_TAL         .AMUMID          
Res_ARN         .SYRMID          
Res_TMP         .AMUMID          
/;

SET  retf(bd,bo) "Return flow arcs" / 
                                 
SYRFER          .FER_UZB         
SYRMID          .SYR_UZB         
SYRMID          .CHI_UZB         
SYRDW2          .SUR_UZB         
AMUUPS          .KAS_UZB         
AMUMID          .ZAR_UZB         
AMUDWN          .SOU_UZB         
SYRMID          .SYR_KAZ         
SYRMID          .CHI_KAZ         
SYRDW2          .NOR_KAZ         
AMUDWN          .AMU_TUR         
AMUDWN          .SOU_TUR         
SYRFER          .FER_KYR         
SYRFER          .FER_TAD         
AMUUPS          .UPA_TAD         
AMUMID          .ZAR_TAD         
AMUUPS          .UPA_AFG         
LAK_SYR         .FER_UZB         
LAK_SYR         .SYR_UZB         
LAK_SYR         .CHI_UZB         
LAK_SYR         .SUR_UZB         
LAK_AMU         .KAS_UZB         
LAK_AMU         .ZAR_UZB         
LAK_AMU         .SOU_UZB         
LAK_SYR         .SYR_KAZ         
LAK_SYR         .CHI_KAZ         
LAK_SYR         .NOR_KAZ         
LAK_AMU         .AMU_TUR         
LAK_GOL         .SOU_TUR         
LAK_SYR         .FER_KYR         
LAK_SYR         .FER_TAD         
LAK_AMU         .UPA_TAD         
LAK_AMU         .ZAR_TAD         
LAK_AMU         .UPA_AFG         
/;

SET bSec(b,bs) "Basin section-node map" / 
                                 
FER_UZB         .SYRUPS          
SYR_UZB         .SYRMID          
CHI_UZB         .SYRMID          
SUR_UZB         .AMUMID          
KAS_UZB         .AMUMID          
ZAR_UZB         .AMUMID          
SOU_UZB         .AMUDWN          
SYR_KAZ         .SYRMID          
CHI_KAZ         .SYRMID          
NOR_KAZ         .SYRDWN          
AMU_TUR         .AMUKAI          
SOU_TUR         .AMUDWN          
FER_KYR         .SYRUPS          
FER_TAD         .SYRUPS          
UPA_TAD         .AMUUPS          
ZAR_TAD         .AMUMID          
UPA_AFG         .AMUUPS          
Res_KAM         .SYRUPS          
Res_TOK         .SYRUPS          
Res_AND         .SYRUPS          
Res_KAR         .SYRUPS          
Res_CHA         .SYRMID          
Res_SHA         .SYRMID          
Res_ROG         .AMUUPS          
Res_NUR         .AMUUPS          
Res_TMP         .AMUMID          
Res_DAS         .AMUUPS          
Res_ZAR         .AMUMID          
Res_FER         .SYRUPS          
Res_TMR         .AMUMID          
Res_AHA         .SYRMID          
Res_ARN         .SYRMID          
Res_KOK         .SYRDWN          
Res_ARY         .SYRDWN          
Res_KAF         .AMUUPS          
Res_ZD          .AMUMID          
Res_TUR         .AMUKAI          
Res_SUR         .AMUMID          
Res_KAS         .AMUMID          
Res_TAL         .AMUMID          
Res_BUK         .AMUMID          
Res_NAR         .SYRUPS          
Res_FAR         .SYRUPS          
Res_VAH         .SYRUPS          
Res_CHI         .SYRMID          
Src_TOK         .SYRUPS          
Src_NAR         .SYRUPS          
Src_AND         .SYRUPS          
Src_KAR         .SYRUPS          
Src_FER         .SYRUPS          
Src_SYR         .SYRMID          
Src_AHA         .SYRMID          
Src_CHI         .SYRMID          
Src_KEL         .SYRMID          
Src_ARY         .SYRDWN          
Src_ROG         .AMUUPS          
Src_VAH         .AMUUPS          
Src_PYD         .AMUUPS          
Src_KAF         .AMUUPS          
Src_ZAR         .AMUMID          
Src_SUR         .AMUMID          
Src_KAS         .AMUMID          
Src_AFG         .AMUUPS          
Src_TUR         .AMUKAI          
NARYN           .SYRUPS          
KRDRYA          .SYRUPS          
SYRFER          .SYRUPS          
SYRMID          .SYRMID          
KELES           .SYRMID          
SYRDW1          .SYRDWN          
SYRDW2          .SYRDWN          
VAKHSH          .AMUUPS          
PYANDJ          .AMUUPS          
KUNDUZ          .AMUUPS          
KARFIR          .AMUUPS          
AMUUPS          .AMUUPS          
AMUMID          .AMUMID          
AMUDWN          .AMUDWN          
Lak_AYD         .SYRMID          
Lak_SYR         .SYRDWN          
Lak_GOL         .AMUKAI          
Lak_AMU         .AMUMID          
Lak_ARS         .ARALSO          
Lak_ARN         .ARALNO          
/;

SET sXsecIn(bs,bd,bo) /  
                                                  
SYRMID          .SYRMID          .Res_FAR         
SYRDWN          .SYRDW1          .RES_SHA         
ARALNO          .Lak_ARN         .SYRDW2          
AMUMID          .AMUMID          .AMUUPS          
AMUDWN          .AMUDWN          .Res_TMR         
AMUKAI          .AMU_TUR         .Res_ZD          
ARALSO          .Lak_ARS         .AMUDWN          
/;

SET sXsecOut(bs,bd,bo) /  
                                                  
SYRUPS          .SYRMID          .Res_FAR         
SYRMID          .SYRDW1          .Res_SHA         
SYRDWN          .Lak_ARN         .SYRDW2          
AMUUPS          .AMUMID          .AMUUPS          
AMUMID          .AMUDWN          .Res_TMR         
AMUMID          .AMU_TUR         .Res_ZD         
AMUDWN          .Lak_ARS         .AMUDWN          
/;

PARAMETER jAF0(j) "Crops with sluggish land allocation" /
cot                             1.000000E+00    
wht             1.000000E+00    
ric             1.000000E+00    
alf             1.000000E+00    
fal             0.000000E+00    

/;

