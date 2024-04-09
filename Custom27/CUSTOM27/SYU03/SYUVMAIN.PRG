*!**************************************************************************
*! Name      : SYUVMAIN.PRG
*! Developer : WALEED HAMED (WLD)
*! Date      : 11/11/2002
*! Purpose   : Studio Direct Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C102736,1
*!**************************************************************************
*C102736,1 WLD 11/11/2002 Pick up some additional information in 850 .
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfordhdvar
*! Developer : Waleed Hamed (WLD)
*! Date      : 11/11/2002
*! Purpose   : fill some fields in ordhdr
*!**************************************************************************
*! Passed Parameters : ---
*!**************************************************************************
*C102736,1
FUNCTION lfordhdvar
  REPLACE CCDIV      WITH MCCDIV       ,;
          CCDIVDESCR WITH MCCDIVDESCR  ,;
          CCSEASON   WITH MCCSEASON    ,;
          CCSSONDSCR WITH MCCSSONDSCR  ,;
          CCGROUP    WITH MCCGROUP     ,;
          CCGRPDSCR  WITH MCCGRPDSCR   ,;
          CCGROUP1   WITH MCCGROUP1    ,;
          CCGRP1DSCR WITH MCCGRP1DSCRC ,;
          CGROUP2    WITH MCCGROUP2    ,;
          CCGRP2DSCR WITH MCCGRP2DSCR  ,;
          CRNNUM     WITH MCRNNUM      ,;
          CCLABFORMT WITH MCCLABFORMT  ,;
          CCLABSTOCK WITH MCCLABSTOCK 
          
Replace   CCTOPSEW   WITH MCCTOPSEW    ,;
          CCCNTRYLAB WITH MCCCNTRYLAB  ,;
          CCLABMULTP WITH MCCLABMULTP  ,;
          CCLOTNUM   WITH MCCLOTNUM    ,;
          CCSTYLENUM WITH MCCSTYLENUM  ,;
          CCFABCONT1 WITH MCCFABCONT1  ,;
          CCFABCONT2 WITH MCCFABCONT2  ,;
          CCFABCONT3 WITH MCCFABCONT3  ,;
          CCFABCONT4 WITH MCCFABCONT4  ,;
          CCLABTEXT1 WITH MCCLABTEXT1  ,;
          CCLABTEXT2 WITH MCCLABTEXT2  ,;
          CCLABTEXT3 WITH MCCLABTEXT3  ,;
          CCLABTEXT4 WITH MCCLABTEXT4  ,;
          CCLABSIZE1 WITH MCCLABSIZE1  ,;
          CCLABSIZE2 WITH MCCLABSIZE2  ,;
          CCLABSIZE3 WITH MCCLABSIZE3  ,;
          CCPROTONUM WITH MCCPROTONUM  
          
Replace   CCPACKCODE WITH MCCPACKCODE  ,;
          CCPACKDSCR WITH MCCPACKDSCR  ,;
          CCCUSTCODE WITH MCCCUSTCODE  ,;
          CCCUSTNAME WITH MCCCUSTNAME  ,;
          CQUOTACAT1 WITH MCQUOTACAT1  ,;
          CQUOTACAT2 WITH MCQUOTACAT2  ,;
          CQUOTACAT3 WITH MCQUOTACAT3  ,;
          CCEXFACTDT WITH MCCEXFACTDT  ,;
          CPORTENTRY WITH MCPORTENTRY  ,;
          CCBUYNAME  WITH MCCBUYNAME   ,;
          CCBUYCODE  WITH MCCBUYCODE   ,;
          CCCNTRYNAM WITH MCCCNTRYNAM  
          
Replace   CCCNTRYORG WITH MCCCNTRYORG  ,;
          CCAGENTNAM WITH MCCAGENTNAM  ,;
          CCAGENTCOD WITH MCCAGENTCOD  ,;
          CCFACTNAM1 WITH MCCFACTNAM1  ,;
          CCFACTCOD1 WITH MCCFACTCOD1  ,;
          CCFACTNAM2 WITH MCCFACTNAM2  ,;
          CCFACTCOD2 WITH MCCFACTCOD2  ,;
	      CCARELAB   WITH MCCARELAB    ,;
          CPOLYBAG   WITH CPOLYBAG     ,;
          CCHANGSTK  WITH MCCHANGSTK  IN OrdHdr
          
*-- end of lfordhdvar.
*!**************************************************************************
*! Name      : lforddtvar
*! Developer : Waleed Hamed (WLD)
*! Date      : 11/11/2002
*! Purpose   : fill some fields in ordline
*!**************************************************************************
*! Passed Parameters : ---
*!**************************************************************************
*C102736,1

FUNCTION lforddtvar
  REPLACE CCCSTITMNM WITH MCCCSTITMNM ,;
          CCSTRETPRC WITH MCCSTRETPRC  IN OrdLine

*-- end of lforddtvar.

