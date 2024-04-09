*!**************************************************************************
*! Name      : ASMAIN.PRG                           *B803629  
*! Developer : MHM 
*! Date      : 09/05/2000
*! Purpose   : Asian American DBA Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modifications : 
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars


lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*!***************************************************************
*! Name      : lfSTY_SAV                             *B803629
*! Developer : MHM
*! Date      : 09/06/2000
*! Purpose   : Save Custom fields in Style file.
*!***************************************************************
*! Program   : 'ICSTYLE' -> Customer screen.
*! Event     : 'STY_SAV' -> Save (custom fields).
*!                          Save in all colors if there. 
*!***************************************************************
*! Returns   : None.
*!***************************************************************
FUNCTION lfSTY_SAV  
PRIVATE lcSaveMaj , lcSSty , laUpdate
DECLARE laUpdate[5]

IF !EMPTY(lcFrmStyle)
  lnAlias=select(0)
  SELECT STYLE
  SEEK lcFrmStyle
  lcsavemaj = PADR(STYLE.CSTYMAJOR,lnstylewid)
  lnUsrItm1 = ASCAN(laUsrFields,"CKIDSZGRP")
  lnUsrItm2 = ASCAN(laUsrFields,"CSTYGRP")
  lnUsrItm3 = ASCAN(laUsrFields,"NMSRP")
  lnUsrItm4 = ASCAN(laUsrFields,"CSTCONTWT")
  lnUsrItm5 = ASCAN(laUsrFields,"CHRMCOD")
  lcUsrVal1 = IIF(lnUsrItm1 > 0 , CKIDSZGRP , '')
  lcUsrVal2 = IIF(lnUsrItm2 > 0 , CSTYGRP , '')
  lnUsrVal3 = IIF(lnUsrItm3 > 0 , NMSRP , 0)
  lcUsrVal4 = IIF(lnUsrItm4 > 0 , CSTCONTWT , '')
  lcUsrVal5 = IIF(lnUsrItm5 > 0 , CHRMCOD , '')
  SCAN WHILE lcFrmStyle=lcsavemaj
  IF !(lcsavemaj = PADR(STYLE.CSTYMAJOR,lnstylewid))
    EXIT
  ENDIF  
  lcSSty = style
  *lnUsrItm1 = ASCAN(laUsrFields,"CKIDSZGRP")
  IF lnUsrItm1 > 0
    lnUsrItm1 = ASUBSCRIPT(laUsrFields ,lnUsrItm1 , 1)  
    IF  (laUsrFields[lnUsrItm1,6] = lcUsrVal1)
      *laUsrFields[lnUsrItm1,6] = CKIDSZGRP
      laUpdate[1] = CKIDSZGRP
    ELSE
      laUpdate[1] = laUsrFields[lnUsrItm1,6]
    ENDIF
  ENDIF
    
  *lnUsrItm2 = ASCAN(laUsrFields,"CSTYGRP")
  IF lnUsrItm2 > 0
    lnUsrItm2 = ASUBSCRIPT(laUsrFields ,lnUsrItm2 , 1)  
    IF  (laUsrFields[lnUsrItm2,6] = lcUsrVal2)
      *laUsrFields[lnUsrItm2,6] = CSTYGRP
      laUpdate[2] = CSTYGRP
    ELSE
      laUpdate[2] = laUsrFields[lnUsrItm2,6]
    ENDIF
  ENDIF 
   
  *lnUsrItm3 = ASCAN(laUsrFields,"NMSRP")
  IF lnUsrItm3 > 0
    lnUsrItm3 = ASUBSCRIPT(laUsrFields ,lnUsrItm3 , 1)  
    IF  (laUsrFields[lnUsrItm3,6] = lnUsrVal3)
      *laUsrFields[lnUsrItm3,6] = NMSRP
      laUpdate[3] = NMSRP
    ELSE
      laUpdate[3] = laUsrFields[lnUsrItm3,6]
    ENDIF
  ENDIF  
  
  *lnUsrItm4 = ASCAN(laUsrFields,"CSTCONTWT")
  IF lnUsrItm4 > 0
    lnUsrItm4 = ASUBSCRIPT(laUsrFields ,lnUsrItm4 , 1)  
    *IF  (laUsrFields[lnUsrItm4,6] <> CSTCONTWT)
    IF  (laUsrFields[lnUsrItm4,6] = lcUsrVal4)
      *laUsrFields[lnUsrItm4,6] = CSTCONTWT
      laUpdate[4] = CSTCONTWT
    ELSE
      laUpdate[4] = laUsrFields[lnUsrItm4,6]
    ENDIF  
  ENDIF  
  *lnUsrItm5 = ASCAN(laUsrFields,"CHRMCOD")
  IF lnUsrItm5 > 0
    lnUsrItm5 = ASUBSCRIPT(laUsrFields ,lnUsrItm5 , 1)  
    *IF (laUsrFields[lnUsrItm5,6] <> CHRMCOD)
    IF  (laUsrFields[lnUsrItm5,6] = lcUsrVal5)
      *laUsrFields[lnUsrItm5,6] = CHRMCOD
      laUpdate[5] = CHRMCOD
    ELSE
      laUpdate[5] = laUsrFields[lnUsrItm5,6]
    ENDIF  
  ENDIF  
  SEEK lcMajor+SUBSTR(STYLE,lnstylewid+1,19)
  =RLOCK()
  *REPLACE CKIDSZGRP WITH laUsrFields[lnUsrItm1,6],;
          CSTYGRP   WITH laUsrFields[lnUsrItm2,6],;
          NMSRP     WITH laUsrFields[lnUsrItm3,6],;
          CSTCONTWT WITH laUsrFields[lnUsrItm4,6],;
          CHRMCOD   WITH laUsrFields[lnUsrItm5,6]
  =lfUpdtCust("CKIDSZGRP,CSTYGRP,NMSRP,CSTCONTWT,CHRMCOD")

  UNLOCK
  SEEK lcSSty
ENDSCAN
lcFrmStyle = SPACE(1)

ELSE
  lnUsrItm1 = ASCAN(laUsrFields,"CKIDSZGRP")
  IF lnUsrItm1 > 0
    lnUsrItm1 = ASUBSCRIPT(laUsrFields ,lnUsrItm1 , 1)  
    laUpdate[1] = laUsrFields[lnUsrItm1,6]
  ENDIF
    
  lnUsrItm2 = ASCAN(laUsrFields,"CSTYGRP")
  IF lnUsrItm2 > 0
    lnUsrItm2 = ASUBSCRIPT(laUsrFields ,lnUsrItm2 , 1)  
    laUpdate[2] = laUsrFields[lnUsrItm2,6]
  ENDIF 
   
  lnUsrItm3 = ASCAN(laUsrFields,"NMSRP")
  IF lnUsrItm3 > 0
    lnUsrItm3 = ASUBSCRIPT(laUsrFields ,lnUsrItm3 , 1)  
    laUpdate[3] = laUsrFields[lnUsrItm3,6]
  ENDIF  
  
  lnUsrItm4 = ASCAN(laUsrFields,"CSTCONTWT")
  IF lnUsrItm4 > 0
    lnUsrItm4 = ASUBSCRIPT(laUsrFields ,lnUsrItm4 , 1)  
    laUpdate[4] = laUsrFields[lnUsrItm4,6]
  ENDIF  

  lnUsrItm5 = ASCAN(laUsrFields,"CHRMCOD")
  IF lnUsrItm5 > 0
    lnUsrItm5 = ASUBSCRIPT(laUsrFields ,lnUsrItm5 , 1)  
    laUpdate[5] = laUsrFields[lnUsrItm5,6]
  ENDIF  

  SELECT STYLE
  SEEK lcMajor+lcNMKey
  SCAN WHILE Style=lcMajor+lcNMKey
    =RLOCK()
    *REPLACE CKIDSZGRP WITH laUsrFields[lnUsrItm1,6],;
          CSTYGRP   WITH laUsrFields[lnUsrItm2,6],;
          NMSRP     WITH laUsrFields[lnUsrItm3,6],;
          CSTCONTWT WITH laUsrFields[lnUsrItm4,6],;
          CHRMCOD   WITH laUsrFields[lnUsrItm5,6]
    =lfUpdtCust("CKIDSZGRP,CSTYGRP,NMSRP,CSTCONTWT,CHRMCOD")
    
    UNLOCK
ENDSCAN

ENDIF

SELECT (lnAlias)
RETURN

*!***************************************************************
*! Name      : lfINI_Var1                              *C200076,1
*! Developer : MHM
*! Date      : 09/05/2000
*! Purpose   : Initialize STYLE file Custom fields defaults.
*!***************************************************************
*! Program   : 'ICSTYLE'  -> Customer screen.
*! Event     : 'INI_VAR' -> Initialize variables (custom fields).
*!***************************************************************
*! Returns   : None.
*!***************************************************************
FUNCTION lfINI_Var1

IF !EMPTY(lcFrmStyle)
  lnAlias=select(0)
  SELECT STYLE

  SEEK lcFrmStyle 
  lnUserItem = ASCAN(laUsrFields,"CKIDSZGRP")
  IF lnUserItem > 0
    lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
    laUsrFields[lnUserItem,6] = gfCodDes(STYLE.CKIDSZGRP, 'CKIDSZGRP ')
  ENDIF  
  lnUserItem = ASCAN(laUsrFields,"CSTYGRP")
  IF lnUserItem > 0
    lnUserItem = ASUBSCRIPT(laUsrFields ,lnUserItem , 1)  
    laUsrFields[lnUserItem,6] = gfCodDes(STYLE.CKIDSZGRP, 'CSTYGRP   ')
  ENDIF  
  SELECT (lnAlias)
ELSE

  lnUsrItm3 = ASCAN(laUsrFields,"NMSRP")
  IF lnUsrItm3 > 0
    lnUsrItm3 = ASUBSCRIPT(laUsrFields ,lnUsrItm3 , 1)  
    laUsrFields[lnUsrItm3,6] = 0.00
  ENDIF  

ENDIF
RETURN

*!***************************************************************
*! Name      : lfFIL_VAR                             *B803629
*! Developer : MHM
*! Date      : 09/06/2000
*! Purpose   : Fill Custom fields in Style file.
*!***************************************************************
*! Program   : 'ICSTYLE' -> Customer screen.
*! Event     : 'FIL_VAR' -> fill (custom fields).
*!***************************************************************
*! Returns   : None.
*!***************************************************************
FUNCTION lfFIL_VAR   
IF !EMPTY(lcFrmStyle)
  lnAlias=select(0)
  SELECT STYLE
  SEEK lcFrmStyle
  lnUsrItm1 = ASCAN(laUsrFields,"CKIDSZGRP")
  IF lnUsrItm1 > 0
    lnUsrItm1 = ASUBSCRIPT(laUsrFields ,lnUsrItm1 , 1)  
    laUsrFields[lnUsrItm1,6] = gfCodDes(STYLE.CKIDSZGRP, 'CKIDSZGRP')
  ENDIF  
  lnUsrItm2 = ASCAN(laUsrFields,"CSTYGRP")
  IF lnUsrItm2 > 0
    lnUsrItm2 = ASUBSCRIPT(laUsrFields ,lnUsrItm2 , 1)  
    laUsrFields[lnUsrItm2,6] = gfCodDes(STYLE.CKIDSZGRP, 'CSTYGRP')
  ENDIF  
  lnUsrItm3 = ASCAN(laUsrFields,"NMSRP")
  IF lnUsrItm3 > 0
    lnUsrItm3 = ASUBSCRIPT(laUsrFields ,lnUsrItm3 , 1)  
    laUsrFields[lnUsrItm3,6] = NMSRP
  ENDIF  
  lnUsrItm4 = ASCAN(laUsrFields,"CSTCONTWT")
  IF lnUsrItm4 > 0
    lnUsrItm4 = ASUBSCRIPT(laUsrFields ,lnUsrItm4 , 1)  
    laUsrFields[lnUsrItm4,6] = CSTCONTWT
  ENDIF  
  lnUsrItm5 = ASCAN(laUsrFields,"CHRMCOD")
  IF lnUsrItm5 > 0
    lnUsrItm5 = ASUBSCRIPT(laUsrFields ,lnUsrItm5 , 1)  
    laUsrFields[lnUsrItm5,6] = CHRMCOD
  ENDIF  

  SELECT (lnAlias)
ENDIF
RETURN

*!***************************************************************
*! Name      : lfUpdtCust                             *B803629
*! Developer : MHM
*! Date      : 11/07/2000
*! Purpose   : Save Custom fields.
*!***************************************************************
*! Program   : 'ICSTYLE' -> Customer screen.
*!***************************************************************
*! Returns   : None.
*!***************************************************************
*!
FUNCTION lfUpdtCust
PARAMETERS lcAllFlds
PRIVATE lcAllFlds , laAllFlds , lnCount
DECLARE laAllFlds[1]
=gfSubStr(lcAllFlds,@laAllFlds,",")       
IF !EMPTY(laAllFlds)
  lnCount = 0
  FOR lnCount = 1 TO ALEN(laAllFlds,1)
    IF TYPE(laAllFlds[lnCount]) # "U"
      *lcCount = "lnUsrItm"  + STR(lnCount,1)
      REPLACE &laAllFlds[lnCount] WITH laUpdate[lnCount]
    ENDIF  
  ENDFOR
ENDIF
*-- end of lfUpdtCust.