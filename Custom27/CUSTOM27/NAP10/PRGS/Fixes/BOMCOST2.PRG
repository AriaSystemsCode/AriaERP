*!*************************************************************
*! Name      : BomCost1
*! Developer : AKA
*! Date      : 11/05/2001
*! Purpose   : This program to compare the AP invoice against the BOMCOST file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*!*************************************************************


CLEA ALL
use r:\ARIA27\DBFS\02\APINVTKT IN 0  SHARE
use r:\ARIA27\DBFS\02\APVINVDT IN 0  SHARE 
use r:\ARIA27\DBFS\02\APINVHDR IN 0  SHARE
use r:\ARIA27\DBFS\02\POSLN    IN 0  SHARE  ORDER Poslnw 
USE r:\ARIA27\DBFS\02\BOMCOST  IN 0 SHARE
USE R:\ARIA27\FXPTMP\TESTCST IN 0  SHARE 

SELECT APINVHDR 
SET ORDER TO Vendinv 

SELECT APINVTKT 
SET ORDER TO 1

SELECT BOMCOST 
SET ORDER TO 2
SELECT ApVInvDt
SET ORDER TO 2        &&cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot
SELECT TestCst

SCAN for bocstdif <>0
wait window nowait str(recno())
  lcPo= TestCst.PO
  lcBomCost = TestCst.Type
  SELECT DIST SHIPNO ;
    FROM POSLN ;
   WHERE cstytype+po+cwarecode+account+store+style+trancd="P"+lcPO AND trancd="2" ;
    INTO CURSOR Ship

  SCAN  
      SELECT ApVInvDt
      SCAN FOR cimtyp+shipno+cbomtype = "S"+SHIP.SHIPNO+lcBomCost
          =zEROBomCost()  
        ENDSCAN
        SCAN FOR cimtyp+shipno+cbomtype = "S"+SHIP.SHIPNO+lcBomCost 
          =UpdBomCost()
        ENDSCAN
  ENDSCAN
  
  IF SEEK("I"+lcPO, "ApVInvDt")
    SELECT ApVInvDt
    SCAN REST WHILE cimtyp+ctktno = "I"+lcPo     FOR cbomtype = lcBomCost 
          =zEROBomCost()  
    ENDSCAN       
  ENDIF  

  IF SEEK("I"+lcPO, "ApVInvDt")
    SELECT ApVInvDt
    SCAN REST WHILE cimtyp+ctktno = "I"+lcPo     FOR cbomtype = lcBomCost 
      =UpdBomCost()
    ENDSCAN       
  ENDIF  

ENDSCAN


*****************************************************************************************
FUNCTION UpdBomCost
*****************************************************************************************
PRIVATE lcInvDtKey
lcInvDtKey= APVINVDT.cVendCode+APVINVDT.cApInvNo+APVINVDT.cAPVILNo
=SEEK (APVINVDT.cVendCode+APVINVDT.cApInvNo, "APINVHDR")
ldDate = APINVHDR.dInvDate
SELECT APInvTkt
=SEEK(lcInvDtKey)
lnCount = 0
SCAN REST  WHILE cVendCode+cApInvNo+cAPVILNo= lcInvDtKey 
lnCount = lnCount +  1
  IF EMPTY(APInvTkt.cRSession) 
    SELECT BOMCOST
    =SEEK(APVINVDT.cbomtype+"I"+APInvTkt.ctktno+;
          SPACE(19)+SPACE(6)+APVINVDT.cOprCode+SPACE(6)+SPACE(10)+;
          SPACE(6)+SPACE(6))      

    LOCATE REST WHILE ;
    cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
    APVINVDT.cbomtype+"I"+APInvTkt.ctktno+SPACE(19)+;
    SPACE(6)+APVINVDT.cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6) ;
    FOR cVendCode+cApInvNo = APVINVDT.cVendCode+APVINVDT.cApInvNo
    IF !FOUND() 
      APPEND BLANK
    ENDIF
      IF lnCount = 1

      ENDIF  
        REPLACE CBOMTYPE  WITH APVINVDT.cbomtype ,;
                CIMTYP    WITH "I"                   ,; 
                CTKTNO    WITH APInvTkt.ctktno   ,;
                ITEM      WITH ''                    ,;
                ICLR      WITH ''                    ,;
                MFGCODE   WITH APVINVDT.cOprCode ,;
                CWARECODE WITH ''                    ,;
                CDYELOT   WITH ''                    ,;
                CRSESSION WITH SPACE(6)  ,;
                CVENDCODE WITH APVINVDT.cVendCode ,;
                CAPINVNO  WITH APVINVDT.cApInvNo,;
                CISESSION WITH ''        ,;
                SHIPNO    WITH IIF(ApvInvDt.cImTyp='S',SHIP.SHIPNO, '' ),;
                DTRANDATE WITH ldDate,;
                CCOSTTYPE WITH APVINVDT.cCostType  ,;
                NTOTQTY   WITH NTOTQTY   + APINVTKT.nAPTAplQty ,;
                NTOTCST   WITH NTOTCST   + APInvTkt.nAPAplAmnt ,;
                NUNITCST  WITH NTOTCST/NTOTQTY  ,;
                NTOTACST  WITH NTOTCST  ,;
                NUNITACST WITH NUNITCST ,;
                Actualize WITH 'N'

  ENDIF 
  
  IF !EMPTY(APInvTkt.cRSession)
    SELECT BOMCOST
    =SEEK(APVINVDT.cbomtype+"I"+APInvTkt.ctktno+SPACE(19)+SPACE(6)+APVINVDT.cOprCode+;
         SPACE(6)+SPACE(10)+APInvTkt.cRSession+SPACE(6))
    LOCATE REST WHILE ;
      cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      APVINVDT.cbomtype+"I"+APInvTkt.ctktno+SPACE(19)+;
      SPACE(6)+APVINVDT.cOprCode+SPACE(6)+SPACE(10)+APInvTkt.cRSession+SPACE(6) ;
      FOR cVendCode+cApInvNo =  APVINVDT.cVendCode+APVINVDT.cApInvNo
      IF !FOUND()
          APPEND BLANK
       ENDIF   
      IF lnCount = 1
      ENDIF  

          REPLACE CBOMTYPE  WITH APVINVDT.cbomtype ,;
                  CIMTYP    WITH "I"                   ,;
                  CTKTNO    WITH APInvTkt.ctktno   ,;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH APVINVDT.cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH APInvTkt.cRSession,;
                  CVENDCODE WITH APVINVDT.cVendCode,;
                  CAPINVNO  WITH APVINVDT.cApInvNo  ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH IIF(ApvInvDt.cImTyp='S',SHIP.SHIPNO, '' ),;
                  DTRANDATE WITH ldDate,;
                  CCOSTTYPE WITH APVINVDT.cCostType ,;
                  NTOTQTY   WITH NTOTQTY   +APInvTkt.nApTAplQty ,;
                  NTOTCST   WITH NTOTCST   +APInvTkt.nApAplAmnt ,;
                  NUNITCST  WITH IIF(NTOTQTY=0,0,NTOTCST/NTOTQTY)  ,;
                  NTOTACST  WITH NTOTCST  ,;
                  NUNITACST WITH NUNITCST ,;
                  Actualize WITH 'Y'

   ENDIF  && End of If !EMPTY(APInvTkt.cRSession)
ENDSCAN

RETURN 



FUNCTION zEROBomCost
*****************************************************************************************
PRIVATE lcInvDtKey
lcInvDtKey= APVINVDT.cVendCode+APVINVDT.cApInvNo+APVINVDT.cAPVILNo
=SEEK (APVINVDT.cVendCode+APVINVDT.cApInvNo, "APINVHDR")
ldDate = APINVHDR.dInvDate
SELECT APInvTkt
=SEEK(lcInvDtKey)
lnCount = 0
SCAN REST  WHILE cVendCode+cApInvNo+cAPVILNo= lcInvDtKey 
lnCount = lnCount +  1
  IF EMPTY(APInvTkt.cRSession) 
    SELECT BOMCOST
    =SEEK(APVINVDT.cbomtype+"I"+APInvTkt.ctktno+;
          SPACE(19)+SPACE(6)+APVINVDT.cOprCode+SPACE(6)+SPACE(10)+;
          SPACE(6)+SPACE(6))      

    LOCATE REST WHILE ;
    cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
    APVINVDT.cbomtype+"I"+APInvTkt.ctktno+SPACE(19)+;
    SPACE(6)+APVINVDT.cOprCode+SPACE(6)+SPACE(10)+SPACE(6)+SPACE(6) ;
    FOR cVendCode+cApInvNo = APVINVDT.cVendCode+APVINVDT.cApInvNo
    IF !FOUND() 
      APPEND BLANK
    ENDIF
      IF lnCount = 1

      ENDIF  
        REPLACE CBOMTYPE  WITH APVINVDT.cbomtype ,;
                CIMTYP    WITH "I"                   ,; 
                CTKTNO    WITH APInvTkt.ctktno   ,;
                ITEM      WITH ''                    ,;
                ICLR      WITH ''                    ,;
                MFGCODE   WITH APVINVDT.cOprCode ,;
                CWARECODE WITH ''                    ,;
                CDYELOT   WITH ''                    ,;
                CRSESSION WITH SPACE(6)  ,;
                CVENDCODE WITH APVINVDT.cVendCode ,;
                CAPINVNO  WITH APVINVDT.cApInvNo,;
                CISESSION WITH ''        ,;
                SHIPNO    WITH IIF(ApvInvDt.cImTyp='S',SHIP.SHIPNO, '' ),;
                DTRANDATE WITH ldDate,;
                CCOSTTYPE WITH APVINVDT.cCostType  ,;
                NTOTQTY   WITH 0 ,;
                NTOTCST   WITH 0,;
                NUNITCST  WITH 0,;
                NTOTACST  WITH 0,;
                NUNITACST WITH 0,;
                Actualize WITH 'N'

  ENDIF 
  
  IF !EMPTY(APInvTkt.cRSession)
    SELECT BOMCOST
    =SEEK(APVINVDT.cbomtype+"I"+APInvTkt.ctktno+SPACE(19)+SPACE(6)+APVINVDT.cOprCode+;
         SPACE(6)+SPACE(10)+APInvTkt.cRSession+SPACE(6))
    LOCATE REST WHILE ;
      cBomType+cIMTyp+cTktNo+Item+IClr+MfgCode+cWareCode+cDyelot+cRSession+cISession =;
      APVINVDT.cbomtype+"I"+APInvTkt.ctktno+SPACE(19)+;
      SPACE(6)+APVINVDT.cOprCode+SPACE(6)+SPACE(10)+APInvTkt.cRSession+SPACE(6) ;
      FOR cVendCode+cApInvNo =  APVINVDT.cVendCode+APVINVDT.cApInvNo
      IF !FOUND()
          APPEND BLANK
       ENDIF   
      IF lnCount = 1
      ENDIF  

          REPLACE CBOMTYPE  WITH APVINVDT.cbomtype ,;
                  CIMTYP    WITH "I"                   ,;
                  CTKTNO    WITH APInvTkt.ctktno   ,;
                  ITEM      WITH ''                    ,;
                  ICLR      WITH ''                    ,;
                  MFGCODE   WITH APVINVDT.cOprCode ,;
                  CWARECODE WITH ''                    ,;
                  CDYELOT   WITH ''                    ,;
                  CRSESSION WITH APInvTkt.cRSession,;
                  CVENDCODE WITH APVINVDT.cVendCode,;
                  CAPINVNO  WITH APVINVDT.cApInvNo  ,;
                  CISESSION WITH ''        ,;
                  SHIPNO    WITH IIF(ApvInvDt.cImTyp='S',SHIP.SHIPNO, '' ),;
                  DTRANDATE WITH ldDate,;
                  CCOSTTYPE WITH APVINVDT.cCostType ,;
                  NTOTQTY   WITH 0,;
                  NTOTCST   WITH 0,;
                  NUNITCST  WITH 0,;
                  NTOTACST  WITH 0,;
                  NUNITACST WITH 0,;
                  Actualize WITH 'Y'

   ENDIF  && End of If !EMPTY(APInvTkt.cRSession)
ENDSCAN

RETURN 



