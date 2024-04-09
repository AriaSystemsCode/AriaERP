*:******************************************************************
*: Program file  : ALALOSTU
*: Program desc. : Customize Allocation Report For (Stunt) C102293
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL).
*:     Developer : HEND GHANEM (HBG)
*:          Date : 08/08/2001   
*:******************************************************************
*: Passed Parameters  : None
*:******************************************************************
*: Calls         : lfOpnFile()
*:               : lfDoGrd()  
*:               : lfBldTmp()
*:               : lfPrnRpt()
*:               : lfvPiktkt()
*:               : lfSelect()
*:******************************************************************
*: Example : DO ALALOSTU
*:******************************************************************



*-- if user change filter expression.(We Collect the Data of the report again)
IF llOGFltCh
  *-- Create Tables needed for this report
  =lfBldTmp()
  *-- Collect data
  =lfCollect()
ENDIF


SELECT &lcTempFile
SET ORDER TO lcTempFile
GO TOP

DO gfDispRe WITH EVALUATE('lcRpName') 

*!*************************************************************
*! Name      : lfBldTmp
*! Developer : HEND GHANEM (HBG)
*! Date      : 08/08/2001   
*! Purpose   : To build the temp file which the data will be 
*!             printed from 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfBldTmp()
*!*************************************************************
FUNCTION lfBldTmp

*-- Creating the temporary file that will used in printing the report
CREATE TABLE (gcWorkDir+lcTempFile) (cPikTkt C(6), cOrder C(6), cStore C(8), dCompDate D(8),;
                                     dPickDate D(8),nPicQty N(6), cAccount C(5), cAccName C(30))

INDEX ON cPikTkt TAG lcTempFile ADDITIVE

*!*************************************************************
*! Name      : lfCollect
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Collect the Data of the report
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCollect

*-- Building the temporary file

SELECT PIKTKT
SCAN FOR &lcRpExp AND STATUS <> 'X' AND STATUS <> 'C' 
  =SEEK ('O'+PIkTkt.Order,'OrdHdr')
  =SEEK ('M'+PikTkt.Account,'Customer')

  INSERT INTO (lcTempFile) (cPikTkt,cOrder,dPickDate,cAccount,cStore,dCompDate,cAccName);
                 VALUES    (PIKTKT.PikTkt,PIKTKT.Order,PIKTKT.Date,PIKTKT.Account,;
                            PIKTKT.Store,OrdHdr.Complete,Customer.StName)


  IF OrdHdr.Multi = "Y"
    SELECT OrdLine

    lcOldOrd = ORDER()
    SET ORDER TO Ordlinst

    IF SEEK('O'+PIkTkt.Order+PIkTkt.Store)
      lnTotPik = 0
      SCAN WHILE cOrdType + Order+ Store + Style +STR(LineNo,6) = 'O' + PIkTkt.Order + PIkTkt.Store 
        IF &lcTempFile..cPIKTKT = PIKTKT
          lnTotPik = lnTotPik + TotPik
        ENDIF
      ENDSCAN
      
      SELECT (lcTempFile)
      REPLACE nPicQty WITH lnTotPik
    
      SET ORDER TO TAG lcOldOrd IN OrdLine
    ENDIF  
  ELSE

    SELECT OrdLine
  
    =SEEK('O'+PIkTkt.Order)
    lnTotPik = 0

    SCAN WHILE  cOrdType + Order = 'O' + PIkTkt.Order
      IF &lcTempFile..cPIKTKT = PIKTKT
        lnTotPik = lnTotPik + TotPik
      ENDIF
    ENDSCAN

    SELECT (lcTempFile)
    REPLACE nPicQty WITH lnTotPik
  ENDIF
  
  SELECT PIKTKT  
ENDSCAN          

WAIT CLEAR

SELECT (lcTempFile)
GOTO TOP


*!*************************************************************
*! Name      : lfvPikTkt
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Valid function of the PikTkt
*!*************************************************************
*! Called from : Option grid [Account Get field]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikTkt

lcObjName = SYS(18)      
lcObjVal = EVALUATE(SYS(18))
laOldVal = EVALUATE(SYS(18))  

SELECT PIKTKT
SET FILTER TO STATUS <> 'X'

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'PIKTKT'))
  lcObjVal = gfPkTkBrow(@lcObjVal,.T.)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : HEND GHANEM (HBG)
*! Date      : 07/29/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lnDatapos = ASCAN(laOGFxFlt,'PIKTKT.DATE')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate)) + '|' + ALLTRIM(DTOC(gdSysDate))     
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF

*-- R_WIDTH  Variable that hold the report width to the print
R_WIDTH = 'N'
