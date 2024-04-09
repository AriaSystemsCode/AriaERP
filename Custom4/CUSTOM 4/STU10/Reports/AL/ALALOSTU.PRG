*:******************************************************************
*: Program file  : ALALOSTU
*: Program desc. : Customize Allocation Report For (Stunt) 
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL).
*:     Developer : AYMAN MAHMOUD AHMED (AYM)
*:          Date : 08/30/2006   
*:******************************************************************
*: Passed Parameters  : None
*:******************************************************************
*:******************************************************************
*: Example : DO ALALOSTU
*:******************************************************************

*!*	_SCREEN.Visible = .T.
*!*	ACTIVATE WINDOW TRACE
*!*	SUSP

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
loogScroll.cCROrientation = 'P'

DO gfDispRe WITH EVALUATE('lcRpName') 

*!*************************************************************
*! Name      : lfBldTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 08/30/2006
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
CREATE TABLE (OARIAAPPLICATION.WORKDIR+lcTempFile) (cPikTkt C(6), cOrder C(6), cStore C(8), dCompDate D(8),;
                                     dPickDate D(8),nPicQty N(6), cAccount C(5), cAccName C(30))

INDEX ON cPikTkt TAG lcTempFile ADDITIVE

*!*************************************************************
*! Name      : lfCollect
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 08/30/2006
*! Purpose   : Collect the Data of the report
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCollect
lcSeek=" .T. "


lcPK= lfCheckFilter(1, 'PIKTKT.PIKTKT')  	
llPK   = !EMPTY(lcPK) AND USED(lcPK) AND RECCOUNT(lcPK) > 0
IF llPK   
  SELECT (lcPK)
  INDEX ON PIKTKT TAG (lcPK)
  lcSeek=lcSeek+" AND SEEK(PIKTKT,'"+lcPK+"')"
ELSE
  IF TYPE("lcPK") = "C" AND USED(lcPK)
    USE IN (lcPK)
  ENDIF
  lcPK= ''
ENDIF

*Date Filter
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'PIKTKT.DATE'),1)
LDATE = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
HDATE = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)

IF !EMPTY(LDATE)
  lcSeek=lcSeek +" AND  BETWEEN(DATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"')) "
ELSE
  IF  !EMPTY(HDATE)
    lcSeek=lcSeek +" AND  DATE<=CTOD('"+HDATE+"') "
  ENDIF
ENDIF 

lcSeek=lcSeek+" AND STATUS <> 'X' AND STATUS <> 'C' "



SELECT PIKTKT
SCAN FOR &lcSeek 
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


SELECT (lcTempFile)
GOTO TOP



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


*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************

FUNCTION lfCheckFilter()
  LPARAMETERS lnArrayType, lcFilter
  LOCAL lcReturn, lnPOS 	
  DO CASE
	CASE lnArrayType = 1 
	  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	CASE lnArrayType = 2  
	  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	CASE lnArrayType = 3  
	  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	OTHERWISE :
		lcReturn = ""
  ENDCASE	
  RETURN lcReturn

