*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*B604005,1 MHM 11/02/00 Fix bug of Numeric overflow in AP reports (modified in reports only)
*B604005,1 MHM          we modify the folloing fields {invoice amount,previous payment}
****************************************************************************
SELECT APINVHDR
SET ORDER TO TAG VENCODE IN APVENDOR
SET RELATION TO apinvhdr.cvendcode INTO APVENDOR ADDITIVE

DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp
SET RELATION TO

*!**************************************************************************
*!
*!      Function: lfvVend
*!
*!**************************************************************************
*
FUNCTION lfvVend

*** save the current alias. ***
lcSavAlias = ALIAS()
lcFldNam=SYS(18)
*** if not empty of vendor or the mouse clicked. ***
IF !EMPTY(EVAL(SYS(18))) 
  SELECT APVENDOR
  lcSavOrder = SET('ORDER')   && Save old order
  SET ORDER TO TAG VENCODE
  IF SEEK(EVAL(SYS(18)))
    &lcFldNam=CVENDCODE
  ELSE
    *** Define an array get Vendors codes and company.***
    IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
      DIMENSION laTemp[2]
      laTemp = '' && fill the array.
      *** Save the old fields and title.***   
      lcSavBrFld=lcBrFields 
      lcSavTitle=lcFile_Ttl 
    
      *** Get new fields name and title.***    
      lcBrFields="CVENDCODE :H= 'Vendor',;
                  CVENCOMP  :H= 'Company'"
      lcFile_Ttl="Vendors"
      *** Browse ***
      =gfBrows(.F.,'CVENDCODE,CVENCOMP','laTemp')
      *** Get the old fields name and title.***    
      lcBrFields=lcSavBrFld
      lcFile_Ttl=lcSavTitle
      *** if not empty of browse. ***
      IF !EMPTY(laTemp[1])
        &lcFldNam  = laTemp[1]  && get the vendor code.
      ELSE                      && if empty
        &lcFldNam  = " "           
      ENDIF
    ENDIF  
  ENDIF
ENDIF
SELECT(lcSavAlias) 

*!**************************************************************************
*!
*!      Function: lfvparior
*!
*!**************************************************************************
*
*
FUNCTION lfvparior
lcFldNam=SYS(18)
IF !INLIST(EVAL(SYS(18)),'0','1','2','3','4','5','6','7','8','9')
  &lcFldNam  = " "
ENDIF

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
*
FUNCTION lfRepShow

*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
laOGObjCnt[2] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")
*E300296,1 End

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
