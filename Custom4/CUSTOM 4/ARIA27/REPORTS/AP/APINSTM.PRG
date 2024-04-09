*B500790,1  MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1               to print the commas   
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APINSTM.FRX
****************************************************************************

SELECT APINVAHD
SET ORDER TO TAG HTYPCOD 
SET ORDER TO TAG VENCODE IN APVENDOR
SET ORDER TO TAG INVVEND IN APINVHDR
SET RELATION TO apinvahd.cvendcode INTO APVENDOR ADDITIVE
SET RELATION TO apinvahd.cinvno+ apinvahd.cvendcode INTO APINVHDR ADDITIVE
SET SKIP TO APINVHDR

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
*!      Function: lfvInvoice
*!
*!**************************************************************************
* 
FUNCTION lfvInvoice

DO CASE
  CASE lcRpStatus = 'O'
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [ABS(APINVHDR.NINVAMNT)]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'N'    
    laOGHDFlt[2,4] = .T.    
    laOGHDFlt[2,5] = 'Greater Than'    
    laOGHDFlt[2,6] = [ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK)]    
    laOGHDFlt[2,7] = 'E'

  CASE lcRpStatus = 'P'
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [APINVHDR.NINVAMNT]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'N'    
    laOGHDFlt[2,4] = .T.    
    laOGHDFlt[2,5] = 'Like'    
    laOGHDFlt[2,6] = [APINVHDR.NINVPAID+APINVHDR.NINVADJ+APINVHDR.NINVDISTK]        
    laOGHDFlt[2,7] = 'E'

  CASE lcRpStatus = 'A'  
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [APINVHDR.CINVSTAT]
    laOGHDFlt[2,2] = 'F'    
    laOGHDFlt[2,3] = 'C'    
    laOGHDFlt[2,4] = .F.    
    laOGHDFlt[2,5] = 'Like'    
    laOGHDFlt[2,6] = 'V'    
    laOGHDFlt[2,7] = 'V'        
ENDCASE

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
*!      Function: lfvChecks
*!
*!**************************************************************************
*
FUNCTION lfvChecks

SELECT APCHECKS
lcSavOrder = SET('ORDER')   && Save old order
SET ORDER TO TAG BANKCHECK
lcFldNam = SYS(18)
IF SEEK(EVAL(SYS(18)))
  &lcFldNam=APCHECKS.CCHKACCT
ELSE
  IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
    DIMENSION laTemp[2]  
    laTemp = ''
    lcSavBrFld=lcBrFields
    lcSavTitle=lcFile_Ttl
  
    lcBrFields="CBNKCODE :H= 'Bank Code',;
                CCHKACCT :H= 'Check Code'"
    lcFile_Ttl= "Checks"

    =gfBrows(.F.,'CBNKCODE,CCHKACCT','laTemp')

    lcBrFields=lcSavBrFld
    lcFile_Ttl=lcSavTitle
    
    IF !EMPTY(laTemp[2])
      &lcFldNam=laTemp[2]
    ELSE
      &lcFldNam=SPACE(24)
    ENDIF
  ENDIF
ENDIF
SET ORDER TO &lcSavOrder
SELECT APPAYMNT

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
* 
FUNCTION lfRepShow
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
laOGObjCnt[3] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
