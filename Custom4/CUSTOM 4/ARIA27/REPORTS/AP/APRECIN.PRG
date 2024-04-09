*B500790,1  MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1               to print the commas   
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APRECIND.FRX
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
******************************************************************************
SELECT APINVAHD
SET ORDER TO TAG HTYPCOD 
IF lcRpForm=="APRECIND"
  SET ORDER TO TAG VENCODE IN APVENDOR
  SET ORDER TO TAG DTYPCOD IN APINVADT
  SET RELATION TO apinvahd.cautmtype+ apinvahd.cautmcode INTO APINVADT ADDITIVE
  SET RELATION TO apinvahd.cvendcode INTO APVENDOR ADDITIVE
  SET SKIP TO APINVADT
ELSE
  SET ORDER TO TAG VENCODE IN APVENDOR
  SET RELATION TO apinvahd.cvendcode INTO APVENDOR ADDITIVE
ENDIF

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
*E300296,1 M.H 12/18/95 Add the currency to the AP reports.
*MAN
*laOGObjCnt[6] = gfGetMemVar('LLMULCURR')
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
