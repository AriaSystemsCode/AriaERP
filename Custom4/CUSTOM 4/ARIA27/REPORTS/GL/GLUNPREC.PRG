*:************************************************************************
*:
*: Procedure file: GLUNPREC.RPR
*:               : Account reconciliation - Unposted entries
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: MAN
*:      Copyright (c) 
*:  Last modified:04/09/96
*:
*:  Procs & Fncts: 
*:               lfvFisYer  To validate the Fiscal Year
*:               lfvFisPrd  To validate the fiscal period
*:               lfwOldVal  To save the old value for some objects
*:               lfGetPic   To get the account code structure picture
*:               lfvActBy   To get the curr. year and period
*:               lfClearRep To clear the report
*:                
*: Tracking ID   : N48
*:************************************************************************
*:N48,1 MAN 04/08/96 Reconcile unposted entries
*E300789,7 WAM WALID 03/04/99 Make needed modifications according to the changes
*E300789,6 MAB 03/06/99 Make needed modifications according to the changes
*B603142,1 WALID Abou El-Magd (WAM) 09/06/99 Fix the bug (File does not exist) in summary&detail

SELECT GLTRNSDT
*--- To optomize the speed in case you selected 1 account
lcRest = IIF("GLTRNSDT.CACCTCODE" $ UPPER(lcRpExp) ,;
              ' REST WHILE GLTRNSDT.CACCTCODE = "'+laOGFxFlt[3,6]+'"','')
SEEK ALLTRIM(laOGFxFlt[3,6])

*--- If the user selected 1 of the apparel module
*--- Because the AAS updates the source module in this case with AS 
lcRpExp = STRTRAN(lcRpExp,'"ZA"','"AS"')
lcRpExp = STRTRAN(lcRpExp,'"ZM"','"AS"') 
lcRpExp = STRTRAN(lcRpExp,'"ZR"','"AS"') 
lcRpExp = STRTRAN(lcRpExp,'"ZB"','"AS"') 
lcRpExp = STRTRAN(lcRpExp,'"ZS"','"AS"') 
*B603142,1 Fix the bug (File does not exist) [Start]
*DO gfDispRe WITH lcRpForm, lcRest+IIF(EMPTY(lcRpExp),'',' FOR ' +lcRpExp)
DO gfDispRe WITH EVAL('lcRpForm'), lcRest+IIF(EMPTY(lcRpExp),'',' FOR ' +lcRpExp)
*B603142,1 Fix the bug (File does not exist) [End..]

*!************************************************************************
*!
*!      Function lfvFisYer
*!
*!************************************************************************
*

FUNCTION lfvFisYer

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

lcRpFicsYr = EVALUATE(SYS(18))

lcRpCurFld = VARREAD()
IF .NOT.EMPTY(lcRpFicsYr)  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *IF NOT USED("SYCFISHD") 
  *  SELECT 0
  *  USE (gcSysHome+'SYCFISHD') ORDER TAG compfyear
  *  llUesdBefo = .T.
  *ENDIF
  *SELECT SYCFISHD
  IF NOT USED("FISHD") 
    SELECT 0
    USE (gcDataDir+'FISHD') ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *** Search for the current company+year
    *E300789,6 Remove company Id from expression[Begin
    *IF ('?' $ &lcRpCurFld. .OR. !SEEK(gcPrnt_Cmp+lcRpFicsYr)) 
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr)) 
        *=gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
      &lcRpCurFld. = laRpRetFld[1]
      lcRpFicsYr   = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    *E300789,6 Remove company Id from expression[End
  
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
    *USE IN SYCFISHD
    USE IN FISHD
    *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvFisPrd
*!
*!************************************************************************
*
FUNCTION lfvFisPrd

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT. EMPTY(lcRpFicsYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = SELECT()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    
    *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
    
    *IF NOT USED("SYCFSPRD") 
    *  SELECT 0
    *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
    *  llUesdBefo = .T.
    *ENDIF
    *SELECT SYCFSPRD
    IF NOT USED("FSPRD") 
      SELECT 0
      USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
      llUesdBefo = .T.
    ENDIF
    SELECT FSPRD
    
    *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
    
    *** Search for the current company+year+Prd
    *E300789,7 [BEGIN]
    *IF ('?' $ &lcRpCurFld. .OR.;  
    *  !SEEK(gcAct_comp+ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
    *    =gfBrows([gcAct_comp+ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
        =gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)    
    *E300789,7 [END..]
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
      *USE IN SYCFSPRD
      USE IN FSPRD
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
      
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ENDIF
RETURN 


*!************************************************************************
*!
*!      FUNCTION lfwOldVal
*!
*!************************************************************************
*
FUNCTION lfwOldVal

lcOldVal=EVAL(VARREAD())

*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF NOT USED("SYCACCOD")
*  SELECT 0
*  USE &gcSysHome.SYCACCOD ORDER TAG COMPID
*  llUesdBefo = .T.
*ENDIF
*SELECT SYCACCOD
*E300789,7 [BEGIN] Make needed modifications according to the changes
* in ACCOD structure .
IF NOT USED("ACCOD")
  SELECT 0
*E USE &gcDataDir.ACCOD ORDER TAG COMPID
  USE &gcDataDir.ACCOD 
  llUesdBefo = .T.
ENDIF
SELECT ACCOD

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]

*E IF SEEK(gcAct_Comp)
  IF !EOF()
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  *lcRpSegMas = ALLTRIM(SYCACCOD.cacsmask)
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF
*E300789,7 [END..]
IF llUesdBefo       && .F.- this file used by the system
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  *USE IN SYCACCOD
  USE IN ACCOD
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMas



*!************************************************************************
*!
*!      FUNCTION : lfvActBy  
*!
*!************************************************************************
*
FUNCTION lfvActBy

IF !EMPTY(LAOGFXFLT[1,6]) AND !EMPTY(LAOGFXFLT[2,6]) 
  RETURN
ENDIF

lnCurAlias = SELECT()
llCompUsd=.f.
IF !USED('SYCCOMP')
  USE &gcSyshome.SYCCOMP IN SELECT(1)
  llCompUsd=.T.
ENDIF

SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(gcAct_Comp,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD
ENDIF

IF llCompUsd
  USE IN SYCCOMP
ENDIF

LAOGFXFLT[1,6] = LEFT(lcRetVal,4)
LAOGFXFLT[2,6] = RIGHT(lcRetVal,2)+lcElmSep+RIGHT(lcRetVal,2)  

SELECT (lnCurAlias)  

FUNCTION lfClearRep
