*:****************************************************************
*: Program file  : APBNKRS
*: Program desc. : Open Bank Reconciliation option grid.
*: System        : Aria Apparel System - Version 40.
*: Module        : (AP)
*: Developer     : SABER A.Razek -  (SAB)
*: Date          : 01/30/2012
*: Tracking#     : E303041
*:****************************************************************
*: Parameters    : lcRequestID    ==> 
*:                 lcXMLFileName  ==> 
*:                 ClientId       ==> 
*:****************************************************************
*:Modifications  :
*:****************************************************************




*!************************************************************************
*!
*!      Function lfvToDay
*!
*!************************************************************************
* Check if the ldRpFrDat is smaller than ldRpToDat or not
Function lfvToDay
PARAMETERS lcVar

IF !EMPTY(ldRpFrDat) AND !EMPTY(ldRpToDat) AND ldRpFrDat > ldRpToDat
  ** MESSAGE : "From Data can not be less than Through Date"
  **           "                       ® Ok ¯ 
  =gfModalGen("TRM04028B00000","DIALOG","TO"+"|"+"FROM")
  loFld = loOgScroll.ActiveControl
  loFld.Value = loFld.OldValue
  &lcVar      = loFld.OldValue
ENDIF
ENDFUNC 

*!************************************************************************
*!
*!      Function lfwRepWhen
*!
*!************************************************************************
FUNCTION lfwRepWhen

IF !USED('SYCCOMP')
  =gfOpenTABLE(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.DATADIR+'CCOMP_ID','SH')
  SELECT SYCCOMP
  gfSeek('')
ENDIF

SELECT APSETUP
gfSeek('')

SELECT APCHECKS
=gfSeek('')

SELECT APBANKS
gfSeek('')

SELECT FSPRD    
gfSeek('')

*-SAB [Start]
*=lfvActBy()
*-SAB [End]

=lfGetVarPos("lcRpForm" , .T.)
=lfGetVarPos("lcRpType" , .T.)      
=lfGetVarPos("lcRpSortBy" , .T.)

*- Initiate the BANK CODE to be as the saved in APSETUP file
lnPOS = ASCAN(loOgScroll.laOGVrFlt,"APPAYMNT.CBNKCODE")
IF lnPos > 0
  lnPOS    = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
  loOgScroll.laOGVrFlt[lnPOS,6] = APSETUP.CBNKCODE
ENDIF
lnPOS = ASCAN(loOgScroll.laOGVrFlt,"APPAYMNT.CCHKACCT")
IF lnPos > 0
  lnPOS    = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
  loOgScroll.laOGVrFlt[lnPOS,6] = APSETUP.CCHKACCT
ENDIF
ENDFUNC


*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
FUNCTION lfvBank

LOCAL loFld
loFld = loOgScroll.ActiveControl

IF loFld.OldValue = loFld.Value
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

SELECT APBANKS
gfSetOrder('BANKCODE')
IF loFld.OldValue <> loFld.Value

  lnPOS = ASCAN(loOgScroll.laOGVrFlt,"APPAYMNT.CBNKCODE") 
  IF lnPos > 0
    lnPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
  ENDIF

  *** Search for the current Group code
  loFld.Value = PADR(ALLTRIM(loFld.Value),8)
  IF !EMPTY(loFld.Value) AND ('?' $ loFld.Value .OR.!SEEK(loFld.Value))
    =gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
    IF !EMPTY(laRpRetFld[1])  
      loFld.Value = laRpRetFld[1]
    ELSE
      loFld.Value = loFld.OldValue
    ENDIF
    loFld.Refresh
  ENDIF

  loOgScroll.laOGVrFlt[lnPOS,6] = loFld.Value 
  
  lnChkPos = ASCAN(loOgScroll.laOGVrFlt,"APPAYMNT.CCHKACCT") 
  IF lnChkPos > 0
    lnChkPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnChkPos,1)
  ENDIF
  IF EMPTY(loFld.Value)
    loOgScroll.laOGVrFlt[lnChkPos,6] = ''
  ELSE
    IF !gfSeek(loFld.Value+loOgScroll.laOGVrFlt[lnChkPos,6],'APCHECKS')
      loOgScroll.laOGVrFlt[lnChkPos,6] = ''
    ENDIF
  ENDIF
ENDIF

IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF
ENDFUNC 

*!**************************************************************************
*!
*!      Function: lfvChkAct
*!
*!**************************************************************************
FUNCTION lfvChkAct

IF EMPTY(loOgScroll.ActiveControl.Value) OR loOgScroll.ActiveControl.OldValue = loOgScroll.ActiveControl.Value
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

SELECT APCHECKS
gfSetOrder('BANKCHECK')
IF EMPTY(loOgScroll.laOgVrFlt[1,6])
  *-SAB [Start]
  *gfSeek('')
  SET KEY TO
  *-SAB [End]
ELSE
  *-SAB [Start]
  *gfSeek(PADR(loOgScroll.laOgVrFlt[1,6],8))
  SET KEY TO PADR(loOgScroll.laOgVrFlt[1,6],8)
  *-SAB [End]
ENDIF 
LOCATE

  lcRpCurFld      = loOgScroll.ActiveControl.Value
  && Check If year field is empty
  IF loOgScroll.ActiveControl.OldValue <> loOgScroll.ActiveControl.Value
    *** Search for the current Group code
    IF !SEEK(laOGVrFlt[1,6]+loOgScroll.ActiveControl.Value)
      =gfBrows('','CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)
      
      IF EMPTY(laRpRetFld[1])
        loOgScroll.ActiveControl.Value = ''
        loOgScroll.laOGVrFlt[2,6] = '' 
      ELSE
        IF loOgScroll.ActiveControl.Value <> laRpRetFld[1]
          loOgScroll.ActiveControl.Value = laRpRetFld[1]
         
          lnPOS = ASCAN(loOgScroll.laOGVrFlt,"APPAYMNT.CCHKACCT") 
          IF lnPos > 0
            lnPOS    = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
            loOgScroll.laOGVrFlt[lnPOS,6] = laRpRetFld[1]
          ENDIF
        ENDIF
      ENDIF
      loOgScroll.ActiveControl.REFRESH       
    ENDIF
    
    IF !EMPTY(loOgScroll.ActiveControl.Value) AND EMPTY(loOgScroll.laOGVrFlt[1,6] )
      loOgScroll.laOGVrFlt[1,6] = APCHECKS.CBNKCODE
    ENDIF
  ENDIF
  
  
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF
ENDFUNC 

*!**************************************************************************
*!
*!      Function: lfBnkCode
*!
*!**************************************************************************
FUNCTION lfBnkCode

PRIVATE lcRetVal 

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias
SELECT APSETUP
lcRetVal   = APSETUP.CBNKCODE

llRpGlLink = IIF(APSETUP.CAPSGLLINK='Y',.T.,.F.)
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
                 ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink 
  SELECT SYCCOMP
  =SEEK(oAriaApplication.ActiveCompanyID)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = oAriaApplication.DataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(oAriaApplication.ActiveCompanyID)
  ENDIF
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName) 
  &lcOGVarName=lcRetVal
ENDIF

RETURN REPLI('!',8)
ENDFUNC

*!**************************************************************************
*!
*!      Function: lfChkAct
*!
*!**************************************************************************
FUNCTION lfChkAct
PARAMETERS llFirsTime

PRIVATE lcRetVal 

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias

IF llFirsTime
  SELECT APSETUP
  lcRetVal = APSETUP.CCHKACCT
ELSE
  SELECT APCHECKS
  gfSetOrder('BANKCHECK')
  =gfSeek(laOGVrFlt[2,6])
  lcRetVal = APCHECKS.CCHKACCT
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName) AND llFirsTime
  &lcOGVarName=lcRetVal
ENDIF
RETURN IIF(llFirsTime,REPL('!',12),lcRetVAl)
ENDFUNC

*!*************************************************************
*! Name      : lfvActBy
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/07/2009
*! Purpose   : Validate the Selected By Choices
*!*************************************************************
*! Called    : Option Grid
*!*************************************************************
FUNCTION lfvActBy

SET STEP ON 
LOCAL lnSlct,lnPOS
lnSlct = SELECT(0)

lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APPAYMNT.CFISFYEAR+'-'+APPAYMNT.CFSPPRDID") 
IF lnPos > 0
  lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)        
ENDIF
lcSlcPrd = loOgScroll.laOGFxFlt[lnPos,6]


DO CASE
  CASE lcRpActBy ='D'
    LAOGOBJCNT[5] = .T.
    LFOGSHOWGET('ldRpFrDat')
    LAOGOBJCNT[6] = .T.
    LFOGSHOWGET('ldRpToDat')

    IF !EMPTY(lcSlcPrd) AND USED(lcSlcPrd)
      SELECT (lcSlcPrd)
      DELETE ALL 
    ENDIF
    LAOGOBJCNT[9] = .F.
    LFOGSHOWGET('laOgFxFlt[1,6]')
    
  CASE lcRpActBy = 'P'
    LAOGOBJCNT[5] = .F.
    ldRpFrDat = {}
    LFOGSHOWGET('ldRpFrDat')
    LAOGOBJCNT[6] = .F.
    LFOGSHOWGET('ldRpToDat')
    ldRpToDat = {}

    LAOGOBJCNT[9] = .T.
    LFOGSHOWGET('laOgFxFlt[1,6]')
ENDCASE

SELECT (lnSlct)

ENDFUNC

*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Saber A.Razek(SAB)
*! Date      : 01-30-2012
*! Purpose   : Create Filter Expression for Bank reconciliation screen
*!*************************************************************
FUNCTION lfCreatExp

IF EMPTY(loOgScroll.laOgVrFlt[1,6])
  gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please select a bank!')
  RETURN .F.
ENDIF
IF EMPTY(loOgScroll.laOgVrFlt[2,6])
  gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please select a checking account!')
  RETURN .F.
ENDIF

lcBnkCode    = PADR(loOgScroll.laOgVrFlt[1,6],8)
lcChkAccount = PADR(loOgScroll.laOgVrFlt[2,6],12)
ENDFUNC

* End of lfCreatExp()


*:**************************************************************************
*:* Name        : lfSRVPerd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/30/2010
*:* Purpose     : Set-Reset-Valid function for the periods selected
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfSRVPerd
PARAMETERS lcSRV
LOCAL lnPos,lnSlct,lcSlcPrd
lnSlct = SELECT()

DO CASE
CASE lcSRV = 'R'
  *- be sure that only two id's are selected at most
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APPAYMNT.CFISFYEAR+'-'+APPAYMNT.CFSPPRDID") 
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)        
  ENDIF
  lcSlcPrd = loOgScroll.laOGFxFlt[lnPOS,6]
  
  SELECT (lcSlcPrd)
  LOCATE 
  COUNT TO lnCnt
  IF lnCnt>2
    LOCATE
    FOR lnI = 2 TO lnCnt-1
      SKIP
      DELETE
    ENDFOR 
  ENDIF
ENDCASE

SELECT(lnSlct)
ENDFUNC

*!*************************************************************
*! Name      : lfGetVarPos
*! Developer : Tarek Mohamed Ibrahim - TMI
*!*************************************************************
FUNCTION lfGetVarPos

PARAMETERS lcVarName , llEnabDis
PRIVATE lnVarPos
lnVarPos = ASCAN(laOGObjType,lcVarName)
IF lnVarPos > 0
  lnVarPos = ASUBSCRIPT(laOGObjType,lnVarPos,1)
  laOGObjCnt[lnVarPos] = llEnabDis
ENDIF
RETURN lnVarPos

ENDFUNC 