*:************************************************************************
*: Program file  : APBNKRC.PRG
*: Program desc. : Bank Reconciliation
*: System        : Aria4xp
*: Module        : AP
*: Developer     : Tarek Mohamed Ibrahim - TMI
*: Date          : 07/31/2011
*: Tracking #    : E302646,3 TMI  Enable the report to work for A4xp Fox tables 
*:************************************************************************
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011 
*E303370,1 TMI 03/16/2013 include void payments [T20130226.0007] 
*E303371,1 TMI 03/21/2013 Add a new sort option to the repot to sort by the date only regardless the transaction type[T20130221.0011] 
*B610325,1 TMI 05/02/2013 [T20130416.0020] collect all lines to be sure the summations before the report dates are calculated correctly
*B610486,1 TMI 08/25/2013 let lnTBooks be calculated originally based on gfDispAmnt function and remove it from the frx [T20130812.0014] 
*B610551,1 TMI 10/21/2013 fix a problem that the report sums a +ve value when  issued for the forgin currency while negative value when  issued with the standard currency [T20130812.0014] 
*B610619,1 TMI 12/09/2013 allow the lfGetTot works same as lfInsrtRow in the point that to get the correct NEXRATE [T20130812.0014 ] 
*:************************************************************************
STORE 0 TO lnWholCler,lnWholOpen, lnWholBook
STORE 0 TO lntClear,lntOpen,lnTBooks

IF loOgScroll.llOGFltCh
  *E303370,1 TMI 03/16/2013 [Start] define order variable
  lcOrd = ORDER('APPAYMNT')
  *E303370,1 TMI 03/16/2013 [End  ] 

  =lfColctData()
  SELECT (lcTmpApPay)
  LOCATE
  IF !FOUND()
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN 
  ENDIF 
ENDIF 

*- show the report
SELECT (lcTmpApPay)
=lfvOrder()
LOCATE

DO gfDispRe WITH EVAL('LCRPFORM')

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

=lfvActBy()

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
* End of lfwRepWhen()    

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
FUNCTION lfRepShow

laOGObjCnt[5] = gfGetMemVar('LLMULCURR')

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*- Add the currency to the AP reports.
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfgetTot
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : Function to get totals of the periods previous to 
*!             the chosen ones
*!*************************************************************
FUNCTION lfGetTot
lnAlias = SELECT(0)

STORE 0 TO lntClear,lntOpen,lnTBooks

lcTotFlt = "CPAYSTAT <> 'V' .AND. CPAYMETH <> 'H'"
*E303370,1 TMI 03/16/2013 [Start] do not check void payments in case of 'Include void payments'
IF llIncVdPts
  lcTotFlt = "CPAYMETH <> 'H'"
ENDIF 
*E303370,1 TMI 03/16/2013 [End  ] 

IF lcRpType = 'NOBATCH'  
  IF (lcRpActBy='D' .AND. EMPTY(ldRpFrDat) ) .OR. ;
     (lcRpActBy='P' .AND. lnPrSel = 0)
    RETURN ''
  ENDIF   
ELSE
  IF (lcRpActBy='D' .AND. EMPTY(ldRpFrDat) ) .OR. ;
     (lcRpActBy='P' .AND. lnPrSel = 0)
    RETURN ''
  ENDIF   
ENDIF

* If select by date, calculate the totals up to the FROM date.
IF lcRpActBy = 'D'                           && If filterring by date
  lcTotFlt = lcTotFlt + " .AND. DPAYDATE < ldRpFrDat "
ELSE
  lcPrd = RIGHT(lcFstPrd,2)
  lcTotFlt =  lcTotFlt+" .AND. (cFisfYear < &lcTmpApPay..cFisFYear"+ ;
                         " .OR. (cFisfYear=&lcTmpApPay..cFisFYear .AND. VAL(cfspprdid) < VAL(lcPrd)))"
ENDIF

*E303370,1 TMI 03/16/2013 [Start] 
IF !llIncVdPts
  *E303370,1 TMI 03/16/2013 [End  ] 
  USE (oAriaApplication.DataDir+'APPAYMNT') ORDER CHKTMNO AGAIN ALIAS lcTmpPmt IN 0
  *E303370,1 TMI 03/16/2013 [Start] 
ELSE 
  USE (oAriaApplication.WorkDir+lcTempPMNT) ORDER CHKTMNO AGAIN ALIAS lcTmpPmt IN 0
ENDIF    
*E303370,1 TMI 03/16/2013 [End  ]

SELECT lcTmpPmt
=SEEK(&lcTmpApPay..CBNKCODE+&lcTmpApPay..CCHKACCT)
lcCondStr  = "CBNKCODE+CCHKACCT = &lcTmpApPay..CBNKCODE+&lcTmpApPay..CCHKACCT"
     
SCAN REST WHILE &lcCondStr FOR &lcTotFlt
  *B610619,1 TMI 12/09/2013 18:19 [Start] get the correct exrate 
  M.CPAYRECST = lcTmpPmt.CPAYRECST
  IF gfSEEK(lcTmpPmt.cPayMeth+lcTmpPmt.cBnkCode+lcTmpPmt.cChkAcct+lcTmpPmt.cPayDocNo,'ApDist')
    SELECT ApDist
    LOCATE REST WHILE capdtrtyp+cbnkcode+cchkacct+capdref+cinvno+capdactid = ;
                lcTmpPmt.cPayMeth+lcTmpPmt.cBnkCode+lcTmpPmt.cChkAcct+lcTmpPmt.cPayDocNo ;
                FOR capdactid = 'C'
    IF FOUND()
      m.cCurrCode = cCurrCode
      m.nExRate   = nExRate
      m.nCurrUnit = nCurrUnit
    ENDIF
  ENDIF
  *B610619,1 TMI 12/09/2013 18:19 [End  ] 
  
  lnValue   =  lcTmpPmt.nPayAmnt
  *B610486,3 TMI 09/22/2013 [Start] use gfdispamt as in lntBooks 
  *lntClear = lntClear + IIF(CPAYRECST = 'C' , -lnValue  , 0 )  
  *lntOpen  = lntOpen  + IIF(CPAYRECST = 'O' , -lnValue  , 0 )
  lntClear = lntClear + IIF(CPAYRECST = 'C' , -gfAmntDisp(lnValue,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.) , 0 )  
  lntOpen  = lntOpen  + IIF(CPAYRECST = 'O' , -gfAmntDisp(lnValue,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.) , 0 )
  *B610486,3 TMI 09/22/2013 [End  ] 
  
  *B610486,1 TMI 08/25/2013 [Start] use gfdispamt
  *lntBooks = lnTBooks + IIF(CPAYRECST = 'V' , 0 , -lnValue  )
  lntBooks = lnTBooks + IIF(CPAYRECST = 'V' , 0 , -gfAmntDisp(lnValue,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)  )
  *B610486,1 TMI 08/25/2013 [End  ] 
ENDSCAN

USE IN lcTmpPmt
SELECT (lnAlias)
RETURN ''

*!**************************************************************************
*!
*!      Function: lfvOrder
*!
*!**************************************************************************
*
FUNCTION lfvOrder

*E303371,1 TMI 03/21/2013 [Start] check the new added index
*IF lcRpSortBy = 'N'
*  SET ORDER TO TAG CHKTMNO
*ELSE
*  SET ORDER TO TAG PayDate 
*ENDIF 
DO case
CASE lcRpSortBy = 'N'
  SET ORDER TO TAG CHKTMNO
CASE lcRpSortBy = 'T'
  SET ORDER TO TAG PayDate 
CASE lcRpSortBy = 'D'
  SET ORDER TO TAG ByDate
ENDCASE 
*E303371,1 TMI 03/22/2013 [End  ] 


*E303371,1 TMI 03/22/2013 [Start] In case of Format One recalculate the nBooks , nOpen & nClear fields as the lines may be rearranged according to the different sorts 
IF lcRpForm = 'APBNKRC'
  LOCAL lnClear,lnBooks,lnOpen
  SELECT (lcTmpApPay)
  LOCATE 
  STORE 0 TO lnClear,lnBooks,lnOpen
  lcBnkChkAcc = CBNKCODE+CCHKACCT
  SCAN 
    IF !(lcBnkChkAcc == CBNKCODE+CCHKACCT)
      STORE 0 TO lnClear,lnBooks,lnOpen
    ENDIF 
    lnBooks = lnBooks + &lcTmpApPay..nBooks2
    lnClear = lnClear + &lcTmpApPay..nClear2
    lnOpen  = lnOpen  + &lcTmpApPay..nOpen2
    replace nBooks WITH lnBooks ;
            nClear WITH lnClear ;
            nOpen  WITH lnOpen
  ENDSCAN 
  LOCATE 
ENDIF 
*E303371,1 TMI 03/22/2013 [End  ] 
RETURN 


*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Tarek Mohamed Ibrahim - TMI
*! Date      : 08/15/2011
*! Purpose   : 1) Create temp. table 
*!             2) Open or close table based on parameter
*!*************************************************************
FUNCTION  lfCreatTmp
PRIVATE  llBalnBfor           && Flag to know if the temp. table has been created 
IF FILE(oAriaApplication.WorkDir +lcTmpApPay+ '.DBF') 
  SELECT (lcTmpApPay)
  ZAP
ELSE
  DIME ArryPayTmp(1,4)
  SELECT APPAYMNT
  =AFIELDS(ArryPayTmp)
  nAryLen = ALEN(ArryPayTmp ,1 ) 
  DIME ArryPayTmp[nAryLen+11,18]

  lnIndx = nAryLen
  
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'NRECNO'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 6
  ArryPayTmp[lnIndx,4] = 0  
    
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'Cbnklndes'
  ArryPayTmp[lnIndx,2] = 'C'
  ArryPayTmp[lnIndx,3] = 40
  ArryPayTmp[lnIndx,4] = 0  
    
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'Trtyp'
  ArryPayTmp[lnIndx,2] = 'C'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 0
    
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'Stat'
  ArryPayTmp[lnIndx,2] = 'C'
  ArryPayTmp[lnIndx,3] = 30
  ArryPayTmp[lnIndx,4] = 0
    
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'Amount'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 2
 
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'nClear'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 2
 
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'nBooks'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 2
 
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'nOpen'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 2
  
  *E302646,4 TMI 09/07/2011 [Start] the following fields will be used in Format two
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'nClear2'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 2
 
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'nBooks2'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 2
 
  lnIndx = lnIndx + 1
  ArryPayTmp[lnIndx,1] = 'nOpen2'
  ArryPayTmp[lnIndx,2] = 'N'
  ArryPayTmp[lnIndx,3] = 20
  ArryPayTmp[lnIndx,4] = 2
  *E302646,4 TMI 09/07/2011 [End  ] 
 
  FOR lnJ = nAryLen+1 TO ALEN(ArryPayTmp,1)
    FOR lnI = 7 TO 16
      ArryPayTmp[lnJ,lnI] = ''
    ENDFOR
    ArryPayTmp[lnJ,17] = 0
    ArryPayTmp[lnJ,18] = 0
  ENDFOR
  
  *E302646,4 TMI 09/06/2011 [Start] create table using gfCrtTmp 
  DIMENSION laIndx[2,2]
  laIndx[1,1] = "cbnkcode+cchkacct+cpaytype+cpaymeth+cpaydocno"
  laIndx[1,2] = "CHKTMNO"
  laIndx[2,1] = "cbnkcode+cchkacct+cpaytype+cpaymeth+DTOS(dPayDate)"
  laIndx[2,2] = "PayDate"
  *E303371,1 TMI 03/21/2013 [Start] 
  DIMENSION laIndx[3,2]
  laIndx[3,1] = "cbnkcode+cchkacct+DTOS(dPayDate)+cpaytype+cpaymeth"
  laIndx[3,2] = "ByDate"
  *E303371,1 TMI 03/21/2013 [End  ] 
  =gfCrtTmp(lcTmpApPay,@ArryPayTmp,@laIndx)  
  *E302646,4 TMI 09/06/2011 [End  ] 
  
  SELECT (lcTmpApPay)
ENDIF
RETURN


*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
* 
FUNCTION lfClearRep

*! Based on the report type whetehr is batch or nonbatch the index file 
*! lcTmpApPay will be created. so if user have selected batch option report this file 
*! (lcTmpApPay) will be created as DBF and CDX ,  but if the user have selected 
*! nonbatch option report this file (lcTmpApPay) will be created CDX only based on
*! related to master file APPAYMNT. 
*! Therfore while erasing that file i can't now it's status so to avoid foxpro error 
*! message i wrote the below command becuase the index file lcTmpApPay will be
*! deleted in one of the below commands.

PRIVATE lcCurErHnd
lcCurErHnd = ON("ERROR")
ON ERROR STORE .T. TO llDumErHnd

IF FILE(oAriaApplication.WorkDir +lcTmpApPay+ '.DBF') 
  SELECT (lcTmpApPay)
  USE 
  ERASE (oAriaApplication.WorkDir +lcTmpApPay+ '.DBF')
  ERASE (oAriaApplication.WorkDir +lcTmpApPay+ '.CDX')
ENDIF

IF FILE(oAriaApplication.WorkDir +lcTmpApPay+ '.CDX') 
  SELECT APPAYMNT
  CLOSE INDEX
  ERASE (oAriaApplication.WorkDir +lcTmpApPay+ '.CDX')
ENDIF
ON ERROR &lcCurErHnd

lcCurErHnd = ON("ERROR")
ON ERROR STORE .T. TO llDumErHnd

IF TYPE('lcTmpIndx') <> 'U'
  IF FILE(oAriaApplication.WorkDir +lcTmpIndx+ '.CDX') 
    SELECT APPAYMNT  
    CLOSE INDEX
    ERASE (oAriaApplication.WorkDir +lcTmpIndx+ '.CDX')
  ENDIF  
ENDIF

*E303370,1 TMI 03/16/2013 [Start] clear temp 
IF USED('APPAYMNT')
  USE IN ('APPAYMNT')
ENDIF 
ERASE (oAriaApplication.WorkDir+lcTempPMNT+'.*')
*E303370,1 TMI 03/16/2013 [End  ] 
    
ON ERROR &lcCurErHnd

*!*************************************************************
*! Name      : lfWholeTot
*! Developer : Tarek Mohammed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : 1) Sum book, cleared and open amount of whole transactions
*!*************************************************************
*! Called from : APBNKRCS.FRX 
*!*************************************************************
FUNCTION lfWholeTot
lnAlias = SELECT(0)
STORE 0 TO lnWholCler,lnWholOpen, lnWholBook
lcTotFlt = "CPAYSTAT <> 'V' .AND. CPAYMETH <> 'H'"
*E303370,1 TMI 03/16/2013 [Start]  do not check void payments in case of 'Include void payments'
IF llIncVdPts
  lcTotFlt = "CPAYMETH <> 'H'"
ENDIF 
IF !llIncVdPts
  *E303370,1 TMI 03/16/2013 [End  ] 
  USE (oAriaApplication.DataDir+'APPAYMNT') ORDER CHKTMNO AGAIN ALIAS lcTmpPmt IN 0
  *E303370,1 TMI 03/16/2013 [Start] 
ELSE 
  USE (oAriaApplication.WorkDir+lcTempPMNT) ORDER CHKTMNO AGAIN ALIAS lcTmpPmt IN 0
ENDIF
*E303370,1 TMI 03/16/2013 [End  ]

SELECT lcTmpPmt
=SEEK(&lcTmpApPay..CBNKCODE+&lcTmpApPay..CCHKACCT)

lcCondStr  = "CBNKCODE+CCHKACCT = &lcTmpApPay..CBNKCODE+&lcTmpApPay..CCHKACCT"
SCAN REST WHILE &lcCondStr  FOR &lcTotFlt
  lnValue   =  lcTmpPmt.nPayAmnt
  lnWholCler = lnWholCler + IIF(CPAYRECST = 'C', -lnValue , 0  ) 
  lnWholOpen = lnWholOpen + IIF(CPAYRECST = 'O', -lnValue , 0  ) 
  lnWholBook = lnWholBook - lnValue
ENDSCAN
USE IN lcTmpPmt
SELECT (lnAlias)
RETURN ''
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
  gfSeek('')
ELSE
  gfSeek(PADR(loOgScroll.laOgVrFlt[1,6],8))
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

*!*************************************************************
*! Name      : lfvActBy
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/07/2009
*! Purpose   : Validate the Selected By Choices
*!*************************************************************
*! Called    : Option Grid
*!*************************************************************
FUNCTION lfvActBy

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

* End of lfvActBy()
*!*************************************************************
*! Name      : lfColctData
*! Developer : Tarek Mohamed Ibrahim - TMI
*! Date      : 10/04/2011
*! Purpose   : Collecting Data
*!*************************************************************
FUNCTION lfColctData
STORE '' TO lcFiscYear, lcFiscPrd, lcBnkCode, lcChkAccount, lcReconStat, lcFstPrd, lcLstPrd, lcDateFltExp

lcFiscPrd    = lfCheckFilter(1,"APPAYMNT.CFISFYEAR+'-'+APPAYMNT.CFSPPRDID",1)
lcBnkCode    = lfCheckFilter(3,"APPAYMNT.CBNKCODE  ",1)
lcChkAccount = lfCheckFilter(3,"APPAYMNT.CCHKACCT",1)
lcReconStat  = lfCheckFilter(3,"APPAYMNT.CPAYRECST",1)
lnPrSel      = 0

IF !EMPTY(lcFiscPrd)
  SELECT (lcFiscPrd)
  COUNT TO lnPrSel FOR !DELETED()
  IF lnPrSel > 0
    SELECT (lcFiscPrd)
    GO TOP 
    lcFstPrd = &lcFiscPrd..KeyExp
    GO BOTTOM 
    lcLstPrd = &lcFiscPrd..KeyExp
  ENDIF 
ENDIF 

*E302646,4 TMI 09/04/2011 [Start] build the "lcDateFltExp" varialbe
lcDateFltExp=''                                 && Intialize the Date filtering expression
IF lcRpActBy='D'                                && If filterring by date
  DO CASE
    CASE !EMPTY(ldRpFrDat) AND !EMPTY(ldRpToDat)  
      lcDateFltExp = [BETWEEN(DPAYDATE,ldRpFrDat,ldRpToDat)]

    CASE !EMPTY(ldRpFrDat) 
      lcDateFltExp = [DPAYDATE >= ldRpFrDat]   
    CASE !EMPTY(ldRpToDat)
      lcDateFltExp = [DPAYDATE <= ldRpToDat]   
  ENDCASE    
ENDIF

*E302646,4 Bank reconciliation TMI 10/17/2011 [Start] remove filters already in lcRpExp to use lcRpExp in advanced option
*lcFltExpr = " IIF(!EMPTY(lcBnkCode)   , APPAYMNT.CBNKCODE  $  lcBnkCode,    .T.) AND " +;
            " IIF(!EMPTY(lcChkAccount), APPAYMNT.CCHKACCT  $  lcChkAccount, .T.) AND " +;
            " IIF(!EMPTY(lcReconStat) , APPAYMNT.CPAYRECST $  lcReconStat,    .T.) AND " +;
            " IIF( lnPrSel > 0        , APPAYMNT.CFISFYEAR+'-'+APPAYMNT.CFSPPRDID >= lcFstPrd AND APPAYMNT.CFISFYEAR+'-'+APPAYMNT.CFSPPRDID <= lcLstPrd , .T.)"
lcFltExpr = " IIF( lnPrSel > 0        , APPAYMNT.CFISFYEAR+'-'+APPAYMNT.CFSPPRDID >= lcFstPrd AND APPAYMNT.CFISFYEAR+'-'+APPAYMNT.CFSPPRDID <= lcLstPrd , .T.)"
*E302646,4 Bank reconciliation TMI 10/17/2011 [End  ] 
lcFltExpr = IIF(!EMPTY(lcFltExpr), lcFltExpr + IIF(!EMPTY(lcDateFltExp), " AND " + lcDateFltExp, '') , lcDateFltExp)
*E303370,1 TMI 03/14/2013 [Start] if the payment is voided in different period then include it.
*lcFltExpr = IIF(!EMPTY(lcFltExpr), lcFltExpr + " AND Appaymnt.Cpaystat <> 'V' .AND. Appaymnt.CPAYMETH <> 'H' " , ;
                                                   " Appaymnt.Cpaystat <> 'V' .AND. Appaymnt.CPAYMETH <> 'H' " )
lcFltExpr = IIF(!EMPTY(lcFltExpr), lcFltExpr + " AND " , "" ) + " Appaymnt.CPAYMETH <> 'H' .AND." + ;
                                   IIF(llIncVdPts = .F.," Appaymnt.Cpaystat<>'V'",".T.")
*E303370,1 TMI 03/14/2013 [End  ] 
*E302646,4 Bank reconciliation TMI 10/17/2011 [Start] 
lcFltExpr = lcFltExpr + IIF(!EMPTY(lcFltExpr) AND !EMPTY(lcRpExp),' AND '+lcRpExp,'')
*E302646,4 Bank reconciliation TMI 10/17/2011 [End  ] 
=lfCreatTmp() && Creates temps

*E303370,2 TMI 03/19/2013 [Start] move this below
*!*	SELECT APPAYMNT
*!*	SET RELATION TO cpayclno INTO Apvendor, cbnkcode INTO Apbanks
*E303370,2 TMI 03/19/2013 [End  ] 

*E303370,1 TMI 03/16/2013 [Start] to decive the report, create a new temp table with all lines as the original file, add to it all voided lines with the paydate as the void date
IF llIncVdPts
  USE (oAriaApplication.DataDir+'APPAYMNT') ORDER &lcOrd IN APPAYMNT
  SELECT APPAYMNT
  SET RELATION TO cpayclno INTO Apvendor, cbnkcode INTO Apbanks  
  lcSafe = SET("Safety")
  SET SAFETY OFF
  *- build the temp file with the same record that will show in the report
  =SEEK(lcBnkCode+lcChkAccount)
  *B610325,1 TMI 05/02/2013 [Start] collect all lines to be sure the summations before the report dates are calculated correctly
  *COPY TO (oAriaApplication.WorkDir+lcTempPMNT) ;
          WITH cdx ;
          WHILE CBNKCODE+CCHKACCT+CPAYTYPE+CPAYMETH+CPAYDOCNO = lcBnkCode+lcChkAccount ;
          FOR &lcFltExpr
  COPY TO (oAriaApplication.WorkDir+lcTempPMNT) WITH cdx
  *B610325,1 TMI 05/02/2013 [End  ] 
  SET SAFETY &lcSafe
  USE (oAriaApplication.WorkDir+lcTempPMNT) IN 0 SHARED order &lcOrd
  lcTmpVoid = gfTempName()
  SELECT (lcTempPMNT)
  AFIELDS(laStruc)
  CREATE CURSOR (lcTmpVoid) FROM ARRAY laStruc
  SELECT (lcTempPMNT)
  LOCATE 
  SCAN FOR Cpaystat = 'V'
    SCATTER MEMVAR
    m.DPAYDATE = m.DPAYVDATE
    SELECT fsprd 
    LOCATE 
    LOCATE FOR BETWEEN(m.DPAYVDATE,fsprd.DFSPPBGDT,fsprd.DFSPPENDT)
    m.CFISFYEAR = fsprd.CFISFYEAR
    m.CFSPPRDID = fsprd.CFSPPRDID
    m.NPAYAMNT  = -m.NPAYAMNT
    SELECT (lcTmpVoid)
    APPEND BLANK
    GATHER MEMVAR     
  ENDSCAN 
  SELECT (lcTempPMNT)
  LOCATE
  REPLACE Cpaystat WITH ' ' ALL
  APPEND FROM (DBF(lcTmpVoid))  
  USE IN (lcTmpVoid)

  SELECT (lcTempPMNT)
  USE 
  USE IN APPAYMNT
  ERASE (oAriaApplication.WorkDir + lcTmpIndx+ '.CDX')
  USE (oAriaApplication.WorkDir+lcTempPMNT) IN 0 SHARED alia APPAYMNT
  SELECT APPAYMNT
  SET ORDER TO &lcOrd
ELSE 
  IF UPPER(oAriaApplication.WorkDir) $ DBF('APPAYMNT')
    USE IN APPAYMNT
    ERASE (oAriaApplication.WorkDir + lcTmpIndx+ '.CDX')
    SELECT 0
    USE (oAriaApplication.DataDir+'APPAYMNT') ORDER &lcOrd
  ENDIF 
ENDIF                                   
*E303370,1 TMI 03/16/2013 [End  ] 

*E303370,2 TMI 03/19/2013 [Start] moved from above
SELECT APPAYMNT
SET RELATION TO cpayclno INTO Apvendor, cbnkcode INTO Apbanks
*E303370,2 TMI 03/19/2013 [End  ] 

gfSetOrder("CHKTMNO")   && CBNKCODE+CCHKACCT+CPAYTYPE+CPAYMETH+CPAYDOCNO
lnPayCnt = RECCOUNT()
IF lcRpType = 'BATCH'
  IF !FILE(oAriaApplication.WorkDir + lcTmpIndx+ '.CDX')
    INDEX ON cbnkcode+cchkacct+batch TAG chkbtch OF (oAriaApplication.WorkDir + lcTmpIndx+ '.CDX')
  ENDIF
  SET ORDER TO chkbtch OF (oAriaApplication.WorkDir + lcTmpIndx+ '.CDX')
ENDIF
lcWhCrit = ALLTRIM(lcBnkCode+lcChkAccount)
=gfSeek(lcWhCrit)


lcBnkChkAcc = ''
STORE 0 TO lnClear,lnBooks,lnOpen

SCAN REST WHILE CBNKCODE+CCHKACCT+CPAYTYPE+CPAYMETH+CPAYDOCNO = lcWhCrit ;
          FOR &lcFltExpr

  SELECT APPAYMNT    
  
  IF !(lcBnkChkAcc == CBNKCODE+CCHKACCT)
    lcBnkChkAcc = CBNKCODE+CCHKACCT
    STORE 0 TO lnClear,lnBooks,lnOpen
  ENDIF
  
  WAIT WINDOW NOWAIT 'Collecting data ...'
  IF EMPTY(APPAYMNT.Batch)
  
    DO lfInsrtRow with APPAYMNT.NPAYAMNT 
  
  ELSE
    IF lcRpType = 'BATCH'
      lcBatchNo  = APPAYMNT.Batch
      nlRecNo = RECNO('APPAYMNT')
      lnAmount = 0 
      SCAN REST WHILE APPAYMNT.BATCH = lcBatchNo .AND. !EOF('APPAYMNT')
        lnAmount = lnAmount  + APPAYMNT.NPAYAMNT
        *E302646,4 TMI 09/07/2011 [Start] keep the recno 
        nlRecNo = RECNO('APPAYMNT')
        *E302646,4 TMI 09/07/2011 [End  ] 
      ENDSCAN
      IF lnAmount <> 0
        GOTO nlRecNo
        DO lfInsrtRow WITH lnAmount
      ENDIF
      GOTO nlRecNo
    ELSE
      DO lfInsrtRow WITH APPAYMNT.NPAYAMNT
    ENDIF
  ENDIF
ENDSCAN

WAIT CLEAR


*!*************************************************************
*! Name      : lfInsrtRow
*! Developer : Tarek Mohamed Ibrahim - TMI
*! Date      : 06/20/1999
*! Purpose   : Copy the current data of APPATMNT row to temp. table 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) nAmtValue
*!*************************************************************
*! Return      :  None
*!*************************************************************
PROCEDURE lfInsrtRow
PARAMETER nAmtValue

PRIVATE lcCurrency , lnExRate , lnCurrUnit,lnSlct
lnSlct = SELECT(0)

SELECT APPAYMNT 
SCATTER MEMVAR MEMO
*B610551,1 TMI 10/21/2013 [Start] moved from beolw
IF gfSEEK(ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo,'ApDist')
  SELECT ApDist
  LOCATE REST WHILE capdtrtyp+cbnkcode+cchkacct+capdref+cinvno+capdactid = ;
              ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo ;
              FOR capdactid = 'C'
  IF FOUND()
    m.cCurrCode = cCurrCode
    m.nExRate   = nExRate
    m.nCurrUnit = nCurrUnit
  ENDIF
ENDIF
*B610551,1 TMI 10/21/2013 [End  ] 
m.nOpen2 = IIF(CPAYRECST='O',gfAmntDisp(-npayamnt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),0)
lnOpen = lnOpen + m.nOpen2
m.nOpen = lnOpen 

m.nClear2 = IIF(CPAYRECST='C',gfAmntDisp(-npayamnt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),0)
lnClear = lnClear + m.nClear2
m.nClear = lnClear 

m.nBooks2 = gfAmntDisp(-npayamnt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
lnBooks = lnBooks + m.nBooks2
m.nBooks = lnBooks 

*B610551,1 TMI 10/21/2013 [Start] comment this code and move it up, the rate should be adjusted before calling gfAmntDisp
*!*	IF gfSEEK(ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo,'ApDist')
*!*	  SELECT ApDist
*!*	  LOCATE REST WHILE capdtrtyp+cbnkcode+cchkacct+capdref+cinvno+capdactid = ;
*!*	              ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo ;
*!*	              FOR capdactid = 'C'
*!*	  IF FOUND()
*!*	    m.cCurrCode = cCurrCode
*!*	    m.nExRate   = nExRate
*!*	    m.nCurrUnit = nCurrUnit
*!*	  ENDIF
*!*	ENDIF
*B610551,1 TMI 10/21/2013 [End  ] 

m.Cbnklndes = APbANKS.Cbnklndes
m.Trtyp = IIF(CPAYMETH='P','Prn Chk',IIF(CPAYMETH='M','Man Chk', IIF(CPAYMETH='N','Non Chk',IIF(CPAYMETH='B',IIF(CPAYMETH='B' AND NPAYAMNT < 0,'Deposit','Charge'),'A\R'))))
m.CPAYCOMP = IIF(EMPTY(cpaycomp) .AND. CPAYTYPE = 'P',APVENDOR.CVENCOMP,cpaycomp)
m.Stat = IIF(CPAYRECST='O','Open',IIF(CPAYRECST='C','Clrd','Hide'))
m.Amount = gfAmntDisp(-npayamnt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
*E303370,1 TMI 03/14/2013 [Start] show the status of the payment as void 
IF llIncVdPts
  m.Stat = IIF(CPAYSTAT='V','Void',m.Stat)
ENDIF 
*E303370,1 TMI 03/14/2013 [End  ] 

SELECT (lcTmpApPay)
APPEND BLANK
GATHER MEMVAR MEMO

SELECT (lnSlct )

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Hesham Elmasry
*! Date      : 10/07/2009
*! Purpose   : Return the select values from filters
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter, lnRetTyp

LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)    
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6] 
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]  
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)  
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6] 
    ENDIF
ENDCASE

IF lnRetTyp = 1
  RETURN lcReturn 
ELSE 
  RETURN lnPos
ENDIF 
* End of lfCheckFilter

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
*-- end of lfSRVPerd.