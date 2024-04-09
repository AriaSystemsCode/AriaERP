*:***************************************************************************
*: Program file  : APPAYRAT.PRG
*: Program desc. : CUSTOMIZED PAYABLE EXCHANGE RATE FOR BRATEX.
*: Date          : 06/24/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT PAYABLE (AP)
*: Developer     : ALBERT RAIF ATTIA (ALR)
*: Tracking Job Number: C102633
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO APPAYRAT
*:***************************************************************************
*: Modifications :
***************************************************************************
PRIVATE ll2Print

lcExSin2 = ''
lcExSin1 = ''
IF (gcDevice == "SCREEN") 			&& if user pushed Preview
  *** Display the report to screen 
  *** and check if there is any record or not
  ll2Print = USED(lcRpTargt) .AND. RECCOUNT(lcRpTargt) <> 0
  IF !USED(lcRpTargt) OR llOGFltCh 
    llOGFltCh = .T.
    ll2Print = lfGetData()
  ENDIF
  IF ll2Print
    SELECT (lcRpTargt)
    LOCATE
    DO gfDispRe WITH EVAL('lcRpForm')
  ELSE
    *-- NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ENDIF
ELSE

  ll2Print = IIF(llGenEnt,lfvPrntBat(),lfGetData())
  *** Print the report to Printer
  *** and check if there is any record or not
  IF ll2Print
    SELECT (lcRpTargt)
    LOCATE
    DO gfDispRe WITH EVAL('lcRpForm')
  ELSE
    *-- NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ENDIF
ENDIF
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfGetData
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/24/2002
*! Purpose   : Get the data
*!*************************************************************
*! Called from : APPAYRAY.PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfGetData()
*!*************************************************************
FUNCTION lfGetData

lcFileRel = ''
lcFileRel = lcFileRel + " APINVHDR.cinvno + APINVHDR.cvendcode = APDIST.CINVNO "
lcFileRel = lcFileRel + " AND APVENDOR.cvendcode = APDIST.cvendcode "
lcFileRel = lcFileRel + " AND APINVHDR.cinvstat <> 'V' "
lcFileRel = lcFileRel + " AND Apdist.capdactid = 'D' AND APINVHDR.ccurrcode <> gcBaseCurr "

lnApAcct = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLACCHAR.CACCTCODE'),1)
IF lnApAcct > 0
  lnDatPost = AT('GLACCHAR.CACCTCODE',LCRPEXP)
  lnPos1    = AT('AND' , SUBSTR(LCRPEXP,lnDatPost))
  IF lnPos1 > 0
    lcDatPost = STRTRAN( lcRpExp , SUBSTR(lcRpExp ,lnDatPost , lnPos1-1) , lcFileRel )
  ELSE
*    IF lnDatPost > 0 
*      lcDatPost = lcFileRel  
*    ELSE
*      lcDatPost = lcFileRel  + " AND " + lcRpExp
*    ENDIF
    lcDatPost = IIF(lnDatPost > 0 ,lcFileRel  ,lcFileRel  + " AND " + lcRpExp)
  ENDIF
ENDIF
lcRpExp    = lcDatPost
lcRpFiles  = "APINVHDR,APVENDOR,APDIST"			&& Get slected files name
lcRpfields = ""
lcRpfields = lcRpfields + "APVENDOR.cvendcode,APVENDOR.cvencomp, Apdist.cinvno, Apinvhdr.dinvdate, "
lcRpfields = lcRpfields + "Apdist.ncurrunit, capdtrtyp, nApdAmnt, Apdist.nexrate, "
lcRpfields = lcRpfields + "APINVHDR.ccurrcode, Apinvhdr.ninvamnt, Apinvhdr.ninvdistk, "
lcRpfields = lcRpfields + "Apinvhdr.ninvadj, (ninvpaid+ninvdistk+ninvadj) as lnPaidTot, "
lcRpfields = lcRpfields + "SUM(Apdist.neqvamnt) as lnEqvTot"
lcRpOrder  = " ORDER BY Apdist.ccurrcode "
lcGroupBy  = " GROUP BY Apdist.cinvno "
  

IF llOGFltCh

  *** Intialize the varliable that count rows selected
  _TALLY = 0
    
  WAIT 'Collecting data...' WINDOW NOWAIT
  ***   Create select  statment
  SELECT  &lcRpFields.;
    FROM  &lcRpFiles. ;
    WHERE &lcRpExp.   ;
          &lcGroupBy. ;
          &lcRpOrder. ;
    HAVING ninvamnt <> lnPaidTot;
    INTO CURSOR (lcRpTargt)
  WAIT CLEAR
ENDIF 
RETURN IIF(_TALLY > 0,.T.,.F.)

*!--End function lfGetData.
*!*************************************************************
*! Name      : lfGetCurr
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/23/2002
*! Purpose   : Get the Currancy Exchange rate in spaciffic date
*!*************************************************************
*! Called from : APPAYRAY.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lnCurrUnit		Currency Unit
*!                     lcCurrCode		Currency Code
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfGetCurr()
*!*************************************************************
FUNCTION lfGetCurr
PARAMETER lnCurrUnit,lcCurrCode,ldExgDate,lnOrExg

lcExSin2 = ''
lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
lnCurrRate = 0.00
*--Get the exchange rate in the currant date
lnCurrRate = gfChkRate(lnCurrUnit,lcCurrCode,ldExgDate,.F.,'C',gcBaseCurr)
IF lnCurrRate <> 0
  lnCurrRate = 1 &lcExSin1. lnCurrRate &lcExSin2.lnCurrUnit
ELSE
  lnCurrRate = 1 &lcExSin1. lnOrExg &lcExSin2.lnCurrUnit
ENDIF

RETURN lnCurrRate 

*--End of lfGetCurr.
*!*************************************************************
*! Name      : lfGetEquv
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 07/07/2002
*! Purpose   : Get the Equivlante amount of the currency
*!*************************************************************
*! Called from : APPAYRAY.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lnCurrUnit		Currency Unit
*!                     lcCurrCode		Currency Code
*!					   lnOrgRate		Original Rate	
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfGetEquv()
*!*************************************************************
FUNCTION lfGetEquv
PARAMETER lnCurrUnit,lcCurrCode,lnOrgRate
PRIVATE lnCurrRate ,lcExSin1,lcExSin2

lcExSin2 = ''
lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
lnCurrRate = 0.00
*--Get the exchange rate in the currant date
lnCurrRate = 1 &lcExSin1. lnOrgRate &lcExSin2.lnCurrUnit

RETURN lnCurrRate 

*--End of lfGetEquv.

*!*************************************************************
*! Name      : lfvExhRat
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/19/2002
*! Purpose   : Control the browser in Exchange rate field
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfvExhRat()
*!*************************************************************
FUNCTION lfvExhRat
PARAMETERS lcParmRat
PRIVAT lcFltCond

lnApAcctB = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLACCHAR.CACCTCODE'),1)
lcApAccnt = laogfxflt[lnApAcctB,6]
lcFltCond = "FOR glacchar.csegalpos='Y' "

SELECT GLACCHAR
IF lcParmRat = "A"
  llCorrAct = IIF(SEEK(lcExchAcct),IIF(glacchar.csegalpos="Y",.T.,.F.),.F.)
  IF '?' $ lcExchAcct .OR.  (!EMPTY(lcExchAcct) AND !llCorrAct)
    lcFltCond = lcFltCond + IIF(EMPTY(ALLTRIM(lcApAccnt)),"","AND glacchar.CACCTCODE<> lcApAccnt")
    =lfvlFld('GLACCHAR','CACCTCODE',VARREAD(),lcExchAcct,.F.,.F.,[CACCTCODE:H="Account code",CACCNLDES:H="Long report description"],'1',lcFltCond,.T.)
  ENDIF
  
  llCorrAct = IIF(SEEK(lcExchAcct),IIF(glacchar.csegalpos="Y",.T.,.F.),.F.)
  IF !llCorrAct
    STORE SPACE(0) TO lcExchAcct
  ENDIF
ELSE
  llCorrAct = IIF(SEEK(lcApAccnt),IIF(glacchar.csegalpos="Y",.T.,.F.),.F.)
  IF '?' $ lcApAccnt .OR.  (!EMPTY(lcApAccnt) AND !llCorrAct)
    lcFltCond = lcFltCond + IIF(EMPTY(ALLTRIM(lcExchAcct)),"","AND glacchar.CACCTCODE<> lcExchAcct")
    =lfvlFld('GLACCHAR','CACCTCODE',VARREAD(),lcApAccnt,.F.,.F.,[CACCTCODE:H="Account code",CACCNLDES:H="Long report description"],'1',lcFltCond,.T.)
  ENDIF
  SELECT GLACCHAR
  lcApAccnt = laogfxflt[lnApAcctB,6]
  llCorrAct = IIF(SEEK(lcApAccnt),IIF(glacchar.csegalpos="Y",.T.,.F.),.F.)
  IF !llCorrAct
    STORE SPACE(0) TO laogfxflt[lnApAcctB,6]
  ENDIF
ENDIF

*--End of lfvExhRat.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/19/2002
*! Purpose   : Restore the default value for AP and exchange accounts
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
PRIVATE lcvar1,lcvar2

STORE SPACE(0) TO lcvar1 , lcvar2 , lcRetVal
IF FILE(gcDataDir+'APPAYRAT.MEM')
  RESTORE FROM gcDataDir+'APPAYRAT.MEM' ADDITIVE
ENDIF

lnApAcct = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLACCHAR.CACCTCODE'),1)
laogfxflt[lnApAcct,6] = lcVar1
lcExchAcct            = lcVar2

*-Open the Currency file
llOpnCur = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'CCURRCODE','SH')

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfAccSet
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/23/2002
*! Purpose   : let the brows file point to the top
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lnParm
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfAccSet()
*!*************************************************************
FUNCTION lfAccSet
PARAMETERS lnParm

IF lnParm = "S"
  GOTO TOP IN APVENDOR
ENDIF

*--End of lfAccSet.
*!*************************************************************
*! Name      : lfCurrSet
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/23/2002
*! Purpose   : let the brows file point to the top
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lnParm
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfCurrSet()
*!*************************************************************
FUNCTION lfCurrSet
PARAMETERS lnParm

IF lnParm = "S"
  GOTO TOP IN SYCCURR
ENDIF

*--End of lfCurrSet.
*!*************************************************************
*! Name      : lfvPrntBat
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/24/2002
*! Purpose   : Control the run botton ins. off OG
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvPrntBat()
*!*************************************************************
FUNCTION lfvPrntBat

PRIVATE lcTarFl

STORE SPACE(0) TO lcDistPrd , lcDistYer , lcRevPrd , lcRevYer
STORE .F. TO llLokPerB , llLokPer , llCorrAcct , llUpdateRec , llAddRec
STORE .T. TO llProsComp
llChDate = lfVlDate(gcPrnt_Cmp,@lcRevPrd,@lcRevYer,ldReverDate,@llLokPer)
llChDate = lfVlDate(gcPrnt_Cmp,@lcDistPrd,@lcDistYer,ldBatchDate,@llLokPerB)
llLokPer = llLokPer .OR. llLokPerB
lnAcct = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLACCHAR.CACCTCODE'),1)
 
lcTarFl = ALIAS()
SELECT glacchar


SELECT (lcTarFl)
llOpnBat = gfOpenFile(gcDataDir+'GLBATCH',gcDataDir+'BATCHNO','SH')
llOpnBat = gfOpenFile(gcDataDir+'GLTRNSHD',gcDataDir+'BATCHTRN','SH')
llOpnBat = gfOpenFile(gcDataDir+'GLTRNSDT',gcDataDir+'BATCHTRN','SH')
llOpnBat = gfOpenFile(gcDataDir+'FSPRD',gcDataDir+'COMFYRPRDI','SH')

IF (llGenEnt AND (EMPTY(lcExchAcct) OR EMPTY(ALLTRIM(laOgFxflt[lnAcct,6])))) .OR. (llGenEnt AND llLokPer) 
  IF llLokPer
    =gfModalGen(.F.,'DIALOG',.F.,.F.,'Batch date and/or Reverse date falls within a locked period')
  ELSE
    =gfModalGen(.F.,'DIALOG',.F.,.F.,'AP Account and/or Exchange account is empty')
  ENDIF
  llProsComp = .F.
ELSE
  
  llOpnBat = gfOpenFile(gcDataDir+'GLBATEX',gcDataDir+'PRDYR','SH')
  IF SEEK(lcDistPrd +lcDistYer , 'GLBATEX')
    SCATTER memvar memo
    lcBatchNo = m.cbatchno

    SELECT GLBATCH
    =SEEK(lcBatchNo)
    SCATTER memvar memo
    IF !(m.cbatstat $ "PA")  
      llUpdateRec = .T.
      SELECT ctranno FROM GLTRNSHD WHERE cbatchno = lcBatchNo ;
      GROUP BY cbatchno INTO ARRAY laTranNo
      lcTrnsNo = laTranNo[1]
    ELSE
      lcMassage = IIF(m.cbatstat='P','A posted batch transaction is detected within this period. Can not add a new batch.',;
      								 'An Approved batch transaction belongs to this period, please clear the approval and re-run the report.')
      =gfModalGen(.F.,'DIALOG',.F.,.F.,lcMassage)
      llProsComp = .F.
    ENDIF
  ELSE
    llAddRec = .T.
  ENDIF
  IF llAddRec .OR. llUpdateRec
    IF lfGetData()
      lnTotExch = 0.0000
      SELECT (lcRpTargt)
      
      SCAN 
      
        lnBalance = ninvamnt - lnPaidTot
        lnExchRat = lfGetCurr(ncurrunit,ccurrcode,gdSysDate)
        lnTotExch = lnTotExch + ((lnBalance &lcExSin1 nexrate &lcExSin2 ncurrunit) - ;
        			 (lnBalance * lnExchRat))
        
      ENDSCAN
      IF lnTotExch <> 0
        IF llAddRec
          lcBatchNo = gfSequence('CBATCHNO')
          lcTrnsNo  = gfSequence('CTRANNO') 
          SELECT GLBATEX
          SCATTER MEMVAR MEMO BLANK

          M.CBATCHNO   = lcBatchNo 
          M.CTRANPRIOR = lcDistPrd 
          M.CTRNPYR    = lcDistYer
          INSERT INTO GLBATEX FROM MEMVAR
          =gfAdd_Info('GLBATEX')
          =lfAddBatRc()				&& Add records to batch files
        ELSE						&& If update the batch
          =lfDelBatRc('U')			&& Delete all records from batch files related to batch no
        ENDIF
      ELSE						&& If the Total exchange = 0
        IF llAddRec					&& if no difference and flage add
          =gfModalGen(.F.,'DIALOG',.F.,.F.,'Report will not generate any batch record because no exchange rate difference is found')
        ELSE						&& if no difference and flage edit
          SELECT GLBATEX
          DELETE FOR CBATCHNO = lcBatchNo
          =lfDelBatRc('D')			&& Delete all records from batch files related to batch no
        ENDIF
      ENDIF
    ELSE
    ENDIF
*  ELSE
*    WAIT "Can not add or update batch records " WINDOW
  ENDIF
ENDIF

RETURN llProsComp

*--End of lfvPrntBat.
*!*************************************************************
*! Name      : lfDelBatRc
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/25/2002
*! Purpose   : Delete records from batch files
*!*************************************************************
*! Called from : APPAYRAT
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcActionStat		Type of Action
*!						D:   replce 'V' to record statas
*!						U:   Update the records with new values
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfDelBatRc()
*!*************************************************************
FUNCTION lfDelBatRc
PARAMETER lcActionStat

SELECT GLTRNSDt
DELETE FOR CBATCHNO = lcBatchNo

IF lcActionStat = 'D'
  *--- Replace state with "V"
  SELECT GLTRNSHD
  *DELETE FOR CBATCHNO = lcBatchNo
  REPLACE ALL CTRNSTAT WITH "V" FOR CBATCHNO = lcBatchNo

  *--- Replace state with "V"
  SELECT GLBATCH
  *DELETE FOR CBATCHNO = lcBatchNo
  REPLACE ALL CBATSTAT WITH "V" FOR CBATCHNO = lcBatchNo

ELSE
  lnTotExch  = ABS(lnTotExch)
  SELECT GLTRNSHD
  REPLACE ALL CTRNSTAT  WITH "U" ;
              ntrntotcr WITH lnTotExch;
              ntrntotdr WITH lnTotExch;
             FOR CBATCHNO+CTRANNO = lcBatchNo

  *--- Replace state with " "
  SELECT GLBATCH
  REPLACE ALL CBATSTAT WITH "U" ;
              nbatcntot WITH lnTotExch;
              nbatotcr  WITH lnTotExch;
              nbatotdr  WITH lnTotExch;
             FOR CBATCHNO = lcBatchNo

  lcApAcct = ALLTRIM(laOgFxflt[lnAcct,6])
  SELECT GLTRNSDT
  SCATTER MEMVAR MEMO BLANK
  m.cacctcode = lcApAcct
  m.cbatchno  = lcBatchNo 
  m.ctranno   = lcTrnsNo
  m.dtrnpdate = ldBatchDate
  m.cdrorcr   = IIF(lnTotExch < 0,"C","D")
  m.ctrdtexp  = "Created by AP exchange rate report"
  m.ctrnpyr   = lcDistYer 
  m.ctrnpprd  = lcDistPrd 
  m.namount   = lnTotExch

  INSERT INTO GLTRNSDT FROM MEMVAR
  =gfAdd_Info('GLTRNSDT')

  SCATTER MEMVAR MEMO BLANK
  m.cacctcode = lcExchAcct
  m.cbatchno  = lcBatchNo 
  m.ctranno   = lcTrnsNo
  m.dtrnpdate = ldBatchDate
  m.cdrorcr   = IIF(lnTotExch < 0,"D","C")
  *m.ctrdtexp  = " The exchange rate difference from AP"
  m.ctrdtexp   = "Created by AP exchange rate report"
  m.ctrnpyr   = lcDistYer 
  m.ctrnpprd  = lcDistPrd 
  m.namount   = lnTotExch

  INSERT INTO GLTRNSDT FROM MEMVAR
  =gfAdd_Info('GLTRNSDT')
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Batch # '+ lcBatchNo + ' has been Updated successfully.') 
ENDIF

*--End of lfDelBatRc.
*!*************************************************************
*! Name      : lfAddBatRc
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/25/2002
*! Purpose   : Delete records from batch files
*!*************************************************************
*! Called from : APPAYRAT
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfAddBatRc()
*!*************************************************************
FUNCTION lfAddBatRc

lcApAcct = ALLTRIM(laOgFxflt[lnAcct,6])

SELECT GLBATCH
SCATTER MEMVAR MEMO BLANK
m.cbatchno  = lcBatchNo 
m.cbatdesc  = "Created by AP exchange rate report"
m.cbattype  = "N"
m.cbatstat  = "U"
m.ccomp_id  = gcPrnt_Cmp
m.cbatrefer = "On " + DTOC(gdSysDate)
m.cbatpyr   = lcDistYer
m.dbatpbeg  = lfGetBgEdD("Bg",lcDistPrd,lcDistYer)
m.dbatpend  = lfGetBgEdD("Ed",lcDistPrd,lcDistYer)
m.csrcmodul = "AP"
STORE ABS(lnTotExch) TO m.nbatcntot , m.nbatotcr , m.nbatotdr

INSERT INTO GLBATCH FROM MEMVAR
=gfAdd_Info('GLBATCH')

SELECT GLTRNSHD
SCATTER MEMVAR MEMO BLANK
m.cbatchno  = lcBatchNo 
m.ccomp_id  = gcPrnt_Cmp
m.csrcmodul = "AP"
m.ctranno   = lcTrnsNo
m.ctrndesc  = "Created by AP exchange rate report"
m.dtrnpdate = ldBatchDate
m.ctrnpyr   = lcDistYer 
m.ctrnpprd  = lcDistPrd 
m.ctrnrevpr = lcRevPrd
m.ctrnrevyr = lcRevYer
m.ctrnrefer = "On " + DTOC(gdSysDate)
m.ctrnrever = "Y"
m.ctrnstat  = "U"
m.ctrntype  = "N"
m.dtrnrevdt = ldReverDate
*m.csrcjrnl  = "AP"
m.csrcjrnl  = "GJ"

STORE ABS(lnTotExch) TO m.ntrntotcr , m.ntrntotdr

INSERT INTO GLTRNSHD FROM MEMVAR
=gfAdd_Info('GLTRNSHD')

SELECT GLTRNSDT
SCATTER MEMVAR MEMO BLANK
m.cacctcode = lcApAcct
m.cbatchno  = lcBatchNo 
m.ctranno   = lcTrnsNo
m.dtrnpdate = ldBatchDate
m.cdrorcr   = IIF(lnTotExch < 0,"C","D")
m.ctrdtexp  = "Created by AP exchange rate report"
m.ctrnpyr   = lcDistYer 
m.ctrnpprd  = lcDistPrd 
m.namount   = ABS(lnTotExch)

INSERT INTO GLTRNSDT FROM MEMVAR
=gfAdd_Info('GLTRNSDT')

SCATTER MEMVAR MEMO BLANK
m.cacctcode = lcExchAcct
m.cbatchno  = lcBatchNo 
m.ctranno   = lcTrnsNo
m.dtrnpdate = ldBatchDate
m.cdrorcr   = IIF(lnTotExch < 0,"D","C")
m.ctrdtexp  = "Created by AP exchange rate report"
m.ctrnpyr   = lcDistYer 
m.ctrnpprd  = lcDistPrd 
m.namount   = ABS(lnTotExch)

INSERT INTO GLTRNSDT FROM MEMVAR
=gfAdd_Info('GLTRNSDT')

=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Batch # '+ lcBatchNo + ' has been generated successfully.') 



*--End of lfAddBatRc.
*!*************************************************************
*! Name      : lfGetBgEdD
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 06/26/2002
*! Purpose   : Return Begin and end date for certin period 
*!*************************************************************
*! Called from : APPAYRAT
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcBgEnd,lcPeriod,lcYear
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfGetBgEdD()
*!*************************************************************
FUNCTION lfGetBgEdD
PARAMETER lcBgEnd,lcPeriod,lcYear

PRIVATE lcSelect
ldBgEndD = {}
lcSelect = ALIAS()

SELECT FSPRD
IF SEEK(lcYear+lcPeriod)
  SCATTER MEMVAR MEMO
  ldBgEndD = IIF(lcBgEnd = "Bg" , m.dfsppbgdt , m.dfsppendt)
ENDIF
SELECT (lcSelect)
RETURN ldBgEndD

*--End of lfGetBgEdD.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 03/20/1999
*! Purpose   : Close any opened files if user press OG <Close> Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

PRIVATE lcvar1 , lcvar2
lnApAcct = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'GLACCHAR.CACCTCODE'),1)
lcvar1 = ALLTRIM(laogfxflt[lnApAcct,6])
lcvar2 = lcexchacct
SAVE TO gcDataDir+'APPAYRAT.MEM' ALL LIKE lcvar*

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfclearbr
*! Developer : ALBERT RAIF ATTIA (ALR)
*! Date      : 03/20/1999
*! Purpose   : Refresh the option grid.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfclearbr()
*!*************************************************************
FUNCTION lfclearbr
CLEAR READ

*--End of lfclearbr.




****************************************************************************************
*:************************************************************************
*: Program file  : lfvlFld.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*
FUNCTION lfvlFld
PARAMETERS lcDbfName,lcFrstField,lcFrstRet,lcOldVal,;
           lcSecField,lcSecRet,lcBrowsFld,lcOrdName,;
           lcKeyToUse,llCanFilter
PRIVATE lcCurDbf,lcFrstType,lcSecType,lcTmpBrFld,lnCurRec,lcWinTitle,;
        lcFilter,laScrMode
        

IF lcFrstField="CACCTCODE"
  IF EMPTY(STRTRAN(EVAL(lcFrstRet),'-'))
    IF ATC('-',EVAL(lcFrstRet)) > 0
      STORE "" TO (lcFrstRet)
    ENDIF  
    RETURN
  ENDIF
ENDIF  
        
IF EMPTY(EVAL(lcFrstRet))
  RETURN
ENDIF
DIMENSION laScrMode[5]        
STORE .F. TO laScrMode
lcCurDbf=SELECT()
lnCurRec=IIF(RECNO()>RECCOUNT(),0,RECNO())
 
llMyUse=.F.           
****** Validate the database to use name and the first field name           
lcFileName=IIF(TYPE('lcFileName')='U','',lcFileName)
IF EMPTY(lcDbfName) OR EMPTY(lcFrstField) ;
  OR TYPE('lcDbfName')<>'C' OR TYPE('lcFrstField')<>'C'
    &lcFrstRet = lcOldVal
    RETURN
ENDIF  
********************************************************

**** Validate the variable name to return value in it
IF TYPE('lcFrstRet')<>'C' OR TYPE(lcFrstRet)='U'
  &lcFrstRet = lcOldVal
  RETURN
ENDIF
********************************************************
lcFrstType = lfGetType(lcFrstRet)
lcSecType  =IIF(TYPE('lcSecRet')='C',lfGetType(lcSecRet),'U')
lcTmpBrFld=lcBrFields
IF !USED('SYDFILES')
  llMyUse=.T.
  SELECT 0
  USE &gcSysHome.SYDFILES ORDER Cfile_nam 
ELSE

  SELECT SYDFILES
  lcOldOrd=ORDER()
  lcFilter=FILTER()
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
  SET ORDER TO TAG Cfile_nam
  SET FILTER TO
ENDIF  
lcWinTitle=lcDbfName
IF SEEK(IIF(ATC('.',lcDbfName)>=0,ALLTRIM(lcDbfName),ALLTRIM(lcDbfName)+'.DBF'))
  lcWinTitle=IIF(!EMPTY(cfile_ttl),cFile_ttl,lcWinTitle)
ENDIF  

IF llMyUse
  USE IN SYDFILES
ELSE
  SELECT SYDFILES  
  IF !EMPTY(lcOldOrd)
    SET ORDER TO &lcOldOrd
  ELSE
    SET ORDER TO
  ENDIF
  IF !EMPTY(lcFilter)
    SET FILTER TO &lcFilter
  ELSE
    SET FILTER TO
  ENDIF
  IF lnRecNo<>0
    GO lnRecNo
  ENDIF
ENDIF

llMyUse=.F.
IF !USED(lcDbfName)
  llMyUse=.T.
  lcPath=IIF(SUBSTR(UPPER(lcDbfName),1,2)='SY',gcSysHome,gcDataDir)  
  SELECT SELECT(1)
  USE &lcPath.&lcDbfName ORDER IIF(EMPTY(lcOrdName),1,&lcOrdName)
ELSE
  SELECT(lcDbfName)
  lcOldOrd=ORDER()
  lcFilter=FILTER()
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
  SET ORDER TO IIF(EMPTY(lcOrdName),1,&lcOrdName)
  SET FILTER TO
ENDIF
DECLARE laFieldBr[IIF(lcSecType='U',1,2)]
lcScatField=lcFrstField+IIF(EMPTY(lcSecField),'',','+lcSecField)
lcBrFields=IIF(TYPE('lcBrowsFld')<>'C',gfDbfField(lcDbfName),lcBrowsFld)
IF !EMPTY(ALLTRIM(&lcFrstRet))
  IF SEEK(&lcFrstRet)
    SCATTER FIELDS &lcScatField TO laFieldBr MEMO
  ELSE  
    IF LASTKEY()=13
       IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
         GO RECNO(0)
       ELSE
         GO TOP
       ENDIF
      =gfBrows(lckeyToUse,lcScatField,'laFieldBr',lcWinTitle,llCanFilter)
    ENDIF  
  ENDIF
ENDIF  
lcBrFields=lcTmpBrFld
SELECT(lcCurDbf)
IF lnCurRec<>0
  GO lnCurRec
ENDIF
IF !EMPTY(laFieldBr[1])
  IF lcFrstType='F'
    REPLACE &lcFrstRet WITH laFieldBr[1]
  ELSE
    IF lcFrstType='V'
     &lcFrstRet=laFieldBr[1]  
    ENDIF 
  ENDIF
  IF lcSecType='F'
    REPLACE &lcSecRet WITH laFieldBr[2]
  ELSE
    IF lcSecType='V'
      &lcSecRet=laFieldBr[1]  
    ENDIF 
  ENDIF
ELSE
  IF lcFrstType='F'
    REPLACE &lcFrstRet WITH lcOldVal
  ELSE
    IF lcFrstType='V'
     &lcFrstRet=lcOldVal
    ENDIF 
  ENDIF
ENDIF

IF llMyUse
  USE IN &lcDbfName
ELSE
  SELECT(lcDbfName)  
  IF !EMPTY(lcOldOrd)
    SET ORDER TO &lcOldOrd
  ELSE
    SET ORDER TO
  ENDIF
  IF !EMPTY(lcFilter)
    SET FILTER TO &lcFilter
  ELSE
    SET FILTER TO
  ENDIF
  IF lnRecNo<>0
    GO lnRecNo
  ENDIF
ENDIF
SELECT(lcCurDbf)
SHOW GET &lcFrstRet
IF lcSecType<>'U'
  SHOW GET &lcSecRet
ENDIF  

*!**************************************************************
*!
*!               FUNCTION lfGetType
*!
*!**************************************************************
*  FUNCTION to check a name if it is an array element or a field
*  or variable
FUNCTION lfGetType
PARAMETERS lcWhchType
PRIVAT lcSelect,lcType
lcSelect=SELECT()
SELECT SELECT(1)
lcType=''
IF TYPE(lcWhchType)<>'U'
 lcType='V'
ENDIF
SELECT(lcSelect)
RETURN IIF(EMPTY(lcType),IIF(TYPE(lcWhchType)='U','U','F'),'V')

****************************************************************************************