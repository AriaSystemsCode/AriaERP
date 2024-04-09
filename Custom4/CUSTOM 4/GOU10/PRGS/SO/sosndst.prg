*:***************************************************************************
*: PROGRAM FILE  : SOSNDST.PRG
*: Program desc. : Custom program to send order status /Ship status
*: For screen    : SOSNDSTS.scx
*:        System : Aria 4 XP
*:        Module : SO
*:     Developer : Mariam Mazhar (MMT)
*:     EXEs      : C201325.122,C201326.Exe[T20101109.0013]
*:***************************************************************************
*! B609925,1 SAB 05/17/2012 Fix Ship To Name in Order Browse for Order Ship Status [T20120420.0001]
*:***************************************************************************
lcRpOrdFl = ''
lcRpShpFl= ''

IF oAriaApplication.MULTIINST 
  =gfCallForm('SOSNDSTS','SO')
ELSE
  DO FORM (oAriaApplication.ScreenHome+"\SO\SOSNDSTS.SCX")
ENDIF 
RETURN
*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Calls the Scope Option grid
*!*************************************************************
FUNCTION lfvScope
PARAMETERS loFormSet
 lndataSessPre = SET("Datasession" )
 loFormSet.llCallScop = .F.       
 lcDataSessI = SET("Datasession" )
 llShowTrck  = .F.
 lcStatusVar = ''
 DECLARE laScopExpr[1]
 STORE "" TO laScopExpr
 lcRpOrdFile = ''
 lcRpShpFile= ''
 lcExpr = gfOpGrid('SOSNDST' , .T.)&&,.F.,.F.,.T.,.T.)
 SET DATASESSION TO lcDataSessI 
 loFormSet.lcRpOrdFl = lcRpOrdFile 
 loFormSet.lcRpShpFl = lcRpShpFile
 IF lcExpr <> ".F."
   IF USED(loFormSet.lcTmpOrdhdr)
     SELECT(loFormSet.lcTmpOrdhdr) 
     ZAP 
   ENDIF 
   loFormSet.llHasInv = .F.
   lfCollectData(loFormSet)
   lfAddGrdCntSrc(loFormSet)
   SELECT(loFormSet.lcTmpOrdhdr)
   LOCATE
   *loFormSet.AriaForm1.grdOrders.GrdMultiSelectionGrid.DoScroll(2)
   IF !EOF()
     loFormSet.llEnableInvert = .T.
     loFormSet.llEnableSelect = .T.
     loFormSet.llEnableSelectall = .T.
     loFormSet.llEnableSelectnone = .F.
     loFormSet.AriaForm1.grdOrders.GrdMultiSelectionGrid.AFTERROWCOLCHANGE()
     lfvpbSel(loFormSet)      
   ELSE    && Else
     loFormSet.llEnableInvert = .F.
     loFormSet.llEnableSelect = .F.
     loFormSet.llEnableSelectAll = .F.
     loFormSet.llEnableSelectNone = .F.
     =gfModalGen('TRM00052B00000','DIALOG')
     RETURN      
   ENDIF    && End of IF
 ELSE
   loFormSet.llEnableInvert = .F.
   loFormSet.llEnableSelect = .F.
   loFormSet.llEnableSelectAll = .F.
   loFormSet.llEnableSelectNone = .F.
   RETURN 
 ENDIF
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : When Function of the Option grid
*!*************************************************************
FUNCTION lfwOGWhen
IF EMPTY(lcRpShpFl) OR EMPTY(lcRpOrdFl)
  IF FILE(oariaApplication.DataDir+'SendStFl'+'.MEM')
    RESTORE FROM oariaApplication.DataDir+'SendStFl'+'.MEM' ADDITIVE
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvShpPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Validate Ship File
*!*************************************************************
FUNCTION lfvShpPath
IF !EMPTY(lcRpShpFl) AND '?' $ lcRpShpFl
  lcRpShpFl= GETFILE('TXT','Select the File Location and Name')
ENDIF 
IF !EMPTY(lcRpShpFl) 
  lcRpShpFl =  FORCEEXT(lcRpShpFl,'txt') 
ENDIF

RETURN .T.
*!*************************************************************
*! Name      : lfvOrdPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Validate Order File
*!*************************************************************
FUNCTION lfvOrdPath
IF !EMPTY(lcRpOrdFl) AND '?' $ lcRpOrdFl
  lcRpOrdFl= GETFILE('TXT','Select the File Location and Name')
ENDIF 
IF !EMPTY(lcRpOrdFl) 
  lcRpOrdFl =  FORCEEXT(lcRpOrdFl,'txt') 
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Init Method of the screen
*!*************************************************************
FUNCTION lfInit
LPARAMETERS loFrmSet
SET MULTILOCKS ON
loFrmSet.llCallScop = .F.   &&Flag to hold the first time of the session  
loFrmSet.llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time
=gfOpenTable('ORDHDR','ORDHDR','SH')
=gfOpenTable('INVLINE','INVLINE','SH')
=gfOpenTable('INVHDR','INVHDR','SH')
loFrmSet.lcTmpOrdhdr = gfTempName()
SELECT ORDHDR
lnOrdHdrCnt = AFIELDS(laOrdhdrStr)
DIMENSION laOrdhdrStr[lnOrdHdrCnt +8,18]
laOrdhdrStr[lnOrdHdrCnt +1,1] = 'LLSEL'
laOrdhdrStr[lnOrdHdrCnt +1,2] = 'L'
laOrdhdrStr[lnOrdHdrCnt +1,3] = 1
laOrdhdrStr[lnOrdHdrCnt +1,4] = 0

laOrdhdrStr[lnOrdHdrCnt +2,1] = 'ShipDate'
laOrdhdrStr[lnOrdHdrCnt +2,2] = 'D'
laOrdhdrStr[lnOrdHdrCnt +2,3] = 8
laOrdhdrStr[lnOrdHdrCnt +2,4] = 0

laOrdhdrStr[lnOrdHdrCnt +3,1] = 'INVOICE'
laOrdhdrStr[lnOrdHdrCnt +3,2] = 'C'
laOrdhdrStr[lnOrdHdrCnt +3,3] = 6
laOrdhdrStr[lnOrdHdrCnt +3,4] = 0

laOrdhdrStr[lnOrdHdrCnt +4,1] = 'Invtrckno'
laOrdhdrStr[lnOrdHdrCnt +4,2] = 'C'
laOrdhdrStr[lnOrdHdrCnt +4,3] = 30
laOrdhdrStr[lnOrdHdrCnt +4,4] = 0


laOrdhdrStr[lnOrdHdrCnt +5,1] = 'StsMsgln1'
laOrdhdrStr[lnOrdHdrCnt +5,2] = 'C'
laOrdhdrStr[lnOrdHdrCnt +5,3] = 35
laOrdhdrStr[lnOrdHdrCnt +5,4] = 0

laOrdhdrStr[lnOrdHdrCnt +6,1] = 'StsMsgln2'
laOrdhdrStr[lnOrdHdrCnt +6,2] = 'C'
laOrdhdrStr[lnOrdHdrCnt +6,3] = 30
laOrdhdrStr[lnOrdHdrCnt +6,4] = 0

laOrdhdrStr[lnOrdHdrCnt +7,1] = 'StsCode'
laOrdhdrStr[lnOrdHdrCnt +7,2] = 'C'
laOrdhdrStr[lnOrdHdrCnt +7,3] = 10
laOrdhdrStr[lnOrdHdrCnt +7,4] = 0

laOrdhdrStr[lnOrdHdrCnt +8,1] = 'StsDate'
laOrdhdrStr[lnOrdHdrCnt +8,2] = 'D'
laOrdhdrStr[lnOrdHdrCnt +8,3] = 8
laOrdhdrStr[lnOrdHdrCnt +8,4] = 0

FOR lnT =1 TO 8
  STORE '' TO laOrdhdrStr[lnOrdHdrCnt +lnT,7],laOrdhdrStr[lnOrdHdrCnt +lnT,8],laOrdhdrStr[lnOrdHdrCnt +lnT,9],;
              laOrdhdrStr[lnOrdHdrCnt +lnT,10],laOrdhdrStr[lnOrdHdrCnt +lnT,11],laOrdhdrStr[lnOrdHdrCnt +lnT,12],;
              laOrdhdrStr[lnOrdHdrCnt +lnT,13],laOrdhdrStr[lnOrdHdrCnt +lnT,14],laOrdhdrStr[lnOrdHdrCnt +lnT,15],;
              laOrdhdrStr[lnOrdHdrCnt +lnT,16]
  STORE 0 TO  laOrdhdrStr[lnOrdHdrCnt +lnT,17],laOrdhdrStr[lnOrdHdrCnt +lnT,18]
ENDFOR 

=gfCrtTmp(loFrmSet.lcTmpOrdhdr,@laOrdhdrStr,"CORDTYPE+ORDER",loFrmSet.lcTmpOrdhdr)
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Collect Orders based on selected criteria
*!*************************************************************
FUNCTION lfCollectData
PARAMETERS loFormSet

*Start Date
ldStStartDate = {}
ldStEndDate   = {}
llDateStSelect = .F.
lnStDatePos = ASCAN(laScopExpr,'ORDHDR.START')
IF lnStDatePos > 0
  lnStDatePos= ASUBSCRIPT(laScopExpr,lnStDatePos,1)
  llDateStSelect = !EMPTY(laScopExpr[lnStDatePos,6])
  IF llDateStSelect 
    ldStStartDate = CTOD(SUBSTR(laScopExpr[lnStDatePos,6],1,10))
    ldStEndDate   = CTOD(SUBSTR(laScopExpr[lnStDatePos,6],12,21))
    IF SUBSTR(laScopExpr[lnStDatePos,6],1,1) = "|"
      ldStStartDate = {}
      ldStEndDate   = CTOD(SUBSTR(laScopExpr[lnStDatePos,6],2,11))
    ENDIF 
  ENDIF   
ENDIF
  
*Complete Date
llDateCmSelect = .F.
ldCmStartDate = {}
ldCmEndDate   = {}
lnCmDatePos = ASCAN(laScopExpr,'ORDHDR.COMPLETE')
IF lnCmDatePos > 0
  lnCmDatePos= ASUBSCRIPT(laScopExpr,lnCMDatePos,1)
  llDateCmSelect = !EMPTY(laScopExpr[lnCmDatePos,6])
  IF llDateCmSelect 
    ldCmStartDate = CTOD(SUBSTR(laScopExpr[lnCMDatePos,6],1,10))
    ldCmEndDate   = CTOD(SUBSTR(laScopExpr[lnCmDatePos,6],12,21))
    IF SUBSTR(laScopExpr[lnCMDatePos,6],1,1) = "|"
      ldCmStartDate = {}
      ldCmEndDate   = CTOD(SUBSTR(laScopExpr[lnCmDatePos,6],2,11))
    ENDIF 
  ENDIF   
ENDIF
  
*Entered Date
ldEnStartDate = {}
ldEnEndDate   = {}
llDateEnSelect = .F.
lnEnDatePos = ASCAN(laScopExpr,'ORDHDR.ENTERED')
IF lnEnDatePos > 0
  lnEnDatePos= ASUBSCRIPT(laScopExpr,lnEnDatePos,1)
  llDateEnSelect = !EMPTY(laScopExpr[lnEnDatePos,6])
  IF llDateEnSelect 
    ldEnStartDate = CTOD(SUBSTR(laScopExpr[lnEnDatePos,6],1,10))
    ldEnEndDate   = CTOD(SUBSTR(laScopExpr[lnEnDatePos,6],12,21))
    IF SUBSTR(laScopExpr[lnEnDatePos,6],1,1) = "|"
      ldEnStartDate = {}
      ldEnEndDate   = CTOD(SUBSTR(laScopExpr[lnEnDatePos,6],2,11))
    ENDIF 
  ENDIF   
ENDIF

*INVHDR.INVDATE
ldStInvDate = {}
ldInvEndDate   = {}
llDateInvSelect = .F.
lnInvDatePos = ASCAN(laScopExpr,'INVHDR.INVDATE')
IF lnInvDatePos > 0
  lnInvDatePos = ASUBSCRIPT(laScopExpr,lnInvDatePos ,1)
  llDateInvSelect = !EMPTY(laScopExpr[lnInvDatePos ,6])
  IF llDateInvSelect 
    ldStInvDate = CTOD(SUBSTR(laScopExpr[lnInvDatePos ,6],1,10))
    ldInvEndDate  = CTOD(SUBSTR(laScopExpr[lnInvDatePos ,6],12,21))
    IF SUBSTR(laScopExpr[lnInvDatePos ,6],1,1) = "|"
      ldStInvDate = {}
      ldInvEndDate   = CTOD(SUBSTR(laScopExpr[lnInvDatePos ,6],2,11))
    ENDIF 
  ENDIF   
ENDIF

* Account 
llAccSelected = .F.
lcAccSel = ''
lnPosAcc = ASCAN(laScopExpr,"CUSTOMER.ACCOUNT")
IF lnPosAcc > 0 
  lnPosAcc = ASUBSCRIPT(laScopExpr,lnPosAcc,1)
  lcAccSel =IIF(!EMPTY(laScopExpr[lnPosAcc,6]),laScopExpr[lnPosAcc,6],'')
  IF !EMPTY(lcAccSel) AND USED(lcAccSel)
    SELECT(lcAccSel)
    LOCATE
    IF !EOF()
      llAccSelected = .T.      
    ENDIF 
  ENDIF 
ENDIF       

*ORder
llOrderSelected = .F.
lcOrderSel = ''
lnPosOrd = ASCAN(laScopExpr,"ORDHDR.ORDER")
IF lnPosOrd> 0 
  lnPosOrd= ASUBSCRIPT(laScopExpr,lnPosOrd,1)
  lcOrderSel=IIF(!EMPTY(laScopExpr[lnPosOrd,6]),laScopExpr[lnPosOrd,6],'')
  IF !EMPTY(lcOrderSel) AND USED(lcOrderSel)
    SELECT(lcOrderSel)
    LOCATE
    IF !EOF()
      llOrderSelected = .T.      
    ENDIF 
  ENDIF 
ENDIF       
WAIT WINDOW 'Collecting Data...' NOWAIT 
 
SELECT Ordhdr 
DO CASE
  CASE  llOrderSelected 
    SELECT Ordhdr
    =gfSetOrder('Ordhdr')
    SELECT(lcOrderSel)
    LOCATE 
    SCAN
      SELECT ORDHDR
      =gfSEEK('O'+&lcOrderSel..Order)
      SCAN REST WHILE CORDTYPE+ORDER = 'O'+&lcOrderSel..Order FOR IIF(llDateStSelect,BETWEEN(ORDHDR.START,ldStStartDate,ldStEndDate),.T.)  AND ;
                                         IIF(llDateCmSelect,BETWEEN(ORDHDR.COMPLETE,ldCmStartDate,ldCmEndDate),.T.) AND ;
                                         IIF(llDateEnSelect,BETWEEN(ORDHDR.ENTERED,ldEnStartDate,ldEnEndDate),.T.) AND ;
                                         IIF(llAccSelected ,SEEK(ORDHDR.ACCOUNT,lcAccSel),.T.) AND !EMPTY(ordhdr.CCONTREF) AND Status $ 'COH' AND LMORELY
                                         
                                        
        IF IIF(llDateInvSelect ,!gfSeek(ORDHDR.Order,'INVLINE','INVLINEO') OR !BETWEEN(INVLINE.INVDATE,ldStInvDate ,ldInvEndDate),.F.)
          LOOP
        ENDIF 
        SELECT ORDHDR
        SCATTER MEMO MEMVAR 
        m.StsDate = oAriaApplication.SystemDate
        IF gfSeek(ORDHDR.Order,'INVLINE','INVLINEO') AND gfSeek(INVLINE.INVOICE,'INVHDR')
          m.Invoice = INVHDR.INVOICE
          m.Invtrckno = Invhdr.ccartrckno
          m.ShipDate = Invhdr.ShipDate
          loFormSet.llHasInv = .T.
        ELSE
          m.Invoice = ""
          m.Invtrckno = ""
          m.ShipDate = {}
        ENDIF 
        
        IF !SEEK(m.CORDTYPE+m.ORDER,loFormSet.lcTmpOrdhdr)  
          INSERT INTO (loFormSet.lcTmpOrdhdr) FROM MEMVAR 
        ENDIF 
      ENDSCAN 
    ENDSCAN 
  CASE llAccSelected 
    SELECT ORDHDR
    =gfSetOrder("ORDACCT")
    SELECT(lcAccSel)
    SCAN
      SELECT ORDHDR
      =gfSeek(&lcAccSel..Account+'O')
  		SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcAccSel..Account+'O' FOR IIF(llDateStSelect,BETWEEN(ORDHDR.START,ldStStartDate,ldStEndDate),.T.)  AND ;
  		                                         IIF(llDateCmSelect,BETWEEN(ORDHDR.COMPLETE,ldCmStartDate,ldCmEndDate),.T.) AND ;
  		                                         IIF(llDateEnSelect,BETWEEN(ORDHDR.ENTERED,ldEnStartDate,ldEnEndDate),.T.) AND !EMPTY(ordhdr.CCONTREF) AND Status $ 'COH' AND LMORELY
  		                                         
  		                                         
  		                                        
	      IF IIF(llDateInvSelect ,!gfSeek(ORDHDR.Order,'INVLINE','INVLINEO') OR !BETWEEN(INVLINE.INVDATE,ldStInvDate ,ldInvEndDate),.F.)
	        LOOP
	      ENDIF 

	      SCATTER MEMO MEMVAR 
        m.StsDate= oAriaApplication.SystemDate                        
        IF gfSeek(ORDHDR.Order,'INVLINE','INVLINEO') AND gfSeek(INVLINE.INVOICE,'INVHDR')
          m.Invoice = INVHDR.INVOICE
          m.Invtrckno = Invhdr.ccartrckno
          m.ShipDate = Invhdr.ShipDate
          loFormSet.llHasInv = .T.
        ELSE
          m.Invoice = ""
          m.Invtrckno = ""
          m.ShipDate = {}
        ENDIF 
        
	      IF !SEEK(m.CORDTYPE+m.ORDER,loFormSet.lcTmpOrdhdr)  
	        INSERT INTO (loFormSet.lcTmpOrdhdr) FROM MEMVAR 
	      ENDIF 
  		ENDSCAN 
    ENDSCAN    
  OTHERWISE 
    SELECT Ordhdr
    =gfSetOrder('Ordhdr')
     
    =gfSEEK('O')
    SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR IIF(llDateStSelect,BETWEEN(ORDHDR.START,ldStStartDate,ldStEndDate),.T.)  AND ;
                                         IIF(llDateCmSelect,BETWEEN(ORDHDR.COMPLETE,ldCmStartDate,ldCmEndDate),.T.) AND ;
                                         IIF(llDateEnSelect,BETWEEN(ORDHDR.ENTERED,ldEnStartDate,ldEnEndDate),.T.) AND ;
                                         IIF(llAccSelected ,SEEK(ORDHDR.ACCOUNT,lcAccSel),.T.) AND !EMPTY(ordhdr.CCONTREF) AND Status $ 'COH' AND LMORELY
                                         
                                         
      IF IIF(llDateInvSelect ,!gfSeek(ORDHDR.Order,'INVLINE','INVLINEO') OR !BETWEEN(INVLINE.INVDATE,ldStInvDate ,ldInvEndDate),.F.)
        LOOP
      ENDIF  
      
      SCATTER MEMO MEMVAR 
      m.StsDate= oAriaApplication.SystemDate      
      IF gfSeek(ORDHDR.Order,'INVLINE','INVLINEO') AND gfSeek(INVLINE.INVOICE,'INVHDR')
        m.Invoice = INVHDR.INVOICE
        m.Invtrckno = Invhdr.ccartrckno
        m.ShipDate = Invhdr.ShipDate
        loFormSet.llHasInv = .T.
      ELSE
        m.Invoice = ""
        m.Invtrckno = ""
        m.ShipDate = {}
      ENDIF 
      IF !SEEK(m.CORDTYPE+m.ORDER,loFormSet.lcTmpOrdhdr)    
         INSERT INTO (loFormSet.lcTmpOrdhdr) FROM MEMVAR 
      ENDIF 
    ENDSCAN 
ENDCASE 
WAIT CLEAR 
*!*************************************************************
*! Name      : lfAddGrdCntSrc
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Add Control Source of the grid
*!*************************************************************
FUNCTION lfAddGrdCntSrc
PARAMETERS loFormSet
WITH loFormSet.ariaform1.grdOrders.GrdMultiSelectionGrid 
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpOrdhdr
  .Column1.Header1.Caption = ""
  .Column1.CurrentControl = "AriaCheckBox1"
  .colUMN1.ControlSource = loFormSet.lcTmpOrdhdr+'.LLSEL'
  .colUMN2.ControlSource = loFormSet.lcTmpOrdhdr+'.ORDER'
  .colUMN3.ControlSource = loFormSet.lcTmpOrdhdr+'.ACCOUNT'
  .colUMN4.ControlSource = loFormSet.lcTmpOrdhdr+'.STORE'
  .colUMN5.ControlSource = loFormSet.lcTmpOrdhdr+'.CUSTPO'
  .colUMN6.ControlSource = loFormSet.lcTmpOrdhdr+'.status'
  .colUMN7.ControlSource = loFormSet.lcTmpOrdhdr+'.entered'
  .colUMN8.ControlSource = loFormSet.lcTmpOrdhdr+'.Open'
  .colUMN9.ControlSource = loFormSet.lcTmpOrdhdr+'.openamt'
  .colUMN10.ControlSource = loFormSet.lcTmpOrdhdr+'.CCONTREF'
  .Column1.Enabled = .T.
  .SETALL('ReadOnly',.T.,'COLUMN')
  .Column1.readonly = .F.
  .Enabled = .T. 
  .Column1.AriaCheckBox1.Enabled = .T.
  .refresh()
ENDWITH
WITH loFormSet.ariaform1
  .txtStatus.ControlSource = loFormSet.lcTmpOrdhdr+'.StsCode'
  .txtMsgln2.ControlSource = loFormSet.lcTmpOrdhdr+'.StsMsgln2'
  .txtMsgln1.ControlSource  = loFormSet.lcTmpOrdhdr+'.StsMsgln1'
  .dtpStsDate.ControlSource  =  loFormSet.lcTmpOrdhdr+'.StsDate'
  .txtTrack.ControlSource  = loFormSet.lcTmpOrdhdr+'.Invtrckno' 
  .dtpShpDate.ControlSource  = loFormSet.lcTmpOrdhdr+'.ShipDate'
ENDWITH 

*!*************************************************************
*! Name      : lfOrdExp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Copy Option grid Criteria
*!*************************************************************
FUNCTION lfOrdExp
*Order Status File Path
IF EMPTY(lcRpOrdFl) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Order Status File")
  RETURN .F.
ENDIF 
IF !EMPTY(lcRpOrdFl) 
  lcDir = JUSTPATH(lcRpOrdFl)
  IF !DIRECTORY(lcDir)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Order Status File Path")
    RETURN .F.
  ENDIF   
ENDIF 

**Ship Status File Path
IF EMPTY(lcRpShpFl) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Ship Status File")
  RETURN .F.
ENDIF 
IF !EMPTY(lcRpShpFl) 
  lcDir = JUSTPATH(lcRpShpFl)
  IF !DIRECTORY(lcDir)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid Ship Status File Path")
    RETURN .F.
  ENDIF   
ENDIF 
IF ALLTRIM(lcRpOrdFl) = ALLTRIM(lcRpShpFl)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Cannot use the same file name for Ship Status File and Order Status File")
  RETURN .F.
ENDIF
lcRpOrdFile = lcRpOrdFl
lcRpShpFile= lcRpShpFl

=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)
SAVE TO oariaApplication.DataDir+'SendStFl'+'.MEM' ALL LIKE lcRp*Fl*
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 04/11/2011
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel
PARAMETERS loFormSet
IF LLSEL 
  loFormSet.lcCaptionSel = 'UnSe\<lect'
ELSE 
  loFormSet.lcCaptionSel = 'Se\<lect'
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 04/11/2011
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS loFormSet,llFromChkBox
SELECT(loFormSet.lcTmpOrdhdr)
IF !llFromChkBox
  REPLACE LLSEL WITH !LLSEL
ENDIF  
lfvpbSel(loFormSet)
llSelected = .F.
llNotSelected = .F.
lnRecNumb = RECNO(loFormSet.lcTmpOrdhdr)
LOCATE FOR LLSEL
IF FOUND()
  llSelected = .T.
ENDIF 
LOCATE 
LOCATE FOR !LLSEL
IF FOUND()
  llNotSelected = .T.
ENDIF 

IF BETWEEN(lnRecNumb,1,RECCOUNT())
  GO RECORD lnRecNumb
ENDIF 
*No records was selected
IF !llSelected
  loFormSet.llenableinvert = .T.
  loFormSet.llenableselect = .T.
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
ELSE    && Else
  loFormSet.llenableselectnone = .T.
  *-- All the records were selected
  IF !llNotSelected
    loFormSet.llenableselectall = .F.
  ELSE
    loFormSet.llenableselectall = .T.
  ENDIF
ENDIF  
*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 04/11/2011
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll
PARAMETERS loFormSet
SELECT(loFormSet.lcTmpOrdhdr)
lnRecCurrn = RECNO()
REPLACE ALL LLSEL WITH .T.
IF BETWEEN(lnRecCurrn,1,RECCOUNT())
 GO RECORD lnRecCurrn
ENDIF 
loFormSet.lcCaptionSel = 'UnSe\<lect'
loFormSet.llenableselectall = .F.
loFormSet.llenableselectnone = .T.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Mariam Mazhar [MMT]
*! Date      : 04/11/2011
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon
PARAMETERS loFormSet
SELECT(loFormSet.lcTmpOrdhdr)
lnRecCurr = RECNO()
REPLACE ALL LLSEL WITH .F.
IF BETWEEN(lnRecCurr ,1,RECCOUNT())
 GO RECORD lnRecCurr 
ENDIF 

loFormSet.lcCaptionSel = 'Se\<lect'
loFormSet.llEnableSelectAll  = .T.
loFormSet.llEnableSelectNone = .F.
*loFormSet.llEnableRel = .F.

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mariam Mazhar [MMT]
*! Date      : 04/11/2011
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert
PARAMETERS loFormSet
SELECT(loFormSet.lcTmpOrdhdr)
llSelected = .F.
llNotSelected = .F.
lnRecNOCurr = RECNO()
REPLACE ALL LLSEL WITH !LLSEL
LOCATE 
LOCATE FOR LLSEL
IF FOUND()
  llSelected = .T.
ENDIF 
LOCATE 
LOCATE FOR !LLSEL
IF FOUND()
  llNotSelected = .T.
ENDIF 


IF BETWEEN(lnRecNOCurr ,1,RECCOUNT())
 GO RECORD lnRecNOCurr 
ENDIF 
lfvpbSel(loFormSet)
*there is no selected records
IF !llSelected 
  loFormSet.llenableselectall = .T.
  loFormSet.llenableselectnone = .F.
ELSE 
  loFormSet.llenableselectnone = .T.
  *--All the records were selected
  IF !llNotSelected 
    loFormSet.llenableselectall = .F.
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfAfterRowCol
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Grid After Row Col. Changed Method
*!*************************************************************
FUNCTION lfAfterRowCol
PARAMETERS loFormSet
SELECT(loFormSet.lcTmpOrdhdr)

loFormSet.ariaForm1.txtOrder.Value  = Order
loFormSet.ariaForm1.txtOrder.Enabled = .F.

loFormSet.ariaForm1.txtStatus.Value  = StsCode
loFormSet.ariaForm1.txtStatus.Enabled = llSel

loFormSet.ariaForm1.txtMorelyOrd.Value  = ccontref
loFormSet.ariaForm1.txtMorelyOrd.Enabled = .F.

loFormSet.ariaForm1.txtCustPO.Value  = custpo
loFormSet.ariaForm1.txtCustPO.Enabled = .F.

loFormSet.ariaForm1.dtpStsDate.Value =  IIF(EMPTY(StsDate),oAriaApplication.SystemDate,StsDate)
loFormSet.ariaForm1.dtpStsDate.Enabled = llSel

loFormSet.ariaForm1.txtMsgln1.Enabled = llSel
loFormSet.ariaForm1.txtMsgln2.Enabled = llSel

loFormSet.ariaForm1.txtMsgln1.Value = StsMsgln1
loFormSet.ariaForm1.txtMsgln2.Value = StsMsgln2

 
IF !EMPTY(INVOICE)
  loFormSet.ariaForm1.txtTrack.Enabled = llSel
  loFormSet.ariaForm1.dtpShpDate.Enabled = llSel
  loFormSet.ariaForm1.txtTrack.Value = Invtrckno 
  loFormSet.ariaForm1.dtpShpDate.Value = ShipDate
ELSE
  loFormSet.ariaForm1.txtTrack.Enabled = .F.
  loFormSet.ariaForm1.dtpShpDate.Enabled = .F.
  loFormSet.ariaForm1.txtTrack.Value = ''
  loFormSet.ariaForm1.dtpShpDate.Value = {}
ENDIF

*!*************************************************************
*! Name      : lfvSndOrd
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Send Order Status 
*!*************************************************************
FUNCTION lfvSndOrd
PARAMETERS loFormSet
lcRpOrdFl = loFormSet.lcRpOrdFl
IF FILE(lcRpOrdFl)
  llRetVal = .F.
  TRY 
    ERASE (lcRpOrdFl)
  CATCH
     =gfModalGen('TRM00000B00000','ALERT','','','Could not create file '+ALLTRIM(lcRpOrdFl)+' .Cannot proceed.')
     llRetVal = .T.
  ENDTRY 
  IF llRetVal 
    RETURN 
  ENDIF
ENDIF 

SELECT(loFormSet.lcTmpOrdhdr)
lnOldRec = RECNO()
LOCATE 
lnOutFile = FCREATE(lcRpOrdFl,0)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Could not create file '+ALLTRIM(lcRpOrdFl)+' .Cannot proceed.')
  RETURN
ENDIF
lcCent = SET("Century" )
SET CENTURY ON
SCAN FOR LLSEL
  lcSegLine = ''  
  lcSegLine = lcSegLine + SUBSTR(ccontref,1,7)
  lcSegLine = lcSegLine + PADR(Order,8)
  lcSegLine = lcSegLine + PADR(StsCode,10)
  lcSegLine = lcSegLine + ALLTRIM(STR(YEAR(StsDate)))+'-'+PADL(ALLTRIM(STR(MONTH(StsDate))),2,'0')+'-'+PADL(ALLTRIM(STR(DAY(StsDate))),2,'0')
  lcSegLine = lcSegLine + PADR(StsMsgln1 + StsMsgln2,65)
  = FPUTS(lnOutFile,lcSegLine)  
  =gfSeek('O'+Order,'ORDHDR','ORDHDR')
  SELECT ORDHDR
  =gfReplace('lsendack  WITH .T.')
ENDSCAN 
=FCLOSE(lnOutFile)
SELECT ORDHDR
=gfTableUpdate()
SET CENTURY &lcCent.
=gfModalGen('TRM00000B00000','ALERT','','','File '+ALLTRIM(lcRpOrdFl)+' has been created successfully')
SELECT(loFormSet.lcTmpOrdhdr)
IF BETWEEN(lnOldRec ,1,RECCOUNT())
  GO RECORD lnOldRec 
ENDIF

*!*************************************************************
*! Name      : lfvSndShp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : Send Ship Status 
*!*************************************************************
FUNCTION lfvSndShp
PARAMETERS loFormSet


lcRpShpFl= loFormSet.lcRpShpFl
IF FILE(lcRpShpFl)
  llRetVal = .F.
  TRY 
    ERASE (lcRpShpFl)
  CATCH 
     =gfModalGen('TRM00000B00000','ALERT','','','Could not create file '+ALLTRIM(lcRpShpFl)+' .Cannot proceed.')
    llRetVal = .T.   
  ENDTRY   
  IF llRetVal 
    RETURN 
  ENDIF
ENDIF 

SELECT(loFormSet.lcTmpOrdhdr)
lnOldRec = RECNO()
LOCATE FOR LLSEL AND !EMPTY(INVOICE)
IF !FOUND()
  IF BETWEEN(lnOldRec ,1,RECCOUNT())
    GO RECORD lnOldRec 
  ENDIF
  RETURN 
ENDIF
SELECT(loFormSet.lcTmpOrdhdr)
LOCATE 
lnOutFile = FCREATE(lcRpShpFl,0)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Could not create file '+ALLTRIM(lcRpShpFl)+' .Cannot proceed.')
  RETURN
ENDIF
lcCent = SET("Century" )
SET CENTURY ON
SCAN FOR LLSEL AND !EMPTY(INVOICE)
  lcSegLine = ''  
  lcSegLine = lcSegLine + SUBSTR(ccontref,1,7)
  lcSegLine = lcSegLine + PADR(Order,8)
  lcSegLine = lcSegLine + SUBSTR(Invtrckno,1,20)
  lcSegLine = lcSegLine + ALLTRIM(STR(YEAR(ShipDate)))+'-'+PADL(ALLTRIM(STR(MONTH(ShipDate))),2,'0')+'-'+PADL(ALLTRIM(STR(DAY(ShipDate))),2,'0')
  lcSegLine = lcSegLine + PADR(Invoice,15)
  = FPUTS(lnOutFile,lcSegLine)  
ENDSCAN 
=FCLOSE(lnOutFile)
SET CENTURY &lcCent.
=gfModalGen('TRM00000B00000','ALERT','','','File '+ALLTRIM(lcRpShpFl)+' has been created successfully')
SELECT(loFormSet.lcTmpOrdhdr)
IF BETWEEN(lnOldRec ,1,RECCOUNT())
  GO RECORD lnOldRec 
ENDIF

*!*************************************************************
*! Name      : lfvAdjRela
*! Developer : Saber A.Razek (SAB)
*! Date      : 05/17/2012
*! Purpose   : Function to set and reset relation when browse for order
*!*************************************************************
FUNCTION lfvAdjRela
LPARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SELECT OrdHdr
    SET RELATION TO IIF(EMPTY(OrdHdr.Store), 'M', 'S')+PADR(OrdHdr.Account,5)+PADR(OrdHdr.Store,8) INTO Customer
  CASE lcParm = 'R'
    SELECT OrdHdr
    SET RELATION OFF INTO Customer
ENDCASE
ENDFUNC