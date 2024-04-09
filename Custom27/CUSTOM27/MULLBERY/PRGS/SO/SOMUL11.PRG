*:************************************************************************
*: Program file  : SoMUL11.PRG
*: Program desc. : Update Mulberry Dayton Hudson POs
*:
*:         System: Aria Apparel System
*:      Developer: WAM - Wael Aly Mohamed
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*:**********************************************************************
DECLARE laCodes[2,10],laSeasons[1,2],laDivision[1,2]
STORE '' TO laCodes,laSeasons,laDivision

STORE 1   TO lnSeason,lnDivision
STORE {}  TO ldLowDate,ldHighDate
STORE .F. TO llBrowse
STORE SPACE(5) TO lcAccount
lcTmpOrd = gfTempName()
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')

USE (gcDataDir+'ORDHDR')  AGAIN ORDER TAG Ordacct ALIAS EDIORDHD IN 0 
USE (gcDataDir+'ORDLINE') AGAIN ORDER TAG ORDLINE ALIAS EdiOrdLn IN 0 

SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store),'M'+Account,'S'+Account+Store) INTO CUSTOMER

=gfOpenFile(gcsyshome+'SYCCOMP',gcsyshome+'CCOMP_id','SH')
=SEEK(gcAct_Comp,'SYCCOMP')
lcCompName = SYCCOMP.cCom_Name             && Variable to hold the Company Name
lcCompPhon = SYCCOMP.cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format

DECLARE laCompAdd[6]
STORE '' TO laCompAdd
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6] = IIF(EMPTY(lcCompPhon),'',TRANSFORM(lcCompPhon , lcPhonPict))
=lfAdrShift('laCompAdd')

lcBrowseTl = ''
lcTranBrow = ''
lnTrans = 0
laCodes[1,1] = 'SEASON'
laCodes[1,2] = 'laSeasons'
laCodes[1,3] = 'lnSeason'
laCodes[1,4] = ''
laCodes[1,5] = .T.
laCodes[1,6] = .F.
laCodes[1,10] = 'SEASON'

laCodes[2,1] = 'CDIVISION'
laCodes[2,2] = 'laDivision'
laCodes[2,3] = 'lnDivision'
laCodes[2,4] = ''
laCodes[2,5] = .T.
laCodes[2,6] = .F.
laCodes[2,10] = 'cDivision'
llCollectData = .T.
lcOldValue = ''
lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcClose    = gcBmpHome + "Close2.bmp"
lcProceed  = gcBmpHome + "proceed.bmp"
lcPrint    = gcBmpHome + "printer.bmp"
lcSelect   = gcBmpHome + "SEL.bmp"

DO (gcScrDir+gcWinAppl+"\SOMUL11.SPX")

*!*************************************************************
*! Name      : lfvSelOrd
*! Developer : WAM
*! Date      : 05/17/2000
*! Purpose   : Select Dayton Hudson Orders
*!*************************************************************
*! Calls     : SOMULB.SPX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSelOrd()
*!*************************************************************
FUNCTION lfvSelOrd
PRIVATE ldDateFlt

IF llCollectData
  ldDateFlt = '.T.'
  DO CASE
    CASE !EMPTY(ldLowDate) AND !EMPTY(ldHighDate)
      ldDateFlt = "BETWEEN(ENTERED,ldLowDate,ldHighDate)"
    CASE EMPTY(ldLowDate) AND !EMPTY(ldHighDate)
      ldDateFlt = "ENTERED <= ldHighDate"
    CASE !EMPTY(ldLowDate) AND EMPTY(ldHighDate)
      ldDateFlt = "ENTERED >= ldLowDate"
  ENDCASE
  SELECT ORDHDR
  SELECT ORDHDR.Order,SPACE(1) AS cSelect,SPACE(1) AS Flag ;
  FROM ORDHDR WHERE Account+cordtype+order = lcAccount+'O' AND ;
             IIF(lnSeason=1,.t.,Season= SUBSTR(laSeasons[lnSeason,2],1,6)) AND  ;
             IIF(lnDivision=1,.t.,cDivision = SUBSTR(laDivision[lnDivision,2],1,6)) AND  &ldDateFlt ;
             INTO DBF (gcWorkDir+lcTmpOrd)
  INDEX ON Order TAG (lcTmpOrd)
  SET RELATION TO 'O'+ORDER INTO ORDHDR
  GO TOP
  llCollectData = .F.
ENDIF  
GO TOP IN (lcTmpOrd)
lcTranBrow = 'Sales Orders'
DO (gcScrDir+gcWinAppl+"\SOMULB.SPX")
SELECT (lcTmpOrd)
LOCATE FOR !EMPTY(cSelect)
IF FOUND()
  SHOW GET pbProceed ENABLE
ELSE
  SHOW GET pbProceed DISABLE
ENDIF
SHOW GET pbPrint DISABLE

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Validate Account
*!*************************************************************
*! Calls     : CUSBROWM,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount
PRIVATE xAccount

IF llBrowse .OR. (!EMPTY(lcAccount) .AND. !SEEK('M'+lcAccount,'CUSTOMER'))
  xAccount = lcAccount
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  lcAccount = xAccount
ENDIF
=lfRefresh('SOMUL11')
llBrowse = .F.

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Call functions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcType :
*!             'S' : Select/UnSelect
*!             'A' : Select All
*!             'V' : Invert
*!             'N' : Select None
*!             'O' : Switch
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSelect('S')
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcType
PRIVATE lcKey

SELECT (lcTmpOrd)
lcKey = Order
DO CASE
  CASE lcType = 'S'
    REPLACE cSelect WITH IIF(cSelect='»',' ','»')
  CASE lcType = 'A'
    REPLACE ALL cSelect WITH '»' 
  CASE lcType = 'N'
    REPLACE ALL cSelect WITH ' '
  CASE lcType = 'V'
    REPLACE ALL cSelect WITH IIF(cSelect='»',' ','»') 
ENDCASE
=SEEK(lcKey)
=lfwBrowTrn()


*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Screen Activate function
*!*************************************************************
*! Calls     : lfClearKey,gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfDeAct
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Screen Deactivate function
*!*************************************************************
*! Calls     : lpAction, lpTab, lpBackTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDeAct()
*!*************************************************************
FUNCTION lfDeAct

IF WONTOP()=lcTranBrow
  ON KEY LABEL ALT+S DO lpAction WITH 'S'
  ON KEY LABEL ALT+A DO lpAction WITH 'A'
  ON KEY LABEL ALT+N DO lpAction WITH 'N'
  ON KEY LABEL ALT+V DO lpAction WITH 'V'

  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab     WITH 'SOMULB2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'SOMULB2','pbClose'
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpAction
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Call functions
*!*************************************************************
*! Calls     : lfvSwitch,lfvSelect
*!*************************************************************
*! Parameters: lcType :
*!             'S' : Select/UnSelect
*!             'A' : Select All
*!             'V' : Invert
*!             'N' : Select None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpAction WITH 'S'
*!*************************************************************
PROCEDURE lpAction
PARAMETERS lcType

=lfvSelect(lcType)

*!*************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'EBSNDTR2','pbSelect'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'EBSNDTR2','pbSelect'
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : WAM
*! Date      : 05/17/2000
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+S 
ON KEY LABEL ALT+A 
ON KEY LABEL ALT+N 
ON KEY LABEL ALT+V 
ON KEY LABEL ALT+B

*!*************************************************************
*! Name      : lfBrowTran
*! Developer : WAM
*! Date      : 05/17/2000
*! Purpose   : Browse Dayton Hudson Orders
*!*************************************************************
*! Calls     : lfwBrowTrn
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowTran()
*!*************************************************************
FUNCTION lfBrowTran
lcSelect = SELECT()
SELECT (lcTmpOrd)
lnTrans = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnTrans,'>',' '):H=' ':R:1:W=.F.,cSelect :H=' ' :R,"
lcFields = lcFields + "Order  :R,OrdHdr.Season :R,OrdHdr.cDivision :R :H='Division',OrdHdr.Status :R,x=IIF(OrdHdr.Multi='Y', '*MULTI*',OrdHdr.Store) :R :H='Store',"
lcFields = lcFields + "OrdHdr.Start :R ,OrdHdr.OpenAmt :P='9999999.99' :R :H='Open Amnt.',OrdHdr.CustPO :R,OrdHdr.Complete :R,ordhdr.ApprAmt :R :H='Approved'"
BROWSE FIELDS &lcFields  ;
       WINDOW SOMULB1    ;
       IN WINDOW SOMULB  ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwBrowTrn() ;
       TITLE lcTranBrow
SELECT (lcSelect)

*!*************************************************************
*! Name      : lfwBrowTrn
*! Developer : WAM
*! Date      : 05/17/2000
*! Purpose   : Show Sales Orders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwBrowTrn()
*!*************************************************************
FUNCTION lfwBrowTrn

SELECT (lcTmpOrd)
lnTrans = RECNO()
SHOW WINDOW (lcTranBrow) REFRESH SAME
IF EMPTY(cSelect)
  SHOW GET pbSelect,1 ENABLE PROMPT  '\<Select'
ELSE
  SHOW GET pbSelect,1 ENABLE PROMPT  'Un\<Select'
ENDIF

*!*************************************************************
*! Name      : lfvProceed
*! Developer : WAM
*! Date      : 05/17/2000
*! Purpose   : 
*!*************************************************************
*! Calls     : gfDispRep
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed
SELECT (lcTmpOrd)
SCAN
  =SEEK('O'+&lcTmpOrd..Order,'OrdHdr')
  IF lfGetOrder(OrdHdr.CUSTPO,OrdHdr.ACCOUNT,OrdHdr.STORE)
    SET ORDER TO TAG Ordlinst IN EdiOrdLn
    
    SELECT OrdHdr
    REPLACE CustPo    WITH EDIORDHD.CustPo    ,; 
            BLANK_ORD WITH EDIORDHD.BLANK_ORD ,;
            DISTRB_NO WITH EDIORDHD.DISTRB_NO

    SELECT (lcTmpOrd)
    REPLACE FLAG WITH 'Y'
    
    SELECT ORDLINE
    =SEEK('O'+ORDHDR.Order)
    SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+OrdHdr.Order
      IF SEEK('T'+EdiOrdHd.Order+Store+Style,'EDIORDLN')
        REPLACE NOTE_MEM WITH EdiOrdLn.NOTE_MEM
      ENDIF
    ENDSCAN
  ELSE
    REPLACE FLAG WITH 'N'
  ENDIF
ENDSCAN
SELECT (lcTmpOrd)
DELETE ALL FOR EMPTY(cSelect)
gcDevice = "SCREEN"
DO gfDispRep WITH '\SO\SOMUL11.FRX'
llCollectData = .T.
SHOW GET pbProceed DISABLE
SHOW GET pbPrint   ENABLE

*!*************************************************************
*! Name      : lfGetOrder
*! Developer : WAM
*! Date      : 05/17/2000
*! Purpose   : Get Dayton Hudson EDI temp order 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcCustPo : Custom PO
*!                       lcAccount: Account
*!                       lcStore  : Store
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetOrder()
*!*************************************************************
FUNCTION lfGetOrder
PARAMETERS lcCustPo,lcAccount,lcStore

IF ORDHDR.Multi='Y'
  =SEEK('O'+OrdHdr.Order,'ORDLINE')
  SET ORDER TO TAG ORDLINE IN EdiOrdLn
  SELECT EdiOrdHd
  =SEEK(lcAccount+'T')
  llFound=.F.
  SCAN REST WHILE Account+cordtype+order = lcAccount+'T';
            FOR SUBSTR(CustPo,5,7) = ALLTRIM(lcCustPo)
    SELECT EdiOrdLn
    =SEEK('T'+EdiOrdHd.ORDER)
    LOCATE REST WHILE cordtype+order+STR(lineno,6) = 'T'+EdiOrdHd.Order ;
                FOR   Store = OrdLine.Store
    IF FOUND()
      llFound=.T.
      EXIT
    ENDIF
  ENDSCAN
  SELECT ORDHDR
  RETURN(llFound)
ELSE
  SELECT EdiOrdHd
  =SEEK(lcAccount+'T')
  LOCATE REST WHILE Account+cordtype+order = lcAccount+'T';
              FOR SUBSTR(CustPo,5,7) = ALLTRIM(lcCustPo)
  RETURN(FOUND())
ENDIF

*!*************************************************************
*! Name      : lfPrint
*! Developer : Wael Aly Mohamed
*! Date      : 05/17/2000
*! Purpose   : Print Updated orders
*!*************************************************************
*! Calls     : gfDispRep 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfPrint()
*!*************************************************************
FUNCTION lfvPrint

SELECT (lcTmpOrd)
DELETE ALL FOR EMPTY(cSelect)

IF pSetup(.T.)
  DO gfDispRep WITH '\SO\SOMUL11.FRX'
ENDIF
llCollectData = .T.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVA.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
