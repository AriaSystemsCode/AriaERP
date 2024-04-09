*:****************************************************************
*: Program file  : POCUT.PRG
*: Program desc. : Docket Cut Figures
*: For screen    : POCUT.SPX
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Purchased Order (PO)
*: Developer     : AHMED MAHER (AMH)
*: Date          : 08/06/2003
*: Tracking Job Number : C#200570,1
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200570,1.
*:****************************************************************
*:Modifications  :
*B608280,1 TMI 09/23/2007 Define the browse fields lcBrFields variable contents ( ticket  T20070911.0003 ) 
*:****************************************************************
*:

EXTERNAL ARRAY laData , laDefProc , laKeyField
DECLARE laRec[9],laReceiv[8]
STORE ''  TO lcStatus,lcStyPict,lcOldPo,lcOldStyle,lcOldLineN
STORE .F. TO llbrowse,llGoNext,llHasRec
STORE 0   TO laRec,laReceiv,lnOldValue

*--ToolBar external proc.
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
DECLARE laKeyField [2,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'POSLN'
laKeyField[1,4] = 1

laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'POSLN'
laKeyField[2,4] = 2

STORE .F. TO llAlowNew
llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run

STORE 0  TO laRec,m.Cnt,m.nWo1,m.nWo2,m.nWo3,m.nWo4,m.nWo5,m.nWo6,m.nWo7,m.nWo8,m.nTotWo
STORE '' TO m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8
STORE 0  TO laReceiv,m.Cut1,m.Cut2,m.Cut3,m.Cut4,m.Cut5,m.Cut6,m.Cut7,m.Cut8,m.TotCut
lcTmpRec = gfTempName()

IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(lcBaseWind)
  lcStyPict = gfItemMask('PI')
  
  *-- Create the temp file of receipts records.
  CREATE CURSOR (lcTmpRec) (TRANCD C(1),DATE D,QTY1 N(6,0),QTY2 N(6,0),QTY3 N(6,0),QTY4 N(6,0),;
                            QTY5 N(6,0),QTY6 N(6,0),QTY7 N(6,0),QTY8 N(6,0),TOTQTY N(6,0))
  *-- Screen fields variable, to be used to build the laData array.
  SELECT POSLN
  lcScFields ='PO,STYLE,LINENO'
  SCATTER FIELDS &lcScFields. TO laData BLANK
ENDIF

*--Activate Options pad.
=lfActPad()

DO (gcScrDir+gcWinAppl+"\POCUT.SPX")

SELECT POSLN
SET FILTER TO

*--Release Option pad also
RELEASE PAD _Option OF _MSYSMENU
*-- End OF Code.

*:*************************************************************
*: Name      : lfvStyle
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/06/2003
*: Purpose   : Valid function to validate style field.
*:*************************************************************
*: Calls     : 
*:             Procedures : 
*:             Functions  : gfStyBrw()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfvStyle

IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF

PRIVATE lnAlias, lcFile_Ttl
DIMENSION laTempData[3]
STORE '' TO laTempData

lnAlias = SELECT(0)
SELECT POSLN
lcFile_Ttl = 'Styles'
laData[2] = IIF(llBrowse,'?',laData[2])

IF !EMPTY(laData[2]) .AND. !SEEK('P'+laData[1]+laData[2])
  lcKeyVal = '"P"' + IIF(EMPTY(laData[1]),'','+laData[1]') + ' FOR TRANCD="1"'
  *B608280,1 TMI [Start] Define the browse fields lcBrFields variable contents
  lcBrFields  = [Po , STYLE , Qty1 , Qty2, Qty3, Qty4, Qty5, Qty6, Qty7, Qty8, TotQty]
  *B608280,1 TMI [End  ] 
  =AriaBrow(lcKeyVal,lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,;
            'PO,STYLE,LINENO','laTempData')
  laData[2] = laTempData[2]
  laData[3] = laTempData[3]
  IF !EMPTY(laTempData[1])
    laData[1] = laTempData[1]
  ENDIF
  llBrowse = .F.
ENDIF

IF !EMPTY(laData[2])
  laData[3] = POSLN.LINENO
  =SEEK('P'+laData[1],'POSHDR')
  lcStatus     = POSHDR.STATUS
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS
  SHOW GET laData[1] DISABLE
  SHOW GET laData[2] DISABLE
  SHOW GET ibStyle   DISABLE
  SHOW GET ibPO      DISABLE
ENDIF

SELECT (lnAlias)
*-- End OF lfvStyle.

*:*************************************************************
*! Name      : lfvPO
*! Developer : AHMED MAHER
*! Date      : 08/06/2003
*! Purpose   : Valid function P/O #.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvPO

IF llBrowse OR ( !EMPTY(laData[1]) AND !SEEK('P'+laData[1],'POSHDR') )
  lcPo=laData[1]
  SELECT POSHDR
  DO POSBrow WITH lcPo,'','P'
  laData[1]=lcPo
  llBrowse = .F.
ENDIF

IF !EMPTY(laData[1]) .AND. POSHDR.STATUS $ 'XHS'
  lcPoStatus = IIF(POSHDR.STATUS='X','Canceld',;
               IIF(POSHDR.STATUS='H','Hold','Closed'))
  =gfModalGen("TRM000000B00000","DIALOG",'','',"Cannot working with "+lcPoStatus+" PO.")
  STORE '' TO laData
  _CUROBJ = OBJNUM(laData[1])
ENDIF
*-- end of lfvPO.

*:*************************************************************
*: Name      : lpShow
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/06/2003
*: Purpose   : Show function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lpShow()
*:*************************************************************
*:
FUNCTION lpShow

DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    STORE '' TO laData[1] , laData[2] , laData[3] , lcStatus
    STORE 0  TO laRec,m.Cnt,m.nWo1,m.nWo2,m.nWo3,m.nWo4,m.nWo5,m.nWo6,m.nWo7,m.nWo8
    STORE '' TO m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8
    STORE 0  TO laReceiv,m.Cut1,m.Cut2,m.Cut3,m.Cut4,m.Cut5,m.Cut6,m.Cut7,m.Cut8
    
    SHOW GET ibStyle ENABLE
    SHOW GET ibPO    ENABLE
    IF llGoNext
      SELECT POSLN
      laData[1] = lcOldPo
      =SEEK('P'+lcOldPo+lcOldStyle+lcOldLineN+'1')
      LOCATE REST WHILE CSTYTYPE+PO = 'P'+lcOldPo;
                    FOR STYLE+STR(LINENO,6) # lcOldStyle+lcOldLineN .AND. TRANCD = '1'
      IF FOUND()
        laData[2] = STYLE
        laData[3] = LINENO
      ELSE
        STORE '' TO laData[2],laData[3]
      ENDIF
      _CUROBJ = OBJNUM(laData[2])
      SHOW GET pbSlct ENABLE
      llGoNext = .F.
    ENDIF
  
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]
    IF llGoNext
      STORE .F. TO laScrMode
      STORE .T. TO laScrMode[1]
      SHOW GETS
      RETURN
    ENDIF
    
    =SEEK('P'+laData[1],'POSHDR')
    lcStatus = POSHDR.STATUS
    
    *-- recollect the data again.
    =lfCollect()
    
    SHOW GET ibStyle DISABLE
    SHOW GET ibPO    DISABLE
    SHOW GET lcStatus
  
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]
    SHOW GET ibStyle DISABLE
    SHOW GET ibPO    DISABLE
  
ENDCASE
SHOW GET pbUsrFields DISABLE
SHOW GET pbNotePad   DISABLE
SHOW GET pbAudTrail  DISABLE
SHOW GET pbDlt       DISABLE
*-- End OF lpShow.

*:*************************************************************
*: Name      : lfCollect
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/06/2003
*: Purpose   : Function to collect the data for the current PO line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfCollect()
*:*************************************************************
*:
FUNCTION lfCollect

PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT (lcTmpRec)
ZAP

SELECT POCUT
IF SEEK(laData[1]+laData[2]+STR(laData[3],6))
  SCATTER MEMVAR
ELSE
  STORE 0 TO m.nTotWo,m.totCut
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    STORE EVALUATE('POSLN.QTY'+lcI) TO ('m.nWo'+lcI)
    STORE 0 TO ('m.Cut'+lcI)
    m.nTotWo = m.nTotWo + EVALUATE('m.nWo'+lcI)
  ENDFOR
ENDIF

SELECT POSLN
lnRecNo = RECNO()
IF SEEK('P'+laData[1]+laData[2]+STR(laData[3],6))
  =SEEK(laData[2],'STYLE')
  =SEEK('S'+STYLE.SCALE,'SCALE')
  SELECT SCALE
  SCATTER FIELDS SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,CNT MEMVAR
  SELECT POSLN
  SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6) = 'P'+laData[1]+laData[2]+STR(laData[3],6);
              FOR TRANCD $ '245'
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      IF TRANCD $ '24'
        laRec[lnI] = laRec[lnI] + EVALUATE('QTY'+lcI)
        laRec[9]   = laRec[9]   + EVALUATE('QTY'+lcI)
      ENDIF
      laReceiv[lnI] = laReceiv[lnI] + EVALUATE('QTY'+lcI)
    ENDFOR
    SCATTER FIELDS TRANCD,DATE,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY MEMVAR
    INSERT INTO (lcTmpRec) FROM MEMVAR
  ENDSCAN
ENDIF
llHasRec = (RECCOUNT(lcTmpRec)>0)
SELECT POSLN
GO lnRecNo
SELECT (lnAlias)
*-- End OF lfCollect

*:*************************************************************
*: Name      : lpSavScr
*: Developer : AHMED MAHER
*: Date      : 08/06/2003
*: Purpose   : Save/Update.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpSavScr

IF m.TotCut <= 0
  =gfModalGen("TRM000000B00000","DIALOG",'','',"The total quantity cannot be equal zero.")
  llCSave = .F.
  RETURN
ENDIF

PRIVATE lnAlias
lnAlias = SELECT(0)

*-- Update the POCUT file.
SELECT POCUT
IF !SEEK(laData[1]+laData[2]+STR(laData[3],6))
  APPEND BLANK
  GATHER FIELDS &lcScFields. FROM laData
  GATHER FIELDS nWo1,nWo2,nWo3,nWo4,nWo5,nWo6,nWo7,nWo8,nTotWo MEMVAR
ENDIF
GATHER FIELDS Cut1,Cut2,Cut3,Cut4,Cut5,Cut6,Cut7,Cut8,TotCut MEMVAR

*-- Update the POSLN file.
SELECT POSLN
DECLARE laOldQty[8]
STORE 0 TO lnOldQty,laOldQty
IF SEEK('P'+laData[1]+laData[2]+STR(laData[3],6)+'1')
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOldQty
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE ('QTY'+lcI) WITH EVALUATE('m.Cut'+lcI)
  ENDFOR
  lnOldQty = TOTQTY
  REPLACE TOTQTY WITH m.TotCut
ENDIF

*-- Update the POSHDR file.
SELECT POSHDR
DECLARE laOpnQty[8],laOldOpn[8]
STORE 0 TO lnOpnQty,lnOldOpn,laOpnQty,laOldOpn
IF SEEK('P'+laData[1])
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laOpnQty[lnI] = MAX(EVALUATE('m.Cut'+lcI) - laReceiv[lnI],0)
    laOldOpn[lnI] = MAX(laOldQty[lnI] - laReceiv[lnI],0)
    lnOpnQty      = lnOpnQty + laOpnQty[lnI]
    lnOldOpn      = lnOldOpn + laOldOpn[lnI]
  ENDFOR
  REPLACE NSTYORDER WITH NSTYORDER - lnOldQty + m.TotCut
  REPLACE OPEN      WITH OPEN      - lnOldOpn + lnOpnQty
  REPLACE STATUS    WITH IIF(OPEN=0,'C','A')
ENDIF

*-- Update the STYLE file.
SELECT STYLE
IF SEEK(laData[2])
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE ('WIP'+lcI) WITH EVALUATE('WIP'+lcI) - laOldOpn[lnI] + laOpnQty[lnI]
    REPLACE ('NWO'+lcI) WITH EVALUATE('NWO'+lcI) - laOldQty[lnI] + EVALUATE('m.Cut'+lcI)
  ENDFOR
  REPLACE TOTWIP WITH TOTWIP - lnOldOpn + lnOpnQty
  REPLACE NTOTWO WITH NTOTWO - lnOldQty + m.TotCut
ENDIF

*-- Update the STYDYE file.
SELECT STYDYE
IF SEEK(laData[2]+POSLN.CWARECODE)
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE ('WIP'+lcI) WITH EVALUATE('WIP'+lcI) - laOldOpn[lnI] + laOpnQty[lnI]
    REPLACE ('NWO'+lcI) WITH EVALUATE('NWO'+lcI) - laOldQty[lnI] + EVALUATE('m.Cut'+lcI)
  ENDFOR
  REPLACE TOTWIP WITH TOTWIP - lnOldOpn + lnOpnQty
  REPLACE NTOTWO WITH NTOTWO - lnOldQty + m.TotCut
ENDIF

*-- Update the CTKTBOM and BOMLINE files.
IF gfModalGen("QRM00000B00006","DIALOG",'','',;
              "Do you wish to recalculate trim requirements?") = 1
  SELECT BOMLINE
  =SEEK('I1'+laData[1]+STR(laData[3],6)+'3'+laData[2])
  SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                  'I1'+laData[1]+STR(laData[3],6)+'3'+laData[2]
    STORE 0 TO lnReq1,lnReq2,lnReq3,lnReq4,lnReq5,lnReq6,lnReq7,lnReq8
    lnStyQty = 0
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF lcCount $ cSizes
        lnStyQty   = lnStyQty + m.Cut&lcCount
        lcCompSize = SUBSTR(cCompSizes,AT(lcCount,cSizes),1)
        IF !EMPTY(lcCompSize)
          lnReq&lcCompSize = lnReq&lcCompSize + m.Cut&lcCount
        ENDIF
      ENDIF
    ENDFOR
    lnOldQty = ItemQty
    lnOldAmt = ItemAmt
    REPLACE StyQty  WITH lnStyQty ,;
            ItemQty WITH StyQty*UnitQty ,; 
            ItemAmt WITH ItemQty*UnitCost
    
    SELECT CTKTBOM
    =SEEK('I'+laData[1]+Bomline.cBomTyp+Bomline.item+Bomline.iclr+Bomline.mfgcode+Bomline.Dyelot)
    REPLACE Pieces   WITH lnStyQty       ,;
            Req_qty  WITH Pieces*UntQty  ,;
            Est_Cost WITH Req_qty*UntCost,;
            Req_Qty1 WITH lnReq1*UntQty  ,;
            Req_Qty2 WITH lnReq2*UntQty  ,;
            Req_Qty3 WITH lnReq3*UntQty  ,;
            Req_Qty4 WITH lnReq4*UntQty  ,;
            Req_Qty5 WITH lnReq5*UntQty  ,;
            Req_Qty6 WITH lnReq6*UntQty  ,;
            Req_Qty7 WITH lnReq7*UntQty  ,;
            Req_Qty8 WITH lnReq8*UntQty
  ENDSCAN
ENDIF

STORE .T. TO llGoNext
lcOldPo    = POSLN.PO
lcOldStyle = POSLN.STYLE
lcOldLineN = STR(POSLN.LINENO,6)
SELECT (lnAlias)
*-- End Of lpSavScr.

*:*************************************************************
*: Name      : lfvCut
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/06/2003
*: Purpose   : Valid function of the Cut fields.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfvCut()
*:*************************************************************
*:
FUNCTION lfvCut
PARAMETERS lcCol

IF EVALUATE('m.Cut'+lcCol) < 0
  =gfModalGen("TRM000000B00000","DIALOG",'','',"Cut figures cannot be less than zero.")
  STORE lnOldValue TO ('m.Cut'+lcCol)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF EVALUATE('m.Cut'+lcCol) < laReceiv[VAL(lcCol)] - laRec[VAL(lcCol)]
  =gfModalGen("TRM000000B00000","DIALOG",'','',"Cut figures cannot be less than Canceled quantity.")
  STORE lnOldValue TO ('m.Cut'+lcCol)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

m.TotCut = m.TotCut - lnOldValue + EVALUATE('m.Cut'+lcCol)
SHOW GET m.TotCut
*-- end of lfvCut.

*:*************************************************************
*: Name      : lfwCut
*: Developer : AHMED MAHER (AMH)
*: Date      : 08/10/2003
*: Purpose   : When function of the Cut fields.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : =lfwCut()
*:*************************************************************
*:
FUNCTION lfwCut

lnOldValue = EVALUATE(SYS(18))
*-- end of lfwCut.

*!*************************************************************
*! Name      : lfActPad
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/06/2003
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad()
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'PO Cost Sheet' SKIP FOR !laScrMode[2]
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Receipts'      SKIP FOR !laScrMode[2] .AND. !llHasRec
ON SELECTION BAR 1 OF _OPTIONPOP DO lfPoCstSht
ON SELECTION BAR 2 OF _OPTIONPOP DO lfPoRec
*-- end of lfActPad.

*!*************************************************************
*! Name      : lfPoCstSht
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/06/2003
*! Purpose   : Display the PO Cost Sheet screen
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfPoCstSht()
*!*************************************************************
FUNCTION lfPoCstSht

PRIVATE llOpnBom
*-- Call the Po cost sheet program.
llNoShow    = .F.
llCallShow  = .T.
llShow      = .F.

lcParameter = "'" + laData[1] + "'"+",.F."
DO gpDoProg WITH "AWRPOCSSH", .F., "PO", lcParameter
*-- end of lfPoCstSht.

*!*************************************************************
*! Name      : lfBrowRec
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/06/2003
*! Purpose   : Browse Receipts
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowRec()
*!*************************************************************
FUNCTION lfBrowRec

SELECT (lcTmpRec)
lcBrowFlds = "cMarker =IIF(RECNO()=lnDMarker,'>',' '):H='':W=.F.,"+;
             "cReceipt=IIF(TRANCD='2','Stock',IIF(TRANCD='4','Damage','Canceled'))"+;
             ":H='Received to':R,"+;
             "Date:H='Date':R,"
FOR lnI = 1 TO m.Cnt
  lcI = STR(lnI,1)
  lcBrowFlds = lcBrowFlds + "Qty" + lcI + ":H=m.Sz" + lcI + ":R,"
ENDFOR
lcBrowFlds = lcBrowFlds + "TotQty:H='Total':R"
BROWSE FIELDS &lcBrowFlds.      ;
              WINDOW POCUTRE1   ;
              IN WINDOW POCUTREC;
              NOMENU            ;         
              NOAPPEND          ;
              NODELETE          ;
              NOWAIT            ;
              SAVE              ;
              NOCLEAR           ;
              WHEN lfwRecBrs()  ;
              TITLE lcRecTit
*-- end of lfBrowRec.

*!*************************************************************
*! Name      : lfwRecBrs
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
*! Purpose   : Show Receipts
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwStyBrs()
*!*************************************************************
FUNCTION lfwRecBrs

lndMarker = RECNO(lcTmpRec)
SHOW WINDOW (lcRecTit) REFRESH SAME
*-- end of lfwRecBrs.

*!*************************************************************
*! Name      : lfReadAct
*! Developer : AHMED MAHER
*! Date      : 08/04/2003
*! Purpose   : READ Activate function
*!*************************************************************
*! Calls     : lfClearKey.
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
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcRecTit)
*-- end of lfReadAct.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AHMED MAHER
*! Date      : 08/04/2003
*! Purpose   : Clear Hot Keys
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
*-- end of lfClearKey.

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
*! Purpose   : Deactivate main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDMainLot()
*!*************************************************************
FUNCTION lfDMainLot

IF WONTOP()= lcRecTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB     DO lpTabKey  WITH 'POCUTRE2','pbOk'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'POCUTRE2','pbOk'
ENDIF
RETURN .F.
*-- end of lfDMainLot

*!*************************************************************
*! Name      : lpTabKey
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTabKey WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpTabKey
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/04/2003
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'lcWinCh32', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck

ON KEY LABEL BACKTAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!**************************************************************************
*! Name      : lfPoRec
*! Developer : Ahmed Maher (AMH)
*! Date      : 08/05/2003
*! Purpose   : Function to display the Receipts screen.
*!**************************************************************************
*! Example   : =lfPoRec()
*!**************************************************************************
*
FUNCTION lfPoRec

lnDMarker = 1
lcRecTit  = 'Receipts'
lcOk      = gcBmpHome + "OK.BMP"
SELECT (lcTmpRec)
LOCATE

*-- Call the Receipts screen.
DO (gcScrDir+gcWinAppl+"\POCUTREC.SPX")
*-- end of lfPoRec.