*!**************************************************************************
*! Name      : MemMain.PRG
*! Developer : Walid A. Wahab (WAB)
*! Date      : 03/12/2003
*! Purpose   : Memory Fashions  Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modification :- 
*!B607246,1 ABD 06/08/2003 Fix More than One bug.
*!B122221,1 NNA 04/22/2004 Fix bug that the receiving to carton dealing only with one po in the Shipment
*!C123616,1 TMI 25/09/2004 Changes to the entries C200488,C200489
*!B125800,1 NNA 01/06/2005 fix bug that the program take more time to save in both of PO shipment or Receiving
*!B126937,1 TMI 03/17/2005 if the style is not assigned to the selcted warehouse , ask for this
*!B126708,1 TMI 04/04/2005 Fix problem that Pik by carton screen show cartons not related to the seleced WH in the allocate by order screen
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars
*B122221,1 NNA 04/22/2004 (Begin) New variable to hold the PO# number
STORE 0 TO lnPoNo
*B122221,1 NNA (End)
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfCREATTMP
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : open the Carton Type and Create the Temp.files 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCREATTMP()
*!**************************************************************************
*
FUNCTION lfCREATTMP
PRIVATE lnAlias
lnAlias = SELECT()
lcCrtnHdr = gfTempName()
lcCrtnLine= gfTempName()
lcScaleFle= gfTempName()
lcPoCrTmp = gfTempName()

IF !USED(lcScaleFle)
  =gfOpenFile(gcDataDir+'SCALE','SCALE','SH',@lcScaleFle,.T.)
ENDIF
=gfOpenFile(gcDataDir+'POCRTNMF','POCRTNMF','SH')
DIMENSION laFileStru[1,4]
SELECT POCRTNMF
=AFIELDS(laFileStru)

lnFldNo =ALEN(laFileStru,1)
DIMENSION laFileStru[lnFldNo+1,4]
laFileStru[lnFldNo+1,1] = 'CrtnNo'
laFileStru[lnFldNo+1,2] = 'N'
laFileStru[lnFldNo+1,3] =  5
laFileStru[lnFldNo+1,4] =  0

CREATE TABLE (gcWorkDir+lcCrtnHdr) FROM ARRAY laFileStru
INDEX ON cCartontyp+Style TAG lcCrtnHdr OF (gcWorkDir+lcCrtnHdr)

lnFldNo =ALEN(laFileStru,1)
DIMENSION laFileStru[lnFldNo+9,4]
laFileStru[lnFldNo+1,1] = 'SCALE'
laFileStru[lnFldNo+1,2] = 'C'
laFileStru[lnFldNo+1,3] =  3
laFileStru[lnFldNo+1,4] =  0

FOR lnCount = 1 TO 8 
  lcCount = ALLTRIM(STR(lnCount))
  laFileStru[lnFldNo+1+lnCount,1] = 'OldQty'+lcCount
  laFileStru[lnFldNo+1+lnCount,2] = 'N'
  laFileStru[lnFldNo+1+lnCount,3] =  6
  laFileStru[lnFldNo+1+lnCount,4] =  0
ENDFOR

CREATE TABLE (gcWorkDir+lcCrtnLine) FROM ARRAY laFileStru
INDEX ON Style+cCartonType TAG lcCrtnLine OF (gcWorkDir+lcCrtnLine)

SET RELATION TO 'S'+Scale INTO (lcScaleFle) ADDITIVE

CREATE Cursor  'tempname' (ctempname M(10))
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcCrtnHdr 
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcCrtnLine 
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcScaleFle
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcPoCrTmp

SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfCartnScr
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Call The Carton Screen
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCartnScr()
*!**************************************************************************
*
FUNCTION lfCartnScr
llModRecpt = .T.
IF lfCheckPO()
  IF gfModalGen("TRM00000B42002","DIALOG",.F.,.F.,'Enter Carton Details?.') = 1
    lcHldTab = ON('KEY','TAB')
    lcHldBtb = ON('KEY','BACKTAB')    
    lcHldEnt = ON('KEY','ENTER')
    ON KEY LABEL TAB    
    ON KEY LABEL BACKTAB
    ON KEY LABEL ENTER
    *B122221,1 NNA 04/22/2004 (Begin) Scan for all PO To Receiving all of Them , one PO in the one Session
    SELECT (lcTmpline)
    GO TOP
    lnRecNo = RECNO()
    SCAN FOR TRANCD $ '24'
      lnPoNo = &lcTmpline..PO
      lcStyMajor = PADR(Style,lnMjrWid)
    *B122221,1 NNA (End)
      =lfCallScrn()
    *B122221,1 NNA 04/22/2004 (Begin) End Scan
    ENDSCAN
    *B122221,1 NNA (End)

    IF !llModRecpt
      STORE .F. TO llShow,llCSave
    ENDIF
    RETURN llModRecpt
    ON KEY LABEL TAB     &lcHldTab
    ON KEY LABEL BACKTAB &lcHldBtb
    ON KEY LABEL ENTER   &lcHldEnt
  ENDIF
ENDIF
RETURN llModRecpt

*!**************************************************************************
*! Name      : lfCallScrn
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Call The Carton Type Screen
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCREATTMP()
*!**************************************************************************
FUNCTION lfCallScrn
PRIVATE lcWinCh5,lcWinCh6,lcWinCh7,lcWinCh8,lcChck,lcUnChck,lcRLTit,;
        lcFab,lcClr,lcWare,lcDye,lnTotApply,lnUsrApply,lnRcvdQty

PRIVATE lcFileToUse,llBrowse
PRIVATE lcSerchExp ,lcStyHdr 
SELECT TempName
SCAN 
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

STORE SPACE(0) To lcSerchExp
llNewCrt = .T.
llBrowse = .F.
lnRcvdQty = 0 
SELECT (lcTmpline)
lnRecNo = RECNO()

*B122221,1 NNA 04/22/2004 (Begin) Scan For Transaction And The PO # Number
*SCAN FOR TRANCD $ '24' 
SCAN FOR TRANCD $ '24' .AND. PO = lnPoNo
*B122221,1 NNA (End)
  lnRcvdQty = lnRcvdQty + TotQty
  lcStyle  = PADR(IIF(TRANCD='2',Style,cRetSty),lnMjrWid)
ENDSCAN
SELECT (lcTmpline)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF

lcFileToUse=lcCrtnHdr
lnNewRec = RECNO(lcFileToUse)

lcStyHdr =  gfItemMask('HM')
STORE SPACE(0) TO lcWinCh5,lcWinCh6,lcWinCh7,lcWinCh8
lcDesc   = IIF(SEEK(PADR(lcStyle,lnMjrWid),'Style'),Style.Desc,'')

lnTotCrtNo = 0
lcWare = ''
lcDye  = ''

STORE 0 TO lnOldVal

STORE 0 TO lnUsrApply
STORE 0 TO lnTotApply

*B122221,1 NNA 04/22/2004 (Begin) Show the PO# Number in The screen's Title
*lcRLTit = 'Carton types'
lcRLTit = 'Carton types for PO# ' + lnPoNo
*B122221,1 NNA (End)

*E301345,1 (Start)
lcLotRo  = gfTempName()
lcLotRo1 = gfTempName()
lcLotRo2 = gfTempName()
lcLotRo3 = gfTempName()
*E301345,1 (End)

PRIVATE lcRollTitl,lcRlBrFild
lcRlBrFild = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "cCartonTyp :R :H='Carton Type':17,"+;
             "Weight :R :10,"+;
             "nLength :R :H='Length' :10,"+;
             "Width :R :10,"+;
             "nDepth :R :H='Depth' :10,"+;
             "CrtnNo :R :H='No Of Cartons' :17"
lcRollTitl = 'Cartons'
*-- Call the screen
PUSH KEY
lcSysMen = SET('SYSMENU')
SET SYSMENU ON
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcRollTitl)

RELEASE PAD _BROWSE OF _MSYSMENU
DO (gcScrDir+'PO\POCRTYP.SPX')

SET SYSMENU &lcSysMen
POP KEY
RETURN 

*!**************************************************************************
*! Name      : lfActBrwRo
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : activte the Browse window ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfActBrwRo()
*!**************************************************************************
FUNCTION lfActBrwRo

SELECT(lcFileToUse)
LOCATE
lnBrRecNo = RECNO(lcFileToUse)

BROWSE FIELDS &lcRlBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwRo();
       VALID :F lfvBrwRo();
       TITLE lcRollTitl   ;
       WINDOW lcWinCh6 IN WINDOW lcWinCh5
=lfwBrwRo()

*!**************************************************************************
*! Name      : lfwBrwRo
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : When Function of the Browse window ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfwBrwRo()
*!**************************************************************************
FUNCTION lfwBrwRo
SELECT (lcFileToUse)
lnBrRecNo = RECNO()
SUM CrtnNo TO lnTotCrtNo
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF
SHOW WINDOW (lcRollTitl) REFRESH SAME
IF lnTotCrtNo < 1 
  SHOW GET pbRModify DISABLE 
  SHOW GET pbRRemove DISABLE
ELSE
  SHOW GET pbRModify ENABLE
  SHOW GET pbRRemove ENABLE
ENDIF

=lfLOtRoRef()

*!**************************************************************************
*! Name      : lfvBrwRo
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid Function of the Browse window ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfvBrwRo()
*!**************************************************************************
FUNCTION lfvBrwRo

IF WONTOP() # (lcRollTitl)
  =gfStopBrow()
ENDIF

*!**************************************************************************
*! Name      : lfTrap
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap  Function of the Browse window ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfTrap()
*!**************************************************************************
FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcRollTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab
  ON KEY LABEL BACKTAB DO lfBrBack
  ON KEY LABEL ESCAPE  DO lfEscape
ENDIF

*!**************************************************************************
*! Name      : lfClrTrap
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Clear Trap Function of the Browse window ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfClrTrap()
*!**************************************************************************
FUNCTION lfClrTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESCAPE  DO lfEscape

*!**************************************************************************
*! Name      : lfBrTab
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the TAB KEY ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfBrTab()
*!**************************************************************************
FUNCTION lfBrTab
ON KEY LABE TAB
ACTIVATE WINDOW (lcLotRo3)
_CUROBJ = OBJNUM(pbRNew)

*!**************************************************************************
*! Name      : lfBrBack
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the SHIEFT TAB KEY ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfBrBack()
*!**************************************************************************
FUNCTION lfBrBack

ON KEY LABE BACKTAB	
ACTIVATE WINDOW (lcLotRo3)
_CUROBJ = OBJNUM(pbRClose)

*!**************************************************************************
*! Name      : lfEscape
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the ESC KEY ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfEscape()
*!**************************************************************************
FUNCTION lfEscape

ON KEY LABE ESCAPE
*= lfvpbClose()
=lfVldCrtOk()
*!**************************************************************************
*! Name      : lfwOldVals
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Old Value Function 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfwOldVals()
*!**************************************************************************
FUNCTION lfwOldVals

lnOldVal  = EVALUATE(SYS(18))



*:**************************************************************************
*:* Name        : lfActRolScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/23/2004
*:* Purpose     : 
*:***************************************************************************
FUNCTION lfActRolScr

ON KEY LABEL ESCAPE
*ON KEY LABEL ESCAPE DO lfvbpCancel

*!**************************************************************************
*! Name      : lfLOtRoBrw
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Display the Browse Screen ( Carton Stypes Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfLOtRoBrw()
*!**************************************************************************
FUNCTION lfLOtRoBrw
SELECT(lcFileToUse)
GO TOP
lnNewRec = RECNO(lcFileToUse)
BROWSE FIELDS &lcRlBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOEDIT             ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwRo();
       VALID :F lfvBrwRo();
       TITLE lcRollTitl   ;
       WINDOW (lcLotRo2) IN WINDOW (lcLotRo)
= lfwBrwRo()


*!**************************************************************************
*! Name      : lfVldRmv
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid of the remove Button ( Carton Stypes Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfVldRmv()
*!**************************************************************************
FUNCTION lfVldRmv
PRIVATE lnAlias , lnTotqty , lnRecNo
lnAlias = ALIAS()
SELECT (lcCrtnHdr)
lcCartnTyp = cCartonTyp 

IF SEEK(ALLTRIM(lcStyle),lcCrtnLine)
  SELECT (lcCrtnLine)
  DELETE REST WHILE STYLE = ALLTRIM(lcStyle) FOR cCartonTyp = lcCartnTyp
ENDIF
SELECT (lcCrtnHdr)
DELETE 
LOCATE
=lfwBrwRo()

*!**************************************************************************
*! Name      : lfVldCrtOk
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid of the Close Button ( Carton Stypes Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfVldCrtOk()
*!**************************************************************************
FUNCTION lfVldCrtOk
PRIVATE lnAlias
lnAlias = SELECT()

lnTotQty = 0
SELECT (lcCrtnLine)

*B122221,1 NNA (Begin) Seek For the style to get it's total qty. and compare it with the receiving qty.
IF SEEK(LCSTYLE)
  *SCAN FOR TOTqty <> 0
  SCAN REST WHILE Style+cCartonType = lcStyle FOR TOTqty <> 0
*B122221,1 NNA (End)

    lnTotQty = lnTotQty + (TotQty*CrtnNo)
  ENDSCAN

*B122221,1 NNA (Begin) Close if Statement
ENDIF
*B122221,1 NNA (End)

IF lnRcvdQty <> lnTotQty 
  IF gfModalGen("TRM00000B34019","DIALOG",.F.,.F.,'The total cartoned quantity is less than the total received quantity.') = 2
     RETURN
  ENDIF
  llModRecpt = .F.
ENDIF
SELECT (lnAlias)
*Clear READ

*!**************************************************************************
*! Name      : lfCartDtls
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Call the Carton Details Screen
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCartDtls()
*!**************************************************************************
FUNCTION lfCartDtls
PARA llNewRcrds
llNewCrt = llNewRcrds
lcOldTab = ON('KEY','TAB')
lcOldBtb = ON('KEY','BACKTAB')    
lcOldEnt = ON('KEY','ENTER')
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
=lfDetlScrn()
ON KEY LABEL TAB     &lcOldTab
ON KEY LABEL BACKTAB &lcOldBtb
ON KEY LABEL ENTER   &lcOldEnt

=lfwBrwRo()
RETURN

*!**************************************************************************
*! Name      : lfDetlScrn
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Call the Carton Details Screen
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfDetlScrn()
*!**************************************************************************
FUNCTION lfDetlScrn
PRIVATE lcWinCRt5,lcWinCrt6,lcWinCRt7,lcWinCRt8,lcChck,lcUnChck,lcRLTit,;
        lnWeight,lnLength,lnWidth,lnDepth,lnNOfCrtn,lcFields,lcSize1,lcSize2,;
        lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8,lcPoNo,lcCrtStat

PRIVATE lcUsedFile,llBrowse
PRIVATE lcSerchExp ,lcStyHdr ,lcNnMjrHdr ,lnNonMjrLn 
lcCrtStat = IIF(llNewCrt,"ENABLE","DISABLE")
lnWeight = 0
lnLength = 0 
lnWidth  = 0 
lnDepth  = 0
lnNOfCrtn= 1

llBrowse = .F.
lcUsedFile=lcCrtnLine
lnNewRec = RECNO(lcUsedFile)

IF llNewCrt
  SELECT (lcUSedFile)
  lcCartnTyp = lfSequence()
  SELECT (lcTmpLine)
  lnRecNo = RECNO()
  lcPoNo  = PO
  LOCATE
  *B122221,1 NNA 04/22/2004 (Begin) Scan For the Transaction and The PO# Number
  *SCAN FOR TRANCD $ '24' 
  SCAN FOR TRANCD $ '24' .AND. PO = lnPoNo
    *B122221,1 NNA (End)
  
    SCATTER MEMVAR MEMO
    SELECT (lcUsedFile)
    APPEND BLANK
    REPLACE cCartonTyp WITH lcCartnTyp ,;
            STYLE      WITH IIF(m.Trancd='2',m.Style,m.cRetSty) ,;
            SCALE      WITH m.Scale
  ENDSCAN
  SELECT (lcTmpLine)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNO
  ENDIF
ELSE
  SELECT (lcCrtnHdr)
  lcCartnTyp = cCartonTyp
  lcPoNo     = &lcTmpLine..PO
  lnWeight   = weight     
  lnLength   = nlength    
  lnDepth    = ndepth     
  lnWidth    = VAL(width) 
  lnNofCrtn  = CrtnNo     
  SELECT (lcUSedFile)
  SCAN FOR cCartonTyp = lcCartnTyp
    FOR lnCount = 1 To 8
      lcCount = ALLTRIM(STR(lnCount))
      REPLACE OldQty&lcCount WITH Qty&lcCount
    ENDFOR
  ENDSCAN
ENDIF
SELECT (lcUSedFile)

*B122221,1 NNA (Start) Stop Command [Locate] To not moving the Pointer to the First Record and
*B122221,1 NNA         get the current style for the current PO
*LOCATE
*B122221,1 NNA (End)

lcFields = "Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty"
SCATTER FIELDS &lcFields  MEMVAR 

*--NonMajor Header
lcStyHdr   =  gfItemMask('HM')
lcNnMjrHdr =  gfItemMask('HN')
lnNonMjrLn =  LEN(gfItemMask('PN'))
STORE SPACE(0) TO lcWinCRt5,lcWinCrt6,lcWinCrt7,lcWinCrt8
STORE SPACE(0) TO lcSize1,lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8
lcStyle  = PADR(m.Style,lnMjrWid)
lcDesc   = IIF(SEEK(PADR(lcStyle,lnMjrWid),'Style'),Style.Desc,'')
lnTotCrtNo = 0

STORE 0 TO lnOldVal

STORE 0 TO lnUsrApply
STORE 0 TO lnTotApply

lcRLTit = 'Colors Details'


lcCrtDet  = gfTempName()
lcCrtDet1 = gfTempName()
lcCrtDet2 = gfTempName()
lcCrtDet3 = gfTempName()


PRIVATE lcCrtTitl,lcCrtBrFld

lcCrtBrFld = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "Color=ALLTRIM(RIGHT(Style,lnNonMjrLn)) :H='COLOR' :12,"+;
             "Qty1   :R :6,"+;
             "Qty2   :R :6,"+;
             "Qty3   :R :6,"+;
             "Qty4   :R :6,"+;
             "Qty5   :R :6,"+;
             "Qty6   :R :6,"+;
             "Qty7   :R :6,"+;
             "Qty8   :R :6,"+;
             "TotQty :R :H='Total Qty' :9"
             
*B122221,1 NNA 04/22/2004 (Begin) Show the PO# Number in The screen's Title
*lcCrtTitl = 'Carton Details'
lcCrtTitl = 'Carton Details For PO# ' + lnPoNo
*B122221,1 NNA (End)

*-- Call the screen
PUSH KEY
lcSysMen = SET('SYSMENU')
SET SYSMENU ON
ON KEY LABEL ALT+C ACTIVATE WINDOW (lcCrtTitl)

RELEASE PAD _BROWSE OF _MSYSMENU
DO (gcScrDir+'PO\POCRTON.SPX')

SET SYSMENU &lcSysMen
POP KEY
RETURN 


*!**************************************************************************
*! Name      : lfActBrwCR
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Activate  the Carton Details Screen
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfActBrwCR()
*!**************************************************************************
FUNCTION lfActBrwCR

SELECT(lcUsedFile)
LOCATE
lnNewRec = RECNO(lcUsedFile)

BROWSE FIELDS &lcCrtBrFld FOR cCartonTyp = lcCartnTyp ; 
       LOCK 0             ;
       SAVE               ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwCrt();
       VALID :F lfvBrwCrt();
       TITLE lcCrtTitl   ;
       WINDOW lcWinCrt6 IN WINDOW lcWinCrt5
=lfwBrwCrt()

*!**************************************************************************
*! Name      : lfwBrwCrt
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : When function of the Browse screen ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfwBrwCrt()
*!**************************************************************************
FUNCTION lfwBrwCrt
SELECT (lcUsedFile)
lnBrRecNo = RECNO()
SCATTER FIELDS &lcFields  MEMVAR 
SHOW WINDOW (lcCrtTitl) REFRESH SAME
FOR lnCount = 1 To 8
  lcCount = ALLTRIM(STR(lncount))
  SHOW GET m.qty&lcCount
ENDFOR
=lfLOtRoRef()

*!**************************************************************************
*! Name      : lfvBrwCrt
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid function of the Browse screen ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfvBrwCrt()
*!**************************************************************************
FUNCTION lfvBrwCrt

IF WONTOP() # (lcCrtTitl)
  =gfStopBrow()
ENDIF

*!**************************************************************************
*! Name      : lfCrtTrap
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap key  function of the Browse screen ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfvBrwCrt()
*!**************************************************************************
FUNCTION lfCrtTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcCrtTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrCrtTab
  ON KEY LABEL BACKTAB DO lfBrCrtBack
  *C123616,1  TMI [Start] release ESC key definition
  *ON KEY LABEL ESCAPE  DO lfCrtEscape
  ON KEY LABE ESCAPE llDummy = .T.
  *C123616,1  TMI [End  ] 
ENDIF

*!**************************************************************************
*! Name      : lfCrtEscape
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the ESC KEY ( Carton Details Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCrtEscape()
*!**************************************************************************
FUNCTION  lfCrtEscape
ON KEY LABE ESCAPE
=lfVCancel()

*!**************************************************************************
*! Name      : lfCrClrTrp
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Clear Trap Function of the Browse window ( Carton Types Screen )
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCrClrTrp()
*!**************************************************************************
FUNCTION lfCrClrTrp

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*C123616,1  TMI [Start] release ESC key definition
*ON KEY LABEL ESCAPE  DO lfCrtEscape
ON KEY LABE ESCAPE llDummy = .T.
*C123616,1  TMI [End  ] 

*!**************************************************************************
*! Name      : lfBrCrtTab
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the TAB key   ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfBrCrtTab()
*!**************************************************************************
FUNCTION lfBrCrtTab
ON KEY LABE TAB
ACTIVATE WINDOW (lcCrtDet3)
_CUROBJ = OBJNUM(m.Qty1)

*!**************************************************************************
*! Name      : lfBrCrtTab
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the SHIFT TAB key   ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfBrCrtTab()
*!**************************************************************************
FUNCTION lfBrCrtBack

ON KEY LABE BACKTAB	
ACTIVATE WINDOW (lcCrtDet1)
_CUROBJ = OBJNUM(lnDepth)


*!**************************************************************************
*! Name      : lfActCrtScr
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Activate the Carton Details Screen 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfActCrtScr()
*!**************************************************************************
FUNCTION lfActCrtScr
ON KEY LABEL ESCAPE
ON KEY LABEL ESCAPE DO lfvbpCancel

*!**************************************************************************
*! Name      : lfLOtCrtBrw
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Display the browse screen  ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfLOtCrtBrw()
*!**************************************************************************
FUNCTION lfLOtCrtBrw
SELECT(lcUsedFile)
GO TOP
lnNewRec = RECNO(lcUsedFile)
BROWSE FIELDS &lcCrtBrFld FOR cCartonTyp = lcCartnTyp ; 
       LOCK 0             ;
       SAVE               ;
       NOEDIT             ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwCrt();
       VALID :F lfvBrwcRt();
       TITLE lcCrtTitl   ;
       WINDOW (lcCrtDet2) IN WINDOW (lcCrtDet)
= lfwBrwCrt()

*!**************************************************************************
*! Name      : lfvSizeQty
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : validate the size qty ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfvSizeQty()
*!**************************************************************************
FUNCTION lfvSizeQty
PARAM lcSize

lcSize = ALLTRIM(lcSize)
PRIVATE lnAlias ,lnSizeQty 
lnAlias   = ALIAS()
IF m.Qty&lcSize < 0 
  *Negative values are not allowed.
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcSize = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnSizeQty = 0
SELECT (lcTmpLine)
lcORDER = ORDER()
SET ORDER TO TAG 'TMPLINE3'
lnRecNo=RECNO()
IF SEEK(lcPoNo+m.Style)
  SCAN REST WHILE  Po+Style= lcPoNo+m.Style for TRANCD = '2'
    lnSizeQty = lnSizeQty + QTY&lcSize
  ENDSCAN
ENDIF
*--inc case of damage or second quality
SCAN FOR TRANCD = '4' AND cRetSty = m.Style
  lnSizeQty = lnSizeQty + QTY&lcSize
ENDSCAN
SET ORDER TO TAG &lcORDER

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF

lntotalsize = ( m.Qty&lcSize * lnNOfCrtn )+ lfGetSizeQty(lcSize)
IF lnTotalSize > lnSizeQty
  
  =gfModalGen("TRM00000B42000","DIALOG",.F.,.F.,'This quantity exceeds the total received quantity.')
  m.Qty&lcSize = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
SELECT (lcCrtnLine)
REPLACE QTY&lcSize WITH m.QTY&lcSize
REPLACE TOTQTY     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
m.TotQty = TotQty
=lfLOtRoRef()
SHOW WINDOW (lcCrtTitl) REFRESH SAME

SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfSequence
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Temp. Sequence No ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfSequence()
*!**************************************************************************
FUNCTION lfSequence
PRIVATE lnSequence,lnRecNo
lnSequence = 0
lnRecNo = RECNO()
SCAN
  lnSequence = MAX(lnSequence,VAL(cCartonTyp))
ENDSCAN
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
RETURN PADL(ALLTRIM(STR(lnSequence+1)),6,"0")

*!**************************************************************************
*! Name      : lfGetSizeQty
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : get all the carton qty from specific size ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfGetSizeQty()
*!**************************************************************************
FUNCTION lfGetSizeQty
PARA lcSize
lcSize = ALLTRIM(lcSize)
PRIVATE lnAlias , lnTotqty , lnRecNo
lnAlias = ALIAS()
lnTotQty = 0
SELECT (lcCrtnLine)
lnRecNo = RECNO()
IF SEEK(m.STYLE)
  SCAN REST WHILE Style+cCartonType = m.Style FOR lnRecNo <> RECNO() 
    lnTotQty = lnTotQty + (QTY&lcSize * CrtnNo)
  ENDSCAN
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
SELECT (lnAlias)
RETURN lnTotQty

*!**************************************************************************
*! Name      : lfCartNO
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid functon of the carton field ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCartNO()
*!**************************************************************************
FUNCTION lfCartNO
IF lnNOfCrtn < 1
  = gfModalGen('TRM42000B40011','DIALOG')
  lnNOfCrtn = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
PRIVATE lnRecNo , lnAlias
lnAlias = SELECT()
SELECT (lcCrtnLine)
lnRecNo = RECNO()
REPLACE ALL CrtnNo WITH lnNOfCrtn FOR cCartonTyp = lcCartnTyp
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
_CUROBJ = OBJNUM(lnWeight)
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfVCancel
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid functon of the Cancel Button ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfVCancel()
*!**************************************************************************
FUNCTION lfVCancel
PRIVATE lnAlias , lnTotqty , lnRecNo
lnAlias = ALIAS()
lnTotQty = 0
SELECT (lcCrtnLine)
lnRecNo = RECNO()
IF llNewCrt
  DELETE FOR cCartonTyp = lcCartnTyp
ELSE
  IF SEEK(m.STYLE)
    SCAN REST WHILE Style+cCartonType = m.Style+lcCartnTyp
      FOR lnCount = 1 To &lcScaleFle..Cnt
        lcCount = ALLTRIM(STR(lnCount))
        REPLACE Qty&lcCount WITH OldQty&lcCount
      ENDFOR
      REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    ENDSCAN
  ENDIF
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
SELECT (lnAlias)
Clear Read 

*!**************************************************************************
*! Name      : lfVldOk
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid functon of the OK  Button ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfVldOk()
*!**************************************************************************
FUNCTION lfVldOk
PRIVATE lnAlias , lnTotqty , lnRecNo
lnAlias = ALIAS()
lnTotQty = 0
SELECT (lcCrtnLine)
SCAN FOR cCartonTyp = lcCartnTyp AND TOTQty <> 0
  lnTotQty = lnTotQty + TOTQty
ENDSCAN
IF lnTotQty > 0 
  SELECT (lcCrtnHdr)
  IF llNewCrt
    APPEND BLANK
    REPLACE cCartonTyp WITH lcCartnTyp 
  ENDIF
  REPLACE weight     WITH lnWeight   ,;
          nlength    WITH lnLength   ,;
          ndepth     WITH lnDepth    ,;
          width      WITH ALLTRIM(STR(lnWidth)) ,;
          CrtnNo     WITH lnNofCrtn   ,;
          cStyMajor  WITH ALLTRIM(lcStyle) ,;
          TotQty     WITH lnTotQty 
ENDIF
SELECT (lnAlias)
Clear Read 

*!**************************************************************************
*! Name      : lfSAVCRTNS
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Save the carton detail from the temp file to the custom table
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfSAVCRTNS()
*!**************************************************************************
FUNCTION lfSAVCRTNS
PRIVATE lnAlias
lnAlias = Alias()
WAIT WINDOW '' NOWAIT
SELECT TempName
SCAN 
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

SELECT (lcCrtnHdr)
PRIVATE lnICount 

SCAN FOR ToTQty <> 0
   SCATTER MEMVAR MEMO
   lcStyMajor   = cStyMajor
   lcCartonTyp  = cCartonTyp
   m.cCartonTyp = lfGetSeqnce(lcCartonTyp,m.TotQty)
   *B607246,1 ABD - Update the shipment field in case recive by shipment. [Begin]
   *-- In Case Recived by shipment.
   IF lcPType = 'S'
     M.cShipNo = lcShpCode
   ENDIF
   *B607246,1 ABD - [End]
   FOR lnICount = 1 to CrtnNo
     m.cCartonNo  = gfSEQUENCE('cCartonNo')
     SELECT (lcCrtnLine)  
     SCAN FOR cCartonTyp = lcCartonTyp
       lcFields = "Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty"
       SCATTER FIELDS &lcFields  MEMVAR 
       m.cStyMajor  = PADR(m.Style,lnMjrWid)
       SELECT POCRTNMF
       APPEND BLANK
       GATHER MEMVAR MEMO
       REPLACE cCartonTyp WITH lcCarton
       SELECT (lcCrtnLine)
     ENDSCAN
   ENDFOR
ENDSCAN

*!**************************************************************************
*! Name      : lfGetSeqnce
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : get the Carton type ( from gfsequence function or from the Custom 
*! 			   table if the same carton qty is exist 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfGetSeqnce()
*!**************************************************************************
FUNCTION lfGetSeqnce
PARAM lcTyp,lnTotCrQty
PRIVATE lnAlias,lcOrder
lnAlias = SELECT()
SELECT cCartonTyp,ccartonno,cStyMajor,SUM(TotQty) AS TOTQTY FROM POCRTNMF WHERE cStyMajor =PADR(lcStyMajor,19);
       INTO CURSOR (lcPoCrTmp) GROUP BY cCartonTyp,ccartonno,cStyMajor 
SELECT POCRTNMF
lcOrder = ORDER()
SET ORDER TO TAG 'POCRTSTYLE'
SELECT (lcPoCrTmp)       
lcCarton = ''
SCAN FOR TOTQTY = lnTotCrQty
  lnTotal = 0
  SELECT (lcCrtnLine)
  llCrtExist = .T. 
  SCAN FOR cCartonTyp = lcTyp
    IF SEEK(STYLE+&lcPoCrTmp..cCartonTyp,'POCRTNMF') AND Qty1 = POCRTNMF.Qty1 AND Qty2 = POCRTNMF.Qty2;
       AND Qty3 = POCRTNMF.Qty3 AND Qty4 = POCRTNMF.Qty4 AND Qty5 = POCRTNMF.Qty5 AND ;
       Qty6 = POCRTNMF.Qty6 AND Qty7 = POCRTNMF.Qty7 AND Qty8 = POCRTNMF.Qty8

       lnTotal = lnTotal + TotQty
    ELSE
      llCrtExist = .F. 
    ENDIF
  ENDSCAN
  SELECT (lcPoCrTmp)       
  IF llCrtExist AND lnTotCrQty = lnTotal 
    lcCarton = &lcPoCrTmp..cCartonTyp  
    EXIT
  ENDIF
ENDSCAN
lcCarton = IIF(EMPTY(lcCarton),gfSEQUENCE('cCartonTyp'),lcCarton)
SELECT POCRTNMF
SET ORDER TO &lcOrder
SELECT (lnAlias)
RETURN lcCarton

*!**************************************************************************
*! Name      : lfCLOSFILS
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Close & erase the temp file 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCLOSFILS()
*!**************************************************************************
FUNCTION lfCLOSFILS
*C123616,1  TMI [Start] 
IF USED('TempName')
  *C123616,1  TMI [End  ] 
  SELECT TempName
  SCAN 
    RESTORE FROM Memo cTempName ADDITIVE
  ENDSCAN
  
  IF TYPE('lcCrtnHdr')='C' .AND. USED(lcCrtnHdr)
    USE IN (lcCrtnHdr)
    ERASE (gcWorkDir+lcCrtnHdr+'.DBF')
    ERASE (gcWorkDir+lcCrtnHdr+'.CDX')
  ENDIF
  IF TYPE('lcCrtnLine')='C' .AND. USED(lcCrtnLine)
    USE IN (lcCrtnLine)
    ERASE (gcWorkDir+lcCrtnLine+'.DBF')
    ERASE (gcWorkDir+lcCrtnLine+'.CDX')
  ENDIF

  *C123616,1  TMI [Start] Close also tempname cursor
  IF TYPE('lcShpCrtMF')='C' .AND. USED(lcShpCrtMF)
    USE IN (lcShpCrtMF)
    ERASE (gcWorkDir+lcShpCrtMF+'.DBF')
    ERASE (gcWorkDir+lcShpCrtMF+'.CDX')
  ENDIF
  
  USE IN TEMPNAME
ENDIF  
*C123616,1  TMI [End  ] 

*!**************************************************************************
*! Name      : lfCheckPO
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Check if the recieved contain more then one major or multiple Quality
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfCheckPO()
*!**************************************************************************
FUNCTION lfCheckPO
PRIVATE lnALIAS
lnAlias = SELECT()
SELECT (lcTmpline)
lnRecNo = RECNO()
llReturn = .T.
LOCATE FOR TRANCD $ '24'
IF !FOUND()
  RETURN .F.
ENDIF
lcStyMajor = PADR(Style,lnMjrWid)
*B122221,1 NNA 04/22/2004 (Begin) Hold the PO# Number to use it in the receiving and the Screen's title
lnPoNo     = PO
*B122221,1 NNA (End)

lcTranCD = TRANCD
llReturnPo = .F.
llOvrTwoSt = .F.
llMxdQulty = .F.
SCAN FOR TRANCD $ '24'

*B122221,1 NNA 04/22/2004 (Begin) Look for more one Style in the PO , Because if that the Receiving 
*B122221,1 NNA             Operation can't Continue because it dealing with one style in the PO
  *IF lcStyMajor <> PADR(Style,lnMjrWid)  
  IF lcStyMajor <> PADR(Style,lnMjrWid) .and. PO = lnPoNo   
*B122221,1 NNA (End)

    llOvrTwoSt = .T.
    EXIT
*B122221,1 NNA 04/22/2004 (Begin) if there is no more one style in the PO and there are more one PO
*B122221,1 NNA            I Hold the New style major and the PO#No To Research in the new PO
  ELSE
    lcStyMajor = PADR(Style,lnMjrWid)
    lnPoNo     = PO
*B122221,1 NNA (End)
  ENDIF
  IF lcTranCD <> TranCD
    llMxdQulty = .T.
    EXIT
   ENDIF
ENDSCAN
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF

IF llOvrTwoSt 
  IF gfModalGen("TRM00000B00023","DIALOG",.F.,.F.,'The purchase order has different styles. Cannot receive by carton.') = 2
    llModRecpt = .T.
  ENDIF
  llReturn = .F.
ENDIF
IF llMxdQulty 
  IF gfModalGen("TRM00000B00023","DIALOG",.F.,.F.,'Styles have different qualities. Cannot receive by carton.') = 2
    llModRecpt = .T.
  ENDIF
  llReturn = .F.
ENDIF
SELECT (lnALias)
RETURN llReturn

*!**************************************************************************
*! Name      : lfZAPTEMP
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Delete the records from the temp file 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfZAPTEMP()
*!**************************************************************************
FUNCTION lfZAPTEMP
Private lnAlias,lcCartnTyp
lcCartnTyp = ''
lnAlias = ALIAS()
IF USED('TempName')
  SELECT TempName
  SCAN 
    RESTORE FROM Memo cTempName ADDITIVE
  ENDSCAN
  IF TYPE('lcCrtnHdr')='C' .AND. USED(lcCrtnHdr)
    SELECT (lcCrtnHdr)
    DELETE ALL
  ENDIF
  IF TYPE('lcCrtnLine')='C' .AND. USED(lcCrtnLine)
    SELECT (lcCrtnLine)
    SET FILTER TO
    DELETE ALL
  ENDIF
  *C123616,1  TMI [Start] Close tempname file
  USE IN TEMPNAME
  *C123616,1  TMI [End  ] 
ENDIF
SELECT (lnAlias)


*!**************************************************************************
*! Name      : lfADDBAR
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/18/2003
*! Purpose   : add custom bar to the option menu
*! Reference : C200490,1
*!**************************************************************************
*! Example   : lfADDBAR()
*!**************************************************************************
FUNCTION lfADDBAR

*--Add 'Auto Search' bar to Option menu.
*C123616,1  TMI [Start] Let bar # be variable
*DEFINE BAR 10 OF _lPopOpt PROMPT "\<View Carton Details" SKIP FOR !(laScrMode[2] AND lnActFolder= 2)
PRIVATE lnBarNo
lnBarNo = CNTBAR('_lPopOpt') + 1
DEFINE BAR lnBarNo OF _lPopOpt PROMPT "\-" SKIP FOR .T.

lnBarNo = CNTBAR('_lPopOpt') + 1
DEFINE BAR lnBarNo OF _lPopOpt PROMPT "\<View Carton Details" SKIP FOR !(laScrMode[2] AND lnActFolder= 2)
ON SELECTION BAR lnBarNo OF _lPopOpt DO lfRunStyNt IN MEMMAIN.PRG
*C123616,1  TMI [End  ] 


*!**************************************************************************
*! Name      : lfRunStyNt
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/18/2003
*! Purpose   : run function to browse the  cartons
*! Reference : C200490,1
*!**************************************************************************
*! Example   : lfRunStyNt()
*!**************************************************************************
FUNCTION lfRunStyNt
PRIVATE lcStyAlias

lcStyAlias = ALIAS()
  
*-- Clear the trapped keys.
PUSH KEY
ON KEY

*C123616,1  TMI [Start] comment this line
*IF BAR() = 10
*C123616,1  TMI [End  ] 

  PUSH MENU _MSYSMENU
  RELEASE PAD _OPTIONPOP OF _MSYSMENU
  =lfCartons()
  POP MENU _MSYSMENU
  
*C123616,1  TMI [Start] comment this line
*ENDIF  
*C123616,1  TMI [End  ] 
  
*-- Restore the trapped keys.
POP KEY
SELECT (lcStyAlias)
*-- End of lfRunStyNt.

*!**************************************************************************
*! Name      : lfCartons
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/18/2003
*! Purpose   : browse the  cartons details
*! Reference : C200490,1
*!**************************************************************************
*! Example   : lfCartons()
*!**************************************************************************
FUNCTION lfCartons
PARAMETER lcStyMajor
PRIVATE lcBrFields,lnAlias,lctmpCrtn,lcNMjrTl,lcMjrPct,lctmpFile,;
        lcNMjrPt,lnstylewid,lncolorwid,llAllClrs

*--lcBrFields --> Hold Browse Fields
*--lnAlias    --> Hold Current Area
*--lctmpCrtn  --> Hold Carton temp file name
*--lcStyleTmp --> hold Style alias temp name
*--lcScaleTmp --> hold Scale alias temp name
*--lcNMjrTl   --> hold nonmajor title
*--lcMjrPct   --> Hold major picture
*--lcNMjrPt   --> hold non major picture
*--lnstylewid --> hold major length
*--lncolorwid --> hold color length
*--llAllClrs  --> flag if all colors

lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcNMjrTl  = gfItemMask('HN')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lctmpCrtn = gftempname()
lctmpFile = gftempname()
lcPoHdrTmp = gftempname()
lcPolinTmp = gftempname()
lcShpHdTmp = gftempname()
lcCutHdTmp = gftempname()
lcCutlnTmp = gftempname()
lcOrdHdTmp = gftempname()
lcOrdLnTmp = gftempname()
lcStyleTmp = gftempname()
lcStyDyTmp = gftempname()
lcScaleTmp = gftempname()

lnAlias=SELECT()
=gfOpenFile(gcDataDir+"PoCrtnMF", "PoCrtSTYLE", "SH")
=gfOpenFile(gcDataDir+"SCALE", "SCALE", "SH",@lcScaleTmp,.T.)
=gfOpenFile(gcDataDir+"STYLE", "STYLE", "SH",@lcStyleTmp,.T.)
*--Checking existance of OTS information.
lcStyMajor = laData[1]
=SEEK(lcStyMajor) 

WAIT WINDOW 'Collecting Open to Sell information...' NOWAIT

*B607246,1 ABD - Add piktkt field to the temp file to browse this field. [Begin]
*CREATE CURSOR (lcTmpFile) (cDesc C(19),cCartonTyp C(6) ,Size C(5) ,;
                          qty1 N(7),qty2 N(7),qty3 N(7),qty4 N(7),;
                          qty5 N(7),qty6 N(7),qty7 N(7),qty8 N(7),;
                          TotQty N(8),nCrtnNo N(7),cShipNO c(6))
CREATE CURSOR (lcTmpFile) (cDesc C(19),cCartonTyp C(6) ,Size C(5) ,;
                          qty1 N(7),qty2 N(7),qty3 N(7),qty4 N(7),;
                          qty5 N(7),qty6 N(7),qty7 N(7),qty8 N(7),;
                          TotQty N(8),nCrtnNo N(7),cShipNO c(6),Piktkt c(6))
*B607246,1 ABD - [End]
IF !SEEK(ALLTRIM(lcStyMajor),'PoCrtnMF')
   WAIT WINDOW 'NO RECORDS'
   RETURN
ENDIF

SELECT POCRTNMF
*B607246,1 ABD - Add This ShipNo to the select statment, and remove the check for invoice field.   [Begin]
*-- Get the Shipment No.
*SELECT cCartonTyp,Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Count('CCartonNo') AS nTotCrtNo ;
*       FROM pocrtnmf INTO CURSOR (lctmpCrtn) WHERE cStyMAjor = lcStyMajor AND EMPTY(piktkt) AND EMPTY(invoice);
*       GROUP BY cCartonTyp,Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty

SELECT cCartonTyp,Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,Count('CCartonNo') AS nTotCrtNo,cShipNo,Piktkt ;
       FROM pocrtnmf INTO CURSOR (lctmpCrtn) WHERE cStyMAjor = lcStyMajor AND EMPTY(invoice);
       GROUP BY cCartonTyp,Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
*B607246,1 ABD - [End]

SELECT  (lctmpCrtn)
LOCATE
lcCrtnTyp = cCartonTyp
lnCount   = 0
lnRecords = 0 
STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty

SCAN 
  SCATTER MEMVAR MEMO
  
  *B607246,1 ABD - Get the First Shipment No. [Begin]
  M.cShipNo = IIF(SEEK(M.style+M.ccartontyp,'POCRTNMF'),POCRTNMF.cShipNo,'')
  *B607246,1 ABD - [End]
  
  IF lcCrtnTyp <> cCartonTyp
    =lfAddTotLn()
    lcCrtnTyp = cCartonTyp
    STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty,lnRecords,lnCount
  ENDIF
  FOR lnSize = 1 TO 8
    lcSize = ALLTRIM(STR(lnSize))
    lnQty&lcSize = lnQty&lcSize + qty&lcSize
  ENDFOR
  lnTotQty = lnTotQty + TotQty
  lnCount  = nTotCrtNo
  lnRecords= lnRecords + 1
  SELECT (lcTmpFile)
  APPEND BLANK
  m.cDesc =  SUBSTR(m.Style,lnStyleWid+2,lnColorWid)
  IF lnRecords > 1
    m.cCartonTyp = ''
  ENDIF
  GATHER MEMVAR MEMO
ENDSCAN

STORE '' TO lcMn1,lcMn2,lcMn3,lcMn4,lcMn5,lcMn6,lcMn7,lcMn8
IF SEEK(lcStyMajor,lcStyleTmp) AND SEEK('S'+&lcStyleTmp..SCALE,lcScaleTmp)
  FOR lnSCount = 1 TO &lcScaleTmp..Cnt
    lcSize = ALLTRIM(STR(lnSCount))
    lcMn&lcSize = &lcScaleTmp..Sz&lcSize
  ENDFOR
ENDIF
SELECT (lcTmpFile)
GO BOTTOM
IF !EMPTY(cDESC)
   =lfAddTotLn()
ENDIF
SELECT (lcTmpFile)
LOCATE
lcBrFields = "cCartonTyp :H = 'Carton Type',cDesc     :H=lcNMjrTl,"
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>0,"Qty1   :H=lcMn1  :P='999999',","")
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>1,"Qty2   :H=lcMn2  :P='999999',","")
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>2,"Qty3   :H=lcMn3  :P='999999',","")
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>3,"Qty4   :H=lcMn4  :P='999999',","")
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>4,"Qty5   :H=lcMn5  :P='999999',","")
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>5,"Qty6   :H=lcMn6  :P='999999',","")
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>6,"Qty7   :H=lcMn7  :P='999999',","")
lcBrFields = lcBrFields + IIF(&lcScaleTmp..Cnt>7,"Qty8   :H=lcMn8  :P='999999',","")
*B607246,1 ABD - Add Piktkt field to the browse. [Begin]
*lcBrFields = lcBrFields + "TotQty :H='Total':P='999999',"+;
        	 "nCrtnNo :H='No of Catns' :P='999999',"+;
        	 "cShipNO :H='Ship NO' "
lcBrFields = lcBrFields + "TotQty :H='Total':P='999999',"+;
        	 "nCrtnNo :H='No of Catns' :P='999999',"+;
        	 "cShipNO :H='Ship NO',Piktkt :H= 'Piktkt' "
*B607246,1 ABD - [End]        	 
WAIT CLEAR
=ARIABROW('','Carton Details',gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
         	'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
SELECT(lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfAddTotLn
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/18/2003
*! Purpose   : add Total Record to the temp. file
*! Reference : C200490,1
*!**************************************************************************
*! Example   : lfAddTotLn()
*!**************************************************************************
FUNCTION lfAddTotLn
PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcTmpFile)
APPEND BLANK
REPLACE cCartonTyp WITH ''        ,;
        cDesc      WITH ''        ,;
        Qty1       WITH lnQty1    ,;
        Qty2       WITH lnQty2    ,;
        Qty3       WITH lnQty3    ,;
        Qty4       WITH lnQty4    ,;
        Qty5       WITH lnQty5    ,;
        Qty6       WITH lnQty6    ,;
        Qty7       WITH lnQty7    ,;
        Qty8       WITH lnQty8    ,;
        TotQty     WITH lnTotQty  ,;
	    nCrtnNO    WITH lnCount
SELECT (lnAlias)
RETURN


*wab
*SELECT cCartonTyp,ccartonno,cStyMajor,SUM(TotQty) AS TOTQTY FROM POCRTNMF WHERE cStyMajor =PADR(lcStyMajor,19);
*       INTO CURSOR (lcPoCrTmp) GROUP BY cCartonTyp,ccartonno,cStyMajor 

SELECT DISTINCT cCartonTyp,cCartonNo,cStyMajor from pocrtnmf into cursor 'AAAA';
       group by cCartonTyp,cCartonNo,cStymajor

*************

*wab NEW SCREEN
*!**************************************************************************
*! Name      : lfALOpnFls
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Call The Carton Type Screen
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfALOpnFls()
*!**************************************************************************
FUNCTION lfALOpnFls 
PRIVATE lnAlias
lnAlias = SELECT()
lcOrdTemp = gftempname() 
lcCrtnDet = gftempname() 
lcCrtnTyp = gftempname() 
lcOldPckd = gftempname() 
CREATE Cursor  'tempname' (ctempname M(10))
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcOrdTemp 
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcCrtnDet 
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcCrtnTyp 
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcOldPckd

=gfOpenFile(gcDataDir+'POCRTNMF','POCRTNMF','SH')

DIMENSION laFileStru[1,4]
*SELECT POCRTNMF
*=AFIELDS(laFileStru)
*CREATE TABLE (gcWorkDir+lcCrtnTyp) FROM ARRAY laFileStru

SELECT OrdLine
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcOrdTemp) FROM ARRAY laFileStru
INDEX ON Style TAG lcOrdTemp OF (gcWorkDir+lcOrdTemp)

*B126708,1  TMI [Start] Add CWARECODE field to the temp table
*CREATE TABLE (gcWorkDir+lcCrtnDet) (ORDER C(6),cCartonTyp C(6) ,TotQty N(7) ,nCrtNo N(7) ,nAlQty N(7) , ;
                                    nPikCrtn N(7), PickTkt c(6), OldPickTkt c(6) , noldQty N(7))
CREATE TABLE (gcWorkDir+lcCrtnDet) (ORDER C(6),cCartonTyp C(6) ,TotQty N(7) ,nCrtNo N(7) ,nAlQty N(7) , ;
                                    nPikCrtn N(7), PickTkt c(6), OldPickTkt c(6) , noldQty N(7),;
                                    CWARECODE C(6))
*B126708,1  TMI [End  ] 
INDEX ON cCartonTyp TAG lcCrtnDet OF (gcWorkDir+lcCrtnDet)

*CREATE TABLE (gcWorkDir+lcOldPckd) (cCartonTyp C(6) ,nAlQty N(7) , PickTkt c(6))
*INDEX ON cCartonTyp TAG lcOldPckd OF (gcWorkDir+lcOldPckd)

*C123616,1  TMI [Start] Create a temp file that relates pocrtnmf and lc_tmpOrdL
lcTmpRel = gfTempName()
CREATE CURSOR &lcTmpRel (RECNO1 N(5),RECNO2 N(5))
INDEX ON RECNO1 TAG &lcTmpRel
SELECT TEMPNAME
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcTmpRel
*C123616,1  TMI [End  ] 
SELECT (lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfAlCarton 
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Call The Carton Type Screen
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfAlCarton()
*!**************************************************************************
FUNCTION lfAlCarton
PRIVATE lcWinALCR5,lcWinALCR6,lcWinALCR7,lcWinALCR8,lcChck,lcUnChck,lcAlTitl,;
        lcOrder,lcAccount,lnTotCrton,lnTotPickd

PRIVATE lcFileToUse,llBrowse ,lnAlias
llCSave  = .T.      

IF !llGenPkTk .OR. !lfCheckpik()
  RETURN .T.
ENDIF
IF gfModalGen("TRM00000B42002","DIALOG",.F.,.F.,'Update Carton Details?.') = 2
  RETURN .T.
ENDIF
SELECT TempName
SCAN 
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN
IF !lfCheckAlo()
  IF gfModalGen("TRM00000B00023","DIALOG",.F.,.F.,'All sales order lines must be picked in one ticket. Cannot pick to cartons.') = 2
    llCSave  = .F.      
    RETURN llCSave
  ENDIF
  RETURN llCSave
ENDIF

llBrowse = .F.
lnAlias = SELECT()
lnRecNo = RECNO()
lcFileToUse=lcCrtnDet
lnBrRecNo = RECNO(lcFileToUse)
STORE SPACE(0) TO lcWinALCR5,lcWinALCR6,lcWinALCR7,lcWinALCR8
lnTotCrton = 0
lnTotPickd = 0
lcOrder    = ''
lcAccount  = ''

STORE 0 TO lnOldVal

lcAlTitl = 'Alocation By Cartons'

lcALCRT  = gfTempName()
lcALCRT1 = gfTempName()
lcALCRT2 = gfTempName()
lcALCRT3 = gfTempName()

PRIVATE lcALCRTitl,lcALBrFild

lcALBrFild = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "cCartonTyp :R :H='Carton Type':14,"+;
             "nCrtNo :R :H='Cartons Available' :17,"+;
             "nAlQty    :H='Cartons Allocated' :17,"+;
             "nPieces = TotQty * nAlQty  :R :H='Total Pieces' :12"

lcALCRTitl = 'Cartons'
=lfClctCrtn()
lnTotCrtqt = lfChkCarton()
IF lnTotCrtqt > 0 
  *-- Call the screen
  PUSH KEY
  lcSysMen = SET('SYSMENU')
  SET SYSMENU ON
  ON KEY LABEL ALT+L ACTIVATE WINDOW (lcAlCrTitl)

  RELEASE PAD _BROWSE OF _MSYSMENU
  DO (gcScrDir+'AL\ALCRTYP.SPX')
  SET SYSMENU &lcSysMen
  POP KEY
ELSE
  IF gfModalGen("TRM00000B00023","DIALOG",.F.,.F.,'There are no cartons available for this style.') = 2 
    llCSave = .F.
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN llCSave

*!**************************************************************************
*! Name      : lfActBrwAL
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Activate  the Carton Details Screen
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfActBrwAL()
*!**************************************************************************
FUNCTION lfActBrwAL

SELECT(lcFileToUse)
LOCATE
lnNewRec = RECNO(lcFileToUse)

BROWSE FIELDS &lcALBrFild ;
       LOCK 0             ;
       SAVE               ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwAL();
       VALID :F lfvBrwAL();
       TITLE lcALCRTitl   ;
       WINDOW lcWinAlCr6 IN WINDOW lcWinAlCr5
=lfwBrwAL()

*!**************************************************************************
*! Name      : lfwBrwAL
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : When Function of the Browse window ( Carton Types Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfwBrwAL()
*!**************************************************************************
FUNCTION lfwBrwAL
SELECT (lcFileToUse)
lnBrRecNo = RECNO()
SUM (TotQty * nAlQty) to lnTotCrton
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF
SHOW WINDOW (lcALCRTitl) REFRESH SAME
=lfLOtRoRef()

*!**************************************************************************
*! Name      : lfvBrwAL
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Valid Function of the Browse window ( Carton Types Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfvBrwAL()
*!**************************************************************************
FUNCTION lfvBrwAL

IF WONTOP() # (lcALCRTitl)
  =gfStopBrow()
ENDIF

SELECT (lcFileToUse)
lnBrRecNo = RECNO()
SUM (TotQty * nAlQty) to lnTotCrton
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF
SHOW WINDOW (lcALCRTitl) REFRESH SAME
=lfLOtRoRef()

*!**************************************************************************
*! Name      : lfAlTrap
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap  Function of the Browse window ( Carton Types Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfAlTrap()
*!**************************************************************************
FUNCTION lfALTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcAlCrTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrAlTab
  ON KEY LABEL BACKTAB DO lfBrAlBack
  ON KEY LABEL ESCAPE  DO lfALEscape
ENDIF

*!**************************************************************************
*! Name      : lfAlClrTrp
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Clear Trap Function of the Browse window ( Carton Types Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfAlClrTrp()
*!**************************************************************************
FUNCTION lfAlClrTrp

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESCAPE  DO lfAlEscape

*!**************************************************************************
*! Name      : lfBrAlTab
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the TAB KEY ( Carton Types Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfBrAlTab()
*!**************************************************************************
FUNCTION lfBrAlTab
ON KEY LABE TAB
ACTIVATE WINDOW (lcALCRT3)
_CUROBJ = OBJNUM(pbAlOk)

*!**************************************************************************
*! Name      : lfBrAlBack
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the SHIEFT TAB KEY ( Carton Types Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfBrAlBack()
*!**************************************************************************
FUNCTION lfBrAlBack

ON KEY LABE BACKTAB	
ACTIVATE WINDOW (lcALCRT3)
_CUROBJ = OBJNUM(pbAlCancel)

*!**************************************************************************
*! Name      : lfAlEscape
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Trap the ESC KEY ( Carton Types Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfAlEscape()
*!**************************************************************************
FUNCTION lfAlEscape

ON KEY LABE ESCAPE
= lfAlCancel()

*!**************************************************************************
*! Name      : lfAlCrtBrw
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Display the Browse Screen ( Carton Stypes Screen )
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfAlCrtBrw()
*!**************************************************************************
FUNCTION lfAlCrtBrw
SELECT(lcFileToUse)
GO TOP
lnNewRec = RECNO(lcFileToUse)
BROWSE FIELDS &lcALBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwAl();
       VALID :F lfvBrwAl();
       TITLE lcAlCrTitl   ;
       WINDOW (lcALCRT2) IN WINDOW (lcALCRT)
= lfwBrwAL()



*!**************************************************************************
*! Name      : lfClctCrtn
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : COLLECT THE CARTON TYPES 
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfClctCrtn()
*!**************************************************************************
FUNCTION lfClctCrtn
PARA  lcStatus
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcOrdTemp)
SELECT ORDLINE
lnRecNo = RECNO()
lnStyCnt = 0
lcStyMajor = ''
lcOrder    = laData[1]
lcAccount  = laData[2]
lnTotPickd = 0 
IF SEEK(Style,'Style')
  lcStyMajor = STYLE.cStyMajor
ENDIF
lcPickTkt  = piktkt

SCAN FOR cordtype+order+STR(lineno,6)= 'O'+laDATA[1]
  SCATTER MEMVAR MEMO
  SELECT (lcOrdTemp)
  IF !SEEK(m.Style)
    APPEND BLANK
  ENDIF
  GATHER MEMVAR MEMO
  SELECT ORdLine
  IF !EOF(lc_tmpordl)
    SELECT (lc_tmpordl)
  ENDIF
  lnTotPickd = lnTotPickd + totpik
  lnStyCnt= lnStyCnt + 1
ENDSCAN
SELECT ORDLINE
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
SELECT DISTINCT cCartonTyp,ccartonno,cStyMajor , ' ' AS lcnQty FROM POCRTNMF WHERE cStyMajor =PADR(lcStyMajor,19);
       INTO CURSOR (lcCrtnTyp) GROUP BY cCartonTyp,ccartonno,cStyMajor

SELECT (lcCrtnTyp)       
SCAN 
  lnCrtcnt= 0 
  lnTotQty = 0 
  llCrtExist = .T.
  IF SEEK(cCartonTyp+ccartonno+cStyMajor,'POCRTNMF') 
    SELECT POCRTNMF
    SCAN REST WHILE ccartontyp+ccartonno+cstymajor+style = &lcCrtnTyp..cCartonTyp+&lcCrtnTyp..ccartonno+&lcCrtnTyp..cStyMajor ;
              FOR   EMPTY(piktkt)   AND EMPTY(INVOICE)
      IF SEEK(STYLE,lcOrdTemp)
        lnCrtcnt= lnCrtcnt + 1
        lnTotQty = lnTotQty + TotQty
      ELSE
        llCrtExist = .F.
      ENDIF
    ENDSCAN
  ENDIF
  SELECT (lcCrtnTyp)
  IF lnStyCnt=lnCrtcnt AND llCrtExist
    SCATTER MEMVAR MEMO
    m.TotQty = lnTotQty
    m.nCrtNo = 1
    m.OldPickTkt  = IIF(lcPickTkt<>"******",lcPickTkt,"")
    SELECT (lcCrtnDet)
    IF !SEEK(&lcCrtnTyp..cCartonTyp)
      APPEND BLANK
      GATHER MEMVAR MEMO
      IF SEEK(cCartonTyp,lcOldPckd)
        REPLACE nAlQty   WITH nAlQty + &lcOldPckd..nAlQty
      ENDIF
    ELSE
      REPLACE nCrtNo WITH  nCrtNo + 1
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcCrtnDet)
SCAN
  REPLACE nOldQty WITH nAlQty
ENDSCAN

SELECT (lnAlias)
RETURN 


*!**************************************************************************
*! Name      : lfALCrtSav
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : SAVE THE CAtRON aLOCATION
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfALCrtSav()
*!**************************************************************************
FUNCTION lfALCrtSav
SELECT TempName
SCAN 
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

*--Release the old picktkt if any ..
SELECT (lcOldPckd)
SCAN
  IF SEEK(ccartontyp,'POCRTNMF')
     SELECT POCRTNMF
     SCAN REST WHILE ccartontyp+ccartonno+cstymajor+style = &lcOldPckd..cCartonTyp ;
                 FOR  EMPTY(INVOICE)
       IF piktkt = &lcOldPckd..PickTkt 
         REPLACE piktkt WITH SPACE(0)
       ENDIF
     ENDSCAN
  ENDIF
ENDSCAN

*C123616,1  TMI [Start] get order styles into array
PRIVATE laStyls,lnLen
DIMENSION laStyls[1]
lnLen = 0
SELECT ORDLINE
=SEEK('O'+laData[1],'ORDLINE')
SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+laData[1]
  lnLen = lnLen + 1
  DIMENSION laStyls[lnLen]
  laStyls[lnLen] = ORDLINE.STYLE
ENDSCAN
*C123616,1  TMI [End  ] 

*- Update pocrtnmf.PikTkt field from ordline.piktkt
=SEEK('O'+laData[1],'ORDLINE')
lcPickTkt = ORDLINE.piktkt
SELECT (lcCrtnDet)
SCAN FOR nAlQty <> 0
  lnAlQty = nAlQty
  FOR lnCount = 1 TO lnAlQty
    IF SEEK(ccartontyp,'POCRTNMF')
      SELECT POCRTNMF
      LOCATE REST WHILE ccartontyp+ccartonno+cstymajor+style = &lcCrtnDet..cCartonTyp ;
                 FOR  EMPTY(piktkt)   AND EMPTY(INVOICE)
      IF FOUND()
        lcCartontyp = ccartontyp
        lcCartonno  = ccartonno
        *C123616,1  TMI [Start] check that the style exists in the order
        *SCAN REST WHILE ccartontyp+ccartonno+cstymajor+style = lcCartontyp+lcCartonno
        SCAN REST WHILE ccartontyp+ccartonno+cstymajor+style = lcCartontyp+lcCartonno ;
                    FOR ASCAN(laStyls,POCRTNMF.STYLE)>0
          REPLACE piktkt WITH  lcPickTkt
        ENDSCAN
      ENDIF
      SELECT (lcCrtnDet)
    ENDIF 
  ENDFOR
ENDSCAN


*!**************************************************************************
*! Name      : lfOldPickD
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : collect the old allocation
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfOldPickD()
*!**************************************************************************
FUNCTION lfOldPickD
SELECT TempName
SCAN 
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcCrtnDet)
DELETE ALL

SELECT ORDLINE
lnRecNo = RECNO()
lnStyCnt = 0
lcStyMajor = ''
lcOrder    = laData[1]
lcAccount  = laData[2]
lnTotPickd = 0 
IF SEEK(Style,'Style')
  lcStyMajor = STYLE.cStyMajor
ENDIF
lcPicktkt  =  piktkt
SELECT ORDLINE
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
*C123616,1  TMI [Start] Allow multiple styles in one carton
*SELECT cCartonTyp , Count(distinct cCartonNO) AS nAlQty , lcPicktkt AS PickTkt  ;
*       FROM POCRTNMF  WHERE !EMPTY(piktkt) AND cStyMajor =PADR(lcStyMajor,19) ;
*       AND piktkt = lcPicktkt INTO CURSOR (lcOldPckd) GROUP BY cCartonTyp
SELECT cCartonTyp , Count(Distinct cCartonNO) AS nAlQty , lcPicktkt AS PickTkt  ;
   FROM POCRTNMF ;
   WHERE !EMPTY(piktkt) AND piktkt = lcPicktkt ;
   GROUP BY cCartonTyp ;
   INTO CURSOR (lcOldPckd) 
*C123616,1  TMI [End  ] 
INDEX ON cCartonTyp TAG lcOldPckd OF (gcWorkDir+lcOldPckd)
SELECT (lnAlias) 

*!**************************************************************************
*! Name      : lfCheckAlo
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : check alocation
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfCheckAlo()
*!**************************************************************************
FUNCTION lfCheckAlo
PRIVATE lnAlias
lnAlias = SELECT()
SELECT ORDLINE
lnRecNo = RECNO()
lcPickTkt = piktkt
llOnePktkt = .T. 

SCAN FOR cordtype+order+STR(lineno,6)= 'O'+laDATA[1]
  IF EOF(lc_tmpordl) .OR. !&lc_tmpordl..picked
    llOnePktkt = .F. 
  ENDIF
ENDSCAN
SELECT ORDLINE
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
SELECT (lnAlias)
RETURN llOnePktkt

*!**************************************************************************
*! Name      : lfCheckpik
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : check picked qty
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfCheckpik()
*!**************************************************************************
FUNCTION lfCheckpik
PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lc_tmpordl)
llPktkt = .F.
SCAN REST FOR cordtype+order+STR(lineno,6)= 'O'+laDATA[1]
  IF Picked 
    llPktkt = .T.
  ENDIF
ENDSCAN
SELECT ORDLINE
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
SELECT (lnAlias)
RETURN llPktkt

*!**************************************************************************
*! Name      : lfAlCancel
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : validate of cancel button
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfAlCancel()
*!**************************************************************************
FUNCTION lfAlCancel
SELECT (lcCrtnDet)
lnOldQty = 0
SCAN
  REPLACE nAlQty WITH nOldQty
  lnOldQty = lnOldQty + nAlQty
ENDSCAN
IF lnOldQty <> 0  AND (lnOldQty <> lnTotCrton  .OR. !lfChkAlSiz())

  lnAnswer=gfModalGen("TRM00000B44017","DIALOG",.F.,.F.,'Picked quantity is different from the total cartoned  quantity.') 
  DO CASE 
    CASE lnAnswer = 1
      =lfUpPkdQty()
    CASE lnAnswer = 2
      RETURN
    CASE lnAnswer = 3
      llCSave = .F.
  ENDCASE
ENDIF
CLEAR READ

*!**************************************************************************
*! Name      : lfAlCrtOk
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : validate of Ok button
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfAlCrtOk()
*!**************************************************************************
FUNCTION lfAlCrtOk

IF lnTotPickd <> lnTotCrton  .OR. !lfChkAlSiz()
  lnAnswer = gfModalGen("TRM00000B44017","DIALOG",.F.,.F.,'Picked quantity is different from the total cartoned  quantity.')
  DO CASE 
    CASE lnAnswer = 1
      =lfUpPkdQty()
    CASE lnAnswer = 2
      RETURN
    CASE lnAnswer = 3
      SELECT (lcCrtnDet)
      SCAN
        REPLACE nAlQty WITH nOldQty
      ENDSCAN
      llCSave = .F.
  ENDCASE
ENDIF
CLEAR READ

*!**************************************************************************
*! Name      : lfUpPkdQty
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : update the original qty
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfUpPkdQty()
*!**************************************************************************
FUNCTION lfUpPkdQty
SELECT (lcOrdTemp)
REPLACE ALL PIK1   WITH 0 ,;
            PIK2   WITH 0 ,;
            PIK3   WITH 0 ,;
            PIK4   WITH 0 ,;
            PIK5   WITH 0 ,;
            PIK6   WITH 0 ,;
            PIK7   WITH 0 ,;
            PIK8   WITH 0 ,;
            TOTPIK WITH 0 

SELECT (lcCrtnDet)
SCAN FOR nAlQty <> 0
  lnAlQty = nAlQty
  IF SEEK(cCartonTyp,'POCRTNMF')
    SELECT POCRTNMF
    lcCartontyp = ccartontyp
    lcCartonno  = ccartonno
    SCAN REST WHILE ccartontyp+ccartonno+cstymajor+style = lcCartontyp+lcCartonno
      IF SEEK(STYLE,lcOrdTemp)      
        REPLACE &lcOrdTemp..PIK1   WITH &lcOrdTemp..PIK1 + (Qty1 * lnAlQty) ,;
                &lcOrdTemp..PIK2   WITH &lcOrdTemp..PIK2 + (Qty2 * lnAlQty)  ,;
                &lcOrdTemp..PIK3   WITH &lcOrdTemp..PIK3 + (Qty3 * lnAlQty)  ,;
                &lcOrdTemp..PIK4   WITH &lcOrdTemp..PIK4 + (Qty4 * lnAlQty)  ,;
                &lcOrdTemp..PIK5   WITH &lcOrdTemp..PIK5 + (Qty5 * lnAlQty)  ,;
                &lcOrdTemp..PIK6   WITH &lcOrdTemp..PIK6 + (Qty6 * lnAlQty)  ,;
                &lcOrdTemp..PIK7   WITH &lcOrdTemp..PIK7 + (Qty7 * lnAlQty)  ,;
                &lcOrdTemp..PIK8   WITH &lcOrdTemp..PIK8 + (Qty8 * lnAlQty)  ,;
                &lcOrdTemp..TotPIK WITH &lcOrdTemp..TotPIK + (TotQty * lnAlQty) 
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lc_tmpordl)
SCAN
  IF SEEK(Style,lcOrdTemp) AND (PIK1 <> &lcOrdTemp..Pik1 .OR. PIK2 <> &lcOrdTemp..Pik2 .OR. ;
                                PIK3 <> &lcOrdTemp..Pik3 .OR. PIK4 <> &lcOrdTemp..Pik4 .OR. ;
                                PIK5 <> &lcOrdTemp..Pik5 .OR. PIK6 <> &lcOrdTemp..Pik6 .OR. ;
                                PIK7 <> &lcOrdTemp..Pik7 .OR. PIK7 <> &lcOrdTemp..Pik7) 
    SCATTER MEMVAR MEMO
    FOR lnCount = 1 to 8
      lcStr = STR(lnCount,1)
      lcOldVal = PIK&lcStr
      m.PIK&lcStr = 0 
      =lfChngPkQty(lcStr)
      lcOldVal = PIK&lcStr
      m.PIK&lcStr = &lcOrdTemp..PIK&lcStr
      =lfChngPkQty(lcStr)
    ENDFOR
  ENDIF
ENDSCAN
RETURN

*!**************************************************************************
*! Name      : lfChngPkQty
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Change the original qty
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfChngPkQty()
*!**************************************************************************
FUNCTION lfChngPkQty
PARAM lcSize
lcSize = ALLTRIM(lcSize)
lcObjName  = 'm.PIK'+lcSize
lnObjVal   = EVALUATE(lcObjName)


*-- If the value is changed,
IF lnObjVal <> lcOldVal
  lnStk      = MAX(EVAL(lcStkAlias+'.Stk'+lcSize+'-'+ lcStkAlias+'.Alo'+lcSize), 0)
  *-- Compare allocated quantity with ordered quantity
  IF lnObjVal <> m.Qty&lcSize 
    m.cStatus = 'M'
    *-- Check if there is available stock
    IF (lnObjVal-lcOldVal <= lnStk .AND. ;
       (!llChkAprov .OR. lfAprvLine(lnObjVal - lcOldVal)))
      =lfCrtForcAl(lnObjVal - lnStk))
      lnCurAlias = SELECT(0)
      IF m.cStatus = 'M' .AND. lnObjVal > m.Qty&lcSize 
         m.TotQty        = m.TotQty - m.Qty&lcSize + lnObjVal
         m.TotBook       = m.TotBook - m.Qty&lcSize + lnObjVal
         m.Qty&lcSize  = lnObjVal
         m.Book&lcSize = lnObjVal
   	  ENDIF
      m.TotPik   = m.Pik1+m.pik2+m.Pik3+m.pik4+m.Pik5+m.pik6+m.Pik7+m.pik8
      SELECT (lc_TmpOrdL)
      GATHER MEMVAR MEMO
        
      IF !SEEK(m.Style + m.cWareCode + SPACE(10), lc_TmpStyD) .AND.;
        SEEK(m.Style + m.cWareCode + SPACE(10), 'STYDYE')
 		SELECT STYDYE
        SCATTER TO laStyDye MEMVAR
        INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye 
      ENDIF
      SELECT (lc_TmpStyD)
      REPLACE Alo&lcSize WITH Alo&lcSize - lcOldVal + lnObjVal,;
              TotAlo       WITH TotAlo       - lcOldVal + lnObjVal
      *-- Update temp stydye
      IF llUseDyes .AND. STYLE.cDye_flg = 'Y' 
        IF !SEEK(m.Style + m.cWareCode + m.Dyelot, lc_TmpStyD) .AND.;
          SEEK(m.Style + m.cWareCode + m.Dyelot, 'STYDYE')
          SELECT STYDYE
          SCATTER TO laStyDye MEMVAR
          INSERT INTO (lc_TmpStyD) FROM ARRAY laStyDye 
		ENDIF
		SELECT (lc_TmpStyD)
        REPLACE Alo&lcSize WITH Alo&lcSize - lcOldVal + lnObjVal,;
                TotAlo       WITH TotAlo       - lcOldVal + lnObjVal
        =lfAdjTStyD('',lcSize,lcOldVal,lnObjVal)
        
      ENDIF
      lcStkAlias = lc_TmpStyD
      SELECT (lnCurAlias)
      llShave = lfCanShave()
      *-- If the popup is diabled, then, the user has just decreased
      *-- the allocated quantities.
      IF llShave 
        IF lcDispReas = "DISABLE"
          lcDispReas = IIF(lnOrdShv <> 3,'ENABLE','DISABLE')
          IF lnOrdShv = 1
            lnSelRsn   = lnReson
            SELECT (lc_TmpOrdL) 
            REPLACE cReason WITH laShReson[lnSelRsn , 2]
          ELSE
            lnSelRsn   = 1
          ENDIF  
        ENDIF
      ELSE
        lcDispReas = "DISABLE"
        lnSelRsn   = 1 
        SELECT (lc_TmpOrdL)         
        REPLACE &lc_TmpOrdL..cReason WITH ' '  
      ENDIF
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*! Name      : lfCrtForcAl
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : force the allocation
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfCrtForcAl()
*!**************************************************************************
FUNCTION lfCrtForcAl
PARA lnForcdQty
m.nForcdQty = m.nForcdQty - lcOldVal + lnForcdQty 
STORE .T. TO m.lForceAlc

*!**************************************************************************
*! Name      : lfArInvSav
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : save the invoice no to the pocrton file 
*! Reference : C200489,1
*!**************************************************************************
*! Example   : lfArInvSav()
*!**************************************************************************
FUNCTION lfArInvSav
Private lnAlias
lnAlias = SELECT()
=gfOpenFile(gcDataDir+'POCRTNMF','POPIKTKT','SH')
SELECT POCRTNMF
IF SEEK(&lcHdrFile..PikTkt)
  SCAN REST WHILE piktkt= &lcHdrFile..PikTkt
    REPLACE INVOICE WITH &lcHdrFile..INVOICE
  ENDSCAN
ENDIF
SELECT (lnAlias)
RETURN

*:***************************************************************************
*:***************************************************************************
FUNCTION lfChkCarton
PRIVATE lnAlias,lnQty
lnAlias = SELECT()
lnQty = 0
SELECT (lcCrtnDet)
*B126708,1  TMI [Start] count no. of cartons of selected WH
SET FILTER TO CWARECODE = laWareHous[puWareHous,2]
LOCATE
*B126708,1  TMI [End  ] 
SCAN 
  lnQty = lnQty + nCrtNo
ENDSCAN
RETURN lnqty

*:***************************************************************************
*:***************************************************************************
FUNCTION lfChkAlSiz
PRIVATE lnAlias
PRIVATE lnTotPik1,lnTotPik2,lnTotPik3,lnTotPik4,lnTotPik5,lnTotPik6,lnTotPik7,lnTotPik8
PRIVATE lnTotCrt1,lnTotCrt2,lnTotCrt3,lnTotCrt4,lnTotCrt5,lnTotCrt6,lnTotCrt7,lnTotCrt8
lnAlias = SELECT()

STORE 0 TO lnTotPik1,lnTotPik2,lnTotPik3,lnTotPik4,lnTotPik5,lnTotPik6,lnTotPik7,lnTotPik8
SELECT (lc_tmpordl)
SCAN
  FOR lnTCount = 1 TO 8
    lcStr=STR(lnTCount,1)
    lnTotPik&lcStr=lnTotPik&lcStr + Pik&lcStr
  ENDFOR
ENDSCAN
STORE 0 TO lnTotCrt1,lnTotCrt2,lnTotCrt3,lnTotCrt4,lnTotCrt5,lnTotCrt6,lnTotCrt7,lnTotCrt8
SELECT (lcCrtnDet)
SCAN FOR nAlQty <> 0
  lnAlQty = nAlQty
  IF SEEK(cCartonTyp,'POCRTNMF')
    SELECT POCRTNMF
    lcCartontyp = ccartontyp
    lcCartonno  = ccartonno
    SCAN REST WHILE ccartontyp+ccartonno+cstymajor+style = lcCartontyp+lcCartonno
      FOR lnCRCount = 1 TO 8
        lcStr=STR(lnCrCount,1)
        *B607246,1 ABD - get the correct Pik. [Begin]
        *lnTotCrt&lcStr=lnTotCrt&lcStr + Qty&lcStr
        lnTotCrt&lcStr = lnTotCrt&lcStr + (Qty&lcStr * lnAlQty )
        *B607246,1 ABD - [End]
      ENDFOR
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lnAlias)
RETURN (lnTotPik1=lnTotCrt1 AND lnTotPik2=lnTotCrt2 AND lnTotPik3=lnTotCrt3 AND lnTotPik4=lnTotCrt4 AND ;
        lnTotPik5=lnTotCrt5 AND lnTotPik6=lnTotCrt6 AND lnTotPik7=lnTotCrt7 AND lnTotPik8=lnTotCrt8)

*B607246,1 ABD - [Begin]
*!**************************************************************************
*! Name      : lfRELEASPT
*! Developer : Abdou Elgendy
*! Date      : 06/08/2003
*! Purpose   : Release the piktkt no from release piktkt program.
*! Reference : B607246,1.
*!**************************************************************************
*! Example   : lfRELEASPT()
*!**************************************************************************
FUNCTION lfRELEASPT
PRIVATE lnOldAlias , llPOCRTNMF , lcPiktkt
llPOCRTNMF = .F.
llFound    = .T.
lnOldAlias = SELECT (0)

IF !USED("POCRTNMF")
  llPOCRTNMF = gfOpenFile(gcDataDir+'POCRTNMF','POPIKTKT','SH')
ENDIF

*--Release the old picktkt 
SELECT(lcTmpKtTk)
GO TOP
SCAN
  llFound = .T.
  DO WHILE llFound
    IF SEEK(Piktkt,'POCRTNMF')
      SELECT POCRTNMF
      REPLACE piktkt WITH SPACE(0)
      SELECT(lcTmpKtTk)
    ELSE
      llFound = .F.
    ENDIF
  ENDDO
ENDSCAN

IF llPOCRTNMF
  USE IN POCRTNMF
ENDIF

SELECT(lnOldAlias)
*-- End Of lfRELEASPT.
*B607246,1.

*:**************************************************************************
*:* Name        : lfADCTNBAR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 15/09/2004
*:* Purpose     : Add custom bar to shipment screen to open 'Carton Details'
*:***************************************************************************
*C123616,1
FUNCTION lfADCTNBAR
PRIVATE lnBarNo
lnBarNo = CNTBAR('_lPopOpt') + 1
DEFINE BAR lnBarNo OF _lPopOpt PROMPT '\<Carton Details' SKIP FOR EMPTY(laData[1]) .OR. lnActFolder=1 .OR. laScrMode[1]
ON SELECTION BAR lnBarNo OF _lPopOpt DO lfOpnCtnSc IN MEMMAIN.PRG
*&&>>> 
*-- end of lfADCTNBAR.

*:**************************************************************************
*:* Name        : lfDEFPAD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/10/2004
*:* Purpose     : Define pad for "POSREC" SCREEN
*:***************************************************************************
*:* Called from : POSTREC.PRG
*:***************************************************************************
*C123616,1
FUNCTION lfDEFPAD
PRIVATE lnBarNo

IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P,' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF

lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT '\<Receive Cartons' SKIP FOR !laScrMode[4] .OR. !INLIST(lnType,1,2)
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfOpnCtnSc WITH .T. IN MEMMAIN.PRG

*-- end of lfDEFPAD.


*:**************************************************************************
*:* Name        : lfRELPAD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/10/2004
*:* Purpose     : Release defined pad for Nik Nak in reciving PO screen
*:***************************************************************************
*:* Called from : postrec.prg
*:***************************************************************************
*C123616,1
FUNCTION lfRELPAD
RELEASE PAD _Option OF _MSYSMENU
ON KEY LABEL ENTER
=lfCLOSFILS()
*-- end of lfRELPAD.

*:**************************************************************************
*:* Name        : lfADBRPICK
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/10/2004
*:* Purpose     : Add a  new bar to Pick Cartons
*:***************************************************************************
*:* Called from : ALORDAL.PRG
*:***************************************************************************
*C123616,1
FUNCTION lfADBRPICK
PRIVATE lnBarNo
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT '\<Pick Cartons' SKIP FOR EMPTY(laData[1]) .OR. laScrMode[1] .OR. !llGenPkTk
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfPikCrt IN MEMMAIN.PRG

*:**************************************************************************
*:* Name        : lfADBRPICK
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/10/2004
*:* Purpose     : Add a  new bar to Pick Cartons
*:***************************************************************************
*:* Called from : ALORDAL.PRG
*:***************************************************************************
*C123616,1
FUNCTION lfGENCRTPD
PRIVATE lnBarNo
lnBarNo = CNTBAR('_OPTPOP') + 1
DEFINE BAR lnBarNo OF _OPTPOP PROMPT '\<Generate Cartons' SKIP FOR EMPTY(laData[2]) .OR. EMPTY(laData[3]) .OR. laScrMode[1] .OR. laScrMode[2] .OR. lnActFolder<>2
ON SELECTION BAR lnBarNo OF _OPTPOP DO lfGenCrt IN MEMMAIN.PRG


*-- end of lfADBRPICK.
*!*************************************************************
*! Name      : lfFoundPad
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/29/2001
*! Purpose   : check if any pad menu is exit in _sysmenu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C102172
*:**************************************************************************
*
*C123616,1
FUNCTION lfFoundPad
PARAMETER lcPadName

PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
RETURN(llFOund)
*-- end of lfFoundPad.

*:**************************************************************************
*:* Name        : lfOpnCtnSc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/16/2004
*:* Purpose     : Open Cartons type Screen
*:***************************************************************************
*C123616,1
FUNCTION lfOpnCtnSc
PARAMETERS llRcv

PRIVATE lnAlias, lcCrtnHdr, lcCrtnLine, lcScaleFle, lcPoCrTmp,lnTotCrtNo ;
        lnClrPos,lnClrLen,lnExtSzWd,lnMaxLns,lnCurLin,lcClrDesc,;
        llChanged,lnStyTot

*- lnMaxLns   : The customer needs all scale groups for extended scale system to be saved ontinuously 
*-              so if we have scale SS as follows
*-                   SS1  with sizes => 20,21
*-                   SS2  with sizes => 30,35,40,45,50
*-                   SS3  with sizes => 60,61,62,63
*-              Then they will be saved in lcCrtnLine as follows 
*-                          20,21,30,35,40,45,50,60   ( first line )
*-                          61,62,63                  ( second line )  and so on ..
*- This variable shows the maximum number of lines for a style in a po , we show only one line
*- at a moment using filter with the variable lnCurLin 

*- llChanged  : check if data is changed
lnAlias = SELECT()

*- Activate the window that contains the stock qty to be refreshed when we return back
ACTIVATE WINDOW &lcWinCh0  

IF llRcv
  DO CASE
  CASE lnType = 1
    lcCodeNo = lcTCode 
  CASE lnType = 2
    lcCodeNo = lcShpCode
  ENDCASE
ELSE
  lcCodeNo = laData[1]
ENDIF  

lnTotCrtNo = 0
lnStyTot = 0

STORE 0 TO lnClrPos,lnClrLen
=lfGetClrDt()    && Get Color data
lnExtSzWd = gfGetMemVar('M_EXTWIDTH')
lnMaxLns = 0
lnCurLin = 1
STORE ' ' TO M.SZ1,M.SZ2,M.SZ3,M.SZ4,M.SZ5,M.SZ6,M.SZ7,M.SZ8
lcClrDesc = ' '
llChanged = .F.

*--NonMajor Header
lcStyHdr   =  gfItemMask('HM')
lcNnMjrHdr =  gfItemMask('HN')
lnNonMjrLn =  LEN(gfItemMask('PN'))
lnstylewid = LEN(gfItemMask('PM'))

****lcTempName = '_'+SUBSTR(lcWinCh0,2)
IF !USED('TempName')
  =lfCrTmpFls()
ENDIF

SELECT TempName
SCAN 
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

SELECT &lcCrtnHdr
LOCATE FOR CCARTONTYP = 'S'  && Check that there is types added in Add/Edit mode , 
llAdded = FOUND()            && use this variable in view mode to show data from saved files not from temp files

*- If shipment# changed Re Fill lcShpCrtmf temp file
IF TYPE('lcSHPCRTMF')='C' .AND. USED(lcSHPCRTMF)
  SELECT &lcSHPCRTMF
  GO TOP
  IF &lcSHPCRTMF..CSHIPNO <> lcCodeNo .OR. (laScrMode[2] .AND. llAdded)
    =lfFillCrtMf()
  ENDIF
ENDIF


lcUsedFile=lcCrtnLine  && Used in browse

*- Fill lcCrtnHdr and lcCrtnLine with data saved in lcShpCrtMf file if any
=lfClcShpDt()

PUSH KEY
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

PRIVATE lcWinCh5,lcWinCh6,lcChck,lcUnChck,lcRLTit,;
        lcFab,lcClr,lcWare,lcDye,lnTotApply,lnUsrApply,lnRcvdQty,;
        lcFileToUse,llBrowse,lcStyHdr 
PRIVATE lcRollTitl,lcRlBrFild

llNewCrt = .T.
llBrowse = .F.
lnRcvdQty = 0 

lcFileToUse = lcCrtnHdr
lnNewRec = RECNO(lcFileToUse)

lcStyHdr =  gfItemMask('HM')
STORE SPACE(0) TO lcWinCh5,lcWinCh6

lcStyle = ''
lcDesc  = ''

lcWare = ''
lcDye  = ''

STORE 0 TO lnOldVal

STORE 0 TO lnUsrApply
STORE 0 TO lnTotApply

lcRLTit = 'Carton types'

lcLotRo  = gfTempName()
lcLotRo1 = gfTempName()
lcLotRo2 = gfTempName()
lcLotRo3 = gfTempName()


SELECT(lcCrtnHdr)
LOCATE
lnBrRecNo = RECNO()

lcRlBrFild = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "cCartonTyp :R :H='Carton Type':17,"+;
             "Weight :R :10,"+;
             "nLength :R :H='Length' :10,"+;
             "Width :R :10,"+;
             "nDepth :R :H='Depth' :10,"+;
             "CrtnNo :R :H='No Of Cartons' :17"
lcRollTitl = 'Cartons'
*-- Call the screen
PUSH KEY
lcSysMen = SET('SYSMENU')
SET SYSMENU ON
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcRollTitl)

****** Define detail screen variables
lcRLTit = 'Colors Details'

lcCrtDet  = gfTempName()
lcCrtDet1 = gfTempName()
lcCrtDet2 = gfTempName()
lcCrtDet3 = gfTempName()

PRIVATE lcCrtTitl,lcCrtBrFld,lcQty1,lcQty2,lcQty3,lcQty4,lcQty5,lcQty6,lcQty7,lcQty8
lcCrtTitl = 'Carton Details'

*- Update columns titles of the carton detail browse
lcQty1 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+1))
lcQty2 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+2))
lcQty3 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+3))
lcQty4 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+4))
lcQty5 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+5))
lcQty6 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+6))
lcQty7 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+7))
lcQty8 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+8))

lcCrtBrFld = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "STYLE  :R :16:H='Style/Color' ,"+;
             "Qty1   :R :7 :H='&lcQty1',"+;
             "Qty2   :R :7 :H='&lcQty2',"+;
             "Qty3   :R :7 :H='&lcQty3',"+;
             "Qty4   :R :7 :H='&lcQty4',"+;
             "Qty5   :R :7 :H='&lcQty5',"+;
             "Qty6   :R :7 :H='&lcQty6',"+;
             "Qty7   :R :7 :H='&lcQty7',"+;
             "Qty8   :R :7 :H='&lcQty8',"+;
             "STYTot :R :H='Total Qty' :9"
******

RELEASE PAD _BROWSE OF _MSYSMENU
DO (gcScrDir+'PO\POCRHDR.SPX')

SET SYSMENU &lcSysMen
POP KEY

IF llRcv
  *- Update "lnTotStk" variable on "posrec" screen
  lnTotStk = 0
  SELECT &lcTmpLine
  SUM TOTSTK TO lnTotStk FOR TRANCD = '1'
  GO TOP
  SHOW GET lnTotStk
ELSE
  *- Update "lnTotQty" variable on "poshp" screen
  lnTotQty = 0
  SELECT &lcShpLine
  SUM TOTQTY TO lnTotQty 
  GO TOP
  SHOW GET lnTotQty
ENDIF  

SELECT (lnAlias)
*-- end of lfOpnCtnSc.

*:**************************************************************************
*:* Name        : lfCrTmpFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 28/09/2004
*:* Purpose     : Create temp files
*:***************************************************************************
*C123616,1
FUNCTION lfCrTmpFls

lcCrtnHdr  = gfTempName()
lcCrtnLine = gfTempName()
lcScaleFle = gfTempName()
lcPoCrTmp  = gfTempName()
lcShpCrtMf = gfTempName()

*IF !USED(lcScaleFle)
*  =gfOpenFile(gcDataDir+'SCALE','SCALE','SH',@lcScaleFle,.T.)
*ENDIF
=gfOpenFile(gcDataDir+'SHPCRTMF','SHPCRTMF','SH')
=gfOpenFile(gcDataDir+'POCRTNMF','POCRTNMF','SH')
=gfOpenFile(gcDataDir+'SHPMTHDR','SHPMTHDR','SH')
=gfOpenFile(gcDataDir+'POSLN','POSLN','SH')

DIMENSION laFileStru[1,4]
SELECT SHPCRTMF
=AFIELDS(laFileStru)
 
lnFldNo =ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnFldNo,4]
laFileStru[lnFldNo,1] = 'CrtnNo'
laFileStru[lnFldNo,2] = 'N'
laFileStru[lnFldNo,3] =  5
laFileStru[lnFldNo,4] =  0

lnFldNo =ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnFldNo,4]
laFileStru[lnFldNo,1] = 'CUPDATE'
laFileStru[lnFldNo,2] = 'C'
laFileStru[lnFldNo,3] =  1
laFileStru[lnFldNo,4] =  0

CREATE TABLE (gcWorkDir+lcCrtnHdr) FROM ARRAY laFileStru
INDEX ON cCartontyp TAG lcCrtnHdr OF (gcWorkDir+lcCrtnHdr)

*- Add more fields and create lcCrtnLine from the same array

lnFldNo =ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnFldNo,4]
laFileStru[lnFldNo,1] = 'NLINE'
laFileStru[lnFldNo,2] = 'N'
laFileStru[lnFldNo,3] =  2
laFileStru[lnFldNo,4] =  0

FOR lnCount = 1 TO 8 
  lnFldNo =ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnFldNo,4]
  laFileStru[lnFldNo,1] = 'OldQty'+STR(lnCount,1)
  laFileStru[lnFldNo,2] = 'N'
  laFileStru[lnFldNo,3] =  6
  laFileStru[lnFldNo,4] =  0
ENDFOR

FOR lnCount = 1 TO 8 
  lnFldNo =ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnFldNo,4]
  laFileStru[lnFldNo,1] = 'SZ'+STR(lnCount,1)      && Add these fields for scale description
  laFileStru[lnFldNo,2] = 'C'
  laFileStru[lnFldNo,3] =  5
  laFileStru[lnFldNo,4] =  0
ENDFOR

lnFldNo =ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnFldNo,4]
laFileStru[lnFldNo,1] = 'STYTot'
laFileStru[lnFldNo,2] = 'N'
laFileStru[lnFldNo,3] =  8
laFileStru[lnFldNo,4] =  0

*- Add fields to save scale for each size
FOR lnCount = 1 TO 8 
  lnFldNo =ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnFldNo,4]
  laFileStru[lnFldNo,1] = 'SCALE'+STR(lnCount,1)      && Add these fields for scale description
  laFileStru[lnFldNo,2] = 'C'
  laFileStru[lnFldNo,3] =  3
  laFileStru[lnFldNo,4] =  0
ENDFOR

*- Add fields to save line # for each size
FOR lnCount = 1 TO 8 
  lnFldNo =ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnFldNo,4]
  laFileStru[lnFldNo,1] = 'LINENO'+STR(lnCount,1)      && Add these fields for scale description
  laFileStru[lnFldNo,2] = 'N'
  laFileStru[lnFldNo,3] =  6
  laFileStru[lnFldNo,4] =  0
ENDFOR

CREATE TABLE (gcWorkDir+lcCrtnLine) FROM ARRAY laFileStru
*INDEX ON Style+cCartonType TAG &lcCrtnLine OF (gcWorkDir+lcCrtnLine)
*INDEX ON CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE TAG TYPESTYLE OF (gcWorkDir+lcCrtnLine)
INDEX ON CCARTONTYP+STYLE TAG TYPESTYLE OF (gcWorkDir+lcCrtnLine)

*SET RELATION TO 'S'+Scale INTO (lcScaleFle) ADDITIVE

CREATE Cursor TempName (ctempname M(10))
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcCrtnHdr 
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcCrtnLine 
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcScaleFle
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcPoCrTmp
APPEND BLANK
SAVE TO MEMO cTempName ALL LIKE lcShpCrtMf

*- Create the temp cursor lcShpCrtMf
DIMENSION laStru[1,4]
SELECT SHPCRTMF
=AFIELDS(laStru)

CREATE TABLE (gcWorkDir+lcShpCrtMf) FROM ARRAY laStru
INDEX ON CSHIPNO+CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE TAG &lcShpCrtMf

=lfFillCrtMf()

*-- end of lfCrTmpFls.

*:**************************************************************************
*:* Name        : lfFillCrtMf
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/18/2004
*:* Purpose     : Fill the temp file lcShpCrtMf from ShpCrtMf file
*:***************************************************************************
*C123616,1
FUNCTION lfFillCrtMf

*- Fill the temp file lcShpCrtMf from ShpCrtMf file
IF !EMPTY(lcCodeNo)
  IF SEEK(lcCodeNo,'ShpCrtMf') .OR. laScrMode[4]
    SELECT &lcSHPCRTMF
    ZAP
    SELECT &lcCrtnHdr
    ZAP
    SELECT &lcCrtnLine
    ZAP
    
    SELECT ShpCrtMf
    SCAN REST WHILE CSHIPNO+CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE = lcCodeNo ;
              FOR !LRECEIVED 
      SCATTER MEMVAR 
      INSERT INTO &lcShpCrtMf FROM MEMVAR
    ENDSCAN
  ENDIF
ENDIF

*-- end of lfFillCrtMf.

*:**************************************************************************
*:* Name        : lfCrtnLns
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 22/09/2004
*:* Purpose     : Open carton lines screen
*:***************************************************************************
*:* Called from : pocrtyp.scx
*:***************************************************************************
*C123616,1
FUNCTION lfCrtnLns
PARAMETERS llNewCrt

PRIVATE lcWinCRt5,lcWinCrt6,lcWinCRt7,lcWinCRt8,lcChck,lcUnChck,lcRLTit,;
        lnWeight,lnLength,lnWidth,lnDepth,lnNOfCrtn,lcFields,;
        lcPoNo,lcCrtStat,;
        llBrowse,lcOldValue,;
        lcStyHdr ,lcNnMjrHdr ,lnNonMjrLn 

llChanged = .F.

lcPrvNxt = 'DISABLE'
lcCrtStat = IIF(llNewCrt,"ENABLE","DISABLE")
lnWeight = 0
lnLength = 0 
lnWidth  = 0 
lnDepth  = 0
lnNOfCrtn= 1

lcOldValue = ' '
lnTotPerCt = 0 

llShpmnt = lfCallFrm('POSHP.FXP')   && Check if the calling program is POSHP.FXP
lcTmpFl = IIF(llShpmnt,lcShpLine,lcTmpLine)

llBrowse = .F.
lnNewRec = RECNO(lcCrtnLine)
lcCartnTyp = ' '

*- Release a filter if any..
SELECT (lcCrtnLine)
SET FILTER TO 
GO TOP

IF llNewCrt
  
  IF laScrMode[2]
    WAIT WINDOW NOWAIT 'Can not add new types in View mode.'
    RETURN
  ENDIF  
  
  SELECT (lcCrtnLine)
  lcCartnTyp = lfTypSeq()
  
  IF llRcv 
    IF lnType = 1 .AND. !EMPTY(lcTCode)
      lcPO       = lcTCode
      lcWareHous = POSHDR.CWARECODE 
      lcCrtStat = 'DISABLE'

      SHOW GET lcPo DISABLE  
      SHOW GET ibPo DISABLE
  
      *- Call valid funciton for PO to add lines of styles found in PO but with zero qty's in cartons
      llBrowse = .F.
      =lfvPO(.T.)
    ENDIF
  ENDIF  
    
ELSE  

  IF laScrMode[2]
    WAIT WINDOW NOWAIT 'You can not update any qty in View mode.'
  ENDIF  

  SELECT (lcCrtnHdr)
  
  lcPo       = &lcCrtnHdr..PO
  lcWareHous = &lcCrtnHdr..CWARECODE
  lcCartnTyp = &lcCrtnHdr..CCARTONTYP
  lnWeight   = &lcCrtnHdr..weight     
  lnLength   = &lcCrtnHdr..nlength    
  lnDepth    = &lcCrtnHdr..ndepth     
  lnWidth    = VAL(&lcCrtnHdr..width) 
  lnNofCrtn  = &lcCrtnHdr..CrtnNo     
  
  DIMENSION laStySum[1,2]
  SELECT STYLE,SUM(TOTQTY) ;
    FROM &lcCrtnLine ;
     WHERE cCartonTyp = lcCartnTyp ;
     GROUP BY STYLE ;
     INTO ARRAY laStySum
  SELECT &lcCrtnLine

  SELECT (lcCrtnLine)
  SCAN FOR cCartonTyp = lcCartnTyp
    FOR lnCount = 1 To 8
      lcCount = ALLTRIM(STR(lnCount))
      REPLACE OldQty&lcCount WITH Qty&lcCount
      lnTotPerCt = lnTotPerCt + Qty&lcCount
    ENDFOR    
    lnPos = ASCAN(laStySum,STYLE)
    REPLACE STYTOT WITH laStySum[lnPos+1]
  ENDSCAN
  
  SHOW GET lcPo DISABLE  
  SHOW GET ibPo DISABLE
  
  *- Call valid funciton for PO to add lines of styles found in PO but with zero qty's in cartons
  llBrowse = .F.
  =lfvPO(.T.)

ENDIF

SELECT (lcCrtnLine)
SET FILTER TO CCARTONTYP=lcCartnTyp AND NLINE=1
GO TOP
lcFields = "Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,STYTot"
SCATTER FIELDS &lcFields MEMVAR

STORE SPACE(0) TO lcWinCRt5,lcWinCrt6,lcWinCrt7,lcWinCrt8

STORE 0 TO lnOldVal

STORE 0 TO lnUsrApply
STORE 0 TO lnTotApply

             
*-- Call the screen
PUSH KEY
lcSysMen = SET('SYSMENU')
SET SYSMENU ON
ON KEY LABEL ALT+C ACTIVATE WINDOW (lcCrtTitl)
ON KEY LABEL CTRL+LEFTARROW 
ON KEY LABEL CTRL+RIGHTARROW DO lfvSzScrl WITH 1

RELEASE PAD _BROWSE OF _MSYSMENU
lcOldTab = ON('KEY','TAB')
lcOldBtb = ON('KEY','BACKTAB')    
lcOldEnt = ON('KEY','ENTER')
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
llScr1 = .F.  && used in moving between fields on the screen pocrlin

DO (gcScrDir+'PO\POCRLIN.SPX')

ON KEY LABEL TAB     &lcOldTab
ON KEY LABEL BACKTAB &lcOldBtb
ON KEY LABEL ENTER   &lcOldEnt

**=lfwTypsBrw()
SHOW WINDOW (lcRollTitl) REFRESH SAME
SHOW GET lnTotStk

SET SYSMENU &lcSysMen
POP KEY

*-- end of lfCrtnLns.

*:**************************************************************************
*:* Name        : lfTypSeq
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2004
*:* Purpose     : Temp sequence for type field
*:***************************************************************************
*C123616,1
FUNCTION lfTypSeq
PRIVATE lnSequence,lnRecNo
lnSequence = 0
lnRecNo = RECNO()
SCAN
  lnSequence = MAX(lnSequence,VAL(SUBSTR(cCartonTyp,2)) )
ENDSCAN
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
RETURN 'S'+PADL(ALLTRIM(STR(lnSequence+1)),5,"0")

*:**************************************************************************
*:* Name        : lfvRmv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 26/09/2004
*:* Purpose     : Remove a carton type
*:***************************************************************************
*C123616,1
FUNCTION lfvRmv
PRIVATE lnAlias , lnTotqty , lnRecNo
lnAlias = ALIAS()

IF laScrMode[2]
  WAIT WINDOW NOWAIT 'Can not remove types in View mode.'
  RETURN
ENDIF
IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure to remove this Carton type?') = 1
  SELECT (lcCrtnHdr)
  lcCartnTyp = cCartonTyp 
  
  SELECT (lcCrtnLine)
  DELETE FOR cCartonTyp = lcCartnTyp
  
  SELECT (lcCrtnHdr)
  REPLACE CUPDATE WITH 'D'
  DELETE 
  LOCATE
  
  =lfwTypsBrw()
ENDIF
*-- end of lfvRmv.
*:**************************************************************************
*:* Name        : lfvCrtOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 26/09/2004
*:* Purpose     : Accept carton breakdown entered
*:***************************************************************************
*C123616,1
FUNCTION lfvCrtOk
PRIVATE lnResp,lnICount,lcFields,lnRcvd,;
        lnCrtnSeq,lcSvOrd,llContinue
IF laScrMode[2]
  RETURN
ENDIF
IF EOF(lcCrtnHdr)  && if no types then continue without showing a message  
  SELECT (IIF(llRcv,lcTmpLine,lcShpLine))
  DELETE ALL 
  SELECT &lcShpcrtmf
  ZAP 
  RETURN
ENDIF
  
lcFields = "Style,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty"
lcSvOrd = ORDER('POSLN')
SET ORDER TO POSLN IN POSLN

SELECT &lcCrtnHdr
SET DELETED OFF
LOCATE FOR CUPDATE <> ' ' 
llContinue = FOUND()
SET DELETED ON

lnResp = 1

IF llContinue
  REPLACE CUPDATE WITH '' ALL
  PACK
  
  *- Check that the shipment started to be shipped , if so no updates will take place
  IF llRcv

    lnResp = gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Ok to update recieving lines by cartons types?')

  ELSE
  
    =SEEK(laData[1],'SHPMTHDR')     
    IF SHPMTHDR.STATUS = 'X' 
      =gfModalGen('QRM00000B00000',.F.,.F.,.F.,'This shipment is Cancelled , can not update !')
      lnResp = 2
    ELSE

    lnRcvd = SHPMTHDR.RECV_STK + SHPMTHDR.RECV_DAM + SHPMTHDR.RECV_CAN
    IF lnRcvd = 0
      lnResp = gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Ok to update the shipment with cartons data?')
    ELSE
      lcStats = IIF(SHPMTHDR.STATUS = 'C' , 'Completed','is being shipped')      
      =gfModalGen('QRM00000B00000',.F.,.F.,.F.,'This shipment &lcStats , can not update !')
      lnResp = 2
    ENDIF
    
    ENDIF

  ENDIF
  
  IF lnResp = 1    && Update lcShpCrtMf file with changes applied to lcCrtnHdr and lcCrtnLine 
    
    SELECT TEMPNAME
    COPY TO (gcWorkDir+'TEMPNAME.TMP')   && Use this file to refresh the browse screen in the standard
    
    SELECT &lcShpCrtMF
    ZAP

    *B125800,1 NNA 01/06/2005 (Begin) move this code to a next place to show the Carton no.
    *WAIT WINDOW NOWAIT 'Creating Cartons.. '
    *B125800,1 NNA (End)
    
    *- Release filter if any..
    SELECT (lcCrtnLine)  
    SET FILTER TO 
    
    lnCrtnSeq = 0
    SELECT &lcCrtnHdr
    GO TOP
    SCAN FOR ToTQty <> 0
      lcCartonTyp  = cCartonTyp
      FOR lnICount = 1 TO CrtnNo
      
        lnCrtnSeq = lnCrtnSeq + 1
        m.cCartonNo = 'S'+PADL(lnCrtnSeq,5,'0')

        *B125800,1 NNA 01/06/2005 (Begin) Wait window with counter for Carton Number
        WAIT WINDOW NOWAIT 'Creating Carton No. : ' + ALLTRIM(STR(lnCrtnSeq))
        *B125800,1 NNA (End)
    
        SELECT (lcCrtnLine)  
        SCAN FOR cCartonTyp = lcCartonTyp
          lcStyClr = &lcCrtnLine..STYLE

          *- Fill scale array
          lcSclCrit = SUBSTR(&lcCrtnLine..SCALE1,1,lnExtSzWd)
          SELECT SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SCALE,CNT,0 ;
             FROM SCALE ;
             WHERE SCALE.TYPE+SCALE.SCALE = 'S'+lcSclCrit ;
             INTO ARRAY laScl
          lnSzCnt = 0
  
          *- Update data from lcCrtnLine to lcShpCrtMf
          FOR lnK = 1 TO ALEN(laScl,1)
            SELECT (lcCrtnLine)  
            STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.TOTQTY,;
                       M.LINENO
            FOR lnS = 1 TO laScl[lnK,10]
              lnSzCnt = lnSzCnt + 1
              IF lnSzCnt > 8
                lnSzCnt = 1
                SKIP
              ENDIF
              lcS = STR(lnS,1)
              lcSzCnt = STR(lnSzCnt,1)
              M.QTY&lcS = &lcCrtnLine..Qty&lcSzCnt
              IF M.QTY&lcS>0
                M.LINENO = &lcCrtnLine..LINENO&lcSzCnt
              ENDIF
            ENDFOR
            M.TOTQTY = M.QTY1+M.QTY2+M.QTY3+M.QTY4+M.QTY5+M.QTY6+M.QTY7+M.QTY8
            IF M.TOTQTY > 0
              SELECT &lcShpCrtMF
              APPEND BLANK
              REPLACE CCARTONNO  WITH m.cCartonNo ;
                      CCARTONTYP WITH lcCartonTyp ;
                      STYLE      WITH SUBSTR(&lcCrtnLine..STYLE,1,lnClrPos+lnClrLen-1)+laScl[lnK,9] ;
                      SCALE      WITH laScl[lnK,9] ;
                      CSTYMAJOR  WITH &lcCrtnLine..CSTYMAJOR ;
                      PO         WITH &lcCrtnLine..PO ;
                      CSHIPNO    WITH &lcCrtnLine..CSHIPNO ;
                      WEIGHT     WITH &lcCrtnHdr..WEIGHT ;
                      NLENGTH    WITH &lcCrtnHdr..NLENGTH ;
                      NDEPTH     WITH &lcCrtnHdr..NDEPTH ;
                      WIDTH      WITH &lcCrtnHdr..WIDTH ;
                      CWARECODE  WITH &lcCrtnHdr..CWARECODE ;
                      LINENO     WITH M.LINENO ;
                      QTY1       WITH M.QTY1 ;
                      QTY2       WITH M.QTY2 ;
                      QTY3       WITH M.QTY3 ;
                      QTY4       WITH M.QTY4 ;
                      QTY5       WITH M.QTY5 ;
                      QTY6       WITH M.QTY6 ;
                      QTY7       WITH M.QTY7 ;
                      QTY8       WITH M.QTY8 ;
                      TOTQTY     WITH M.TOTQTY 
            ENDIF
          ENDFOR       
           
        ENDSCAN
      ENDFOR
    ENDSCAN
    
    *- Update shipline/lcTmpLine
    =lfUpdShpLn()

  ELSE
  
    *- Restore data saved in lcShpCrtMf to lcCrtnHdr and lcCrtnLine
    SELECT &lcCrtnHdr
    ZAP
    SELECT &lcCrtnLine
    ZAP  
    *- Fill lcCrtnHdr and lcCrtnLine with data saved in lcShpCrtMf file if any
    =lfClcShpDt()
  
  ENDIF

ENDIF
SET ORDER TO (lcSvOrd)    IN POSLN


*-- end of lfvCrtOk.

*:**************************************************************************
*:* Name        : lfUpdShpLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 30/09/2004
*:* Purpose     : Update shipline temp file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdShpLn()
*:***************************************************************************
*C123616,1
FUNCTION lfUpdShpLn

PRIVATE lnSlct,lcTmpFl,lcSvOrd,lcSvOrd2,lcShp
lnSlct = SELECT()
IF RECCOUNT(lcShpCrtMf) > 0

  *- Update lcShpLine/lcTmpLine temp file
  *- First delete all    
  
  lcTmpFl = IIF(llRcv,lcTmpLine,lcShpLine)
  lcPoTp = IIF(llRcv,'P','')
  STORE ' ' TO lcSvOrd,lcSvOrd2 
  IF llRcv
    lcSvOrd = ORDER(lcTmpFl)
    lcSvOrd2 = ORDER('POSLN')
    SET ORDER TO TAG POSLN IN (lcTmpFl)
    SET ORDER TO TAG POSLNSH IN POSLN
  ENDIF
  
  SELECT &lcTmpFl
  DELETE ALL  
  
  SELECT &lcShpCrtMf
  SCAN
    WAIT WINDOW NOWAIT 'Updating Shpping data with style ' + STYLE
    SCATTER MEMVAR
    lcStyle = &lcShpCrtMf..STYLE
    IF !SEEK(lcPoTp+PO+STYLE+STR(LINENO,6),lcTmpFl)
      *=SEEK('      '+'P'+PO+STYLE+STR(LINENO,6)+'1','POSLN')
      lcShp = IIF(llRcv,'      ','')
      IF TYPE('lcShpCode')='C' .AND. !EMPTY(lcShpCode)
        IF !SEEK(lcShpCode+'P'+PO+STYLE+STR(LINENO,6)+'3','POSLN') 
          =SEEK(lcShp+'P'+PO+STYLE+STR(LINENO,6)+'1','POSLN')
        ENDIF
      ELSE
        =SEEK(lcShp+'P'+PO+STYLE+STR(LINENO,6)+'1','POSLN')      
      ENDIF      
      SELECT POSLN
      SCATTER MEMVAR
      =SEEK('P'+POSLN.PO,'POSHDR')
      M.CSTYGRADE = '1' 
      M.CWARECODE = &lcShpCrtmf..cWareCode
      IF llRcv .AND. POSHDR.STATUS = 'O'  && include only Open Po's
        M.TRANCD = '1'
        INSERT INTO &lcTmpFl FROM MEMVAR
      ENDIF
      
      STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8      
      M.TRANCD    = IIF(llRcv,'2','3')
      M.SHIPNO    = &lcShpCrtMf..CSHIPNO
      IF IIF(llRcv , POSHDR.STATUS = 'O' , .T. )      
        INSERT INTO &lcTmpFl FROM MEMVAR
      ENDIF
      *B126708,1  TMI [Start] be sure that for the same PO/style /LineNo the same WH is stored in shpcrtmf file
    ELSE
      SELECT &lcShpCrtMf
      REPLACE CWARECODE WITH &lcTmpFl..CWARECODE
    *B126708,1  TMI [End  ] 
    ENDIF
    IF IIF(llRcv , SEEK('P'+PO+STYLE+STR(LINENO,6)+'2',lcTmpFl) , .T. )
      SELECT &lcTmpFl
      REPLACE QTY1   WITH QTY1 + &lcShpCrtMf..QTY1 ;
              QTY2   WITH QTY2 + &lcShpCrtMf..QTY2 ;
              QTY3   WITH QTY3 + &lcShpCrtMf..QTY3 ;
              QTY4   WITH QTY4 + &lcShpCrtMf..QTY4 ;
              QTY5   WITH QTY5 + &lcShpCrtMf..QTY5 ;
              QTY6   WITH QTY6 + &lcShpCrtMf..QTY6 ;
              QTY7   WITH QTY7 + &lcShpCrtMf..QTY7 ;
              QTY8   WITH QTY8 + &lcShpCrtMf..QTY8 ;
              TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
    ENDIF
  ENDSCAN


  IF llRcv
    lnRate1 = lnCrRt1
    lnRate2 = lnCrRt2

    DIMENSION laQty[9]
    laQty = 0
    lnOriginal = 0
    
    *- Loop to update totstk and totbal fields
    SELECT &lcTmpLine
    SCAN
      REPLACE nLanPrRat WITH lnCrRt1 ;
              nLanDuRat WITH lnCrRt2              

      DO CASE
      CASE TRANCD = '1'
        lnOriginal = TOTQTY
      CASE TRANCD = '2'
        SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO laQty
        lnTotStk = TOTQTY
        REPLACE TOTSTK WITH lnOriginal
        SKIP -1
        REPLACE TOTSTK WITH lnTotStk ;
                TOTBAL WITH MAX(QTY1-laQty[1],0)+MAX(QTY2-laQty[2],0)+MAX(QTY3-laQty[3],0)+MAX(QTY4-laQty[4],0)+;
                            MAX(QTY5-laQty[5],0)+MAX(QTY6-laQty[6],0)+MAX(QTY7-laQty[7],0)+MAX(QTY8-laQty[8],0)
        SKIP
      ENDCASE
            
    ENDSCAN
    
    IF !EMPTY(lcSvOrd)
      SET ORDER TO TAG (lcSvOrd) IN (lcTmpFl)
      SET ORDER TO TAG (lcSvOrd2) IN POSLN
    ENDIF
  ENDIF

  SELECT &lcTmpFl
  GO TOP
  
ENDIF
WAIT CLEAR

SELECT (lnSlct)
*-- end of lfUpdShpLn.


*:**************************************************************************
*:* Name        : lfClcShpDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 23/09/2004
*:* Purpose     : Collect Shipment data from lcShpcrtmf to lcCrtnHdr and lcCrtnLine
*:***************************************************************************
*C123616,1
FUNCTION lfClcShpDt
PRIVATE lnSlct,lnCrtnNo,lnTotQty,lcCrtnNo,laStru,lnLn,lcCrtNo

lnLn = 0

*- Since at first time we get carton data from SHPCRTMF file , so I will create a copy of the shipment data 
*- and save it in a temp file , so when editing carton information the program will look to this
*- temp file , in saving , we will copy the data saved in this temp file to SHPCRTMF file 

lnSlct = SELECT()

*- Clear filters of lcCrtnLine if any
SELECT &lcCrtnLine
SET FILTER TO 
GO TOP

SELECT &lcSHPCRTMF
GO TOP
lnCrtnNo = 1
lnTotQty = 0

DO WHILE &lcSHPCRTMF..CSHIPNO = lcCodeNo .AND. !EOF(lcSHPCRTMF)

  lcCartnTyp = &lcSHPCRTMF..CCARTONTYP
  lcCrtNo = &lcShpCrtMF..CCARTONNO 


  SELECT &lcSHPCRTMF
  WAIT WINDOW NOWAIT STYLE
  IF !SEEK(lcCartnTyp,lcCrtnHdr)
    SELECT &lcCrtnHdr
    APPEND BLANK
    REPLACE CCARTONTYP WITH &lcSHPCRTMF..CCARTONTYP,;
            PO         WITH &lcSHPCRTMF..PO        ,;
            CWARECODE  WITH &lcSHPCRTMF..CWARECODE ,;
            WEIGHT     WITH &lcSHPCRTMF..WEIGHT    ,;
            NLENGTH    WITH &lcSHPCRTMF..NLENGTH   ,;
            NDEPTH     WITH &lcSHPCRTMF..NDEPTH    ,;
            WIDTH      WITH &lcSHPCRTMF..WIDTH     ,;
            cStyMajor  WITH &lcSHPCRTMF..CSTYMAJOR
    lcCrtNo = &lcShpCrtMF..CCARTONNO 
    lnCrtnNo = 1
    lnTotQty = 0
  ENDIF
  
  
  DIMENSION laScl[1]
  laScl[1] = ' '

  *- Fill scale array
  lcSclCrit = SUBSTR(&lcSHPCRTMF..SCALE,1,lnExtSzWd)
  SELECT SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SCALE,CNT,0 ;
     FROM SCALE ;
     WHERE SCALE.TYPE+SCALE.SCALE = 'S'+lcSclCrit ;
     INTO ARRAY laScl
  lnSzCnt = 0
  FOR lnK = 1 TO ALEN(laScl,1)
    laScl[lnK,11] = lnSzCnt  && This scale group starts with This No + 1
    lnSzCnt = lnSzCnt + laScl[lnK,10]
  ENDFOR       
  
*  lcCrtStyClr = CSHIPNO+CCARTONTYP+CCARTONNO+CSTYMAJOR+;
                     SUBSTR(&lcSHPCRTMF..STYLE,1,lnClrPos+lnClrLen-1)
  lcCrtStyClr = CCARTONTYP+SUBSTR(&lcSHPCRTMF..STYLE,1,lnClrPos+lnClrLen-1)  
  lcStyClr = SUBSTR(&lcSHPCRTMF..STYLE,1,lnClrPos+lnClrLen-1)
  
  *- Add lines for the current style in lcCrtnLine file
  IF !SEEK(lcCrtStyClr,lcCrtnLine)
    SELECT &lcSHPCRTMF
    SCATTER MEMVAR
    lnLn = 0
    
    STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.TOTQTY

     *- Add lines to lcCrtnLine for the current style           
    M.STYLE = lcStyClr
    M.NLINE = 1
    INSERT INTO &lcCrtnLine FROM MEMVAR
    
    lnCnt = 0
    =SEEK('S'+lcSclCrit,'SCALE')
    SELECT SCALE
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+lcSclCrit
      FOR lnK = 1 TO SCALE.CNT
        lcK = STR(lnK,1)
        lnCnt = lnCnt + 1
        IF lnCnt > 8
          lnCnt = 1
          M.NLINE = M.NLINE + 1
          INSERT INTO &lcCrtnLine FROM MEMVAR
        ENDIF
        lcCnt = STR(lnCnt,1)
        SELECT &lcCrtnLine
        REPLACE SZ&lcCnt    WITH SCALE.SZ&lcK ;
                SCALE&lcCnt WITH SCALE.SCALE
      ENDFOR
    ENDSCAN
          
  ENDIF
  
  *- Update added lines for the current style    
  SELECT &lcSHPCRTMF
  lcCCARTONNO = &lcSHPCRTMF..CCARTONNO
  SCAN REST WHILE CCARTONNO = lcCCARTONNO .AND. STYLE = lcStyClr
  
    *- Update "lcCrtnLine" such that sizes are saved continuously in one line
    *- << if there is a Style/Color Exists in the PO that has no qty's in SHPCRTMF , then add it
    *-    with zero qty's >>

    SELECT &lcCrtnLine
    lnRow = ASUBSCRIPT(laScl,ASCAN(laScl,&lcSHPCRTMF..SCALE),1)
    FOR lnK = 1 TO laScl[lnRow,10]
      =SEEK(lcCrtStyClr,lcCrtnLine)
      SKIP INT((laScl[lnRow,11]+lnK-1)/8)  && go to the line in lcCrtnLine where this group of sizes exist
      lnSz = laScl[lnRow,11]+lnK
      lcSz = STR(lnSz-(NLINE-1)*8,1)
      lcK  = STR(lnK,1)
      REPLACE QTY&lcSz    WITH &lcSHPCRTMF..QTY&lcK ;
              LINENO&lcSz WITH &lcSHPCRTMF..LINENO  ;
              TOTQTY      WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
    ENDFOR
    lnTotQty = lnTotQty + &lcSHPCRTMF..TOTQTY      

  ENDSCAN 
  
  *- skip rest lines of the same type , since it has the same structure, GET # OF CARTONS FOR THIS TYPE
  IF lcCrtNo <> &lcSHPCRTMF..CCARTONNO .AND. !EOF(lcShpCrtmf)
    SCAN REST WHILE CCARTONTYP = lcCartnTyp
      IF lcCrtNo <> &lcSHPCRTMF..CCARTONNO
        lnCrtnNo = lnCrtnNo + 1
        lcCrtNo = &lcSHPCRTMF..CCARTONNO
      ENDIF      
      lnTotQty = lnTotQty + &lcSHPCRTMF..TOTQTY      
    ENDSCAN
  ENDIF
  
  SELECT &lcCrtnHdr
  REPLACE CRTNNO WITH lnCrtnNo ,;
          TOTQTY WITH lnTotQty

  *- Initializing variables to collect data for a new style in the same carton , or this is a new type
  lcCrtNo  = &lcShpCrtMF..CCARTONNO 
  lnCrtnNo = 1
  lnTotQty = 0
  lnLn     = 0

  SELECT &lcSHPCRTMF
ENDDO



WAIT CLEAR
SELECT (lnSlct)

*-- end of lfClcShpDt.

*:**************************************************************************
*:* Name        : lfvWareCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 22/09/2004
*:* Purpose     : WH brwose function
*:***************************************************************************
*:* Called from : pocrlin.scx
*:***************************************************************************
*C123616,1
FUNCTION lfvWareCode
IF laScrMode[2] 
  lcWareHous = lcOldValue
  RETURN
ENDIF  

*B126937,1  TMI [Start] return if no changes
IF lcWareHous = lcOldValue
  RETURN
ENDIF
*B126937,1  TMI [END  ]
  
IF llBrowse 
  llBrowse = .F.
  lcOldValue = lcWareHous 
  lcWareHous = gfBrowWare(.F.)
ELSE  
  IF lcWareHous <> lcOldValue
    IF !SEEK(lcWareHous,'WAREHOUS')
      lcWareHous = gfBrowWare(.F.)
    ENDIF
    IF EMPTY(lcWareHous)
      lcWareHous = lcOldValue
    ENDIF
  ENDIF
ENDIF  

*B126937,1  TMI [Start] if the style is not assigned to the selcted warehouse , ask for this
PRIVATE lnSlct,lnRecno,llContinue,lcSty,lnI,lcI
lnSlct = SELECT()

SELECT &LCCRTNLINE
lnRecno = RECNO()

LOCATE 
llContinue = .T.
SCAN

  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    IF !EMPTY(SCALE&lcI)
    
      lcSty = SUBSTR(&lcCrtnLine..Style,1,lnClrPos+lnClrLen-1)+SCALE&lcI
    
      IF !SEEK(lcSty + PADR(lcWareHous,6) + SPACE(10) , 'STYDYE' )
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B34004','DIALOG',ALLTRIM(lcSty)+'|'+lcWareHous) = 1
          DO gpAdStyWar WITH lcSty,SPACE(10),lcWareHous
        ELSE
          llContinue = .F.
          EXIT
        ENDIF
      ENDIF
    ENDIF
  
  ENDFOR

  IF !llContinue
    EXIT
  ENDIF

ENDSCAN

IF !llContinue 
  lcWareHous = lcOldValue
ENDIF  
*B126937,1  TMI [End  ]  if the style is not assigned to the selcted warehouse , ask for this

SHOW GET lcWareHous

IF lcWareHous <> lcOldValue  
  llChanged = .T.
  SHOW GET pbLNew ENABLE
ENDIF

*-- end of lfvWareCode.

*:**************************************************************************
*:* Name        : lfvPO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 22/09/2004
*:* Purpose     : PO valid function
*:***************************************************************************
*:* Called from : pocrlin.scx
*:***************************************************************************
*C123616,1
FUNCTION lfvPO
PARAMETERS llCalOnly
PRIVATE lcPoNo,lcPoOrd,lnK,lcK,lcStyClr,lnSzCnt,lnLn,lcFlt,lnRecno

lnLn = 0

* lcPo field will be enabled only when in New mode
IF llBrowse OR ( !EMPTY(lcPO) AND !SEEK('P'+lcPO,'POSHDR') )
  lcPoNo = lcPo
  SELECT POSHDR
  lcFlt = FILTER()
  lnRecno = RECNO()
  IF llRcv
    IF lnType = 2
      SET FILTER TO cStyType = 'P' AND STATUS $ 'OH'
    ELSE
      IF !EMPTY(lcTCode)
        SET FILTER TO PO=lcTCode .AND. 'P' AND STATUS = 'O'
      ENDIF
    ENDIF
  ENDIF
  GO TOP
  DO lfPOSBrow WITH lcPoNo,'        ','P'
  SET FILTER TO &lcFlt    && restore old POSHDR filter if any
  GO TOP
  IF BETWEEN(lnRecno,1,RECCOUNT('POSHDR'))
    GOTO (lnRecno)
  ENDIF
  lcPo = lcPoNo
  llBrowse = .F.
  IF EMPTY(lcPO)
    _CUROBJ = OBJNUM(lcPO)
  ENDIF
ENDIF

*- Get PO warehous
IF !llCalOnly
  =SEEK('P'+lcPO,'POSHDR')
  lcWareHous = POSHDR.CWARECODE 
ENDIF  

*- clear filter from cartons detail file if any
SELECT (lcCrtnLine)
SET FILTER TO 
GO TOP

IF EMPTY(lcPO)
  _CUROBJ = OBJNUM(lcWareHous)
  RETURN
ENDIF

lcPoOrd = ORDER('POSLN')

IF !EMPTY(lcPo)
  SHOW GET lcPO  
  SET ORDER TO TAG POSLN IN POSLN
  =SEEK('P'+lcPO,'POSLN')
  SELECT POSLN
  
  DIMENSION laScl[1,11]
  laScl = '   '
  
  SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = 'P'+lcPO
    lnLn = 0
    *- Define array to save scales
    IF LEFT(laScl[1,9],lnExtSzWd) <> LEFT(POSLN.SCALE,lnExtSzWd)
      lcSclCrit = SUBSTR(POSLN.SCALE,1,lnExtSzWd)
      SELECT SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SCALE,CNT,0 ;
         FROM SCALE ;
         WHERE SCALE.TYPE+SCALE.SCALE = 'S'+lcSclCrit ;
         INTO ARRAY laScl
      lnSzCnt = 0
      FOR lnK = 1 TO ALEN(laScl,1)
        laScl[lnK,11] = lnSzCnt  && This scale group starts with This No + 1
        lnSzCnt = lnSzCnt + laScl[lnK,10]
      ENDFOR
    ENDIF
    
    lcStyClr = SUBSTR(POSLN.STYLE,1,lnClrPos+lnClrLen-1)
    IF !SEEK(lcCartnTyp+lcStyClr,lcCrtnLine)
      SELECT &lcCrtnLine
      SCATTER MEMVAR BLANK     && empty memory variables to update needed fields only
      APPEND BLANK
      REPLACE CCARTONTYP WITH lcCartnTyp  ;
              CSTYMAJOR  WITH SUBSTR(POSLN.STYLE,1,lnStyleWid) ;
              CSHIPNO    WITH lcCodeNo   ; 
              PO         WITH POSLN.PO    ;
              STYLE      WITH lcStyClr    ;
              NLINE      WITH 1           

      *- take a copy of fields in memory variables
      SCATTER MEMVAR 
      
      *- Add more lines for the current scale,let all scales be added so that user can scroll to 
      *- see more sizes
      lnCnt = 0
      =SEEK('S'+lcSclCrit,'SCALE')
      SELECT SCALE
      SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+lcSclCrit
        FOR lnK = 1 TO SCALE.CNT
          lcK = STR(lnK,1)
          lnCnt = lnCnt + 1
          IF lnCnt > 8
            lnCnt = 1
            M.NLINE = M.NLINE + 1  
            INSERT INTO &lcCrtnLine FROM MEMVAR  
          ENDIF
          lcCnt = STR(lnCnt,1)
          SELECT &lcCrtnLine
          REPLACE SZ&lcCnt     WITH SCALE.SZ&lcK ;
                  SCALE&lcCnt  WITH SCALE.SCALE  
        ENDFOR
      ENDSCAN      
    ENDIF
  
    *- Get line # from POSLN file
    SELECT &lcCrtnLine
    lnRow = ASUBSCRIPT(laScl,ASCAN(laScl,POSLN.SCALE),1)
    FOR lnK = 1 TO laScl[lnRow,10]
      =SEEK(lcCartnTyp+lcStyClr,lcCrtnLine)
      SKIP INT((laScl[lnRow,11]+lnK-1)/8)     && go to the line in lcCrtnLine where this group of sizes exist
      lnSz = laScl[lnRow,11]+lnK
      lcSz = STR(lnSz-(NLINE-1)*8,1)
      REPLACE LINENO&lcSz WITH POSLN.LINENO
    ENDFOR
  ENDSCAN
ENDIF

*- Get maximum possible lines for a style color
SELECT (lcCrtnLine)
lnMaxLns = 0
SCAN FOR cCartonTyp = lcCartnTyp
  lnMaxLns = MAX(lnMaxLns,NLINE)
ENDSCAN
lcPrvNxt = IIF(lnMaxLns<2,'DISABLE','ENABLE')
SHOW GET pbNx &lcPrvNxt


SELECT &lcCrtnLine
SET FILTER TO CCARTONTYP=lcCartnTyp .AND. NLINE = 1
GO TOP
  
SET ORDER TO TAG (lcPoOrd) IN POSLN

*- Refresh browse screen and size descriptions screen
IF !llCalOnly
  =lfwBrwCrtLn()
ENDIF  

IF !EMPTY(lcPo)
  SHOW GET lcPo DISABLE
  SHOW GET ibPo DISABLE
ENDIF

*- set focus to WH field
_CUROBJ = OBJNUM(lcWareHous)

*-- end of lfvPO.

*:**************************************************************************
*:* Name        : lfCallFrm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 19/09/2004
*:* Purpose     : Get calling program
*:***************************************************************************
*C123616,1
FUNCTION lfCallFrm
PARAMETERS lcPrg
PRIVATE lnI
lnI = 1
DO WHILE !EMPTY(SYS(16,lnI))
  IF lcPrg $ SYS(16,lnI)
    RETURN .T.
  ENDIF
  lnI = lnI + 1
ENDDO
RETURN .F.
*-- end of lfCallFrm.

*:**************************************************************************
*:* Name        : lfvUpdCtn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 27/09/2004
*! Purpose      : Valid functon of the OK  Button ( Carton Details Screen ) 
*:***************************************************************************
*C123616,1
FUNCTION lfvUpdCtn
PRIVATE lnAlias , lnTotqty , lnRecNo

IF EMPTY(lcPO)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No PO is selected , can not accept')
  _CUROBJ = OBJNUM(lcPO)
  RETURN .F.
ENDIF
IF lnTotPerCt = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No quantities added, can not accept.')
  _CUROBJ = OBJNUM(M.QTY1)
  RETURN .F.
ENDIF

lnAlias = ALIAS()

SELECT (lcCrtnLine)
SET FILTER TO 
GO TOP

llChanged = .F.
SELECT (lcCrtnHdr)
IF llNewCrt
  APPEND BLANK
  REPLACE cCartonTyp WITH lcCartnTyp
ENDIF
REPLACE PO         WITH lcPO       ;
        CWARECODE  WITH lcWareHous ;
        weight     WITH lnWeight   ;
        nlength    WITH lnLength   ;
        ndepth     WITH lnDepth    ;
        width      WITH ALLTRIM(STR(lnWidth)) ;
        CrtnNo     WITH lnNofCrtn  ;
        TotQty     WITH lnTotPerCt ;
        CUPDATE    WITH 'N'
SELECT (lnAlias)
CLEAR READ

*:**************************************************************************
*:* Name        : lfvCanCtn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 23/09/2004
*:* Purpose     : Cancel data for entered carton
*:***************************************************************************
*:* Called from : pocrlin.scx
*:***************************************************************************
*C123616,1
FUNCTION lfvCanCtn
PRIVATE lnAlias , lnTotqty , lnRecNo
lnAlias = ALIAS()
lnTotQty = 0

IF !laScrMode[2] .AND. llChanged
  IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,'Are you sure to ignore changes') = 1  
    SELECT (lcCrtnLine)
    IF SEEK(lcCartnTyp,lcCrtnLine)

      lnRecNo = RECNO()
      IF llNewCrt
        DELETE FOR cCartonTyp = lcCartnTyp
      ELSE
        SCAN REST WHILE Style+cCartonType = m.Style+lcCartnTyp
          FOR lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            REPLACE Qty&lcCount WITH OldQty&lcCount
          ENDFOR
          REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        ENDSCAN
      ENDIF
      IF BETWEEN(lnRecNo,1,RECCOUNT())
        GO lnRecNo
      ENDIF
      SELECT (lnAlias)
    ENDIF
    CLEAR READ  
  
  ENDIF
  
ELSE
  
  CLEAR READ  

ENDIF  
*-- end of lfvCanCtn.

*:**************************************************************************
*:* Name        : lfDefKey
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 26/09/2004
*:* Purpose     : Define up and dn keys for qty fields
*                 Called in the when event of Qty fields
*:***************************************************************************
*C123616,1
FUNCTION lfDefKey
=lfOldValue()
ON KEY LABEL UPARROW DO lfUpDnArw WITH 'U'
ON KEY LABEL DNARROW DO lfUpDnArw WITH 'D'
*-- end of lfDefKey.

*:**************************************************************************
*:* Name        : lfUpDnArw
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 26/09/2004
*:* Purpose     : Skip line up if Uparrow pressed, line down if downarrow is pressed
*:***************************************************************************
*C123616,1
FUNCTION lfUpDnArw
PARAMETERS  lcUpDn
DO CASE
CASE lcUpDn = 'U'
  IF !BOF()
    SKIP -1
  ENDIF
CASE lcUpDn = 'D'
  IF !EOF()
    SKIP
  ENDIF
ENDCASE

*- Refresh browse screen and size descriptions screen
=lfwBrwCrtLn()

*-- end of lfUpDnArw.

*:**************************************************************************
*:* Name        : lfvSzQty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 23/09/2004
*! Purpose      : validate the size qty ( Carton Details Screen ) 
*:***************************************************************************
*C123616,1
FUNCTION lfvSzQty
PARAM lcSize
PRIVATE lnAlias ,lnSizeQty ,lcPOStat,lcSty ,; 
        lcCurSty,lcSvOrd

* In this function I create the quantities that will be actually recieved , no as "lfvSizeQty" , where it 
* works with the shipped or recived qty in the temp line and can not modify them

lcSize = ALLTRIM(lcSize)

IF EMPTY(lcPO)
  m.Qty&lcSize = lcOldValue
ENDIF

lnAlias   = ALIAS()
IF m.Qty&lcSize < 0 
  *- Negative values are not allowed.
  = gfModalGen('TRM42000B40011','DIALOG')
  m.Qty&lcSize = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*- If view mode , no updates will occure
IF laScrMode[2]
  m.Qty&lcSize = lcOldValue
  RETURN
ENDIF

*- if this style/color/scale is not cited in the PO , then do not add it
lcSty = SUBSTR(m.Style,1,lnClrPos+lnClrLen-1)+SCALE&lcSize

IF m.Qty&lcSize <> lcOldValue 
  lcSvOrd = ORDER('POSLN')
  SET ORDER TO POSLN IN POSLN
  llIncluede = SEEK('P'+lcPO+lcSty,'POSLN')
  SET ORDER TO (lcSvOrd) IN POSLN 
  IF !llIncluede
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This size scale is not included in the PO, can not accept!')
    m.Qty&lcSize = 0
    RETURN
  ENDIF
ENDIF

*- check if there is change in qty's .
IF m.Qty&lcSize <> lcOldValue
  llChanged = .T.
  SHOW GET pbLNew ENABLE
ELSE
  RETURN  
ENDIF
  
lnSizeQty = 0
lcCurSty = &lcCrtnLine..STYLE
SELECT (lcCrtnLine)
REPLACE QTY&lcSize WITH m.QTY&lcSize ;
        TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
m.TotQty = TotQty

lnTotPerCt = 0
lnStyTot   = 0

*- Get the sum
lnRecno = RECNO()
lcFlt = FILTER()
SET FILTER TO 
GO TOP

SUM TOTQTY TO lnTotPerCt FOR cCartonTyp = lcCartnTyp
GO TOP

SUM TOTQTY TO lnStyTot FOR  cCartonTyp+STYLE = lcCartnTyp + lcCurSty
GO TOP

REPLACE STYTOT WITH lnStyTot FOR cCartonTyp+STYLE = lcCartnTyp + lcCurSty
SET FILTER TO &lcFlt
GO TOP

IF BETWEEN(lnRecno,1,RECCOUNT(lcCrtnLine))
  GOTO (lnRecno)
ENDIF

SHOW GET lnTotPerCt
lcPOStat = IIF(lnTotPerCt=0,'ENABLE','DISABLE')
SHOW GET lcPo &lcPOStat
SHOW GET ibPo &lcPOStat

=lfwBrwCrtLn()

*- Release definitions of Up / Dn Arrows
ON KEY LABEL UPARROW 
ON KEY LABEL DNARROW 

SELECT (lnAlias)
RETURN

*:**************************************************************************
*:* Name        : lfGetClrDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 23/09/2004
*:* Purpose     : Get Color data
*:***************************************************************************
*C123616,1
FUNCTION lfGetClrDt
PRIVATE laItemSeg,lnCount
DIMENSION laItemSeg[1,1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg , 1)
  IF laItemSeg[lnCount,1] = 'C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrPos   = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*-- end of lfGetClrDt.

*:**************************************************************************
*:* Name        : lfvSzScrl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 23/09/2004
*:* Purpose     : Scroll for more sizes
*:***************************************************************************
*:* Called from : pocrtlin.scx
*:***************************************************************************
*C123616,1
FUNCTION lfvSzScrl
PARAMETERS lnScroll,llFlag
PRIVATE lcCurrSty,lnGotoRec
SELECT &lcCrtnLine
lnCurLin = &lcCrtnLine..NLINE + lnScroll
lcCurrSty = &lcCrtnLine..CCARTONTYP + &lcCrtnLine..STYLE

*- Remove filter ,and get sum for current type by style
SET FILTER TO 
DIMENSION laStySum[1,2]
laStySum = ' '
SELECT STYLE,SUM(TOTQTY) ;
   FROM &lcCrtnLine ;
   WHERE cCartonTyp = lcCartnTyp ;
   GROUP BY STYLE ;
   INTO ARRAY laStySum
=SEEK(lcCartnTyp,lcCrtnLine)
SELECT &lcCrtnLine
SCAN REST WHILE cCartonTyp+STYLE = lcCartnTyp
  lnPos = ASCAN(laStySum,STYLE)
  REPLACE STYTOT WITH laStySum[lnPos+1]
ENDSCAN   

SET FILTER TO CCARTONTYP=lcCartnTyp .AND. NLINE = lnCurLin 
GO TOP

SHOW GET pbPr ENABLE
SHOW GET pbNx ENABLE
DO CASE
CASE lnCurLin = 1
  SHOW GET pbPr DISABLE
  ON KEY LABEL CTRL+LEFTARROW 
  ON KEY LABEL CTRL+RIGHTARROW DO lfvSzScrl WITH 1

CASE lnCurLin = lnMaxLns
  SHOW GET pbNx DISABLE
  ON KEY LABEL CTRL+LEFTARROW DO lfvSzScrl WITH -1
  ON KEY LABEL CTRL+RIGHTARROW 

ENDCASE

lcQty1 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+1))
lcQty2 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+2))
lcQty3 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+3))
lcQty4 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+4))
lcQty5 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+5))
lcQty6 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+6))
lcQty7 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+7))
lcQty8 = 'Qty'+ALLTRIM(STR((lnCurLin-1)*8+8))
lcCrtBrFld = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "STYLE  :H='Style/Color' :16,"+;
             "Qty1   :R :7 :H='&lcQty1',"+;
             "Qty2   :R :7 :H='&lcQty2',"+;
             "Qty3   :R :7 :H='&lcQty3',"+;
             "Qty4   :R :7 :H='&lcQty4',"+;
             "Qty5   :R :7 :H='&lcQty5',"+;
             "Qty6   :R :7 :H='&lcQty6',"+;
             "Qty7   :R :7 :H='&lcQty7',"+;
             "Qty8   :R :7 :H='&lcQty8',"+;
             "STYTot :R :H='Total Qty' :9"
IF !SEEK(lcCurrSty,lcCrtnLine)
  GO TOP
ENDIF
lnGotoRec = RECNO(lcCrtnLine)

IF !llFlag
  =lfCrtLnBrw()
  =lfwBrwCrtLn(lnGotoRec)

  _CUROBJ = OBJNUM(m.Qty1)
ENDIF  

*-- end of lfvSzScrl.
*:**************************************************************************
*:* Name        : lfOldValue
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/23/2004
*:* Purpose     : Save old value of a field
*:***************************************************************************
*C123616,1
FUNCTION lfOldValue
lcOldValue = EVAL(SYS(18))
*-- end of lfOldValue.


*:**************************************************************************
*:* Name        : lfvUpd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 30/09/2004
*:* Purpose     : Update variable function
*:***************************************************************************
*C123616,1
FUNCTION lfvUpd
PRIVATE lcVar
lcVar = SYS(18)

IF laScrMode[2]
  &lcVar = lcOldValue
  RETURN
ENDIF  

IF &lcVar <> lcOldValue  
  llChanged = .T.
  SHOW GET pbLNew ENABLE
ENDIF

*-- end of lfvUpd.

*!**************************************************************************
*! Name      : lfLOtCrtBrw
*! Developer : Walid A. Wahab  (WAB)
*! Date      : 03/12/2003
*! Purpose   : Display the browse screen  ( Carton Details Screen ) 
*! Reference : C200488,1
*!**************************************************************************
*! Example   : lfLOtCrtBrw()
*!**************************************************************************
*C123616,1
FUNCTION lfCrtLnBrw
PARAMETERS lnGoToRec
SELECT(lcUsedFile)
IF !EMPTY(lnGoToRec) .AND. TYPE('lnGoToRec') = 'N'
  GOTO (lnGoToRec)
ELSE  
  GO TOP
ENDIF

lnNewRec = RECNO(lcUsedFile)
BROWSE FIELDS &lcCrtBrFld FOR cCartonTyp = lcCartnTyp ; 
       LOCK 0             ;
       SAVE               ;
       NOEDIT             ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwCrtLn();
       VALID :F lfvBrwcRt();
       TITLE lcCrtTitl   ;
       WINDOW (lcCrtDet2) IN WINDOW (lcCrtDet)
=lfwBrwCrtLn()

*:**************************************************************************
*:* Name        : lfwBrwCrtLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 25/09/2004
*:* Purpose     : Browse carton lines
*:***************************************************************************
*C123616,1
FUNCTION lfwBrwCrtLn
PARAMETERS lnRecno
PRIVATE lcFields,lcCurrSty
lcFields = "Style,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,"        +;
           "Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,STYTot"
           
SELECT (lcUsedFile)
IF !EMPTY(lnRecno)
  GOTO (lnRecno)
ELSE  
  IF EOF() OR BOF()
    GO TOP
  ENDIF  
ENDIF

*- Always show the first set of sizes
*=IIF( STR(LASTKEY(),2)$' 5|24' .AND. !'1'$FILTER() , lfvSzScrl(-1,.T.) , '' )

lnBrRecNo = RECNO()
SCATTER FIELDS &lcFields MEMVAR
FOR lnCount = 1 To 8
  lcCount = ALLTRIM(STR(lncount))
  SHOW GET m.qty&lcCount
  SHOW GET m.SZ&lcCount
ENDFOR
lcClrDesc = gfCodDes(PADR(SUBSTR(m.Style,lnClrPos,lnClrLen),6),'COLOR')
SHOW GET lcClrDesc

SHOW WINDOW (lcCrtTitl) REFRESH SAME
=lfRefresh()
*-- end of lfwBrwCrtLn.

*:**************************************************************************
*:* Name        : lfTypsBrw
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/10/2004
*:* Purpose     : Carton types browse function
*:***************************************************************************
*C123616,1
FUNCTION lfTypsBrw
SELECT(lcFileToUse)
GO TOP
lnNewRec = RECNO(lcFileToUse)
BROWSE FIELDS &lcRlBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOEDIT             ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwTypsBrw();
       VALID :F lfvBrwRo();
       TITLE (lcRollTitl)   ;
       WINDOW (lcLotRo2) IN WINDOW (lcLotRo)
= lfwTypsBrw()

*-- end of lfTypsBrw.

*:**************************************************************************
*:* Name        : lfwTypsBrw
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/10/2004
*:* Purpose     : When types browse function
*:***************************************************************************
*C123616,1
FUNCTION lfwTypsBrw
SELECT (lcFileToUse)
lnBrRecNo = RECNO()
SUM CrtnNo TO lnTotCrtNo
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF
SHOW WINDOW (lcRollTitl) REFRESH SAME
IF lnTotCrtNo < 1 
  SHOW GET pbRModify DISABLE 
  SHOW GET pbRRemove DISABLE
ELSE
  SHOW GET pbRModify ENABLE
  SHOW GET pbRRemove ENABLE
ENDIF
=lfRefresh(lcCrtTitl)

*:**************************************************************************
*:* Name        : lfvCartNO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 25/09/2004
*:* Purpose     : Valid function for carton No.
*:***************************************************************************
*C123616,1
FUNCTION lfvCartNO
PRIVATE lnRecNo , lnAlias
lnAlias = SELECT()
IF lnNOfCrtn < 1
  = gfModalGen('TRM42000B40011','DIALOG')
  lnNOfCrtn = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
SHOW GET lnNOfCrtn DISABLE
lcCrtStat = "DISABLE"
SELECT (lcCrtnLine)
lnRecNo = RECNO()
REPLACE ALL CrtnNo WITH lnNOfCrtn FOR cCartonTyp = lcCartnTyp
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
_CUROBJ = OBJNUM(lnWeight)
SELECT (lnAlias)
*-- end of lfvCartNO.
*:**************************************************************************
*:* Name        : lfSHPWRN
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/21/2004
*:* Purpose     : Show a warning message if there is a difference between 
*                 totqty from cartons and totqty from main shipment screen
*:***************************************************************************
*:* Called from : lpsavscr in poshp.prg
*:***************************************************************************
*C123616,1
FUNCTION lfSHPWRN
PARAMETERS lnShpQty
PRIVATE lnTotCtnQ,lnResp,llRcv
*- if cartons screen not opened , open its files

llRcv = lfCallFrm('POSTREC.FXP')

IF llRcv
  DO CASE
  CASE lnType = 1
    lcCodeNo = lcTCode 
  CASE lnType = 2
    lcCodeNo = lcShpCode 
  ENDCASE
ELSE
  lcCodeNo = laData[1]
ENDIF  

IF !USED('TempName')
  =lfCrTmpFls()
ENDIF
SELECT ('TempName')
SCAN
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

lnTotCtnQ = 0
SELECT &lcShpCrtMf
SUM TOTQTY TO lnTotCtnQ 

lnResp = 1
IF lnTotCtnQ <> lnShpQty
  lnResp = gfModalGen('TRM00000B34001',.F.,.F.,.F.,'Shipped qty is different than cartoned qty, Proceed ?')
  IF lnResp <> 1
    STORE .F. TO llShow,llCSave
  ENDIF
ENDIF

RETURN lnResp = 1
*-- end of lfSHPWRN.

*:**************************************************************************
*:* Name        : lfSvShp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 27/09/2004
*:* Purpose     : Save data of file lcShpCrtMf to ShpCrtMf
*:***************************************************************************
*:* Called from : poshp.prg
*:***************************************************************************
*C123616,1
FUNCTION lfSvShp
PRIVATE lnSlct,lcNewCrtNo,lcTempName,lcTmpCur,llRcv,lcSavTo,lcSpTpCr
lnSlct = SELECT()

*B125800,1 NNA 01/06/2005 (Begin) add three variables to use them at the Sequence Function,and
*B125800,1 NNA            a fourth for Carton no. to use it in a waiting window
STORE .F. TO llGetBefor
STORE ''  TO lcUnqPreFx
STORE 'N' TO llDivOnSeq
STORE 0 TO lnCrtnNo
*B125800,1 NNA (End)

llRcv = lfCallFrm('POSTREC.FXP')
lcSavTo = IIF(llRcv,'POCRTNMF','SHPCRTMF')

IF llRcv

  IF !llCSave  && If cancel is pressed , do not update carton file
    RETURN
  ENDIF
  
  DO CASE
  CASE lnType = 1
    lcCodeNo = lcTCode 
  CASE lnType = 2
    lcCodeNo = lcShpCode 
  ENDCASE
ELSE
  lcCodeNo = laData[1]
ENDIF  

*- if cartons screen not opened , open its files
IF !USED('TempName')
  =lfCrTmpFls()
ENDIF
SELECT ('TempName')
SCAN
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

IF RECCOUNT(lcShpcrtmf) > 0

  *- Save record # to delete them later
  DIMENSION laRecno[1]
  laRecno[1] = 0
  lnDim = 0
  IF SEEK(lcCodeNo,'SHPCRTMF')
    SELECT SHPCRTMF  
    SCAN REST WHILE CSHIPNO+CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE = lcCodeNo
      lnDim = lnDim + 1
      DIMENSION laRecno[lnDim]
      laRecno[lnDim] = RECNO()
    ENDSCAN 
  ENDIF
 
  IF llRcv
    SET ORDER TO TAG POCRTNMFTP IN POCRTNMF
    SET ORDER TO TAG SHPCRTMF   IN SHPCRTMF
  ELSE
    SET ORDER TO TAG SHPCRTMFTP IN SHPCRTMF
  ENDIF
  
  *- Update lcShpCrtMf temp file , and finally copy it to the shpcrtmd actual file
  SELECT &lcShpCrtMF
  GO TOP

  DO WHILE !EOF(lcShpcrtmf)
    lcCCARTONTYP = CCARTONTYP
    lcCCARTONNO  = CCARTONNO
    IF 'S' $ &lcShpCrtMf..CCARTONTYP

      *B125800,1 NNA 01/06/2005 (Begin) get Sequence from a Local Function instead of a global
      *B125800,1 NNA            function to Speedup processing 
      lcUpdType = lfSEQUENCE('cCartonTyp')
      *B125800,1 NNA (End)

    ELSE
      lcUpdType = &lcShpCrtMf..CCARTONTYP
    ENDIF
    SELECT &lcShpCrtMf      
    DO WHILE CCARTONTYP = lcCCARTONTYP .AND. !EOF(lcShpCrtMf)
      lcCCARTONNO  = CCARTONNO
      
      *B125800,1 NNA 01/06/2005 (Begin) get Sequence from a Local Function instead of a global
      *B125800,1 NNA            function to Speedup processing and Show waiting window with counter
      *B125800,1 NNA            for the processing carton No. to make a user be aware and interested
      *lcUpdCrtNo = IIF( !'S' $ lcCCARTONNO , lcCCARTONNO , gfSequence('CCARTONNO') )
      lcUpdCrtNo = IIF( !'S' $ lcCCARTONNO , lcCCARTONNO , lfSequence('CCARTONNO') )
      lnCrtnNo = lnCrtnNo+1
      WAIT WINDOW NOWAIT "Saving data for Carton No. : " + ALLTRIM(STR(lnCrtnNo))
      *B125800,1 NNA (End)
      
      SCAN REST WHILE CCARTONNO = lcCCARTONNO
        SCATTER MEMVAR
        M.CCARTONTYP = lcUpdType 
        M.CCARTONNO  = lcUpdCrtNo
        M.CSHIPNO = IIF(llRcv,IIF(lnType=1,'      ',M.CSHIPNO),laData[1])
        IF llRcv
          IF SEEK('P'+&lcShpCrtMf..PO,'POSHDR') .AND. POSHDR.STATUS $ 'CO'  && check that the PO is not hold
            INSERT INTO POCRTNMF FROM MEMVAR
            lcSpTpCr = CSHIPNO+CCARTONTYP+CCARTONNO   && Ship#/Type/Carton#
            IF SEEK(lcSpTpCr,'SHPCRTMF')
              SELECT SHPCRTMF
              REPLACE LRECEIVED WITH .T. ;
                 REST WHILE CSHIPNO+CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE = lcSpTpCr                
            ENDIF
          ENDIF
        ELSE
          INSERT INTO SHPCRTMF FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDDO
  ENDDO
  
  *- Delete records saved previously of the current ship
  SELECT SHPCRTMF
  FOR lnK = 1 TO ALEN(laRecno)
    IF !llRcv .AND. laRecno[lnK] > 0
      GO (laRecno[lnK])
      DELETE 
      BLANK 
    ENDIF
  ENDFOR
  SELECT SHPCRTMF
  SET ORDER TO TAG SHPCRTMF
ENDIF
SELECT (lnSlct)
*-- end of lfSvShp.

*:**************************************************************************
*:* Name        : lfRMVSHP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/18/2004
*:* Purpose     : Remove shipment from shpcrtmf custom file for nik nak
*:***************************************************************************
*:* Called from : lpDelScr IN poshp.prg
*:***************************************************************************
*C123616,1
FUNCTION lfRMVSHP
PRIVATE lnSlct
lnSlct = SELECT()
IF !USED('SHPCRTMF')
  =gfOpenFile(gcDataDir+'SHPCRTMF','SHPCRTMF','SH')
ENDIF
IF SEEK(laData[1],'SHPCRTMF')
  SELECT SHPCRTMF
  DELETE REST WHILE CSHIPNO+CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE = laData[1] ;
              FOR !LRECEIVED 
ENDIF
SELECT (lnSlct)
*-- end of lfRMVSHP.

*:**************************************************************************
*:* Name        : lfEMPTMP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 29/09/2004
*:* Purpose     : empty temp files for nik nak
*:***************************************************************************
*:* Called from : poshp.prg ( lpshow )
*:***************************************************************************
*C123616,1
FUNCTION lfEMPTMP
PRIVATE lnSlct,lcTempName

lnSlct = SELECT()
IF laScrMode[1] .OR. laScrMode[2]
**  lcTempName = '_'+SUBSTR(lcWinCh0,2)
  IF USED('TempName')
    SELECT ('TempName')
    SCAN
      RESTORE FROM Memo cTempName ADDITIVE
    ENDSCAN
    
    IF TYPE('lcCrtnHdr')='C' .AND. USED(lcCrtnHdr)
      SELECT &lcCrtnHdr 
      ZAP
    ENDIF
    IF TYPE('lcCrtnLine')='C' .AND. USED(lcCrtnLine)
      SELECT &lcCrtnLine 
      ZAP
    ENDIF
    IF TYPE('lcShpCrtMf')='C' .AND. USED(lcShpCrtMf)
      SELECT &lcShpCrtMf
      ZAP
    ENDIF
    USE IN TempName
  ENDIF
ENDIF  
SELECT (lnSlct)
*-- end of lfEMPTMP.


*:**************************************************************************
*:* Name        : lfCrtnTrap
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 30/09/2004
*:* Purpose     : activated when closing carton details screens
*:***************************************************************************
*C123616,1
FUNCTION lfCrtnTrap
*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcCrtTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrCrtTb
  ON KEY LABEL BACKTAB DO lfBrCrtBb
*  ON KEY LABEL ESCAPE  DO lfCrtEsc
  ON KEY LABEL ESCAPE
ENDIF

*-- end of lfCrtnTrap.

*:**************************************************************************
*:* Name        : lfCrtEsc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 30/09/2004
*:* Purpose     : Escape fn
*:***************************************************************************
*C123616,1
FUNCTION lfCrtEsc
ON KEY LABE ESCAPE
=lfvCanCtn()
*-- end of lfCrtEsc.

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfBrCrtTb
ON KEY LABE TAB
ACTIVATE WINDOW (lcCrtDet3)
_CUROBJ = OBJNUM(m.Qty1)

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfBrCrtBb
ON KEY LABE BACKTAB	
ACTIVATE WINDOW (lcCrtDet1)
_CUROBJ = OBJNUM(lnDepth)

*:**************************************************************************
*:* Name        : lfClrTrap2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/17/2004
*! Purpose   : Clear Trap Function of the Browse window ( Carton Types Screen )
*:***************************************************************************
*C123616,1
FUNCTION lfClrTrap2

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =lfwTypsBrw()
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*ON KEY LABEL ESCAPE  DO lfEscape
ON KEY LABEL ESCAPE llDummy = .T.

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfTrap2

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key
IF WONTOP()  = lcRollTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab2
  ON KEY LABEL BACKTAB DO lfBrBack2
  ON KEY LABEL ESCAPE llDummy = .T.
ENDIF

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfBrTab2
ON KEY LABE TAB
ACTIVATE WINDOW (lcLotRo3)
_CUROBJ = OBJNUM(pbRNew)

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfBrBack2
ON KEY LABE BACKTAB	
ACTIVATE WINDOW (lcLotRo3)
_CUROBJ = OBJNUM(pbRClose)

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfEscape2

ON KEY LABE ESCAPE
=lfvCrtOk()


*:**************************************************************************
*:* Name        : lfREFBRW    
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/24/2004
*:* Purpose     : refresh main browse screen for shipment screen
*:***************************************************************************
*:* Called from : lfUnTrap IN poshp.prg
*:***************************************************************************
*C123616,1
FUNCTION lfREFBRW    
IF FILE(gcWorkDir+'TEMPNAME.TMP')
  DO CASE
  CASE lfCallFrm('POSHP.FXP')
    =lfwBrow1()
  CASE lfCallFrm('POSTREC.FXP')
    SHOW WINDOW (lcBrDtTtl) REFRESH
  ENDCASE
  ERASE (gcWorkDir+'TEMPNAME.TMP')
ENDIF  
*-- end of lfREFBRW    .
*:**************************************************************************
*:* Name        : lfRefTyp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/17/2004
*:* Purpose     : refresh type screen
*:***************************************************************************
*C123616,1
FUNCTION lfRefTyp
IF WONTOP() = lcRollTitl
  =lfwTypsBrw()
ENDIF
*-- end of lfRefTyp.

*:**************************************************************************
*:* Name        : lfvCrtCnt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 30/09/2004
*:* Purpose     : Carton Count 
*:***************************************************************************
*C123616,1
FUNCTION lfvCrtCnt
lnAlias = SELECT()

IF laScrMode[2]
  PRIVATE lcVar
  lcVar = SYS(18)
  &lcVar = lcOldValue
  RETURN
ENDIF  

IF lnNOfCrtn < 1
  = gfModalGen('TRM42000B40011','DIALOG')
  lnNOfCrtn = lcOldValue
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*- check if there is change in qty's .
IF lnNOfCrtn <> lcOldValue
  llChanged = .T.  
  SHOW GET pbLNew ENABLE
ELSE
  RETURN  
ENDIF

lcCrtStat = "DISABLE"
SELECT (lcCrtnLine)
lnRecNo = RECNO()
REPLACE ALL CrtnNo WITH lnNOfCrtn FOR cCartonTyp = lcCartnTyp
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
_CUROBJ = OBJNUM(lnWeight)
SELECT (lnAlias)

*-- end of lfvCrtCnt.


*:**************************************************************************
*:* Name        : lfPOSBROW
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/10/2004
*:* Purpose     : Browse pos
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPOSBROW()
*:***************************************************************************
PROCEDURE lfPOSBROW
PARAMETER lcPO,lcVEN,lcAType
PRIVATE lcBrFields,lcAlias,lcVenCode,laPData
DECLARE laPData[2] && array to get values from browse

laPData  = ' '

lcBrFields =   [PO        :R :H='P/O ' +' #':8,]+;
  	           [Status    :R :H='S':2,]+;
               [Vendor    :R :H='Vendor':11,]+;
	           [Entered   :R :H='Entered':10,]+;
	           [Complete  :R :H='Complete':10,]+;
  	           [nStyOrder :R :H='Tot.Qty.':7,]+;
               [POTotal   :R :H='Amount':10,]+;
               [Receive   :R :H='Receive':7,]+;
               [Open      :R :H='Open':7]

lcPO  = SPACE(6) 
IF gfBrows('',"Po","laPData",'Style Purchase Orders')
  lcPO  = laPData[1]
ENDIF


*:***************************************************************************
*:* Name        : lfPikCrt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/10/2004
*:* Purpose     : Pick cartons for Nik Nak
*:***************************************************************************
*:* Called from : alordal.prg
*:***************************************************************************
*C123616,1
FUNCTION lfPikCrt
PRIVATE lcWinALCR5,lcWinALCR6,lcWinALCR7,lcWinALCR8,lcChck,lcUnChck,lcAlTitl,;
        lcOrder,lcAccount,lnTotCrton,lnTotPickd

PRIVATE lcFileToUse,llBrowse ,lnAlias

SELECT TempName
SCAN 
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

llBrowse = .F.
lnAlias = SELECT()
lnRecNo = RECNO()
lcFileToUse=lcCrtnDet
lnBrRecNo = RECNO(lcFileToUse)
STORE SPACE(0) TO lcWinALCR5,lcWinALCR6,lcWinALCR7,lcWinALCR8
lnTotCrton = 0
lnTotPickd = 0
lcOrder    = ''
lcAccount  = ''
lcOldValue = ' '
lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)
STORE 0 TO lnOldVal

lcAlTitl = 'Alocation By Cartons'

lcALCRT  = gfTempName()
lcALCRT1 = gfTempName()
lcALCRT2 = gfTempName()
lcALCRT3 = gfTempName()

PRIVATE lcALCRTitl,lcALBrFild

ReadSt = IIF(laScrMode[2],':R','')
lcALBrFild = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "cCartonTyp :W=.F. :R :H='Carton Type':14,"+;
             "TotQty :R  :W=.F. :H='Carton Qty' : 13,      "+;
             "nCrtNo :R  :W=.F. :H='Cartons Available' :17,"+;
             "nAlQty     :H='Cartons Allocated' :17 &ReadSt :V=lfvAlQty(),"+;
             "nPieces = TotQty * nAlQty  :W=.F. :R :H='Total Pieces' :12"
lcALCRTitl = 'Cartons Types'

*- Initialize "lnTotPickd" variable
lnTotPickd = 0

*- Collect types that are suitable for this order
=lfAlClcTyp()

*- Get # of cartons for selected types
lnTotCrtqt = lfChkCarton()

IF lnTotCrtqt > 0 
  *-- Call the screen
  PUSH KEY
  ON KEY
  lcSysMen = SET('SYSMENU')
  SET SYSMENU ON
  ON KEY LABEL ALT+L ACTIVATE WINDOW (lcAlCrTitl)

  RELEASE PAD _BROWSE OF _MSYSMENU
  
  lcTabKey = ON('KEY','TAB')
  lcBkTbKey = ON('KEY','BACKTAB')
  ON KEY LABEL TAB     DO lfBrTab3
  ON KEY LABEL BACKTAB DO lfBrBack3

  SELECT &lcCrtnDet 
  REPLACE nOldQty WITH nAlQty ALL
  GO TOP
  
  DO (gcScrDir+'AL\ALCRTP.SPX')
  
  ON KEY LABEL TAB     &lcTabKey
  ON KEY LABEL BACKTAB &lcBkTbKey
  
  SET SYSMENU &lcSysMen
  POP KEY
ELSE
  *B126708,1  TMI [Start] Change the message to reflect the WH id
  *=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There are no cartons available for this order type.')
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There are no cartons available for given styles in :'+ALLTRIM(laWareHous[puWareHous,1])+'.')
  *B126708,1  TMI [End  ] 
ENDIF
SELECT (lnAlias)
RETURN llCSave

*:**************************************************************************
*:* Name        : lfvAlQty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/20/2004
*:* Purpose     : Valid fn. for allocated cartons
*:***************************************************************************
*C123616,1
FUNCTION lfvAlQty
IF nAlQty > nCrtNo
  REPLACE nAlQty WITH nCrtNo
ENDIF

*-- end of lfvAlQty.

*:**************************************************************************
*:* Name        : lfAlClcTyp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/10/2004
*:* Purpose     : Collect types that are suitable for this order
*:***************************************************************************
*C123616,1
FUNCTION lfAlClcTyp
PARAMETERS  lcStatus
PRIVATE lnAlias,lcStyMajor,lnTotQty,lcOrdNo,llCollect

lnAlias = SELECT()

llCollect = .T.
IF RECCOUNT(lcCrtnDet)>0
  SELECT &lcCrtnDet
  *B126708,1  TMI [Start] Clear setted fiter ( if any )
  SET FILTER TO
  *B126708,1  TMI [End  ] 
  LOCATE   
  IF &lcCrtnDet..ORDER = laData[1]
    llCollect = .F.
  ENDIF
ENDIF

IF llCollect
  SELECT &lcCrtnDet
  ZAP  && Empty file to recollect types

  =SEEK('O'+laDATA[1],'ORDLINE')
  SELECT ORDLINE
  lnRecNo = RECNO()
  lnStyCnt = 0
  lcStyMajor = ''
  lnTotPickd = 0 
  lcPickTkt  = piktkt
  
  SELECT &lcOrdTemp
  ZAP
  
  lnArrlen = 0
  *DIMENSION laOrdSty[1,9]
  DIMENSION laOrdSty[1]
  SELECT ORDLINE
  SCAN REST WHILE cordtype+order+STR(lineno,6)= 'O'+laDATA[1]
    SCATTER MEMVAR MEMO
    INSERT INTO &lcOrdTemp FROM MEMVAR
    lcStyMajor = SUBSTR(ORDLINE.STYLE,1,lnstylewid)
    IF ASCAN(laOrdSty,lcStyMajor) = 0
      lnArrlen = lnArrlen + 1
      DIMENSION laOrdSty[lnArrlen]
      laOrdSty[lnArrlen] = lcStyMajor
    ENDIF
  ENDSCAN
  
  SELECT TEMPNAME
  APPEND BLANK
  SAVE TO MEMO CTEMPNAME ALL LIKE laOrdSty
  
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  
  *- Cartons are saved in the file POCRTNMF , for a specific order to allocate from cartons , a 
  * copy of the order will be taken in an array , and get the type with styles 
  * typical to the current order , if so show the allocation screen
  
  SELECT POCRTNMF
  GO TOP
  lcCCARTONNO = ' '
  lcCARTONTYP = ' '
  lnTotQty = 0
  
  *- How to fill lcCrtnDet file
  *-   1) Select types with styles into a temp cursor
  *-   2) Loop on this cursor to see types compatble with current order
  *-   3) Count available cartons for each compatble type and update the nCrtNo field
  
  PRIVATE lcTypCurs
  lcTypCurs = gfTempName()
  lcOrdNo = laData[1]
  IF EMPTY(lcPickTkt)
    *B126708,1  TMI [Start] Add WH field
    *SELECT CCARTONTYP,COUNT(DISTINCT CCARTONNO) AS nCrtNo ;
    *  FROM POCRTNMF ;
    *  WHERE EMPTY(PIKTKT) .AND. ;
    *        ASCAN(laOrdSty,PADR(CSTYMAJOR,lnStyleWid)) > 0 ;
    *  GROUP BY CCARTONTYP ;
    *  INTO CURSOR &lcTypCurs
    WAIT WINDOW NOWAIT 'Counting available cartons...'
    SELECT CCARTONTYP,COUNT(DISTINCT CCARTONNO) AS nCrtNo ,CWARECODE;
      FROM POCRTNMF ;
      WHERE EMPTY(PIKTKT) .AND. ;
            ASCAN(laOrdSty,PADR(CSTYMAJOR,lnStyleWid)) > 0 ;
      GROUP BY CCARTONTYP ;
      INTO CURSOR &lcTypCurs
    *B126708,1  TMI [End  ] 
  ELSE
    lcOrdNo = ' '
    *B126708,1  TMI [Start] Add WH field
    *SELECT CCARTONTYP,COUNT(DISTINCT CCARTONNO) AS nCrtNo ;
    *  FROM POCRTNMF ;
    *  WHERE PIKTKT=lcPickTkt .AND. ;
    *        ASCAN(laOrdSty,PADR(CSTYMAJOR,lnStyleWid)) > 0 ;
    *  GROUP BY CCARTONTYP ;
    *  INTO CURSOR &lcTypCurs
    WAIT WINDOW NOWAIT 'Counting available cartons...'
    SELECT CCARTONTYP,COUNT(DISTINCT CCARTONNO) AS nCrtNo ,CWARECODE;
      FROM POCRTNMF ;
      WHERE PIKTKT=lcPickTkt .AND. ;
            ASCAN(laOrdSty,PADR(CSTYMAJOR,lnStyleWid)) > 0 ;
      GROUP BY CCARTONTYP,CWARECODE ;
      INTO CURSOR &lcTypCurs
    *B126708,1  TMI [End  ]
  ENDIF
  SELECT &lcTypCurs  
  INDEX ON CCARTONTYP TAG &lcTypCurs
  
  *- Update TotQty,nCrtNo fields
  SET ORDER TO POCRTNMF IN POCRTNMF
  SELECT &lcTypCurs
  SCAN
    =SEEK(&lcTypCurs..CCARTONTYP,'POCRTNMF')
    SELECT POCRTNMF
    lcCrtNo = POCRTNMF.CCARTONNO
    lnTotQty = 0
    SCAN REST WHILE CCARTONTYP+CCARTONNO+CSTYMAJOR+STYLE = &lcTypCurs..CCARTONTYP+lcCrtNo
      lnTotQty = lnTotQty + POCRTNMF.TOTQTY
    ENDSCAN
    INSERT INTO &lcCrtnDet (ORDER,cCartonTyp,nCrtNo,TotQty) ;
                    VALUES (lcOrdNo,&lcTypCurs..CCARTONTYP,&lcTypCurs..nCrtNo,lnTotQty)
    *B126708,1  TMI [Start] update the CWARECODE field
    SELECT &lcCrtnDet
    REPLACE CWARECODE WITH &lcTypCurs..CWARECODE
    *B126708,1  TMI [End  ]                     
  ENDSCAN
  
  USE IN &lcTypCurs  

ENDIF
  
lcOrder    = laData[1]
lcAccount  = laData[2]

lnTotCrton = 0
SELECT &lcCrtnDet 
SUM nAlQty TO lnTotCrton

*- Update the variable "lnTotPickd"
SELECT ORDLINE
=SEEK('O'+laData[1],'ORDLINE')
*B126708,1  TMI [Start] write seek exprstion more better
*SCAN FOR cordtype+order+STR(lineno,6)= 'O'+laDATA[1]
SCAN REST WHILE cordtype+order+STR(lineno,6)= 'O'+laDATA[1]
  *B126708,1  TMI [End  ] 
  lnTotPickd = lnTotPickd + IIF(!EOF(lc_tmpordl) , &lc_tmpordl..TOTPIK , ORDLINE.TOTPIK )
ENDSCAN

SELECT (lnAlias)
*-- end of lfAlClcTyp.

*:**************************************************************************
*:* Name        : lfvAlCrtOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2004
*:* Purpose     : Update lc_TmpOrdL temp file with picked qty's
*:***************************************************************************
*:* Called from : "ALCRTP" SCREEN
*:***************************************************************************
*C123616,1
FUNCTION lfvAlCrtOk
PRIVATE lnSlct,llUndo,lnK,lcMjrPct
lnSlct = SELECT()

SELECT TEMPNAME
SCAN
  RESTORE FROM MEMO CTEMPNAME ADDITIVE
ENDSCAN

llUndo = .F.

IF !llUndo
  IF laScrMode[2]
    llUndo = .T.
  ENDIF
ENDIF

IF !llUndo
  llUndo = lcPickStat <> 'ENABLE'
  IF llUndo
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Order is picked , release it and then reallocate cartons.")
  ENDIF
ENDIF

IF !llUndo
  =SEEK('O'+laData[1],'ORDLINE')
  llUndo = !EMPTY(ORDLINE.PIKTKT)
  IF llUndo
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Order is picked , release and Save then Edit and reallocate cartons.")
  ENDIF
ENDIF

IF !llUndo
  SELECT &lcCrtnDet  
  LOCATE FOR nAlQty > nCrtNo
  llUndo = FOUND()
  IF llUndo
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Allocated cartons can not exceed availabe cartons.")    
  ENDIF
ENDIF

IF llUndo
  llUndo = .F.
  SELECT &lcCrtnDet  
  REPLACE ALL nAlQty WITH nOldQty  
ELSE  
  SELECT (lnSlct)
  CLEAR READ 
ENDIF

RETURN

*:**************************************************************************
*:* Name        : lfAlwCrtOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/10/2004
*:* Purpose     : When function for carton Ok button
*:***************************************************************************
*C123616,1
FUNCTION lfAlwCrtOk
PRIVATE lnSlct,llRet 
lnSlct = SELECT()
IF laScrMode[2]
  RETURN .F.
ENDIF

SELECT ORDLINE
IF !EOF()
  GO RECNO()
ELSE
  GO TOP
ENDIF

IF !EOF(lc_tmpOrdL)
  SELECT &lc_tmpOrdL
ENDIF  
llRet = !PICKED
SELECT (lnSlct)
RETURN llRet 
  
*-- end of lfAlwCrtOk.
*:**************************************************************************
*:* Name        : lfvAlCan
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2004
*:* Purpose     : Cancel allocation by carton
*:***************************************************************************
*:* Called from : "ALCRTP" SCREEN
*:***************************************************************************
*C123616,1
FUNCTION lfvAlCan

SELECT &lcCrtnDet
REPLACE nALQty WITH nOldQty ALL

CLEAR READ
*-- end of lfvAlCan.

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfAlTrap
IF WONTOP()  = lcAlCrTitl
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lfBrTab3
  ON KEY LABEL BACKTAB DO lfBrBack3
  ON KEY LABEL ESCAPE
ENDIF
*-- end of lfAlTrap.

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfAlUnTrap
IF glFromBrow
  =gfStopBrow()
ENDIF  

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESCAPE

*-- end of lfAlUnTrap.

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfBrTab3
ON KEY LABE TAB
ACTIVATE WINDOW (lcAlCrt3)
_CUROBJ = OBJNUM(pbAlOk)

*:***************************************************************************
*:***************************************************************************
*C123616,1
FUNCTION lfBrBack3
ON KEY LABE BACKTAB
ACTIVATE WINDOW (lcAlCrt3)
_CUROBJ = OBJNUM(pbAlCancel)

*:**************************************************************************
*:* Name        : lfAlTpBrw
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/10/2004
*:* Purpose     : Browse function for types
*:***************************************************************************
*C123616,1
FUNCTION lfAlTpBrw
SELECT(lcFileToUse)
GO TOP
lnNewRec = RECNO(lcFileToUse)
BROWSE FIELDS &lcALBrFild ; 
       LOCK 0             ;
       SAVE               ;
       NOCLEAR            ;
       NOWAIT             ;
       NOMENU             ;
       NODELETE           ;
       WHEN     lfwBrwAl3();
       VALID :F lfvBrwAl3();
       TITLE lcAlCrTitl   ;
       WINDOW (lcALCRT2) IN WINDOW (lcALCRT)
= lfwBrwAL3()
KEYBOARD '{TAB}{TAB}{TAB}'

*- Enable or disable Ok Button
=SEEK('O'+laData[1],'ORDLINE')
IF laScrMode[2] .OR. lcPickStat <> 'ENABLE' .OR. !EMPTY(ORDLINE.PIKTKT)
  SHOW GET pbAlOk DISABLE
ENDIF

*-- end of lfAlTpBrw.

*:**************************************************************************
*:* Name        : lfwBrwAl3
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/10/2004
*:* Purpose     : When function for browse types 
*:***************************************************************************
*C123616,1
FUNCTION lfwBrwAl3
SELECT (lcFileToUse)
lnBrRecNo = RECNO()
IF !laScrMode[2]
  SUM nAlQty    , TotQty*nAlQty TO ;
      lnTotCrton, lnTotPickd

  IF BETWEEN(lnBrRecNo,1,RECCOUNT())
    GO lnBrRecNo
  ENDIF
ENDIF
SHOW WINDOW (lcALCRTitl) REFRESH SAME
=lfRefresh()

*-- end of lfwBrwAl3.
*:**************************************************************************
*:* Name        : lfvBrwAl3
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/10/2004
*:* Purpose     : Valid function for browse types
*:***************************************************************************
*C123616,1
FUNCTION lfvBrwAl3
IF WONTOP() # (lcALCRTitl)
  =gfStopBrow()
ENDIF

SELECT (lcFileToUse)
lnBrRecNo = RECNO()

SUM (TotQty * nAlQty) TO lnTotCrton
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF
SHOW WINDOW (lcALCRTitl) REFRESH SAME
=lfRefresh()

*-- end of lfvBrwAl3.


*:**************************************************************************
*:* Name        : lfALPCKALL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/10/2004
*:* Purpose     : If user will pick using cartons , then do not pick a single 
*:*             : line , allow only pick all , for [Nik Nak]
*:***************************************************************************
*:* Called from : lfvPick IN ALORDAL.PRG
*:***************************************************************************
*C123616,1
FUNCTION lfALPCKALL
PRIVATE lnSlct,llOnlyPkAll
lnSlct = SELECT()

IF llPickAll
  RETURN .F.
ENDIF  

SELECT TEMPNAME
SCAN
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

SELECT &lcCrtnDet
LOCATE FOR nAlQty > 0
llOnlyPkAll = FOUND()
IF llOnlyPkAll
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Some cartons are assigned, you can not pick using single lines , only pick all is allowed.')
ENDIF

SELECT (lnSlct)

RETURN llOnlyPkAll
*-- end of lfALPCKALL.

*:**************************************************************************
*:* Name        : lfUPDTMAL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/10/2004
*:* Purpose     : Update the temp allocation file lc_tmpOrdL from pocrtnmf [for Nik Nak]
*:***************************************************************************
*:* Called from : lfvPickAll in ALORDAL.PRG
*:***************************************************************************
*C123616,1
FUNCTION lfUPDTMAL
PRIVATE lnSlct,lcSvOrd,lnJ
lnSlct = SELECT()

*- Then update picked qty's from pocrtnmf file 
SELECT TEMPNAME
SCAN
  RESTORE FROM Memo cTempName ADDITIVE
ENDSCAN

*- empty pick fields for lc_tmpOrdL file
SELECT &lc_TmpOrdL
REPLACE ALL PIK1 WITH 0, PIK2 WITH 0, PIK3 WITH 0, PIK4 WITH 0,;
            PIK5 WITH 0, PIK6 WITH 0, PIK7 WITH 0, PIK8 WITH 0, TOTPIK WITH 0 

lcSvOrd = ORDER(lc_TmpOrdL)
SELECT &lc_TmpOrdL
INDEX ON STYLE TO (gcWorkDir+lc_TmpOrdL)

*- Update picked quantities in lc_TmpOrdL
SELECT &lcCrtnDet
SCAN
  =SEEK(&lcCrtnDet..CCARTONTYP,'POCRTNMF')
  SELECT POCRTNMF
  LOCATE REST FOR EMPTY(PIKTKT)  
  FOR lnK = 1 TO &lcCrtnDet..nAlQty
    lCCARTONNO = POCRTNMF.CCARTONNO   
    SCAN REST WHILE CCARTONTYP+CCARTONNO = &lcCrtnDet..CCARTONTYP+lCCARTONNO
      SELECT &lc_tmpOrdL
      IF SEEK(POCRTNMF.STYLE,lc_tmpOrdL)                && Key of lc_tmpOrdL : STYLE
        REPLACE PIK1   WITH PIK1 + POCRTNMF.QTY1 ;
                PIK2   WITH PIK2 + POCRTNMF.QTY2 ;
                PIK3   WITH PIK3 + POCRTNMF.QTY3 ;
                PIK4   WITH PIK4 + POCRTNMF.QTY4 ;
                PIK5   WITH PIK5 + POCRTNMF.QTY5 ;
                PIK6   WITH PIK6 + POCRTNMF.QTY6 ;
                PIK7   WITH PIK7 + POCRTNMF.QTY7 ;
                PIK8   WITH PIK8 + POCRTNMF.QTY8 ;
                TOTPIK WITH PIK1+PIK2+PIK3+PIK4+PIK5+PIK6+PIK7+PIK8 ;

        *- This enable to Update ordhdr , style, stydye files
        FOR lnJ = 1 TO 8
          lcJ = STR(lnJ,1)
          M.cStatus = IIF( PIK&lcJ>QTY&lcJ , 'M' , M.cStatus )
        ENDFOR

        IF M.cStatus = 'M'
          REPLACE CSTATUS WITH 'M' ;
                  QTY1    WITH MAX(QTY1,PIK1) ;
                  QTY2    WITH MAX(QTY2,PIK2) ;
                  QTY3    WITH MAX(QTY3,PIK3) ;
                  QTY4    WITH MAX(QTY4,PIK4) ;
                  QTY5    WITH MAX(QTY5,PIK5) ;
                  QTY6    WITH MAX(QTY6,PIK6) ;
                  QTY7    WITH MAX(QTY7,PIK7) ;
                  QTY8    WITH MAX(QTY8,PIK8) ;
                  TOTQTY  WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
          REPLACE BOOK1   WITH MAX(BOOK1,PIK1) ;
                  BOOK2   WITH MAX(BOOK2,PIK2) ;
                  BOOK3   WITH MAX(BOOK3,PIK3) ;
                  BOOK4   WITH MAX(BOOK4,PIK4) ;
                  BOOK5   WITH MAX(BOOK5,PIK5) ;
                  BOOK6   WITH MAX(BOOK6,PIK6) ;
                  BOOK7   WITH MAX(BOOK7,PIK7) ;
                  BOOK8   WITH MAX(BOOK8,PIK8) ;
                  TOTBOOK WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8
        ENDIF
      ENDIF
    ENDSCAN
  ENDFOR
ENDSCAN

SELECT &lc_TmpOrdL
SET ORDER TO TAG (lcSvOrd)
SELECT (lnSlct)
*-- end of lfUPDTMAL.

*:**************************************************************************
*:* Name        : lfZPCRTDET
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/10/2004
*:* Purpose     : Triggre to zap file lcCrtnDet for Nik Nak
*:***************************************************************************
*:* Called from : lpShow in ALORDAL.PRG
*:***************************************************************************
*C123616,1
FUNCTION lfZPCRTDET
PRIVATE lnSlct
lnSlct = SELECT()
IF USED('TEMPNAME')
  SELECT TEMPNAME
  SCAN
    RESTORE FROM MEMO cTEMPNAME ADDITIVE
  ENDSCAN
  
  IF USED(lcCrtnDet)
    SELECT &lcCrtnDet
    ZAP
  ENDIF
ENDIF  
SELECT (lnSlct)
*-- end of lfZPCRTDET.


*:**************************************************************************
*:* Name        : lfGenCrt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/10/2004
*:* Purpose     : Generate cartons
*:***************************************************************************
*:* Called from : alplist.prg
*:***************************************************************************
*C123616,1
FUNCTION lfGenCrt
PRIVATE lcSkip,lcSvOrd,lnSlct,lnRecCnt
lnSlct = SELECT()

lnRecCnt = 0
SELECT &lcCtnHdr
COUNT TO lnRecCnt
IF lnRecCnt > 0 .AND. gfModalGen('INM00000B00006',.F.,.F.,.F.,;
                      'There are cartons already generated,Overwrite ?') = 2
  SELECT (lnSlct)
  RETURN
ENDIF  

IF !USED('POCRTNMF')
  =gfOpenFile(gcDataDir+'POCRTNMF','POPIKTKT','SH')
ENDIF
IF SEEK(laData[3],'POCRTNMF')
  SELECT &lcCtnHdr
  DELETE ALL
  SELECT &lcCtnDtl
  DELETE ALL
  
  STORE 0 TO lnPackQty ,lnPackCtn ,lnPackWgh
  
  SELECT &lcpcklin
  lcSvOrd = ORDER()
  SET ORDER TO &lcpcklin
  
  lcSkip = SET('SKIP')
  SET SKIP TO 
  
  REPLACE ALL ;
          CSELECT1  WITH IIF(!EMPTY(ORDQTY1),'»','') ;
          CSELECT2  WITH IIF(!EMPTY(ORDQTY2),'»','') ;
          CSELECT3  WITH IIF(!EMPTY(ORDQTY3),'»','') ;
          CSELECT4  WITH IIF(!EMPTY(ORDQTY4),'»','') ;
          CSELECT5  WITH IIF(!EMPTY(ORDQTY5),'»','') ;
          CSELECT6  WITH IIF(!EMPTY(ORDQTY6),'»','') ;
          CSELECT7  WITH IIF(!EMPTY(ORDQTY7),'»','') ;
          CSELECT8  WITH IIF(!EMPTY(ORDQTY8),'»','') ;
          OQTY1     WITH 0 ;
          OQTY2     WITH 0 ;
          OQTY3     WITH 0 ;
          OQTY4     WITH 0 ;
          OQTY5     WITH 0 ;
          OQTY6     WITH 0 ;
          OQTY7     WITH 0 ;
          OQTY8     WITH 0 
  REPLACE CTNQTY1 WITH POCRTNMF.QTY1 ;
          CTNQTY2 WITH POCRTNMF.QTY2 ;
          CTNQTY3 WITH POCRTNMF.QTY3 ;
          CTNQTY4 WITH POCRTNMF.QTY4 ;
          CTNQTY5 WITH POCRTNMF.QTY5 ;
          CTNQTY6 WITH POCRTNMF.QTY6 ;
          CTNQTY7 WITH POCRTNMF.QTY7 ;
          CTNQTY8 WITH POCRTNMF.QTY8 ;
          
  m.CART_NO   = 0
  SELECT POCRTNMF
  DO WHILE PIKTKT = laData[3] .AND. !EOF('POCRTNMF')
    lCCARTONNO = CCARTONNO 
    m.CART_NO = m.CART_NO + 1
    lnTOTPCS = 0
    lnTOTWGH = 0
    SCAN REST WHILE PIKTKT = laData[3] .AND. CCARTONNO = lCCARTONNO
      IF !SEEK(STR(m.CART_NO,4),lcCtnHdr)
        SELECT &lcCtnHdr
        APPEND BLANK
        REPLACE CART_NO   WITH m.CART_NO ;
                EMPTY     WITH 'N'
      ENDIF
      =SEEK(POCRTNMF.STYLE,lcpcklin)
      =SEEK(POCRTNMF.STYLE,'STYLE')
      SELECT &lcCtnDtl
      APPEND BLANK
      REPLACE CART_NO    WITH m.CART_NO ;
              STYLE      WITH POCRTNMF.STYLE ;
              NORDLINENO WITH &lcpcklin..NORDLINENO ;
              SIZE1      WITH &lcpcklin..CSIZE1 ;
              SIZE2      WITH &lcpcklin..CSIZE2 ;
              SIZE3      WITH &lcpcklin..CSIZE3 ;
              SIZE4      WITH &lcpcklin..CSIZE4 ;
              SIZE5      WITH &lcpcklin..CSIZE5 ;
              SIZE6      WITH &lcpcklin..CSIZE6 ;
              SIZE7      WITH &lcpcklin..CSIZE7 ;
              SIZE8      WITH &lcpcklin..CSIZE8 ;
              QTY1       WITH POCRTNMF.QTY1 ;
              QTY2       WITH POCRTNMF.QTY2 ;
              QTY3       WITH POCRTNMF.QTY3 ;
              QTY4       WITH POCRTNMF.QTY4 ;
              QTY5       WITH POCRTNMF.QTY5 ;
              QTY6       WITH POCRTNMF.QTY6 ;
              QTY7       WITH POCRTNMF.QTY7 ;
              QTY8       WITH POCRTNMF.QTY8 
      REPLACE WEIGHT1    WITH IIF(QTY1>0,STYLE.NSTYWEIGHT,0) ;
              WEIGHT2    WITH IIF(QTY2>0,STYLE.NSTYWEIGHT,0) ;
              WEIGHT3    WITH IIF(QTY3>0,STYLE.NSTYWEIGHT,0) ;
              WEIGHT4    WITH IIF(QTY4>0,STYLE.NSTYWEIGHT,0) ;
              WEIGHT5    WITH IIF(QTY5>0,STYLE.NSTYWEIGHT,0) ;
              WEIGHT6    WITH IIF(QTY6>0,STYLE.NSTYWEIGHT,0) ;
              WEIGHT7    WITH IIF(QTY7>0,STYLE.NSTYWEIGHT,0) ;
              WEIGHT8    WITH IIF(QTY8>0,STYLE.NSTYWEIGHT,0) ;
              ORGWGH     WITH STYLE.NSTYWEIGHT ;
              SZCNT      WITH &lcpcklin..SZCNT ;
              CSTATUS    WITH 'M' ;
              BR1        WITH QTY1>0 ;
              BR2        WITH QTY2>0 ;
              BR3        WITH QTY3>0 ;
              BR4        WITH QTY4>0 ;
              BR5        WITH QTY5>0 ;
              BR6        WITH QTY6>0 ;
              BR7        WITH QTY7>0 ;
              BR8        WITH QTY8>0 
       
      lnTOTPCS = lnTOTPCS + QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      lnTOTWGH = lnTOTWGH + WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8      
      
      lnPackQty = lnPackQty + QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      lnPackWgh = lnPackWgh + WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8      
      lnPackCtn = m.CART_NO   

      SELECT &lcPckLin
      REPLACE PQTY1   WITH PQTY1 + POCRTNMF.QTY1 ;
              PQTY2   WITH PQTY2 + POCRTNMF.QTY2 ;
              PQTY3   WITH PQTY3 + POCRTNMF.QTY3 ;
              PQTY4   WITH PQTY4 + POCRTNMF.QTY4 ;
              PQTY5   WITH PQTY5 + POCRTNMF.QTY5 ;
              PQTY6   WITH PQTY6 + POCRTNMF.QTY6 ;
              PQTY7   WITH PQTY7 + POCRTNMF.QTY7 ;
              PQTY8   WITH PQTY8 + POCRTNMF.QTY8 ;
              PWGH1   WITH PWGH1 + &lcCtnDtl..WEIGHT1 ;
              PWGH2   WITH PWGH2 + &lcCtnDtl..WEIGHT2 ;
              PWGH3   WITH PWGH3 + &lcCtnDtl..WEIGHT3 ;
              PWGH4   WITH PWGH4 + &lcCtnDtl..WEIGHT4 ;
              PWGH5   WITH PWGH5 + &lcCtnDtl..WEIGHT5 ;
              PWGH6   WITH PWGH6 + &lcCtnDtl..WEIGHT6 ;
              PWGH7   WITH PWGH7 + &lcCtnDtl..WEIGHT7 ;
              PWGH8   WITH PWGH8 + &lcCtnDtl..WEIGHT8
              
    ENDSCAN
    SELECT &lcCtnHdr
    REPLACE TOTPCS     WITH TOTPCS    + lnTOTPCS ;
            TOTWGH     WITH TOTWGH    + lnTOTWGH

    SELECT POCRTNMF
  ENDDO

  SELECT &lcpcklin
  SET ORDER TO (lcSvOrd)
  SET SKIP TO &lcSkip 
  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Cartons Generated.')
ENDIF

SELECT (lnSlct)
*-- end of lfGenCrt.


*!*************************************************************
*! Name      : lfSequence                  
*! Developer : Nader Nabil 
*! Date      : 01/06/2005
*! Purpose   : To get new sequance number for any item
*!*************************************************************
*! Calls     :  None
*!*************************************************************
*! Passed Parameters  : Sequance type
*!*************************************************************
*! Returns   : A Sequence No.
*!*************************************************************
*! Example   :  lcData[1] = lfSequence('CINVOICE')
*!*************************************************************
*! Note : this function copied from Aria27.prg with some modifications
*!        to Speedup this processing
*!*************************************************************
*!B125800,1
FUNCTION lfSequence
PARAMETERS lcSeqType
PRIVATE lnRetVal,lcSavAlias,lcDataDir, lnOldGenNm,lcExtraStr,llNoEdtFld,llNoVerFld
PRIVATE lcChrToUpd
lcChrToUpd = CHR(0)

lcSavAlias = SELECT(0)
lcSeqType  = UPPER(lcSeqType)
lcDataDir  = gcDataDir

IF !llGetBefor
  lcUnqPreFx = gfGetMemVar("M_UNQSTPRX",gcAct_Comp)
  llDivOnSeq = gfgetMemvar('M_DIV_SEQ' , gcAct_Comp) = 'Y'
  llGetBefor =.T.
ENDIF
IF !USED('SEQUENCE')
  luSequence = .T.
  USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
ELSE
  SELECT SEQUENCE
  luSequence = .F.
  ltSequence = VAL(SYS(21))
  leSequence = RECNO()
  SET ORDER TO TAG Cseq_type IN SEQUENCE
ENDIF

IF !SEEK(PADR(lcSeqType,10),'SEQUENCE')
  IF SEEK(PADR(lcSeqType,10),'SEQUENCE')
    SELECT SEQUENCE
    lnDefSeq = 0
    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
      lnDefSeq = MAX(lnDefSeq,nSeq_No)
    ENDSCAN
    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
  ENDIF
  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth,cSeq_Chr) ;
       VALUES (lcSeqType,lnDefSeq,SPACE(2),sydfield.cData_Typ,;
       sydfield.nFld_Wdth,CHR(0))
ENDIF
*--MAN Added RLOCK Condition[Start]
   DO WHILE !RLOCK("SEQUENCE")
   ENDDO
  lnRetVal   = SEQUENCE.nSeq_No
  lcChrToUpd = Sequence.cSeq_Chr

*--MAN Added RLOCK Condition[End]

lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
lnOldGenNm = SEQUENCE.nSeq_No
lcExtraStr = ''
IF !EMPTY(SEQUENCE.cSeq_Chr)
  PRIVATE lcChar , lnCharPos , lnI
  IF !(SEQUENCE.cSeq_Chr = CHR(0))
    IF MOD(ASC(SEQUENCE.cSeq_Chr),26) = 0
      lcChar = "Z"
      lnCharPos = ASC(SEQUENCE.cSeq_Chr)/26
    ELSE
      lcChar =  CHR(MOD(ASC(SEQUENCE.cSeq_Chr),26)+64)
      lnCharPos = INT(ASC(SEQUENCE.cSeq_Chr)/26)+1
    ENDIF  
    FOR lnI = 1 TO lnCharPos - 1
      lcExtraStr = lcExtraStr + "Z"
    ENDFOR
    lcExtraStr = lcExtraStr + lcChar
  ELSE
  lcChar=""
  ENDIF
ENDIF
SELECT SEQUENCE
REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1), cSeq_Chr WITH lcChrToUpd
lnOldGenNm = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
llNoEdtFld = (TYPE('SEQUENCE.cEdit_User') = 'U') OR (TYPE('SEQUENCE.dEdit_Date') = 'U') OR (TYPE('SEQUENCE.cEdit_Time') = 'U')
IF EMPTY(cAdd_user)  .OR. llNoEdtFld
  REPLACE cAdd_User  WITH gcUser_ID ,;
          dAdd_Date  WITH DATE()    ,;
          cAdd_Time  WITH gfGetTime()
ELSE
  REPLACE cEdit_User  WITH gcUser_ID ,;
          dEdit_Date  WITH DATE()    ,;
          cEdit_Time  WITH gfGetTime()
ENDIF
llNoVerFld = (TYPE(EVAL('lcFileName')+'.'+'CADD_VER') = 'U') OR (TYPE(EVAL('lcFileName')+'.'+'CEDT_VER') = 'U')
IF  !llNoVerFld
  IF EMPTY(CADD_VER)  
    REPLACE CADD_VER  WITH "A27"          
  ELSE
    REPLACE CEDT_VER  WITH "A27"  
  ENDIF
ENDIF

*--MAN Added RLOCK Condition[Start]
IF  nSeq_No = 0 .AND. lnOldGenNm <> 0
  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
ENDIF
IF !EMPTY(lcExtraStr)
  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
ENDIF
UNLOCK
 lnRetVal = lcUnqPreFx + PADL(lnOldGenNm, lnRetLen, "0")
IF luSequence
  USE IN Sequence
ELSE
  SET ORDER TO TAG ltSequence IN Sequence
  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
    GOTO leSequence IN 'Sequence'
  ENDIF
ENDIF
SELECT (lcSavAlias)
RETURN(lnRetVal)
