*:************************************************************************
*: Program file  : MFRECLOT.PRG             *C102172,1 
*: Program desc. : Receiving C/T By Lot.
*: For screen    : MFRECLOT.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MF
*:      Developer: AMH (Ahmed Maher)
*:           Date: 01/30/2001
*:************************************************************************
*: Passed Parameters :-
*:************************************************************************
*: Modifations : 
*: C102172,4 AMH 03/19/2001 Rework of Task C102172,1
*: B605913,1 AMH 05/19/2001 Fix the bug of cancel shipment option disabled.
*:************************************************************************
STORE ' ' TO lcWindCh1,lcWindCh2
*--lcWindCh1  ---> hold browse window name 
*--lcWindCh2  ---> hold window name for recieving by lot
*--lcRecStyle ---> style field
*--lcRStyDesc ---> style description
*--lcBrowTitl ---> hold window title
*--lcTktSheet ---> temp file name for receiving by lot
*--lcStyleSt  ---> hold 'DISABLE'/'ENABLE' for showing style field
*--lcnewst    ---> hold 'DISABLE'/'ENABLE' for showing new Butt.
*--lcremst    ---> hold 'DISABLE'/'ENABLE' for showing remove Butt.
lcWindCh1 = gfTempName()
lcWindCh2 = gfTempName()


STORE " " TO lcRecStyle,lcRStyDesc,lcFactory,lcInvoice,lcContaner
STORE {  /  /  } TO ldEta,ldContrec
STORE 'Line Quantity' TO lcByLtTtl
lcSkMode='DISABLE' 
lcTktSheet=gfTempName()

*C102172,4 AMH Save the variable lcTmpLine [Start]
lcTktShee1 = lcTmpLine
*C102172,4 AMH [End]

*--call Function to create temp file for receiving by lot
=lfStyleTmp()
STORE 'DISABLE' TO lcStyleSt, lcNewSt, lcRemSt
lcnewst = 'ENABLE'

llFrist = .T.
IF llRecLot
  lcBrowTitl  = 'Receive By Lot'
ELSE
  lcBrowTitl  = 'Receive By Invoice'
  = gfOpenFile(gcDataDir+'TRANSHDR',gcDataDir+'TRANSHDR','SH')
ENDIF


*-- Save trapping setting .
lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')
*C102172,4 AMH Fix Esc botton doesn't work [Start]
lcHldEsc = ON('KEY','ESC')
*C102172,4 AMH [End]
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
*C102172,4 AMH Fix Esc botton doesn't work [Start]
ON KEY LABEL ESC DO lfvReturn
*C102172,4 AMH [End]

ON KEY LABEL ALT+Z ACTIVATE WINDOW (lcBrowTitl)

*-- call screen recieving by lot

DO (gcScrDir+gcWinAppl+"\MFRECLOT.SPX")

*C102172,4 AMH Fix not update Qty overed the original Qty [Start]
*=lfDistribt()
IF !llRecLot
  =lfDistribt()
ENDIF
*C102172,4 AMH [End]

=gfCloseFile('TRANSHDR')
*-- if there is cuttkt selected by the user --> llfoundRec = .T.
*C102172,4 AMH Fix uncomplete session message apperars [Start]
*IF RECCOUNT(lcTktSheet) > 0 
SELECT (lcTktSheet)
LOCATE
IF !EOF()
  llFoundRec = .T.
  llRecLot = .T.
ELSE
  SELECT (lcTmpLine)
  LOCATE
  IF !EOF()
    llFoundRec = .T.
  ENDIF
ENDIF
  *llFoundRec = .T.
*ENDIF
*C102172,4 AMH [End]

SELECT (lcTktSheet)
LOCATE
SCAN
  SCATTER MEMO MEMVAR
  SELECT (lcTmpLine)
  APPEND BLANK
  GATHER MEMO MEMVAR
ENDSCAN

SELECT (lcTmpLine) 
LOCATE
*--calculate the total stock and the total damage and the total canceld
STORE 0 TO lnTotStk,lnTotDam,lnTotCan
SCAN FOR TranCD = '1'
  lnTotStk=lnTotStk + TOTSTK
  lnTotDam=lnTotDam + TotDam
  lnTotCan=lnTotCan + TotCan
ENDSCAN

*--delete temp file
IF USED(lcTktSheet)
  USE IN (lcTktSheet)
  ERASE (gcWorkDir+lcTktSheet+'.DBF')
  ERASE (gcWorkDir+lcTktSheet+'.CDX')
ENDIF
ON KEY LABEL ALT+Z 
*-- Restore trapping setting .
ON KEY LABEL TAB     &lcHldTab
ON KEY LABEL BACKTAB &lcHldBtb
ON KEY LABEL ENTER   &lcHldEnt
*C102172,4 AMH Fix Esc botton doesn't work [Start]
ON KEY LABEL ESC     &lcHldEsc
*C102172,4 AMH [End]
llByStyle = .F.
RETURN

*!*************************************************************
*! Name      : lfStyleTmp
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : create temp file for receiving by lot
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfStyleTmp()
*!*************************************************************
*
FUNCTION lfStyleTmp
DIMENSION laFileStru[1,4]
SELECT (lcTmpLine)
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTktSheet) FROM ARRAY laFileStru
INDEX ON cuttkt+style+dyelot+trancd TAG lcTktSheet OF (gcWorkDir+lcTktSheet)
INDEX ON trancd TAG lcTktShet1 OF (gcWorkDir+lcTktSheet)
*C102172,4 AMH add index for line Quantaty [Start]
INDEX ON TranCd+cCarton+Cuttkt+Style+Dyelot+cWareCode+STR(LineNo,6) TAG lcTktShet2 OF (gcWorkDir+lcTktSheet)
*C102172,4 AMH [End]
SELECT (lcTktSheet)
SET ORDER TO TAG lcTktSheet
RETURN
*-- end of lfStyleTmp.

*!*************************************************************
*! Name      : lfDeact
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : Trap keys (TAB - BACKTAB - ESCAPE)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfDeact()
*!*************************************************************
*
FUNCTION lfDeact
*-- Trap keys in this function
IF WONTOP()=(lcBrowTitl)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpStTrap   
  ON KEY LABEL BACKTAB    DO lpStShTrap
  *C102172,4 AMH Comment next line [Start]
  *ON KEY LABEL ESCAPE     LLDUMMY1=.T.
  *C102172,4 AMH [End]
ENDIF
RETURN
*-- end of lfDeact.

*!*************************************************************
*! Name      : lpStTrap
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : Trap key (  TAB  )
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : DO lpStTrap
*!*************************************************************
*
PROCEDURE lpStTrap
ON KEY LABEL TAB
IF WONTOP()=(lcBrowTitl)
  ACTIVATE WINDOW (lcWindCh2)
  _CUROBJ=OBJNUM(lcRecStyle)
ELSE
   ACTIVATE WINDOW (lcBrowTitl)
ENDIF  
RETURN
*-- end of lpStTrap.

*!*************************************************************
*! Name      : lpStShTrap
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : Trap key (  BACKTAB  )
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : DO lpStShTrap
*!*************************************************************
*
PROCEDURE lpStShTrap
ON KEY LABEL BACKTAB
IF WONTOP()=(lcBrowTitl)
  ACTIVATE WINDOW (lcWindCh2)
  _CUROBJ=OBJNUM(pbClose)
ELSE
  ACTIVATE WINDOW (lcBrowTitl)
ENDIF
RETURN
*-- end of lpStShTrap.

*!*************************************************************
*! Name      : lfAct
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : *---Untrap keys ( TAB -  BACKTAB - ESCAPE )
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfAct()
*!*************************************************************
*
FUNCTION lfAct
IF WONTOP() # (lcBrowTitl)  
  IF glFromBrow
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    *C102172,4 AMH Comment next line [Start]
    *ON KEY LABEL ESC DO gfEscap
    *C102172,4 AMH [End]
    =gfStopBrow()
  ENDIF
ENDIF
*-- end of lfAct.

*!*************************************************************
*! Name      : lfvReturn
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : when user press close Butt.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfvReturn()
*!*************************************************************
*
FUNCTION lfvReturn
*C102172,4 AMH don't make any checks before clear read [Start]
*PRIVATE lnRecNo, llRecFound, lnAlias
*--lnRecNo    ---> hold  current record no
*--llRecFound ---> determine if there are an record without qty
*lnAlias = SELECT(0)
*SELECT (lcTktSheet)
*lnRecNo = RECNO()
*llRecFound = .F.
*GO TOP 
*--scan only for budjet qty ( trancd = 1 )
*SCAN FOR TranCd = '1'
  *-- if there is no qty
*  IF TotStk = 0 AND TotDam = 0 AND TotCan = 0
*    =gfModalGen('TRM00000B52361',.F.,.F.,.F.,'You cannot leave the line Quantity empty.')
*    llRecFound = .T.
*    EXIT
*  ENDIF
*ENDSCAN
*--if there is qty for all lines close screen
*IF !llRecFound
*  CLEAR READ
*ELSE
*  IF BETWEEN(lnRecNo,1,RECCOUNT())
*    GOTO lnRecNo
*  ENDIF  
*ENDIF
*SELECT (lnAlias)
CLEAR READ
*C102172,4 AMH [End]
*-- end of lfvReturn.

*!*************************************************************
*! Name      : lfByLtActB
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : browse the receiving by Lot temp file
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfByLtActB()
*!*************************************************************
*
FUNCTION lfByLtActB
lcBrTpNme = 'C/T #'
lnBrRecNo = RECNO()
*--lcBlFields ---> hold the fields name we need it to browse
lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "Style     :R :H=lcStyHdr :26,"+;
             "Cuttkt    :R :H='Cuttkt #' :9,"+;
             "nLot_no   :R :H='Lot #' :5,"+;
             IIF(llWareHous ,"lcWHTtl = IIF(lcPType$'NA',Vendor,cWareCode) :H=IIF(lcPType$'NA','Source','Location'):10,","")+;
             IIF(lcPType$'NA',"cWareCode :H='Target':12,","")+;
             "TotStk :R :H='Stock'   :10,"+;
             "TotDam :R :H='Other'   :10,"+;
             "TotCan :R :H='Cancel'  :10"
SELECT (lcTktSheet)
GO TOP 
BROWSE FIELDS &lcBlfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       FOR TRANCD = '1'     ;
       TITLE (lcBrowTitl) ;
       WHEN lfByLtwBrw()     ;       
       VALID :F lfByLtVBrw();         
       WINDOW (lcWindCh1) ;
       IN WINDOW MFRECLOT
RETURN
*-- end of lfByLtActB.

*!*************************************************************
*! Name      : lfByLtwBrw
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : When valid function for browse.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfByLtwBrw()
*!*************************************************************
*
FUNCTION lfByLtwBrw
lnBrRecNo = RECNO(lcTktSheet)

=lfReadLTLn(EOF())
glFromBrow = .T.
RETURN
*-- end of lfByLtwBrw.

*!*************************************************************
*! Name      : lfByLtVBrw
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : valid function for browse.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfByLtVBrw()
*!*************************************************************
*
FUNCTION lfByLtVBrw
IF WONTOP() # (lcBrowTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfByLtVBrw.

*!*************************************************************
*! Name      : lfReadLTLn
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : read line from record and show lcWindCh2 window
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : llClearln ---->.F. in case of clear value 
*!                                    from fields 
*!*************************************************************
*! Example     : =lfReadLTLn()
*!*************************************************************
*
FUNCTION lfReadLTLn
PARA llClearLn
lnAlias = SELECT()
*-- if not need to clear value from fields get data from fields
IF ! llClearLn
  SELECT (lcTktSheet)
  lnBrRecNo  =RECNO()
  lcRecStyle   = Style
  lcRStyDesc = IIF(SEEK(lcRecStyle,'STYLE'),STYLE.Desc1,'')
  lcCtkt = CUTTKT
  lnLot  = NLOT_NO
ELSE
  *--clear value from fields
  STORE ' ' TO lcRecStyle,lcRStyDesc,lnLot,lcCtkt,lcFactory,lcInvoice,lcContaner
  STORE {  /  /  } TO ldEta,ldContrec
ENDIF
*--call function to show the window and disable or enable buttons
=lfShwLtScr()
SHOW WINDOW (lcBrowTitl) REFRESH
=lfRefresh(lcWindCh2)
SELECT(lnAlias)
RETURN
*-- end of lfReadLTLn.

*!*************************************************************
*! Name      : lfShwLtScr
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : show the window and disable or enable buttons
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     : =lfShwLtScr()
*!*************************************************************
*
FUNCTION lfShwLtScr
SHOW GETS WINDOW (lcWindCh2) DISABLE

*lcNewSt = 'ENABLE'

IF !EOF(lcTktSheet)
  STORE 'ENABLE' TO lcRemSt,lcNewSt
  STORE 'DISABLE' TO lcStyleSt
  lcNewSt = 'DISABLE'
  SHOW GET ldContRec ENABLE
ELSE
  STORE 'DISABLE' TO lcRemSt,lcStyleSt
  lcNewSt = 'ENABLE'
  SHOW GET ldContRec DISABLE
ENDIF
IF llRecLot
  lcNewSt = 'ENABLE'
ENDIF
SHOW WINDOW (lcBrowTitl) REFRESH
SHOW GET lcRecStyle &lcStyleSt
SHOW GET lnLot      &lcStyleSt
SHOW GET lcFactory  &lcStyleSt
SHOW GET lcType     &lcStyleSt
SHOW GET lcInvoice  &lcStyleSt
SHOW GET ldContRec  &lcStyleSt
SHOW GET pbxNew     &lcNewSt
SHOW GET pbxRem     &lcRemSt
SHOW GET pbEditQ    &lcRemSt
SHOW GET pbClose    ENABLE
SHOW GET lcCtkt     ENABLE
IF !EOF(lcTktSheet)
  SHOW GET ldContRec ENABLE
ELSE
  SHOW GET ldContRec DISABLE
ENDIF
RETURN
*-- end of lfShwLtScr.

*!*************************************************************
*! Name      : lfByLotNew
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : Valid the new button
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     : =lfByLotNew()
*!*************************************************************
*
FUNCTION lfByLotNew
*--Clear Line.
lcPrvTRc = lcRecStyle
=lfReadLTLn(.T.)
lcStyleSt = 'ENABLE'
SHOW GETS WINDOW (lcWindCh2) DISABLE
SHOW GET pbClose  ENABLE
SHOW GET lcRecStyle &lcStyleSt
SHOW GET ibRecStyle &lcStyleSt
SHOW GET lnLot      &lcStyleSt
SHOW GET ibLot      &lcStyleSt
SHOW GET lcCtkt     ENABLE
SHOW GET lcFactory  &lcStyleSt
SHOW GET ibFactory  &lcStyleSt
SHOW GET lcType     &lcStyleSt
SHOW GET lcInvoice  &lcStyleSt
SHOW GET ldContRec  &lcStyleSt
IF llRecLot
  _CUROBJ=OBJNUM(lcRecStyle)
ELSE
  _CUROBJ=OBJNUM(lcFactory)
ENDIF
RETURN
*--end of lfByLotNew.

*!*************************************************************
*! Name      : lfvByLtRem
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/30/2001
*! Purpose   : valid the Remove button.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvByLtRem()
*!*************************************************************
*
FUNCTION lfvByLtRem
*-Are you sure you want to delete this line?
IF gfModalGen('QRM34036B42002','DIALOG') = 1
  lnAlias = SELECT(0)
  SELECT (lcTktSheet)
  lnRecNo=RECNO() - 1
  lcCtkt = cuttkt
  lcRecStyle = style
  IF SEEK(lcCtkt+lcRecStyle)
    DELETE REST WHILE CUTTKT+STYLE+DYELOT+TRANCD = lcCtkt+lcRecStyle
  ENDIF
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  LOCATE
  =lfReadLTLn(EOF())
  SHOW WINDOW (lcBrowTitl) REFRESH
  SELECT (lnAlias)
ENDIF
RETURN
*-- end of lfvByLtRem.

*!*************************************************************
*! Name      : lfvLtEdtQt
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/31/2001
*! Purpose   : valid the Line Quantaty button.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvLtEdtQt()
*!*************************************************************
*
FUNCTION lfvLtEdtQt
*C102172,4 AMH Comment next lines [Start]
*PRIVATE lcCuttlOrd , lcCuttlPos , lnCurAlias
*SELECT (lcTktSheet)
*SCATTER MEMVAR
*lcLKey = cuttkt+style+dyelot
*DECLARE  laSok[8],laDam1[8],laDam2[8],laCan[8]
*STORE 0 TO laSok,laDam1,laDam2,laCan
*DECLARE laOldOut[8]
*STORE 0 TO laOldOut

*--New Origenal and order quantity.
*=SEEK(lcLKey+'1')
*=SEEK(Style,'STYLE')
*=SEEK('S'+STYLE.Scale,'SCALE')
*--get the scale sizes
*lcSz1=SCALE.Sz1
*lcSz2=SCALE.Sz2
*lcSz3=SCALE.Sz3
*lcSz4=SCALE.Sz4
*lcSz5=SCALE.Sz5
*lcSz6=SCALE.Sz6
*lcSz7=SCALE.Sz7
*lcSz8=SCALE.Sz8

*STORE '' TO lcRetSty1,lcRetSty2,lcRetSHd,lcRetSHd1,lcRetSHd2,lcRetSHd3,lcRetSHd4
*lcMStyQlty=STYLE.cStyGrade
*lcSndGrd = "2"
*lcTrdGrd = "3"
*DO CASE
*  CASE lcMStyQlty='1'
*    lcRetSHd ="1st Quality"
*    IF lcPType<>'A'
*      lcRetSHd1="2nd Quality"
*      lcRetSHd2="Damaged"
*      lcRetSHd3="Second Quality"
*      lcRetSHd4="Damaged"
*    ENDIF
*  CASE lcMStyQlty='2'
*    lcRetSHd ="2nd Quality"
*    IF lcPType<>'A'
*      lcRetSHd1="1st Quality"
*      lcRetSHd2="Damaged"
*      lcRetSHd3="First Quality"
*      lcRetSHd4="Damaged"
*    ENDIF
*    lcSndGrd = "1"
*    lcTrdGrd = "3"
*  CASE lcMStyQlty='3'
*    lcRetSHd ="Damaged"
*    IF lcPType<>'A'
*      lcRetSHd1="1st Quality"
*      lcRetSHd2="2nd Quality"    
*      lcRetSHd3="First Quality"
*      lcRetSHd4="Second Quality"
*    ENDIF
*    lcSndGrd = "1"
*    lcTrdGrd = "2"
*ENDCASE

*--1) Get Stock quantity.
*IF SEEK(lcLKey+'2')
*  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSok
*ENDIF

*--2) Get Damage quantity.
*STORE 'DISABLE' TO lcRSt1Stat,lcRSt2Stat
*IF SEEK(lcLKey+lcOthrTrCd)
*  SCAN REST WHILE cuttkt+style+dyelot+TranCd=lcLKey+lcOthrTrCd
*    IF cStyGrade = lcSndGrd
*      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam1
*      lcRetSty1=cRetSty
*      lcRSt1Stat='ENABLE'
*    ELSE
*      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam2
*      lcRetSty2=cRetSty
*      lcRSt2Stat='ENABLE'
*    ENDIF
*  ENDSCAN
*ENDIF
*--3) Get Cancel quantity.
*IF SEEK(lcLKey+lcCanlTrCd)
*  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laCan
*ENDIF
*lcSkMode = 'ENABLE'
*lcOtMode = 'ENABLE'
*IF lcPType $ 'NA'
*  lcOtMode = 'DISABLE'
*  lcRSt1Stat = 'DISABLE'
*  lcRSt2Stat = 'DISABLE'
*ENDIF

*=SEEK(lcLKey+'1')
*lcHldTab = ON('KEY','TAB')
*lcHldBtb = ON('KEY','BACKTAB')    
*lcHldEnt = ON('KEY','ENTER')
*ON KEY LABEL TAB    
*ON KEY LABEL BACKTAB
*ON KEY LABEL ENTER

*calculate old values of Out quantities from this line.
*FOR lnI = 1 TO 8
*  laOldOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
*ENDFOR

*--Call line quantity screen.
*DO (gcScrDir+gcWinAppl+"\mfLtRcvQ.SPX")

*ON KEY LABEL TAB     &lcHldTab
*ON KEY LABEL BACKTAB &lcHldBtb
*ON KEY LABEL ENTER   &lcHldEnt

*--Get Totals.
*lnTStk =laSok[1]+laSok[2]+laSok[3]+laSok[4]+laSok[5]+laSok[6]+laSok[7]+laSok[8]
*lnTCan =laCan[1]+laCan[2]+laCan[3]+laCan[4]+laCan[5]+laCan[6]+laCan[7]+laCan[8]
*lnTDam1=laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]
*lnTDam2=laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]
*lnTDam =lnTDam1+lnTDam2

*--1) Update stock quantity.
*IF SEEK(lcLKey+'2')
*  IF lnTStk<>0
*    GATHER FROM laSok FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*    REPLACE TotQty WITH lnTStk
*  ELSE
*    BLANK
*    DELETE
*  ENDIF
*ELSE
*  IF lnTStk<>0
*    APPEND BLANK
*    GATHER MEMVAR
*    REPLACE Trancd WITH '2',;
*            cStyGrade WITH lcMStyQlty
*    GATHER FROM laSok  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 
*    REPLACE TotQty WITH lnTStk
*  ENDIF
*ENDIF
*--2) Update Damaged quantity.
*=SEEK(lcLKey+lcOthrTrCd)
*LOCATE REST WHILE cuttkt+style+dyelot+Trancd=lcLKey+lcOthrTrCd ;
*              FOR cStyGrade=lcSndGrd
*IF FOUND()
*  IF lnTDam1=0
*    BLANK
*    DELETE
*  ELSE
*    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*    REPLACE TotQty  WITH lnTDam1,;
*            cRetSty WITH lcRetSty1
*  ENDIF
*ELSE
*  IF lnTDam1<>0
*    APPEND BLANK
*    GATHER MEMVAR
*    REPLACE Trancd    WITH lcOthrTrCd,;
*            cStyGrade WITH lcSndGrd,;
*            cRetSty   WITH lcRetSty1,;
*            TotQty    WITH lnTDam1
*    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*  ENDIF
*ENDIF

*=SEEK(lcLKey+lcOthrTrCd)
*LOCATE REST WHILE cuttkt+style+dyelot+Trancd=lcLKey+lcOthrTrCd ;
*              FOR cStyGrade=lcTrdGrd
*IF FOUND()
*  IF lnTDam2=0
*    BLANK
*    DELETE
*  ELSE
*    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*    REPLACE TotQty  WITH lnTDam2,;
*            cRetSty WITH lcRetSty2
*  ENDIF
*ELSE
*  IF lnTDam2<>0
*    APPEND BLANK
*    GATHER MEMVAR
*    REPLACE Trancd    WITH lcOthrTrCd,;
*            cStyGrade WITH lcTrdGrd,;
*            cRetSty   WITH lcRetSty2,;
*            TotQty    WITH lnTDam2
*    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*  ENDIF
*ENDIF
*--3) Update cancel quantity.
*IF SEEK(lcLKey+lcCanlTrCd)
*  IF lnTCan<>0
*    GATHER FROM laCan FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*    REPLACE TotQty WITH lnTCan
*  ELSE
*    BLANK 
*    DELETE
*  ENDIF
*ELSE
*  IF lnTCan<>0
*    APPEND BLANK
*    GATHER MEMVAR
*    REPLACE Trancd WITH lcCanlTrCd,;
*            TotQty WITH lnTCan
*    GATHER FROM laCan  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
*  ENDIF
*ENDIF
*IF SEEK(lcLKey+'1')
*  REPLACE TotStk WITH lnTStk,;
*          TotDam WITH lnTDam,;
*          TotCan WITH lnTCan
*ENDIF
*IF (lcPType $ 'ISMD')
*  DIMENSION laOut[8]
*  STORE 0 TO lnTotOut,laOut
*  FOR lnI = 1 TO 8
*    Z = STR(lnI,1)
*    laOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
*    lnTotOut   = lnTotOut + laOut[lnI]
*  ENDFOR
*  REPLACE TOTBAL WITH IIF(TOTQTY-TOTSTK-TOTDAM-TOTCAN>0,TOTQTY-TOTSTK-TOTDAM-TOTCAN,0)
*  =SEEK(lcLKey+'1')
*ENDIF
*=lfRefresh(lcWindCh1)
*=lfCrUnSess('','',.T.)   
*SHOW WINDOW (lcBrowTitl) REFRESH
*RETURN
SELECT (lcTktSheet)
SET ORDER TO TAG lcTktShet2
=lfvEditQty()
SELECT (lcTktSheet)
SET ORDER TO TAG lcTktSheet
*C102172,4 AMH [End]
*-- end of lfvLtEdtQt.

*!*************************************************************
*! Name      : lfvByStQty
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/31/2001
*! Purpose   : valid the recieve & canceld & damaged qty
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvByStQty()
*!*************************************************************
*
FUNCTION lfvByStQty
PARAMETER llBStClose
lnAlias=SELECT()
IF ! laScrMode[2]
   *-- can not accept  negative qty 
  IF EVAL(SYS(18)) < 0 THEN
    = gfModalGen("TRM42000B00000","DIALOG",'')
    _CUROBJ = _CUROBJ
  ENDIF
  *-- if the use close line qty screen with damaged qty and empty(style)
  IF llBStClose
    *-You cannot leave the XXXX style empty since the XXXX quantity was entered.
    IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0 AND ;
       EMPTY(lcRetSty1)
       =gfModalGen('TRM34067B42000','DIALOG',lcRetSHd3+'|'+lcRetSHd3)
      _CUROBJ=OBJNUM(lcRetSty1) 
      SELECT(lnAlias)
      RETURN
    ENDIF
    IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0 AND ;
      EMPTY(lcRetSty2)
      =gfModalGen('TRM34067B42000','DIALOG',lcRetSHd4+'|'+lcRetSHd4)
      _CUROBJ=OBJNUM(lcRetSty2) 
      SELECT(lnAlias)
      RETURN
    ENDIF
    IF llWareHous
      *--if the style is not assign to the ware house
      IF !EMPTY(lcRetSty1) AND !SEEK(PADR(lcRetSty1,19)+lcWareCode+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcRetSty1)+'|'+lcWareCode) = 1
          DO gpAdStyWar WITH lcRetSty1,SPACE(10),lcWareCode
        ELSE
          _CUROBJ=OBJNUM(lcRetSty1) 
          SELECT(lnAlias)
          RETURN
        ENDIF
      ENDIF
      *--if the style is not assign to the ware house
      IF !EMPTY(lcRetSty2) AND !SEEK(PADR(lcRetSty2,19)+lcWareCode+SPACE(10),'STYDYE')
        *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcRetSty2)+'|'+lcWareCode) = 1
          DO gpAdStyWar WITH lcRetSty2,SPACE(10),lcWareCode
        ELSE
          _CUROBJ=OBJNUM(lcRetSty2) 
          SELECT(lnAlias)
          RETURN
        ENDIF
      ENDIF
    ENDIF
    *Check if other style is a dyelot yes style but the original style was No.
    IF llDyelot AND EMPTY(lcDyelot)
      IF !EMPTY(lcRetSty1) AND SEEK(lcRetSty1,'STYLE') AND STYLE.CDYE_FLG='Y'
        _CUROBJ=OBJNUM(lcRetSty1)
        *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
        *--Please make sure the the other quality style has same dyelot usage.
        =gfModalGen('TRM34135B42000','DIALOG',lcRetSty1+'|'+lcStyle)
        SELECT(lnAlias)
        RETURN
      ENDIF
      IF !EMPTY(lcRetSty2) AND SEEK(lcRetSty2,'STYLE') AND STYLE.CDYE_FLG='Y' 
        *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
        *--Please make sure the the other quality style has same dyelot usage.
        =gfModalGen('TRM34135B42000','DIALOG',lcRetSty2+'|'+lcStyle)
        _CUROBJ=OBJNUM(lcRetSty2)
        SELECT(lnAlias)
        RETURN
      ENDIF
    ENDIF
    CLEAR READ
  ENDIF
  *--enable/disable second qty style field in case of second qty
  IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
    IF EMPTY(lcRetSty1)
      lcRetSty1=STYLE.cRetSty
    ENDIF
    SHOW GET lcRetSty1 ENABLE
    SHOW GET ibRtSt1   ENABLE
  ELSE
    lcRetSty1=SPACE(19)
    SHOW GET lcRetSty1 DISABLE
    SHOW GET ibRtSt1   DISABLE
  ENDIF
  IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
    IF EMPTY(lcRetSty2)
      lcRetSty2=STYLE.cRetSty2
    ENDIF
    SHOW GET lcRetSty2 ENABLE
    SHOW GET ibRtSt2   ENABLE
  ELSE
    lcRetSty2=SPACE(19)
    SHOW GET lcRetSty2 DISABLE
    SHOW GET ibRtSt2   DISABLE
  ENDIF
  =lfRefresh("mfLtRcvQ")
ENDIF
SELECT(lnAlias)
RETURN
*-- end of lfvByStQty.

*!*************************************************************
*! Name      : lfvFactory
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/31/2001
*! Purpose   : valid the Factory field.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvFactory()
*!*************************************************************
*
FUNCTION lfvFactory

PRIVATE lnAlias
lnAlias = SELECT(0)
= gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VENCODE','SH')
SELECT APVENDOR
lcFactory = PADR(ALLTRIM(lcFactory),8)

IF llBrowse .OR. ATC("?",lcFActory) > 0 .OR. (!EMPTY(lcFactory) .AND. (!SEEK(lcFactory) .OR. !SEEK(lcFactory,'TRANSHDR')))
  DIMENSION laTemp[1]
  laTemp = ''
  lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                CVENCOMP :H= 'Company',;
                CPHONENO :H= 'Phone'"
  lcFile_Ttl = "Vendor"

  =gfBrows('','CVENDCODE','laTemp')
  llBrowse  = .F.
  lcFactory = laTemp[1]
  IF EMPTY(laTemp[1])
    _CUROBJ = OBJNUM(lcFactory)
  ENDIF
ENDIF
= gfCloseFile('APVENDOR')
SELECT (lnAlias)
*-- end of lfvFactory.

*!*************************************************************
*! Name      : lfvInvoice
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/31/2001
*! Purpose   : valid the Invoice field.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvInvoice()
*!*************************************************************
*
FUNCTION lfvInvoice

PRIVATE lnAlias, lcOrder, lnI, lcI
IF EMPTY(lcInvoice)
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT TRANSHDR
IF !SEEK(lcFactory+SUBSTR(lcType,1,1)+lcInvoice)
  DIMENSION laTemp[5]
  laTemp = ''
  
  *B605913,1 AMH Conceder the cancel invoice status in browse fields [Start]
  *lcBrFields = "CCUTVEN   :H= 'Factory',;
                lcBrType=IIF(CSHIP_TYPE='R','Regular',IIF(CSHIP_TYPE='S','Seconds','Balances')) :H= 'Rec Type',;
                CCUTINV   :H= 'Invoice',"+;               
               "lcInvStats=IIF(TransHdr.Status='O','Open','Complete') :10:H='Status',;
                CCONTANR  :H= 'Container',;
                ETA       :H= 'E.T.A.'"
  lcBrFields = "CCUTVEN   :H= 'Factory',;
                lcBrType=IIF(CSHIP_TYPE='R','Regular',IIF(CSHIP_TYPE='S','Seconds','Balances')) :H= 'Rec Type',;
                CCUTINV   :H= 'Invoice',"+;               
               "lcInvStats=IIF(TransHdr.Status='O','Open',IIF(TRANSHDR.STATUS='C','Complete','Canceled')) :10:H='Status',;
                CCONTANR  :H= 'Container',;
                ETA       :H= 'E.T.A.'"
  *B605913,1 AMH [End]
 
  lcFile_Ttl = "Invoice"
  lcBrKey = IIF(SEEK(lcFactory),[lcFactory],[])
  =gfBrows(lcBrKey,'CCUTVEN,CSHIP_TYPE,CCUTINV,CCONTANR,ETA','laTemp')
  lcInvoice  = laTemp[3]
  IF !EMPTY(laTemp[1])
    lcFactory  = laTemp[1]
    lcType     = IIF(laTemp[2]='R','Regular',IIF(laTemp[2]='S','Seconds','Balances'))
    lcContaner = laTemp[4]
    ldEta      = laTemp[5]
  ELSE
    _CUROBJ = OBJNUM(lcInvoice)
    SELECT (lnAlias)
    RETURN
  ENDIF
ENDIF
IF TRANSHDR.STATUS = 'C'
  = gfModalGen('INM00000B42000',.F.,.F.,.F.,'Invoice # '+ALLTRIM(lcInvoice)+' has been completly received. Cannot proceed.')
  
  *B605913,1 AMH Don't contiuing invoice validation if it is completed [Start]
  STORE SPACE(0) TO lcContaner, lcInvoice, lcFactory
  STORE 'Regular' TO lcType
  STORE {  /  /  } TO ldEta
  _CUROBJ=OBJNUM(lcInvoice)
  =lfShwLtScr()
  SELECT (lnAlias)
  RETURN
  *B605913,1 AMH [End]
  
ENDIF

*B605913,1 AMH Privant to recieving from canceled invoice [Start]
IF TRANSHDR.STATUS = 'X'
  = gfModalGen('INM00000B42000',.F.,.F.,.F.,'Invoice # '+ALLTRIM(lcInvoice)+' has been canceled. Cannot proceed.')
  STORE SPACE(0) TO lcContaner, lcInvoice, lcFactory
  STORE 'Regular' TO lcType
  STORE {  /  /  } TO ldEta
  _CUROBJ=OBJNUM(lcInvoice)
  =lfShwLtScr()
  SELECT (lnAlias)
  RETURN
ENDIF
*B605913,1 AMH [End]

lcContaner = CCONTANR
ldEta      = ETA
SELECT CUTTKTL
lcOrder = SET('ORDER')
SET ORDER TO TAG CUTTKTLI
*khalid
*IF SEEK(lcFactory+SUBSTR(lcType,1,1)+ALLTRIM(lcInvoice))
IF SEEK(lcFactory+SUBSTR(lcType,1,1)+PADR(lcInvoice,10))
*khalid
  lcReject = SPACE(6)
  llAllCmplt = .T.
  llAddRec = .F.
  *khalid
  *SCAN REST WHILE ccutven+cship_type+ccutinv+style+STR(nlot_no,4)+trancd+STR(RECNO(),7) = lcFactory+SUBSTR(lcType,1,1)+ALLTRIM(lcInvoice);
            FOR IIF(SUBSTR(lcType,1,1)='S',TRANCD='6',TRANCD='5')
  SCAN REST WHILE ccutven+cship_type+ccutinv+style+STR(nlot_no,4)+trancd+STR(RECNO(),7) = lcFactory+SUBSTR(lcType,1,1)+PADR(lcInvoice,10);
            FOR IIF(SUBSTR(lcType,1,1)='S',TRANCD='6',TRANCD='5')
  *khalid
    lcCtkt = cuttkt
    lcRecStyle = style
    IF lcCtkt $ lcReject
      LOOP
    ENDIF
    IF SEEK(CUTTKT,'CUTTKTH')
      IF CUTTKTH.Status <> 'C'
        llAllCmplt = .F.
      ENDIF
      DO CASE
        CASE CUTTKTH.Status = 'S'
          = gfModalGen('INM00000B42000',.F.,.F.,.F.,'The cutting ticket '+lcCtkt+' status is closed.'+;
                       ' Therefore,no receivings can be done for this cutting ticket. It will be '+;
                       'omitted from the invoice.')
          lcReject = lcReject + '|' + lcCtkt
          LOOP
        CASE CUTTKTH.Status = 'H'
          = gfModalGen('INM00000B42000',.F.,.F.,.F.,'The cutting ticket '+lcCtkt+' status is hold '+;
                       'since a cutting ticket cost sheet has not been created yet.'+;
                       ' Therefore,no receivings can be done for this cutting ticket. It will be '+;
                       'omitted from the invoice.')
          lcReject = lcReject + '|' + lcCtkt
          LOOP
        CASE CUTTKTH.Status = 'X'
          = gfModalGen('INM00000B42000',.F.,.F.,.F.,'The cutting ticket '+lcCtkt+' status is canceled.'+;
                       ' Therefore,no receivings can be done for this cutting ticket. It will be '+;
                       'omitted from the invoice.')
          lcReject = lcReject + '|' + lcCtkt
          LOOP
        CASE CUTTKTH.Status = 'C'
          lcReject = lcReject + '|' + lcCtkt
          LOOP
      ENDCASE
    ENDIF
    SELECT CUTTKTL
    lnRecNoInv = RECNO()
    SET ORDER TO &lcOrder.
    IF SEEK(lcCtkt+lcRecStyle)
      SCAN REST WHILE CUTTKT+STYLE+DYELOT+TRANCD = lcCtkt+lcRecStyle
        SCATTER MEMVAR
        SELECT (lcTktSheet)
        STORE 0 TO m.Totcan, m.TotStk, m.TotDam, m.Totbal
        IF m.Trancd $ '156'
          IF m.Trancd <> '1' 
            IF SEEK(lcCtkt+lcRecStyle)
              REPLACE TOTSTK WITH IIF(m.Trancd='5',m.TotQty,0),;
                      TOTDAM WITH IIF(m.Trancd='5',0,m.TotQty)
              REPLACE TOTBAL WITH IIF(TOTQTY-TOTSTK-TOTDAM>0,TOTQTY-TOTSTK-TOTDAM,0)
            ENDIF
            APPEND BLANK
            GATHER MEMVAR
            m.Trancd = IIF(m.Trancd='5','2','3')
          ENDIF
          APPEND BLANK
        ELSE
          m.TotQty = 0
          lnI = 1
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            m.Qty&lcI. = Qty&lcI. - m.Qty&lcI.
            m.TotQty = m.TotQty + m.Qty&lcI.
          ENDFOR
          m.Trancd = '1'
        ENDIF
        GATHER MEMVAR
      ENDSCAN
      llAddRec = .T.
    ELSE
      *-The lines for this 'C/T' are missing ! cannot proceed.
      =gfModalGen('TRM34017B42000','DIALOG','C/T')
    ENDIF
    SELECT CUTTKTL
    SET ORDER TO TAG CUTTKTLI
    IF BETWEEN(lnRecNoInv,1,RECCOUNT())
      GOTO lnRecNoInv
    ENDIF
  ENDSCAN
  IF llAllCmplt
    = gfModalGen('INM00000B42000',.F.,.F.,.F.,'All cutting tickets have been completely received. Cannot proceed.')
  ENDIF
ELSE
  *-The lines for this 'C/T' are missing ! cannot proceed.
  =gfModalGen('TRM34017B42000','DIALOG','C/T')
ENDIF
SELECT CUTTKTL
SET ORDER TO &lcOrder.
IF !llAddRec
  STORE SPACE(0) TO lcContaner, lcInvoice, lcFactory
  STORE 'Regular' TO lcType
  STORE {  /  /  } TO ldEta
  _CUROBJ=OBJNUM(lcInvoice)
ENDIF
=lfShwLtScr()
SELECT (lnAlias)
*-- end of lfvInvoice.

*!*************************************************************
*! Name      : lfDistribt()
*! Developer : AMH (Ahmed Maher)
*! Date      : 02/07/2001
*! Purpose   : Distribute the over quantity among the lots 
*!           : starting from the oldest for a style/color
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfDistribt()
*!*************************************************************
*
FUNCTION lfDistribt

PRIVATE lnAlias, lcTktShet2, laOpenQty, laReminder, lnI, lcI, llNeedDist, lcOrder, lcNewCtkt,;
        laNewOpen, laNewDist, llExit, lcxCuttkt, lcxStyle, lcxDyelot, lcxTrancd, llOverDist
lnAlias = SELECT(0)
lcTktShet2=gfTempName()
DIMENSION laOpenQty[8], laReminder[8], laNewOpen[8], laNewDist[9]
STORE 0 TO laOPenQty, laReminder, laNewOpen, laNewDist
USE (gcWorkDir+lcTktSheet) AGAIN IN 0 ALIAS (lcTktShet2)
SELECT (lcTktSheet)
SET ORDER TO TAG lcTktShet1
LOCATE
SCAN REST WHILE TRANCD = '1'
  lnI = 1
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laOpenQty[lnI] = EVALUATE('Qty'+lcI)
  ENDFOR
  lcxCuttkt = CUTTKT
  lcxStyle  = STYLE
  lcxDyelot = DYELOT
  lcxTrancd = TRANCD
  SELECT (lcTktShet2)
  SET ORDER TO TAG lcTktSheet
  SEEK lcxCuttkt+lcxStyle+lcxDyelot+lcxTrancd
  SCAN REST WHILE cuttkt+style+dyelot+trancd = lcxCuttkt+lcxStyle+lcxDyelot FOR trancd $ '234'
    llNeedDist = .F.
    lnI = 1
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      llNeedDist = IIF(laOpenQty[lnI]>EVALUATE('Qty'+lcI) .AND. !llNeedDist,.F.,.T.)
    ENDFOR
    IF llNeedDist
      lnI = 1
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        laReminder[lnI] = IIF(laOpenQty[lnI]>EVALUATE('Qty'+lcI),0,EVALUATE('Qty'+lcI)-laOpenQty[lnI])
        IF laReminder[lnI] > 0
          REPLACE Qty&lcI. WITH laOpenQty[lnI], TotQty WITH TotQty - laReminder[lnI]
        ENDIF
        laOpenQty[lnI] = MAX(laOpenQty[lnI]-EVALUATE('Qty'+lcI),0)
      ENDFOR
      lcxTrancd = trancd
      SELECT CUTTKTL
      lcOrder = SET('ORDER')
      SET ORDER TO TAG CUTTKTLS
      SEEK(lcxStyle)
      lcNewCtkt = SPACE(6)
      llExit = .T.
      SCAN REST WHILE style+cuttkt+trancd = lcxStyle FOR cuttkt <> lcxCuttkt
        IF !SEEK(cuttkt,'CUTTKTH') .OR. CUTTKTH.status <>'A'
          LOOP
        ENDIF
        IF trancd $ '56'
          LOOP
        ENDIF
        IF lcNewCtkt <> cuttkt
          IF !EMPTY(lcNewCtkt)
            lnI = 1
            FOR lnI = 1 TO 8
              laNewDist[lnI] = IIF(laNewOpen[lnI]>laReminder[lnI],laReminder[lnI],laNewOpen[lnI])
              laNewDist[9] = laNewDist[9] + laNewDist[lnI]
              laReminder[lnI] = IIF(laNewOpen[lnI]>laReminder[lnI],0,laReminder[lnI]-laNewOpen[lnI])
              llExit = IIF(laReminder[lnI]=0 .AND. llExit,.T.,.F.)
            ENDFOR
            SELECT (lcTktSheet)
            APPEND BLANK
            GATHER MEMVAR
            REPLACE TRANCD WITH lcxTrancd, TOTQTY WITH laNewDist[9]
            lnI = 1
            FOR lnI = 1 TO 8
              lcI = STR(lnI,1)
              REPLACE QTY&lcI. WITH laNewDist[lnI]
            ENDFOR
            SELECT CUTTKTL
            IF llExit
              EXIT
            ENDIF
          ENDIF
          SCATTER MEMVAR
          lcNewCtkt = cuttkt
          STORE 0 TO laNewOpen, laNewDist
        ENDIF
        lnI = 1
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          laNewOpen[lnI] = IIF(trancd='1',laNewOpen[lnI]+Qty&lcI.,MAX(laNewOpen[lnI]-Qty&lcI,0))
        ENDFOR
      ENDSCAN
      SELECT CUTTKTL
      SET ORDER TO &lcOrder.
      IF !EMPTY(lcNewCtkt) .AND. !llExit
        llOverDist = .F.
        lnI = 1
        FOR lnI = 1 TO 8
          laNewDist[lnI] = IIF(laNewOpen[lnI]>laReminder[lnI],laReminder[lnI],laNewOpen[lnI])
          laNewDist[9] = laNewDist[9] + laNewDist[lnI]
          laReminder[lnI] = IIF(laNewOpen[lnI]>laReminder[lnI],0,laReminder[lnI]-laNewOpen[lnI])
          llOverDist = IIF(laReminder[lnI]=0 .AND. !llOverDist,.F.,.T.)
        ENDFOR
        SELECT (lcTktSheet)
        APPEND BLANK
        GATHER MEMVAR
        REPLACE TRANCD WITH lcxTrancd, TOTQTY WITH laNewDist[9]
        lnI = 1
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          REPLACE QTY&lcI. WITH laNewDist[lnI]
        ENDFOR
        IF llOverDist
          lnI = 1
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            REPLACE QTY&lcI. WITH QTY&lcI. + laReminder[lnI], TOTQTY WITH TOTQTY + laReminder[lnI]
            laOpenQty[lnI] = MAX(laOpenQty[lnI]-laReminder[lnI],0)
          ENDFOR
        ENDIF
        SELECT (lcTktShet2)
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN
USE IN (lcTktShet2)
SELECT (lnAlias)
*-- end of lfDistribt.