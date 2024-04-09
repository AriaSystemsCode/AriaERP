*:************************************************************************
*: Program file  : MFRECST.PRG             *C101683,1 
*: Program desc. : Receiving C/T By style.
*: For screen    : MFRECST.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: MF
*:      Developer: WAB - Walid A. Wahab
*:           Date: 12/01/1999             
*:************************************************************************
*: Passed Parameters :-
*:************************************************************************
STORE ' ' TO lcWindCh1,lcWindCh2
*--lcWindCh1  ---> hold first window name 
*--lcWindCh2  ---> hold Second window name
*--lcWindCh1  ---> hold first window name
*--lcxrefer   ---> refrence field
*--lcxdyelot  ---> dyelot field
*--lcxstyle   ---> style/color field
*--lcxStyDesc ---> style/color description
*--lcBrowTitl  ---> hold window title
*--lnByStWare ---> ware house popup
*--lcTktSheet ---> temp file name for receiving by style/color
*--lcStyleSt  ---> hold 'DISABLE'/'ENABLE' for showing style/color field
*--lcWareSt   ---> hold 'DISABLE'/'ENABLE' for showing Ware house popup
*--lcDyelotSt ---> hold 'DISABLE'/'ENABLE' for showing dyelot field
*--lcnewst    ---> hold 'DISABLE'/'ENABLE' for showing new Butt.
*--lcremst    ---> hold 'DISABLE'/'ENABLE' for showing remove Butt.
lcWindCh1 = gfTempName()
lcWindCh2 = gfTempName()
STORE " " TO lcxrefer,lcxdyelot,lcxstyle,lcxStyDesc
STORE 'Line Quantity' TO lcByStTtl
STORE 0 TO lnByStWare
lnxware=1
lcSkMode='DISABLE' 
lcTktSheet=gfTempName()
*--call Function to create temp file for receiving by style
=lfStyleTmp()
lcStyleSt = 'DISABLE'
lcWareSt  = 'DISABLE'
lcDyelotSt= 'DISABLE'
lcnewst = 'ENABLE'
lcremst = 'DISABLE'
llFrist = .T.
lcBrowTitl  = 'Receive By Style'

*-- Save trapping setting .
lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

ON KEY LABEL ALT+Z ACTIVATE WINDOW (lcBrowTitl)
*--Call Function to convert the cuttkt line from (lctempline) to the (lctktsheet)
*--by Accumulate style qty
=lfConvert()

*-- call screen recieving by style
DO (gcScrDir+gcWinAppl+"\MFRECST.SPX")

*--Call Function to convert the recieving style qty to cuttkt line 
=lfUnConvrt()

*-- if there is style selected by the user --> llfoundRec = .T.
IF RECCOUNT(lcTktSheet) > 0 
  llFoundRec = .T.
ENDIF
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
llByStyle = .F.
RETURN

*!*************************************************************
*! Name      : lfStyleTmp
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : create temp file for receiving by style
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfStyleTmp()
*!*************************************************************
FUNCTION lfStyleTmp
DIMENSION laFileStru[1,4]
SELECT (lcTmpLine)
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTktSheet) FROM ARRAY laFileStru
INDEX ON TRANCD+STYLE+Dyelot+cWareCode TAG lcTktSheet
RETURN
*!*************************************************************
*! Name      : lfDeact
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : Trap keys (TAB - BACKTAB - ESCAPE)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfDeact()
*!*************************************************************
FUNCTION lfDeact
*-- Trap keys in this function
IF WONTOP()=(lcBrowTitl)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpStTrap   
  ON KEY LABEL BACKTAB    DO lpStShTrap
  ON KEY LABEL ESCAPE     LLDUMMY1=.T.
ENDIF
RETURN
*!*************************************************************
*! Name      : lpStTrap
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : Trap key (  TAB  )
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : DO lpStTrap
*!*************************************************************
PROCEDURE lpStTrap
ON KEY LABEL TAB
IF WONTOP()=(lcBrowTitl)
  ACTIVATE WINDOW (lcWindCh2)
  _CUROBJ=OBJNUM(lcxStyle)
ELSE
   ACTIVATE WINDOW (lcBrowTitl)
ENDIF  
RETURN
*!*************************************************************
*! Name      : lpStShTrap
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : Trap key (  BACKTAB  )
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : DO lpStShTrap
*!*************************************************************
PROCEDURE lpStShTrap
ON KEY LABEL BACKTAB
IF WONTOP()=(lcBrowTitl)
  ACTIVATE WINDOW (lcWindCh2)
  _CUROBJ=OBJNUM(pbClose)
ELSE
  ACTIVATE WINDOW (lcBrowTitl)
ENDIF
RETURN
*!*************************************************************
*! Name      : lpStShTrap
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : *---Untrap keys ( TAB -  BACKTAB - ESCAPE )
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfAct()
*!*************************************************************
FUNCTION lfAct
IF WONTOP() # (lcBrowTitl)  
  IF glFromBrow
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    ON KEY LABEL ESC DO gfEscap
    =gfStopBrow()
  ENDIF
ENDIF  
*!*************************************************************
*! Name      : lfvReturn
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : when user press close Butt.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfvReturn()
*!*************************************************************
FUNCTION lfvReturn
*--lnRecNo    ---> hold  current record no
*--llRecFound ---> determine if there are an record without qty
lnRecNo = RECNO()
llRecFound = .F.
GO TOP 
*--scan only for budjet qty ( trancd = 1 )
SCAN FOR TranCd = '1'
  *-- if there is no qty
  IF TotStk = 0 AND TotDam = 0 AND TotCan = 0
    =gfModalGen('TRM00000B52361',.F.,.F.,.F.,'You cannot leave the line Quantity empty.')
    llRecFound = .T.
    EXIT
  ENDIF
ENDSCAN
*--if there is qty for all lines close screen
IF !llRecFound
  CLEAR READ
ELSE
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF  
ENDIF
RETURN
*!*************************************************************
*! Name      : lfByStActB
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : browse the receiving by style temp file
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfByStActB()
*!*************************************************************
FUNCTION lfByStActB
lcBrTpNme = 'C/T #'
lnBrRecNo = RECNO()
*--lcBlFields ---> hold the fields name we need its to browse
lcBlFields = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "Style     :R :H=lcStyHdr :26,"+;
             IIF(llDyelot,"Dyelot :R :17,","")+;
             IIF(llWareHous ,"lcWHTtl = IIF(lcPType$'NA',Vendor,cWareCode) :H=IIF(lcPType$'NA','Source','Location'):10,","")+;
             IIF(lcPType$'NA',"cWareCode :H='Target':12,","")+;
             "TotStk :R :H='Stock'   :10,"+;
             "TotDam :R :H='Other'   :10,"+;
             "TotCan :R :H='Cancel'  :10,"+;
             "Reference :R :30"
SELECT (lcTktSheet)
GO TOP 
BROWSE FIELDS &lcBlfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       KEY '1'  ;       
       TITLE (lcBrowTitl) ;
       WHEN lfByStwBrw()     ;       
       VALID :F lfByStVBrw();         
       WINDOW (lcWindCh1) ;
       IN WINDOW MFRECST
RETURN
*!*************************************************************
*! Name      : lfByStwBrw
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : When valid function for browse.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfByStwBrw()
*!*************************************************************
FUNCTION lfByStwBrw
lnBrRecNo = RECNO()
*--call function to read line and show lcWindCh2 window 
=lfReadSTLn(EOF())
glFromBrow = .T.
RETURN
*!*************************************************************
*! Name      : lfByStVBrw
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : valid function for browse.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfByStVBrw()
*!*************************************************************
FUNCTION lfByStVBrw
IF WONTOP() # (lcBrowTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*!*************************************************************
*! Name      : lfConvert
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/03/1999
*! Purpose   : convert the cuttkt line from (lctempline) to the (lctktsheet)
*!             by Accumulate style qty
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfConvert()
*!*************************************************************
FUNCTION lfConvert
*--get the record from (lctmpline) record by record and check if the style
*--is not exist in the (lctktsheet) so we add new line else accumulate qty to 
*--the exist line
SELECT (lcTmpLine) 
GO TOP
SCAN
SCATTER TO laByStyle MEMO
SELECT (lcTktSheet)
IF !SEEK(&lcTmpLine..TRANCD+&lcTmpLine..STYLE+&lcTmpLine..Dyelot+&lcTmpLine..cWareCode) ;
  OR (SEEK(&lcTmpLine..TRANCD+&lcTmpLine..STYLE+&lcTmpLine..Dyelot+&lcTmpLine..cWareCode) AND ;
      cStyGrade <> &lcTmpLine..cStyGrade AND  &lcTmpLine..TRANCD = '3')
  APPEND BLANK
  GATHER FROM laByStyle MEMO
ELSE
REPLACE QTY1      WITH QTY1   + &lcTmpLine..QTY1   ,;
        QTY2      WITH QTY2   + &lcTmpLine..QTY2   ,;
        QTY3      WITH QTY3   + &lcTmpLine..QTY3   ,;
        QTY4      WITH QTY4   + &lcTmpLine..QTY4   ,;
        QTY5      WITH QTY5   + &lcTmpLine..QTY5   ,;
        QTY6      WITH QTY6   + &lcTmpLine..QTY6   ,;
        QTY7      WITH QTY7   + &lcTmpLine..QTY7   ,;
        QTY8      WITH QTY8   + &lcTmpLine..QTY8   ,;
        TotStk    WITH TotStk + &lcTmpLine..TotStk ,;
        TotDam    WITH TotDam + &lcTmpLine..TotDam ,;
        TotCan    WITH TotCan + &lcTmpLine..TotCan ,;
        cRetSty   WITH &lcTmpLine..cRetSty         ,;
        cStyGrade WITH &lcTmpLine..cStyGrade
ENDIF
ENDSCAN
SELECT (lcTktSheet)
RETURN
*!*************************************************************
*! Name      : lfvStyQty
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/04/1999
*! Purpose   : valid the line qty butt.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example     : =lfvStyQty()
*!*************************************************************
FUNCTION lfvStyQty
PRIVATE lcCuttlOrd , lcCuttlPos , lnCurAlias
*-- if user didn't select ware house in case of multiware house
IF llWareHous AND EMPTY(lcWareCode)
  *-- Message : 'First, you must select location.'
  =gfModalGen('TRM42150B42001','DIALOG')
  _CUROBJ = OBJNUM(lnByStWare)
  RETURN
ENDIF
SELECT (lcTktSheet)
SCATTER MEMVAR
lcLKey=Style+Dyelot+PADR(cWareCode,6)
DECLARE  laSok[8],laDam1[8],laDam2[8],laCan[8]
STORE 0 TO laSok,laDam1,laDam2,laCan
DECLARE laOldOut[8]
STORE 0 TO laOldOut

*--New Origenal and order quantity.
=SEEK('1'+lcLKey)
=SEEK(Style,'STYLE')
=SEEK('S'+STYLE.Scale,'SCALE')
*--get the scale sizes
lcSz1=SCALE.Sz1
lcSz2=SCALE.Sz2
lcSz3=SCALE.Sz3
lcSz4=SCALE.Sz4
lcSz5=SCALE.Sz5
lcSz6=SCALE.Sz6
lcSz7=SCALE.Sz7
lcSz8=SCALE.Sz8

STORE '' TO lcRetSty1,lcRetSty2,lcRetSHd,lcRetSHd1,lcRetSHd2,lcRetSHd3,lcRetSHd4
lcMStyQlty=STYLE.cStyGrade
lcSndGrd = "2"
lcTrdGrd = "3"
DO CASE
  CASE lcMStyQlty='1'
    lcRetSHd ="1st Quality"
    IF lcPType<>'A'
      lcRetSHd1="2nd Quality"
      lcRetSHd2="Damaged"
      lcRetSHd3="Second Quality"
      lcRetSHd4="Damaged"
    ENDIF
  CASE lcMStyQlty='2'
    lcRetSHd ="2nd Quality"
    IF lcPType<>'A'
      lcRetSHd1="1st Quality"
      lcRetSHd2="Damaged"
      lcRetSHd3="First Quality"
      lcRetSHd4="Damaged"
    ENDIF
    lcSndGrd = "1"
    lcTrdGrd = "3"
  CASE lcMStyQlty='3'
    lcRetSHd ="Damaged"
    IF lcPType<>'A'
      lcRetSHd1="1st Quality"
      lcRetSHd2="2nd Quality"    
      lcRetSHd3="First Quality"
      lcRetSHd4="Second Quality"
    ENDIF
    lcSndGrd = "1"
    lcTrdGrd = "2"
ENDCASE

*--1) Get Stock quantity.
IF SEEK('2'+lcLKey)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSok
ENDIF

*--2) Get Damage quantity.
STORE 'DISABLE' TO lcRSt1Stat,lcRSt2Stat
IF SEEK(lcOthrTrCd+lcLKey)
  SCAN REST WHILE TranCd+Style+Dyelot+PADR(cWareCode,6)=lcOthrTrCd+lcLKey
    IF cStyGrade = lcSndGrd
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam1
      lcRetSty1=cRetSty
      lcRSt1Stat='ENABLE'
    ELSE
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam2
      lcRetSty2=cRetSty
      lcRSt2Stat='ENABLE'
    ENDIF
  ENDSCAN
ENDIF
*--3) Get Cancel quantity.
IF SEEK(lcCanlTrCd+lcLKey)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laCan
ENDIF
lcSkMode = 'ENABLE'
lcOtMode = 'ENABLE'
IF lcPType $ 'NA'
  lcOtMode = 'DISABLE'
  lcRSt1Stat = 'DISABLE'
  lcRSt2Stat = 'DISABLE'
ENDIF

=SEEK('1'+lcLKey)
lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*calculate old values of Out quantities from this line.
FOR lnI = 1 TO 8
  laOldOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
ENDFOR

*--Call line quantity screen.
DO (gcScrDir+gcWinAppl+"\mfStRcvQ.SPX")

ON KEY LABEL TAB     &lcHldTab
ON KEY LABEL BACKTAB &lcHldBtb
ON KEY LABEL ENTER   &lcHldEnt

*--Get Totals.
lnTStk =laSok[1]+laSok[2]+laSok[3]+laSok[4]+laSok[5]+laSok[6]+laSok[7]+laSok[8]
lnTCan =laCan[1]+laCan[2]+laCan[3]+laCan[4]+laCan[5]+laCan[6]+laCan[7]+laCan[8]
lnTDam1=laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]
lnTDam2=laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]
lnTDam =lnTDam1+lnTDam2

*--1) Update stock quantity.
IF SEEK('2'+lcLKey)
  IF lnTStk<>0
    GATHER FROM laSok FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty WITH lnTStk
  ELSE
    BLANK
    DELETE
  ENDIF
ELSE
  IF lnTStk<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd WITH '2',;
            cStyGrade WITH lcMStyQlty
    GATHER FROM laSok  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 
    REPLACE TotQty WITH lnTStk
  ENDIF
ENDIF
*--2) Update Damaged quantity.
=SEEK(lcOthrTrCd+lcLKey)
LOCATE REST WHILE Trancd+Style+Dyelot+PADR(cWareCode,6)=lcOthrTrCd+lcLKey ;
              FOR cStyGrade=lcSndGrd
IF FOUND()
  IF lnTDam1=0
    BLANK
    DELETE
  ELSE
    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty  WITH lnTDam1,;
            cRetSty WITH lcRetSty1
  ENDIF
ELSE
  IF lnTDam1<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH lcOthrTrCd,;
            cStyGrade WITH lcSndGrd,;
            cRetSty   WITH lcRetSty1,;
            TotQty    WITH lnTDam1
    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
  ENDIF
ENDIF

=SEEK(lcOthrTrCd+lcLKey)
LOCATE REST WHILE Trancd+Style+Dyelot+PADR(cWareCode,6)=lcOthrTrCd+lcLKey ;
              FOR cStyGrade=lcTrdGrd
IF FOUND()
  IF lnTDam2=0
    BLANK
    DELETE
  ELSE
    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty  WITH lnTDam2,;
            cRetSty WITH lcRetSty2
  ENDIF
ELSE
  IF lnTDam2<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH lcOthrTrCd,;
            cStyGrade WITH lcTrdGrd,;
            cRetSty   WITH lcRetSty2,;
            TotQty    WITH lnTDam2
    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
  ENDIF
ENDIF
*--3) Update cancel quantity.
IF SEEK(lcCanlTrCd+lcLKey)
  IF lnTCan<>0
    GATHER FROM laCan FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty WITH lnTCan
  ELSE
    BLANK 
    DELETE
  ENDIF
ELSE
  IF lnTCan<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd WITH lcCanlTrCd,;
            TotQty WITH lnTCan
    GATHER FROM laCan  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
  ENDIF
ENDIF
IF SEEK('1'+lcLKey)
  REPLACE TotStk WITH lnTStk,;
          TotDam WITH lnTDam,;
          TotCan WITH lnTCan
ENDIF
IF (lcPType $ 'ISMD')
  DIMENSION laOut[8]
  STORE 0 TO lnTotOut,laOut
  FOR lnI = 1 TO 8
    Z = STR(lnI,1)
    laOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
    lnTotOut   = lnTotOut + laOut[lnI]
  ENDFOR
  *-- Calculate Out Quantity in this Line. [BEGIN]
  lcCurLine =  cCarton+IIF(llMFCall,Cuttkt,Po)+Style+STR(LineNo,6)
  *-- Subtract out quantity from the same lines with another dyelots.
  lcScanVar = [Trancd+Style]
  SCAN FOR EVALUATE(lcScanVar) = '1' + lcCurLine AND ;
           DYELOT+cWareCode # lcDyelot+lcWareCode
    FOR lnI = 1 TO 8
        lcZ = STR(lnI,1)  
        REPLACE QTY&lcZ WITH QTY&lcZ + laOldOut[lnI] - laOut[lnI] ,;
                TOTQTY  WITH TOTQTY  + laOldOut[lnI] - laOut[lnI]
    ENDFOR 
    REPLACE TOTBAL WITH TOTQTY-TOTSTK-TOTDAM-TOTCAN
  ENDSCAN
  =SEEK('1'+lcLKey)
ENDIF
=lfRefresh(lcWindCh1)
=lfCrUnSess('','',.T.)   
SHOW WINDOW (lcBrowTitl) REFRESH
RETURN

*!*************************************************************
*! Name      : lfReadSTLn
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : read line from record and show lcWindCh2 window
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : llClearln ---->.F. in case of clear value 
*!                                    from fields 
*!*************************************************************
*! Example     : =lfReadSTLn()
*!*************************************************************
FUNCTION lfReadSTLn
PARA llClearLn
lnAlias = SELECT()
*-- if not need to clear value from fields get data from fields
IF ! llClearLn
  SELECT (lcTktSheet)
  lnBrRecNo  =RECNO()
  lcxstyle   = Style
  lnByStWare = ASCAN(laWare,cWareCode,1)
  lcxrefer   = Reference
  lcxdyelot  = Dyelot
  lcxStyDesc = IIF(SEEK(lcxstyle,'STYLE'),STYLE.Desc1,'')
ELSE
  *--clear value from fields
  STORE ' ' TO lcxstyle,lcxrefer,lcxStyDesc,lcxdyelot,lcWareCode
  STORE 0   TO lnByStWare
ENDIF
*--call function to show the window and disable or enable buttons
=lfShwStScr()
SHOW WINDOW (lcBrowTitl) REFRESH
=lfRefresh(lcWindCh2)
SELECT(lnAlias)
RETURN
*!*************************************************************
*! Name      : lfShwStScr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : show the window and disable or enable buttons
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     : =lfShwStScr()
*!*************************************************************
FUNCTION lfShwStScr
SHOW GETS WINDOW (lcWindCh2) DISABLE
lcNewSt = 'ENABLE'
IF !EOF(lcTktSheet)
  STORE 'ENABLE' TO lcRemSt,lcNewSt,lcWareSt
  STORE 'DISABLE' TO lcStyleSt
ELSE
  STORE 'DISABLE' TO lcRemSt,lcWareSt,lcStyleSt
ENDIF
SHOW WINDOW (lcBrowTitl) REFRESH
SHOW GET lcxstyle &lcStyleSt
SHOW GET pbxNew   &lcNewSt
SHOW GET pbxRem   &lcRemSt
SHOW GET pbEditQ  &lcRemSt
SHOW GET pbClose  ENABLE
SHOW GET lcxRefer &lcWareSt
SHOW GET lcxdyelot  &lcDyelotSt
IF llWareHous
  SHOW GET lnByStWare &lcWareSt
ENDIF
RETURN

*!*************************************************************
*! Name      : lfShwStScr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : show the window and disable or enable buttons
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     : =lfShwStScr()
*!*************************************************************
FUNCTION lfByStyNew
*--Clear Line.
lcPrvTRc = lcxstyle
=lfReadSTLn(.T.)
lcStyleSt = 'ENABLE'
SHOW GETS WINDOW (lcWindCh2) DISABLE
SHOW GET pbClose  ENABLE
SHOW GET lcxstyle &lcStyleSt
SHOW GET ibxStyle &lcStyleSt
_CUROBJ=OBJNUM(lcxstyle)
SHOW GET ibStyle ENABLE
RETURN
*!*************************************************************
*! Name      : lfByStvWar
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : valid the ware house popup
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfByStvWar()
*!*************************************************************
FUNCTION lfByStvWar
IF (lnByStWare= lcOldValue) OR (lcOldValue = 0) 
  RETURN
ENDIF  
IF (lnByStWare = 1) AND (lnByStWare <> lcOldValue)
  lnWare = lcOldValue
  lcWareCode=SUBSTR(laWare[lnByStWare],1,6)
  SHOW GET lnWare
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lcWareCode=SUBSTR(laWare[lnByStWare],1,6)
*--if the style is not assigned the selected warehouse
IF !SEEK(PADR(lcxstyle,19)+lcWareCode+SPACE(10),'STYDYE')
  *-Style: xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
  IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcxstyle)+'|'+lcWareCode) = 1
    DO gpAdStyWar WITH lcxstyle,SPACE(10),lcWareCode
  ELSE
    lnByStWare = lcOldValue
    lcWareCode=SUBSTR(laWare[lnByStWare],1,6)
    SHOW GET lnByStWare
    RETURN
  ENDIF
ENDIF
SELECT (lcTktSheet)
lnRecNo= RECNO()
lcLKey = Style+Dyelot+PADR(cWareCode,6)
SEEK '1'+lcLKey
*--replace all lines (budjet-canceled-damaged) with ware house code
REPLACE REST cWareCode WITH lcWareCode ;
       WHILE Style+Dyelot+cWareCode = lcLKey
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF  
SHOW WINDOW (lcBrowTitl) REFRESH
RETURN
*!*************************************************************
*! Name      : lfvByStQty
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : valid the recieve & canceld & damaged qty
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvByStQty()
*!*************************************************************
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
  =lfRefresh("mfStRcvQ")  
ENDIF
SELECT(lnAlias)
RETURN
*!*************************************************************
*! Name      : lfvByStQty
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : valid the style/color fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvByStQty()
*!*************************************************************
FUNCTION lfvByStyle
PRIVATE lnAlias,lcCuttOrd

lcxstyle=IIF(llBrowse,'?',lcxstyle)
IF EMPTY(lcxstyle)
  RETURN
ENDIF
*--lcCuttOrd --> hold the current order for cutktl file
lcCuttOrd = 'No'
lnAlias    = SELECT(0)
SELECT (lcTktSheet)
*--check if the style is already enetered 
IF SEEK('1'+lcxstyle+lcxDyelot)
  *--Style XXX is already entered .cannot add its entry again.
  = gfModalGen('INM42093B42000','DIALOG',lcxstyle)
  lcxStyle=SPACE(19)
  SHOW GET lcxStyle
 _CUROBJ=OBJNUM(lcxStyle) 
ENDIF  
SELECT STYLE
*-- Validate style
IF EMPTY(SUBSTR(lcxstyle,lnMjrWid+2))
  llbrowse = .F.
ENDIF
*-- browse the styles to select one
IF llbrowse OR ATC('?',lcxStyle) <> 0 OR (!EMPTY(PADR(lcxStyle,lnMjrWid)) .AND. !SEEK(lcxStyle,'STYLE'))
  llbrowse = .F.
  *-- Call gfStyBrw() to brows all Majors for domestic styles.
  lcxStyle = gfStyBrw('I',lcxStyle,"",.F.)
  SHOW GET lcxStyle
ENDIF
*-- if not a valid code or user press Esc or Cancel from gfStyBrw()  
IF EMPTY(PADR(lcxStyle,lnMjrWid))
  lcxStyle=SPACE(19)
  SHOW GET lcxStyle
 _CUROBJ=OBJNUM(lcxStyle) 
ELSE  && not empty of lcxStyle
  *-- if select style from Style brows 
  SELECT CUTTKTL
  lcCuttOrd = ORDER('CUTTKTL') 
  SET ORDER TO Cuttktls 
  SET RELATION TO Cuttktl.cuttkt INTO Cuttkth ADDITIVE
  =SEEK(lcxStyle,'CUTTKTL') 
  LOCATE REST WHILE Style = lcxStyle For CUTTKTH.STATUS $ 'AO'
  IF !Found()
    *-- There are no cutting tickets for this style .
    =gfModalGen('TRM38170B42001','DIALOG')
    lcxStyle=SPACE(19)
    SHOW GET lxcStyle
    _CUROBJ=OBJNUM(lcxStyle)
  ENDIF
ENDIF 
*-- return the order to the old one
IF lcCuttOrd <> 'No'
  SELECT CUTTKTL
  SET ORDER TO TAG &lcCuttOrd
  SET RELATION TO
ENDIF  
*-- add the style selected to the receive by style file
IF !EMPTY(lcxStyle)
  lcxStyDesc = IIF(SEEK(lcxstyle,'STYLE'),STYLE.Desc1,'')
  SELECT (lcTktSheet)
  APPEND BLANK
  REPLACE Style  WITH lcxStyle,;
          TranCd WITH '1'
  lcStyleSt = 'DISABLE'          
  lnBrRecNo=RECNO()
  IF llDyelot AND STYLE.cDye_Flg = 'Y'
     lcDyelotSt= 'ENABLE'
  ELSE
     lcDyelotSt= 'DISABLE'
  ENDIF
  *--call function to show lcwinch2 window
  =lfShwStScr()
ENDIF
SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvStylRef
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : valid the refrence field
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvStylRef()
*!*************************************************************
FUNCTION lfvStylRef
PRIVATE lnAlias
lnAlias = SELECT(0)
IF (lcxRefer = lcOldValue) 
  RETURN
ENDIF  
SELECT (lcTktSheet)
lnRecNo=RECNO()
lcKey = Style+Dyelot+PADR(cWareCode,6)
SEEK '1'+lcKey
*--replace all lines the refrence (budjet-receive-damage) 
REPLACE REST Reference WITH lcxRefer ;
       FOR Style+Dyelot+PADR(cWareCode,6) = lcKey
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SHOW WINDOW (lcBrowTitl) REFRESH
SELECT (lnAlias)
RETURN
*!*************************************************************
*! Name      : lfvStyDylt
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : valid the dyelot field
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvStyDylt()
*!*************************************************************
FUNCTION lfvStyDylt
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTktSheet)
lnRecNo=RECNO()
lcKey = Style+Dyelot+PADR(cWareCode,6)
SEEK '1'+lcKey
REPLACE REST Dyelot WITH lcxDyelot ;
       FOR Style+Dyelot+PADR(cWareCode,6) = lcKey
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
lcDyelotSt= 'DISABLE'
SHOW GET lcxDyelot &lcDyelotSt
SHOW WINDOW (lcBrowTitl) REFRESH
SELECT (lnAlias)
RETURN
*!*************************************************************
*! Name      : lfvStyDylt
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : valid the dyelot field
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvStyDylt()
*!*************************************************************
FUNCTION lfvBystRem
*-Are you sure you want to delete this line?
IF gfModalGen('QRM34036B42002','DIALOG') = 1
  lnAlias = SELECT(0)
  SELECT (lcTktSheet)
  lnRecNo=RECNO() - 1
  lcKey = Style+Dyelot+PADR(cWareCode,6)
  SEEK '1'+lcKey
  SCAN REST FOR Style+Dyelot+PADR(cWareCode,6)=lcKey
    DELETE
  ENDSCAN                  
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  =lfReadSTLn(EOF())
  SHOW WINDOW (lcBrowTitl) REFRESH
ENDIF
RETURN
*!*************************************************************
*! Name      : lfUnConvrt
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : Call Function to convert the recieving style qty to 
*!             cuttkt line 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfvStyDylt()
*!*************************************************************
FUNCTION lfUnConvrt
*--open the cuttkt line in onther eria 
lcCutlnTmp= gfTempName()
=gfOpenFile(gcDataDir+"CUTTKTL", "Cutlin", "SH",@lcCutlnTmp,.T.)
*--check first if there is cuttkt already exist in the (lctempfile)
*--get the style accumulated record by record and locate in the cuttktl
*--for style with available qty and the status $ 'AO'
SELECT CUTTKTL
*--get the current order for the cuttkl
*--and set relation with cuttkth
lcCuttOrd = ORDER('CUTTKTL') 
SET ORDER TO Cuttktls 
SET RELATION TO Cuttktl.cuttkt INTO Cuttkth ADDITIVE
IF !EOF(lcTmpLine)
   *--check first if there is cuttkt already exist in the (lctempfile)
  =lfOldCutkt()
ENDIF
SELECT (lcTktSheet)
GO TOP
SCAN FOR TRANCD <> '1'
  SELECT CUTTKTL
  IF SEEK(&lcTktSheet..Style)  
    *--scan in the cuttkl and get the style until the qty (recived-cancel-damage)
    *--in the receiving by style file is > zero
    SCAN REST WHILE STYLE = &lcTktSheet..Style  FOR CUTTKTH.STATUS $ 'AO' AND ;
         &lcTktSheet..QTY1+&lcTktSheet..QTY2+&lcTktSheet..QTY3+;
         &lcTktSheet..QTY4+&lcTktSheet..QTY5+&lcTktSheet..QTY6+;
         &lcTktSheet..QTY7+&lcTktSheet..QTY8 > 0  AND TRANCD = '1'
      *--labudjet array hold the original qty ( available qty )
      DECLARE laBudjet[9]         
      *-- get the original qty for this cutkt line (open-recive-damage-cancel)
      IF lfFoundQty() = 0
        loop
      ENDIF
      SCATTER TO laCutTktl MEMO
      SELECT (lcTmpLine) 
      *--if the budjet line is not exist add line with trancd = '1'
      IF !SEEK('1'+SPACE(3)+CUTTKTL.CutTkt+CUTTKTL.STYLE)
        APPEND BLANK
        GATHER FROM laCutTktl MEMO
        *-- gather the original qty (available qty )
        GATHER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty FROM laBudjet        
        IF !EMPTY(&lcTktSheet..Reference)
          REPLACE Reference WITH &lcTktSheet..Reference      
        ENDIF
        REPLACE cWareCode  WITH &lcTktSheet..cWareCode
      ENDIF
      lnTotal = 0
      *--calculate the qty can deleted from (by style) qty 
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        IF laBudjet[lnCount] > &lcTktSheet..Qty&lcCount
          lnTotal = lnTotal + &lcTktSheet..Qty&lcCount
        ELSE
          lnTotal = lnTotal + laBudjet[lnCount]
        ENDIF
      ENDFOR
      lcTranCD = &lcTktSheet..TranCD
      IF lcTranCd $'4' AND lnTotal > (TOTQTY -(TOTSTK+TOTDAM+TOTCAN))
        LOOP
      ENDIF
      *--update field ( totstk or totdam or totcan ) depend on trancd
      *--trancd = '2' ---> update field totstk
      *--trancd = '3' ---> update field totDAM
      *--trancd = '4' ---> update field totcan
      lcField = IIF(lcTranCD='2','TOTSTK',IIF(lcTranCd='3','TOTDAM','TOTCAN'))
      REPLACE &lcField  WITH &lcField + lnTotal
      REPLACE TOTBAL WITH TotQty -(TOTSTK+TOTDAM+TOTCAN)
      *--check if the receive line or the damajed line or the canceld line
      *-- is exist
      *-- if the line is not exist add line and replace qty? with qty from
      *-- (by style) file ( only qty canot more the availble qty in the cuttkt line
      *-- if the line is exist add the qty to the exist qty
      DECLARE laDelqty[8]
      laDelqty = 0
      llFound = .T.
      IF !SEEK(lcTranCD+SPACE(3)+CUTTKTL.CutTkt+CUTTKTL.STYLE) OR ;
         (SEEK(lcTranCD+SPACE(3)+CUTTKTL.CutTkt+CUTTKTL.STYLE) AND ;
          lcTranCD = '3')
        llFound = .F.
        IF lcTranCD = '3' 
          LOCATE REST FOR TranCD = '3' and cStyGrade = &lcTktSheet..cStyGrade
          IF FOUND()
            llFound = .T.
          ENDIF
        ENDIF
        IF !llFound
          APPEND BLANK
          GATHER FROM laCutTktl MEMO
          REPLACE TranCd WITH lcTranCD
          REPLACE TOTQTY WITH 0
        ENDIF
      ENDIF
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        IF laBudjet[lnCount] > &lcTktSheet..Qty&lcCount
          REPLACE QTY&lcCount WITH IIF(llFound,QTY&lcCount,0) + &lcTktSheet..Qty&lcCount
          laDelqty[lnCount] = &lcTktSheet..Qty&lcCount
        ELSE
          REPLACE QTY&lcCount WITH IIF(llFound,QTY&lcCount,0) + laBudjet[lnCount]
          laDelqty[lnCount] = laBudjet[lnCount]
        ENDIF
        REPLACE TOTQTY WITH TOTQTY + QTY&lcCount
      ENDFOR
      IF !EMPTY(&lcTktSheet..cRetSty)
        REPLACE cRetSty   WITH &lcTktSheet..cRetSty    ,;
                cStyGrade WITH &lcTktSheet..cStyGrade
      ENDIF
      IF !EMPTY(&lcTktSheet..Reference)
        REPLACE Reference WITH &lcTktSheet..Reference      
      ENDIF
      REPLACE cWareCode  WITH &lcTktSheet..cWareCode
      SELECT (lcTktSheet)
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        REPLACE QTY&lcCount WITH MAX(QTY&lcCount - laDelqty[lnCount],0)
      ENDFOR
    ENDSCAN
    *--if there is no more cuttkt open for this style over recieve the rest 
    *--to the last one
    =lfOvrRecv()
  ENDIF
ENDSCAN
SELECT (lcTmpLine) 
GO TOP 
*--calculate the total stock and the total damage and the total canceld
STORE 0 TO lnTotStk,lnTotDam,lnTotCan
SCAN FOR TranCD = '1'
  lnTotStk=lnTotStk + TOTSTK
  lnTotDam=lnTotDam + TotDam
  lnTotCan=lnTotCan + TotCan
ENDSCAN
*--return the old order to the cuttktl file
SELECT CUTTKTL
SET ORDER TO TAG &lcCuttOrd
SET RELATION TO
SELECT (lcTktSheet)
RETURN
*!*************************************************************
*! Name      : lfOvrRecv
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : over recieve qty
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfOvrRecv()
*!*************************************************************
FUNCTION lfOvrRecv
SELECT (lcTmpLine) 
IF &lcTktSheet..QTY1+&lcTktSheet..QTY2+&lcTktSheet..QTY3+;
   &lcTktSheet..QTY4+&lcTktSheet..QTY5+&lcTktSheet..QTY6+;
   &lcTktSheet..QTY7+&lcTktSheet..QTY8 > 0  AND &lcTktSheet..TranCD <> '4' 
  IF !SEEK(&lcTktSheet..TranCD+SPACE(3)) 
     =lfAddline()
  ELSE
    lnlastrec = 0
    SCAN REST WHILE TranCD = &lcTktSheet..TranCD For Style = &lcTktSheet..STYLE ;
                    AND (cStyGrade = &lcTktSheet..cStyGrade OR &lcTktSheet..Trancd = '2')
      lnLastRec = RecNo()
    ENDSCAN
    IF lnLastRec = 0
      =lfAddLine()
    ELSE
      IF BETWEEN(lnLastRec,1,RECCOUNT())
        GOTO lnLastRec
      ENDIF     
    ENDIF
  ENDIF  
  IF !EMPTY(&lcTktSheet..cRetSty)
    REPLACE cRetSty   WITH &lcTktSheet..cRetSty    ,;
            cStyGrade WITH &lcTktSheet..cStyGrade
  ENDIF
  lnTotal = 0
  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    lnTotal = lnTotal + QTY&lcCount
    REPLACE QTY&lcCount WITH QTY&lcCount + &lcTktSheet..Qty&lcCount
  ENDFOR
  REPLACE TOTQTY    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 
  IF !EMPTY(&lcTktSheet..cRetSty)
    REPLACE cRetSty   WITH &lcTktSheet..cRetSty    ,;
            cStyGrade WITH &lcTktSheet..cStyGrade
  ENDIF
  lnTotQty = TotQty 
  lcCutTkt = CutTkt
  lcTranCd = TranCD
  lcxStyle = Style
  IF SEEK('1'+SPACE(3)+lcCutTkt+lcxStyle)
    lcField = IIF(lcTranCD='2','TOTSTK',IIF(lcTranCd='3','TOTDAM','TOTCAN'))
    REPLACE &lcField  WITH &lcField + lnTotQty  - lnTotal
    REPLACE TOTBAL    WITH TotQty -(TOTSTK+TOTDAM+TOTCAN)
    IF !EMPTY(&lcTktSheet..Reference)
      REPLACE Reference WITH &lcTktSheet..Reference      
    ENDIF
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfAddLine
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : add line to tempfile
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfAddLine()
*!*************************************************************
FUNCTION lfAddLine
=SEEK('1'+SPACE(3)) 
lnlastrec = 0
SCAN REST WHILE TranCD = '1' For Style = &lcTktSheet..STYLE    
   lnLastRec = RecNo()
ENDSCAN
IF BETWEEN(lnLastRec,1,RECCOUNT())
   GOTO lnLastRec
ENDIF     
SCATTER TO laCutTktl MEMO
APPEND BLANK
GATHER FROM laCutTktl MEMO
REPLACE TranCd WITH &lcTktSheet..TranCD , ;
        TOTQTY WITH 0 ,Qty1 WITH 0 ,Qty2 WITH 0 ,Qty3 WITH 0 ,;
        Qty4 WITH 0 ,Qty5 WITH 0 ,Qty6 WITH 0 ,Qty7 With 0 ,;
        Qty8 With 0 ,;
        cWareCode  WITH &lcTktSheet..cWareCode
IF !EMPTY(&lcTktSheet..Reference)
  REPLACE Reference WITH &lcTktSheet..Reference      
ENDIF

*!*************************************************************
*! Name      : lfFoundQty
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/05/1999
*! Purpose   : get the original qty for this cutkt line 
*!             (open - (recive+damage+cancel) )
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Example     :=lfFoundQty()
*!*************************************************************
*! RETURN      : the availble qty from the cutkt line
*!*************************************************************
FUNCTION lfFoundQty
PRIVATE lnCrrAlias
lnCrrAlias = ALIAS()
*--get the original qty ( available qty ) in other way get the qty of the 
*--cuttkt line that we could recieved or canceld or damged from it 
*-- the avaiable qty = budjet -(received +damaged+canceled)
SELECT (lcTmpLine)
lnRecNo = RECNO()
SELECT (lcCutlnTmp)
DECLARE laRecived[9],laDameged[9],laSecond[9],laCanceled[9],laOldRcvd[9],;
        laOldSecnd[9],laOldDmged[9],laOldCncld[9]
STORE 0 TO laBudjet,laRecived,laDameged,laCanceled,laSecond,laOldRcvd,;
        laOldSecnd,laOldDmged,laOldCncld
IF SEEK(CutTktl.CutTkt+CutTktl.Style+Str(CutTktl.LineNo,6)+'1')
  SCAN REST WHILE CutTkt+Style+Str(LineNo,6)= ;
                  CutTktl.CutTkt+CutTktl.Style+Str(CutTktl.LineNo,6)
    DO CASE
      CASE TranCd = '1'
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laBudjet
      CASE TranCd = '2'
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laRecived
      CASE TranCd = '3' AND cStyGrade = '2'
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,ToTQty TO laSecond
      CASE TranCd = '3' AND cStyGrade = '3'        
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,ToTQty TO laDameged
      CASE TranCd = '4'
        SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laCanceled
    ENDCASE
  ENDSCAN
  *--get qty already select from lctempline
  SELECT (lcTmpLine)
  IF SEEK('1'+SPACE(3)+CutTktl.CutTkt+CutTktl.Style)
    SCAN REST FOR CutTkt+Style+Str(LineNo,6)= ;
                  CutTktl.CutTkt+CutTktl.Style+Str(CutTktl.LineNo,6)
      DO CASE
        CASE TranCd = '1'
          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOldBdjet
        CASE TranCd = '2'
          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOldRcvd
        CASE TranCd = '3' AND cStyGrade = '2'
          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,ToTQty TO laOldSecnd
        CASE TranCd = '3' AND cStyGrade = '3'        
          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,ToTQty TO laOldDmged
        CASE TranCd = '4'
          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOldCncld
      ENDCASE
    ENDSCAN
  ENDIF
ENDIF
*--calculate the total avialble qty can received or cancele or damaged
*-- avl qty = budjet qty - [all recived qty + all damaged qty + all canceld qty 
*--(from cuttkt lines ) + all recived qty + all damaged qty + all canceld qty 
*-- that already selected and not yet saved from temp. file)
laBudjet[9]= 0
FOR lnCount = 1 TO 8
  laBudjet[lnCount]=MAX(laBudjet[lnCount]-(laRecived[lnCount]+laSecond[lnCount]+;
                    laDameged[lnCount]+laCanceled[lnCount]+laOldRcvd[lnCount]+;
                    laOldSecnd[lnCount]+laOldDmged[lnCount]+laOldCncld[lnCount]),0)
  laBudjet[9] = laBudjet[9] + laBudjet[lnCount]
ENDFOR
SELECT (lcTmpLine)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF     
SELECT (lnCrrAlias)
RETURN(laBudjet[9])
*!*************************************************************
*! Name      : lfOldCutkt
*! Developer : WAB - WALID A. WAHAB
*! Date      : 12/02/1999
*! Purpose   : delete the exist qty from the total style qty
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Example     : =lfOldCutkt()
*!*************************************************************
FUNCTION lfOldCutkt
SELECT (lcTmpLine)
GO TOP
*--delete from the (by style) file the exist qty in the (lctmpline)
*--and delete from the (lcTmpline) any more cuttkt line havve qty
*--more than the qty in the (by style) file
SCAN 
  IF SEEK(TranCd+Style+Dyelot+cWareCode,lcTktSheet) AND TranCD <> '1'
    *--if the line from templine is exist in (lctktsheet) by style file
    IF &lctktsheet..TRANCD = '3' AND &lctktsheet..cStyGrade <> cStyGrade
      SKIP IN (lcTktSheet)
*      IF &lcTktSheet..TranCd+&lcTktSheet..Style+&lcTktSheet..Dyelot+&lcTktSheet..cWareCode <> ;
        '3'+Style+Dyelot+cWareCode AND &lcTktSheet..cStyGrade <> cStyGrade
      IF !(Style+Dyelot+cWareCode = &lcTktSheet..Style+&lcTktSheet..Dyelot+&lcTktSheet..cWareCode;
           AND &lctktsheet..TRANCD = '3' AND &lcTktSheet..cStyGrade = cStyGrade)
         DELETE           
         LOOP
      ENDIF
    ENDIF    
    lnTotal = 0
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      DO CASE 
        CASE &lcTktSheet..QTY&lcCount = 0
          REPLACE QTY&lcCount WITH 0
        CASE &lcTktSheet..QTY&lcCount > Qty&lcCount 
          SELECT (lcTktSheet)
          REPLACE QTY&lcCount WITH MAX(QTY&lcCount - &lcTmpLine..Qty&lcCount,0)
        CASE &lcTktSheet..QTY&lcCount <= Qty&lcCount 
          REPLACE QTY&lcCount WITH &lcTktSheet..QTY&lcCount
          SELECT (lcTktSheet)
          REPLACE QTY&lcCount WITH 0
      ENDCASE
      SELECT (lcTmpLine)
      lnTotal = lnTotal + QTY&lcCount
    ENDFOR
    lnRecNo    = RECNO()
    lcCutTktNo = CutTkt
    lcStyle    = Style
    lcDyelot   = Dyelot
    lcWareCode = cWareCode
    lcField = IIF(TranCD='2','TOTSTK',IIF(TranCd='3','TOTDAM','TOTCAN'))
    IF SEEK('1'+SPACE(3)+lcCutTktNo+lcStyle+lcDyelot+lcWareCode)
      REPLACE &lcField  WITH &lcField + lnTotal
      REPLACE TOTBAL WITH TotQty -(TOTSTK+TOTDAM+TOTCAN)
    ENDIF
    IF !EMPTY(&lcTktSheet..Reference)
      REPLACE Reference WITH &lcTktSheet..Reference      
    ENDIF
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
      IF !EMPTY(&lcTktSheet..cRetSty)
         REPLACE cRetSty   WITH &lcTktSheet..cRetSty    ,;
                cStyGrade WITH &lcTktSheet..cStyGrade
      ENDIF
      IF !EMPTY(&lcTktSheet..Reference)
        REPLACE Reference WITH &lcTktSheet..Reference      
      ENDIF
    ENDIF     
    *-- delete the line if the qty is 0
    IF lnTotal = 0 
      DELETE
    ENDIF
  ELSE
    *--if the line is not exist in the (lctktsheet) delete the line
    IF TRANCD <> '1'
      lnRecNo    = RECNO() 
      lcCutTktNo = CutTkt
      lcStyle    = Style
      lcDyelot   = Dyelot
      lcWareCode = cWareCode
      lnTotal = 0
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        lnTotal = lnTotal + QTY&lcCount
      ENDFOR
      lcField = IIF(TranCD='2','TOTSTK',IIF(TranCd='3','TOTDAM','TOTCAN'))
      IF SEEK('1'+SPACE(3)+lcCutTktNo+lcStyle+lcDyelot+lcWareCode)
        REPLACE &lcField  WITH MAX(&lcField - lnTotal,0)
      ENDIF
      IF BETWEEN(lnRecNo,1,RECCOUNT())
        GOTO lnRecNo
        DELETE 
      ENDIF     
    ELSE
      REPLACE TOTSTK WITH 0 ,TOtDam WITH 0 ,;
              TOTCAN WITH 0 ,TOTBAL WITH 0 
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcTmpLine)
GO TOP
*--delete budget line in case of there is no stk qty and no canceld qty and no damage qty
SCAN FOR TranCd = '1'
  IF TOTSTK = 0 AND TOTDAM = 0 AND TOTCAN = 0
    DELETE
  ENDIF
ENDSCAN
