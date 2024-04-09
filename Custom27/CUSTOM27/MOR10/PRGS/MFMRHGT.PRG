*!*************************************************************
*! Name      : MFMRHGT.PRG          
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 B:130635
*! Purpose   : Print UPC price tickets  ( Customr Morelle )
*!*************************************************************
*! Calls     : lpSelTkt
*!             lpSelpo  
*!             lpSelOrder 
*!             lpPriceTkt  
*!             lfDispStat
*!             lfVSelect   
*!             lfModDetails
*!             lfvVersion
*!             lfvOutDir
*!             lfvFileName
*!*************************************************************

*-- Saving the system color
lcOldClr = SET('COLOR')
*-- To indicate whether U R using the program from manufacturing or PO  module.
=gfOpnFiles()

qClrNrm   = "GR+/B,GR+/N,N"
qClrSay   = "N/BG,GR+/N"
qClrMag   = "BG+/RB,B/W"
QCLRBLK   = "N/W,GR+/N,N"
QCLRREV   = "W+/R,GR+/N"


*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))

*-- To get the style header
lcStyHdr = gfItemMask("HM")
STORE 0 TO lnClrPo,lnFreeLen , lnColorLen, lnTopCol

=lfGtNonMaj()

lcCursName = gfTempName()
SELECT cVer FROM PaxarLbl DIST ORDER BY cVer INTO CURSOR (lcCursName)

IF _WINDOWS
  DEFINE WINDOW MainWin FROM 0,0 SIZE 25,80 ;
         FLOAT NONE NOCLOSE FONT "FoxFont", 10 STYLE "R";
         IN WINDOW FNDATION
  MOVE WINDOW MainWin CENTER
  ACTIVATE WINDOW MainWin
ENDIF
SET COLOR TO &qClrNrm
CLEA

DO lpPrnHdr WITH 'MOR300.',' PRINT UPC PRICE TICKETS '
SET MESSAGE TO 24
SET COLOR TO &QCLRNRM
@ 23,00 TO 23,79
SET COLOR TO &QCLRMAG
@ 07,23 SAY '< Esc to exit >'

ON KEY LABEL ESC

DO GRID WITH 9,'N'
lcSelBy  = 'T'
lcStatus = 'A'
STORE SPACE(5) TO xAccount
STORE SPACE(8) TO lcVendor
STORE {} TO ldLowDate,ldHighDate
STORE SPACE(0) TO lcCtFilter, lcPoFilter, lcOrFilter
STORE SYS(5)+SYS(2003) TO lcOutDir
STORE SPACE(8) TO lcFile

lcSelSize = 'A'
lnPercent = 100
lcStyPoCt = 'A'
lcVer     = SPACE(3)
lnOutDir  = 1
@ 03,05 SAY 'UPC tickets for (S)tyle POs (C)ut Tickets Customer (O)rders (A)ll : '
@ 05,05 SAY 'Select : (E)ntered (C)omplete (T)icket#           ................: '
@ 07,05 SAY 'Status : (O)pen    (H)old               (A)ll.....................: '
@ 11,05 SAY 'Percentage to process.........................................:    %'
@ 13,05 SAY 'Select Size:  (A)LL /1/2/3/4/5/6/7/8 .............................: '
@ 15,05 SAY 'U.P.C. price tickets version .....................................: '
@ 17,05 SAY 'Output file directory :'
@ 19,05 SAY 'Output file name (No extension) :'
SAVE SCREEN TO lsMastGrid
DO WHILE .T.
  RESTORE SCREEN FROM lsMastGrid
  @ 03,74 GET lcStyPoCt PICTURE '!'  ;
          VALID lcStyPoCt $ 'SCOA' .AND. lfDispStat(lcStyPoCt)
  @ 05,74 GET lcSelBy   PICTURE '!'  VALID lfvSelect()
  @ 07,74 GET lcStatus  PICTURE '!'  ;
          VALID IIF(lcStyPoCt $ 'CA',lcStatus $ 'OHAC',lcStatus $ 'OHA')
  @ 11,74 GET lnPercent PICTURE '999' VALID lnPercent > 0 ;
          ERROR "Percentage to process should be greate than zero"
  @ 13,74 GET lcSelSize PICTURE 'X'  VALID lcSelSize $ 'A12345678'
  @ 15,74 GET lcVer     PICTURE '@!' VALID lfvVersion()
  @ 17,29 SAY PADR(lcOutDir,30,' ') COLOR (qClrSay)
  @ 17,62 GET lnOutDir PICTURE '@*N Get Directory' SIZE 1,15 VALID lfvOutDir()
  @ 19,40 GET lcFile PICTURE '@!' VALID lfvFileName()
  READ
  IF !lfvVersion() .OR. !lfvFileName()
    LOOP
  ENDIF  
  IF READKEY()=12
    *CLOSE DATA
    RETURN
  ENDIF

  IF lcSelBy ='V'
    @ 09,05 SAY 'Vendor...........................:'
    @ 09,40 GET lcVendor PICTURE '!!!!!'
    READ
    SELECT ApVendor
    IF !SEEK(lcVendor)
      DO gfApVnBrow WITH lcVendor
      SET COLOR TO &QCLRSAY
      @ 09,40 SAY lcVendor
    ENDIF
    IF EMPTY(lcVendor)
      LOOP
    ENDIF
    @ 09,49 SAY ApVendor.cvencomp
  ENDIF
  IF lcSelBy ='S'
    @ 09,05 SAY 'Customer...........................:'
    @ 09,42 GET xAccount PICTURE '!!!!!'
    READ
    SELECT Customer
    IF !SEEK('M'+xAccount)
      SAVE SCREEN TO lsCusScr
      DO CUSBROWM WITH xAccount
      RESTORE SCREEN FROM lsCusScr
      SET COLOR TO &qClrSay
      @ 09,42 SAY xAccount
    ENDIF
    IF EMPTY(xAccount)
      LOOP
    ENDIF
    @ 09,48 SAY Customer.BTName
  ENDIF

  IF lcSelBy $ 'EC'   
    IF lcSelBy='E'
      @ 09,05 SAY 'Date Entered range................................:'
    ELSE
      @ 09,05 SAY 'Date Completed range..............................:'
    ENDIF
    @ 09,57 GET ldLowDate
    @ 09,68 GET ldHighDate
    READ
    IF EMPTY(ldHighDate)
      ldHighDate=ldLowDate
    ENDIF
    IF ! EMPTY(ldLowDate) .AND. (ldHighDate<ldLowDate)
      DO MSG22 WITH 'Date range error !!!','$'
      LOOP
    ENDIF
  ENDIF
  lcOutFile = lcOutDIr+ALLTRIM(lcFile)+'.'+lcVer
  IF FILE(lcOutFile) .AND. ;
    gfDialog('?','Output file '+lcOutFile+' allready exists.','\<Overwrite;\!\<Reenter' ) = 2
    LOOP
  ENDIF    
  SET COLOR TO (qClrSay)
  DO MSG22 WITH '<P>roceed <M>odify <C>ancel ? ','PMC'
  DO CASE
    CASE CHOICE = 'P'
      EXIT
    CASE CHOICE = 'C'
      RETURN
  ENDCASE
ENDDO
lnFilePtr = FCREATE("&lcOutFile",0)
IF lnFilePtr < 0 
  DO Msg22 WITH  'CANNOT CREATE THE OUTPUT FILE ','$'
  RETURN
ENDIF

DO MSG22 WITH 'Selecting the records ....','@'
IF lcSelBy ='V'
  *lcCtFilter='(lcVendor=Contr1 .OR. lcVendor=Contr2 .OR. lcVendor=Contr3)'
  lcPoFilter='(lcVendor=Vendor)'    
ENDIF
IF lcSelBy ='S'
  lcOrFilter='(xAccount=Account)'
ENDIF
IF lcSelBy $ 'EC'   
  IF ! EMPTY(ldHighDate)
    IF lcSelBy='E'
      lcCtFilter='BETWEEN(Entered, ldLowDate,ldHighDate)'
    ELSE
      lcCtFilter='BETWEEN(Complete, ldLowDate, ldHighDate)'
    ENDIF
    STORE lcCtFilter TO lcPoFilter,lcOrFilter 
  ENDIF
ENDIF
lcStatus = IIF(lcStatus='A','OHAC',IIF(lcStatus='C','A',lcStatus))
lcCtFilter= lcCtFilter + IIF(EMPTY(lcCtFilter),'',' .AND. ') + 'STATUS $ lcStatus '
lcPoFilter= lcPoFilter + IIF(EMPTY(lcPoFilter),'',' .AND. ') + 'STATUS $ lcStatus '
lcOrFilter= lcOrFilter + IIF(EMPTY(lcOrFilter),'',' .AND. ') + 'STATUS $ lcStatus '

tmpCuttkt = gfTempName()
CREATE DBF (gcWorkDir+tmpCuttkt) ;
            (Ticket C(6), RecFlag C(1), Type C(1))
INDEX ON Type+Ticket TAG (tmpCuttkt)

IF lcStyPoCt $ 'CA'
  SELECT CUTTKTH
  SCAN FOR &lcCtFilter
    INSERT INTO (tmpCuttkt) (TYPE, TICKET, RECFLAG) ;
    VALUES ('C',CutTktH.CutTkt, 'N')      
  ENDSCAN
ENDIF
IF lcStyPoCt $ 'SA'
  SELECT POSHDR
  SCAN FOR &lcPoFilter
    INSERT INTO (tmpCuttkt) (TYPE, TICKET, RECFLAG) ;
    VALUES ('S',PosHdr.Po, 'N')      
  ENDSCAN
ENDIF

IF lcStyPoCt $ 'OA'
  SELECT ORDHDR
  SCAN FOR &lcOrFilter
    INSERT INTO (tmpCuttkt) (TYPE, TICKET, RECFLAG) ;
    VALUES ('O',OrdHdr.Order, 'N')      
  ENDSCAN
ENDIF

SELECT (tmpCuttkt)
SET RELATION TO Ticket INTO CUTTKTH
SET RELATION TO 'P'+Ticket INTO POSHDR ADDITIVE
SET RELATION TO 'O'+Ticket INTO ORDHDR ADDITIVE

GOTO TOP
IF EOF()
  DO MSG22 WITH 'No records selected !!!','$'
  =FCLOSE(lnFilePtr)  
  RETURN
ENDIF

IF lcSelBy='T'
  SELECT (tmpCuttkt)
  IF lcStyPoCt $ 'CA'
    DO lpSelTkt
  ENDIF  
  IF lcStyPoCt $ 'SA'
    DO lpSelPO
  ENDIF  
  IF lcStyPoCt $ 'OA'
    DO lpSelOrder
  ENDIF  
  
  SELECT (tmpCuttkt)
  SET FILTER TO RecFlag='Y'
  GOTO TOP
  IF EOF()
    DO MSG22 WITH 'No records selected !!!','$'
    =FCLOSE(lnFilePtr)    
    RETURN
  ENDIF
ELSE
  REPLACE ALL RecFlag WITH 'Y' 
ENDIF

tmpDetails = gfTempName()
CREATE DBF (gcWorkDir+tmpDetails) ;
           (Type C(1), Ticket C(6), Style C(19), Size C(5), SizeNo C(1), ;
             Quantity N(6), Dyelot C(10))
INDEX ON Size+Type+Ticket+Style TAG (tmpDetails)

IF lcSelSize = 'A'
  lnFrstScl = 1
  lnLastScl = 8
ELSE
  STORE VAL(lcSelSize) TO lnFrstScl, lnLastScl
ENDIF

SELECT (tmpCuttkt)
SET RELATION TO
SCAN FOR RecFlag = 'Y'
  DO CASE
    CASE Type = 'C'
      SELECT CutTktL
      SEEK &tmpCutTkt..Ticket
      SCAN WHILE CutTkt = &tmpCutTkt..Ticket FOR TranCd = '1'
        FOR lnCount = lnFrstScl TO lnLastScl
          lcCount = STR(lnCount,1)
          IF Qty&lcCount <> 0 .AND. SEEK(Style,'Style')
            INSERT INTO (tmpDetails) ;
                   (Type, Ticket,Style,Size,SizeNo,Quantity,Dyelot);
            VALUES ('C',CutTktL.CutTkt,CutTktL.Style,Scale.Sz&lcCount,;
                   lcCount,CutTktL.Qty&lcCount,CutTktL.Dyelot)
          ENDIF
        ENDFOR            
      ENDSCAN
    CASE Type = 'S'
      SELECT PosLn    
      SEEK 'P'+&tmpCutTkt..Ticket
      SCAN WHILE Po = &tmpCutTkt..Ticket FOR TranCd = '1'
        FOR lnCount = lnFrstScl TO lnLastScl
          lcCount = STR(lnCount,1)
          IF Qty&lcCount <> 0 .AND. SEEK(Style,'Style')
            INSERT INTO (tmpDetails) ;
                        (Type, Ticket, Style, Size, SizeNo, Quantity);
                 VALUES ('S',PosLn.Po,PosLn.Style,Scale.Sz&lcCount ,;
                         lcCount,PosLn.Qty&lcCount)
          ENDIF
        ENDFOR
      ENDSCAN
    CASE Type = 'O'
      SELECT OrdLine
      SEEK 'O' + &tmpCutTkt..Ticket
      SCAN WHILE cordtype+order+STR(lineno,6) = 'O'+ &tmpCutTkt..Ticket
        FOR lnCount = lnFrstScl TO lnLastScl
          lcCount = STR(lnCount,1)
          IF Qty&lcCount <> 0 .AND. SEEK(Style,'Style')
            INSERT INTO (tmpDetails) ;
                        (Type, Ticket, Style, Size, SizeNo, Quantity);
                 VALUES ('O',OrdLine.Order,OrdLine.Style,Scale.Sz&lcCount,;
                         lcCount,OrdLine.Qty&lcCount)
          ENDIF
        ENDFOR
      ENDSCAN
  ENDCASE      
ENDSCAN
SELECT (tmpDetails)
DO MSG22 WITH 'Do you want to modify selected styles? [Y/N] ','YN'
IF CHOICE = 'Y'
  =lfModDetails()
ENDIF  
GO TOP
IF EOF()
  DO MSG22 WITH 'No records selected !!!','$'
  =FCLOSE(lnFilePtr)    
  RETURN
ENDIF
SET RELATION TO Style INTO STYLE

SET COLOR TO (qClrNrm)
@ 23,00 TO 23,79 CLEAR
SET COLOR TO (qClrSay)
DO lpPriceTkt
IF WEXIST("MainWin")
  RELEASE WINDOW MainWin
ENDIF

ON KEY LABEL ESC KEYBOARD "{ALT+X}"

IF USED('&tmpCuttkt')
  SELECT &tmpCuttkt
  use 
  ERASE(gcWorkDir+tmpCuttkt+'.DBF')
  ERASE(gcWorkDir+tmpCuttkt+'.CDX')
ENDIF

SET COLOR TO &lcOldClr 

*!*************************************************************
*! Name      : lpSelTkt
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Select cutting tickes to print.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpSelTkt()
****************************************************************************
PROCEDURE lpSelTkt
PRIVATE lcLowTkt, lcHighTkt, lcTmpColor, lnRow, lsTktScr, lcAlias
SELECT (tmpCuttkt)   
IF !SEEK('C')
  DO MSG22 WITH 'No cuttkts match selected criteria','$'
  RETURN
ENDIF
lcAlias= SELECT()
SAVE SCREEN TO lsTktScr
CLEAR
SET COLOR TO &QCLRNRM
DO lpPrnHdr WITH 'lpSelTkt','SELECT CUTTING TICKETS TO PRINT '

DO DRAWBOX WITH QCLRMAG,'',1,0,21,79
SET COLOR TO &QCLRMAG
@ 02,01 SAY ' CUTKT  STYLE        S ENTERED  COMPLETE SE DI BUDGET  RECVD   DAMAGED   OPEN'
@ 03,01 TO 03,78

DO WHILE .T.
  SELECT (tmpCuttkt)
  IF EOF() .OR. Type <> 'C'  
    SEEK 'C'    
  ENDIF
  STORE SPACE(6) TO lcLowTkt, lcHighTkt
  lnRow    = 4
  QRECNO = 0
  SAVREC = RECNO()
  SET COLOR TO &QCLRMAG
  @ 04,01 CLEAR TO 20,78
  SET COLOR TO &QCLRSAY
  
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
* CUTKT  STYLE        S ENTERED  COMPLETE SE DI BUDGET  RECVD   DAMAGED  OPEN
* XXXXXX 123456789012 X 99/99/99 99/99/99 XX XX 9999999 9999999 9999999 9999999
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
  SCAN WHILE TYPE = 'C' .AND. lnRow < 21  
    lcTmpColor = IIF(RecFlag='Y', qClrRev, qClrSay)
    @ lnRow,02 SAY CutTktH.CutTkt COLOR &lcTmpColor
    @ lnRow,09 SAY CutTktH.Style
    @ lnRow,22 SAY CutTktH.Status
    @ lnRow,24 SAY CutTktH.Entered
    @ lnRow,33 SAY CutTktH.Complete
    @ lnRow,42 SAY CutTktH.Season
    @ lnRow,45 SAY CutTktH.cDivision  
    @ lnRow,48 SAY CutTktH.Pcs_Bud PICTURE '9999999'
    @ lnRow,56 SAY CutTktH.Pcs_Rec PICTURE '9999999'
    @ lnRow,64 SAY CutTktH.Pcs_Dam PICTURE '9999999'
    @ lnRow,72 SAY CutTktH.Pcs_Opn PICTURE '9999999'
    QRECNO(lnRow) = RECNO()
    lnRow = lnRow + 1
  ENDSCAN
  BP(1) = 4
  BP(2) = 1
  BP(3) = 'C'
  BP(8) = 'Type = "C"'
  SET COLOR TO &QCLRMAG
  THISKEY = BROW()
  IF THISKEY = 27
    EXIT
  ENDIF
  IF THISKEY <> 13
    LOOP
  ENDIF
  SELECT (tmpCuttkt)
  SET COLOR TO &QCLRNRM
  @ 23,00 CLEAR
  STORE Ticket TO lcLowTkt, lcHighTkt
  @ 23,20 SAY 'SELECT CUTTKT# => '
  @ 23,40 GET lcLowTkt  PICTURE '@!'
  @ 23,48 GET lcHighTkt PICTURE '@!'
  READ
  @ 23,00
  IF EMPTY(lcLowTkt)
    GOTO SAVREC
    LOOP
  ENDIF
  IF !SEEK('C'+lcLowTKt)
    ? CHR(7)
    GOTO SAVREC
    LOOP
  ENDIF
  SAVREC = RECNO()
  lcHighTkt = IIF(!EMPTY(lcHighTkt).AND.lcHighTKt>lcLowTKt, lcHighTKt, lcLOwTkt)
  REPLACE REST RecFlag WITH IIF(RecFlag = 'Y', 'N', 'Y') WHILE Type+Ticket <= 'C'+lcHighTKt
  GOTO SAVREC
ENDDO
SELECT (lcAlias)
RESTORE SCREEN FROM lsTktScr
RETURN

*!*************************************************************
*! Name      : lpSelPo
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Select Style POs to print.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpSelPo()
****************************************************************************
PROCEDURE lpSelPo
PRIVATE lcLowTkt, lcHighTkt, lcTmpColor, lnRow, lsTktScr, lcAlias

SELECT (tmpCuttkt)
IF !SEEK('S')
  DO MSG22 WITH 'No style POs match selected criteria','$'
  RETURN
ENDIF
lcAlias= SELECT()
SAVE SCREEN TO lsTktScr
SET COLOR TO &QCLRNRM
CLEAR
DO lpPrnHdr WITH 'lpSelTkt','SELECT STYLE POs TO PRINT '

DO DRAWBOX WITH QCLRMAG,'',1,0,21,79
SET COLOR TO &QCLRMAG
@ 02,01 SAY '   PO #  S VENDOR .....NAME..... COMPLETE TOT.QTY.    AMOUNT   RECVD    OPEN'
@ 03,01 TO 03,78
DO WHILE .T.
  SELECT (tmpCuttkt)
  IF EOF() .OR. Type <> 'S' 
    SEEK 'S'   
  ENDIF
  STORE SPACE(6) TO lcLowTkt, lcHighTkt
  lnRow    = 4
  QRECNO = 0
  SAVREC = RECNO()
  SET COLOR TO &QCLRMAG
  @ 04,01 CLEAR TO 20,78
  SET COLOR TO &QCLRSAY

*....*....1...*....2....*....3....*....4....*....5....*....6....*....7....*...
*
*   PO #  S VENDOR .....NAME..... COMPLETE TOT.QTY.    AMOUNT   RECVD    OPEN
*  XXXXXX X XXXXX XXXXXXXXXXXXXXX XX/XX/XX 9999999 9999999.99 9999999 9999999

   SCAN WHILE TYPE = 'S' .AND. lnRow < 21  
    lcTmpColor = IIF(RecFlag='Y', qClrRev, qClrSay)
    @ lnRow,02 SAY PosHdr.Po COLOR &lcTmpColor
    @ lnRow,10 SAY PosHdr.Status
    @ lnRow,12 SAY PosHdr.Vendor
    =SEEK(PosHdr.Vendor,'ApVendor')
    @ lnRow,19 SAY SUBSTR(ApVendor.cvencomp,1,15)
    @ lnRow,35 SAY PosHdr.Complete

    IF SEEK('2'+PosHdr.Po,'CutPick')
      @ lnRow,44 SAY CutPick.Order PICTURE '9999999'
    ENDIF  

    @ lnRow,52 SAY PosHdr.PoTotal PICTURE '9999999.99'
    @ lnRow,63 SAY PosHdr.Receive PICTURE '9999999'
    @ lnRow,71 SAY PosHdr.Open PICTURE '9999999'
    QRECNO(lnRow) = RECNO()
    lnRow = lnRow + 1
  ENDSCAN
  BP(1) = 4
  BP(2) = 1
  BP(3) = 'S'
  BP(8) = 'Type = "S"'
  SET COLOR TO &qClrMag
  THISKEY = BROW()
  IF THISKEY = 27
    EXIT
  ENDIF
  IF THISKEY <> 13
    LOOP
  ENDIF
  SELECT (tmpCuttkt)
  SET COLOR TO &qClrNrm
  @ 23,00 CLEAR
  STORE Ticket TO lcLowTkt, lcHighTkt
  @ 23,20 SAY 'SELECT PO# => '
  @ 23,40 GET lcLowTkt  PICTURE '@!'
  @ 23,48 GET lcHighTkt PICTURE '@!'
  READ
  @ 23,00
  IF EMPTY(lcLowTkt)
    GOTO SAVREC
    LOOP
  ENDIF
  IF !SEEK('S'+lcLowTKt)
    ? CHR(7)
    GOTO SAVREC
    LOOP
  ENDIF
  SAVREC = RECNO()
  lcHighTkt = IIF(!EMPTY(lcHighTkt).AND.lcHighTKt>lcLowTKt, lcHighTKt, lcLowTkt)
  REPLACE REST RecFlag WITH IIF(RecFlag = 'Y', 'N', 'Y') WHILE TYPE+Ticket <= 'S'+lcHighTKt
  GOTO SAVREC
ENDDO
SELECT (lcAlias)
RESTORE SCREEN FROM lsTktScr
RETURN

*!*************************************************************
*! Name      : lpSelOrder
*! Developer : WAM - Wael Aly Mohamed
*! Date      : 10/18/1995
*! Purpose   : Select Orders to print.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpSelOrder()
****************************************************************************
PROCEDURE lpSelOrder
PRIVATE lcLowTkt, lcHighTkt, lcTmpColor, lnRow, lsTktScr, lcAlias, lnSavRec

SELECT (tmpCuttkt)
IF !SEEK('O')
  DO MSG22 WITH 'No customer orders match selected criteria','$'
  RETURN
ENDIF
lcAlias= SELECT()
SAVE SCREEN TO lsTktScr
SET COLOR TO &qClrNrm
CLEAR
DO lpPrnHdr WITH 'lpSelTkt',"SELECT CUSTOMER ORDERS TO PRINT "

DO DRAWBOX WITH qClrMag,'',2,0,22,79
SET COLOR TO &qClrMag
@ 3,2 SAY 'ORDER# S SE DI ACCT/STR NAME         STRT/CMP P/O#       SHIPPED   OPEN'
@ 4,1 TO 4,78
DO WHILE .T.
  SELECT (tmpCuttkt)
  IF EOF() .OR. Type <> 'O'
    SEEK 'O'   
  ENDIF
  STORE SPACE(6) TO lcLowTkt, lcHighTkt
  lnRow    = 5
  QRECNO = 0
  lnSavRec = RECNO()
  SET COLOR TO &qClrMag
  @ 05,01 CLEAR TO 21,78
  SET COLOR TO &qClrSay
  SCAN WHILE TYPE = 'O' .AND. lnRow < 21  
    lcTmpColor = IIF(RecFlag='Y', qClrRev, qClrSay)
    QRECNO(lnRow) = RECNO()
    @ lnRow,02 SAY OrdHdr.Order COLOR &lcTmpColor
    @ lnRow,09 SAY OrdHdr.Status
    @ lnRow,11 SAY OrdHdr.Season
    @ lnRow,14 SAY OrdHdr.cDivision
    @ lnRow,17 SAY OrdHdr.Account
    = SEEK('M'+OrdHdr.Account,'Customer')
    @ lnRow,26 SAY SUBSTR(Customer.StName,1,11)
    @ lnRow,38 SAY OrdHdr.Start
    @ lnRow,47 SAY IIF(OrdHdr.MultiPo,'*MULTI-PO*',OrdHdr.CustPo)
    @ lnRow,58 SAY OrdHdr.ShipAmt
    @ lnRow,69 SAY OrdHdr.OpenAmt
    lnRow = lnRow + 1
    @ lnRow,17 SAY IIF(OrdHdr.MULTI='Y','*MULTI*',OrdHdr.Store)
    @ lnRow,38 SAY OrdHdr.Complete
    @ lnRow,58 SAY OrdHdr.Ship
    @ lnRow,69 SAY OrdHdr.Open
    lnRow = lnRow + 1
    IF lnRow <21
      SET COLOR TO &qClrMag
      @ lnRow,01 SAY REPLICATE('Ä',78)
      lnRow = lnRow + 1
      SET COLOR TO &qClrSay      
    ENDIF
  ENDSCAN
  BP(1) = 5
  BP(2) = 1
  BP(3) = 'O'
  BP(8) = 'Type = "O"'
  SET COLOR TO &qClrMag
  THISKEY = BROW()
  IF THISKEY = 27
    EXIT
  ENDIF
  IF THISKEY <> 13
    LOOP
  ENDIF
  SELECT (tmpCuttkt)
  SET COLOR TO &qClrNrm
  @ 23,00 CLEAR
  STORE Ticket TO lcLowTkt, lcHighTkt
  @ 23,20 SAY 'SELECT ORDERS => '
  @ 23,40 GET lcLowTkt  PICTURE '@!'
  @ 23,48 GET lcHighTkt PICTURE '@!'
  READ
  @ 23,00
  IF EMPTY(lcLowTkt)
    GOTO lnSavRec
    LOOP
  ENDIF
  IF !SEEK('O'+lcLowTKt)
    ? CHR(7)
    GOTO lnSavRec
    LOOP
  ENDIF
  lnSavRec = RECNO()
  lcHighTkt = IIF(!EMPTY(lcHighTkt).AND.lcHighTKt>lcLowTKt, lcHighTKt, lcLowTkt)
  REPLACE REST RecFlag WITH IIF(RecFlag = 'Y', 'N', 'Y') WHILE TYPE+Ticket <= 'O'+lcHighTKt
  GOTO lnSavRec
ENDDO
SELECT (lcAlias)
RESTORE SCREEN FROM lsTktScr
RETURN

*!*************************************************************
*! Name      : lpPriceTkt
*! Developer : WAM - Wael Aly Mohamed
*! Date      : 10/18/1995
*! Purpose   : Print UPC price ticket
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpPriceTkt()
****************************************************************************
PROCEDURE lpPriceTkt
PRIVATE lnCount, lcCount, lnFrstScl, lnLastScl, lnQuantity,;
        lcStyle, lcClrDesc, lcStyDesc, lcUpcCode, lcStyCon1, lcStyCon2,;
        lcSizeDesc, lcTiket, lcData, lnRow, lcString

DO MSG22 WITH 'Output file '+lcOutFile+' beening created. Please standby....','@'

STORE '' TO lcStyle              &&  Style code
STORE '' TO lcClrDesc            &&  Color description
STORE '' TO lcStyDesc            &&  Style description
STORE '' TO lcUpcCode            &&  U.P.C. code
STORE '' TO lcStyCon1            &&  Style content 1
STORE '' TO lcStyCon2            &&  Style content 2
STORE '' TO lcSizeDesc           &&  Size description
STORE '' TO lcDyelot             &&  Dyelot
STORE '' TO LCRETPRICE
lnRow = 1
lcTicket = SPACE(6)
SELECT (tmpDetails)
GOTO TOP
DO WHILE !EOF()
  lcSizeDesc = Size                                && Size description
  @ 22,1 SAY 'Size :' + lcSizeDesc COLOR (qClrNrm)
  SCAN WHILE Size = lcSizeDesc

    lcTicket   = Ticket                            && Ticket number
    lcStyle    = Style                             && Style code
    lcDyelot   = Dyelot                            && Dyelot
    lcStyCon1  = Style.Content1                    && Style content 1
    lcStyCon2  = Style.Content2                    && Style content 2
    lcStyDesc  = Style.Desc                        && Style discription
    lcClrDesc = gfCodDes(SUBSTR(lcStyle,lnClrPo,lnColorLen),'COLOR',.F.)
    LCRETPRICE = '$ '+ALLTRIM(STR(STYLE.NSUGRETPRI,6,2))
    = SEEK(Style+SizeNo,'StyleUpc')
    lcUpcCode  = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
    lnQuantity = ROUND(Quantity*lnPercent/100,0)
    lnQuantity = IIF(lnQuantity = 0, 1, lnQuantity)   && prints at least 1
    IF !EMPTY(STYLE.FABRIC) AND SEEK(Style.Fabric,'FABRIC')
      lcFabDesc = FABRIC.DESC
    ELSE
      lcFabDesc ='' 
    ENDIF
    SELECT PaxarLbl
    SEEK lcVer
    SCAN WHILE cVer = lcVer
      STORE cData TO lcData
      =FWRITE(lnFilePtr,&lcData)
    ENDSCAN	
    =FPUTS(lnFilePtr,'')
  ENDSCAN
ENDDO
=FCLOSE(lnFilePtr)
RETURN

*!*************************************************************
*! Name      : lfDispStat
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Display availabe Style PO/CT status to be selected
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfDispStat(lcStyPoCt)
****************************************************************************
FUNCTION lfDispStat
PARAMETERS lcStyPoCt

@ 05,45 SAY IIF(lcStyPoCt $ 'O','Cu(s)tomer',IIF(lcStyPoCt $ 'A',SPACE(10),'(V)endor  ')) COLOR &qClrNrm
@ 07,35 SAY IIF(lcStyPoCt $ 'CA','a(C)tual',SPACE(8)) COLOR &qClrNrm
RETURN(.T.)

*!*************************************************************
*! Name      : lfVSelect
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Validate user selection
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfVSelect()
****************************************************************************
FUNCTION lfVSelect
DO CASE
  CASE lcStyPoCt = 'O' .AND. !(lcSelBy $ 'ECTS')
    RETURN(.F.)
  CASE lcStyPoCt = 'A' .AND. !(lcSelBy $ 'ECT')    
    RETURN(.F.)  
  CASE lcStyPoCt $ 'SC' .AND. !(lcSelBy $ 'ECTV')    
    RETURN(.F.)  
ENDCASE
RETURN(.T.)    

*!*************************************************************
*! Name      : lfModDetails
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Modify selected styles
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfModDetails()
****************************************************************************
FUNCTION lfModDetails

SET COLOR TO (qClrNrm)
CLEAR
DO lpPrnHdr WITH 'UPC003',"MODIFY SELECTED STYLES"

DO DRAWBOX WITH qClrMag,'',2,14,22,66
SET COLOR TO (qClrMag)
@ 3,16 SAY 'TYPE   TICKET# STYLE        COLOR  SIZE  QUANTITY'
@ 4,15 TO 4,65
SELECT (tmpDetails)
Arrow = 0
GO TOP
DO WHILE .T.
  lnRow  = 5
  QRECNO = 0
  SET COLOR TO (qClrMag)
  @ 05,15 CLEAR TO 21,65
  SET COLOR TO (qClrSay)
  SCAN WHILE lnRow < 21  
    DO CASE
      CASE Type = 'C'
        lcTypeDesc = 'CUTTKT'
      CASE Type = 'O'
        lcTypeDesc = 'ORDER '
      CASE Type = 'S'
        lcTypeDesc = 'P.O.  '      
    ENDCASE              
    @ lnRow,16 SAY lcTypeDesc
    @ lnRow,23 SAY Ticket
    @ lnRow,31 SAY Style
    @ lnRow,51 SAY Size
    @ lnRow,57 SAY Quantity PICTURE '999999'
    QRECNO(lnRow) = RECNO()    
    lnRow = lnRow + 1
  ENDSCAN
  lnSavRec = qRecNo(5)
  BP(1) = 5
  BP(2) = 15
  BP(4) = 23
  BP(5) = 66
  BP(7) = 'Y'
  SET COLOR TO (qClrMag)
  THISKEY = BROW()
  DO CASE
    CASE THISKEY = QESCKEY
      EXIT
    CASE THISKEY = QRETKEY
      @ Arrow,57 GET Quantity PICTURE '999999' COLOR (qClrSay)
      READ
      BP(10) = Arrow      
      GO lnSavRec
    CASE THISKEY = QDELKEY
      DO MSG22 WITH 'Are you sure you want to delete this Style/Color? [Y/N] ','YN'
      IF CHOICE = 'Y'
        DELETE 
        PACK
      ENDIF
      GO TOP
  ENDCASE    
ENDDO
RETURN

*!*************************************************************
*! Name      : lfvVersion
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Validate UPC version
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : .T. for valid version
*!*************************************************************
*! Example   : =lfvVersion()
*!*************************************************************
FUNCTION lfvVersion

IF !SEEK(lcVer,'PaxarLbl')
  SELECT (lcCursName)
  PUSH KEY
  ON KEY LABEL ENTER RELEASE WINDOW 'Tkt. Versions'
  
  GO TOP
  DEFINE WINDOW lwVersions FROM 14,50 TO 22,66 ;
         SYSTEM NOGROW NOMINIMIZE SHADOW COLOR SCHEME 10
  BROWSE FIELDS cVer :h='Version      ';
                TITLE   'Tkt. Versions';
                NOEDIT NODELETE NOMENU NOAPPEND ;
                WINDOW lwVersions COLOR SCHEME 10
  POP KEY         
  lcVer = IIF(LASTKEY()=27,SPACE(3), cVer)
ENDIF
RETURN !(EMPTY(lcVer))

*!*************************************************************
*! Name      : lfvOutDir
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Validate output file directory.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : .T. for valid directory
*!*************************************************************
*! Example   : =lfvOutDir()
*!*************************************************************
FUNCTION lfvOutDir
PRIVATE lcOldDir

lcOldDir = lcOutDir
lcOutDir = GETDIR(lcOutDir,'Send output file to directory:')
lcOutDir = IIF(EMPTY(lcOutDir),lcOldDir,lcOutDir)
@ 17,29 SAY PADR(lcOutDir,30,' ') COLOR (qClrSay)
RETURN(.T.)

*!*************************************************************
*! Name      : lfvFileName
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : Validate file name.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : .T. for valid file name.
*!*************************************************************
*! Example   : =lfvFileName()
*!*************************************************************
FUNCTION lfvFileName

IF EMPTY(lcFile)
  WAIT 'Must Enter output file name' WINDOW
  RETURN(.F.)
ENDIF
IF AT('.',lcFile) <> 0
  WAIT 'Enter output file name without extension' WINDOW
  lcFile = SPACE(8)
  RETURN(.F.)
ENDIF
RETURN(.T.)
*!*************************************************************
*! Name      : gfOpnFiles
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : To open the necessary files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  gfOpnFiles
*!*************************************************************
FUNCTION gfOpnFiles

=gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'CODES','CODES','SH')
=gfOpenFile(gcDataDir+'Customer', 'Customer', 'SH')
=gfOpenFile(gcDataDir+'ApVendor', 'VenCode', 'SH')
=gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')


=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
=gfOpenFile(gcDataDir+'POSLN' ,'POSLN' ,'SH')
=gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE' ,'SH')
=gfOpenFile(gcDataDir+'CutPick','CutPick' ,'SH')

=gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
=gfOpenFile(gcDataDir+'CUTTKTL','CUTTKTL','SH')
=gfOpenFile(gcDataDir+'PAXARLBL','PAXARLBL','SH')

SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
*!*************************************************************
*! Name      : lfGtNonMaj
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 B:130635
*! Purpose   : To get the nonmajor segements.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfGtNonMaj
*!*************************************************************
FUNCTION lfGtNonMaj

lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lnNonMajPo = 0
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
llStopConc = .F.
STORE '' TO lcNonMajPi,lcFreeClr
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''

*!*************************************************************
*! Name      : lpPrnHdr
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 
*! Purpose   : To the program's header.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcProg : The program's name
*!                       lcTitle: The program's title 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpPrnHdr
*!*************************************************************
PROCEDURE lpPrnHdr
PARAMETERS lcProg,lcTitle
SET COLOR TO &QCLRMAG
PRIVATE lnPos
@ 00,01 SAY lcProg+'('+gcAct_Comp+')'
lnPos = ((80-(LEN(TRIM(lcTitle))))/2)
@ 00,lnPos SAY lcTitle
@ 00,68 SAY gdSysDate
SET COLOR TO &QCLRNRM
RETURN


*!*************************************************************
*! Name      : DRAWBOX
*! Developer : MHM - Mohamed Shokry
*! Date      : 22/12/20055                 B:130635
*! Purpose   : To draw the box
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  Color,Title,TLine,TCol,BLine,BCol
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO DRAWBOX
*!*************************************************************
PROCEDURE DRAWBOX
PARAMETERS COLOR,TITLE,TLINE,TCOL,BLINE,BCOL

PRIVATE COLOR,TCOL,TLINE,TITLE,BLINE,BCOL

SET COLOR TO &COLOR
@ TLINE+1,TCOL+1 TO BLINE-1,BCOL-1 CLEAR

@ TLINE,TCOL,BLINE,BCOL BOX CHR(219)+CHR(223)+CHR(219)+CHR(219)+CHR(219);
+CHR(220)+CHR(219)+CHR(219)+" "

IF .NOT. EMPTY(TITLE) .AND. LEN(TITLE)>1
  XCOL = CENTER(TCOL,BCOL,TITLE)
  @ TLINE,XCOL SAY TITLE
ENDIF

SET COLOR TO &QCLRNRM
RETURN
