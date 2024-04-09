*:--------------------------------------------------------------------------
*: Program file        : MFPRCTCC.PRG C#101885
*: Program description : Custom CutTkt Form for Copy Cats
*:                     : Convert MFG820Z.prg from V2.6 To V2.7
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Manufactring - (MF)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Called From         : 1. Manufactring
*:                     : 2. OutPut
*:                     : 3. Print Cutting Ticket.
*:--------------------------------------------------------------------------
*: Example             : DO MFPRCTCC
*:--------------------------------------------------------------------------

*--- Temp File From CutTktL (lcMainF)
*--- Temp File From CtktBom (lcCostF)

*--Init variables.
xMaxRow  = 55
ROW      = 00
DIMENSION XTOTAL(9)
XTOTAL   = 0
lnCLen   = 0
lnClrPos = 0

DIMENSION laSetups[1,2]
laSetups[1,1] = 'M_DYELOT'
=gfGetMemVar(@laSetups,gcAct_Comp)
llDyelot = (laSetups[1,2] = 'Y')
STORE ' ' TO lcItmHdr , lcStyScl , lcContr1 , lcContr2 , lcContr3,;
             lcOpr1 , lcOpr2 , lcOpr3
=lfAdjSeg()
lcOpr = gfTempName()
CREATE TABLE (gcWorkDir+lcOpr) (Oper C(6), ContCode C(8) , Seq C(2))
INDEX ON Seq TAG &lcOpr

SELECT (lcMainF)
GO TOP
llNoRec = EOF()

DO WHILE !EOF()
  lcTempCut = CutTkt
  lcTempWar = cWareCode
  lcTempSty = Style
  SET DEVICE TO PRINT
  =lfGetCont(lcTempCut)
  =lfPrnMain()
ENDDO
SET DEVICE TO SCREEN
RETURN

*:--------------------------------------------------------------------------
*: Program file        : lpPrCtBom.
*: Program description : Print Cutting ticket Cost sheet.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lpPrCtBom()
*:--------------------------------------------------------------------------
*:
PROCEDURE lpPrCtBom

SELECT CTKTBOM
IF SEEK('M'+CUTTKTH.Cuttkt)
  LOCATE REST WHILE cIMTyp+Cuttkt = 'M'+CUTTKTH.Cuttkt FOR cCatgTyp = 'F'
  IF FOUND()
    DIMENSION laSetups[1,2]
    laSetups[1,1] = 'M_CMSLBL'+TYP
    =gfGetMemVar(@laSetups,gcAct_Comp)
    lcItmHdr='< '+ALLTRIM(laSetups[1,2])+' >'
    *....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*.2
    *ITEM    COLOR  WIDTH  VENDOR   PO#       YIELD     REQUIRED
    *XXXXXXX XXXXXX XXXXXX XXXXXXXX XXXXXX  999.999 99999999.999
    @ ROW,01 SAY 'ITEM    COLOR  WIDTH  VENDOR   PO#       YIELD     REQUIRED'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('-',(41-(LEN(lcItmHdr)/2)))
    @ ROW,(42-(LEN(lcItmHdr)/2)) SAY lcItmHdr
    @ ROW,(42-(LEN(lcItmHdr)/2)+LEN(lcItmHdr)) SAY REPLICATE('-',(41-(LEN(lcItmHdr)/2)))
    ROW=ROW+1
    SCAN WHILE cIMTyp+Cuttkt = 'M'+CUTTKTH.Cuttkt FOR cCatgTyp = 'F'
      IF ROW >= xMaxRow
        =lfOverPge(2)
      ENDIF
      =SEEK(SUBSTR(ITEM,1,7)+IIF(ICLR='******','',ICLR),'FABRIC')
      @ ROW,01 SAY SUBSTR(ITEM,1,7)
      @ ROW,09 SAY ICLR
      @ ROW,16 SAY FABRIC.WIDTH
      @ ROW,23 SAY FABRIC.VENDOR
      @ ROW,40 SAY UntQty
      @ ROW,48 SAY Req_Qty
      ROW=ROW+1
      @ ROW,01 SAY 'DESCRIPTION : '+FABRIC.DESC
      ROW=ROW+2
    ENDSCAN
  ENDIF
ENDIF
RETURN


*:--------------------------------------------------------------------------
*: Program file        : lpPrNtPad.
*: Program description : Print Cutting ticket Note Pad.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lpPrNtPad()
*:--------------------------------------------------------------------------
*:
PROCEDURE lpPrNtPad

SELECT NOTEPAD
@ ROW,00 SAY 'Manufacturing instructions:'
ROW=ROW+1
lnOldMemW = SET("MEMOWIDTH")  
SET MEMOWIDTH TO 75
lnCutLine = 1
lnMemLins = MEMLINES(NOTEPAD.MNOTES)
FOR lnCutLine = 1 TO lnMemLins
  IF ROW >= xMaxRow
    =lfOverPge(3)
  ENDIF
  @ ROW,01 SAY MLINE(MNOTES,lnCutLine)
  ROW = ROW+1
ENDFOR
SET MEMOWIDTH TO lnOldMemW
RETURN

*:--------------------------------------------------------------------------
*: Program file        : lfOverPge.
*: Program description : When page is over but the cuttkt is not.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfOverPge()
*:--------------------------------------------------------------------------
*:
FUNCTION lfOverPge
PARA lnPart

@ ROW,00 SAY REPLICATE('-',82)
@ ROW+1,31 SAY 'C O N T I N U E D '
@ ROW+2,00 SAY REPLICATE('-',82)
=SEEK(lcTempCut,'CUTTKTH')
@ 01,58 SAY 'TICKET# : '+CUTTKTH.CUTTKT
@ 02,58 SAY 'ENTERED :'
@ 02,68 SAY CUTTKTH.ENTERED
@ 03,58 SAY 'COMPLETE:'
@ 03,68 SAY CUTTKTH.COMPLETE
@ 04,58 SAY 'STATUS  : '
DO CASE
  CASE CUTTKTH.STATUS='O'
    @ 04,68 SAY 'OPEN'
  CASE CUTTKTH.STATUS='H'
    @ 04,68 SAY 'HOLD'
  CASE CUTTKTH.STATUS='X'
    @ 04,68 SAY 'CANCELLED'
  CASE CUTTKTH.STATUS='A'
    @ 04,68 SAY 'ACTUAL'
  CASE CUTTKTH.STATUS='C'
    @ 04,68 SAY 'COMPLETED'
  CASE CUTTKTH.STATUS='S'
    @ 04,68 SAY 'CLOSED'
ENDCASE
@ 07,31 SAY 'W O R K    O R D E R'

@ 19,00 SAY REPLICATE('-',81)
DO CASE
  CASE lnPart = 1
    @ 20,00 SAY 'COLOR'
    IF llDyelot
      @ 20,07 SAY 'DYELOT #'
    ELSE
      @ 20,07 SAY 'DESC.......'
    ENDIF
    @ 20,19 SAY lcStyScl
    @ 20,75 SAY 'TOTAL'
    @ 21,00 SAY REPLICATE('-',81)

  CASE lnPart = 2
    @ 20,01 SAY 'ITEM    COLOR  WIDTH  VENDOR   PO#       YIELD     REQUIRED'
    @ 21,01 SAY REPLICATE('-',(41-(LEN(lcItmHdr)/2)))
    @ 21,(42-(LEN(lcItmHdr)/2)) SAY lcItmHdr
    @ 21,(42-(LEN(lcItmHdr)/2)+LEN(lcItmHdr)) SAY REPLICATE('-',(41-(LEN(lcItmHdr)/2)))

  CASE lnPart = 3    
    @ 21,00 SAY 'Manufacturing instructions:'

ENDCASE
ROW=22

*:--------------------------------------------------------------------------
*: Program file        : lfPrnMain
*: Program description : Print Header Function.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfPrnMain()
*:--------------------------------------------------------------------------
*:
FUNCTION lfPrnMain

PRIVATE lnOldAls

lnOldAls = SELECT(0)
*--Contractor 1.
DO lpGetPrn WITH lcContr1,'V',lcContr2,lcOpr1
=lfPrnScl(lcTempSty)
=lfLinHdr()
=lfProceed(.F.)
SELECT(lnOldAls)

*:--------------------------------------------------------------------------
*: Program file        : lpGetPrn.
*: Program description : Function to get and print report header.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lpGetPrn()
*:--------------------------------------------------------------------------
*:
PROCEDURE lpGetPrn

PARAMETER XVENTO,XSTYP,XSHIP,XOPER

*-- Prepare the heder information from parameters .
*- Get the vendor to name and address.
IF !USED('APVENDOR')
  =gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')
ENDIF
SELECT APVENDOR
SET ORDER TO Vencode
=SEEK(PADR(XVENTO,8))
XVNAME = APVENDOR.cVenComp
DECLARE laAddOrd [ 6, 1 ]
lcPhonPict  = gfPhoneTem()
lcVenPhon   = cPhoneNo
laAddOrd[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
laAddOrd[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
laAddOrd[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
laAddOrd[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
laAddOrd[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
laAddOrd[6] = TRANSFORM(lcVenPhon , lcPhonPict)
=lfAdrShift('laAddOrd')

XVADDR1 = laAddOrd[1]
XVADDR2 = laAddOrd[2]
XVADDR3 = laAddOrd[3]
XVADDR4 = laAddOrd[6]

*- Get name and address for shipto.
IF XSTYP='V'
  SELECT APVENDOR
  =SEEK(XSHIP)
  XSTNAME = APVENDOR.cVenComp
  lcPhonPict  = gfPhoneTem()
  lcVenPhon   = cPhoneNo
  laAddOrd[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laAddOrd[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laAddOrd[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laAddOrd[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laAddOrd[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
  laAddOrd[6] = TRANSFORM(lcVenPhon , lcPhonPict)
  =lfAdrShift('laAddOrd')
  XSTADDR1 = laAddOrd[1]
  XSTADDR2 = laAddOrd[2]
  XSTADDR3 = laAddOrd[3]
  XSTADDR4 = laAddOrd[6]
ELSE            && USE COMPANY NAME
  XSTNAME  = laCompAdd[1]
  XSTADDR1 = laCompAdd[2]
  XSTADDR2 = laCompAdd[3]
  XSTADDR3 = laCompAdd[4]
  XSTADDR4 = laCompAdd[5]
ENDIF

*--Center the heder.
COL1=(ROUND((80-LEN(SUBSTR(laCompAdd[1],1,30)))/2,0))
COL2=(ROUND((80-LEN(SUBSTR(laCompAdd[2],1,30)))/2,0))
COL3=(ROUND((80-LEN(SUBSTR(laCompAdd[3],1,30)))/2,0))
COL4=(ROUND((80-LEN(SUBSTR(laCompAdd[4],1,30)))/2,0))
COL5=(ROUND((80-LEN(SUBSTR(laCompAdd[5],1,30)))/2,0))
*--Get mfg oper desc.

XOPER_DSC = ALLTRIM(gfCodDes(XOPER,'MFGCODE'))

*--Init variables.
DIMENSION XTOTAL(9)
*--Start print.

@ 01,COL1 SAY SUBSTR(laCompAdd[1],1,30)
=SEEK(lcTempCut,'CUTTKTH')
@ 01,58 SAY 'TICKET# : '+CUTTKTH.CUTTKT

@ 02,COL2 SAY SUBSTR(laCompAdd[2],1,30)
@ 02,58 SAY 'ENTERED :'
@ 02,68 SAY CUTTKTH.ENTERED

@ 03,COL3 SAY SUBSTR(laCompAdd[3],1,30)
@ 03,58 SAY 'COMPLETE:'
@ 03,68 SAY CUTTKTH.COMPLETE

@ 04,COL4 SAY SUBSTR(laCompAdd[4],1,30)
@ 04,58 SAY 'STATUS  : '
DO CASE
  CASE CUTTKTH.STATUS='O'
    @ 04,68 SAY 'OPEN'
  CASE CUTTKTH.STATUS='H'
    @ 04,68 SAY 'HOLD'
  CASE CUTTKTH.STATUS='X'
    @ 04,68 SAY 'CANCELLED'
  CASE CUTTKTH.STATUS='A'
    @ 04,68 SAY 'ACTUAL'
  CASE CUTTKTH.STATUS='C'
    @ 04,68 SAY 'COMPLETED'
  CASE CUTTKTH.STATUS='S'
    @ 04,68 SAY 'CLOSED'
ENDCASE

 @ 05,COL5 SAY SUBSTR(laCompAdd[5],1,30)

@ 07,31 SAY 'W O R K    O R D E R'

@ 09,04 SAY '........ VENDOR .........'
@ 09,32 SAY '........ SHIP TO ........'

@ 10,04 SAY SUBSTR(XVNAME ,1,25)
@ 10,32 SAY SUBSTR(XSTNAME,1,25)

@ 11,04 SAY SUBSTR(XVADDR1 ,1,25)
@ 11,32 SAY SUBSTR(XSTADDR1,1,25)

@ 12,04 SAY SUBSTR(XVADDR2 ,1,25)
@ 12,32 SAY SUBSTR(XSTADDR2,1,25)

@ 13,04 SAY SUBSTR(XVADDR3 ,1,25)
@ 13,32 SAY SUBSTR(XSTADDR3,1,25)

IF !EMPTY(XVNAME)
  @ 14,04 SAY XVADDR4
ENDIF
@ 14,32 SAY XSTADDR4

@ 16,04 SAY XOPER_DSC
*--- trancd+ctktno+style
*=SEEK('1'+CUTTKTH.CutTkt+CUTTKTH.STYLE,'CutPick')
*=SEEK('O'+CutPick.Order,'OrdHdr')
@ 16,26 SAY 'ACCOUNT: '+ OrdHdr.ACCOUNT
@ 16,47 SAY 'ORDER: '  + CutPick.ORDER

@ 18,04 SAY 'STYLE : '+ALLTRIM(CUTTKTH.STYLE)+' '+SUBSTR(CUTTKTH.DESC,1,23)
@ 18,47 SAY 'MARKER : '+CUTTKTH.PATTERN

*:--------------------------------------------------------------------------
*: Program file        : lfPrnLin
*: Program description : Print Lines.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfPrnLin()
*:--------------------------------------------------------------------------
*:
FUNCTION lfPrnLin

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--- lnCLen
*--- lnClrPos
IF ROW >= xMaxRow
  =lfOverPge(1)
ENDIF
@ ROW,00 SAY SUBSTR(STYLE,lnClrPos,lnCLen)
IF llDyelot
  @ ROW,07 SAY DYELOT
ELSE
  XCLR_DESC = SUBSTR(gfCodDes(ALLTRIM(SUBSTR(Style,lnClrPos,lnCLen)),'COLOR'),1,11)
  @ ROW,07 SAY XCLR_DESC
ENDIF
XCOL=11
FOR X=1 TO 8
  Z=STR(X,1)
  XCOL=XCOL+7
IF QTY&Z <> 0
  @ ROW,XCOL SAY QTY&Z PICTURE '@Z 999999'
ENDIF
  XTOTAL(X)=XTOTAL(X)+QTY&Z
ENDFOR
@ ROW,73 SAY TOTQTY PICTURE '9999999'
XTOTAL(9)=XTOTAL(9)+TOTQTY
ROW = ROW + 1
SELECT(lnOldAls)


*:--------------------------------------------------------------------------
*: Program file        : lfPrnScl
*: Program description : Print Style Scales.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfPrnScl()
*:--------------------------------------------------------------------------
*:
FUNCTION lfPrnScl
PARAMETER lcStyle

PRIVATE lnOldAls
lnOldAls = SELECT(0)
lcStyScl=SPACE(0)
IF SEEK(lcStyle,'STYLE')
  SELECT SCALE 
  IF SEEK('S'+STYLE.SCALE )
    lcStyScl=SPACE(5-LEN(ALLTRIM(SZ1)))+ALLTRIM(SZ1)+'  '+;
            SPACE(5-LEN(ALLTRIM(SZ2)))+ALLTRIM(SZ2)+'  '+;
            SPACE(5-LEN(ALLTRIM(SZ3)))+ALLTRIM(SZ3)+'  '+;
            SPACE(5-LEN(ALLTRIM(SZ4)))+ALLTRIM(SZ4)+'  '+;
            SPACE(5-LEN(ALLTRIM(SZ5)))+ALLTRIM(SZ5)+'  '+;
            SPACE(5-LEN(ALLTRIM(SZ6)))+ALLTRIM(SZ6)+'  '+;
            SPACE(5-LEN(ALLTRIM(SZ7)))+ALLTRIM(SZ7)+'  '+;
            SPACE(5-LEN(ALLTRIM(SZ8)))+ALLTRIM(SZ8)
  ENDIF
ENDIF
SELECT(lnOldAls)

*:--------------------------------------------------------------------------
*: Program file        : lfLinHdr
*: Program description : Print Header of the lines.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfLinHdr()
*:--------------------------------------------------------------------------
*:
FUNCTION lfLinHdr

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--Display heder for lines.
@ 19,00 SAY REPLICATE('-',81)
@ 20,00 SAY 'COLOR'
IF llDyelot
  @ 20,07 SAY 'DYELOT #'
ELSE
  @ 20,07 SAY 'DESC.......'
ENDIF
@ 20,19 SAY lcStyScl
@ 20,75 SAY 'TOTAL'
@ 21,00 SAY REPLICATE('-',81)
ROW=22
SELECT(lnOldAls)

*:--------------------------------------------------------------------------
*: Program file        : lfPrnFoot
*: Program description : Print Fotter.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfPrnFoot()
*:--------------------------------------------------------------------------
*:
FUNCTION lfPrnFoot

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--Print Footer totals.
@ ROW,00 SAY REPLICATE('=',82)
ROW=ROW+1
@ ROW,01 SAY 'TOTAL:'
XCOL=11
FOR X=1 TO 8
  XCOL=XCOL+7
  IF XTOTAL(X) <> 0
    @ ROW,XCOL SAY XTOTAL(X) PICTURE '@Z 999999'
  ENDIF
ENDFOR
@ ROW,73 SAY XTOTAL(9)  PICTURE '9999999'
ROW=ROW+1
@ ROW,00 SAY REPLICATE('-',82)
ROW=ROW+1
@ ROW,01 SAY 'TOTAL DOZEN:'
@ ROW,73 SAY (XTOTAL(9)/12)  PICTURE '9999999'
ROW=ROW+1
@ ROW,00 SAY REPLICATE('=',82)
ROW=ROW+1
FOR lnInd = 1 TO ALEN(XTOTAL)
  XTOTAL[lnInd]   = 0
ENDFOR
*--End of totals.
SELECT(lnOldAls)

*:--------------------------------------------------------------------------
*: Program file        : lfPrnBom
*: Program description : Print Bom Cost Sheet.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfPrnBom()
*:--------------------------------------------------------------------------
*:
FUNCTION lfPrnBom
PARAMETER lcCutTkt

PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF SEEK(lcCutTkt,'CutTktH')
  IF llrPrtCs
    DO lpPrCtBom
  ENDIF
  IF llRPrtCtn .AND. SEEK('I'+CUTTKTH.CUTTKT,'NOTEPAD')
    DO lpPrNtPad
  ENDIF
ENDIF
SELECT(lnOldAls)

*:--------------------------------------------------------------------------
*: Program file        : lfGetCont
*: Program description : Print To Get Contractors of operations.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfGetCont()
*:--------------------------------------------------------------------------
*:
FUNCTION lfGetCont
PARAMETER lcCutTkt
PRIVATE lnOldAls , lnInd , lcInd

lnOldAls = SELECT(0)
lnInd = 1
lcInd = '1'
lcOpr1   = '  '
lcContr1 = '  '
lcOpr2   = '  '
lcContr2 = '  '

SELECT (lcOpr)
ZAP
DECLARE laMfgRFld[3,2]
STORE ''  TO laMfgRFld , lcContCode , lnOrder
llMfgOpr = .T.
laMfgRFld[1,1] = 'CCONTCODE'
laMfgRFld[1,2] = 'lcContCode'
laMfgRFld[2,1] = 'COPERSEQ'
laMfgRFld[2,2] = 'lnOrder'
laMfgRFld[3,1] = 'LMFGOPR'
laMfgRFld[3,2] = 'llMfgOpr'

SELECT CtktBom
*--- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
IF SEEK("M"+lcCutTkt)
  SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot=;
                  "M"+lcCutTkt;
            FOR   cCatgTyp = "M" .AND. lnInd < 4 .AND. !lVoid
    lcInd = STR(lnInd,1)
    = gfRltFld(CtktBom.MfgCode,@laMfgRFld,'MFGCODE')
    IF llMfgOpr
      SELECT (lcOpr)
      APPEND BLANK
      REPLACE Oper     WITH CtktBom.MfgCode,;
              ContCode WITH lcContCode,;
              Seq      WITH ALLTRIM(lnOrder)
      lnInd = lnInd + 1
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcOpr)
GOTO TOP
lnInd = 1
SCAN WHILE lnInd<4
  lcInd  = STR(lnInd,1)
  lcOpr&lcInd   = Oper
  lcContr&lcInd = ContCode
  lnInd = lnInd + 1
ENDSCAN

SELECT(lnOldAls)

*:--------------------------------------------------------------------------
*: Program file        : lfAdjSeg
*: Program description : Get the style code segments information.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfAdjSeg()
*:--------------------------------------------------------------------------
*:
FUNCTION lfAdjSeg

STORE 0 TO lnCPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.
DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
FOR lnC = 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnC,1] = 'C'
    lnCPos = LEN(laMajSeg[lnC -1 ,3])
    IF EMPTY(laMajSeg[lnC,5])
      laMajSeg[lnC,5] = 'Color'
    ENDIF
    lnCLen = LEN(laMajSeg[lnC,3])
    lnClrPos = laMajSeg[lnC,4]
    EXIT
  ENDIF
ENDFOR


*:--------------------------------------------------------------------------
*: Program file        : lfAdrShift.
*: Program description : Function to Shift the Address array if there is any
*:                       empty lines in the address
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfAdrShift(Array Name)
*:--------------------------------------------------------------------------
*:
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
    EMPTY(&lcArrayNam.[lnCount])
    = ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR


*:--------------------------------------------------------------------------
*: Program file        : lfProceed
*: Program description : Function to procced in printing lines
*:                       empty lines in the address
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101885
*:--------------------------------------------------------------------------
*: Example             : = lfProceed()
*:--------------------------------------------------------------------------
*:
FUNCTION lfProceed
PARAMETER llNotSeek
PRIVATE lnOld
lnOld = SELECT(0)
IF SEEK(lcTempCut+lcTempWar+lcTempSty,lcMainF)
  SELECT (lcMainF)
  SCAN REST WHILE CutTkt+cWareCode+Style+Dyelot+NoteFlag=;
                  lcTempCut+lcTempWar+SUBSTR(lcTempSty,1,lnClrPos-2);
            FOR NoteFlag='N'
    =lfPrnLin()
  ENDSCAN
  =lfPrnFoot()
  llDummy = !llNotSeek .AND. lfPrnBom(lcTempCut)
ENDIF
llDummy = llNotSeek .AND. SEEK(lcTempCut+lcTempWar+lcTempSty)
SELECT(lnOld)