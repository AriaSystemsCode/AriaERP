*:***************************************************************************
*: Program file  : RMRAUTAN.PRG
*: Program desc. : R/A RECEIPT FORM Z
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Return Marchandize (RM)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Andrew Mark
*: C101390,1 KHM 02/09/99
*:***************************************************************************
*: Modifications:
*: B607259,1 KHM 05/25/2003 Fix bug of screen corruption after printer to printer
*:***************************************************************************

*-- To get the setting of the company's warehouse.
llMultiWH  = (gfGetMemVar('M_WareHouse') = 'Y')

*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))

*-- To get the color segment position
STORE 0  TO lnFreeLen,lnColorLen,lnNonMajSt
STORE '' TO lcNonMajPi,lcNonMajTl,lcPrintTxt
=lfEvalSegs()

*-- Filtering the RetAuth file according to the selected criteria.
lcRpExp = lcRpExp + " AND Status = 'O'"
SELECT RetAuth
SET FILTER TO &lcRpExp

GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  llNoRec  = .T.
  
  *B607259,1 KHM 05/25/2003 (Begin) Set device to screen.
  SET DEVICE TO SCREEN
  *B607259,1 KHM 05/25/2003 (End)
  
  RETURN
ENDIF

MAXROW  = 50
NEWDOC  = .T.

HLine1 = lcCompName

=gfOpenFile(gcDataDir+'InvHdr','InvHdr','SH')

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
*--------------------------------------------------------------------------------
*STYLE        COLOR  DESCRIPTION         SZ1  SZ2  SZ3  SZ4 SZ5 SZ6 SZ7 SZ8  TOTQTY
*--------------------------------------------------------------------------------
*123456789012 123456 123456789012345678  999  999  999  999 999 999 999 999  999999    
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8

A = REPLICATE('*',80)
B = 'STYLE        COLOR  DESCRIPTION                                   TOTQTY  PRICE'
DIMENSION laAddress[6,3]
laAddress = ''
SET DEVICE TO PRINT

SELECT RetAuth
DO WHILE .T. AND INKEY()<>32
  WAIT WINDOW "R/A #: " + RaNo NOWAIT
  *-- GET R/A INFORMATION
  SELECT RetAuth
  IF EOF() 
    EXIT
  ENDIF
 
  IF NEWDOC
    NEWDOC   = .F.
    XRANO    = RANO
    XACCOUNT = ACCOUNT
    XVOID    = VOID
    XREASON  = REASON
    XINVOICE = INVOICE
    XORDER   = ORDER
    XNOTE1   = IIF(cRetNote1 = '*',' ',cRetNote1)
    XNOTE2   = IIF(cRetNote2 = '*',' ',cRetNote2)
    XNOTE3   = IIF(cRetNote3 = '*',' ',cRetNote3)
    XNOTE4   = IIF(cRetNote4 = '*',' ',cRetNote4)

    laWareAdd[1]= gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    laWareAdd[2]= gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    laWareAdd[3]= gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    laWareAdd[4]= gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
    laWareAdd[5]= gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
    laWareAdd[6]= TRANSFORM(WareHous.cphone , lcPhonPict)
    = lfAdrShift('laWareAdd') 
    Hline2 = laWareAdd[2]
    Hline3 = laWareAdd[3]
    Hline4 = laWareAdd[4]
    Hline5 = laWareAdd[5]
    
    HLine1 = SPACE(ROUND((80-LEN(ALLTRIM(HLine1)))/2,0))+ALLTRIM(HLine1)
    HLine2 = SPACE(ROUND((80-LEN(ALLTRIM(HLine2)))/2,0))+ALLTRIM(HLine2)
    HLine3 = SPACE(ROUND((80-LEN(ALLTRIM(HLine3)))/2,0))+ALLTRIM(HLine3)
    HLine4 = SPACE(ROUND((80-LEN(ALLTRIM(HLine4)))/2,0))+ALLTRIM(HLine4)
    HLine5 = SPACE(ROUND((80-LEN(ALLTRIM(HLine5)))/2,0))+ALLTRIM(HLine5)
   
    *-- To get the description of the return reason.
    XREAS_DATA = gfCodDes(RetAuth.Reason,'REASON    ')
      
    SELECT CUSTOMER
    SEEK 'M'+XACCOUNT    
    XBTNAME  = BTNAME
    
    =gfGetAdr('Customer','','','',1,'2')
    XBTADDR1 = laAddress[1,2]
    XBTADDR2 = laAddress[2,2]
    XBTADDR3 = ALLTRIM(laAddress[3,2]) + ' ' +ALLTRIM(laAddress[4,2]) + ' ' + ALLTRIM(laAddress[5,2])
    IF EMPTY(XBTADDR2)
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF
  ENDIF

  @ 01,01 SAY HLINE1
  @ 01,60 SAY 'R/A     #: '+XRANO
  @ 02,01 SAY HLINE2
  @ 02,60 SAY 'VOID     : '+DTOC(XVOID)
  @ 03,01 SAY HLINE3
  @ 04,01 SAY HLINE4
  @ 05,01 SAY HLINE5
  @ 07,26 SAY 'R / A   R E C E I P T   F O R M'

  SELECT RetAuth

  @ 11,01 SAY '........ SOLD TO ........'
  @ 11,42 SAY 'DATE   :'
  @ 11,50 SAY RADATE
  @ 12,01 SAY ACCOUNT
  @ 12,42 SAY 'CUSTPO :'+CUSTPO
  @ 13,01 SAY XBTNAME

  @ 13,42 SAY 'INVOICE:' + INVOICE +IIF(llMultiWH,;
              '   WAREHOUSE:' + CWARECODE,'')
  @ 14,01 SAY XBTADDR1
  @ 14,42 SAY 'ORDER  :' + ORDER
  @ 15,01 SAY XBTADDR2
  @ 15,42 SAY 'TOT.PCS:' + STR(AUTH,4)
  @ 16,01 SAY XBTADDR3
  @ 16,42 SAY 'CARTONS:' + STR(CARTONS,4)
  @ 17,42 SAY 'REASON :'  + XREAS_DATA

  XNOTES = 'NOTES:'
  ROW    = 18

  FOR lnCounter = 1 TO 4
    lcCounter = STR(lnCounter,1)
    IF .NOT. EMPTY( XNOTE&lcCounter )
      @ ROW,01 SAY XNOTES + XNOTE&lcCounter
      XNOTES = SPACE(6)
      ROW = ROW + 1
    ENDIF
  ENDFOR

  @ ROW,01 SAY A
  ROW = ROW + 1
  @ ROW,01 SAY B
  ROW = ROW + 1
  @ ROW,01 SAY A
  ROW = ROW + 2

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
*--------------------------------------------------------------------------------
*STYLE        COLOR  DESCRIPTION         SZ1  SZ2  SZ3  SZ4 SZ5 SZ6 SZ7 SZ8  TOTQTY
*--------------------------------------------------------------------------------
*123456789012 123456 123456789012345678  ---  ---  ---  --- --- --- --- ---  ------
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
  SELECT RaLine
  IF XRANO <> RANO
    SEEK XRANO
  ENDIF
  SCAN WHILE RaNo+Style+Cra_LinNo = XRaNo .AND. ROW < MAXROW  
    XDESCRIPT = Style.Desc
    @ ROW,01 SAY SUBSTR(Style,1,lnMajLen)
    @ ROW,14 SAY SUBSTR(Style,lnNonMajSt,lnColorLen)
    @ ROW,21 SAY SUBSTR(XDESCRIPT,1,18)
    @ ROW,72 SAY Price       
    ROW = ROW + 1
    STORE 1 TO lnCol  
    FOR lnCount = 1 TO Scale.Cnt
      lcCount = STR(lnCount,1)
      IF Qty&lcCount > 0
        lcPrintTxt = IIF(!EMPTY(QTY&lcCount),ALLTRIM(SCALE.SZ&lcCount)+"/";
                        +ALLTRIM(STR(Qty&lcCount)),"")
        @ ROW,lnCol SAY lcPrintTxt
        lnCol = lnCol + LEN(ALLTRIM(lcPrintTxt)) + 1
      ENDIF  
    ENDFOR 
    @ ROW,67 SAY TOTQTY PICTURE "999999"
    ROW = ROW + 1
  ENDSCAN      
  IF XRaNo = RaNo
    LOOP
  ENDIF

  lnDisc = RETAUTH.AuthAmt * IIF(!EMPTY(RETAUTH.Invoice) AND;
           SEEK(RETAUTH.Invoice,"INVHDR"),INVHDR.DiscPcnt/100,0); 

  @ MAXROW+4,40 SAY 'DISCOUNT=>'
  @ MAXROW+4,51 SAY lnDisc PICTURE '999.99'
  @ MAXROW+5,01 SAY A
  @ MAXROW+6,40 SAY 'TOTAL CREDIT =>'
  @ MAXROW+6,56 SAY RETAUTH.AuthAmt - lnDisc PICTURE '9999999.99'
  @ MAXROW+7,01 SAY A

  SELECT RETAUTH
  REPLACE FLAG WITH 'P'
  IF !EOF()
    SKIP
  ELSE
    EXIT
  ENDIF  
  NEWDOC = .T.
ENDDO
WAIT CLEAR

*B607259,1 KHM 05/25/2003 (Begin) Set device to screen.
SET DEVICE TO SCREEN
*B607259,1 KHM 05/25/2003 (End)

RETURN

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 02/09/1999
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg = gfItemMask('SM')  && No. of major segments.

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.      
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

RETURN ''