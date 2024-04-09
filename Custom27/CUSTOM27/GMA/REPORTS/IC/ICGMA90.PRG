****************************************************************************
*: Program file      : ICGMA90.PRG (C#101800)
*: Program desc.     : STYLE MASTER LIST REPORT
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : Khalid Mohi El-Din Mohamed KHM
*: Date              : 05/17/2000
*:**************************************************************************
*:Modifications  :
*:B803354,1   AMH 07/17/2000 Fix Printing Division and Group Description
*:B604079,1   WAB 12/26/2000 fix the bug of "Variable PICTURE not found"
*:**************************************************************************

*-- To get the length of the major segment
lcTmpFile  = gfTempName()
lcMjrPct   = gfItemMask('PM')
lnMajWidth = LEN(lcMjrPct)
R_WIDTH    = 'N'
lcRpExp    = lcRpExp + IIF(lcRpStatus<>'L',' AND Status = lcRpStatus','')

SELECT Style
SET ORDER TO Style

*-- Creating a temporary file from the style's file sturcture.
COPY STRUCTURE TO (gcWorkDir+lcTmpFile)
=gfOpenFile ('&gcWorkDir.&lcTmpFile',' ','EX')
INDEX ON Style TAG(lcTmpFile)

*-- If the user selects Styles.
lcStyFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    GOTO TOP
  ENDIF  
ENDIF

IF !EMPTY(lcStyFile) AND USED(lcStyFile) AND !EOF(lcStyFile)
  SELECT (lcStyFile)
  SCAN
    lcStyle = SUBSTR(cStyMajor,1,lnMajWidth)
    IF SEEK(lcStyle,'Style')
      SELECT Style
      SCAN REST WHILE cStyMajor = lcStyle FOR &lcRpExp
        WAIT WINDOW 'Style - Color: ' + Style NOWAIT
        SCATTER MEMVAR MEMO
        INSERT INTO (lcTmpFile) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  SELECT Style
  SCAN FOR &lcRpExp
    WAIT WINDOW 'Style - Color: ' + Style NOWAIT
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTmpFile) FROM MEMVAR
  ENDSCAN
ENDIF
WAIT CLEAR
SELECT Style
SET ORDER TO cStyle

SELECT (lcTmpFile)
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  USE 
  ERASE(gcWorkDir+lcTmpFile+'.DBF')
  ERASE(lcTmpFile+'.CDX')
  RETURN
ENDIF

PAGENO = 0
MAXROW = 52

*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
*STYLE: 1234567 12345678901234567890      STATUS: 123456
*
*SEASON..: 123456 1234567890   FABRIC  : 12345 1234567890  PRICEA : 1234.99
*DIVISION: 123456 1234567890   COMPLETE: 99/99/99          PRICEB : 1234.99
*GROUP   : 123456 1234567890   SOLDOUT : 99/99/99          PRICEC : 1234.99
*PATTERN : XXXXXXXXXX      LOCATION: XXXXXXXXXX        WEIGHT : 1234.99
*PICTURE : XXXXXXXX      COMMISSION: X                 QTY/CTN: 1234
*SIZE (X): 111 222 333 444 555 666 777 888
*PREPK(X): 111 222 333 444 555 666 777 888
*COLORS  : 111 1234567890 222 1234567890 333 1234567890 444 1234567890
*          111 1234567890 222 1234567890 333 1234567890 444 1234567890
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3

ROW     = 99
R_TITLE = 'STYLE MASTER LIST'
SET DEVICE TO PRINT

DO WHILE INKEY() <>32   
  WAIT WINDOW 'PRINTING ... <Space Bar> to Abort' NOWAIT
  XSTYLE = SUBSTR(STYLE,1,lnMajWidth)

  IF ROW >= MAXROW
    PAGENO = PAGENO +1
    DO RPT_HDR WITH 'ICGMA90',lcRpTitle,R_WIDTH
    ROW = 5
  ENDIF

  *-- Season Code
  XSEAS_DATA = gfCodDes(&lcTmpFile..Season,'SEASON')
  XSEAS_DATA = IIF(&lcTmpFile..Season = 'Y ','YEAR ROUND',XSEAS_DATA)
  
  *-- Division Code
  *B803354,1   AMH 07/17/2000 Fix Printing Division and Group Description (Start)
  *XDIVI_DATA = gfCodDes(&lcTmpFile..cDivision,'DIVISION')
  XDIVI_DATA = gfCodDes(&lcTmpFile..cDivision,'CDIVISION')

  *-- Group Code
  *XGROU_DATA = gfCodDes(&lcTmpFile..cStyGroup,'GROUP')
  XGROU_DATA = gfCodDes(&lcTmpFile..cStyGroup,'CSTYGROUP')
  *B803354,1   AMH 07/17/2000 Fix Printing Division and Group Description (End)

  * LINE 1
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  * STYLE: 123456789012  12345678901234567890             STATUS: 123456789
  @ ROW,01 SAY 'STYLE:'
  @ ROW,08 SAY SUBSTR(STYLE,1,lnMajWidth)
  @ ROW,22 SAY DESC
  @ ROW,55 SAY 'STATUS:'
  DO CASE
    CASE Status = 'A'
      @ ROW,63 SAY 'ACTIVE  '
    CASE Status = 'H'
      @ ROW,63 SAY 'On Hold'
    CASE Status = 'X'
      @ ROW,63 SAY 'CANCELLED'
  ENDCASE
  ROW = ROW + 2

  * LINE 3
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *SEASON    : 123456 1234567890   FABRIC  : 1234567 1234567890  PRICEA : 1234.99
  @ ROW,01 SAY 'SEASON    :'
  @ ROW,13 SAY ALLTRIM(SEASON) + ' ' +SUBSTR(ALLTRIM(XSEAS_DATA),1,10)
  *@ ROW,19 SAY SUBSTR(ALLTRIM(XSEAS_DATA),1,10)
  *@ ROW,32 SAY 'FABRIC  :'
  @ ROW,33 SAY 'FABRIC  :'
  *@ ROW,42 SAY FABRIC
  @ ROW,43 SAY FABRIC
  @ ROW,63 SAY 'PRICEA :'
  @ ROW,72 SAY PRICEA PICTURE '9999.99'
  ROW = ROW + 1

  * LINE4
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *DIVISION  : 12 1234567890   START   : 99/99/99          PRICEB : 1234.99
  @ ROW,01 SAY 'DIVISION  :'
  @ ROW,13 SAY ALLTRIM(cDivision)+' '+SUBSTR(ALLTRIM(XDIVI_DATA),1,10)
  *@ ROW,19 SAY SUBSTR(ALLTRIM(XDIVI_DATA),1,10)
  @ ROW,33 SAY 'START   :'    
  @ ROW,43 SAY START          
  @ ROW,63 SAY 'PRICEB :'
  @ ROW,72 SAY PRICEB PICTURE '9999.99'
  ROW = ROW + 1

  * LINE 5
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *GROUP     : 12 1234567890   SOLDOUT : 99/99/99          PRICEC : 1234.99
  @ ROW,01 SAY 'GROUP     :'
  @ ROW,13 SAY ALLTRIM(cStyGroup)+' '+SUBSTR(ALLTRIM(XGROU_DATA),1,10)
  @ ROW,33 SAY 'SOLDOUT :'
  @ ROW,43 SAY SOLDOUT
  @ ROW,63 SAY 'PRICEC :'
  @ ROW,72 SAY PRICEC PICTURE '9999.99'
  ROW = ROW + 1

  * LINE 6
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *PATTERN   : XXXXXXXXXX      LOCATION: XXXXXXXXXX        WEIGHT : 1234.99
  @ ROW,01 SAY 'PATTERN   :' 
  @ ROW,13 SAY PATTERN
  @ ROW,33 SAY 'LOCATION:'
  @ ROW,43 SAY LOCATION
  @ ROW,63 SAY 'WEIGHT :'
  @ ROW,72 SAY nStyWeight PICTURE '9999.99'
  ROW = ROW + 1

  * LINE 7
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *PICTURE   : XXXXXXXX      COMMISSION: X                 QTY/CTN: 1234
  *B604079,1 WAB (START ) Field Picture not exist in aria27 Standard commited and reorder the next 2 coloumn
  *@ ROW,01 SAY 'PICTURE   :'
  *@ ROW,13 SAY PICTURE
  *@ ROW,31 SAY 'COMMISSION:'
  *@ ROW,43 SAY IIF(COMMISSION,'Y','N')
  *@ ROW,63 SAY 'QTY/CTN:'            
  *@ ROW,72 SAY QTY_CTN PICTURE '9999' 
  @ ROW,01 SAY 'COMMISSION:'
  @ ROW,13 SAY IIF(COMMISSION,'Y','N')
  @ ROW,33 SAY 'QTY/CTN :'            
  @ ROW,43 SAY QTY_CTN PICTURE '9999' 
  *B604079,1 WAB (END)
  ROW = ROW + 1

  * LINE 8
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *Long desc.: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  @ ROW,01 SAY 'Long desc.:'
  @ ROW,13 SAY &lcTmpFile..Desc1
  ROW = ROW + 1 

  * LINE 10        
  *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3
  *COLORS    : 111111 12345678901  222222 12345678901  333333 12345678901

  IF ROW < MAXROW
    @ ROW,01 SAY 'COLORS    :'
  ENDIF  
  COL = 13
  DO WHILE SUBSTR(STYLE,1,lnMajWidth) = XSTYLE
    IF EOF()
      EXIT
    ENDIF

    IF ROW >= MAXROW
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH 'ICGMA90',lcRpTitle,R_WIDTH
      ROW = 5
      @ ROW,01 SAY 'COLORS    :'
    ENDIF

    @ ROW,COL SAY SUBSTR(&lcTmpFile..Style,lnStartPos,lnNonMajLn)
    XCOLO_DATA = gfCodDes(SUBSTR(&lcTmpFile..Style,lnStartPos,lnNonMajLn),'COLOR')
    COL = COL + 7
    @ ROW,COL SAY SUBSTR(ALLTRIM(XCOLO_DATA),1,10)
    SELECT (lcTmpFile)
    SKIP
    ROW = IIF( COL >= 57 , ROW + 1  , ROW )
    COL = IIF( COL <= 57 , COL + 13 , 13) 
    IF STYLE<> XSTYLE .AND. COL = 14
      ROW = ROW - 1
    ENDIF
  ENDDO
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('-',80)
  ROW = ROW + 1
  SELECT (lcTmpFile) 
  IF EOF()
    EXIT
  ENDIF
ENDDO

WAIT CLEAR
DO EndReport
SET DEVICE TO SCREEN
IF USED(lcTmpFile)
  USE IN (lcTmpFile)
  ERASE(gcWorkDir+lcTmpFile+'.DBF')
  ERASE(lcTmpFile+'.CDX')
ENDIF
RETURN



*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 05/17/2000 
*! Purpose   : The When function of the Option Grid.
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH = 'N'


*!*************************************************************
*! Name      : lfSetSty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 05/17/2000 
*! Purpose   : To set or reset the Style browse
*!*************************************************************
*! Example     : = lfSetSty()
*!*************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
   CASE OpGrdParm = 'S'
      SET ORDER TO TAG CSTYLE IN STYLE
      GOTO TOP IN Style
   CASE OpGrdParm = 'R'
      SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 05/17/2000 
*! Purpose   : To validate the fabric field.
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lcFabObj,lcFab

lcFabObj = VARREAD()
lcFab    = &lcFabObj

IF !EMPTY(lcFab) AND !SEEK(lcFab,'FABRIC')
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
ENDIF


*!*************************************************************
*! Name      : lfEvalSeg
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 05/17/2000 
*! Purpose   : To evaluate the non-major segment.
*!*************************************************************
*! Example     : = lfEvalSeg()
*!*************************************************************
FUNCTION lfEvalSeg
PRIVATE lnClrPos
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
lnClrPos   = INT(ASCAN(laMajSeg,'C')/7+0.9)
lnStartPos = laMajSeg[lnClrPos,4]
lnNonMajLn = LEN(laMajSeg[lnClrPos,3])
