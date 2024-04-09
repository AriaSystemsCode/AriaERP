*:***************************************************************************
*: Program file  : ICFG100 
*: Program desc. : Stock Balance Report for customer F&G
*! Date          : 01/06/2000
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Ahmed Mohamed Ibrahim (AMM)
*: Reference     : C200107,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*:Modifications : 
*:B603536,1 WMA 04/24/00 if the size Qty is zero don't print "0" and just leave it blank
*                        and print the Location the user filters on.
*:C200116,1 RAMY 06/06/2000 Add new variable filtre
*:***************************************************************************

*B603536,1 WMA Call Procedure to print location(s) [Begin]
PRIVATE lcWhFL,lcWhSL
STORE SPACE(0) TO lcWhFL,lcWhSL
DO lpWhsDesc 
*B603536,1 WMA [End]

*-- Disable/enable Only This colors, Free Segment. [begin]


*DO CASE
*  CASE lcFreeClr = 'C'
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
*    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
*    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')

*--Ramy
*IF llOGFltCh
  
  *C200116,1 RAMY [start]
  *lnVarPos = ASCAN(laOgVrFlt , 'lnRpMQty')
  *IF laOgVrFlt[lnVarPos + 5] > 0
  *  lnPos1  = AT('lnRpMQty' , lcRpExp)
  *  lnPos2  = AT( ALLTRIM(STR(laOgVrFlt[lnVarPos + 5]))+ ')' , lcRpExp)
  *  lcSign  = ALLTRIM(SUBSTR(lcRpExp , lnPos1 + 9 , 2))
  *  lcNExp1 = LEFT(lcRpExp,lnPos1-5)
  *  lnLen   = LEN(ALLTRIM(STR(laOgVrFlt[lnVarPos+5])))+1
  *  lcNExp2 = SUBSTR(lcRpExp,lnPos2 + lnLen)
  *  lcRpExp = lcNExp1 + lcNExp2
  *  lnRpMQty = laOgVrFlt[lnVarPos + 5]
  *ELSE
  *  lnRpMQty = 0
  *  lcSign  = '='
  *ENDIF
  lnVarPos = ASUBSCRIPT(laOgVrFlt,ASCAN(laOgVrFlt , 'lnQty'),1)
  lnQty = laOgVrFlt[lnVarPos , 6]
  lcSign   = ALLTRIM(UPPER(laOgVrFlt[lnVarPos , 5]))
  DO CASE
    CASE lcSign = "LIKE"
      lcSign = '='
    
    CASE lcSign = "GREATER THAN"
      lcSign = '>'
       lcRpEXp = STRTRAN(lcRpExp , 'lnQty >' , 'lnQty >=')
      
    CASE lcSign = "GREATER OR EQUAL"
      lcSign = '>='
      
    CASE lcSign = "LESS THAN"
      lcSign = '<'
      lcRpExp = STRTRAN(lcRpExp , 'lnQty <' , 'lnQty <=')

    CASE lcSign = "LESS OR EQUAL"
      lcSign = '<='

    CASE lcSign = "CONTAINS"
      lcSign = '$'

    CASE lcSign = "GRATER THAN OR EQUAL"
      lcSign = '>='

    CASE lcSign = ALLTRIM("BETWEEN")
      
      lnSparat  = AT('|' , lnQty)
      lnFrstNum = VAL(LEFT(lnQty , lnSparat - 1))
      lnScndNum = VAL(RIGHT(lnQty , LEN(lnQty) - lnSparat))
      lnQty     = (lnScndNum + lnFrstNum)/2

    CASE lcSign = ALLTRIM("IN LIST")

      lnSparat  = AT('|' , lnQty)
      lnFrstNum = IIF(lnSparat > 0, VAL(LEFT(lnQty , lnSparat - 1)) , VAL(lnQty))
      lnQty     = lnFrstNum

  ENDCASE
  *C200116,1 RAMY [end]
  *-- Zap the main temporary file if the option grid changes
  SELECT (lcLines)
  ZAP
  DIMENSION laSizes[12]
  **********************************************************
  *-- Start the data collection
  **********************************************************
  SELECT STYDYE
  SET RELATION TO STYLE INTO STYLE
  SCAN FOR &lcRpExp
    WAIT WINDOW 'Collecting data for style '+LEFT(Style,6) NOWAIT
    *-- Get the current scale if not exist in the scales file
    IF !SEEK(LEFT(STYLE.SCALE,1) ,lcScale)
      laSizes = SPACE(0)
      laSizes[1] = LEFT(STYLE.SCALE,1)
      lnSzCnt = 2
      SELECT SCALE
      =SEEK('S'+LEFT(STYLE.SCALE,1))
      *-- Scale is 11 sizes in 11 record in the master scale file, get them in one record in the temporary scale file
      SCAN WHILE TYPE+LEFT(SCALE,1) = 'S'+LEFT(STYLE.SCALE,1) .AND. lnSzCnt <=12
        FOR lnC = 1 TO SCALE.CNT
          lcC = ALLTRIM(STR(lnC))
          lnPos = ASCAN(laSizes,SCALE.SZ&lcC)
          IF lnPos = 0
            laSizes[lnSzCnt] = SCALE.SZ&lcC
            lnSzCnt = lnSzCnt + 1
          ENDIF
        ENDFOR
      ENDSCAN
      INSERT INTO (lcScale) FROM ARRAY laSizes
    ELSE
      SELECT (lcScale)
      SCATTER FIELDS SCALE,SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SZ9,SZ10,SZ11 TO laSizes
    ENDIF
    *-- Add one record per main scale and get its other scales in the same record
    IF !EOF('STYLE') .AND. !SEEK( LEFT(STYLE.Style,lnItmLen-3) , lcLines )
      INSERT INTO (lcLines) (Style , Desc,Scale) VALUES ( LEFT(STYLE.STYLE,lnItmLen-3) , STYLE.DESC,LEFT(STYLE.SCALE,1)) 
    ENDIF
    =SEEK('S'+STYLE.SCALE,'SCALE')
    FOR lnCnt = 1 TO SCALE.Cnt
      lcCnt = STR(lnCnt,1)
      lnPos = ASCAN(laSizes,SCALE.Sz&lcCnt)
      IF lnPos # 0
        lnPos = lnPos - 1
        lcPos = ALLTRIM(STR(lnPos))
        SELECT (lcLines)
        REPLACE Qty&lcPos    WITH Qty&lcPos + STYDYE.Stk&lcCnt ,;
                nTotQty      WITH nTotQty    + STYDYE.Stk&lcCnt
      ENDIF
    ENDFOR
  ENDSCAN
  SELECT (lcLines)
  *-- Set the show zero quantity and minimum quantity filters1
  IF !llRpDZero
    *C200116,1 RAMY [start]
    *SET FILTER TO !(QTY1=0 AND QTY2=0 AND QTY3=0 AND QTY4=0 AND QTY5=0 AND ;
                    QTY6=0 AND QTY7=0 AND QTY8=0 AND QTY9=0 AND QTY10=0 AND QTY11=0 ) ;
                    .AND. IIF(nTotQty#0,nTotQty >= lnQty,.T.)

    DO CASE 
      CASE lcSign = "BETWEEN"
        SET FILTER TO !(QTY1=0 AND QTY2=0 AND QTY3=0 AND QTY4=0 AND QTY5=0 AND ;
                        QTY6=0 AND QTY7=0 AND QTY8=0 AND QTY9=0 AND QTY10=0 AND QTY11=0 ) ;
                        .AND. BETWEEN(nTotQty , lnFrstNum , lnScndNum )


       CASE lcSign = "IN LIST"
        SET FILTER TO !(QTY1=0 AND QTY2=0 AND QTY3=0 AND QTY4=0 AND QTY5=0 AND ;
                        QTY6=0 AND QTY7=0 AND QTY8=0 AND QTY9=0 AND QTY10=0 AND QTY11=0 ) ;
                        .AND. (PADL(nTotQty,8) $ laOgVrFlt[lnVarPos,6])

       CASE lcSign = '=' .AND. lnQty = 0
         SET FILTER TO !(QTY1=0 AND QTY2=0 AND QTY3=0 AND QTY4=0 AND QTY5=0 AND ;
                         QTY6=0 AND QTY7=0 AND QTY8=0 AND QTY9=0 AND QTY10=0 AND QTY11=0 ) ;
                         .AND. IIF(nTotQty#0,nTotQty >= lnQty ,.T.)
         GO TOP
       
       OTHERWISE
         SET FILTER TO !(QTY1=0 AND QTY2=0 AND QTY3=0 AND QTY4=0 AND QTY5=0 AND ;
                         QTY6=0 AND QTY7=0 AND QTY8=0 AND QTY9=0 AND QTY10=0 AND QTY11=0 ) ;
                         .AND. IIF(nTotQty#0,nTotQty &lcSign lnQty ,.T.)
     
     ENDCASE
    *C200116,1 RAMY [end]
  ELSE
    *C200116,1 RAMY [start]
    *SET FILTER TO !BETWEEN(nTotQty,1, lnQtym-1 ) .AND. !(nTotQty <= -1 )
    
    DO CASE

      CASE lcSign = "BETWEEN"
        SET FILTER TO BETWEEN(nTotQty,lnFrstNum, lnScndNum )

      CASE lcSign = "IN LIST"
        SET FILTER TO (PADL(nTotQty,8) $ laOgVrFlt[lnVarPos,6])

      CASE lcSign = '=' .AND. lnQty = 0
        SET FILTER TO

      OTHERWISE
        SET FILTER TO nTotQty &lcSign lnQty

      ENDCASE
    *C200116,1 RAMY [end]
  ENDIF
*-Ramy
*ENDIF
*-Ramy
SELECT (lcLines)
GO TOP
IF EOF()
  *-- No records to display
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcScl = SCALE
SET RELATION TO LEFT(SCALE,1) INTO (LCSCALE)
*-- Display the report
=gfDispRe(lcRpForm)

SELECT (lcLines)
SET RELATION TO
SET FILTER TO


*!*************************************************************
*! Name      : lfWRunGrid
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 01/06/2000
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfWRunGrid
*-- Creat the main temp file
CREATE CURSOR (lclines)  (Style C(19)    ,Scale C(1)  ,;
                         SZ1 C(5)     , SZ2 C(5)      , SZ3 C(5)     , SZ4 C(5)      ,;
                         SZ5 C(5)     , SZ6 C(5)      , SZ7 C(5)      ,;
                         SZ8 C(5)     , SZ9 C(5)      , SZ10 C(5)     , SZ11 C(5)     ,;
                         Qty1 N(6)    , Qty2 N(6)     , Qty3 N(6)     , Qty4 N(6)     ,;
                         Qty5 N(6)    , Qty6 N(6)     , Qty7 N(6)     , Qty8 N(6)     ,;
                         Qty9 N(6)    , Qty10 N(6)    , Qty11 N(6)    , nTotqty N(7)   ,;
                         Desc C(20)    )

INDEX ON Style TAG (lcLines) of (gcWorkDir+lclines)
*-- Create temp file holds scales with 11 size in one record
CREATE CURSOR (lcScale) (scale C(1) ,SZ1 C(5), SZ2 C(5), SZ3 C(5), SZ4 C(5), SZ5 C(5),;
                           SZ6 C(5), SZ7 C(5), SZ8 C(5), SZ9 C(5), SZ10 C(5), SZ11 C(5) )
INDEX ON SCALE TAG(lcScale) OF (gcWorkDir+lcScale)

*-- Get user Name
=SEEK(gcUser_Id , 'SYUUSER')
lcUsrDesc = ALLTRIM(SYUUSER.CUSR_NAME)

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 01/06/2000
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
  GO TOP IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 01/06/2000
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : ICFG100.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
FUNCTION lfAdjSeg

STORE 0 TO lnFPos, lnDPos, lnZPos, lnGPos, lnCPos, lnOPos, lnTPos, ;
           lnQPos, lnSPos
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
  *-- If the style major consists of one segment, don't display it, 
  *-- display the style major instead (style major will browse from the 
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1 
    LOOP
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'F'
      *-- If there are more than one "FREE" segment , get first one only
      lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style'
      ENDIF
    CASE laMajSeg[lnC,1] = 'D'
      lnDPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Division'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Z'
      lnZPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Season'
      ENDIF
    CASE laMajSeg[lnC,1] = 'G'
      lnGPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style Group'
      ENDIF
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Color'
      ENDIF
    CASE laMajSeg[lnC,1] = 'O'
      *-- If there are more than one "OTHER" segment , get first one only
      lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Other'
      ENDIF
    CASE laMajSeg[lnC,1] = 'T'
      lnTPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Make'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Q'
      lnQPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Quality'
      ENDIF
    CASE laMajSeg[lnC,1] = 'S'
      lnSPos = lnC
       IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Scale'
      ENDIF
  ENDCASE

ENDFOR



*!*************************************************************
*! Name      : lfEndGrp
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 01/06/2000
*! Purpose   : To control printing size scale titles
*!*************************************************************
*! Called from : ICFG100.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEndGrp()
*!*************************************************************
FUNCTION lfEndGrp
PARAMETERS llInitscl
lcScl = IIF(llInitscl,SPACE(1),&lcLines..SCALE)
RETURN ''

*!*************************************************************
*! Name      : lpWhsDesc
*! Developer : Wael Mohammed Anwar Hamed
*! Date      : 04/24/2000
*! Purpose   : to print location(s)
*!*************************************************************
*! Called from : ICFG100.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpWhsDesc
*!*************************************************************

*B603536,1 WMA Procedure to print location(s) [Begin]

PROCEDURE lpWhsDesc
PRIVATE lnWarePo,lcWHs

lnWarePo  = ASUBSCRIPT(laOGFxFlt,;
            ASCAN(laOGFxFlt,'STYDYE.CWARECODE'),1)
lcWHs     = laOGFxFlt[lnWarePo,6]

PRIVATE lnCurAilas

lnCurAilas = SELECT(0)
IF USED(lcWHs)
  SELECT (lcWHs)
  SCAN
    IF SEEK(&lcWHs..CWARECODE,'warehous')
      IF OCCURS(' - ',lcWhFL) < 2
        lcWhFL = lcWhFL + IIF(EMPTY(lcWhFL),ALLTRIM(WareHous.cDesc),' - ' + ALLTRIM(WareHous.cDesc))
      ELSE
        lcWhSL = lcWhSL + IIF(EMPTY(lcWhSL),ALLTRIM(WareHous.cDesc),' - ' + ALLTRIM(WareHous.cDesc))
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

IF EMPTY(lcWhFl)
  lcWhFl = "ALL LOCATIONS"
ENDIF

SELECT (lnCurAilas)

*B603536,1 WMA [End]