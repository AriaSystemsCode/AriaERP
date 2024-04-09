*:***************************************************************************
*: Program file  : SOREN300.PRG
*: Program desc. : Best sellers report.
*: System        : Aria Apparel System (A27).
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102755,1
*: Date          : 11/24/2002
*:***************************************************************************
*: Calls :  
*:         Procedures : lpPrint
*:               -----------------------------------------------
*:          Functions : lfNonMaj()  , gfModalGen() , gfItemMask()
*:                    : gfStyBrw()  , FaBrow()     , lfvStyle()
*:                    : lfvFabric() , lfSRVSty()   , lfStySum()
*:                    : lfwRepWhen(), lfItmPos()   , lfvDateRng
*:                    : lfCollect() , lfCreatemp
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*: Notes   : .... 
*:***************************************************************************
*: Example : DO SOREN300
*:***************************************************************************
*: This Program Is Due To Custom Program For REN (102755) 
*:***************************************************************************
*:C102755.
*:***************************************************************************
*:Modifications :
*: B606904,1 BWA 01/28/2003 Fix the bug of sorting the records with style.
*:***************************************************************************
*:
*--laSize     && Holds the dimension of Size order qty.
*--ldRp1Date  && Holds the the first activity date range.
*--ldRp2Date  && Holds the the second activity date range.
*--Initilize variables needed for STY910 program.                  

STORE SPACE(1) TO LStyle,HStyle,XFabric,XSeason,XDivision,XGroup,XPattern,;
                  XStatus,TSea,TDiv,TClr,xRpt_Cd
STORE '' TO XTitle
DIMENSION laSize(10)
STORE 0 TO laSize
R_WIDTH  = 'W'
PAGENO   = 0


*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh

  SELECT INVLINE
  SET RELATION TO invoice INTO INVHDR
  
  SELECT ORDLINE
  SET RELATION TO cordtype+order INTO ORDHDR
 
 
  *-- Create Temp Files.
  = lfCreatemp ()
  WAIT WINDOW 'Collecting data. Please wait....' NOWAIT  
 
  *-- Collecting Code...
  = lfCollect ()
  
  SELECT ORDLINE
  SET RELATION TO
  
  SELECT INVLINE
  SET RELATION TO

  SELECT (lcBstSlTmp)

  *B606904,1 BWA 01/28/2003 Locate the file to sum all the styles not the last one.[START]
  LOCATE
  *B606904,1 BWA 01/28/2003.[END]

  *--Sum the record on a style level to prepare for the index.
  DO WHILE .T.
    IF EOF()
      EXIT
    ENDIF  
    lcStyKey = LEFT(STYLE,lnstylewid)
    SUM nNetBkd TO lnBkd WHILE STYLE+STR(nNetBkd,10) = lcStyKey
    =SEEK(lcStyKey)
    REPLACE REST nBktIndx WITH lnBkd WHILE STYLE+STR(nNetBkd,10) = lcStyKey
  ENDDO

  SET ORDER TO TAG (lcBstSlTmp)
  LOCATE
  SELECT * ,SUBSTR(Style,lnstylewid+2,6) AS Color ,;
         LEFT(STYLE,lnstylewid) AS cStyle  ;
  FROM (lcBstSlTmp) INTO DBF (gcWorkDir+lcRptTemp);
  ORDER BY nBktIndx DESC,cSTYLE ASC,nNetBkd DESC,COLOR ASC

*  SELECT * ,SUBSTR(Style,lnstylewid+2,6) AS Color ,;
         LEFT(STYLE,lnstylewid) AS cStyle  ;
  FROM (lcBstSlTmp) INTO DBF (gcWorkDir+lcRptTemp);
  ORDER BY nBktIndx DESC

ENDIF
*-- End if for user change filter .

SELECT (lcRptTemp)
LOCATE
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF


DO lpPrint

DO ENDREPORT                 && print the report by ENDREPORT PRG
SET DEVICE TO SCREEN         && return the device again to screen
RETURN                        
*-- End of Report.
*:*************************************************************
*: Name      : lpPrint
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Print the data after being collected.
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : Do  lpPrint
*:*************************************************************
*
PROCEDURE lpPrint

*-----------------------------------------------------
* [REPORT] BEGIN REPORT LOOP
*-----------------------------------------------------
R_TITLE = " BEST SELLERS REPORT "
SET DEVICE TO SCREEN
WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
SET DEVICE TO PRINT
ROW = 99
*--Initilize total and subtotal variables.
DIMENSION laSubSize(10),laTotSize(10)
STORE 0 TO laSubSize,laTotSize
lcStyle = LEFT(STYLE,lnstylewid)

DO WHILE INKEY() <>32
  IF EOF()
    EXIT
  ENDIF
  IF ROW >=63
     PAGENO = PAGENO +1
     DO RPT_HDR WITH 'REN300',XTITLE,R_WIDTH
     @ 05,00 SAY 'STYLE        COLOR  DESCRIPTION            PRICE    NET BKD.    EXTENSION      04     06     08     10     12     14     16     18'
     @ 06,00 SAY '------------ ------ -------------------- -------  ----------  -----------  ------ ------ ------ ------ ------ ------ ------ ------'
     ROW = 07
  ENDIF
  *-- Say lines.
  @ ROW,00  SAY LEFT(STYLE,lnstylewid)
  @ ROW,13  SAY SUBSTR(STYLE,lnClrPo,lnColorLen)
  @ ROW,20  SAY DESC
  @ ROW,41  SAY PRICE
  @ ROW,50  SAY nNetBkd PICTURE '9999999999'
  @ ROW,61  SAY nExten  PICTURE '999999999.99'
  @ ROW,75  SAY nSz1    PICTURE '999999'
  @ ROW,82  SAY nSz2    PICTURE '999999'
  @ ROW,89  SAY nSz3    PICTURE '999999'
  @ ROW,96  SAY nSz4    PICTURE '999999'
  @ ROW,103 SAY nSz5    PICTURE '999999'
  @ ROW,110 SAY nSz6    PICTURE '999999'
  @ ROW,117 SAY nSz7    PICTURE '999999'
  @ ROW,124 SAY nSz8    PICTURE '999999'
  ROW = ROW+1
  *--Calculate the sub and brand total the current style.
  laSubSize(1) = laSubSize(1)+nNetBkd
  laSubSize(2) = laSubSize(2)+nExten
  laTotSize(1) = laTotSize(1)+nNetBkd
  laTotSize(2) = laTotSize(2)+nExten
  FOR lnSub = 3 TO 10
    lcSub = STR(lnSub-2,1)
    laSubSize(lnSub) = laSubSize(lnSub)+nSz&lcSub
    laTotSize(lnSub) = laTotSize(lnSub)+nSz&lcSub
  ENDFOR
  SKIP
  *--Say the SubTotal if the style has changed.
  IF (LEFT(STYLE,lnstylewid) <> lcStyle)
    *--See whether the remain lines satisfy 3 lines needed for saying the sub total.
    IF ROW >=60
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'REN300',XTITLE,R_WIDTH
      IF !EOF()
        *-- Other lines remain.
        @ 05,00 SAY 'STYLE        COLOR  DESCRIPTION            PRICE    NET BKD.    EXTENSION      04     06     08     10     12     14     16     18'
        @ 06,00 SAY '------------ ------ -------------------- -------  ----------  -----------  ------ ------ ------ ------ ------ ------ ------ ------'
      ENDIF
      ROW = IIF(EOF(),05,07)
    ENDIF
    @ROW,00 SAY REPLICATE('-',132)
    ROW = ROW+1
    @ROW,22  SAY 'SUBTOTAL FOR  ' + lcStyle
    @ROW,50  SAY laSubSize(1)  PICTURE '9999999999'
    @ROW,61  SAY laSubSize(2)  PICTURE '999999999.99'
    @ROW,75  SAY laSubSize(3)  PICTURE '999999'
    @ROW,82  SAY laSubSize(4)  PICTURE '999999'
    @ROW,89  SAY laSubSize(5)  PICTURE '999999'
    @ROW,96  SAY laSubSize(6)  PICTURE '999999'
    @ROW,103 SAY laSubSize(7)  PICTURE '999999'
    @ROW,110 SAY laSubSize(8)  PICTURE '999999'
    @ROW,117 SAY laSubSize(9)  PICTURE '999999'
    @ROW,124 SAY laSubSize(10) PICTURE '999999'
    ROW = ROW+1
    @ROW,00 SAY REPLICATE('-',132)
    ROW = ROW+1
    lcStyle = LEFT(STYLE,lnstylewid)
    STORE 0 TO laSubSize
  ENDIF
ENDDO
*--SAY THE GRAND TOTRAL. 
*--See whether the remain lines satify 5 lines needed for saying the grand total.
IF ROW >=58
  PAGENO = PAGENO +1
  DO RPT_HDR WITH 'REN300',XTITLE,R_WIDTH
  ROW = 5
ENDIF
ROW = ROW + 2
*--Say the Grand Total
@ROW,00 SAY REPLICATE('=',132)
ROW = ROW+1
@ROW,22  SAY 'GRAND TOTAL '
@ROW,50  SAY laTotSize(1)  PICTURE '9999999999'
@ROW,62  SAY laTotSize(2)  PICTURE '999999999.99'
@ROW,75  SAY laTotSize(3)  PICTURE '999999'
@ROW,82  SAY laTotSize(4)  PICTURE '999999'
@ROW,89  SAY laTotSize(5)  PICTURE '999999'
@ROW,96  SAY laTotSize(6)  PICTURE '999999'
@ROW,103 SAY laTotSize(7)  PICTURE '999999'
@ROW,110 SAY laTotSize(8)  PICTURE '999999'
@ROW,117 SAY laTotSize(9)  PICTURE '999999'
@ROW,124 SAY laTotSize(10) PICTURE '999999'
ROW = ROW+1
@ROW,00 SAY REPLICATE('=',132)


*-- End of lpPrint.
*:*************************************************************
*: Name      : lfNonMaj
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Print the data after being collected.
*:*************************************************************
*: Called from : Option Grid.
*:*************************************************************
*: Calls       : =gfItemMask().
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfNonMaj ()
*:*************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''

*-- End of lfNonMaj.
*:*************************************************************
*: Name      : lfvStyle
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : validate style
*:*************************************************************
*: Called from : Option Grid.
*:*************************************************************
*: Calls       : gfStyBrw().
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvStyle ()
*:*************************************************************
*
FUNCTION lfvStyle
lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE

*--End of lfvStyle.
*:**************************************************************************
*: Name      : lfvFabric
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : validate fabric
*:**************************************************************************
*: Called from : Option Grid
*:**************************************************************************
*: Calls       : FaBrow()
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Return      : None
*:**************************************************************************
*: Example     : = lfvFabric()
*:**************************************************************************
* 
FUNCTION lfvFabric

**ASH on 12/23/2002 Begin
IF !FILE(gcDataDir+'FABRIC')
  RETURN
ENDIF
**ASH END


lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF !EMPTY(lcFab) 
  IF SEEK(lcFab,'FABRIC')
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF  
ENDIF

SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF
  
*-- End of lfvFabric.
*:**************************************************************************
*: Name      : lfsrvSty
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Rise change style flag, in range browse screen.
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Called from : Option Grid
*:**************************************************************************
*: Passed Parameters  : lcParm
*:**************************************************************************
*: Returns            : None
*:**************************************************************************
*: Example   : =lfsrvSty()
*:**************************************************************************
*: Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X    
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    SELECT STYLE
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE

*-- End of lfsrvSty.
*:**************************************************************************
*: Name      : lfStySum
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : sum a specific field for the current style in style file
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Called from : Option Grid,style browse calculated fields.
*:**************************************************************************
*: Passed Parameters  : lcSty,lccomp,lnAddToVar
*:**************************************************************************
*: Returns            : Calculated field value.
*:**************************************************************************
*: Example   : =lfStySum()
*:**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  

  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.


*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Option Grid When function
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*
FUNCTION lfwRepWhen
STORE DATE() TO  ldRp1Date , ldRp2Date
*-- End of lfwRepWhen.
*:**************************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Evaluate fixed filter position within array.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : Position
*:**************************************************************************
*: Example   : = lfItmPos()
*:**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
*:****************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Create the temp files.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCreatemp()
*:****************************************************************
*
FUNCTION lfCreatemp
PRIVATE lnAlias

lnAlias = SELECT(0)
CREATE TABLE (gcWorkDir+lcBstSlTmp) ;
             (Style C(19)  ,Desc C(20)     ,Price N(7,2)  ,;
              nNetBkd N(10),nExten N(12,2) ,nSz1 N(6)     ,;
              nSz2 N(6)    ,nSz3 N(6)      ,nSz4 N(6)     ,;
              nSz5 N(6)    ,nSz6 N(6)      ,nSz7 N(6)     ,;
              nSz8 N(6)    ,nBktIndx N(10) ,nSndIndx N(6))

*--This index (lcBstSlTmp) let the styles with highest booked qtys be on the top and those
*--with lowest booked qtys be on the bottom (i.e. DESCENDING ) and the style\color
*--with highest booked qty will come first before the one with lowest booked qty
*--within a certain style.
INDEX ON STR(nBktIndx,10)+STYLE+STR(nNetBkd,10) DESCENDING TAG (lcBstSlTmp)
INDEX ON STYLE+STR(nNetBkd,10)  ASCENDING TAG 'BKDINDEX' ADDITIVE

SELECT(lnAlias)

*-- End Of lfCreatemp.
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : function Collect data into temp file.
*:*************************************************************
*: Called from : The Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect()
*:*************************************************************
*:
FUNCTION lfCollect
Private lnAlias , lnStylePos , lcTmpSty , llWorkFile
llWorkFile = .F.
lnStyPos   = 0
lnAlias    = SELECT(0)


lnStyPos    = lfItmPos('STYLE.CSTYMAJOR')
lcTmpSty    = laOGFxFlt[lnStyPos,6]
llWorkFile  = USED(lcTmpSty) AND RECCOUNT(lcTmpSty) > 0

IF llWorkFile
  SELECT (lcTmpSty)
  SET RELATION TO LEFT(CSTYMAJOR,lnstylewid) INTO STYLE
  SET SKIP TO STYLE
  LOCATE
ELSE
  SELECT STYLE
ENDIF



*--Collect data
SCAN For &lcRpExp
  SELECT Style
  lcKey = Style
  SCATTER MEMVAR
  INSERT INTO (lcBstSlTmp) (Style,Desc,Price) values(m.style,m.desc,m.priceA)
  IF SEEK(lcKey,'ORDLINE')
    SELECT ORDLINE
    SUM REST  QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
        WHILE STYLE+DTOS(COMPLETE)+ORDER+STORE+STR(LINENO,6) =lcKey;
        FOR   ORDHDR.STATUS $ 'OH' AND BETWEEN(ORDLINE.COMPLETE,ldRp1Date,ldRp2Date);
        TO  ARRAY laSize
    REPLACE &lcBstSlTmp..nSz1    WITH laSize(1),;
            &lcBstSlTmp..nSz2    WITH laSize(2),;
            &lcBstSlTmp..nSz3    WITH laSize(3),;
            &lcBstSlTmp..nSz4    WITH laSize(4),;
            &lcBstSlTmp..nSz5    WITH laSize(5),;
            &lcBstSlTmp..nSz6    WITH laSize(6),;
            &lcBstSlTmp..nSz7    WITH laSize(7),;
            &lcBstSlTmp..nSz8    WITH laSize(8),;
            &lcBstSlTmp..nNetBkd WITH laSize(9)
  ENDIF
  IF SEEK(lcKey,'INVLINE')
    SELECT INVLINE
    SUM REST  QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY ;
        WHILE STYLE+INVOICE+STR(LINENO,6) =lcKey;
        FOR   BETWEEN(INVDATE,ldRp1Date,ldRp2Date);
        .AND. !(InvHdr.Status = 'V');
        TO    ARRAY laSize
    REPLACE &lcBstSlTmp..nSz1    WITH &lcBstSlTmp..nSz1+laSize(1),;
            &lcBstSlTmp..nSz2    WITH &lcBstSlTmp..nSz2+laSize(2),;
            &lcBstSlTmp..nSz3    WITH &lcBstSlTmp..nSz3+laSize(3),;
            &lcBstSlTmp..nSz4    WITH &lcBstSlTmp..nSz4+laSize(4),;
            &lcBstSlTmp..nSz5    WITH &lcBstSlTmp..nSz5+laSize(5),;
            &lcBstSlTmp..nSz6    WITH &lcBstSlTmp..nSz6+laSize(6),;
            &lcBstSlTmp..nSz7    WITH &lcBstSlTmp..nSz7+laSize(7),;
            &lcBstSlTmp..nSz8    WITH &lcBstSlTmp..nSz8+laSize(8),;
            &lcBstSlTmp..nNetBkd WITH &lcBstSlTmp..nNetBkd+laSize(9)
  ENDIF
  REPLACE &lcBstSlTmp..nExten WITH &lcBstSlTmp..nNetBkd*&lcBstSlTmp..Price

  IF llWorkFile
    SELECT (lcTmpSty)
  ENDIF
ENDSCAN

IF llWorkFile
  SELECT (lcTmpSty)
  SET RELATION TO
ENDIF

SELECT(lnAlias)
*-- End OF lfCollect.
*:*************************************************************
*: Name      : lfvDateRng
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Showes date range screen.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvDateRng()
*:*************************************************************
FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo
ldFrom = ldRp1Date
ldTo   = ldRp2Date
lcTitle = 'Activity Date Range'
DO DateRng.Spx

ldRp1Date  = ldFrom
ldRp2Date  = ldTo

*-- End of lfvDateRng.
*:*************************************************************
*: Name      : lfvPbOk
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/24/2002
*: Purpose   : Validate date range screen's OK button.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvPbOk()
*:*************************************************************
*:
FUNCTION lfvPbOk

IF ldFrom > ldTo
  WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldRp1Date  = ldFrom
  ldRp2Date  = ldTo
  CLEAR READ
ENDIF

*-- End of lfvPbOk.
*:*************************************************************


