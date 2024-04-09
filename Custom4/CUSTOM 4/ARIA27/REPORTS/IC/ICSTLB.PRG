*:***************************************************************************
*: Program file  : ICSTLB
*: Program desc. : Print Style Labels
*! Date          : 03/02/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh (SSE)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gflfAdjSeg(),gfItemMask(),lfvFabric(),lfvStyle()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : .... Changing the STYLE.DESC from 18 char. to 11 char. in LABEL
*:***************************************************************************
*: Example : DO ICSTLB
*:***************************************************************************
*: This Program is due to E300880 ...
*:***************************************************************************
*: B605419,1 BWA 01/28/2002 Fix the bug of the broken fields in case there is no records to display.
*:***************************************************************************
lcMakeOp  = IIF(lcRpDomImp='D','','!')
lcRepExpr = IIF(lcRpDomImp='B','', lcMakeOp+[Style.Make])

IF !EMPTY(lcRepExpr)
  lcRpExp   = IIF(lcRpExp = '.T.',lcRepExpr,lcRpExp + [ AND ] + lcRepExpr)
ENDIF  

lcRpExp   = STRTRAN(lcRpExp,'STYLE.','')
lcRpExp   = IIF(EMPTY(lcRpExp),'.T.',lcRpExp)

IF 'MA' $ gcCmpModules
  SELECT FABRIC
  SET ORDER TO TAG FABRIC
  SELECT STYLE
  SET RELATION TO FABRIC INTO FABRIC
ENDIF

SELECT STYLE
LOCATE FOR &lcRpExp
IF !FOUND()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')

  *B605419,1 BWA 01/28/2002 The page broken after this message because the device didn't return to its original status which is screen.[START]
  SET DEVICE TO SCREEN
  *B605419,1 BWA 01/28/2002.[END]

  RETURN
ENDIF

GO TOP
R_WIDTH='N'
ROW  = 0
COL  = 9 && Use When There is More Than Multiple Color For Same Style.
CNT  = 1 && Use To Determine How Many (Max 16) Color/Style To Be Displayed.
LINE = 1

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
*STY:XXXXXXXXXXXX XXXXXXXXXXXXXXXXXX
*SIZ:XXX XXX XXX XXX XXX XXX XXX XXX
*CLR:XXXXXX XXXXXX XXXXXX XXXXXX
*FAB:XXXXXXXXXXXXXXXXXXXX

CLEAR TYPEAHEAD
SET DEVICE TO PRINT
DO lpPrnLbl
DO ENDREPORT
SET DEVICE TO SCREEN

*-- end of program code.


*!*************************************************************
*! Name      : lfvStyle
*! Developer : Sameh (SSE)
*! Date      : 02/25/99
*! Purpose   : Validate Style 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : 
*!*************************************************************

FUNCTION lfvStyle
PRIVATE lcStyFld,lcStyle,lnSelcFile,lcCusTag

lcStyFld   = VARREAD()
lcStyle    = EVAL(lcStyFld)

lnSelcFile = SELECT(0)
SELECT Style
lcCusTag  = ORDER('style')
SET ORDER TO TAG Cstyle IN Style
IF !EMPTY(lcStyle)
  IF ('?' $ lcStyle .OR. !SEEK(lcStyle,'Style'))
    lcStyle = gfStyBrw('M',"","",.F.)
  ELSE 
    &lcStyFld = lcStyle    
  ENDIF	
ENDIF
&lcStyFld = lcStyle
SET ORDER TO lcCusTag
SELECT (lnSelcFile)

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Sameh (SSE) 
*! Date      : 02/28/99
*! Purpose   : Validate Fabric 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvFabric()
*!*************************************************************

FUNCTION lfvFabric
PRIVATE lcFabFld,lcFabric,lnSelcFile,lcSelcTag,laTemp,lcBrFields

lcFabFld  = VARREAD()
lcFabric  = EVAL(lcFabFld)

lnSelcFile = SELECT(0)

SELECT Fabric
lcSelcTag  = ORDER('Fabric')

SET ORDER TO TAG cFabric

IF !EMPTY(lcFabric).AND.('?' $ lcFabric .OR. !SEEK(lcFabric , 'Fabric')) 

  DIMENSION laTemp[1]
  laTemp = ''     

  lcBrFields = "Fabric :H = 'Item' , Desc :H = 'Description' , ;
                Item_type :H = 'Type' , Loc :H = 'Location' , Vendor :H = 'Vendor' ,;
                Pattern :H = 'Pattern' , Onhand :H = 'On hand' , Onorder :H = 'On order' "
               
   = gfBrows('','Fabric','laTemp')
     
  IF !EMPTY(laTemp[1])
    lcFabric = laTemp[1]
  ELSE 
    lcFabric = ''
  ENDIF
ENDIF

&lcFabFld = lcFabric
SET ORDER TO lcSelcTag
SELECT (lnSelcFile)
*-- end 

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : Sameh (SSE)
*! Date      : 02/28/99
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : option grid of POPRLB.PRG
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

STORE 0 TO lnFPos , lnDPos , lnZPos   , lnGPos , lnCPos , lnOPos , lnTPos , ;
           lnQPos , lnSPos , lnMajPos
STORE 0 TO lnMajLen
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
*--Get Major Length

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
      lnCPos   = lnC
      lnClrPos = laMajSeg[lnC,4]
      lnClrPic = LEN(laMajSeg[lnC,3])
      *IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Only These Colors'
      *ENDIF
      
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

*-- Compute Variables titles. [begin]
lcStyMake = lcStyMajor + ' (Make/Buy)'
lcStyGrp  = lcStyMajor + ' Group'
*-- Compute Variables titles. [end]

*!*************************************************************
*! Name      : lpPrnLbl
*! Developer : Sameh (SSE)
*! Date      : 02/28/99
*! Purpose   : Print label loop
*!*************************************************************
*! Called from : Main program
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrnLbl
*!*************************************************************
*
PROCEDURE lpPrnLbl
lnMajorLen = LEN(SUBSTR(lcStylePic,4))

XPRESTYLE = SPACE(lnMajorLen)
SELECT STYLE

SET FILTER TO &lcRpExp
GO TOP

DO WHILE !EOF()
  
  IF XPRESTYLE <> PADR(CSTYMAJOR,lnMajorLen)     &&Previous Style Different  	
    @ ROW,01 SAY 'STY:' + PADR(CSTYMAJOR,lnMajorLen) + ' ' + SUBSTR(DESC,1,11)   && YMA 06/15/94
    ROW  = ROW + 1
    LINE = LINE + 1
    XSCALE = GETSCALE (SCALE,' ')    

    IF LEN(XSCALE)>30
      =SEEK ('S'+SCALE,'SCALE')
      xScale=''
      xScale=PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),3,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),3,' ')+' '+;
             PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),3,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),3,' ')+' '+;
             PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),3,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),3,' ')+' '+;
             PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,4),3,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,4),3,' ')   
    ENDIF

    @ ROW,01 SAY 'SIZ:' + XSCALE
    ROW = ROW + 1
    LINE = LINE + 1
    
    @ ROW,01 SAY 'CLR:' + SUBSTR(Style,lnClrPos,lnClrPic)
    COL = 12                                             && YMA 06/15/94
    CNT = 01

  ELSE

    IF COL=5 .AND. CNT<8   && Another Row of Colors To Be Displayed  && YMA 06/15/94
      ROW = ROW + 1
      LINE = LINE + 1
    ENDIF
    CNT = CNT + 1
    IF CNT < 09              && Number of Color/Style Less Than 17.   && YMA 06/15/94          
      @ ROW,COL SAY SUBSTR(Style,lnClrPos,lnClrPic)
      COL = COL + 7          && YMA 06/15/94
      IF COL > 31            && Number of Color/Style Greater Than 8. && YMA 06/15/94
        COL = 5
      ENDIF
    ENDIF
  ENDIF

  XPRESTYLE = PADR(CSTYMAJOR,lnMajorLen)

  IF COL = 5 .AND. CNT > 08    && More Than 16 Colors Per Style
    XPRESTYLE = SPACE(lnMajorLen)
    CNT = 1
    SKIP -1
  ENDIF
  SKIP

  IF XPRESTYLE <> PADR(CSTYMAJOR,lnMajorLen)  && Next Style Different, Display Last Field of Previous Field
    SKIP -1
    ROW = ROW + 1
    LINE = LINE + 1
    *-- Display Fabric Description Only If One Is Available.
    IF ('MA' $ gcCmpModules) AND !EMPTY(FABRIC.DESC)
      @ ROW,01 SAY 'FAB:'+FABRIC.DESC
      ROW = ROW + 1
      LINE = LINE + 1
    ENDIF

    *** Determine The Next Line To Print The New Label
    IF LINE < 7
      ROW = ROW + (7-LINE)
    ENDIF
    LINE = 1

    SKIP
  ENDIF
ENDDO
*-- end of lpPrnLbl.
*:****************************************************************
*: Name        : lfsrvSty
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 05/06/2001
*: Purpose     : Rise change style flag, in range browse screen.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : None
*:****************************************************************
*: Example     : =lfsrvSty()
*:****************************************************************
*: Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:****************************************************************
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
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE

*-- End of lfsrvSty.
*:****************************************************************
*: Name        : lfStySum
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 05/06/2001
*: Purpose     : sum a specific field for the current style in style file
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid,style browse calculated fields.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : Calculated field value.
*:****************************************************************
*: Example     : =lfStySum()
*:****************************************************************
*
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)

*-- End of lfStySum.
*:****************************************************************
