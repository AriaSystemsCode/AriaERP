*!********************************************************************
*: Program file  : ICAPP200  (REFERENCE, C102210)
*: Program desc. : CUSTOMIZED CUT & SOLD REPORT FOR APPAREL RESOURCE
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 2.7
*:         Module: REPORT
*:      Developer: ABD ELRAIM KOURA . KAM
*:      Date     : 08/03/2001 
*!********************************************************************
*: Calls         :=lfPntBySz() 
*:               :=lfPntNoSz()    
*:               :=lfGetClrD() 
*:               :=lfEvalSegs()
*:               :=lfStySum()
*:               :=lfsrvSty() 
*:               :=lfvBySize() 
*:               :=lfvFabric() 
*:               :=lfwOldVal()
*!********************************************************************
*: Passed Parameters  :NONE
*!********************************************************************
*: Example            : ICAPP200
*!********************************************************************

STORE 0 TO lnClrLen,lnClrPos   &&hold color position and color length
*--call function to get color position and color length
=lfGetClrD()

lcTmpStyle = gfTempName()
*--construct filter criteria
lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = IIF(lcRpMake='B','', lcMakeOp +"Style.Make") 
lcRpExp = IIF( '.T.' $ lcRpExp , lcRepExpr, + IIF(EMPTY(lcRepExpr),lcRpExp,lcRpExp + "  AND  " +lcRepExpr))
lcStusRp   = IIF(lcRpStatus='B','',[  (Style.Status="] + lcRpStatus + [")])
lcRpExp = IIF(EMPTY(lcRpExp) , lcStusRp , IIF(EMPTY(lcStusRp),lcRpExp,lcRpExp + "  AND  " + lcStusRp))

*-- copy from style for that criteria also open temp file
WAIT 'SELECTING RECORDS FOR THE REPORT .....' WINDOW NOWAIT
SELECT STYLE
COPY STRUCTURE TO (gcWorkDir + lcTmpStyle)
SCAN FOR &lcRpExp

  WAIT WINDOW 'STYLE IS' + STYLE NOWAIT

  SCATTER MEMVAR MEMO
  INSERT INTO (gcWorkDir +lcTmpStyle) FROM MEMVAR
ENDSCAN
*--check if there are records or not
IF !USED(lcTmpStyle)
  SET DEVICE TO SCREEN
  =gfDialog('I','There are no records to display')
  RETURN
ENDIF  
*--check to call print with ssize or print without size
DO CASE
  CASE llRpBySize
    *--print by size
    =lfPntBySz()
  CASE !llRpBySize
    *-- print without size 
    =lfPntNoSz()   
ENDCASE
SET DEVICE TO SCREEN
IF USED(lcTmpStyle)
  SELECT(lcTmpStyle)
  USE IN (lcTmpStyle)
ENDIF  

ERASE (gcWorkDir+lcTmpStyle+".DBF")
RETURN
*!*************************************************************
*! Name      : lfPntBySz
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : Print with size.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPntBySz()
*!*************************************************************
FUNCTION lfPntBySz

*--layout for printing with size
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.
*STYLE: 123456789*12 123456789*123456789*          SEA/DIV: 12/12  PATTERN: 123456789*
*
*COLOR  COLOR DESC.....   SCALE: (1)               SZ1   SZ2   SZ3   SZ4   SZ5   SZ6   SZ7   SZ8     TOT  NEG-OTS POS-OTS NET-OTS
*123456 123456789*12345   WIP ...........        12345 12345 12345 12345 12345 12345 12345 12345 1234567
*                         HANGING STOCK..        12345 12345 12345 12345 12345 12345 12345 12345 1234567
*                         TOTAL AVAIL ...        12345 12345 12345 12345 12345 12345 12345 12345 1234567
*                         OPEN ORDERS ...        12345 12345 12345 12345 12345 12345 12345 12345 1234567
*                         OPEN TO SELL ..        12345 12345 12345 12345 12345 12345 12345 12345          1234567 1234567 1234567
*                         YTD SHIPPED ...        12345 12345 12345 12345 12345 12345 12345 12345 1234567
*                         BOOKED ........        12345 12345 12345 12345 12345 12345 12345 12345 1234567

*--initialse varaible for first time
R_WIDTH  = 'W'
R_TITLE  = 'CUT & SOLD'
PAGENO   = 0
MAXPP    = 5
SAVSTYLE = ' '
ROW      =99
llAnyLines=.F.  && flag if not printing any record
XTITLE   =lcRpTitle
SET DEVICE TO SCREEN
WAIT 'PRINTING ... <Space Bar> to Abort ' WINDOW NOWAIT
SET DEVICE TO PRINT
SELECT(lcTmpStyle)
GO TOP
DO WHILE !EOF() AND INKEY() <> 32
  IF ROW > 55
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'APP200',XTITLE,R_WIDTH
    ROW      = 5
    PERPAGE  = 1
    SAVSTYLE = ' '
  ENDIF

*-- FILTER OUT MINIMUM O-T-S REQUEST
  IF lnRpMinQty <> 0
     X=1
     STORE 0 TO POSOTS, NEGOTS
     DO WHILE X<=8
       Z=STR(X,1)
       WKTOT = IIF(lcRpPrtBas='W',WIP&Z,PLAN&Z) + STK&Z - ORD&Z      
       IF WKTOT >0
          POSOTS = POSOTS + WKTOT
       ELSE
         NEGOTS = NEGOTS + WKTOT
       ENDIF
       X = X+1
      ENDDO && X<=8

      DO CASE
        CASE lnRpMinQty >0 .AND. POSOTS<lnRpMinQty
          SKIP
          LOOP
        CASE lnRpMinQty <0 .AND. NEGOTS>lnRpMinQty
          SKIP
          LOOP
     ENDCASE
  ENDIF && lnRpMinQty <> 0
  IF (lcRpPrtBas='W' .AND. TOTSTK+TOTWIP+TOTORD=0) .OR. ;
    (lcRpPrtBas='P' .AND. TOTPLAN=0)
    SKIP
    LOOP
  ENDIF
  IF SUBSTR(STYLE,1,lnClrPos-1)<>SAVSTYLE
    SAVSTYLE = SUBSTR(STYLE,1,lnClrPos-1)
    *--i.e print at least one time
    llAnyLines=.T.
    @ ROW,00 SAY 'STYLE:'
    @ ROW,07 SAY SUBSTR(STYLE,1,lnClrPos-1)
    @ ROW,20 SAY DESC
    @ ROW,50 SAY 'SEA/DIV: ' + SEASON + '/' + cDIVISION
    @ ROW,66 SAY 'PATTERN: ' + PATTERN        
    ROW = ROW+2
    @ ROW,00 SAY 'COLOR  COLOR DESC.....'
    @ ROW,25 SAY 'SCALE: ' + '(' + ALLTRIM(SCALE) + ')'
    X = GETSCALE(SCALE,SPACE(1))
    @ ROW,048 SAY X
    @ ROW,100 SAY 'TOT'
    @ ROW,105 SAY 'NEG-OTS POS-OTS NET-OTS'
    ROW=ROW+1
  ENDIF && STYLE<>SAVSTYLE
  Y=1
  STORE ' ' TO FLD1,FLD2,FLD3
  DO WHILE Y<=7
    DO CASE
      CASE Y=1
        @ ROW,00 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
        lcColrDes=gfCODDES(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR')   && Color Description
        @ ROW,7 SAY SUBSTR(lcColrDes,1,15)
        SELECT(lcTmpStyle)
        @ ROW,25 SAY IIF(lcRpPrtBas='W','WIP ...........','PLAN...........')  
        FLD1 = IIF(lcRpPrtBas='W','WIP','PLAN')                               
      CASE Y=2
        @ ROW,25 SAY 'HANGING STOCK..'
        FLD1='STK'
      CASE Y=3
        @ ROW,25 SAY 'TOTAL AVAIL ...'
        FLD1=IIF(lcRpPrtBas='W','WIP','PLAN')  
        FLD2='STK'
      CASE Y=4
        @ ROW,25 SAY 'OPEN ORDERS ...'
        FLD1='ORD'
      CASE Y=5
        @ ROW,25 SAY 'OPEN TO SELL ..'
        FLD1=IIF(lcRpPrtBas='W','WIP','PLAN')  
        FLD2='STK'
        FLD3='ORD'
      CASE Y=6
        @ ROW,25 SAY 'YTD SHIPPED ...'
        FLD1='SHP'
      CASE Y=7
        @ ROW,25 SAY 'BOOKED ........'
        FLD1='ORD'
        FLD2='SHP'
    ENDCASE
    X=1
    DO WHILE X<=8
      Z=STR(X,1)
      DO CASE
        CASE FLD3<>' '
          @ ROW,(X*6)+42 SAY &FLD1.&Z + &FLD2.&Z - &FLD3.&Z PICTURE '@Z 99999'
        CASE FLD2<>' '
          @ ROW,(X*6)+42 SAY &FLD1.&Z + &FLD2.&Z            PICTURE '@Z 99999'
        OTHERWISE
          @ ROW,(X*6)+42 SAY &FLD1.&Z                       PICTURE '@Z 99999'
      ENDCASE
      X=X+1
    ENDDO && X<=8
    DO CASE
      CASE Y=1
        @ ROW,96 SAY IIF(lcRpPrtBas='W',TOTWIP,TOTPLAN) PICTURE '9999999'  
      CASE Y=2
        @ ROW,96 SAY TOTSTK                PICTURE '9999999'
      CASE Y=3
        @ ROW,96 SAY IIF(lcRpPrtBas='W',TOTWIP,TOTPLAN)+TOTSTK PICTURE '9999999'  
      CASE Y=4
        @ ROW,96 SAY TOTORD                PICTURE '9999999'
      CASE Y=5
        X=1
        STORE 0 TO NEGOTS,POSOTS,NETOTS
        DO WHILE X<=8
          Z=STR(X,1)
          WKTOT = IIF(lcRpPrtBas='W',WIP&Z,PLAN&Z) + STK&Z - ORD&Z 
          IF WKTOT >0
            POSOTS = POSOTS + WKTOT
          ELSE
            NEGOTS = NEGOTS + WKTOT
          ENDIF
          NETOTS = NETOTS + WKTOT
          X = X+1
        ENDDO && X<=8
        IF NEGOTS <>0
          @ ROW,105 SAY NEGOTS PICTURE '9999999'
        ENDIF
        @ ROW,113 SAY POSOTS   PICTURE '9999999'
        @ ROW,121 SAY NETOTS   PICTURE '9999999'
      CASE Y=6
        @ ROW,96 SAY TOTSHP    PICTURE '9999999'
      CASE Y=7
        @ ROW,96 SAY TOTORD+TOTSHP PICTURE '9999999'
    ENDCASE
    ROW=ROW+1
    STORE ' ' TO FLD1,FLD2,FLD3
    Y=Y+1
    XINKEY   = INKEY()
  ENDDO && Y<=7
  SKIP
  *kam
  *PERPAGE = PERPAGE+1
  *kam
  ROW     = ROW+1
  *kam
  *IF PERPAGE > MAXPP
  *  EXIT
  *ENDIF
 *kam 
ENDDO &&  !EOF() AND INKEY() <> 32
IF !llAnyLines
  *--if enter here means no record printing
  SET DEVICE TO SCREEN
  =gfDialog('I','There are no records to display')
  RETURN 
ENDIF
DO ENDREPORT
RETURN
*-- end of function lfPntBySz
*!*************************************************************
*! Name      : lfPntNoSz
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : Print without size.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPntNoSz()
*!*************************************************************
FUNCTION lfPntNoSz
  *-- layout for printing without size
  
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+...
*    COLOR  COLOR DESC..... DIV SEA PATTERN         WIP    STOCK    AVAIL   ORDERS    O-T-S  SHIPPED    BOOKED
*    COLOR  COLOR DESC..... DIV SEA PATTERN        PLAN    STOCK    AVAIL   ORDERS    O-T-S  SHIPPED    BOOKED
*** 123456789*12345  123456789*123456789*
*    123456 123456789012345  XX  XX XXXXXXXXXX 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+...

*-- initialize variable
R_WIDTH  = 'W'
PAGENO    = 0
LINENO    = 99
MAX_LINES = 58
LMARGIN   = 0
llAnyLines=.F.  && flag if not printing any record
R_TITLE   = [ CUT & SOLD SUMMARY]
XTITLE    =lcRpTitle
STORE 0 TO TOT2,TOT3,TOT4,TOT5,TOT6,TOT8,TOT9
STORE 0 TO SUBTOT2,SUBTOT3,SUBTOT4,SUBTOT5,SUBTOT6,SUBTOT8,SUBTOT9
SET DEVICE TO PRINT
SELECT(lcTmpStyle)
GO TOP
XSCALE = GETSCALE(SCALE,SPACE(1))
SUB_SAVE  = SUBSTR(STYLE,1,lnClrPos-1) + SPACE(5) + IIF(EOF(),' ',XSCALE)
lcStyPnt=' '
DO WHILE !EOF() AND INKEY() <> 32
  IF (TOTSTK+TOTWIP+TOTORD) = 0         
    SKIP
    LOOP
  ENDIF      
  IF LINENO >= MAX_LINES
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'APP200',XTITLE,R_WIDTH
    LINENO=8
    *-- Added PATTERN to report heder. 
    IF lcRpPrtBas='W'
      @ PROW()+1,4 SAY 'COLOR  COLOR DESC..... DIV    SEA    PATTERN         WIP    STOCK    AVAIL   ORDERS    O-T-S  SHIPPED    BOOKED'
    ELSE
      @ PROW()+1,4 SAY 'COLOR  COLOR DESC..... DIV    SEA    PATTERN        PLAN    STOCK    AVAIL   ORDERS    O-T-S  SHIPPED    BOOKED'
    ENDIF
    @ PROW()+1,0 SAY REPLICATE('Ä',132)
    LINENO=LINENO+1
    
  ENDIF &&LINENO >= MAX_LINES

  *-- FILTER OUT MINIMUM O-T-S REQUEST
  IF lnRpMinQty <> 0
    X=1
    STORE 0 TO POSOTS, NEGOTS
    DO WHILE X<=8
      Z=STR(X,1)
      WKTOT = IIF(lcRpPrtBas='W',WIP&Z,PLAN&Z) + STK&Z - ORD&Z
      IF WKTOT >0
        POSOTS = POSOTS + WKTOT
      ELSE
        NEGOTS = NEGOTS + WKTOT
      ENDIF
      X = X+1
    ENDDO && X<=8
    DO CASE
      CASE lnRpMinQty >0 .AND. POSOTS<lnRpMinQty
        SKIP
        LOOP
      CASE lnRpMinQty <0 .AND. NEGOTS>lnRpMinQty
        SKIP
        LOOP
    ENDCASE
 ENDIF && lnRpMinQty <> 0
*--print subtotal
 XSCALE = GETSCALE(SCALE,SPACE(1))
 IF SUB_SAVE <> SUBSTR(STYLE,1,lnClrPos-1) + SPACE(5) + IIF(EOF(),' ',XSCALE)
   IF (SUBTOT2+SUBTOT3+SUBTOT5)<>0 
     @ PROW()+2,IIF(LMARGIN>0,LMARGIN-1,LMARGIN) SAY '**Subtotal** '
     LINENO=LINENO+2
     @ PROW(),052 SAY SUBTOT2   PICTURE '99999999'
     @ PROW(),061 SAY SUBTOT3   PICTURE '99999999'
     @ PROW(),070 SAY SUBTOT4   PICTURE '99999999'
     @ PROW(),079 SAY SUBTOT5   PICTURE '99999999'
     @ PROW(),088 SAY SUBTOT6   PICTURE '99999999'
     @ PROW(),097 SAY SUBTOT8   PICTURE '99999999'
     @ PROW(),106 SAY SUBTOT9   PICTURE '999999999'
     LINENO=LINENO+3
   ENDIF
   XSCALE = GETSCALE(SCALE,SPACE(1))
   SUB_SAVE  = SUBSTR(STYLE,1,lnClrPos-1) + SPACE(5) + IIF(EOF(),' ',XSCALE)
   TOT2     = TOT2 + SUBTOT2
   TOT3     = TOT3 + SUBTOT3
   TOT4     = TOT4 + SUBTOT4
   TOT5     = TOT5 + SUBTOT5
   TOT6     = TOT6 + SUBTOT6
   TOT8     = TOT8 + SUBTOT8
   TOT9     = TOT9 + SUBTOT9

   STORE 0 TO SUBTOT2,SUBTOT3,SUBTOT4,SUBTOT5,SUBTOT6,SUBTOT8,SUBTOT9
  
ENDIF &&SUB_SAVE <> STYLE + SPACE(5) + IIF(EOF(),' ',XSCALE)
*-- at least one record printing
llAnyLines=.T.
XSCALE = GETSCALE(SCALE,SPACE(1)) 
IF lcStyPnt<>SUBSTR(STYLE,1,lnClrPos-1) + SPACE(5) + IIF(EOF(),' ',XSCALE)
  @ PROW()+2,IIF(LMARGIN>0,LMARGIN-1,LMARGIN) SAY '** '
  XSCALE = GETSCALE(SCALE,SPACE(1))
  @ PROW(),PCOL() SAY SUBST(STYLE,1,lnClrPos-1) + SPACE(2) + DESC
  LINENO=LINENO+2
ENDIF
  XTOT1 = IIF(lcRpPrtBas='W',TOTWIP,TOTPLAN)+TOTSTK-TOTORD
  @ PROW()+1,0  SAY SPACE(4)
  @ PROW(),4    SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
  lcColrDes=gfCODDES(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR')   && Color Description
  @ PROW(),11 SAY SUBSTR(lcColrDes,1,10)
  SELECT(lcTmpStyle) 
  
  @ PROW(),027 SAY cDIVISION
  @ PROW(),034 SAY SEASON
  @ PROW(),041 SAY PATTERN
  @ PROW(),052 SAY IIF(lcRpPrtBas='W',TOTWIP,TOTPLAN) PICTURE '99999999'   
  @ PROW(),061 SAY TOTSTK           PICTURE '99999999'
  @ PROW(),070 SAY IIF(lcRpPrtBas='W',TOTWIP,TOTPLAN)+TOTSTK PICTURE '99999999'  
  @ PROW(),079 SAY TOTORD           PICTURE '99999999'
  @ PROW(),088 SAY XTOT1            PICTURE '99999999'
  @ PROW(),097 SAY TOTSHP           PICTURE '99999999'
  @ PROW(),106 SAY TOTORD+TOTSHP    PICTURE '999999999'
  LINENO = LINENO + 1

  SUBTOT2 = SUBTOT2 + IIF(lcRpPrtBas='W',TOTWIP,TOTPLAN)  
  SUBTOT3 = SUBTOT3 + TOTSTK
  SUBTOT4 = SUBTOT4 + IIF(lcRpPrtBas='W',TOTWIP,TOTPLAN)+TOTSTK  
  SUBTOT5 = SUBTOT5 + TOTORD
  SUBTOT6 = SUBTOT6 + XTOT1
  SUBTOT8 = SUBTOT8 + TOTSHP
  SUBTOT9 = SUBTOT9 + TOTORD + TOTSHP
  XSCALE = GETSCALE(SCALE,SPACE(1)) 
  lcStyPnt=SUBSTR(STYLE,1,lnClrPos-1) + SPACE(5) + IIF(EOF(),' ',XSCALE)
  SKIP 
 ENDDO &&!EOF() AND INKEY() <> 32
*--check for at least one record print or not
IF !llAnyLines
  *--if enter here means no record printing
  SET DEVICE TO SCREEN
  =gfDialog('I','There are no records to display')
  RETURN 
ENDIF

IF (SUBTOT2+SUBTOT3+SUBTOT5)<>0
  @ PROW()+2,IIF(LMARGIN>0,LMARGIN-1,LMARGIN) SAY '**Subtotal** '
  LINENO=LINENO+2
  @ PROW(),052 SAY SUBTOT2   PICTURE '99999999'
  @ PROW(),061 SAY SUBTOT3   PICTURE '99999999'
  @ PROW(),070 SAY SUBTOT4   PICTURE '99999999'
  @ PROW(),079 SAY SUBTOT5   PICTURE '99999999'
  @ PROW(),088 SAY SUBTOT6   PICTURE '99999999'
  @ PROW(),097 SAY SUBTOT8   PICTURE '99999999'
  @ PROW(),106 SAY SUBTOT9   PICTURE '999999999'
  LINENO = LINENO + 3
ENDIF

TOT2     = TOT2 + SUBTOT2
TOT3     = TOT3 + SUBTOT3
TOT4     = TOT4 + SUBTOT4
TOT5     = TOT5 + SUBTOT5
TOT6     = TOT6 + SUBTOT6
TOT8     = TOT8 + SUBTOT8
TOT9     = TOT9 + SUBTOT9
STORE 0 TO SUBTOT2,SUBTOT3,SUBTOT4,SUBTOT5,SUBTOT6,SUBTOT8,SUBTOT9
*kam
*IF (!EOF()) .AND. (LINENO < MAX_LINES)
*  @ PROW()+2,IIF(LMARGIN>0,LMARGIN-1,LMARGIN) SAY '** '
*  XSCALE = GETSCALE(SCALE,SPACE(1))
*  @ PROW(),PCOL() SAY STYLE + SPACE(2) + STYLE->DESC
*ENDIF
*kam
@ PROW()+2,IIF(LMARGIN>0,LMARGIN-1,LMARGIN) SAY '*** Total ***'
LINENO=LINENO+2
@ PROW(),052 SAY TOT2  PICTURE '99999999'
@ PROW(),061 SAY TOT3  PICTURE '99999999'
@ PROW(),070 SAY TOT4  PICTURE '99999999'
@ PROW(),079 SAY TOT5  PICTURE '99999999'
@ PROW(),088 SAY TOT6  PICTURE '99999999'
@ PROW(),097 SAY TOT8  PICTURE '99999999'
@ PROW(),106 SAY TOT9  PICTURE '999999999'
@ PROW()+1,0 SAY [] &&just to get last line from some printers
DO ENDREPORT
RETURN
*--end of function lfPntNoSz



*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

   lnMajSeg    = gfItemMask('SM')  && No. of major segments.
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
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  *lcStyStat = lcStyMajor + ' Status'
  lcStyMake = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfStySum
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
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
*-- end of lfStySum.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
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
*-- end of lfsrvSty.

FUNCTION lfvBySize
CLEAR READ
*-- End of lfvBySize.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvFabric.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfGetClrD
*! Developer : AbdElrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : To get color position
*!*************************************************************
*! Example            :  =lfGetClrD()
*!*************************************************************
function lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--Get Color Discreption