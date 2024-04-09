*:************************************************************************
*: Program file  : ICFRESTU.PRG
*: Program desc. : Free to Sell Report for STU20
*: System        : Aria Advantage Series VER. 2.7
*: Module        : IC2
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Date          : 06/03/2004
*: Entry #       : *C122455,1  
*:************************************************************************
* Modifications
*C122455,6 TMI 07/06/2004 A modification needed that a style must be printed wholly within the same page.
*               To do this idea , do not use grouping in the report, use detail only, by trials ,
*               get the number of rows that the page can hold
*               put only one big group that based on the reccount
*               for a style, if it ends befor the page max row , it fits, else put it in the next
*               page ( group )
*               for each style divide its rows, header row , details rows, and totasls rows
*C122455,9 TMI 10/14/2004 Develop another way to collect styles with dimension = 1 
*C122455,9  TMI [Start] 
*  Note : The Stu20 customer has styles with dimension 2
*  for example , he has style STY1 with 3 sizes for scale in a scale set 
*i.e let STY1 has scale '40' with 2 dimension as follows
*40A  SZ1,SZ2,SZ3
*40B  SZ1,SZ2,SZ3
*40C  SZ1,SZ2,SZ3
*40D  SZ1,SZ2,SZ3
*40E  SZ1,SZ2,SZ3
*40F  SZ1,SZ2,SZ3
*40G  SZ1,SZ2,SZ3
*40H  SZ1,SZ2,SZ3
*40I  SZ1,SZ2,SZ3
*40J  SPEC
*In reporting to list each style with all its size scales is more consuming for papers , so this report
*list STY1 as follows
*     40A,40B,40C,40D,40E,40F,40G,40H,40I,40J
*SZ1   x   x   x  , ....
*SZ2   x   x   x  , ...
*SZ3   x   x   x  , ...
*where x's stands for qty's
*
*If a style is one dimensional it will be displayed as standard , the code for collecting data for this 
*update is copied from that of styles with dimension 2 as it is with small changes to list all sizes in one line
*not per column
*C122455,9  TMI [End  ] 
*C125285,1 NNA 02/06/2005 Add an option to Export the report to Excel and change the print price
*C125285,1 NNA            Option to (No Price - Euro Price - Sterling Price)
*:***************************************************************************

STORE 16 TO lnClrCnt
*C122455,6  TMI [Start] set page lines#

lnPageLines = 64
*C125285,1 NNA 02/06/2005 (Begin) Change the Page Lines in case of Exported to Excel
IF llRpExport
  lnPageLines = 45
ENDIF
*C125285,1 NNA (End)

*C122455,6  TMI [End  ] 
STORE ' ' TO lcMjrPct,lcSeprt,lcClrSpr
STORE 0   TO lnstylewid, lnClrLen, lnClrPos, lnSclLen, lnSclPos
=lfGtItmDat()
*- Get Extended size width
lnExtSzWd = gfGetMemVar('M_EXTWIDTH')
*-- if it's first time you run option Grid
IF llFrTime
  lcObj_Id  = ''                       && Object Identification in Objlink file.
  *-- if this company have a logo, put it in temp. file and then use it in .FRX
  IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
     SEEK(OBJLINK.cObject_ID,'OBJECTS')
    = lfGetLogo()  && Function to Fill the temp. With company Logo.
  ENDIF
  lnStyPos = lfItmPos('STYLE.CSTYMAJOR')
  lnDivPos = lfItmPos('STYLE.CDIVISION')
  lnSesPos = lfItmPos('STYLE.SEASON')
  lnStsPos = lfItmPos('STYLE.STATUS')
  lnGrpPos = lfItmPos('STYLE.CSTYGROUP')
  lnLocPos = lfItmPos('STYDYE.CWARECODE')
  llFrTime = .F.
ENDIF

*- Collecting data
IF llOGFltCh
  STORE '' TO lcGrpExp, lcSesExp, lcStsExp, lcDivExp
              
  IF !EMPTY(laOgFxFlt[lnGrpPos,6])
    lcGrpExp = "&laOgFxFlt[lnGrpPos,1]." + " $ laOgFxFlt[lnGrpPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnSesPos,6])
    lcSesExp = "&laOgFxFlt[lnSesPos,1]." + " $ laOgFxFlt[lnSesPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnStsPos,6])
    lcStsExp = "&laOgFxFlt[lnStsPos,1]." + " $ laOgFxFlt[lnStsPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnDivPos,6])
    lcDivExp = "&laOgFxFlt[lnDivPos,1]." + " $ laOgFxFlt[lnDivPos,6]"
  ENDIF    

  lcStyFil  = laOgFxFlt[lnStyPos,6]
  llStyDeal = !EMPTY(lcStyFil) AND USED(lcStyFil) AND RECCOUNT(lcStyFil) > 0
  
  lcLocFil  = laOgFxFlt[lnLocPos,6]
  llLocDeal = !EMPTY(lcLocFil) AND USED(lcLocFil) AND RECCOUNT(lcLocFil) > 0
  
 *C125285,1 NNA 02/06/2005 (Begin) Get the Exchange Rate from (GBP) to (EUR) if user select to
 *C125285,1 NNA            print price by Euro And the style has no record in the Styprice file,
 STORE 0 TO lnExtRate
 STORE SPACE(1) TO lcExSin
 IF lcRpPriLn='N' OR  lcRpPriLn='S' 
   lcExSin   = '*'
   lnExtRate = 1
 ELSE lcRpPriLn='E' 
   lcExSin   = gfGetExSin(@lcExSin,'EUR',GcBaseCurr)
   lnExtRate = lfGetRate(GcBaseCurr,'EUR')
 ENDIF
 IF USED(lcTmpFile)
   USE IN &lcTmpFile
 ENDIF
 *C125285,1 NNA (End)

  *- Create/Zap temp file
  =lfCrtTmp()

  *- Collecting data  
  =lfCollData()
ENDIF
WAIT CLEAR

*- Delete unneeded records
SELECT &lcTmpChld
SET ORDER TO TAG STYTYPE
*C122455,5  TMI [Start] if minimum OTS is 0 do not use this option
*                       I will on coping all lines to another temp file and browse from it
*-* RECALL ALL
*-* LOCATE
*-* *- Show OTS (Positive/Negative/All)
*-* DELETE FOR (TYPE='Q' .AND. TOTQTY < lnRpMinOts) .OR. ;
*-*            (TYPE='Q' .AND. IIF(lcRpShwOTS='P',TOTQTY<0,IIF(lcRpShwOTS='N',TOTQTY>0,.F.))) .OR. ;
*-*            IIF(llRpPrnZer,.F.,TOTQTY=0)
*-* *- Show the report
*-* SELECT &lcTmpChld

*- Copy data to new temp file to make needed updates
lcShwRepFl = 'Z'+SUBSTR(lcTmpChld,2)
GO TOP
IF USED(lcShwRepFl)
  USE IN &lcShwRepFl
ENDIF  
COPY TO (gcWorkDir+lcShwRepFl) WITH CDX
USE (gcWorkDir+lcShwRepFl) IN 0 EXCLUSIVE
SELECT &lcShwRepFl

lcDelStat = SET('DELETED')
SET DELETED OFF

*C122455,6  TMI [Start] Add this loop to show or hide lines with type 'R'
GO TOP
lnPageno = 1     
lnLine = 0
lnTypSRec = 0

DO WHILE !EOF()
  lcSty = cStyClr
  lnRecno = RECNO()   && Record # of the new style major in the temp file
  lnTypeR = 0         && record # of TYPE = 'R' record
  
  llSkipPage = .F.    && Set true if going to new page
  lnTotQty = 0        && Total style qty
  lnTotW = 0          && total wip qty
  llShowQ = .F.       && set true if there is at least one line of type 'Q' to be shown
  llShowW = .F.       && set true if there is at least one line of type 'W' to be shown
  llWip = .F.         && used to show the record with ERLDATE 
  llDescAdded = .F.   && show the descreption of the style
  llPrcAAdded = .F.   && show priceA of the style
  
  =SEEK(SUBSTR(lcSty,1,lnStyleWid),'STYLE')
  WAIT WINDOW NOWAIT ALLTRIM(lcSty)
  
  SCAN REST WHILE cStyClr+TYPE+SIZENO+SIZE = lcSty
  
    DELETE  && Delete the record and recall it if needed
    
    REPLACE PAGENO WITH lnPageno

    DO CASE
    
    CASE TYPE = 'Q'

      *-Get Line total
      IF lcRpShwOTS <> 'A'
        lnLineTot = 0
        FOR lnQ = 1 TO lnClrCnt
          lcQ = ALLTRIM(STR(lnQ))
          IF (lcRpShwOTS='P' .AND. QTY&lcQ<0) .OR. (lcRpShwOTS='N' .AND. QTY&lcQ>0)
            REPLACE QTY&lcQ WITH 0
          ENDIF
          lnLineTot = lnLineTot + QTY&lcQ
        ENDFOR
        REPLACE TOTQTY WITH lnLineTot
      ENDIF

      IF ((lcRpShwOTS='P' .AND. TOTQTY>0) .OR. (lcRpShwOTS='N' .AND. TOTQTY<0) .OR. lcRpShwOTS='A') .AND. ;
         IIF(lnRpMinOts=0,.T.,TOTQTY>=lnRpMinOts) .AND. ;
         IIF(TOTQTY=0,llRpPrnZer,.T.)
        RECALL
        lnTotQty = lnTotQty + TOTQTY
        lnLine = lnLine + 1
        llShowQ = .T.          
        IF NOT llDescAdded
          REPLACE DESC1 WITH STYLE.DESC1 ;
                  FLAG1 WITH 'D'
          llDescAdded = .T.
        ELSE

		  *C125285,1 NNA 02/06/2005 (Begin) if user Select To Print No Price then the EX. Rate will
		  *C125285,1 NNA            be (1)
          *IF NOT llPrcAAdded .AND. NOT llRpPriLn
          IF NOT llPrcAAdded .AND. lcRpPriLn='N'
		  *C125285,1 NNA (End)
            REPLACE DESC1 WITH 'Price - ' + STR(STYLE.PRICEA,7,2) ;
                     FLAG1 WITH 'D'

            llPrcAAdded = .T.
          ENDIF
        ENDIF
      ENDIF
      
    CASE TYPE = 'R'   && THis is the summary part of the type 'Q'
      IF llShowQ
        REPLACE TOTQTY WITH lnTotQty ;
                CDATE  WITH 'x'
        lnLine = lnLine + 1
        RECALL
        
        *C125285,1 NNA 02/06/2005 (Begin) Replace Price with Price * Ex. Rate
        *IF NOT llPrcAAdded .AND. NOT llRpPriLn
        IF NOT llPrcAAdded .AND. lcRpPriLn='N'
        *C125285,1 NNA (End)
           REPLACE DESC1 WITH 'Price - ' + ALLTRIM(TRANSFORM(STYLE.PRICEA,'@$99999.99')) ;
                    FLAG1 WITH 'D'
          llPrcAAdded = .T.
        ENDIF

        lnTypeR = RECNO()
        GOTO (lnRecno)
        RECALL
        lnLine = lnLine + 1
        GOTO (lnTypeR)
      ENDIF      
      
    CASE TYPE = 'S'
      lnTypSRec = RECNO()
      
    CASE TYPE = 'W'
      IF TOTQTY<>0 .OR. llRpPrnZer
        llShowW = .T.
        llWip = .T.
      ENDIF
      IF llWip
        IF CDATE = ' '
          RECALL
          lnLine = lnLine + 1
          lnTotW = lnTotW + TOTQTY
        ELSE          
          llWip = .F.
          IF !EMPTY(ERLDATE)
            RECALL
            lnLine = lnLine + 1
            SKIP -1 
            REPLACE FLAG1 WITH 'W'
            SKIP
          ENDIF
        ENDIF
      ENDIF
  
    CASE TYPE = 'X'
      REPLACE TOTQTY WITH lnTotW ;
              CDATE  WITH 'x'
      IF llShowW
        RECALL
        lnLine = lnLine + 1
        
        lnBackHere = RECNO()
        GOTO (lnTypSRec)
        RECALL
        lnLine = lnLine + 1
        IF lnTypeR > 0
          GOTO (lnTypeR)
          REPLACE CDATE WITH ''
        ENDIF
        GOTO (lnRecno)
        IF DELETED()
          RECALL
          lnLine = lnLine + 1
        ENDIF
        GOTO (lnBackHere)
      ENDIF
      
    ENDCASE
    
    IF lnLine > lnPageLines
      llSkipPage = .T.   
      EXIT
    ENDIF
    
  ENDSCAN
  
  IF llSkipPage
    GOTO lnRecno
    lnLine = 0
    lnPageno = lnPageno + 1
    llSkipPage = .F.
  ENDIF

ENDDO 
*C122455,6  TMI [End  ] 
SET DELETED &lcDelStat
SET RELATION TO cStyClr INTO &lcTmpFile
GO TOP

*C122455,5  TMI [End  ] 
INDEX ON STR(PAGENO,4)+cStyClr+TYPE TAG STYTYPE
LOCATE
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ELSE

  *C125285,1 NNA 02/06/2005 (Begin) If user Select to Export to Excel
  IF llRpExport
    =lfExprt2XL()
  ELSE
  *C125285,1 NNA (End)

    DO gfDispRe WITH EVAL('lcRpForm')

  *C125285,1 NNA (Begin)
  ENDIF
  *C125285,1 NNA (End)

ENDIF  

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/26/2004
*:* Purpose     : When function for the report
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfwRepWhen

*-- end of lfwRepWhen.

*:**************************************************************************
*:* Name        : lfsrvSty  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/07/2004
*:* Purpose     : Set-Reset-Valid function for style browse
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
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
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty  .

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/98
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

*:**************************************************************************
*:* Name        : lfItmPos
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/21/2004
*!* Purpose     : Evaluate fixed filter position within array.
*:***************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- end of lfItmPos

*:**************************************************************************
*:* Name        : lfvOpTitle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/07/2004
*:* Purpose     : Optional title
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvOpTitle
PRIVATE laOptMsg
DECLARE laOptMsg[2,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[1,2] = 75                && Line length

=gfOptMsg('laOptMsg')

*-- end of lfvOpTitle.

*:**************************************************************************
*:* Name        : lfCollData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/07/2004
*:* Purpose     : Collecting data function 
*:***************************************************************************
FUNCTION lfCollData
PRIVATE lnClr
SET ORDER TO &lcTmpChld IN &lcTmpChld

WAIT WINDOW NOWAIT 'Start Collecting Data..'
GO TOP IN STYLE

IF llStyDeal    

  SELECT &lcStyFil
  SCAN
    =SEEK(SUBSTR(&lcStyFil..CSTYMAJOR,1,lnStyleWid) , 'STYLE')
     *C125285,1 NNA 02/06/2005 (BEGIN) Get the Foreign price fro the Styprice file
     STORE 0 TO lnPriceA
     lnPriceA = lfStyFrPrc()
     *C125285,1 NNA (End)
    
    IF SEEK(SUBSTR(STYLE.SCALE,1,lnExtSzWd),'SCALEHD') .AND. SCALEHD.NNOOFDIM = 1   && Ignore styles with dimenstion less than 2
      *C122455,9  TMI [Start] develop another way to collect styles with dimension = 1 10/14/2004
      *LOOP
      *C122455,9  TMI [End  ] 
    ENDIF
    DO WHILE NOT EOF('STYLE') .AND. STYLE.CSTYMAJOR = &lcStyFil..CSTYMAJOR
      =lfAddLines()
    ENDDO
  ENDSCAN

ELSE  
  
  SELECT STYLE
  DO WHILE !EOF('STYLE')
    IF SEEK(SUBSTR(STYLE.SCALE,1,lnExtSzWd),'SCALEHD') .AND. SCALEHD.NNOOFDIM = 1
      *C122455,9  TMI [Start] develop another way to collect styles with dimension = 1 10/14/2004
      *lcStyMaj = STYLE.CSTYMAJOR
      *SCAN REST WHILE STYLE.CSTYMAJOR = lcStyMaj 
      *ENDSCAN
      *LOOP
      *C122455,9  TMI [End  ]
    ENDIF
     *C125285,1 NNA 02/06/2005 (BEGIN) Get the Foreign price fro the Styprice file
     STORE 0 TO lnPriceA
     lnPriceA = lfStyFrPrc()
     *C125285,1 NNA (END)

    =lfAddLines()
  ENDDO

ENDIF

*-- End of lfCollData.

*:**************************************************************************
*:* Name        : lfAddLines
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/13/2004
*:* Purpose     : Add lines to temp collect file
*:***************************************************************************
*:* Called from : lfCollData
*:***************************************************************************
FUNCTION lfAddLines
SELECT &lcTmpFile
SCATTER MEMVAR BLANK
SELECT STYLE  
*C122455,3  TMI [Start] Do not Show the color in the report and sum all qtys for all colors
*lcStyClr = SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1)
lcStyClr = SUBSTR(STYLE.STYLE,1,lnstylewid)
*- Create an array of scale to get the # of record color for this style
lcStyScl = SUBSTR(STYLE.SCALE,1,lnExtSzWd)
DIMENSION laScl[1]
*C122455,8  TMI [Start] let scale array be ordered by scale field
*SELECT SCALE FROM SCALE WHERE SUBSTR(SCALE,1,lnExtSzWd) = lcStyScl INTO ARRAY laScl
SELECT SCALE FROM SCALE ;
    WHERE TYPE+SUBSTR(SCALE,1,lnExtSzWd) = 'S'+lcStyScl ;
    ORDER BY SCALE ;
    INTO ARRAY laScl
*C122455,8  TMI [End  ] 
*C122455,3  TMI [End  ] 
M.cStyClr = PADR(lcStyClr,19)
WAIT WINDOW NOWAIT ALLTRIM(M.cStyClr)
lnClr = 0

*C122455,9  TMI [Start] Get sizes in one dime scale in one line array
DIMENSION laLineSz[lnClrCnt]
laLineSz = ' '
IF SCALEHD.NNOOFDIM = 1
  SELECT SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SCALE,CNT ;
    FROM SCALE ;
    WHERE TYPE+SUBSTR(SCALE,1,lnExtSzWd) = 'S'+lcStyScl ;
    ORDER BY SCALE ;
    INTO ARRAY laScl
  lnI = 0
  FOR lnK = 1 TO ALEN(laScl,1)
    FOR lnJ = 1 TO laScl[lnK,10]
      lnI = lnI + 1
      IF lnI > lnClrCnt
        EXIT
      ENDIF
      laLineSz[lnI] = laScl[lnK,lnJ]
    ENDFOR
  ENDFOR
ENDIF
*C122455,9  TMI [End  ] 

*C122455,6  TMI [Start] 
llAddW = .F.
llAddQ = .F.

*C125285,1 NNA 02/06/2005 (Begin) Replace STYLE.PRICEA with STYLE.PRICEA * the Ex. Rate
*lcPriceA = STYLE.PRICEA
IF lcRpPriLn='E'
  IF lnPriceA = 0
    lcPriceA = STYLE.PRICEA &lcExSin lnExtRate
  ELSE
    lcPriceA = lnPriceA
  ENDIF
ELSE
  lcPriceA = STYLE.PRICEA
ENDIF
*lcPriceA = IIF(lcRpPriLn='E',IIF(lnPriceA=0,STYLE.PRICEA &lcExSin lnExtRate,lnPriceA),STYLE.PRICEA)
*C125285,1 NNA (End)
*C122455,6  TMI [End  ] 
SCAN REST WHILE STYLE.STYLE = lcStyClr ;
            FOR IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) .AND. ; 
                IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp)) .AND. ; 
                IIF(EMPTY(lcSesExp),.T., EVALUATE(lcSesExp)) .AND. ; 
                IIF(EMPTY(lcStsExp),.T., EVALUATE(lcStsExp))

  *C122455,9  TMI [Start] Collecting styles with dim > 1
  IF SCALEHD.NNOOFDIM <> 1
    *C122455,9  TMI [End  ] 
  
    *C122455,3  TMI [Start] Get color # from the laScl array
    *lnClr = lnClr + 1
    lnClr = ASCAN(laScl,STYLE.SCALE)
    *C122455,3  TMI [End  ] 
    lcClr = ALLTRIM(STR(lnClr))
    STORE STYLE.DESC TO ('M.CLR'+ALLTRIM(STR(lnClr)) )
    *C122455,6  TMI [Start] Store priceA for each color in the file lcTmpFile variables

    *C125285,1 NNA 02/06/2005 (Begin) Replace STYLE.PRICEA with STYLE.PRICEA * the Ex. Rate
    *STORE STYLE.PRICEA TO ('M.PRICEA'+ALLTRIM(STR(lnClr)) )
    lnPrice = IIF(lcRpPriLn='E',IIF(lnPriceA=0,(STYLE.PRICEA &lcExSin lnExtRate),lnPriceA),STYLE.PRICEA)
    STORE lnPrice TO ('M.PRICEA'+ALLTRIM(STR(lnClr)) )
    *C125285,1 NNA (End)

    *C122455,6  TMI [End  ] 
      
    =SEEK('S'+STYLE.SCALE,'SCALE')
    FOR lnSz = 1 TO SCALE.CNT
    
      SELECT &lcTmpChld
      lcSz = STR(lnSz,1)
  
      IF NOT llLocDeal
  
        IF !SEEK(M.cStyClr+'Q'+lcSz+SCALE.Sz&lcSz,lcTmpChld)
          INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'Q',lcSz,SCALE.Sz&lcSz,SUBSTR(STYLE.DESC1,1,20))        
        ENDIF      
        *C122455,3  TMI [Start] Change the equation as per Tony
        *-* REPLACE QTY&lcClr WITH STYLE.STK&lcSz - ;
        *-*                        IIF(lcRpRepClc='S', STYLE.ALO&lcSz , ;
        *-*                           STYLE.ORD&lcSz + IIF(lcRpRepClc='I' , STYLE.WIP&lcSz , 0 )) ;
        *-*         TOTQTY    WITH TOTQTY + QTY&lcClr ;
        *-*         PRICEA    WITH STYLE.PRICEA
        REPLACE QTY&lcClr WITH QTY&lcClr + STYLE.STK&lcSz - ;
                               IIF(lcRpRepClc='S', STYLE.ALO&lcSz , ;
                                  STYLE.ORD&lcSz + IIF(lcRpRepClc='I' , -STYLE.WIP&lcSz , 0 )) ;
                TOTQTY    WITH TOTQTY + QTY&lcClr 
        *C122455,3  TMI [End  ]   
  
        *-Report Calculations : Stock Service|Seasonal Incl. WIP|Seasonal Excl. WIP ~ S|I|X
        IF lcRpRepClc = 'S'
          IF !SEEK(M.cStyClr+'W'+lcSz+SCALE.Sz&lcSz,lcTmpChld)
            INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'W',lcSz,SCALE.Sz&lcSz,SUBSTR(STYLE.DESC1,1,20))
            *C122455,6  TMI [Start] 
            llAddW = .T.
            *C122455,6  TMI [End  ] 
          ENDIF      
          *C122455,3  TMI [Start] Let qty for colors be calculated by incrementation , to include case of two colors
          *-* REPLACE QTY&lcClr WITH STYLE.WIP&lcSz ;
          *-*         TOTQTY    WITH TOTQTY + STYLE.WIP&lcSz ;
          *-*         ERLDATE   WITH IIF(EMPTY(ERLDATE),lfErlDate(), ERLDATE )
          *C122455,7  TMI [Start] update the date in the line with type 'D'
          *REPLACE QTY&lcClr WITH QTY&lcClr + STYLE.WIP&lcSz ;
                  TOTQTY    WITH TOTQTY + STYLE.WIP&lcSz ;
                  ERLDATE   WITH IIF(EMPTY(ERLDATE),lfErlDate(), ERLDATE )                
          REPLACE QTY&lcClr WITH QTY&lcClr + STYLE.WIP&lcSz ;
                  TOTQTY    WITH TOTQTY + STYLE.WIP&lcSz 
          *C122455,7  TMI [End  ]                 
          *C122455,3  TMI [End  ] 
          *C122455,6  TMI [Start] Add a new line for the date
          m.ERLDATE = &lcTmpChld..ERLDATE
          IF !SEEK(M.cStyClr+'W'+lcSz+SCALE.Sz&lcSz+'D',lcTmpChld)
            INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,CDATE,ERLDATE) VALUES (M.cStyClr,'W',lcSz,SCALE.Sz&lcSz,'D',m.ERLDATE)
          ENDIF
          *C122455,7  TMI [Start] Update the ErlDate field
          REPLACE ERLDATE WITH IIF(EMPTY(ERLDATE),lfErlDate(), ERLDATE )
          *C122455,7  TMI [End  ] 
          *C122455,6  TMI [End  ] 
        ENDIF
          
      ELSE        
        
        SELECT &lcLocFil
        SCAN
          *C122455,8  TMI [Start] seek for style with dyelots = space(10)
          *IF SEEK(STYLE.STYLE+&lcLocFil..CWARECODE,'STYDYE')
          IF SEEK(STYLE.STYLE+&lcLocFil..CWARECODE+SPACE(10),'STYDYE')
            *C122455,8  TMI [End  ] 
            SELECT STYDYE
            *C122455,8  TMI [Start] seek for empty dyelot
            *SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE+&lcLocFil..CWARECODE
            SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE+&lcLocFil..CWARECODE+SPACE(10)
              *C122455,8  TMI [End  ] 
              IF !SEEK(M.cStyClr+'Q'+lcSz+SCALE.Sz&lcSz,lcTmpChld)
                INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'Q',lcSz,SCALE.Sz&lcSz,SUBSTR(STYLE.DESC1,1,20))
              ENDIF      
              SELECT &lcTmpChld
              *C122455,3  TMI [Start] Change the equation as per tony
              *-* lnFre = STYDYE.STK&lcSz - IIF(lcRpRepClc='S', STYDYE.ALO&lcSz , ;
              *-*                            STYDYE.ORD&lcSz + IIF(lcRpRepClc='I' , STYDYE.WIP&lcSz ,0))
              lnFre = STYDYE.STK&lcSz - IIF(lcRpRepClc='S', STYDYE.ALO&lcSz , ;
                                         STYDYE.ORD&lcSz + IIF(lcRpRepClc='I' , -STYDYE.WIP&lcSz ,0))
              *C122455,3  TMI [End  ] 
              REPLACE QTY&lcClr WITH QTY&lcClr + lnFre ;
                      TOTQTY    WITH TOTQTY    + lnFre 
              
              IF lcRpRepClc = 'S'
                IF !SEEK(M.cStyClr+'W'+lcSz+SCALE.Sz&lcSz,lcTmpChld)
                  INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'W',lcSz,SCALE.Sz&lcSz,SUBSTR(STYLE.DESC1,1,20))
                  *C122455,6  TMI [Start]  Add line with type 'W'
                  llAddW = .T.
                  *C122455,6  TMI [End  ] 
                ENDIF      
                REPLACE QTY&lcClr WITH QTY&lcClr + STYDYE.WIP&lcSz ;
                        TOTQTY    WITH TOTQTY    + STYDYE.WIP&lcSz
                *C122455,7  TMI [Start] Update the ErlDate field in the line with type 'D'
                *IF EMPTY(ERLDATE)
                *  REPLACE ERLDATE WITH lfErlDate()
                *ENDIF
                *C122455,7  TMI [End  ] 
                *C122455,6  TMI [Start] Add a new line for the date
                m.ERLDATE = &lcTmpChld..ERLDATE
                IF !SEEK(M.cStyClr+'W'+lcSz+SCALE.Sz&lcSz+'D',lcTmpChld)
                  INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,CDATE,ERLDATE) VALUES (M.cStyClr,'W',lcSz,SCALE.Sz&lcSz,'D',m.ERLDATE)
                ENDIF
                *C122455,6  TMI [End  ] 
                *C122455,7  TMI [Start] Update the ErlDate field
                REPLACE ERLDATE WITH IIF(EMPTY(ERLDATE),lfErlDate(), ERLDATE )
                *C122455,7  TMI [End  ] 
              ENDIF
            ENDSCAN
          ENDIF
        ENDSCAN
        
      ENDIF
    ENDFOR    
    *C122455,9  TMI [Start] Develop a new way to collect data for styles with Dim is 1

  ELSE 

    =SEEK('S'+STYLE.SCALE,'SCALE')  
    FOR lnSz = 1 TO SCALE.CNT
       
      lcSz = STR(lnSz,1)
      lnLnSz = ASCAN(laLineSz,SCALE.SZ&lcSz)
      lcLnSz = ALLTRIM(STR(lnLnSz))

      IF lnLnSz > 0  && Ignore more than 16 sizes
      
        M.CLR&lcLnSz = SCALE.SZ&lcSz

	    *C125285,1 NNA 02/06/2005 (Begin) Replace STYLE.PRICEA with STYLE.PRICEA * the Ex. Rate
        *M.PRICEA&lcLnSz = STYLE.PRICEA
        M.PRICEA&lcLnSz = (STYLE.PRICEA &lcExSin lnExtRate)
        *C125285,1 NNA (End)
    
        SELECT &lcTmpChld
        
        IF NOT llLocDeal
    
          IF !SEEK(M.cStyClr+'Q',lcTmpChld)
            INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'Q',lcSz,'',SUBSTR(STYLE.DESC1,1,20))        
          ENDIF      
          REPLACE QTY&lcLnSz WITH QTY&lcLnSz + STYLE.STK&lcSz - ;
                                 IIF(lcRpRepClc='S', STYLE.ALO&lcSz , ;
                                    STYLE.ORD&lcSz + IIF(lcRpRepClc='I' , -STYLE.WIP&lcSz , 0 )) ;
                  TOTQTY    WITH TOTQTY + QTY&lcLnSz 
    
          *-Report Calculations : Stock Service|Seasonal Incl. WIP|Seasonal Excl. WIP ~ S|I|X
          IF lcRpRepClc = 'S'
            IF !SEEK(M.cStyClr+'W',lcTmpChld)
              INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'W',lcSz,'',SUBSTR(STYLE.DESC1,1,20))
              llAddW = .T.
            ENDIF      
            REPLACE QTY&lcLnSz WITH QTY&lcLnSz + STYLE.WIP&lcSz ;
                    TOTQTY     WITH TOTQTY     + STYLE.WIP&lcSz 
            m.ERLDATE = &lcTmpChld..ERLDATE
            IF !SEEK(M.cStyClr+'W'+'D',lcTmpChld)
              INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,CDATE,ERLDATE) VALUES (M.cStyClr,'W',lcSz,'','D',m.ERLDATE)
            ENDIF
            REPLACE ERLDATE WITH IIF(EMPTY(ERLDATE),lfErlDate(), ERLDATE )
          ENDIF
            
        ELSE        
          
          SELECT &lcLocFil
          SCAN
            IF SEEK(STYLE.STYLE+&lcLocFil..CWARECODE+SPACE(10),'STYDYE')
              SELECT STYDYE
              SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE+&lcLocFil..CWARECODE+SPACE(10)
                IF !SEEK(M.cStyClr+'Q',lcTmpChld)
                  INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'Q',lcSz,'',SUBSTR(STYLE.DESC1,1,20))
                ENDIF      
                SELECT &lcTmpChld
                lnFre = STYDYE.STK&lcSz - IIF(lcRpRepClc='S', STYDYE.ALO&lcSz , ;
                                           STYDYE.ORD&lcSz + IIF(lcRpRepClc='I' , -STYDYE.WIP&lcSz ,0))
                REPLACE QTY&lcLnSz WITH QTY&lcLnSz + lnFre ;
                        TOTQTY     WITH TOTQTY    + lnFre 
                
                IF lcRpRepClc = 'S'
                  IF !SEEK(M.cStyClr+'W'+lcSz+SCALE.Sz&lcSz,lcTmpChld)
                    INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,Desc1) VALUES (M.cStyClr,'W',lcSz,'',SUBSTR(STYLE.DESC1,1,20))
                    llAddW = .T.
                  ENDIF      
                  REPLACE QTY&lcLnSz WITH QTY&lcLnSz + STYDYE.WIP&lcSz ;
                          TOTQTY     WITH TOTQTY     + STYDYE.WIP&lcSz
                  m.ERLDATE = &lcTmpChld..ERLDATE
                  IF !SEEK(M.cStyClr+'W'+'D',lcTmpChld)
                    INSERT INTO &lcTmpChld (cStyClr,TYPE,SIZENO,SIZE,CDATE,ERLDATE) VALUES (M.cStyClr,'W',lcSz,'','D',m.ERLDATE)
                  ENDIF
                  REPLACE ERLDATE WITH IIF(EMPTY(ERLDATE),lfErlDate(), ERLDATE )
                ENDIF
              ENDSCAN
            ENDIF
          ENDSCAN          
        ENDIF        
      ENDIF
      
    ENDFOR    
  ENDIF
  *C122455,9  TMI [End  ]   
  
  *C122455,6  TMI [Start] Add a line to hold style major with sizes ( colors )  
  IF !EMPTY(M.cStyClr) .AND. !SEEK(M.cStyClr+'1',lcTmpChld)
    INSERT INTO &lcTmpChld (cStyClr,TYPE) VALUES (M.cStyClr,'1')
    INSERT INTO &lcTmpChld (cStyClr,TYPE) VALUES (M.cStyClr,'R')
  ENDIF
  IF llAddW .AND. !SEEK(M.cStyClr+'S',lcTmpChld)
    INSERT INTO &lcTmpChld (cStyClr,TYPE) VALUES (M.cStyClr,'S')
    INSERT INTO &lcTmpChld (cStyClr,TYPE) VALUES (M.cStyClr,'X')
  ENDIF
  *C122455,6  TMI [End  ] 
ENDSCAN
  
IF !EMPTY(M.cStyClr) .AND. !SEEK(M.cStyClr,lcTmpFile)
  INSERT INTO &lcTmpFile FROM MEMVAR
ENDIF

*-- end of lfAddLines.

*:**************************************************************************
*:* Name        : lfErlDate
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/08/2004
*:* Purpose     : Get earleist date the style available
*:***************************************************************************
FUNCTION lfErlDate
PRIVATE lnSlct,  ldDt
lnSlct = SELECT()
ldDt = {//}

SELECT POSLN
=SEEK(STYLE.STYLE,'POSLN')

*C122455,3  TMI [Start] Comment these lines
*-* =SEEK(POSLN.CSTYTYPE+POSLN.PO,'POSHDR')
*-* IF POSHDR.STATUS = 'O'
*-*   ldDt = POSHDR.AVAILABLE
*-* ENDIF  
*-* IF !EMPTY(ldDt)
*C122455,3  TMI [End  ] 

  SELECT POSLN
  SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = STYLE.STYLE
    *C122455,5  TMI [Start] Include orders of status O,A and H
    *IF SEEK(POSLN.CSTYTYPE+POSLN.PO,'POSHDR') .AND. POSHDR.STATUS = 'O'
    IF SEEK(POSLN.CSTYTYPE+POSLN.PO,'POSHDR') .AND. POSHDR.STATUS $ 'OAH'
      *C122455,5  TMI [End  ] 
      *C122455,3  TMI [Start] Get Minimum of ldDt and POSHDR.AVAILABLE
      *ldDt = IIF(ldDt > POSHDR.AVAILABLE , POSHDR.AVAILABLE , ldDt )
      *C122455,4  TMI [Start] Pick least date provided it is not empty
      *ldDt = MIN( ldDt , POSHDR.AVAILABLE )
      ldDt = IIF(EMPTY(ldDt) , POSHDR.AVAILABLE , MIN( ldDt , POSHDR.AVAILABLE ) )
      *C122455,4  TMI [End  ] 
      *C122455,3  TMI [End  ] 
    ENDIF
  ENDSCAN

*C122455,3  TMI [Start] 
*-* ENDIF  
*C122455,3  TMI [End  ]   

SELECT (lnSlct)
RETURN ldDt
*-- end of lfErlDate.
*:**************************************************************************
*:* Name        : lfCrtTmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/07/2004
*:* Purpose     : Create temp file to collect data in 
*:***************************************************************************
*:* Called from : lfCollData
*:***************************************************************************
FUNCTION lfCrtTmp
PRIVATE laStru,lnI,lnK,lcK

IF USED(lcTmpFile)
  SELECT &lcTmpFile
  ZAP
  SELECT &lcTmpChld
  ZAP
  RETURN
ENDIF  

lnI = 0
lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'cStyClr'
laStru[lnI,2] = 'C'
laStru[lnI,3] = 19
laStru[lnI,4] = 0

FOR lnK = 1 TO lnClrCnt
  lcK = ALLTRIM(STR(lnK))  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'CLR'+lcK
  laStru[lnI,2] = 'C'
  *C122455,9  TMI [Start] 
  *laStru[lnI,3] = 3
  laStru[lnI,3] = 5
  *C122455,9  TMI [End  ] 
  laStru[lnI,4] = 0
  
  *C122455,6  TMI [Start] Add fields for priceA for each color
  lcK = ALLTRIM(STR(lnK))  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'PRICEA'+lcK
  laStru[lnI,2] = 'N'
  laStru[lnI,3] = 12
  laStru[lnI,4] = 2
  *C122455,6  TMI [End  ] 
ENDFOR

CREATE TABLE (gcWorkDir+lcTmpFile) FROM ARRAY laStru
INDEX ON cStyClr TAG &lcTmpFile

*- Create another temp file to hold qty's
lnI = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'cStyClr'
laStru[lnI,2] = 'C'
laStru[lnI,3] = 19
laStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'TYPE'
laStru[lnI,2] = 'C'
laStru[lnI,3] = 1
laStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'SIZENO'
laStru[lnI,2] = 'C'
laStru[lnI,3] = 1
laStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'SIZE'
laStru[lnI,2] = 'C'
laStru[lnI,3] = 5
laStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'Desc1'
laStru[lnI,2] = 'C'
laStru[lnI,3] = 20
laStru[lnI,4] = 0

FOR lnK = 1 TO lnClrCnt
  lcK = ALLTRIM(STR(lnK))  
  lnI = lnI + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'QTY'+lcK
  laStru[lnI,2] = 'N'
  laStru[lnI,3] = 7
  laStru[lnI,4] = 0
ENDFOR

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'TOTQTY'
laStru[lnI,2] = 'N'
laStru[lnI,3] = 9
laStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'PRICEA'
laStru[lnI,2] = 'N'
laStru[lnI,3] = 12
laStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'ErlDate'
laStru[lnI,2] = 'D'
laStru[lnI,3] = 8
laStru[lnI,4] = 0

*C122455,6  TMI [Start] Add a page field to the temp file
lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'PAGENO'
laStru[lnI,2] = 'N'
laStru[lnI,3] = 4
laStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'CDATE'
laStru[lnI,2] = 'C'
laStru[lnI,3] = 1
laStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStru[lnI,4]
laStru[lnI,1] = 'FLAG1'  && Add a flag to show description and priceA
laStru[lnI,2] = 'C'
laStru[lnI,3] = 1
laStru[lnI,4] = 0
*C122455,6  TMI [End  ] 

CREATE TABLE (gcWorkDir+lcTmpChld) FROM ARRAY laStru
INDEX ON cStyClr+TYPE TAG STYTYPE
*C122455,6  TMI [Start] Add PageNo field to stytype key
*INDEX ON cStyClr+TYPE+SIZENO+SIZE TAG &lcTmpChld
INDEX ON cStyClr+TYPE+SIZENO+SIZE+CDATE TAG &lcTmpChld
*C122455,6  TMI [End  ] 

*- Set the relation 
*C122455,5  TMI [Start] replace this relation with other one from the file "lcShwRepFl"
*SET RELATION TO cStyClr INTO &lcTmpFile
*C122455,5  TMI [End  ] 

*-- end of lfCrtTmp.

*:**************************************************************************
*:* Name        : lfRelation
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/08/2004
*:* Purpose     : Set relations
*:***************************************************************************
FUNCTION lfRelation

SELECT STYLE
IF !'INTO SCALE' $ SET('RELATION')
  SET RELATION TO 'S'+SCALE INTO SCALE
ENDIF
IF !'INTO POSLN' $ SET('RELATION')
  SET RELATION TO STYLE INTO POSLN
ENDIF  
SELECT POSLN
IF !'INTO POSHDR' $ SET('RELATION')
  SET RELATION TO CSTYTYPE+PO INTO POSHDR
ENDIF

*-- end of lfRelation.

*:**************************************************************************
*:* Name        : lfGtItmDat
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/07/2004
*:* Purpose     : Get item structure data
*:***************************************************************************
FUNCTION lfGtItmDat
PRIVATE laItemSeg,lnK

lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)

DIMENSION laItemSeg[1,1]
=gfItemMask(@laItemSeg)

FOR lnK = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnK,1] = 'F'
    lcSeprt = laItemSeg[lnK,6]
    
  CASE laItemSeg[lnK,1] = 'C'
    lnClrLen = LEN(laItemSeg[lnK,3])
    lnClrPos = laItemSeg[lnK,4]
    lcClrSpr = laItemSeg[lnK,6]

  CASE laItemSeg[lnK,1] = 'S'
    lnSclLen = LEN(laItemSeg[lnK,3])
    lnSclPos = laItemSeg[lnK,4]
  
  ENDCASE
ENDFOR

*-- end of lfGtItmDat.

*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : SORDCON.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
lcObj_Id = OBJLINK.cObject_ID
*-- Select general field which have company logo.
SELECT gobject;
 FROM Objects         ;
 WHERE Objects.cobject_id = lcObj_Id ;
 INTO CURSOR (lcLogoPic)
*-- end of lfGetLogo.

*!*************************************************************
*! Name      : lfsrLoc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/01/98
*! Purpose   : Rise change Location flag, in range browse screen.
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
*! Example   : =lfsrLoc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrLoc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChLoc = .T.
    GO TOP IN WAREHOUS
  CASE lcParm = 'R'
    llClearLoc = .F.
ENDCASE
*-- end of lfsrLoc.

*:**************************************************************************
*:* Name        : lfvPrnPrLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/05/2004
*:* Purpose     : Print Prince line option
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************

*C125285,1 NNA 02/06/2005 (Begin) Stop this function because there in no need to it
*FUNCTION lfvPrnPrLn
*PRIVATE lnPrnPrLn
*C125285,1 NNA (End)

*-- end of lfvPrnPrLn.

*!*************************************************************
*! Name        : lfvExprt2XL
*! Developer   : Nader Nabil (NNA)
*! Date        : 01/18/2005
*! Purpose     : Valid function for Export to Excel choice
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvExprt2XL()
*!*************************************************************
*!C125285,1
FUNCTION lfvExprt2XL
lcRunStat = IIF(llRpExport,'DISABLE','ENABLE')
SHOW GET pbRun &lcRunStat

*-- End of lfvExprt2XL.
*!*************************************************************
*! Name        : lfExprt2XL
*! Developer   : Nader Nabil (NNA)
*! Date        : 01/18/2005
*! Purpose     : Export data to Excel file
*!*************************************************************
*! Called from : ARPINVDIR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfExprt2XL()
*!*************************************************************
*!C125285,1
FUNCTION lfExprt2XL

WAIT WINDOW NOWAIT 'Please wait...'
*--copy file ICFRESTU.XLS to our new file 
lcRpPath = gcWorkDir
lcRpDefPth = gcDef_Path && Save The DefPath.

*--to be sure that it is EXCEL Name
lcRpFilNam = 'EXPORTED.XLS'

lcRpDefPth = ALLTRIM(lcRpDefPth)
lcSorCop = ALLTRIM(lcRpDefPth) +'ICFRESTU.XLS'
lcTarCop = lcRpPath + lcRpFilNam

PRIVATE lcErrStr,llError
lcErrStr = ON('ERROR')
llError = .T.
ON ERROR llError = .T.
DO WHILE llError
  llError = .F.
  COPY FILE &lcSorCop TO  &lcTarCop  
  IF llError
    WAIT WINDOW 'Error while creating the Excel file, Press ESC to exit or press any other key to retry.'
    IF LASTKEY() = 27
      RETURN
    ENDIF
  ENDIF
ENDDO  
ON ERROR &lcErrStr

lnLen = LEN(lcRpFilNam)
lcRpFilNam =LEFT(lcRpFilNam,lnLen-4)

lcTempMemo = gfTempName()

SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 
lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')

IF USED(lcShwRepFl)
  USE IN &lcShwRepFl 
ENDIF
IF USED(lcTmpFile)
  USE IN &lcTmpFile
ENDIF
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'IC\'+[ICSTUN20.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF

*C125285,1 NNA 02/06/2005 (Begin) Re_use the Temp File After Do The EXE
IF !USED(lcTmpFile)
  USE (gcWorkDir+lcTmpFile) ORDER 1 IN 0
ENDIF
*C125285,1 NNA (End)

WAIT CLEAR

*-- End of Function lfExprt2XL.
*!**************************************************************************
*! Name      : lfGetRate
*! Developer : Nader Nabil (NNA)
*! Date      : 01/18/2005
*! Purpose   : Return the exchange rate from GBP to EUR
*!********************************************************************************
*! Example   :  =lfGetRate()
*!**************************************************************************
*!C125285,1
FUNCTION lfGetRate
PARAMETER  lcFromCurr,lcToCurr
lcOldAlias = ALIAS()
IF !USED('Sycexch')
  =gfOpenFile(gcSysHome+'Sycexch','','SH')
ENDIF

SELECT Sycexch.nexrate, MAX(Sycexch.dratedate);
 FROM Sycexch;
 WHERE Sycexch.cbasecurr = lcFromCurr;
   AND Sycexch.ccurrcode = lcToCurr  ;
 INTO ARRAY laExRate
 
 SELECT (lcOldAlias)
 RETURN IIF(_TALLY >=1 ,laExRate[1,1],1)

*-- End of Function lfGetRate.
*!**************************************************************************
*! Name      : lfStyFrPrc
*! Developer : Nader Nabil (NNA)
*! Date      : 01/18/2005
*! Purpose   : Get the style foreign price
*!********************************************************************************
*! Example   : =lfStyFrPrc()
*!**************************************************************************
*!C125285,1
FUNCTION lfStyFrPrc
lnOldAlias = SELECT(0)
IF SEEK(STYLE.STYLE+'EUR','STYPRICE')
  SELECT(lnOldAlias)
  RETURN STYPRICE.PRICEA
ENDIF
SELECT(lnOldAlias)
RETURN 0
*-- End of Function lfStyFrPrc.