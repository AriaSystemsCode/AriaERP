*!**********************************************************************************************
*: Program file  : ICCAT960.PRG   (Cnverted from 2.0 to 27 for Cathy Daniels)
*: Program desc. : Custom Cut & Sold report. Done for Cathy Daniels
*:         Module: Aria Apparel Series.
*:         Date  : 11/22/2000
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*!**********************************************************************************************
*! Refer to (C102060)
*!**********************************************************************************************
*! Modifications
*!**********************************************************************************************
*! B604152,1 WAB  01/24/2001 INFINTE LOOP DURING RUNING THE CUT & SOLD REPORT            
*! B604469,1 ADEL 05/13/2001 When selecting just one style, don't for seleting seasons.
*! E301722,1 TMI  09/19/2001 Print SEASON GRAND TOTAL  in a new page.
*! B605035,1 BWA  10/22/2001 Fix the bug of the total line for one color only doesn't appear.
*! B605096,1 BWA  11/20/2001 Fix the bug of page break that fall in wrong position.
*! C200250,1 ADEL 11/29/2001 Enabled Multi-Lot feature for Cutting tickets.
*! B605298,1 BWA  24/01/2002 1) Modify the % to get the ACTUAL + PROPOSal
*! B605298,1 BWA             2) Add a total line to every color.
*! B605901,1 ASH  04/23/2002 Fix the bug of proposed qty is not correct in case of Multi Lot C/T
*! B606447,1 BWA  10/08/2002 Add new filter in the option grid for the status.
*! C102767,1 BWA  11/17/2002 Add new filter "Domestic,Imported" styles and update the program with the code.
*!**********************************************************************************************

*B604469,1 (Begin) As per AMR AND GEHAN, 'Season(s) must be entered' will be shown only
*B604469,1         if the user doesn't select any styles.
*IF AT('STYLE.SEASON',lcRpExp) = 0
IF AT('STYLE.SEASON',lcRpExp) = 0 AND AT('STYLE.CSTYMAJOR',lcRpExp) = 0
*B604469,1 (End)
  *** 'Season(s) must be entered.' ***
  *** < Ok > ***
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Season(s) must be entered.')    
  RETURN
ENDIF

*C102767,1 BWA 11/17/2002 Add new filter "Domestic,Imported" to the lcRpexp.[START]
lcRpExp = lcRpExp + " AND " + IIF(lcRPDomImp = "A" , ".T." , IIF(lcRPDomImp = "D", "STYLE.MAKE" , "!STYLE.MAKE"))
*C102767,1 BWA 11/17/2002.[END]

*--Get the Style records upon the enetred filter.
LOCATE FOR &lcRpExp
IF !FOUND()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*--Create a new DBF to hold the the 3 most recent Grand Total for each season has been previewed or Run.
IF !FILE(gcDataDir+'TMPGNDSE.DBF')
  CREATE TABLE (gcDataDir+'TMPGNDSE') (cSeason C(3),nLineNo N(6),nTot1 N(7.0),nTot2 N(7.0),nTot3 N(7.0),nTot4 N(7.0),;
               nTot5 N(7.0),nTot6 N(7.0),nTot7 N(7.0),nTot8 N(7.0),nTot9 N(7.0),nTot10 N(7.0),;
               dRunDate D, cRunTime C(8))
  INDEX ON cSeason+STR(nLineNo,6) TAG TMPGNDSE
  *USE
  SELECT STYLE
ELSE
  =gfOpenFile(gcDataDir+'TmpGndSe',gcDataDir+'TmpGndSe','SH')
ENDIF

*C200250,1 (Begin) Open Manifucturing Operation detail file.
=gfOpenFile(gcDataDir+'mfgoprdt',gcDataDir+'mfgoprdt','SH')
*C200250,1 (End)



*----Report variables.
lnMajorLen = LEN(gfItemMask('PM'))
lnMaxRow  = 46                 && up to 62 lines per page
lcRpTitle = 'ICCAT960'
R_TITLE   = 'CUT & SOLD REPORT'
R_WIDTH   = 'W'                            && Standard Report is Wide
XGROUP    = SPACE(2)
XCOLOR    = SPACE(3)
XCC       = 0                              && Counter for Colors by Style
lnOldMemo = 0
lcLine    = ' '
lcStyle   = SPACE(7)
DIMENSION XDIVISION(6),XSEA(5),BREAK(3),HBREAK(3)
XDIVISION = '  '
XSEA      = SPACE(3)
XTITLE    = SPACE(40)
DECLARE laSeaLName[1,2]
laSeaLName[1,1] = 'SEALNAME'      && Array to get the Season long name
laSeaLName[1,2] = 'lcSeaLName'
lcSeaLName = ''
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*--This user style structure is 'XXX-XXXXXXXX-XXXXXX'
*                                Sea   Style  Color
lnSeaLen = LEN(laMajSegs[1,3])
lnStyLen = LEN(laMajSegs[2,3])

*B605035,1 BWA 10/22/2001 Fix the bug of the total line for one color only doesn't appear.[START]
lcStyBrk  = SPACE(lnStyLen)
llBreak   = .F.
*B605035,1 BWA 10/22/2001.[END]

****** Finding this week range ******
X=DOW(DATE())-1
IF X=0
   X=7
ENDIF
XDATEA=DATE()+1-X       && Monday's date
XDATEB=DATE()+7-X       && Sunday's date
****** Files Selection ******
SELECT ORDLINE
IF EMPTY(SET('RELATION'))
  SET RELATION TO cordtype+ORDER INTO ORDHDR
ENDIF
SELECT CUTTKTL
IF EMPTY(SET('RELATION'))
  SET RELATION TO CUTTKT INTO CUTTKTH
ENDIF

*C102767,1 BWA 11/17/2002 Make relation from posln to poshdr.[START]
SELECT POSLN
IF EMPTY(SET('RELATION'))
  SET RELATION TO "P" +  POSLN.PO INTO POSHDR
ENDIF
*C102767,1 BWA 11/17/2002.[END]

****** Prepare Subtotals Break ******
** the order of the breaks is important please avoid altering it **
IF llOgFltCh
  IF USED(lcTmpName)
    USE IN (lcTmpName)
  ENDIF
  SELECT STYLE
  COPY REST TO (gcWorkDir+lcTmpName) FOR &lcRpExp
  =gfOpenFile(gcWorkDir+lcTmpName,'','EX')
  INDEX ON SEASON+cDIVISION+cstyGROUP+STYLE TAG (lcTmpName)
ENDIF
*--Sub totals
BREAK(1) = 'SUBSTR(cDIVISION,1,1)+cStyGroup+SUBSTR(STYLE,lnSeaLen+2,lnStyLen)'
BREAK(2) = 'SUBSTR(cDIVISION,1,1)+cStyGroup'
BREAK(3) = 'SUBSTR(cDIVISION,1,1)'

**********************************************************
****** SELECT REPORT FILE & INITIALIZE MEMORY BLOCK ******
**********************************************************
*** setting to print
SELECT (lcTmpName)
IF EMPTY(SET('RELATION'))
  SET RELATION TO 'S'+SCALE INTO SCALE
ENDIF  
SET DEVICE TO PRINT
LOCATE
FOR X=1 TO 3
  HBREAK(X) = &BREAK(X)
ENDFOR
SCAN WHILE INKEY() <> 32
  *B604152,1 (WAB) change the meesage to be continues message
  *WAIT WINDOW 'Report Printing - <SPACE BAR> to abort' NOWAIT
  WAIT WINDOW 'Collecting data .Please wait ....' NOWAIT
  *B604152,1 (WAB)- END
  lcSeaCode = Season
  lcMajor   = Style
  lcSeaDesc = SUBSTR(gfCodDes(lcSeaCode , 'SEASON'),1,15)
  *B604152,1 (WAB) - as per convert prog they put the related field for season into CATHCODE file
  *=gfRltFld(lcSeaCode , @laSeaLName , 'SEASON')
  SELECT CATHCODE
  IF SEEK('Y'+PADR('SEASON',10)+PADR(lcSeaCode,6)+PADR('CSEALNAME',10))
    lcSeaLName = crltd_vlu
  ENDIF
  SELECT (lcTmpName)
  *B604152,1 (WAB) - END
   XTITLE ='Season '+TRIM(lcSeaCode)+'   '+TRIM(lcSeaDesc)+'   '+TRIM(lcSeaLName)
  *FOR lnNoOfSea = 1 TO ALEN(laSeason,1)
  *0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
  *ESTY960                                                        CUT & SOLD REPORT
  *MM/DD/YY                                   Season XX  XXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  *_____________________________________________________________________________________________________________________________________
  *XXXXXXX   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  *(style)   (description-> SDESC1+SDESC2+SDESC3)
  *                          ³----- SALES ------º+++ PRODUCTION +++º----- RECEIVING -----º+++++ SHIPPING ++++++º----- OPEN TO SELL ----
  *COLOR         SIZE³   PLAN³  WEEK³TO DATE³ % ºPROPOS³ ACTUAL³ % ºTRNSIT³ REC1st³REC2ndº  SHIPD³ONHAND³UNSHIPº  STOCK³CUT/PRO³   PLAN
  *                  ³       ³      ³       ³   º      ³       ³   º      ³       ³      º       ³      ³      º       ³       ³
  *XXXXXXXXXXXXXX XXX³1234567³123456³1234567³.99º123456³1234567³.99º123456³1234567³123456º1234567³123456³123456º1234567³1234567³1234567
  *                  ³-------³------³-------³   º------³-------³   º------³-------³------º-------³------³------º-------³-------³-------
  *                  ³1234567³123456³1234567³   º123456³1234567³   º123456³1234567³123456º1234567³123456³123456º1234567³1234567³1234567
  *====================================================================================================================================
  *XXXXXXXXXXX TOTAL ³1234567³123456³1234567³   º123456³1234567³   º123456³1234567³123456º1234567³123456³123456º1234567³1234567³1234567
  *====================================================================================================================================
  *************************************************************************************************************************************
  *************************************************************************************************************************************
  *SEASON GRAND TOTAL³1234567³123456³1234567³   º123456³1234567³   º123456³1234567³123456º1234567³123456³123456º123456³1234567³1234567
  *************************************************************************************************************************************
  *************************************************************************************************************************************
  *0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..

  ****** Variables Initialization ******
  ROW       = 99
  PAGENO    = 0
  lcLine    = ''
  DIMENSION XVAR(9,13),XSUBTOT(3,13),XTOTAL(13)
  Y       = 0
  XCNT    = 0
  XVAR    = 0
  XSUBTOT = 0
  XTOTAL  = 0

  **  XVAR(X,Y)
  **  X     Y: 1 to 13 (columns)
  **  1     ÃÄÄÅÄÄÅ
  **  to    ÃÄÄÅÄÄÅ
  **  9     ÃÄÄÅÄÄÅ...
  ** (rows for each size and a total)
  ****** Begin Printing ******
  SCAN WHILE SEASON+cDIVISION+cstyGROUP+STYLE = lcSeaCode
    ****** Subtotals break assignment ******
    *B604152,1 (WAB) - ADD CONTINUES MESSAGE 
    WAIT WINDOW 'Collecting data for Style # : '+Style NOWAIT
    *B604152,1 (WAB) END.
    lcMajor   = Style
    IF ROW >= (lnMaxRow+10)
       PAGENO=PAGENO+1
       DO RPT_HDR WITH lcRpTitle,XTITLE,R_WIDTH
       ROW=04
    ENDIF
    ******---- Begin Subtotals ----******
    X=0
    FOR Y=1 TO 3
      IF &BREAK(Y)<>HBREAK(Y)
        X=Y
      ENDIF
    ENDFOR
    XM=X
    IF XM>0
      FOR X=1 TO XM
        ****** Control for Stop Printing Subtotals by Style ******
        IF X=1 .AND. XCC<2   && if break by style and not more than one color
          XCC=0
          FOR Y=1 TO 13     && then subtotals by style are reset
              STORE 0 TO XSUBTOT(X,Y)
          ENDFOR
          HBREAK(X)=&BREAK(X)   && and no subtotal by style is printed
        ****** SUBTOTALS PRINT ******
        ELSE
        ****** Change of subtotals divisory line
          IF X=1
             XLINE='Í'      && ÍÍÍÍÍÍ
          ELSE
             IF X=2

               *B605096,1 BWA 11/20/2001 Fix the bug of page break that fall in wrong position.[START]
               *ROW=ROW+3   && skips 3 lines before group subtotals
               *B605096,1 BWA 11/20/2001.[END]

               XLINE='='   && ======
             ELSE
                XLINE='*'   && ******
             ENDIF
          ENDIF
          ROW=ROW+1
          @ ROW,00 SAY REPLICATE(XLINE,132)
          ****** Printing
          ROW=ROW+1
          XCC=0
          IF X=1   && by style
            @ ROW,000 SAY SUBSTR(HBREAK(X),8,8)  && if HBREAK(3)=SUBSTR(DIVISION,1,1)
          ELSE
             IF X=2   && by group
               lcGrpDesc =  SUBSTR(gfCodDes(SUBSTR(HBREAK(X),2,6) , 'CSTYGROUP'),1,15)
               IF !EMPTY(lcGrpDesc)
                  @ ROW,000 SAY SUBSTR(lcGrpDesc,1,11)
               ENDIF
             ELSE     && by division
               DO CASE                && if HBREAK(3)=SUBSTR(DIVISION,1,1)
                  CASE HBREAK(X)='P'
                     @ ROW,000 SAY 'PETITE'
                  CASE HBREAK(X)='L'
                     @ ROW,000 SAY 'LARGE'
                  CASE HBREAK(X)='M'
                     @ ROW,000 SAY 'MISSY'
                  OTHERWISE
                     @ ROW,000 SAY 'OTHER'
               ENDCASE

             ENDIF
             SELECT (lcTmpName)
          ENDIF
          @ ROW,012 SAY 'TOTAL   ³'
          @ ROW,020 SAY XSUBTOT(X,1) PICTURE '9999999³'
          @ ROW,029 SAY XSUBTOT(X,2) PICTURE '999999³'
          @ ROW,036 SAY XSUBTOT(X,3) PICTURE '9999999³'
          @ ROW,047 SAY XSUBTOT(X,4) PICTURE 'º999999³'
          @ ROW,055 SAY XSUBTOT(X,5) PICTURE '9999999³'
          @ ROW,066 SAY XSUBTOT(X,6) PICTURE 'º999999³'
          @ ROW,074 SAY XSUBTOT(X,7) PICTURE '9999999³'
          @ ROW,082 SAY XSUBTOT(X,8) PICTURE '999999º'
          @ ROW,089 SAY XSUBTOT(X,9) PICTURE '9999999³'
          @ ROW,097 SAY XSUBTOT(X,10) PICTURE '999999³'
          @ ROW,104 SAY XSUBTOT(X,3)-XSUBTOT(X,9) PICTURE '999999º'
          *---Say the new column on 119 not 111
          @ ROW,114 SAY XSUBTOT(X,1)-XSUBTOT(X,3)-XSUBTOT(X,8) PICTURE '9999999999'
          ROW=ROW+1
          @ ROW,00 SAY REPLICATE(XLINE,132)
          IF X>1     && skips 3 lines after group and division subtotals

            *B605096,1 BWA 11/20/2001 Fix the bug of page break that fall in wrong position.[START]
            *ROW=ROW+3
            *B605096,1 BWA 11/20/2001.[END]

          ENDIF
          FOR Y=1 TO 13
              STORE 0 TO XSUBTOT(X,Y)
          ENDFOR
          HBREAK(X)=&BREAK(X)
       ENDIF
       ****** change of page before next subtotal
       IF ROW >= (lnMaxRow+10)
         PAGENO=PAGENO+1
         DO RPT_HDR WITH lcRpTitle ,XTITLE,R_WIDTH
         ROW=04
         IF X=2 .AND. XM=3  && 3 spaces before division totals in new page
            ROW=07
         ENDIF
       ENDIF
     ENDFOR
   ENDIF  
   IF ROW >= lnMaxRow    && change of page before a block (line)
     PAGENO=PAGENO+1
     DO RPT_HDR WITH lcRpTitle ,XTITLE,R_WIDTH
     ROW=04
     IF X=2 .AND. XM=3  && 3 spaces before division totals in new page
       ROW=07
     ENDIF
   ENDIF
   ******---- PERFORM CALCULATIONS & PRINTS ----******
   XCNT = SCALE.CNT
   IF XCNT>0
      ****** PLAN is the 1st column
      ****** SALES TO DATE is the 3rd column
      ****** SHIPPED is the 9th column
      ****** ONHAND is the 10th column
      ****** OPEN TO SELL STOCK is the 11th column
      ******              CUT/PROP is the 12th column
      ******              PLAN is the 13th column
      FOR X=1 TO XCNT
        Z=STR(X,1)
        XVAR(X,1)=PLAN&Z
        XVAR(X,3)=ORD&Z+SHP&Z
        XVAR(X,9)=SHP&Z
        XVAR(X,10)=STK&Z
        XVAR(X,11)=STK&Z-ORD&Z
      ENDFOR
      XVAR(XCNT+1,1)=TOTPLAN
      XVAR(XCNT+1,3)=TOTORD+TOTSHP
      XVAR(XCNT+1,9)=TOTSHP
      XVAR(XCNT+1,10)=TOTSTK
      XVAR(XCNT+1,11)=TOTSTK-TOTORD
      ******
      XCOLOR = SUBSTR(STYLE,lnMajorLen+2)
      ****** SALES THIS WK is the 2nd column
      SELECT ORDLINE
      =SEEK(lcMajor)
      SCAN WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcMajor 
        IF BETWEEN(ORDHDR.ENTERED,XDATEA,XDATEB) .AND. ORDHDR.STATUS$'OH'
           FOR X=1 TO XCNT
             Z=STR(X,1)
             XVAR(X,2)=XVAR(X,2)+QTY&Z
           ENDFOR
           XVAR(XCNT+1,2)=XVAR(XCNT+1,2)+TOTQTY
        ENDIF
      ENDSCAN

      ****** columns 4th,5th,6th,7th,8th
      **********************************
      *C102767,1 BWA 11/17/2002 Check if the style is Domestic OR all cases.[START]
      IF lcRPDomImp $ "AD"

        SELECT CUTTKTL
        =SEEK(lcMajor)

        SCAN WHILE style+cuttkt+trancd = lcMajor
          DO CASE
             CASE TRANCD='1'
             ****** PRODUCTION PROPOSED + ACTUAL are the 4th & 5th columns
               *C200250,1 (Begin) Check the actualized lots.
               llAct = .F.             
               IF CUTTKTH.cMultiLot = 'M' AND !INLIST(CUTTKTH.STATUS,'A','C')
                 IF SEEK(CUTTKTH.cMultiLot+CUTTKTH.CUTTKT,'MfgOprDt')
                   SELECT MfgOprDt
                   LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = CUTTKTH.CMULTILOT+CUTTKTH.CUTTKT;
                        FOR  ITEM = CUTTKTL.STYLE AND NACTQTY <> 0
                   llAct = FOUND()
                 ENDIF
               ENDIF
               *-- If there's any lot acualized.
               IF llAct
                 =SEEK('M'+CUTTKTH.CUTTKT)
                 DIMENSION laProp[8],laAct[8]
                 STORE 0 TO laProp[XCNT+1],laAct[XCNT+1],laProp,laAct
                 *B605901,1 ASH 04/23/2002 (Begin) Get the proposed qty from cuttktl file for trancd '1'.
                 FOR X=1 TO XCNT
                   I = STR(X,1)
                   laProp[X] = CUTTKTL.QTY&I
                 ENDFOR
                 laProp[XCNT+1] = CUTTKTL.TOTQTY
                 ** Scan only for Type = '2'
                 *SCAN REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = 'M'+CUTTKTH.CUTTKT;
                      FOR  ITEM = CUTTKTL.STYLE
                 SCAN REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = 'M'+CUTTKTH.CUTTKT;
                      FOR  ITEM = CUTTKTL.STYLE AND TRANCD = '2' AND !EMPTY(ctrgopr)
                 *B605901,1 ASH 04/23/2002 (End)
                    FOR X=1 TO XCNT
                      Z=STR(X,1)
                      *B605901,1 ASH 04/23/2002 (Begin) Remove the if condition as we added this condition in the scan for.
                      *IF TRANCD = '2'
                      *B605901,1 ASH 04/23/2002 (End)
                        laAct[X]  = laAct[X]  + nLotQty&Z
                        *--Extract 2*nLotQty&Z for this line budget and the other line put for the next operation.
                        *B605901,1 ASH 04/23/2002 (Begin) Comment these lines to recompute the proposed qty based on the C/T line file.
                        *laProp[X] = laProp[X] - 2*nLotQty&Z
                      *ELSE
                        *laProp[X] = laProp[X] + IIF(TRANCD <> '4',nLotQty&Z,-nLotQty&Z)
                        *laProp[X] = laProp[X] + IIF(TRANCD = '1',nLotQty&Z,-nLotQty&Z)
                      *ENDIF
                      *B605901,1 ASH 04/23/2002 (End)
                    ENDFOR
                    *B605901,1 ASH 04/23/2002 (Begin) Comment these lines to recompute the proposed qty based on the C/T line file.
                    *IF TRANCD = '2'
                    *  laProp[XCNT+1] = laProp[XCNT+1] + -2*nLotTOTQTY
                    *ELSE
                    *  laProp[XCNT+1] = laProp[XCNT+1] + IIF(TRANCD = '1',nLotTOTQTY,-nLotTOTQTY)
                    *ENDIF
                    *B605901,1 ASH 04/23/2002 (End)
                    laAct[XCNT+1]  = laAct[XCNT+1]+ IIF(TRANCD = '2',nLotTOTQTY,0)
                 ENDSCAN
                 *B605901,1 ASH 04/23/2002 (Begin) Recompute the proposed qty by subtracting the actualize qty from the budget from C/T line file.
                 FOR X=1 TO XCNT
                   laProp[X] = laProp[X] - laAct[X]
                 ENDFOR
                 laProp[XCNT+1] = laProp[XCNT+1] - laAct[XCNT+1]
                 *B605901,1 ASH 04/23/2002 (End)
                 FOR X=1 TO XCNT
                    Z=STR(X,1)
                    XVAR(X,4)=XVAR(X,4)+MAX(laProp[X],0)
                    XVAR(X,5)=XVAR(X,5)+MAX(laAct[X],0)
                 ENDFOR
                 XVAR(XCNT+1,4)=XVAR(XCNT+1,4)+MAX(laProp[XCNT+1],0)
                 XVAR(XCNT+1,5)=XVAR(XCNT+1,5)+MAX(laAct[XCNT+1],0)
               ELSE
                 SELECT CUTTKTL
               *C200250,1 (End)
                 IF CUTTKTH.STATUS$'OH'
                    FOR X=1 TO XCNT
                      Z=STR(X,1)
                      XVAR(X,4)=XVAR(X,4)+QTY&Z
                    ENDFOR
                    XVAR(XCNT+1,4)=XVAR(XCNT+1,4)+TOTQTY
                  ENDIF
                  IF CUTTKTH.STATUS$'AC'
                     FOR X=1 TO XCNT
                       Z=STR(X,1)
                       XVAR(X,5)=XVAR(X,5)+QTY&Z
                     ENDFOR
                     XVAR(XCNT+1,5)=XVAR(XCNT+1,5)+TOTQTY
                  ENDIF
               *C200250,1 (Begin)  Check the actualized lots.
               ENDIF
               *C200250,1 (End)
             CASE TRANCD='2'
             ****** RECEIVING REC1st is the 7th column
                FOR X=1 TO XCNT
                    Z=STR(X,1)
                    XVAR(X,7)=XVAR(X,7)+QTY&Z
                ENDFOR
                XVAR(XCNT+1,7)=XVAR(XCNT+1,7)+TOTQTY

             CASE TRANCD='3'
             ****** RECEIVING REC2nd is the 8th column
                FOR X=1 TO XCNT
                    Z=STR(X,1)
                    XVAR(X,8)=XVAR(X,8)+QTY&Z
                ENDFOR
                XVAR(XCNT+1,8)=XVAR(XCNT+1,8)+TOTQTY

             CASE (TRANCD='5' .OR. TRANCD='6')
             ****** RECEIVING TRNSIT is the 6th column
                FOR X=1 TO XCNT
                    Z=STR(X,1)
                    XVAR(X,6)=XVAR(X,6)+QTY&Z
                ENDFOR
                XVAR(XCNT+1,6)=XVAR(XCNT+1,6)+TOTQTY
          ENDCASE
        ENDSCAN
        SELECT (lcTmpName)
        FOR X=1 TO XCNT
          XVAR(X,12)=(XVAR(X,4)+XVAR(X,5))-XVAR(X,3)
          XVAR(X,13)=XVAR(X,1)-XVAR(X,3)
        ENDFOR
        XVAR(XCNT+1,12)=(XVAR(XCNT+1,4)+XVAR(XCNT+1,5))-XVAR(XCNT+1,3)
        XVAR(XCNT+1,13)=XVAR(XCNT+1,1)-XVAR(XCNT+1,3)

      *C102767,1 BWA 11/17/2002 Add the "ENDIF".[START]
      ENDIF
      *C102767,1 BWA 11/17/2002.[END]

      *C102767,1 BWA 11/17/2002 Add the code of the new filter "Domestic,Imported".[START]
      *C102767,1 BWA 11/17/2002 Case Imported styles and case both selection.
      ****** columns 4th,5th,6th,7th,8th
      **********************************
      IF lcRPDomImp $ "AI"

        SELECT POSLN
        =SEEK(lcMajor)

        DIMENSION laProp[8],laAct[8]
        STORE 0 TO laProp[XCNT+1] , laAct[XCNT+1] , laProp , laAct

        SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = lcMajor 
          IF CSTYTYPE = "P" AND POSHDR.STATUS $ "OHA"
            DO CASE
              *-- PROPOSED + ACTUAL are the 4th & 5th columns
              CASE TRANCD = '1'  &&Creat PO

                FOR X = 1 TO XCNT
                  I = STR(X,1)
                  laProp[X] = POSLN.QTY&I
                  XVAR(X,4) = XVAR(X,4) + POSLN.QTY&I
                ENDFOR
                laProp[XCNT+1] = POSLN.TOTQTY
                XVAR(XCNT+1,4) = XVAR(XCNT+1,4) + MAX(laProp[XCNT+1],0)

              *-- TRNSIT + RECEIVING REC1st is the 6th & 7th column
              CASE TRANCD = '2'

                FOR X = 1 TO XCNT
                  Z = STR(X,1)
                  IF EMPTY(POSLN.SHIPNO)
                    XVAR(X,6) = XVAR(X,6) - POSLN.QTY&Z
                  ENDIF
                  XVAR(X,7) = XVAR(X,7) + POSLN.QTY&Z
                ENDFOR

                IF EMPTY(POSLN.SHIPNO)
                  XVAR(XCNT+1,6) = XVAR(XCNT+1,6) - POSLN.TOTQTY
                ENDIF
                XVAR(XCNT+1,7) = XVAR(XCNT+1,7) + POSLN.TOTQTY

                IF !EMPTY(POSLN.SHIPNO)
                  FOR X = 1 TO XCNT
                    I = STR(X,1)
                    XVAR(X,4) = XVAR(X,4) - POSLN.QTY&I
                  ENDFOR
                  XVAR(XCNT+1,4) = XVAR(XCNT+1,4) - POSLN.TOTQTY
                ENDIF

              *-- PROPOSED + ACTUAL + TRNSIT are the 4th & 5th & 6th columns
              CASE TRANCD = '3'  &&Shipment of the PO.

                FOR X = 1 TO XCNT
                  I = STR(X,1)
                  laProp[X] = POSLN.QTY&I
                  XVAR(X,6) = XVAR(X,6) + POSLN.QTY&I
                ENDFOR

                laProp[XCNT+1] = POSLN.TOTQTY
                XVAR(XCNT+1,6) = XVAR(XCNT+1,6) + POSLN.TOTQTY

                FOR X = 1 TO XCNT
                  Z = STR(X,1)
                  XVAR(X,4) = XVAR(X,4) - laProp[X]
                ENDFOR
                XVAR(XCNT+1,4) = XVAR(XCNT+1,4) - laProp[XCNT+1]

               *-- TRNSIT + RECEIVING REC2nd is the 6th & 8th column
               CASE TRANCD = '4'

                  FOR X = 1 TO XCNT
                      Z = STR(X,1)
                      IF EMPTY(POSLN.SHIPNO)
                        XVAR(X,6) = XVAR(X,6) - POSLN.QTY&Z
                      ENDIF
                      XVAR(X,8) = XVAR(X,8) + POSLN.QTY&Z
                  ENDFOR

                  IF EMPTY(POSLN.SHIPNO)
                    XVAR(XCNT+1,6) = XVAR(XCNT+1,6) - POSLN.TOTQTY
                  ENDIF
                  XVAR(XCNT+1,8) = XVAR(XCNT+1,8) + POSLN.TOTQTY

                 IF !EMPTY(POSLN.SHIPNO)
                   FOR X = 1 TO XCNT
                     I = STR(X,1)
                     XVAR(X,4) = XVAR(X,4) - POSLN.QTY&I
                   ENDFOR
                   XVAR(XCNT+1,4) = XVAR(XCNT+1,4) - POSLN.TOTQTY
                 ENDIF

            ENDCASE
          ENDIF
        ENDSCAN

        XVAR(XCNT+1,6) = 0
        XVAR(XCNT+1,4) = 0
        FOR X = 1 TO XCNT
          XVAR(X,6) = MAX(XVAR(X,6) , 0)
          XVAR(XCNT+1,6) = XVAR(XCNT+1,6) + XVAR(X,6)

          *--Section of the Propos to check in case if there is over receive.
          XVAR(X,4) = MAX(XVAR(X,4) , 0)
          XVAR(XCNT+1,4) = XVAR(XCNT+1,4) + XVAR(X,4)
        ENDFOR

        FOR X = 1 TO XCNT
          XVAR(X,5) = MAX(XVAR(X,6),0) + XVAR(X,7) + XVAR(X,8)
          XVAR(XCNT+1,5) = XVAR(XCNT+1,5) + XVAR(X,5)
        ENDFOR

        SELECT (lcTmpName)
        FOR X = 1 TO XCNT
          XVAR(X,12) = (XVAR(X,4) + XVAR(X,5)) - XVAR(X,3)
          XVAR(X,13) = XVAR(X,1) - XVAR(X,3)
        ENDFOR
        XVAR(XCNT+1,12) = (XVAR(XCNT+1,4) + XVAR(XCNT+1,5)) - XVAR(XCNT+1,3)
        XVAR(XCNT+1,13) = XVAR(XCNT+1,1) - XVAR(XCNT+1,3)
      ENDIF
      *C102767,1 BWA 11/17/2002.[END]

      ****** printing each line
      ROW=ROW+2   &&  1 empty line before block

      IF STYLE<>lcStyle
        lcLine    = ''
      ENDIF
      *B604152,1 (WAB) START -if YOU FILL THE MEMO FILED WITH EMPTY CHARACTER (JUST ONE SPACE )
      *B604152,1 (WAB)        the function EMPTY() return .F. 
      *B604152,1 (WAB)        so i ASSIGN THE MEMO FIELD VALUE TO AN CHARCTER FILED
      *IF !EMPTY(MSALESDESC)
      lcSalesDsc = MSALESDESC
      IF !EMPTY(lcSalesDsc)
      *B604152,1 (WAB) -END
        lnRow=1
        DO WHILE EMPTY(MLINE(MSALESDESC,lnRow))
          lnRow=lnRow+1
        ENDDO
        lnOldMemo = SET ('MEMOWIDTH')
        SET MEMOWIDTH TO 120
        lnMemoLnNo = lnRow
        lnMemolins = MEMLINES(MSALESDESC)
        llFirst   = .T.
        IF lnMemolins > 0
          DO WHILE lnMemoLnNo <= lnMemolins .AND. INKEY() <> 32
            lcLine = IIF(!EMPTY(lcLine),SUBSTR(lcLine,121),lcLine)
            DO WHILE LEN(lcLine) < 120 .AND. lnMemoLnNo<=MEMLINES(MSALESDESC)
              lcLine   = lcLine+ ALLTRIM(MLINE(MSALESDESC,lnMemoLnNo))
              lnMemoLnNo = lnMemoLnNo + 1
            ENDDO
            IF ROW >= (lnMaxRow+10)
              PAGENO=PAGENO+1
              DO RPT_HDR WITH lcRpTitle,XTITLE,R_WIDTH
              ROW=06
            ENDIF
            IF llFirst
              llFirst = .F.
              @ ROW,000 SAY SUBSTR(STYLE,lnSeaLen+2,lnStyLen)+'  '+SUBSTR(lcLine,1,120)
              ROW = ROW + 1
            ELSE
              @ ROW,10 SAY SUBSTR(lcLine,1,120)
              ROW = ROW + 1
            ENDIF
          ENDDO
        ELSE
          @ ROW,000 SAY SUBSTR(STYLE,lnSeaLen+2,lnStyLen)
        ENDIF
      ELSE
        @ ROW,000 SAY SUBSTR(STYLE,lnSeaLen+2,lnStyLen)  
      ENDIF  
      ROW=ROW+1
      @ ROW,00 SAY '                                                                                                                     NET PLAN       '
      ROW=ROW+1         
      @ ROW,00 SAY '                            ³----- SALES ------º+++ PRODUCTION +++º----- RECEIVING -----º+++++ SHIPPING ++++++º--- OPEN TO SELL ----'
      ROW=ROW+1
      @ ROW,00 SAY lcColorTlt+SPACE(11)+'SIZE³   PLAN³  WEEK³TO DATE³ % ºPROPOS³ ACTUAL³ % ºTRNSIT³ REC1st³REC2ndº  SHIPD³ONHAND³UNSHIPº                     '
      ROW=ROW+1
      @ ROW,00 SAY '                    ³       ³      ³       ³   º      ³       ³   º      ³       ³      º       ³      ³      º               '
      FOR X=1 TO (XCNT+1)
        ROW=ROW+1
        IF X=1      && prints color description
          lcColorDesc = gfCodDes(SUBSTR(STYLE,lnMajorLen+2) , 'COLOR')
          IF !EMPTY(lcColorDesc)
            @ ROW,000 SAY SUBSTR(lcColorDesc,1,14)
          ENDIF
        ENDIF
        IF X<>(XCNT+1)
          Z=STR(X,1)
          @ ROW,015 SAY SCALE.SZ&Z
        ENDIF
        @ ROW,020 SAY XVAR(X,1) PICTURE '³9999999³'
        @ ROW,029 SAY XVAR(X,2) PICTURE '999999³'
        @ ROW,036 SAY XVAR(X,3) PICTURE '9999999³'
        IF X<>(XCNT+1) .AND. XVAR(XCNT+1,3)<>0
          @ ROW,044 SAY ROUND(XVAR(X,3)/XVAR(XCNT+1,3),2) PICTURE '.99'
        ENDIF
        @ ROW,47 SAY XVAR(X,4) PICTURE 'º999999³'
        @ ROW,055 SAY XVAR(X,5) PICTURE '9999999³'

        *B605298,1 BWA 24/01/2002 1) Modify the % to get the ACTUAL + PROPOSal.[START]
        *IF X<>(XCNT+1) .AND. XVAR(XCNT+1,4)<>0
        *  @ ROW,063 SAY ROUND(XVAR(X,4)/XVAR(XCNT+1,4),2) PICTURE '.99'
        *ENDIF
        IF X<>(XCNT+1) .AND. ( XVAR(XCNT+1,4)<>0 OR XVAR(XCNT+1,5)<>0 )
          @ ROW,063 SAY ROUND((XVAR(X,4)+XVAR(X,5))/(XVAR(XCNT+1,4)+XVAR(XCNT+1,5)),2) PICTURE '.99'
        ENDIF
        *B605298,1 BWA 24/01/2002.[END]

        @ ROW,066 SAY XVAR(X,6) PICTURE 'º999999³'
        @ ROW,074 SAY XVAR(X,7) PICTURE '9999999³'
        @ ROW,082 SAY XVAR(X,8) PICTURE '999999º'
        @ ROW,089 SAY XVAR(X,9) PICTURE '9999999³'
        @ ROW,097 SAY XVAR(X,10) PICTURE '999999³'
        @ ROW,104 SAY XVAR(X,3)-XVAR(X,9) PICTURE '999999º'
        *---Say the new column on 119 not 111
        @ ROW,114 SAY XVAR(X,1)-XVAR(X,3)-XVAR(X,8) PICTURE '9999999999'
        IF X=XCNT
           ROW=ROW+1
           @ ROW,00 SAY '                    ³-------³------³-------³   º------³-------³   º------³-------³------º-------³------³------º---------------------'
        ENDIF
      ENDFOR
      ROW=ROW+2                        && 1 empty line before double line
      @ ROW,00 SAY REPLICATE('Í',132)

      ****** storing subtotals
     *B605298,1 BWA 24/01/2002 Add a total line to every color.[START]
     DIMENSION XTotClr(3,13)
     XTotClr = 0
     *B605298,1 BWA 24/01/2002.[END]

      FOR X=1 TO 3
        FOR Y=1 TO 13
          XSUBTOT(X,Y) = XSUBTOT(X,Y) + XVAR(XCNT+1,Y)

          *B605298,1 BWA 24/01/2002 Add a total line to every color.[START]
          XTotClr(X,Y) = XTotClr(X,Y) + XVAR(XCNT+1,Y)
          *B605298,1 BWA 24/01/2002.[END]

        ENDFOR
      ENDFOR

      *B605298,1 BWA 24/01/2002 Add a total line to every color.[START]
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('Í',132)
      ROW = ROW + 1

      @ ROW,000 SAY LEFT(lcColorDesc,14)
      @ ROW,012 SAY 'TOTAL '
      @ ROW,020 SAY XTotClr(1,1)  PICTURE '³9999999³'
      @ ROW,029 SAY XTotClr(1,2)  PICTURE '999999³'
      @ ROW,036 SAY XTotClr(1,3)  PICTURE '9999999³'
      @ ROW,047 SAY XTotClr(1,4)  PICTURE 'º999999³'
      @ ROW,055 SAY XTotClr(1,5)  PICTURE '9999999³'
      @ ROW,066 SAY XTotClr(1,6)  PICTURE 'º999999³'
      @ ROW,074 SAY XTotClr(1,7)  PICTURE '9999999³'
      @ ROW,082 SAY XTotClr(1,8)  PICTURE '999999º'
      @ ROW,089 SAY XTotClr(1,9)  PICTURE '9999999³'
      @ ROW,097 SAY XTotClr(1,10) PICTURE '999999³'
      @ ROW,104 SAY XTotClr(1,3) - XTotClr(1,9) PICTURE '999999º'
      @ ROW,114 SAY XTotClr(1,1) - XTotClr(1,3) - XTotClr(1,8) PICTURE '9999999999'

      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('Í',132)
      *B605298,1 BWA 24/01/2002.[END]

      ****** storing totals
      FOR Y=1 TO 13
        XTOTAL(Y)=XTOTAL(Y)+XVAR(XCNT+1,Y)
      ENDFOR
    ENDIF
    ******---- END CALCULATIONS & PRINT ----******

    ****** skip to next record
    SELECT (lcTmpName)

    *B605035,1 BWA 10/22/2001 Fix the bug of the total line for one color only doesn't appear.[START]
    lcStyBrk = SUBSTR(STYLE,lnSeaLen+2,lnStyLen)
    *B605035,1 BWA 10/22/2001.[END]

    SKIP

    *B605035,1 BWA 10/22/2001 Fix the bug of the total line for one color only doesn't appear.[START]
    IF lcStyBrk # SUBSTR(STYLE,lnSeaLen+2,lnStyLen)
      llBreak = .T.
    ENDIF
    *B605035,1 BWA 10/22/2001.[END]

    ****** increments counter of colors by style
    *B605298,1 BWA 24/01/2002 Add a total line to every color.[START]
    *IF XCOLOR<>SUBSTR(STYLE,lnMajorLen+2)
    IF XCOLOR<>SUBSTR(STYLE,lnMajorLen+2)  AND ( lcStyBrk = SUBSTR(STYLE,lnSeaLen+2,lnStyLen) )
    *B605298,1 BWA 24/01/2002.[END]

       XCC = XCC + 1
    ENDIF

    *B605035,1 BWA 10/22/2001 Fix the bug of the total line for one color only doesn't appear.[START]

    *B605298,1 BWA 24/01/2002 Add a total line to every color.[START]
    *IF llBreak AND !EOF(lcTmpName) AND XCC = 0 
    IF (llBreak AND !EOF(lcTmpName) AND XCC = 0  ) OR ( XCC = 1  AND (lcStyBrk # SUBSTR(STYLE,lnSeaLen+2,lnStyLen))  )
    *B605298,1 BWA 24/01/2002.[END]

      llBreak = .F.
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('Í',132)
      ROW = ROW + 1

      @ ROW,000 SAY lcStyBrk
      @ ROW,012 SAY 'TOTAL   '

      *B605298,1 BWA 24/01/2002 Add a total line to every color.[START]
      *@ ROW,020 SAY XVAR(XCNT+1,1) PICTURE '³9999999³'
      *@ ROW,029 SAY XVAR(XCNT+1,2) PICTURE '999999³'
      *@ ROW,036 SAY XVAR(XCNT+1,3) PICTURE '9999999³'
      *@ ROW,047 SAY XVAR(XCNT+1,4) PICTURE 'º999999³'
      *@ ROW,055 SAY XVAR(XCNT+1,5) PICTURE '9999999³'
      *@ ROW,066 SAY XVAR(XCNT+1,6) PICTURE 'º999999³'
      *@ ROW,074 SAY XVAR(XCNT+1,7) PICTURE '9999999³'
      *@ ROW,082 SAY XVAR(XCNT+1,8) PICTURE '999999º'
      *@ ROW,089 SAY XVAR(XCNT+1,9) PICTURE '9999999³'
      *@ ROW,097 SAY XVAR(XCNT+1,10) PICTURE '999999³'
      *@ ROW,104 SAY XVAR(XCNT+1,3) - XVAR(XCNT+1,9) PICTURE '999999º'
      *@ ROW,114 SAY XVAR(XCNT+1,1) - XVAR(XCNT+1,3) - XVAR(XCNT+1,8) PICTURE '9999999999'

      @ ROW,020 SAY XSUBTOT(1,1)  PICTURE '³9999999³'
      @ ROW,029 SAY XSUBTOT(1,2)  PICTURE '999999³'
      @ ROW,036 SAY XSUBTOT(1,3)  PICTURE '9999999³'
      @ ROW,047 SAY XSUBTOT(1,4)  PICTURE 'º999999³'
      @ ROW,055 SAY XSUBTOT(1,5)  PICTURE '9999999³'
      @ ROW,066 SAY XSUBTOT(1,6)  PICTURE 'º999999³'
      @ ROW,074 SAY XSUBTOT(1,7)  PICTURE '9999999³'
      @ ROW,082 SAY XSUBTOT(1,8)  PICTURE '999999º'
      @ ROW,089 SAY XSUBTOT(1,9)  PICTURE '9999999³'
      @ ROW,097 SAY XSUBTOT(1,10) PICTURE '999999³'
      @ ROW,104 SAY XSUBTOT(1,3) - XSUBTOT(1,9) PICTURE '999999º'
      @ ROW,114 SAY XSUBTOT(1,1) - XSUBTOT(1,3) - XSUBTOT(1,8) PICTURE '9999999999'
      *B605298,1 BWA 24/01/2002.[END]

      ROW = ROW + 1

      @ ROW,00 SAY REPLICATE('Í',132)
    ENDIF
    *B605035,1 BWA 10/22/2001.[END]

    XVAR = 0
    SKIP-1
    ******
    STYLE = lcStyle
  ENDSCAN
  X=0
  FOR Y=1 TO 3
    IF &BREAK(Y)<>HBREAK(Y)
      X=Y
    ENDIF
  ENDFOR

  IF !EOF()
    SKIP-1    
  ENDIF

  XM=X
  IF XM>0
     FOR X=1 TO XM
       IF X=1
          XLINE='Í'      && ÍÍÍÍÍÍ
       ELSE
         IF X=2

           *B605096,1 BWA 11/20/2001 Fix the bug of page break that fall in wrong position.[START]
           *ROW=ROW+3   && skips 3 lines before group subtotals
           *B605096,1 BWA 11/20/2001.[END]

           XLINE='='   && ======
         ELSE
           XLINE='*'   && ******
         ENDIF
       ENDIF
       ROW=ROW+1
       @ ROW,00 SAY REPLICATE(XLINE,132)
       ****** Printing
       ROW=ROW+1
       IF X=1   && by style
         @ ROW,000 SAY SUBSTR(HBREAK(X),8,8)  && if HBREAK(3)=SUBSTR(DIVISION,1,1)
       ELSE
         IF X=2   && by group
           lcGrpDesc =  SUBSTR(gfCodDes(SUBSTR(HBREAK(X),2,6) , 'CSTYGROUP'),1,15)
           IF !EMPTY(lcGrpDesc)
              @ ROW,000 SAY SUBSTR(lcGrpDesc,1,11)
           ENDIF
         ELSE     && by division
           DO CASE                && if HBREAK(3)=SUBSTR(DIVISION,1,1)
             CASE HBREAK(X)='P'
               @ ROW,000 SAY 'PETITE'
             CASE HBREAK(X)='L'
               @ ROW,000 SAY 'LARGE'
             CASE HBREAK(X)='M'
               @ ROW,000 SAY 'MISSY'
             OTHERWISE
               @ ROW,000 SAY 'OTHER'
             ENDCASE
          ENDIF
          SELECT (lcTmpName)
       ENDIF
       @ ROW,012 SAY 'TOTAL   ³'
       @ ROW,020 SAY XSUBTOT(X,1) PICTURE '9999999³'
       @ ROW,029 SAY XSUBTOT(X,2) PICTURE '999999³'
       @ ROW,036 SAY XSUBTOT(X,3) PICTURE '9999999³'
       @ ROW,047 SAY XSUBTOT(X,4) PICTURE 'º999999³'
       @ ROW,055 SAY XSUBTOT(X,5) PICTURE '9999999³'
       @ ROW,066 SAY XSUBTOT(X,6) PICTURE 'º999999³'
       @ ROW,074 SAY XSUBTOT(X,7) PICTURE '9999999³'
       @ ROW,082 SAY XSUBTOT(X,8) PICTURE '999999º'
       @ ROW,089 SAY XSUBTOT(X,9) PICTURE '9999999³'
       @ ROW,097 SAY XSUBTOT(X,10) PICTURE '999999³'
       @ ROW,104 SAY XSUBTOT(X,3)-XSUBTOT(X,9) PICTURE '999999º'
       *---Say the new column on 119 not 111
       @ ROW,114 SAY XSUBTOT(X,1)-XSUBTOT(X,3)-XSUBTOT(X,8) PICTURE '9999999999'
       ROW=ROW+1
       @ ROW,00 SAY REPLICATE(XLINE,132)
       IF X>1     && skips 3 lines after group and division subtotals

         *B605096,1 BWA 11/20/2001 Fix the bug of page break that fall in wrong position.[START]
         *ROW=ROW+3
         *B605096,1 BWA 11/20/2001.[END]

       ENDIF
       IF !EOF()
         SKIP
         HBREAK(X)=&BREAK(X)
         SKIP-1
       ELSE  
         HBREAK(X)=&BREAK(X)
       ENDIF
      ****** change of page before next subtotal
      IF ROW >= (lnMaxRow+10)
        PAGENO=PAGENO+1
        DO RPT_HDR WITH lcRpTitle ,XTITLE,R_WIDTH
        ROW=04
        IF X=2 .AND. XM=3  && 3 spaces before division totals in new page
           ROW=07
        ENDIF
      ENDIF
    ENDFOR
  ENDIF  
  ****** Printing Totals ******
  ROW=ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  ROW=ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  ROW=ROW+1
  @ ROW,000 SAY 'SEASON GRAND TOTAL  ³'
  @ ROW,021 SAY XTOTAL(1) PICTURE '9999999³'
  @ ROW,029 SAY XTOTAL(2) PICTURE '999999³'
  @ ROW,036 SAY XTOTAL(3) PICTURE '9999999³'
  @ ROW,047 SAY XTOTAL(4) PICTURE 'º999999³'
  @ ROW,055 SAY XTOTAL(5) PICTURE '9999999³'
  @ ROW,066 SAY XTOTAL(6) PICTURE 'º999999³'
  @ ROW,074 SAY XTOTAL(7) PICTURE '9999999³'
  @ ROW,082 SAY XTOTAL(8) PICTURE '999999º'
  @ ROW,089 SAY XTOTAL(9) PICTURE '9999999³'
  @ ROW,097 SAY XTOTAL(10) PICTURE '999999³'
  @ ROW,104 SAY XTOTAL(3)-XTOTAL(9) PICTURE '999999º'
  *---Say the new column on 119 not 111
  @ ROW,114 SAY XTOTAL(1)-XTOTAL(3)-XTOTAL(8) PICTURE '9999999999'
  ROW=ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  ROW=ROW+1
  *--Say the most recent Grand Totals for this season.
  =lfShowGrnd()
  @ ROW,00 SAY REPLICATE('*',132)
  *--Update the Grand Totals file.
  =lfUpdateF()
ENDSCAN 

DO ENDREPORT     && End of Report or Display on Screen
SET DEVICE TO SCREEN
RETURN

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 11/22/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Parameters  : lcParm
*!**************************************************************************
*! Example     : =lfsrvSty()
*!**************************************************************************
*! Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    GO TOP IN STYLE
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    *GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 11/22/2000
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns     : Calculated field value.
*!**************************************************************************
*! Example     : = lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
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



*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL) 
*! Date      : 11/22/2000
*! Purpose   : OG When Function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Parameters  : None
*!**************************************************************************
*! Returns     : .T.
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
FUNCTION lfwRepWhen

IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
ENDIF
IF !USED('CUTTKTH')
  =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
ENDIF
*B604152,1 (WAB) as per convert prog they put the related field for season into CATHCODE file
*B604152,1 (WAB) open file CATHCODE.dbf
IF !USED('CATHCODE')
  =gfOpenFile(gcDataDir+'CATHCODE',gcDataDir+'Crltfield','SH')
ENDIF

*C102767,1 BWA 11/17/2002 Open the poshdr.[START]
IF !USED('POSHDR')
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF
*C102767,1 BWA 11/17/2002.[END]

*B604152,1 (WAB) END.

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/18/2000
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
FUNCTION lfEvalSegs

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = ALLTRIM(lcNonMajTlt)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfShowGrnd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 11/29/2000
*! Purpose   : Say the most recent Grand Totals for this season.
*!*************************************************************
*! Called from : This program
*!*************************************************************
FUNCTION lfShowGrnd

PRIVATE lnAlias
lnAlias = SELECT()
SELECT TMPGNDSE
SET ORDER TO TAG TMPGNDSE DESC
*E301722,1 TMI [Start] Print SEASON GRAND TOTAL  in a new page
PAGENO=PAGENO+1
R_TITLE   = "Previous Report Totals"
DO RPT_HDR WITH lcRpTitle,XTITLE,R_WIDTH
ROW=05
@ ROW,00 SAY '                                                                                                                     NET PLAN       '
ROW=ROW+1         
@ ROW,00 SAY '                            ³----- SALES ------º+++ PRODUCTION +++º----- RECEIVING -----º+++++ SHIPPING ++++++º--- OPEN TO SELL ----'
ROW=ROW+1
@ ROW,00 SAY SPACE(LEN(lcColorTlt))+SPACE(11)+'    ³   PLAN³  WEEK³TO DATE³ % ºPROPOS³ ACTUAL³ % ºTRNSIT³ REC1st³REC2ndº  SHIPD³ONHAND³UNSHIPº                     '
ROW=ROW+1
@ ROW,00 SAY '                    ³       ³      ³       ³   º      ³       ³   º      ³       ³      º       ³      ³      º               '
ROW=ROW+1
*E301722,1 TMI [End  ] 
IF SEEK(lcSeaCode)
  SCAN REST WHILE cSeason+STR(nLineNo,6) = lcSeaCode 
    *E301722,1 TMI [Start] Display date for this report run
    ROW=ROW+1
    lcTitle = '<<Totals for ' + DTOC(dRunDate) + ' ' + cRunTime + '>>'
    @ ROW,66-LEN(lcTitle)/2 SAY lcTitle
    *E301722,1 TMI [End  ] 

    *B604152,1 (WAB) START - check the end of page before printing grand total
    *ROW=ROW+1
    *@ ROW,00 SAY REPLICATE('*',132)
    *ROW=ROW+1
    IF ROW >= (lnMaxRow+10)
       PAGENO=PAGENO+1
       DO RPT_HDR WITH lcRpTitle,XTITLE,R_WIDTH
       ROW=04
    ELSE
      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('*',132)
      ROW=ROW+1
    ENDIF
    *B604152,1 (WAB) END.
    
    @ ROW,000 SAY 'SEASON GRAND TOTAL  ³'
    @ ROW,021 SAY nTot1 PICTURE '9999999³'
    @ ROW,029 SAY nTot2 PICTURE '999999³'
    @ ROW,036 SAY nTot3 PICTURE '9999999³'
    @ ROW,047 SAY nTot4 PICTURE 'º999999³'
    @ ROW,055 SAY nTot5 PICTURE '9999999³'
    @ ROW,066 SAY nTot6 PICTURE 'º999999³'
    @ ROW,074 SAY nTot7 PICTURE '9999999³'
    @ ROW,082 SAY nTot8 PICTURE '999999º'
    @ ROW,089 SAY nTot9 PICTURE '9999999³'
    @ ROW,097 SAY nTot10 PICTURE '999999³'
    @ ROW,104 SAY nTot3-nTot9 PICTURE '999999º'
    *---Say the new column on 119 not 111
    @ ROW,114 SAY nTot1-nTot3-nTot8 PICTURE '9999999999'
    ROW=ROW+1
    @ ROW,00 SAY REPLICATE('*',132)
    ROW=ROW+1
  ENDSCAN
ENDIF
SET ORDER TO TAG TMPGNDSE ASCE
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUpdateF
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/18/2000
*! Purpose   : Update Grand Total File.
*!*************************************************************
*! Called from : This program
*!*************************************************************
FUNCTION lfUpdateF

PRIVATE lnAlias
lnAlias = SELECT()
SELECT TMPGNDSE

IF !SEEK(lcSeaCode)
  APPEND BLANK
  =lfInsert(1)
ELSE
  COUNT TO nNoOfRec WHILE cSeason+STR(nLineNo,6) = lcSeaCode
  IF nNoOfRec < 3 
    APPEND BLANK
    =lfInsert(nNoOfRec+1)
  ELSE
    *--Get the first record for this season as it is the oldest one
    =SEEK(lcSeaCode)
    =lfInsert(nNoOfRec+1)
    *--The line no now is 2 3 4 so decrease all by 1
    =SEEK(lcSeaCode)
    REPLACE nLineNo WITH nLineNo -1 WHILE cSeason+STR(nLineNo,6) = lcSeaCode
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUpdateF
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/18/2000
*! Purpose   : Update Grand Total File.
*!*************************************************************
*! Called from : This program
*!*************************************************************
FUNCTION lfInsert
PARAMETERS lnLineNo

REPLACE cSeason  WITH lcSeaCode,;
        nLineNo  WITH lnLineNo,;
        nTot1    WITH XTOTAL(1),;
        nTot2    WITH XTOTAL(2),;
        nTot3    WITH XTOTAL(3),;
        nTot4    WITH XTOTAL(4),;
        nTot5    WITH XTOTAL(5),;
        nTot6    WITH XTOTAL(6),;
        nTot7    WITH XTOTAL(7),;
        nTot8    WITH XTOTAL(8),;
        nTot9    WITH XTOTAL(9),;
        nTot10   WITH XTOTAL(10),;
        dRunDate WITH gdSysDate,;
        cRunTime WITH TIME()

*--End of lfInsert.