*:**************************************************************************
*: Program file  : SOCAT100
*: Program desc. : Style Pack Report.
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C102059
*:**************************************************************************
*: Procedures    : lpCreatFil
*:
*: Functions     : lfwRepWhen , lfItmPos , lfEvalSegs , lfPackInfo , 
*:                 lfwOldVal , lfPrintRep , lfvYear , lfvPack , 
*:                 lfSRVSty , lfStySum , gfCodDes , lfClearRep
*: Procedures    : lpGenExpr
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*B604153,1 SSE 01/29/2001 Fix bug of improving performance.
*B604885,1 SSE 09/09/2001 Fix bug of removing Year field from Option Grid.
*B604910,1 SSE 09/16/2001 Fix bug of wrong WIP & wrong Qtys in last Pack no.
*B604998,1 SSE 10/07/2001 Fix bug of adding Lot column to report and also of not displaying 
*B604998,1                Right quantity in the last pack.
*B605231,1 MHM 12/11/2001 Change color code to color description
*B605690,1 SSE 03/13/2002 Fix bug of displaying duplicate styles in Preview and Run.
****************************************************************************

*-- IF Break Pack no. less than Start Pack no.
IF !EMPTY(lcRpBreak) AND lcRpBreak < lcRpStart
  *-- Message <Break pack number can not be less than Starting pack number.>
  *-- Buttons <                             OK                             >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"Break pack number can not be less than Starting pack number!.")
  RETURN
ENDIF

PRIVATE llContinue   && Variable to detect if continued after each function return or terminate.

*B604885,1 Initialize llContinue with False. [Begin]
*llContinue = .T.
llContinue = .F.
*B604885,1 Initialize llContinue with False. [End]

*B604910,1 making the right order for cut ticket header. [Begin]
SELECT Cuttkth
SET ORDER TO TAG Cuttkth
*B604910,1 making the right order for cut ticket header. [End]

SELECT OrdLine
SET RELATION TO cOrdType + Order INTO OrdHdr ADDITIVE

*-- IF filter change collect data again.
IF llOGFltCh
  *B604153,1 use this procedure to create the Temp Packs file [Begin]
  *IF !USED(lcWorkFile) OR (USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0)
    DO lpCreatFil
  *ENDIF
  *B604153,1 use this procedure to create the Temp Packs file [End]
  
  *B604153,1 Remove lcStyleExp variable and get Low , High Packs date [Begin]
  *PRIVATE lnBegnFrom , lcCurrSeas , lcStyleExp
  PRIVATE lnBegnFrom , lcCurrSeas
  STORE {} TO ldPackLow , ldPackHigh
  SELECT Packs
  
  *B604885,1 Always take last and first record in Packs file. [Begin]
  *SET ORDER TO TAG cYear DESCENDING
  *ldPackHigh = IIF(SEEK(lcRpYear),dSale_Date,ldPackHigh)
  *SET ORDER TO TAG cYear ASCENDING
  *ldPackLow  = IIF(SEEK(lcRpYear),dSale_Date,ldPackLow)
  
  SET ORDER TO TAG cYear DESCENDING
  LOCATE FOR !EMPTY(dSale_Date)
  IF FOUND()
    ldPackHigh = dSale_Date
  ENDIF
    
  SET ORDER TO TAG cYear ASCENDING
  LOCATE FOR !EMPTY(dSale_Date)
  IF FOUND()
    ldPackLow = dSale_Date
  ENDIF
  *B604885,1 Always take last and first record in Packs file. [End]
  
  *B604153,1 Remove lcStyleExp variable and get Low , High Packs date [End]

  *B604153,1 No need to call lpGenExpr to cut Style Expr. [Begin]
  *lcStyleExp = ''
  *DO lpGenExpr
  *lcStyleExp = IIF(EMPTY(lcStyleExp),".T.",lcStyleExp)
  
  lcWorkFile = IIF(USED(laOGFxFlt[lnStylePos,6]) AND RECCOUNT(laOGFxFlt[lnStylePos,6])>0,laOGFxFlt[lnStylePos,6],'StyleFil')
  *B604153,1 No need to call lpGenExpr to cut Style Expr. [End]
  
  lnBegnFrom = 1
  
  *-- If season Empty
  IF EMPTY(laOGFxFlt[lnSeasonPo,6])

    *B604153,1 SSE Commented out [Begin]
    *SELECT Style
    *SCAN FOR &lcStyleExp AND Status <> "X"
    *  SCATTER MEMVAR MEMO
    *  INSERT INTO (lcWorkFile) FROM MEMVAR      
    *ENDSCAN
    *SELECT OrdLine
    *LOCATE
    *lcCurrSeas = ALLTRIM(Season)
    *DO WHILE !EOF('OrdLine')
    *  STORE {} TO ldLowDate , ldHighDate
    *  lcLowPack = '01'
    *  SELECT OrdLine
    *  SEEK lcCurrSeas
    *  SCAN REST WHILE SUBSTR(Style,1,lnSeasLen) = lcCurrSeas FOR OrdHdr.Status <> "X"
    *    IF lcRpYear = ALLTRIM(STR(YEAR(Complete)))
    *      ldLowDate = IIF(EMPTY(ldLowDate),Complete,MIN(ldLowDate,IIF(EOF(),;
    *                  ldLowDate,Complete)))      && Minimum Date
    *      IF ldLowDate = Complete                && Minimum Pack#
    *        lcLowPack = cPack
    *      ENDIF
    *      ldHighDate = MAX(ldHighDate,IIF(EOF(),ldHighDate,Complete))    && Maximum Date
    *    ENDIF  
    *  ENDSCAN
    *  m.cSeason   = lcCurrSeas
    *  m.cLowPack  = lcLowPack
    *  m.dLowDate  = ldLowDate
    *  m.dHighDate = ldHighDate
    *  INSERT INTO (lcTmpPacks) FROM MEMVAR
    *  lcCurrSeas = ALLTRIM(Season)
    *ENDDO
    *B604153,1 SSE Commented out [End]
    
    *-- If user selected some styles.
    IF USED(laOGFxFlt[lnStylePos,6]) AND RECCOUNT(laOGFxFlt[lnStylePos,6])>0
      PRIVATE lcCurStyle 
      SELECT (lcWorkFile)
      SET RELATION TO SUBSTR(&lcWorkFile..cStyMajor,1,LEN(lcStylePic)-3) INTO OrdLine
      LOCATE
            
      DO WHILE !EOF(lcWorkFile)      
        SELECT OrdLine
        STORE {} TO ldLowDate , ldHighDate
        lcCurStyle = ALLTRIM(&lcWorkFile..cStyMajor)
        WAIT WINDOW 'Collecting Data for ' + Style NOWAIT

        SEEK lcCurStyle
        SCAN REST WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcCurStyle  
          WAIT WINDOW 'Collecting Data for ' + Style NOWAIT 
           IF ( Complete >= ldPackLow  AND Complete <= ldPackHigh ) AND OrdHdr.Status <> "X"
               ldLowDate = IIF(EMPTY(ldLowDate),Complete,MIN(ldLowDate,IIF(EOF(),;
                              ldLowDate,Complete)))      && Minimum Date
             IF ldLowDate = Complete                && Minimum Pack#
               lcLowPack = cPack
             ENDIF
             ldHighDate = MAX(ldHighDate,IIF(EOF(),ldHighDate,Complete))    && Maximum Date
                 
           ENDIF
        ENDSCAN
        
        IF SEEK(ALLTRIM(SUBSTR(&lcWorkFile..cStyMajor,1,lnSeasLen)),(lcTmpPacks))
          SELECT (lcTmpPacks)
          REPLACE cLowPack  WITH MIN(cLowPack,lcLowPack) ,;
                  dLowDate  WITH MIN(dLowDate,ldLowDate) ,;
                  dHighDate WITH MAX(dHighDate,ldHighDate)
        ELSE
          m.cSeason   = ALLTRIM(SUBSTR(&lcWorkFile..cStyMajor,1,lnSeasLen))
          m.cLowPack  = lcLowPack
          m.dLowDate  = ldLowDate
          m.dHighDate = ldHighDate
          
          IF ldLowDate >= ldPackLow AND ldHighDate <= ldPackHigh
            INSERT INTO (lcTmpPacks) FROM MEMVAR
          ENDIF  
        ENDIF

        IF !EOF(lcWorkFile)
          SELECT (lcWorkFile)
          SKIP
        ENDIF
      ENDDO
      
      SELECT (lcWorkFile)
      SET RELATION TO
    ELSE                     && Else User didn't select style
      PRIVATE lcCurStyle
      SELECT OrdLine
      LOCATE
      lcCurrSeas = ALLTRIM(Season)
              
      DO WHILE !EOF('OrdLine')
        STORE {} TO ldLowDate , ldHighDate
        lcLowPack  = '01'

        SELECT OrdLine
        SEEK lcCurrSeas
        WAIT WINDOW 'Collecting Data for ' + Style NOWAIT
               
        SCAN REST WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcCurrSeas  
          WAIT WINDOW 'Collecting Data for ' + Style NOWAIT 
          
          IF ( Complete >= ldPackLow  AND Complete <= ldPackHigh ) AND OrdHdr.Status <> "X"
             ldLowDate = IIF(EMPTY(ldLowDate),Complete,MIN(ldLowDate,IIF(EOF(),;
                            ldLowDate,Complete)))      && Minimum Date
            IF ldLowDate = Complete                && Minimum Pack#
              lcLowPack = cPack
            ENDIF
            ldHighDate = MAX(ldHighDate,IIF(EOF(),ldHighDate,Complete))    && Maximum Date
          ENDIF
        ENDSCAN

        m.cSeason   = lcCurrSeas
        m.cLowPack  = lcLowPack
        m.dLowDate  = ldLowDate
        m.dHighDate = ldHighDate

        IF ldLowDate >= ldPackLow AND ldHighDate <= ldPackHigh
          INSERT INTO (lcTmpPacks) FROM MEMVAR
        ENDIF  
        lcCurrSeas = ALLTRIM(Season)
      ENDDO
        
    ENDIF
    *B604153,1 If user selected Style InRange [End]
    
  ELSE           && Else user selected season.
    *B604153,1 Change performance in case of user has selected Season in OG [Begin]
    *-- Loop around all selected seasons from OG. [Begin]
    *DO WHILE !(SUBSTR(laOGFxFlt[lnSeasonPo,6],lnBegnFrom,6) == "")
    *  IF SEEK(ALLTRIM(SUBSTR(laOGFxFlt[lnSeasonPo,6],lnBegnFrom,6)),'Style')
    *    lcCurrSeas = SUBSTR(Style.Style,1,lnSeasLen)    
    *    SELECT Style        
    *    SCAN REST WHILE SUBSTR(Style,1,lnSeasLen) = lcCurrSeas FOR Status <> "X" AND &lcStyleExp
    *      SCATTER MEMVAR MEMO
    *      INSERT INTO (lcWorkFile) FROM MEMVAR      
    *    ENDSCAN
    *    IF SEEK(lcCurrSeas,'OrdLine')
    *      STORE {} TO ldLowDate , ldHighDate
    *      lcLowPack  = '01'
    *      SELECT OrdLine
    *      SCAN REST WHILE SUBSTR(Style,1,lnSeasLen) = lcCurrSeas FOR OrdHdr.Status <> "X"
    *        IF lcRpYear = ALLTRIM(STR(YEAR(Complete)))
    *          ldLowDate = IIF(EMPTY(ldLowDate),Complete,MIN(ldLowDate,IIF(EOF(),;
    *                      ldLowDate,Complete)))      && Minimum Date
    *          IF ldLowDate = Complete                && Minimum Pack#
    *            lcLowPack = cPack
    *          ENDIF    
    *          ldHighDate = MAX(ldHighDate,IIF(EOF(),ldHighDate,Complete))    && Maximum Date
    *        ENDIF  
    *      ENDSCAN
    *      m.cSeason   = lcCurrSeas
    *      m.cLowPack  = lcLowPack
    *      m.dLowDate  = ldLowDate
    *      m.dHighDate = ldHighDate
    *      INSERT INTO (lcTmpPacks) FROM MEMVAR
    *    ENDIF    
    *  ENDIF
    *  lnBegnFrom = lnBegnFrom + 7      
    *ENDDO
    *-- Loop around all selected seasons from OG. [End]    
    
    IF USED(laOGFxFlt[lnStylePos,6]) AND RECCOUNT(laOGFxFlt[lnStylePos,6])>0
      PRIVATE lcCurStyle
      SELECT (lcWorkFile)
      SET RELATION TO SUBSTR(&lcWorkFile..cStyMajor,1,LEN(lcStylePic)-3) INTO OrdLine
      LOCATE
      
      DO WHILE !EOF(lcWorkFile)
        SELECT OrdLine
        STORE {} TO ldLowDate , ldHighDate
        lcCurStyle = ALLTRIM(&lcWorkFile..cStyMajor)
        lcCurrSeas = SUBSTR(&lcWorkFile..cStyMajor,1,lnSeasLen)
        WAIT WINDOW 'Collecting Data for ' + Style NOWAIT

        *-- If this style is having the same selected season. 
        IF SEEK(lcCurStyle) AND PADR(lcCurrSeas,6) $ laOGFxFlt[lnSeasonPo,6]
          SCAN REST WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcCurStyle  
            WAIT WINDOW 'Collecting Data for ' + Style NOWAIT 
             IF ( Complete >= ldPackLow  AND Complete <= ldPackHigh ) AND OrdHdr.Status <> "X"
                 ldLowDate = IIF(EMPTY(ldLowDate),Complete,MIN(ldLowDate,IIF(EOF(),;
                                ldLowDate,Complete)))      && Minimum Date
               IF ldLowDate = Complete                && Minimum Pack#
                 lcLowPack = cPack
               ENDIF
               ldHighDate = MAX(ldHighDate,IIF(EOF(),ldHighDate,Complete))    && Maximum Date
                 
             ENDIF
          ENDSCAN

          IF SEEK(ALLTRIM(SUBSTR(&lcWorkFile..cStyMajor,1,lnSeasLen)),(lcTmpPacks))
            SELECT (lcTmpPacks)
            REPLACE cLowPack  WITH MIN(cLowPack,lcLowPack) ,;
                    dLowDate  WITH MIN(dLowDate,ldLowDate) ,;
                    dHighDate WITH MAX(dHighDate,ldHighDate)
          ELSE
            m.cSeason   = ALLTRIM(SUBSTR(&lcWorkFile..cStyMajor,1,lnSeasLen))
            m.cLowPack  = lcLowPack
            m.dLowDate  = ldLowDate
            m.dHighDate = ldHighDate
          
            IF ldLowDate >= ldPackLow AND ldHighDate <= ldPackHigh
              INSERT INTO (lcTmpPacks) FROM MEMVAR
            ENDIF  
          ENDIF
        ENDIF  
        *-- EndIf of this style is having the same selected season. 

        IF !EOF(lcWorkFile)
          SELECT (lcWorkFile)
          SKIP
        ENDIF
      ENDDO
      
      SELECT (lcWorkFile)
      SET RELATION TO
    ELSE
      PRIVATE lcCurStyle
      
      *-- Loop around all selected seasons from OG. [Begin]
      DO WHILE !(SUBSTR(laOGFxFlt[lnSeasonPo,6],lnBegnFrom,6) == "")
        IF SEEK(ALLTRIM(SUBSTR(laOGFxFlt[lnSeasonPo,6],lnBegnFrom,6)),'OrdLine')
          lcCurrSeas = SUBSTR(OrdLine.Style,1,lnSeasLen)
          STORE {} TO ldLowDate , ldHighDate
          lcLowPack  = '01'
          SELECT OrdLine
          SCAN REST WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcCurrSeas  
            WAIT WINDOW 'Collecting Data for ' + Style NOWAIT 
             IF ( Complete >= ldPackLow  AND Complete <= ldPackHigh ) AND OrdHdr.Status <> "X"
               ldLowDate = IIF(EMPTY(ldLowDate),Complete,MIN(ldLowDate,IIF(EOF(),;
                              ldLowDate,Complete)))      && Minimum Date
               IF ldLowDate = Complete                && Minimum Pack#
                 lcLowPack = cPack
               ENDIF
               ldHighDate = MAX(ldHighDate,IIF(EOF(),ldHighDate,Complete))    && Maximum Date
             ENDIF
          ENDSCAN

          m.cSeason   = lcCurrSeas
          m.cLowPack  = lcLowPack
          m.dLowDate  = ldLowDate
          m.dHighDate = ldHighDate
          IF ldLowDate >= ldPackLow AND ldHighDate <= ldPackHigh
            INSERT INTO (lcTmpPacks) FROM MEMVAR
          ENDIF  
        ENDIF
       lnBegnFrom = lnBegnFrom + 7      
      ENDDO
      *-- Loop around all selected seasons from OG. [End]
      *B604153,1 Change performance in case of user has selected Season in OG [End]

    ENDIF
    *-- Endif of user selected style
  ENDIF
  *-- Endif of Season Empty
ENDIF
*-- Endif of Filter change.  

SELECT (lcTmpPacks)
LOCATE
SELECT (lcWorkFile)
LOCATE
IF EOF() OR EOF(lcTmpPacks)
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE
  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT  

  *B604153,1 Make relation with Style file [Begin]
  SELECT (lcWorkFile)
  SET RELATION TO
  SET RELATION TO SUBSTR(&lcWorkFile..cStyMajor,1,LEN(lcStylePic)-3) INTO Style
  *B604153,1 Make relation with Style file [End]
  
  SELECT (lcTmpPacks)
  LOCATE
  
  *B604885,1 Return if llContinue is false (no season is valid). [Begin]
  *IF !lfPrintRep()
  *  SET DEVICE TO SCREEN
  *  RETURN
  *ENDIF
  =lfPrintRep()
  IF !llContinue
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  *B604885,1 Return if llContinue is false (no season is valid). [End]
  
  DO ENDREPORT
  SET DEVICE TO SCREEN  
ENDIF 

*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3.
*EORD970                                            STYLE PACK REPORT
*MM/DD/YY                                   Season XX  XXXXXXXXXXXXXXXXXXXX
*
*                           (pack & completion date)
*____________________________________________________________________________________________________________________________________
*STYLE  COLOR LAST  TOT.WIP   XX     XX     XX     XX     XX     XX     XX     XX     XX     XX     XX     XX     XX     XX     TOTAL
*             LOT           999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999 999999
*XXXXXXX  XXX  XX   1234567 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 1234567
*====================================================================================================================================
**SUBTOT* XXXXXXX  12345678 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 1234567
*====================================================================================================================================
*************************************************************************************************************************************
**SEASON TOT*      12345678 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 123456 1234567
*************************************************************************************************************************************
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3.

*!**************************************************************************
*! Name      : lfPrintRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Print Report to Printer or Screen.
*!**************************************************************************
*! Example   : = lfPrintRep()
*!**************************************************************************
*
FUNCTION lfPrintRep
*-- Variables Initialization.
DIMENSION laPack[52],laDate[52],lnRangeVal[12],lnTotPack[12]
*B605231,1 MHM 12/11/2001 [Start]
*PRIVATE lnThru , lnTot , lnColumn , lnTotWip , lnTotlWip , lnPlanTot ,;
*        lnTotal , lnTotSal , lcColor , lcStyle , lnSum , lnTotSum , PageNo
PRIVATE lnThru , lnTot , lnColumn , lnTotWip , lnTotlWip , lnPlanTot ,;
        lnTotal , lnTotSal , lcColor , lcStyle , lnSum , lnTotSum , PageNo ,lcOldColor
*B605231,1  [End]        
PageNo = 0

*B604885,1 Variable to detect the locate statement. [Begin]
PRIVATE llFound
*B604885,1 Variable to detect the locate statement. [End]

*B604998,1 Variable for Lot no. [Begin]
PRIVATE lnLastLot , ldLastDate
lnLastLot = 0
ldLastDate = {}
*B604998,1 Variable for Lot no. [End]

SELECT (lcTmpPacks)
LOCATE

DO WHILE !EOF()
  PRIVATE lnMaxPack , lcSeason
  lcSeason = ALLTRIM(cSeason)                && Hold Current season from Temp Packs file.
  
  STORE 0 TO lnTot , lnRangeVal , lnTotPack , lnTotal , lnTotSum ,;
             lnMaxPack , lnTotlWip , lnPlanTot , lnTotSal , lnThru , lnSum
             
  laPack = ''
  laDate = {}
  
  IF !lfPackInfo()

    *B604885,1 If season is invalid skip it and continue. [Begin]
    *llContinue = .F.
    *RETURN llContinue
    SELECT (lcTmpPacks)
    IF !EOF()
      SKIP
    ENDIF  
    LOOP
    *B604885,1 If season is invalid skip it and continue. [End]

  ENDIF
  
  ROW     = 99
  XTITLE  = 'Season ' + ALLTRIM(lcSeason) + '   ' + ALLTRIM(gfCodDes(lcSeason, 'SEASON'))
  R_TITLE = 'Style Pack Report'

  SELECT CutTktL
  SET RELATION TO CutTkt INTO CutTktH
  
  SELECT InvLine
  SET RELATION TO Invoice INTO InvHdr
  
  IF !SEEK(lcSeason,lcWorkFile)
    SELECT (lcTmpPacks)
    IF !EOF()
      SKIP
    ENDIF  
    LOOP
  ENDIF
  
  *B604153,1 Change Work File [Begin]
  *SELECT (lcWorkFile)
  SELECT Style
  *B604153,1 Change Work File [End]
  
  DO WHILE INKEY() <> 32
    *B604153,1 Add a message to user [Begin]
    *WAIT WINDOW 'Printing ' + Style.Style NOWAIT
    WAIT WINDOW 'Collecting Data - <Space Bar> to Abort ' NOWAIT
    *B604153,1 Add a message to user [End]
    
    lnThru = 0
    *-- Check for end of file.
    
    *B604153,1 Change Work File [Begin]
    *IF EOF(lcWorkFile)
    IF EOF('Style') OR EOF(lcWorkFile)
    *B604153,1 Change Work File [End]
    
      EXIT
    ENDIF
    
    *B604153,1 Change Work File [Begin]
    *IF &lcWorkFile..Season <> lcSeason
    IF Style.Season <> lcSeason
    *B604153,1 Change Work File [End]
    
      EXIT
    ENDIF

    *B604153,1 Checking for style status [Begin]
    IF Style.Status = "X"
      IF !EOF('Style')
        *B605690,1 Move pointer also in Parent file. [Begin]
        *SKIP
        SKIP IN (lcWorkFile)
        *B605690,1 Move pointer also in Parent file. [End]
        LOOP
      ENDIF
    ENDIF
    *B604153,1 Checking for style status [End]
    
    lnTotWip = 0 
    *-- If Row >= 50 (Report reached end of current page).
    IF ROW >= 50                    && Max Row at 50
      PageNo = PageNo + 1           && Page no variable printed in RPT_HDR (global procedure)
      DO RPT_HDR WITH 'SOCAT100',XTITLE,'W'

      *-- Printing 1st line of Header [Begin]
      IF EMPTY(lcRpStart)
      
        *B604998,1 Add the Lot column. [Begin] 
        *@ 05,000 SAY PADR(UPPER(ALLTRIM(lcMajTtl)),7) + PADR(UPPER(lcNonMajT),5) + '   ' + ;
        *             IIF(!EMPTY(lcRpBreak),'  PACK'+IIF(EMPTY(lcRpStart),'01',lcRpStart),'')
        @ 05,000 SAY PADR(UPPER(ALLTRIM(lcMajTtl)),7) + SPACE(1) + PADR(UPPER(lcNonMajT),5) + '  Lot' + ;
                     IIF(!EMPTY(lcRpBreak),'  PACK'+IIF(EMPTY(lcRpStart),'01',lcRpStart),'')
        *B604998,1 Add the Lot column. [End]

        lnColumn = 30
      ELSE

        *B604998,1 Add the Lot column. [Begin] 
        *@ 05,000 SAY PADR(UPPER(ALLTRIM(lcMajTtl)),6) + PADR(UPPER(lcNonMajT),5) + '    THRU'+IIF(!EMPTY(lcRpBreak),;
        *             '    PACK' + IIF(lcRpStart = lcRpBreak,lcRpStart,ALLTRIM(STR(VAL(lcRpStart)+1))),'')
        @ 05,000 SAY PADR(UPPER(ALLTRIM(lcMajTtl)),7) + SPACE(1) + PADR(UPPER(lcNonMajT),5) + '  Lot'+' THRU'+IIF(!EMPTY(lcRpBreak),;
                     '   PACK' + IIF(lcRpStart = lcRpBreak,lcRpStart,ALLTRIM(STR(VAL(lcRpStart)+1))),'')
        *B604998,1 Add the Lot column. [End] 

        lnColumn = IIF(EMPTY(lcRpBreak),30,34)
      ENDIF
      *-- Printing 1st line of Header [End]

      *B604885,1 Select Packs file. [Begin]
      SELECT Packs
      *B604885,1 Select Packs file. [End]

      *-- For loop around all Packs to print rest of 1st Header line [Begin]
      FOR lnX = 1 TO lnMaxPack
        
        *B604885,1 change all the seek statememt. [Begin]
        *IF SEEK(lcRpYear + laPack(lnX),'Packs')
        LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX)
        IF FOUND()
        *B604885,1 change all the seek statememt. [End]

          ldCurDate = Packs.dSale_Date
        ENDIF

        *B604885,1 change all the seek statememt. [Begin]
        *IF lnX > 1 AND SEEK(lcRpYear + laPack(lnX-1),'Packs')
        llFound = .F.
        IF lnX > 1          
          LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX-1)
          llFound = FOUND()
        ENDIF  
        IF llFound
        *B604885,1 change all the seek statememt. [End]
        
          ldPrevDate = Packs.dSale_Date
        ENDIF
        IF (!EMPTY(lcRpStart) AND ldCurDate < ldBegnDate);
          OR (!EMPTY(lcRpBreak) AND ldCurDate < ldBreakDat);
          OR (lnX > 1 AND ldCurDate < ldPrevDate)
          EXIT
        ENDIF
        @ 05,lnColumn SAY laPack(lnX) PICTURE '@!'
        lnColumn = lnColumn + 7
      ENDFOR
      *-- EndFor loop around all Packs to print rest of 1st Header line [End]
         
      @ 05,112 SAY 'TOTAL  TOTAL   TOTAL'     && Print rest of 1st Header line.

      *-- Print 2nd Header line [Begin]
      IF EMPTY(lcRpStart)
      
        *B604998,1 Shift the following. [Begin]
        *@ 06,013 SAY '    '+IIF(!EMPTY(lcRpBreak),'THRU'+ALLTRIM(lcRpBreak)+' ','')
        @ 06,020 SAY IIF(!EMPTY(lcRpBreak),'THRU'+ALLTRIM(lcRpBreak)+' ','')
        *B604998,1 Shift the following. [End]
        
        lnColumn = 30
      ELSE

        *B604998,1 Shift the following. [Begin]
        *@ 06,010 SAY '     PACK' + lcFromPack + IIF(!EMPTY(lcRpBreak),'  THRU'+lcRpBreak,'')
        @ 06,019 SAY 'PACK' + lcFromPack + IIF(!EMPTY(lcRpBreak),' THRU'+lcRpBreak,'')
        *B604998,1 Shift the following. [Begin]

        lnColumn = IIF(EMPTY(lcRpBreak),30,34)
      ENDIF
      *-- Print 2nd Header line [End]

      *-- For loop around all Packs to print rest of 2nd Header line [Begin]
      FOR lnX = 1 TO lnMaxPack        

        *B604885,1 change all the seek statememt. [Begin]
        *IF SEEK(lcRpYear + laPack(lnX),'Packs')        
        LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX)
        IF FOUND()
        *B604885,1 change all the seek statememt. [End]

          ldCurDate = Packs.dSale_Date
        ENDIF
        
        *B604885,1 change all the seek statememt. [Begin]
        *IF lnX > 1 AND SEEK(lcRpYear + laPack(lnX-1),'Packs')
        llFound = .F.
        IF lnX > 1
          LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX-1)
          llFound = FOUND()
        ENDIF  
        IF llFound
        *B604885,1 change all the seek statememt. [End]
        
          ldPrevDate = Packs.dSale_Date
        ENDIF
        IF (!EMPTY(lcRpStart) AND ldCurDate < ldBegnDate);
          OR (!EMPTY(lcRpBreak) AND ldCurDate < ldBreakDat);
          OR (lnX >1 AND ldCurDate < ldPrevDate)
          EXIT
        ENDIF
        @ 06,lnColumn SAY SUBSTR(DTOS(laDate(lnX)),5,2)+SUBSTR(DTOS(laDate(lnX)),7,2)+SUBSTR(DTOS(laDate(lnX)),3,2)
        lnColumn = lnColumn + 7
      ENDFOR
      *-- EndFor loop around all Packs to print rest of 2nd Header line [End]
   	  	
      @ 06,114 SAY 'WIP   PLAN   SALES'       && Print rest of 1st Header line. 
      @ 07,000 SAY REPLICATE('=',132)    && Print line feed 
      ROW = 07
    ENDIF
    *-- EndIf of Row >= 50 (Report reached end of current page).

    IF ROW >= 50                    && Max Row at 50
      ROW = 99
      LOOP
    ENDIF
     
    * Note: lcWorkFile is the current area
    *B604153,1 Change work area [Begin]
    *lcColor   = SUBSTR(Style,lnClrPo,lnColorLen)
    *lcStyle   = &lcWorkFile..Style
    *lcOnlySty = SUBSTR(&lcWorkFile..Style,lnStyStPos,lnStyleLen)

    *B605231,1 MHM 12/11/2001 Get color desc.[Start]
    *lcColor   = SUBSTR(Style.Style,lnClrPo,lnColorLen)
    lcOldColor = SUBSTR(Style.Style,lnClrPo,lnColorLen)
    lcColor    = LEFT(ALLTRIM(gfCodDes(lcOldColor, 'COLOR     ')),7)
    *B605231,1  [End]
    lcStyle   = Style.Style
    lcOnlySty = SUBSTR(Style.Style,lnStyStPos,lnStyleLen)
    *B604153,1 Change work area [End]

    SELECT CutTktL
    SEEK lcStyle

    *B604988,1 Initialize lnLastLot with zero. [Begin]
    lnLastLot = 0
    *B604988,1 Initialize lnLastLot with zero. [End]

    SCAN REST WHILE Style + Cuttkt + TranCd = lcStyle FOR TranCd = '1'
      IF CutTktH.Status <> 'X'
        lnTotWip  = lnTotWip + TotQty
      ENDIF

      *B604988,1 Get lnLastLot. [Begin]
      lnLastLot = CutTktH.nLot_No
      *B604988,1 Get lnLastLot. [End]
      
    ENDSCAN 
    lnTotlWip = lnTotlWip + lnTotWip
    
    lnRangeVal = 0

    *-- in Scan loop below we get Open Quantity (Ordline) + Shipped Quantity (InvLine)
    SELECT OrdLine
    =SEEK(lcStyle)             && Seek in Ordline to get Total Open Quantity
    
    *-- Scan loop around Ordline to get TotQty. 
    SCAN REST WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcStyle ;
              FOR OrdHdr.Status <> 'X'
    
      *-- For loop around all packs.
      FOR lnX = 1 TO lnMaxPack
        *-- If pack of Ordline equal to Pack array
        *B604153,1 Add the checking of date [Begin]
        *IF cPack == laPack(lnX) 
        IF cPack == laPack(lnX) AND Complete >= ldPackLow  AND Complete <= ldPackHigh
        *B604153,1 Add the checking of date [End]

          lnRangeVal(lnX) = lnRangeVal(lnX) + TotQty          
          IF SEEK (Order+STR(LineNo,6),'InvLine')        && Seek in InvLine
            SELECT InvLine     && Seek in Invline to get Total Shipped Quantity    
            SCAN REST WHILE Order+STR(LineNo,6) = OrdLine.Order+STR(OrdLine.LineNo,6) FOR InvHdr.Status <> 'V'
              lnRangeVal(lnX) = lnRangeVal(lnX) + TotQty
            ENDSCAN
          ENDIF
          EXIT
        ENDIF
        *-- If pack of Ordline equal to Pack array
      ENDFOR
      *-- EndFor loop around all packs.
    
    ENDSCAN 
    *-- EndScan loop around Ordline to get TotQty. 

    *-- this section to get the rest of Packs from lnMaxPack till 52 [Begin]
    IF lnMaxPack > 1
      *-- lnStart is the next pack following MaxPack Value (Ex MaxPack is '16' , lnStart = '17')
      lnStart = ROUND(IIF(laPack(lnMaxPack)=='52',VAL(laPack(lnMaxPack)),VAL(laPack(lnMaxPack))+1),0)
      *lnStart = ROUND(VAL(ALLTRIM(laPack(lnMaxPack))),0) + IIF(ROUND(VAL(ALLTRIM(laPack(lnMaxPack))),0)=52,0,1)

      *B604885,1 change all the seek statememt. [Begin]
      SELECT Packs
      *IF SEEK(lcRpYear + laPack(lnMaxPack-1),'Packs')
      llFound = .F.
      IF lnX > 1
        LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnMaxPack-1)
        llFound = FOUND()
      ENDIF  
      IF llFound
      *B604885,1 change all the seek statememt. [End]
      
        ldPrevDate = Packs.dSale_Date
      ENDIF

      *B604885,1 change all the seek statememt. [Begin]
      *=SEEK(lcRpYear + PADL(ALLTRIM(STR(lnStart)),2,'0'),'Packs')
      LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = PADL(ALLTRIM(STR(lnStart)),2,'0')
      *B604885,1 change all the seek statememt. [End]
                  
      ldStrtDate = Packs.dSale_Date + IIF(ROUND(VAL(SUBSTR(ALLTRIM(STR(YEAR(PackS.dSale_Date))),3)),0);
                  <ROUND(VAL(SUBSTR(DTOC(ldPrevDate),LEN(DTOC(ldPrevDate))-1)),0),365,0)

      SELECT OrdLine
      SEEK lcStyle
      SCAN WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcStyle FOR OrdHdr.Status <> 'X' AND ;
                 IIF(laPack(lnMaxPack) = '52',Complete > ldStrtDate,Complete >= ldStrtDate)

        *B604153,1 Add the checking of date [Begin]

        *B604910,1 Fix wrong display of pack Qtys at last pack no. [Begin]
        *IF Complete >= ldPackLow  AND Complete <= ldPackHigh
        *IF Complete > ldPackHigh
        
        *B604998,1 Display total pack from current pack number till end of packs. [Begin] 
        IF Complete >= ldLastDate
        *B604998,1 Display total pack from current pack number till end of packs. [End] 
        
        *B604910,1 Fix wrong display of pack Qtys at last pack no. [End]
        
        *B604153,1 Add the checking of date [End]
         
          lnRangeVal(lnMaxPack) = lnRangeVal(lnMaxPack) + TotQty 
          IF SEEK (Order+STR(LineNo,6),'InvLine')
            SELECT InvLine
            SCAN WHILE Order+STR(LineNo,6) = OrdLine.Order+STR(OrdLine.LineNo,6) FOR InvHdr.Status <> 'V'
              lnRangeVal(lnMaxPack) = lnRangeVal(lnMaxPack) + TotQty
            ENDSCAN
          ENDIF
        ENDIF    
      ENDSCAN 
    ENDIF
    *-- this section to get the rest of Packs from lnMaxPack till 52 [End]

    IF !EMPTY(lcRpStart)
      SELECT OrdLine
      SEEK lcStyle
      SCAN WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcStyle FOR Complete <= ldBegnDate AND OrdHdr.Status <> 'X'

        *B604153,1 Add the checking of date [Begin]
        IF Complete >= ldPackLow  AND Complete <= ldPackHigh
        *B604153,1 Add the checking of date [End]
         
          lnSum = lnSum + TotQty
          IF SEEK (Order+STR(LineNo,6),'InvLine')
            SELECT InvLine
            SCAN WHILE Order+STR(LineNo,6)=OrdLine.Order+STR(OrdLine.LineNo,6) FOR InvHdr.Status <> 'V'
              lnSum = lnSum + TotQty
            ENDSCAN
          ENDIF
        ENDIF    
      ENDSCAN     
      lnRangeVal[12] = lnRangeVal[12] + lnSum
        
    ENDIF

    *-- This section gets from first pack till beginning of packs [Begin]
    IF !EMPTY(lcRpBreak)

      SELECT OrdLine
      =SEEK(lcStyle)
      SCAN WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = lcStyle FOR OrdHdr.Status <> 'X'
        IF !EMPTY(lcRpStart) AND Complete <= ldBreakDat AND IIF(!EMPTY(lcRpStart),Complete > ldBegnDate,.T.)

          *B604153,1 Add the checking of date [Begin]
          IF Complete >= ldPackLow  AND Complete <= ldPackHigh
          *B604153,1 Add the checking of date [End]
 
            lnThru = lnThru + TotQty
            IF SEEK (Order+STR(LineNo,6),'InvLine')
              SELECT InvLine
              SCAN WHILE Order+STR(LineNo,6)=OrdLine.Order+STR(OrdLine.LineNo,6) FOR InvHdr.Status <> 'V'
                lnThru = lnThru + TotQty
              ENDSCAN
            ENDIF
        
          ENDIF    
        
        ELSE
          IF EMPTY(lcRpStart)
            SCAN WHILE Style+DTOS(complete)+cOrdType+Order+Store+STR(LineNo,6) = Style AND Complete <= ldBreakDat FOR OrdHdr.Status <> 'X'

              *B604153,1 Add the checking of date [Begin]
              IF Complete >= ldPackLow  AND Complete <= ldPackHigh
              *B604153,1 Add the checking of date [End]

                lnThru = lnThru + TotQty
                IF SEEK (Order+STR(LineNo,6),'InvLine')
                  SELECT InvLine
                  SCAN WHILE Order+STR(LineNo,6)=OrdLine.Order+STR(OrdLine.LineNo,6) FOR InvHdr.Status <> 'V'
                    lnThru = lnThru  + TotQty
                  ENDSCAN
                ENDIF
              ENDIF    
            ENDSCAN     
          ENDIF
        ENDIF
      ENDSCAN  
    ENDIF    
    *-- This section gets from first pack till beginning of packs [End]

    lnTot = lnTot + lnThru
    ROW = ROW + 1
    @ ROW,000 SAY ALLTRIM(lcOnlySty)

    IF EMPTY(lcRpStart)
      *@ ROW,009 SAY ALLTRIM(lcColor)
      @ ROW,008 SAY ALLTRIM(lcColor)
      
      *B604998,1 Always print Lot no. [Begin]
      IF lnLastLot <> 0
        @ ROW,016 SAY lnLastLot PICTURE '99'
      ENDIF
      *B604998,1 Always print Lot no. [End]
      
      IF !EMPTY(lcRpBreak)
      
        *B604998,1 Shift the following. [Begin]
        *@ ROW,17 SAY lnThru  PICTURE '999999'
        @ ROW,20 SAY lnThru  PICTURE '999999'
        *B604998,1 Shift the following. [End]

      ENDIF  
      lnColumn = 30
    ELSE
      @ ROW,008 SAY ALLTRIM(lcColor)

      *B604998,1 Always print Lot no. and shift the following. [Begin]
      *@ ROW,15 SAY lnSum PICTURE '999999'
      IF lnLastLot <> 0
        @ ROW,016 SAY lnLastLot PICTURE '99'
      ENDIF      
      @ ROW,19 SAY lnSum PICTURE '999999'
      *B604998,1 Always print Lot no. and shift the following. [End]
      
      IF !EMPTY(lcRpBreak)

        *B604998,1 Shift the following. [Begin]
        *@ ROW,23 SAY lnThru  PICTURE '999999'
        @ ROW,26 SAY lnThru  PICTURE '999999'
        *B604998,1 Shift the following. [End]
        
      ENDIF  
      lnColumn = IIF(EMPTY(lcRpBreak),30,34)
    ENDIF
    lnPlanTot = lnPlanTot + IIF(SEEK(lcStyle,'Style'),Style.TotPlan,0)

    lnTotSal = lnTotSal + lnSum + lnThru
    lnRangeVal[12] = 0

    *B604885,1 Save current file name. [Begin] 
    SELECT Packs
    *B604885,1 Save current file name. [End] 

    FOR lnX = 1 TO lnMaxPack

      *B604885,1 change all the seek statememt. [Begin]
      *IF SEEK(lcRpYear + laPack(lnX),'Packs')
      LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX)
      IF FOUND()
      *B604885,1 change all the seek statememt. [End]
      
        ldCurDate = Packs.dSale_Date
      ENDIF
      
      *B604885,1 change all the seek statememt. [Begin]
      *IF lnX > 1 AND SEEK(lcRpYear + laPack(lnX-1),'Packs')
      llFound = .F.
      IF lnX > 1
        LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX-1)
        llFound = FOUND()
      ENDIF  
      IF llFound
      *B604885,1 change all the seek statememt. [End]      
      
        ldPrevDate = Packs.dSale_Date
      ENDIF
      IF (!EMPTY(lcRpStart) AND ldCurDate < ldBegnDate);
        OR (!EMPTY(lcRpBreak) AND ldCurDate < ldBreakDat);
        OR (lnX >1 AND ldCurDate < ldPrevDate)
        EXIT
      ENDIF
      @ ROW,lnColumn SAY lnRangeVal(lnX) PICTURE '999999'
      lnRangeVal[12] = lnRangeVal[12] + lnRangeVal(lnX)
      lnTotSal       = lnTotSal + IIF(VAL(lcRpStart)<52,lnRangeVal(lnX),0)
      lnColumn       = lnColumn + 7
    ENDFOR
    
    @ ROW,111 SAY lnTotWip PICTURE '999999'
    @ ROW,117 SAY IIF(SEEK(lcStyle,'Style'),Style.TotPlan,SPACE(07))  PICTURE '9999999'
    @ ROW,125 SAY lnTotSal PICTURE '9999999'
    lnTotPack[12] = lnTotPack[12] + lnRangeVal[12]
    lnTotal    = lnTotal + lnTotSal
  
    *-- Storing subtotals and totals
    FOR lnX = 1 TO 11
      lnTotPack(lnX) = lnTotPack(lnX) + lnRangeVal(lnX)
    ENDFOR

    *-- To accumulate the subtotal & grandtotal
    IF !EMPTY(lcRpStart)
      lnTotSum = lnTotSum + lnSum
      lnSum = 0
    ENDIF

    *-- End Calculations & Print , prepare for next line
    STORE 0 TO lnRangeVal , lnTotSal
    
    *B604153,1 Change work area [Begin]                
    *IF !EOF(lcWorkFile)
    *  SELECT (lcWorkFile)
    *  SKIP
    *ENDIF
    
    IF !EOF('Style')
      SELECT Style
      SKIP
    ENDIF
      
    IF !(Style.cStyMajor == &lcWorkFile..cStyMajor)
      IF !EOF(lcWorkFile)
        SELECT (lcWorkFile)
        SKIP
        IF EOF(lcWorkFile)
          EXIT        		
        ENDIF          
      ENDIF
    ENDIF
    *B604153,1 Change work area [End]
    
  ENDDO

  *-- Printing Totals 
  ROW = ROW + 1
  @ ROW,000 SAY REPLICATE('*',132)
  ROW = ROW + 1

  IF EMPTY(lcRpStart)                   
    @ ROW,000 SAY '*SEASON TOTAL*'
    IF !EMPTY(lcRpBreak)

      *B604998,1 Shift the following. [Begin]
      *@ ROW,16 SAY lnTot  PICTURE '9999999'
      @ ROW,19 SAY lnTot  PICTURE '9999999'
      *B604998,1 Shift the following. [End]
      
    ENDIF  
    lnColumn = 30
  ELSE
    @ ROW,000 SAY '*SEASON TOT*'

    *B604998,1 Shift the following. [Begin]
    *@ ROW,015 SAY lnTotSum PICTURE '999999'
    @ ROW,019 SAY lnTotSum PICTURE '999999'
    *B604998,1 Shift the following. [End]

    IF !EMPTY(lcRpBreak)
      
      *B604998,1 Shift the following. [Begin]
      *@ ROW,22 SAY lnTot PICTURE '9999999'
      @ ROW,25 SAY lnTot PICTURE '9999999'
      *B604998,1 Shift the following. [End]
      
    ENDIF  
    lnColumn = IIF(EMPTY(lcRpBreak),30,34)
  ENDIF

  *B604885,1 Save current file name. [Begin] 
  SELECT Packs
  *B604885,1 Save current file name. [End] 

  FOR lnX = 1 TO lnMaxPack

    *B604885,1 change all the seek statememt. [Begin]
    *IF SEEK(lcRpYear + laPack(lnX),'Packs')
    LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX)
    IF FOUND()
    *B604885,1 change all the seek statememt. [End]
        
      ldCurDate = Packs.dSale_Date
    ENDIF

    *B604885,1 change all the seek statememt. [Begin]
    *IF lnX > 1 AND SEEK(lcRpYear + laPack(lnX-1),'Packs')
    llFound = .F.
    IF lnX > 1
      LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = laPack(lnX-1)     
      llFound = FOUND()
    ENDIF      
    IF llFound
    *B604885,1 change all the seek statememt. [End]
    
      ldPrevDate = Packs.dSale_Date
    ENDIF
    IF (!EMPTY(lcRpStart) AND ldCurDate < ldBegnDate);
      OR (!EMPTY(lcRpBreak) AND ldCurDate < ldBreakDat);
      OR (lnX >1 AND ldCurDate < ldPrevDate)
      EXIT
    ENDIF
    @ ROW,lnColumn SAY lnTotPack(lnX) PICTURE '999999'
    lnColumn = lnColumn + 7
  ENDFOR
  
  @ ROW,110 SAY lnTotlWip PICTURE '9999999'
  @ ROW,117 SAY lnPlanTot PICTURE '9999999'
  @ ROW,125 SAY lnTotal   PICTURE '9999999'

  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('*',132)
  ROW = ROW + 2
  
  SELECT (lcTmpPacks)
  IF !EOF()
    SKIP
  ENDIF
ENDDO

RETURN 
*-- End of lfPrintRep.
*-- End of Report.

*!**************************************************************************
*! Name      : lfPackInfo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Check for packs in packs file.
*!**************************************************************************
*! Example   : = lfPackInfo()
*!**************************************************************************
*
FUNCTION lfPackInfo
PRIVATE lcAlias
lcAlias = ALIAS()                   && Save current alias.

SELECT Packs

*B604885,1 Check if Low date found in Packs file. [Begin]
LOCATE FOR dSale_Date = &lcTmpPacks..dLowDate
IF EOF()
  
  *B604998,1 Remove directing the output to screen. [Begin]
  *SET DEVICE TO SCREEN
  *B604998,1 Remove directing the output to screen. [End]
  
  *B604885,1 Return to select another season. [Begin]
  *llContinue = .F.
  *RETURN llContinue
  RETURN .F.
  *B604885,1 Return to select another season. [End]
  
ENDIF
SET ORDER TO TAG cYear
*B604885,1 Check if Low date found in Packs file. [End]

*604998,1 Initialize ldLastDate. [Begin]
ldLastDate = {}
*604998,1 Initialize ldLastDate. [End]

DO CASE
  CASE !EMPTY(lcRpStart) AND EMPTY(lcRpBreak)
    
    *B604885,1 get first pack entered in option grid without year. [Begin] 
    *SEEK lcRpYear + lcRpStart
    LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = lcRpStart
    *B604885,1 get first pack entered in option grid without year. [End]

    *B604885,1 check for records match the locate expr. [Begin]
    *lcFromPack = cPack
    *ldBegnDate = dSale_Date
    *SKIP
    *IF EOF() OR !(cYear == lcRpYear)
    *  SEEK lcRpYear + lcRpStart
    *ENDIF
    lcFromPack = ''
    ldBegnDate = {}
    IF FOUND()
      lcFromPack = cPack
      ldBegnDate = dSale_Date
    ENDIF
    IF !EOF()
      SKIP
    ENDIF  
    *B604885,1 check for records match the locate expr. [End]
    
  CASE !EMPTY(lcRpBreak)
    IF !EMPTY(lcRpStart)
    
      *B604885,1 get first pack entered in option grid without year. [Begin]
      *SEEK lcRpYear + lcRpStart
      *lcFromPack = lcRpStart
      *ldBegnDate = dSale_Date
      LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = lcRpStart
      lcFromPack = ''
      ldBegnDate = {}
      IF FOUND()
        lcFromPack = lcRpStart
        ldBegnDate = dSale_Date
      ENDIF  
      *B604885,1 get first pack entered in option grid without year. [End]

    ENDIF
    
    *B604885,1 get last pack entered in option grid without year. [Begin]
    *SEEK lcRpYear + lcRpBreak
    LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate AND cPack = lcRpBreak
    *B604885,1 get last pack entered in option grid without year. [End]

    *B604885,1 get first pack entered in option grid without year. [Begin]
    *SKIP
    *IF EOF() OR !(cYear == lcRpYear)
    *  SEEK lcRpYear + lcRpStart
    *ENDIF
    IF FOUND()
      ldBreakDat = dSale_Date
    ENDIF  
    IF !EOF()
      SKIP
    ENDIF  
    *B604885,1 get first pack entered in option grid without year. [End]
        
  OTHERWISE

    *B604885,1 get first pack entered in option grid without year. [Begin]
    *SEEK lcRpYear + &lcTmpPacks..cLowPack
    LOCATE FOR dSale_Date >= &lcTmpPacks..dLowDate
    *B604885,1 get first pack entered in option grid without year. [End]
    
ENDCASE

*B604885,1 Commented out. [Begin]
*IF EOF()
*  *-- Message <Information not found in Packs file!>
*  *-- Buttons <                 OK                 >
*  =gfModalGen("TRM000000B00000","DIALOG",'','',"Information not found in Packs file!.")
*  SET DEVICE TO SCREEN
*  llContinue = .F.
*  RETURN llContinue 
*ENDIF
*B604885,1 Commented out. [End]

FOR lnX = 1 TO 52

  *B604885,1 Change seek statement. [Begin] 
  *IF EOF() OR !(cYear == lcRpYear)
  *  SEEK lcRpYear
  *ENDIF  
  IF EOF()
    LOCATE FOR dSale_Date = &lcTmpPacks..dLowDate
  ENDIF
  *B604885,1 Change seek statement. [End]
  
  laPack(lnX) = cPack
  laDate(lnX) = dProd_Date
    
  *IF ldHighDate = dSale_Date
  IF &lcTmpPacks..dHighDate = dSale_Date
    lnMaxPack = lnX
    EXIT
  ENDIF
  SKIP
ENDFOR

IF lnMaxPack = 0
  *-- Message <OrdLine file has inconsistent pack information!.>
  *-- Buttons <                       OK                       >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"OrdLine file has inconsistent pack information!.")
  
  *B604998,1 Remove directing the output to screen. [Begin]
  *SET DEVICE TO SCREEN
  *B604998,1 Remove directing the output to screen. [End]

  *B604885,1 Return to select another season. [Begin]
  *llContinue = .F.
  *RETURN llContinue
  RETURN .F.
  *B604885,1 Return to select another season. [End]

ENDIF

IF lnMaxPack >= 12                && Control for printing 11 Columns
  lnMaxPack = 11
ENDIF

*B604998,1 Store last pack date. [Begin]
LOCATE FOR dProd_Date = laDate[lnMaxPack]
IF FOUND()
  ldLastDate = dSale_Date
ENDIF
*B604998,1 Store last pack date. [End]

*B604885,1 Return with True. [Begin]
llContinue = .T.
*B604885,1 Return with True. [End]

SELECT (lcALIAS)                  && Restore old alias
*-- End of lfPackInfo.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnSeasonPo = lfItmPos('STYLE.SEASON')  && get Rep code Fixed filter Position.
lnStylePos = lfItmPos('STYLE.CSTYMAJOR')  && Style Position

*B604153,1 SSE Commented out [Begin]
*lcInLstExp = IIF(EMPTY(laOGFxFlt[lnSeasonPo,6]),"SPACE(0)",;
*             [']+STRTRAN(laOGFxFlt[lnSeasonPo,6],"|",[','])+['])
*B604153,1 SSE Commented out [End]
             
*B604153,1 Open Style file again with another alias [Begin]
USE (gcDataDir+'Style') AGAIN ALIAS StyleFil ORDER TAG CStyle IN 0
*B604153,1 Open Style file again with another alias [End]

*SELECT FisHd
*LOCATE FOR cFisYStat = "C"
*IF !EOF()
*  lcRpYear = cFisFYear
*ENDIF

*B604885,1 Commented out. [Begin]
*lcRpYear = ALLTRIM(STR(YEAR(gdSysDate)))
*B604885,1 Commented out. [End]
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil

*B604153,1 No need to use Temp Style file [Begin]
*DIMENSION laArrStru[1]
*SELECT Style
*=AFIELDS(laArrStru)

*-- Creating a temporary cursor to hold the records that will be printed.
*CREATE CURSOR (lcWorkFile) FROM ARRAY laArrStru
*ZAP
*INDEX ON Style TAG (lcWorkFile)
*B604153,1 No need to use Temp Style file [End]

*-- Creating a temporary cursor to hold the Packs lower , higher date for each season.
CREATE CURSOR (lcTmpPacks) (cSeason C(6), cLowPack C(2), dLowDate D(8), dHighDate D(8))
ZAP
INDEX ON cSeason TAG (lcTmpPacks)
*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Evaluate NonMajor Type and variables.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Example     : = lfEvalSegs()
*!**************************************************************************
*
FUNCTION lfEvalSegs

*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

*-- Compute Free/Color Items in Style Structure. [Begin]
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = 1 TO ALEN(laMajSeg,1)
  DO CASE
    CASE laMajSeg[lnI,1] = "F"            && Free
      lcMajTtl   = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnStyleLen = LEN(laMajSeg[lnI,3])
      lnStyStPos = laMajSeg[lnI,4]
    CASE laMajSeg[lnI,1] = "C"            && Color 
      lnClrPo    = laMajSeg[lnI,4]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnColorLen = LEN(laMajSeg[lnI,3])
    CASE laMajSeg[lnI,1] = "Z"            && Season
      lnSeasLen = LEN(laMajSeg[lnI,3])      
  ENDCASE
ENDFOR    
RETURN ''
*-- End of lfEvalSegs.

*!***************************************************************************
*! Name      : lfvYear
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Validate Year.
*!***************************************************************************
*! Example   : = lfvYear()
*!***************************************************************************
*B604885,1 This function is not used anymore as we removed the Year field from Option grid.
FUNCTION lfvYear
IF !SEEK(lcRpYear,'Packs')
  *-- Message <This year is not found in the Packs file.>
  *-- Buttons <                     OK                    >
  =gfModalGen("TRM000000B00000","DIALOG",'','',;
              "This year is not found in the Packs file.")
  lcRpYear = laOldVal
ENDIF
*-- End of lfvYear.

*!***************************************************************************
*! Name      : lfvPack
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Validate Break pack number.
*!***************************************************************************
*! Example   : = lfvPack()
*!***************************************************************************
*
FUNCTION lfvPack
PRIVATE lcVar , lcObj , lcYearPack

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*B604885,1 Commented out. [Begin]
*IF !EMPTY(lcObj) AND !SEEK(lcRpYear + lcObj , 'Packs')  
*  lcYearPack = ''
*  DO gfBrowPk WITH lcYearPack

  *-- IF The user selected a record
*  IF !EMPTY(lcYearPack)
*    lcObj    = SUBSTR(lcYearPack,5)       && to take only Pack no.
*    lcRpYear = SUBSTR(lcYearPack,1,4)     && to take only Year.
*    =lfOGShowGet('lcRpYear')
*  ELSE    && Else
*    lcObj = laOldVal
*  ENDIF    && End of IF
*  &lcVar = lcObj      && Update the field
*B604885,1 Commented out. [End]
  
  *B604885,1 Pad value with Zero if less than 10. [Begin]
  IF EMPTY(lcObj)
    RETURN
  ENDIF
  
  lcObj = PADL(ALLTRIM(lcObj),2,'0')
  IF BETWEEN(lcObj,'01','52')
    &lcVar = lcObj      && Update the field
  ELSE
    *-- Message <Pack number must be greater than 01 and less than 52.>
    *-- Buttons <                             OK                             > 
    =gfModalGen("TRM000000B00000","DIALOG",'','',;
                "Pack number must be greater than 01 and less than 52.")
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  *B604885,1 Pad value with Zero if less than 10. [End]
  
  
  IF !EMPTY(lcRpBreak) AND lcRpBreak < lcRpStart
    *-- Message <Break pack number can not be less than Starting pack number.>
    *-- Buttons <                             OK                             > 
    =gfModalGen("TRM000000B00000","DIALOG",'','',;
                "Break pack number can not be less than Starting pack number.")
    _CUROBJ = _CUROBJ
  ENDIF

*B604885,1 Commented out. [Begin]
*ENDIF    && End of IF
*B604885,1 Commented out. [End]
*-- End of lfvPack.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : When function to save the Old value.
*!**************************************************************************
*! Called from : Starting & Break Pack no.
*!**************************************************************************
*! Example     : = lfwOldVal()
*!**************************************************************************
*
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- End of lfwOldVal.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
*
FUNCTION lfSRVSty
PARAMETERS lcParm
PRIVATE lcOldExp
DO CASE
  CASE lcParm = 'S'  && Set code
  
    *B604153,1 SSE Commented out [Begin]
    *lcInLstExp = IIF(EMPTY(laOGFxFlt[lnSeasonPo,6]),"SPACE(0)",;
    *             [']+STRTRAN(laOGFxFlt[lnSeasonPo,6],"|",[','])+['])
    *lcOldExp = lcBrwFltExp
    *lcBrwFltExp = ''    
    *lcBrwFltExp = SUBSTR(lcOldExp,ATC([,],lcOldExp)+1,LEN(lcOldExp))
    *lcBrwFltExp = SUBSTR(lcBrwFltExp,1,LEN(lcBrwFltExp)-1)
    *lcBrwFltExp = STRTRAN(lcOldExp,lcBrwFltExp,[']+STRTRAN(laOGFxFlt[lnSeasonPo,6],"|",[','])+['])
    *B604153,1 SSE Commented out [End]
  
    llClearSty = .T.
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    LOCATE 

  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    *SELECT STYLE
    *SET ORDER TO TAG Style
    llClearSty = .F.
ENDCASE
*-- End of lfsrvSty.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Generate Expression
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*B604153,1 This function is not used anymore.
PROCEDURE lpGenExpr
PRIVATE lcAlias , lcOldExp
lcAlias = ALIAS()

*-- If user selected Style from Style InList [Begin]
IF USED(laOGFxFlt[lnStylePos,6]) AND RECCOUNT(laOGFxFlt[lnStylePos,6]) > 0 
    
  *-- Copy all laOGFxFlt to another array to save the old value.
  DIMENSION laTempExpr[1]
  =ACOPY(laOGFxFlt,laTempExpr)

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  STORE "" TO lcOldExp , laOGFxFlt
  
  PRIVATE lcBrowPos    

  *-- Copy all Browse Filter array which is used in case of INLIST 
  *-- to another temp array to save the old value.
  DIMENSION laBrTmpFlt[1]
  =ACOPY(laBrFldFlt,laBrTmpFlt)

  *-- resize Browse filter array to hold one browse expression only.
  DIMENSION laBrFldFlt[1,5]
  laBrFldFlt = ""

  *-- Copy the Account expression to laOGFxFlt
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStylePos,1),7)
  
  *-- Convert numeric position to string.
  lcBrowPos = ALLTRIM(STR(lnStylePos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
      EXIT
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = 'laOGFxFlt:1' 
  
  *-- Generate expression for Style.
  lcStyleExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
  *-- Replace Style Expr in Report Expression with the new Style Expr
  *lcStyleExp = STRTRAN(lcRpExp,lcOldExp,lcStyleExp)

  *-- Restore original laBrFldFlt
  DIMENSION laBrFldFlt[1]
  =ACOPY(laBrTmpFlt,laBrFldFlt)  
  
    *-- Restore original laOGFxFlt
  DIMENSION laOGFxFlt[1]
  =ACOPY(laTempExpr,laOGFxFlt)
ENDIF
*-- If user selected Style from Style InList [End]

SELECT (lcAlias)
*-- End of lpGenExpr.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Returns   : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
*
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
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

*!***************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/22/2000
*! Purpose   : Clear Report function.
*!***************************************************************************
*! Example   : =lfClearRep()
*!***************************************************************************
*B604153,1 
FUNCTION lfClearRep
IF USED('StyleFil')
  USE IN StyleFil
ENDIF
*-- End of lfClearRep.
