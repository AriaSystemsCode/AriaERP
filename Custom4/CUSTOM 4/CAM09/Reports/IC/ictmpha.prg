*:***************************************************************************
*: Program file       : ICTMPHA
*: Program desc.      : Time-Phased Availability Report
*: Module             : Inventory Control (IC)
*: Developer          : Mariam Mazhar(MMT)
*: Tracking Job Number: Custom(126957)
*: Date               : 01/23/2006
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfItemMask,gfCrtTmp.
*:***************************************************************************
*: Passed Parameters : None
*:***************************************************************************
*: Example : DO ICTMPHA
*:***************************************************************************
loogScroll.cCROrientation = 'P'
*-If anything in report changes
IF loOgScroll.llOGFltCh
  =lfcrtTemp()
  =lfCollect()
ENDIF 

*-Function to adjust the crystal arrays
=lfAdjustCRSettings(ldRpEndDat)

SELECT (lcTempfile )
LOCATE 
IF EOF()
  =gfModalGen('TRM00052B34000','ALERT')
  RETURN .F.
ENDIF 

*--dispaly report
gfDispRe()
RETURN 
*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : Collect data 
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return    : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollect

WAIT WINDOW "Collecting Report Data... " NOWAIT 
lcFinalStyles = lfCheckfilters()

lnLastDay = 0
lnLastDay = lfGetLastDay(ldRpEndDat)

*get the first day of month 
dFirstdayMonth = DATE(YEAR(ldRpEndDat),MONTH(ldRpEndDat),1)
dFirstdayMonthLY = DATE(YEAR(ldRpEndDat)-1,MONTH(ldRpEndDat),1)

*Period1*
dStarting1 = ldRpEndDat
dEnding1  = DATE(YEAR(ldRpEndDat),MONTH(ldRpEndDat),VAL(lnLastDay))
dEnding1LY =   DATE(YEAR(ldRpEndDat)-1,MONTH(ldRpEndDat),VAL(lnLastDay))

*Period2*
dStarting2 = GOMONTH(dFirstdayMonth ,1)
lnLastDay2 = lfGetLastDay(dStarting2)
dEnding2 = DATE(YEAR(dStarting2),MONTH(dStarting2),VAL(lnLastDay2))

*Period3*
dStarting3 = GOMONTH(dFirstdayMonth ,2)
lnLastDay3 = lfGetLastDay(dStarting3)
dEnding3   = DATE(YEAR(dStarting3),MONTH(dStarting3),VAL(lnLastDay3))

*Period4*
dStarting4 = GOMONTH(dFirstdayMonth ,3)
lnLastDay4 = lfGetLastDay(dStarting4)
dEnding4   = DATE(YEAR(dStarting4),MONTH(dStarting4),VAL(lnLastDay4))

*Period5*
dStarting5 = GOMONTH(dFirstdayMonth ,4)
lnLastDay5 = lfGetLastDay(dStarting5)
dEnding5   = DATE(YEAR(dStarting5),MONTH(dStarting5),VAL(lnLastDay5))

*Period6*
dStarting6 = GOMONTH(dFirstdayMonth ,5)
lnLastDay6 = lfGetLastDay(dStarting6)
dEnding6   = DATE(YEAR(dStarting6),MONTH(dStarting6),VAL(lnLastDay6))

*Period7*
dStarting7 = GOMONTH(dFirstdayMonth ,6)
lnLastDay7 = lfGetLastDay(dStarting7)
dEnding7   = DATE(YEAR(dStarting7),MONTH(dStarting7),VAL(lnLastDay7))

dFirstDayYear = DATE(YEAR(ldRpEndDat),1,1)
dLastDayYear  = DATE(YEAR(ldRpEndDat),12,VAL(lfGetLastDay(DATE(YEAR(ldRpEndDat),12,12))))

dFirstDayYearLy = DATE(YEAR(ldRpEndDat)-1,1,1)
dLastDayYearLy  = DATE(YEAR(ldRpEndDat)-1,12,VAL(lfGetLastDay(DATE(YEAR(ldRpEndDat),12,12))))

lcFldName = ""
llAppen  = .F.

SELECT(lcFinalStyles)
SCAN 
  loDBFScale.Seek('S'+&lcFinalStyles..Scale,"Scale")
  *Sold PTD field 
  IF loDBFInvline.Seek(&lcFinalStyles..Style)
    SELECT invline 
    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) =&lcFinalStyles..Style FOR ;
      loDBFinvhdr.Seek(invline.invoice) AND BETWEEN(invhdr.shipdate,dFirstdayMonth,dEnding1)
      FOR i = 1 TO Scale.cnt 
        SELECT (lcTempfile )
        IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + Invline.cwarecode)   
          APPEND BLANK 
          REPLACE Style    WITH &lcFinalStyles..CstyMajor,;
                  Desc     WITH &lcFinalStyles..Desc,;
                  Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
                  SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
                  WAREHOUS WITH Invline.cwarecode
                  REPLACE SOLDPTD WITH SOLDPTD + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))
           ELSE
             REPLACE SOLDPTD WITH SOLDPTD + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))       
        ENDIF 
      ENDFOR       
    ENDSCAN     
  ENDIF 
  
  *Sold YTD field
  IF loDBFInvline.Seek(&lcFinalStyles..Style)
    SELECT INVLINE 
    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) =&lcFinalStyles..Style FOR ;
      loDBFinvhdr.Seek(invline.invoice) AND BETWEEN(invhdr.shipdate,dFirstDayYear ,dLastDayYear)
      FOR i = 1 TO Scale.cnt 
 
        SELECT (lcTempfile )
        IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + InvLine.cwarecode)   
          APPEND BLANK 
          REPLACE Style    WITH &lcFinalStyles..CstyMajor,;
                  Desc     WITH &lcFinalStyles..Desc,;
                  Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
                  SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
                  WAREHOUS WITH Invline.cwarecode
                  REPLACE soldytd WITH soldytd + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))
           ELSE
             REPLACE soldytd WITH soldytd + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))       
        ENDIF 
      ENDFOR       
    ENDSCAN     
  ENDIF 
  
  *LYR Sold PTD
    IF loDBFInvline.Seek(&lcFinalStyles..Style)
    SELECT invline 
    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) =&lcFinalStyles..Style FOR ;
      loDBFinvhdr.Seek(invline.invoice) AND BETWEEN(invhdr.shipdate,dFirstdayMonthLY,dEnding1LY)
      FOR i = 1 TO Scale.cnt 
        SELECT (lcTempfile )
        IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + InvLine.cwarecode)   
          APPEND BLANK 
          REPLACE Style    WITH &lcFinalStyles..CstyMajor,;
                  Desc     WITH &lcFinalStyles..Desc,;
                  Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
                  SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
                  WAREHOUS WITH Invline.cwarecode
                  REPLACE LYRSOLDP WITH LYRSOLDP + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))
           ELSE
             REPLACE LYRSOLDP WITH LYRSOLDP + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))       
        ENDIF 
      ENDFOR       
    ENDSCAN     
  ENDIF 

  *LYR Sold YTD
  IF loDBFInvline.Seek(&lcFinalStyles..Style)
    SELECT invline 
    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) =&lcFinalStyles..Style FOR ;
      loDBFinvhdr.Seek(invline.invoice) AND BETWEEN(invhdr.shipdate,dFirstDayYearLy ,dLastDayYearLy  )
      FOR i = 1 TO Scale.cnt 
        
        SELECT (lcTempfile )
        IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + InvLine.cwarecode)   
          APPEND BLANK 
          REPLACE Style    WITH &lcFinalStyles..CstyMajor,;
                  Desc     WITH &lcFinalStyles..Desc,;
                  Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
                  SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
                  WAREHOUS WITH Invline.cwarecode
          REPLACE LYRSOLDY WITH LYRSOLDY + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))
        ELSE
          REPLACE LYRSOLDY WITH LYRSOLDY + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))       
        ENDIF 
      ENDFOR       
    ENDSCAN     
  ENDIF 

***********************************************************
*!*    lcFldName = ""
*!*    llAppen  = .F.
*!*    loDBFScale.Seek('S'+&lcFinalStyles..Scale,"Scale")
*!*    IF loDBFInvline.Seek(Style)
*!*      SELECT invline 
*!*      SCAN REST WHILE STYLE + INVOICE + STR(LINENO,6) = &lcFinalStyles..Style 
*!*        IF loDBFinvhdr.Seek(invline.invoice) 
*!*          FOR i = 1 TO Scale.cnt 
*!*            DO CASE 
*!*              CASE BETWEEN(invhdr.shipdate,dFirstdayMonth,dEnding1)  &&Sold PTD field 
*!*                lcFldName = 'SOLDPTD' 
*!*                llAppen  = .T.

*!*   
*!*              CASE BETWEEN(invhdr.shipdate,dFirstDayYear ,dLastDayYear)  &&Sold YTD field
*!*                lcFldName = 'SOLDYTD' 
*!*                llAppen  = .T.

*!*           
*!*              CASE BETWEEN(invhdr.shipdate,dFirstdayMonthLY,dEnding1LY)        &&  *LYR Sold PTD
*!*                lcFldName = 'LYRSOLDP' 
*!*                llAppen  = .T.

*!*              
*!*              CASE BETWEEN(invhdr.shipdate,dFirstDayYearLy ,dLastDayYearLy) &&  *LYR Sold YTD
*!*                lcFldName = 'LYRSOLDY' 
*!*                llAppen  = .T.

*!*            ENDCASE 
*!*            IF llAppen
*!*              SELECT (lcTempfile)
*!*              IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + InvLine.cwarecode)
*!*                APPEND BLANK
*!*                REPLACE Style    WITH &lcFinalStyles..CstyMajor,;
*!*                        Desc     WITH &lcFinalStyles..Desc,;
*!*                        Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
*!*                        SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
*!*                        WAREHOUS WITH Invline.cwarecode
*!*                REPLACE &lcFldName  WITH &lcFldName + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))
*!*              ELSE
*!*                REPLACE &lcFldName  WITH &lcFldName + EVALUATE("Invline.QTY"+ALLTRIM(STR(i)))
*!*              ENDIF 
*!*            ENDIF 
*!*          ENDFOR        
*!*        ENDIF 
*!*      ENDSCAN 
*!*    ENDIF 

  *--ON hand will be calculated for the first period only 
  IF loDBFStyInvJl.Seek(&lcFinalStyles..Style,"StyInvJl")
    SELECT ('StyInvJl')
    SCAN REST WHILE  STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) =&lcFinalStyles..Style FOR ;
       INLIST(CIRTYPE,'I','R') AND BETWEEN(dtrdate,dStarting1,dEnding1)

      FOR i = 1 TO Scale.cnt 
        SELECT (lcTempfile )
        
        IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + StyInvJl.cwarecode)   
          APPEND BLANK 
          REPLACE Style  WITH &lcFinalStyles..CstyMajor,;
                  Desc     WITH &lcFinalStyles..Desc,;
                  Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
                  SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
                  WAREHOUS WITH Styinvjl.cwarecode
        IF STYINVJL.CIRTYPE = "R"
          REPLACE PER1ONHAND WITH  PER1ONHAND + EVALUATE("Styinvjl.Nstk"+ALLTRIM(STR(i)))      
        ELSE
          REPLACE PER1ONHAND WITH  PER1ONHAND - ABS(EVALUATE("Styinvjl.Nstk"+ALLTRIM(STR(i))))      
        ENDIF       

      ELSE
      
        IF STYINVJL.CIRTYPE = "R"
          REPLACE PER1ONHAND WITH  PER1ONHAND + EVALUATE("Styinvjl.Nstk"+ALLTRIM(STR(i)))      
        ELSE
          REPLACE PER1ONHAND WITH  PER1ONHAND - ABS(EVALUATE("Styinvjl.Nstk"+ALLTRIM(STR(i))))      
        ENDIF       
      ENDIF       
    ENDFOR   
  ENDSCAN 
ENDIF  

  lcFldName = ""
  llAppen  = .F.

  IF loDBFposln.Seek('0001'+&lcFinalStyles..Style+'P'+'P')
    SELECT POSLN
    SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = "0001"+&lcFinalStyles..Style;
      FOR INLIST(posln.trancd,"1","2","4","5")
      lcFldName = ""
      llAppen  = .F.
      IF loDBFPoshdr.Seek("P"+"P"+POSLN.PO) 
        FOR i = 1 TO Scale.cnt 
          DO CASE 
            CASE BETWEEN(Poshdr.available,dStarting1,dEnding1)  &&ON PO will be calculated for the first period only 
              lcFldName = "PER1ONPO"
              llAppen  = .T.

            CASE BETWEEN(Poshdr.available,dStarting2,dEnding2)  &&ON PO will be calculated for the 2nd period only 
              lcFldName = "PER2ONPO"
              llAppen  = .T.

          
            CASE BETWEEN(Poshdr.available,dStarting3,dEnding3)  &&ON PO will be calculated for the 3rd period only 
              lcFldName = "PER3ONPO"
              llAppen  = .T.

            CASE BETWEEN(Poshdr.available,dStarting4,dEnding4)&&ON PO will be calculated for the 4th period only 
              lcFldName = "PER4ONPO"
              llAppen  = .T.

            CASE BETWEEN(Poshdr.available,dStarting5,dEnding5) &&ON PO will be calculated for the 5th period only 
              lcFldName = "PER5ONPO"
              llAppen  = .T.

            CASE BETWEEN(Poshdr.available,dStarting6,dEnding6)&&  *--ON PO will be calculated for the 6th period only 
              lcFldName = "PER6ONPO"
              llAppen  = .T.

            CASE Poshdr.available > dEnding6   &&PO will be calculated for the future period only 
              lcFldName = "PER7ONPO"
              llAppen  = .T.
          ENDCASE 
          IF llAppen  
            SELECT (lcTempfile )
            IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + posln.cwarecode)   
                APPEND BLANK 
                REPLACE Style    WITH &lcFinalStyles..CstyMajor,;
                        Desc     WITH &lcFinalStyles..Desc,;
                        Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
                        SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
                        WAREHOUS WITH posln.cwarecode
              IF VAL(posln.trancd) = 1
                  REPLACE &lcFldName  WITH &lcFldName + EVALUATE("POSLN.qty"+ALLTRIM(STR(i)))
              ELSE
                REPLACE &lcFldName  WITH &lcFldName  - EVALUATE("POSLN.qty"+ALLTRIM(STR(i)))
              ENDIF   
            ELSE
              IF VAL(posln.trancd) = 1
                  REPLACE &lcFldName  WITH &lcFldName + EVALUATE("POSLN.qty"+ALLTRIM(STR(i)))
              ELSE
                REPLACE &lcFldName  WITH &lcFldName  - EVALUATE("POSLN.qty"+ALLTRIM(STR(i)))
              ENDIF   
            ENDIF   
          ENDIF 
        ENDFOR 
      ENDIF   
    ENDSCAN 
  ENDIF 

lcFldName = ""
llAppen  = .F.
IF loDBFordLine.Seek(&lcFinalStyles..Style)
  SELECT ordline 
  SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = &lcFinalStyles..Style 
    lcFldName = ""
    llAppen  = .F.
    IF loDBFordhdr.Seek("O"+ordline.order) 
      FOR i = 1 TO Scale.cnt  
      DO CASE 
        CASE BETWEEN(ORDHDR.START,dStarting1,dEnding1)&&ON So will be calculated for the first period only 
          lcFldName = "PER1ONSO"
          llAppen  = .T.
        
        CASE BETWEEN(ORDHDR.START,dStarting2,dEnding2)&&ON So will be calculated for the 2nd period only 
          lcFldName = "PER2ONSO"
          llAppen  = .T.

        CASE BETWEEN(ORDHDR.START,dStarting3,dEnding3)&& *--ON So will be calculated for the 3rd period only 
          lcFldName = "PER3ONSO"
          llAppen  = .T.

        CASE BETWEEN(ORDHDR.START,dStarting4,dEnding4)&&  *--ON So will be calculated for the 4th period only 
          lcFldName = "PER4ONSO"
          llAppen  = .T.

        CASE BETWEEN(ORDHDR.START,dStarting5,dEnding5)&&ON So will be calculated for the 5th period only 
          lcFldName = "PER5ONSO"
          llAppen  = .T.

        CASE BETWEEN(ORDHDR.START,dStarting6,dEnding6)&&ON So will be calculated for the 6th period only 
          lcFldName = "PER6ONSO"
          llAppen  = .T.
        
        CASE ORDHDR.START > dEnding6 &&ON So will be calculated for the Future period only 
          lcFldName = "PER7ONSO"
          llAppen  = .T.

        ENDCASE 
        IF llAppen  
          SELECT (lcTempfile )
          IF !SEEK(&lcFinalStyles..CstyMajor+SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)+ EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) + ORDLINE.cwarecode)   
            APPEND BLANK 
            REPLACE Style    WITH &lcFinalStyles..CstyMajor,;
                    Desc     WITH &lcFinalStyles..Desc,;
                    Color    WITH SUBSTR(&lcFinalStyles..STYLE,lnNonMajSt,lnColorLen)    ,;
                    SIZE     WITH EVALUATE("Scale.SZ" + ALLTRIM(STR(i))) ,;
                    WAREHOUS WITH ORDLINE.cwarecode
                   
            IF !EMPTY(EVALUATE("ORDLINE.QTY"+ALLTRIM(STR(i))))
              REPLACE &lcFldName WITH &lcFldName + EVALUATE("ordline.qty"+ALLTRIM(STR(i)))
            ENDIF 
          ELSE
            REPLACE &lcFldName WITH &lcFldName + EVALUATE("ordline.qty"+ALLTRIM(STR(i)))
          ENDIF    
        ENDIF 
      ENDFOR 
    ENDIF 
  ENDSCAN 
ENDIF 
ENDSCAN  
*!*************************************************************
*! Name      : lfCheckfilters
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : check filters and collect data from style file
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return    : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCheckfilters

*!*  style.Cstymajor
*!*  style.cdivision
*!*  style.season
*!*  style.cstygroup
*!*  style.royalty
*!*  style.style

lnMajPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF !EMPTY(loOgScroll.laOGFxFlt[lnMajPos,6]) AND USED(loOgScroll.laOGFxFlt[lnMajPos,6]) AND RECCOUNT(loOgScroll.laOGFxFlt[lnMajPos,6])>0
  DIMENSION laStruct[1,1]
  SELECT style
  AFIELDS(laStruct)
  lcMajFile = loogscroll.gftempname()
  gfCrtTmp(lcMajFile ,@laStruct,,"",.T.)
  lcSeleStyle = loOgScroll.laOGFxFlt[lnMajPos,6]
  SELECT(lcSeleStyle)
  SCAN
    loDBFStyle.Seek(&lcSeleStyle..cStyMajor)
    INSERT INTO (lcMajFile) SELECT * FROM STYLE  WHERE Style.cstymajor = &lcSeleStyle..cStyMajor
  ENDSCAN 
  lcStyle  =lcMajFile
ELSE
  lcStyle  = 'Style'  
ENDIF 
lcInnerJoin =  lcStyle  +".* FROM "+lcStyle  

*----CDIVISION----*
lnDivPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'STYLE.CDIVISION'),1)
IF !EMPTY(loOgScroll.laOGFxFlt[lnDivPos,6])
  lcDivFile = loogscroll.gftempname()
  lcField = lfConvertToCursor(lnDivPos,lcDivFile)
  lcInnerJoin = lcInnerJoin +" Inner join " + lcDivFile + " ON " + lcStyle + ".CDIVISION = "+lcDivFile+"."+lcField+"  " 
ENDIF 

*----SEASON----*
lnSEAPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'STYLE.SEASON'),1)
IF !EMPTY(loOgScroll.laOGFxFlt[lnSEAPos,6])
  lcSeaFile = loogscroll.gftempname()
  lcField = lfConvertToCursor(lnSeaPos,lcSeaFile)
  lcInnerJoin = lcInnerJoin +" Inner join " + lcSeaFile + " ON " + lcStyle + ".SEASON = "+lcSEAFile+"."+lcField+"  " 
ENDIF 

*----CstyGroup----*
lnGrpPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF !EMPTY(loOgScroll.laOGFxFlt[lnGrpPos,6])
  lcGrpFile = loogscroll.gftempname()
  lcField = lfConvertToCursor(lnGrpPos,lcGrpFile)
  lcInnerJoin = lcInnerJoin +" Inner join " + lcGrpFile + " ON " + lcStyle + ".cstygroup = "+lcGrpFile+"."+lcField+"  " 
ENDIF 

*----Royalty----*
lnRoyalPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'STYLE.ROYALTY'),1)
IF !EMPTY(loOgScroll.laOGFxFlt[lnRoyalPos ,6])
  lcRoyalFile = loogscroll.gftempname()
  lcField = lfConvertToCursor(lnRoyalPos ,lcRoyalFile )
  lcInnerJoin = lcInnerJoin +" Inner join " + lcRoyalFile  + " ON " + lcStyle + ".ROYALTY = "+lcRoyalFile +"."+lcField+"  " 
ENDIF 

*----Color----*
lnColorPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
IF !EMPTY(loOgScroll.laOGFxFlt[lnColorPos,6])
  lcColorFile = loogscroll.gftempname()
  lcField = lfConvertToCursor(lnColorPos ,lcColorFile )
  lcInnerJoin = lcInnerJoin +" Inner join " + lcColorFile  + " ON SUBSTR(" +lcStyle + ".STYLE,lnNonMajSt,lnColorLen)= " + lcColorFile +"."+lcField+"  " 
ENDIF 
lcFinalStyle = loogscroll.gfTempName()
SELECT &lcInnerJoin INTO CURSOR (lcFinalStyle )
RETURN (lcFinalStyle)

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : report when function
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return    : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwRepWhen
loDBFStyle    = CreateObject("RemoteTable","Style","Style","Style",SET("DATASESSION"),,.T.)
loDBFScale    = CreateObject("RemoteTable","Scale","Scale","Scale",SET("DATASESSION"),,.T.)
loDBFStyInvJl = CreateObject("RemoteTable","STYINVJL","STYINVJL","STYINVJL",SET("DATASESSION"),,.T.)

loDBFordhdr   = CreateObject("RemoteTable","ORDHDR","ORDHDR","ORDHDR",SET("DATASESSION"),,.T.)
loDBFordline  = CreateObject("RemoteTable","ORDLINE","ORDLINES","ORDLINE",SET("DATASESSION"),,.T.)

loDBFPoshdr   = CreateObject("RemoteTable","Poshdr","Poshdr","Poshdr",SET("DATASESSION"),,.T.)
loDBFPosln    = CreateObject("RemoteTable","Posln","POSLNS","Posln",SET("DATASESSION"),,.T.)

loDBFInvhdr   = CreateObject("RemoteTable","Invhdr","Invhdr","Invhdr",SET("DATASESSION"),,.T.)
loDBFInvline  = CreateObject("RemoteTable","Invline","INVLINES","Invline",SET("DATASESSION"),,.T.)

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : create temp file
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return    : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCrtTemp
DIMENSION laTempacstru[24,4]

laTempacstru[1,1]= 'Style'
laTempacstru[1,2]= 'C'
laTempacstru[1,3]= 19
laTempacstru[1,4]= 0

laTempacstru[2,1]= 'Color'
laTempacstru[2,2]= 'C'
laTempacstru[2,3]= lnColorLen 
laTempacstru[2,4]= 0

laTempacstru[3,1]= 'Size'
laTempacstru[3,2]= 'C'
laTempacstru[3,3]= 5
laTempacstru[3,4]= 0

laTempacstru[4,1]= 'Warehous'
laTempacstru[4,2]= 'C'
laTempacstru[4,3]= 6
laTempacstru[4,4]= 0

laTempacstru[5,1]= 'desc'
laTempacstru[5,2]= 'C'
laTempacstru[5,3]= 20
laTempacstru[5,4]= 0

laTempacstru[6,1]= 'Per1OnHand'
laTempacstru[6,2]= 'N'
laTempacstru[6,3]= 10
laTempacstru[6,4]= 0

laTempacstru[7,1]= 'Per2OnPo'
laTempacstru[7,2]= 'N'
laTempacstru[7,3]= 10
laTempacstru[7,4]= 0

laTempacstru[8,1]= 'Per3OnPo'
laTempacstru[8,2]= 'N'
laTempacstru[8,3]= 10
laTempacstru[8,4]= 0

laTempacstru[9,1]= 'Per4OnPo'
laTempacstru[9,2]= 'N'
laTempacstru[9,3]= 10
laTempacstru[9,4]= 0

laTempacstru[10,1]= 'Per5OnPo'
laTempacstru[10,2]= 'N'
laTempacstru[10,3]= 10
laTempacstru[10,4]= 0

laTempacstru[11,1]= 'Per6OnPo'
laTempacstru[11,2]= 'N'
laTempacstru[11,3]= 10
laTempacstru[11,4]= 0

laTempacstru[12,1]= 'Per7OnPo'
laTempacstru[12,2]= 'N'
laTempacstru[12,3]= 10
laTempacstru[12,4]= 0

laTempacstru[13,1]= 'Per1OnSo'
laTempacstru[13,2]= 'N'
laTempacstru[13,3]= 10
laTempacstru[13,4]= 0

laTempacstru[14,1]= 'Per2OnSo'
laTempacstru[14,2]= 'N'
laTempacstru[14,3]= 10
laTempacstru[14,4]= 0

laTempacstru[15,1]= 'Per3OnSo'
laTempacstru[15,2]= 'N'
laTempacstru[15,3]= 10
laTempacstru[15,4]= 0

laTempacstru[16,1]= 'Per4OnSo'
laTempacstru[16,2]= 'N'
laTempacstru[16,3]= 10
laTempacstru[16,4]= 0

laTempacstru[17,1]= 'Per5OnSo'
laTempacstru[17,2]= 'N'
laTempacstru[17,3]= 10
laTempacstru[17,4]= 0

laTempacstru[18,1]= 'Per6OnSo'
laTempacstru[18,2]= 'N'
laTempacstru[18,3]= 10
laTempacstru[18,4]= 0

laTempacstru[19,1]= 'Per7OnSo'
laTempacstru[19,2]= 'N'
laTempacstru[19,3]= 10
laTempacstru[19,4]= 0

laTempacstru[20,1]= 'SoldPtd'
laTempacstru[20,2]= 'N'
laTempacstru[20,3]= 10
laTempacstru[20,4]= 0

laTempacstru[21,1]= 'SoldYtd'
laTempacstru[21,2]= 'N'
laTempacstru[21,3]= 10
laTempacstru[21,4]= 0

laTempacstru[22,1]= 'LyrSoldP'
laTempacstru[22,2]= 'N'
laTempacstru[22,3]= 10
laTempacstru[22,4]= 0

laTempacstru[23,1]= 'LyrSoldY'
laTempacstru[23,2]= 'N'
laTempacstru[23,3]= 10
laTempacstru[23,4]= 0

laTempacstru[24,1]= 'Per1OnPo'
laTempacstru[24,2]= 'N'
laTempacstru[24,3]= 10
laTempacstru[24,4]= 0

gfCrtTmp(lcTempfile ,@laTempacstru,"Style+Color+ Size + Warehous",lcTempfile,.T.)
SELECT(lcTempfile )
*!*************************************************************
*! Name      : lfEvalVars
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : Fill Default values used in both OG and Report.
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return    : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfEvalVars
lcStyTitle  = gfItemMask('HI')  && Full Style title.
lnMajSeg    = gfItemMask('SM')  && No. of major segments.

*-- Compute Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
=gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)

  *-- If you Find segment of Color Type
  IF laMajSegs[lnI,1] = 'C'

    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = laMajSegs[lnI,3]
    lcColorTlt = "Only This "+PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3]))
    lnColorLen = LEN(lcNonMajPi)
    EXIT

  ENDIF  && end If you Find segment of Color Type.

ENDFOR    && end Loop Around Non Major elements.
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 01/23/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lnPosition,lcNewFile
lcFieldName = SUBSTR(loOgScroll.laOGFxFlt[lnPosition,1],AT('.',loOgScroll.laOGFxFlt[lnPosition,1])+1)
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1]=lcFieldName 

DO CASE 
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CSTYGROUP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'ROYALTY'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
OTHERWISE &&SUBSTR(STYLE.STYLE,)
  lcFieldName = 'CSTYNONMAJOR'
  laTempacstru[1,1]= lcFieldName 
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= lnNonMajSt - lnColorLen
  laTempacstru[1,4]= 0
ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = loOgScroll.laOGFxFlt[lnPosition,6]
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN (lcFieldName)
*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings
PARAMETERS DPeriod
DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[2,2]
SELECT(lcTempfile)
COPY TO oAriaApplication.WorkDir +  lcTempfile  + ".DBF"
loOgScroll.laCRTables[1]  = oAriaApplication.WorkDir +  lcTempfile  + ".DBF"
loOGScroll.cCROrientation = 'L'
  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'Time-Phased Availability Report                     '

loOgScroll.laCRParams[2,1] = 'Period'
loOgScroll.laCRParams[2,2] = DPeriod

*--End of lfAdjustCRSettings.
*************************************************************
*! Name      : lfGetLastDay
*! Developer : Mariam Mazhar(MMT)
*! Date      : 01/23/2006
*! Purpose   : get the last day of month
*!*************************************************************
FUNCTION lfGetLastDay
PARAMETERS ddate
DO CASE 
  Case INLIST(MONTH(ddate),1, 3, 5, 7, 8, 10, 12)
    lnLastDayNo  = "31"

  Case INLIST(MONTH(ddate),4, 6, 9, 11)
    lnLastDayNo = "30"

  Case MONTH(ddate)= 2 
    If MOD(Year(ddate),4) = 0 
      lnLastDayNo = "29"
    Else
      lnLastDayNo = "28"
    ENDIF 
ENDCASE 
RETURN lnLastDayNo 