*!*************************************************************
*! Name      : SORSORD.prg
*! Developer : Mariam Mazhar
*! Date      : 07/15/2016
*! Purpose   : Custom Import RepSpark orders program for SHA13
*! Entry#    : C201846 (T20160630.0003)
*!*************************************************************
*! Modifications:
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003]
*! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5]
*! B611178,1 MMT 08/10/2016 Issue#6 - Modify custom import RepSpark orders program to use store address[P20160714.0002]
*! B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002]
*! B611186,1 MMT 09/08/2016 Issue#9 - Custom import RepSpark orders does not use default location[P20160714.0002]
*! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002]
*! B611241,1 MMT 01/05/2017 import Orders from repspark does not update lines commission[T20161205.0014]
*!*************************************************************
lcExpr = gfOpGrid('SORSPORDER' , .T.)
************************************************************
*! Name      : lfImportPrc
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : importing function
************************************************************
FUNCTION lfImportPrc
IF (!EMPTY(lcRpHFile) AND !FILE(lcRpHFile)) OR '?' $ lcRpHFile OR EMPTY(lcRpHFile) OR UPPER(JUSTEXT(lcRpHFile)) <> 'CSV'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Header file name')
  RETURN .F.
ENDIF

IF (!EMPTY(lcRpLFile) AND !FILE(lcRpLFile)) OR '?' $ lcRpLFile OR EMPTY(lcRpLFile) OR UPPER(JUSTEXT(lcRpLFile)) <> 'CSV'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Detail file name')
  RETURN .F.
ENDIF

IF (!EMPTY(lcRpHNFile) AND !FILE(lcRpHNFile)) OR '?' $ lcRpHNFile OR (!EMPTY(lcRpHNFile) AND UPPER(JUSTEXT(lcRpHNFile)) <> 'CSV')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Header Notes file name')
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][Start]
  *RETURN .F.
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][End]
ENDIF

IF (!EMPTY(lcRpLNFile) AND !FILE(lcRpLNFile)) OR '?' $ lcRpLNFile  OR (!EMPTY(lcRpLNFile) AND UPPER(JUSTEXT(lcRpLFile)) <> 'CSV')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Detail Notes file name')
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][Start]
  *RETURN .F.
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][End]
ENDIF


IF (!EMPTY(lcRpHDir) AND !DIRE(lcRpHDir)) OR '?' $ lcRpHDir OR EMPTY(lcRpHDir)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid History Directory')
  RETURN .F.
ENDIF
IF lcRpLFile== lcRpHFile
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Header File or detail File')
  RETURN .F.
ENDIF
lcT_BomVar = loogscroll.gfTempName()
lcHeaderTmp = loogscroll.gfTempName()
lcLinesTmp = loogscroll.gfTempName()
lcHeaderNotesTmp = loogscroll.gfTempName()
lcLinesNotesTmp  = loogscroll.gfTempName()
lcLineTmpOrd =  loogscroll.gfTempName()

*! B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][Start]
*!*	CREATE CURSOR (lcHeaderTmp) (Account C(5),Store C(8),RepSpSeq N(6),CustPO C(15),Season C(6),;
*!*	               CDivision C(6),Bulk C(1),ReOrder C(1),SpecInst C(30),SalesRep C(30),Entered Date,;
*!*	               Start Date,Complete Date,Terms C(30),ShipViaC C(30),ShipVia C(6),cTermCode C(6),spcinst C(6))
CREATE CURSOR (lcHeaderTmp) (Account C(5),Store C(8),RepSpSeq N(6),CustPO C(15),Season C(6),;
               CDivision C(6),Bulk C(1),ReOrder C(1),SpecInst C(30),SalesRep C(30),cEntered C(10),;
               cStart C(30),cComplete C(10),Terms C(30),ShipViaC C(30),ShipVia C(6),cTermCode C(6),spcinst C(6),Entered Date,;
               Start Date,Complete Date)
*! B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][End]
SELECT (lcHeaderTmp)
INDEX on Account + Store + STR(RepSpSeq ,6) TAG (lcHeaderTmp)

CREATE CURSOR (lcLinesTmp) (Account C(5),Store C(8),RepSpSeq N(6),LineNO N(6),Style C(19),Size C(5),Qty N(9),Price C(12))
SELECT(lcLinesTmp)
INDEX ON Account + Store + STR(RepSpSeq ,6) +STR(LineNO ,6) TAG(lcLinesTmp)


CREATE CURSOR (lcLineTmpOrd) (Account C(5),Store C(8),RepSpSeq N(6),LineNO N(6),Style C(19),Qty1 N(9),;
               Qty2 N(9),Qty3 N(9),Qty4 N(9),Qty5 N(9),Qty6 N(9),Qty7 N(9),Qty8 N(9),Gros_Price N(12,2),Mnotes M(4),Season C(6),cDivision C(6))
SELECT(lcLineTmpOrd)
INDEX ON Account + Store + STR(RepSpSeq ,6)+ STYLE+STR(Gros_Price,12,2)  TAG(lcLineTmpOrd)

CREATE CURSOR (lcHeaderNotesTmp) (Account C(5),Store C(8),RepSpSeq N(6),NoteLine N(6),Notes C(100))
SELECT  (lcHeaderNotesTmp)
INDEX on Account + Store + STR(RepSpSeq ,6)+STR(NoteLine ,6) TAG (lcHeaderNotesTmp)

CREATE CURSOR (lcLinesNotesTmp) (Account C(5),Store C(8),RepSpSeq N(6),LineNO N(6),NoteLine N(6),Notes C(100))
SELECT (lcLinesNotesTmp)
INDEX on Account + Store + STR(RepSpSeq ,6)+STR(LineNO ,6)+STR(NoteLine ,6) TAG (lcLinesNotesTmp)



*!*	lcStrFile = CHR(13)+CHR(10)+FILETOSTR(lcRpHFile)
*!*	STRTOFILE(lcStrFile,lcRpHFile,0)
SELECT (lcHeaderTmp)
*! B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][Start]
lcOldDateFrm = SET("Date")
SET DATE YMD 
*! B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][End]
Append from (lcRpHFile) TYPE CSV
*! B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][Start]
SELECT(lcHeaderTmp)
REPLACE Entered WITH CTOD(cEntered) ,;
        Complete WITH CTOD(cComplete) ,;
        Start WITH CTOD(cStart) ALL
SET DATE &lcOldDateFrm.
*! B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][End]
*!*	lcStrFile = CHR(13)+CHR(10)+FILETOSTR(lcRpLFile)
*!*	STRTOFILE(lcStrFile,lcRpLFile,0)

SELECT(lcLinesTmp)
Append from (lcRpLFile) TYPE CSV

IF !EMPTY(lcRpHNFile) AND FILE(lcRpHNFile) 
  SELECT(lcHeaderNotesTmp)
  Append from (lcRpHNFile) TYPE CSV
ENDIF

IF !EMPTY(lcRpLNFile) AND FILE(lcRpLNFile)
  SELECT (lcLinesNotesTmp)
  APPEND FROM (lcRpLNFile) TYPE CSV
ENDIF

CREATE CURSOR TMPSTR (mStrRep M(10))

SELECT DISTINCT RepSpSeq,Account FROM (lcHeaderTmp) WHERE !DELETED() INTO CURSOR 'AccTmp'
IF !USED('CUSTOMER')
  =gfOpenTable("Customer","Customer")
ENDIF 
SELECT 'AccTmp'
LOCATE
SCAN
  IF !gfSeek('M'+ AccTmp.Account,'Customer','Customer')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(AccTmp.RepSpSeq))+", Account: "+ AccTmp.Account+" is not found in Customer file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN
SELECT Distinct RepSpSeq,Account,Store FROM (lcHeaderTmp) WHERE !DELETED() AND !EMPTY(ALLTRIM(Store)) INTO CURSOR 'StoreTmp'
SELECT 'StoreTmp'
LOCATE 
SCAN
  IF !gfSeek('S'+ StoreTmp.Account+StoreTmp.Store,'Customer','Customer')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(StoreTmp.RepSpSeq))+", Account: "+ StoreTmp.Account+",Store: "+StoreTmp.Store+" is not found in Customer file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT Distinct RepSpSeq,Account,Store FROM (lcLinesTmp) WHERE !DELETED() AND !EMPTY(ALLTRIM(Store)) INTO CURSOR 'StoreTmp'
SELECT 'StoreTmp'
LOCATE 
SCAN
  IF !gfSeek('S'+ StoreTmp.Account+StoreTmp.Store,'Customer','Customer')
   SELECT TMPSTR 
   LOCATE
   IF EOF()
     APPEND BLANK
   ENDIF
   REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(StoreTmp.RepSpSeq))+", Account: "+ StoreTmp.Account+",Store: "+StoreTmp.Store+" is not found in Customer file"+CHR(13)+CHR(10)
  ENDIF
ENDSCAN

SELECT DISTINCT RepSpSeq,ShipViaC FROM (lcHeaderTmp) WHERE !DELETED()  AND !EMPTY(ShipViaC) INTO CURSOR 'ShipViaTmp'
SELECT ShipViaTmp
LOCATE
SCAN
  lcCodSh = lfGeCodeNO(ShipViaTmp.ShipViaC,'SHIPVIA')  
  IF EMPTY(ALLTRIM(lcCodSh))
    SELECT TMPSTR 
    LOCATE
    IF EOF()
      APPEND BLANK
    ENDIF
    REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(ShipViaTmp.RepSpSeq))+", ShipVia: "+ ALLTRIM(ShipViaTmp.ShipViaC)+" is not found in Codes file"+CHR(13)+CHR(10)
  ELSE
    SELECT (lcHeaderTmp)
    LOCATE
    REPLACE ShipVia WITH lcCodSh FOR ShipViaC = ShipViaTmp.ShipViaC ALL
  ENDIF
ENDSCAN


SELECT DISTINCT RepSpSeq,SpecInst FROM (lcHeaderTmp) WHERE !DELETED() AND !EMPTY(SpecInst) INTO CURSOR 'SpecInstTmp'
SELECT SpecInstTmp
LOCATE
SCAN
  lcCodSh = lfGeCodeNO(SpecInstTmp.SpecInst ,'SPCINST')  
  IF EMPTY(ALLTRIM(lcCodSh))
    SELECT TMPSTR 
    LOCATE
    IF EOF()
      APPEND BLANK
    ENDIF
    REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(SpecInstTmp.RepSpSeq))+", Special Instruction : "+ ALLTRIM(SpecInstTmp.SpecInst)+" is not found in Codes file"+CHR(13)+CHR(10)
  ELSE
    SELECT (lcHeaderTmp)
    LOCATE
    REPLACE spcinst WITH lcCodSh FOR SpecInst = SpecInstTmp.SpecInst ALL
  ENDIF
ENDSCAN


SELECT DISTINCT RepSpSeq,Terms FROM (lcHeaderTmp) WHERE !DELETED() AND !EMPTY(Terms) INTO CURSOR 'TermsTmp'
SELECT TermsTmp
LOCATE
SCAN
  lcCodSh = lfGeCodeNO(TermsTmp.Terms ,'CTERMCODE')  
  IF EMPTY(ALLTRIM(lcCodSh))
    SELECT TMPSTR 
    LOCATE
    IF EOF()
      APPEND BLANK
    ENDIF
    REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(TermsTmp.RepSpSeq))+", Payment Terms : "+ ALLTRIM(TermsTmp.Terms)+" is not found in Codes file"+CHR(13)+CHR(10)
  ELSE
    SELECT (lcHeaderTmp)
    LOCATE
    REPLACE cTermCode WITH lcCodSh FOR Terms = TermsTmp.Terms ALL
  ENDIF
ENDSCAN

IF !USED('SalesRep')
  =gfOpenTable('SalesRep','SalesRep')
ENDIF
SELECT DIStinct RepSpSeq,SalesRep FROM (lcHeaderTmp) WHERE !DELETED() AND !EMPTY(SalesRep) INTO CURSOR 'SalesRepTmp'
SELECT 'SalesRepTmp'
LOCATE
IF !EOF()
  SELECT 'SalesRepTmp'
  SCAN
    IF !gfSeek(ALLTRIM(SalesRepTmp.SalesRep) ,'SalesRep','SalesRep')
      SELECT TMPSTR 
      LOCATE
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(SalesRepTmp.RepSpSeq))+", Sales Rep.: "+ ALLTRIM(SalesRepTmp.SalesRep)+" is not found in Sales rep. file"+CHR(13)+CHR(10)
    ENDIF
  ENDSCAN
ENDIF


IF !USED('Codes')
  =gfOpenTable('Codes','CCODE_NO')
ENDIF
SELECT DIStinct RepSpSeq,CDIVISION FROM (lcHeaderTmp) WHERE !DELETED() AND !EMPTY(CDIVISION) INTO CURSOR 'CDIVSIONTmp'
SELECT 'CDIVSIONTmp'
LOCATE
IF !EOF()
  SELECT 'CDIVSIONTmp'
  SCAN
    IF !gfSeek('N'+padr('CDIVISION',10)+ALLTRIM(CDIVSIONTmp.CDIVISION) ,'Codes','CCODE_NO')
      SELECT TMPSTR 
      LOCATE
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(CDIVSIONTmp.RepSpSeq))+", Division: "+ ALLTRIM(CDIVSIONTmp.CDIVISION)+" is not found in Codes file"+CHR(13)+CHR(10)
    ENDIF
  ENDSCAN
ENDIF

SELECT DIStinct RepSpSeq,Season FROM (lcHeaderTmp) WHERE !DELETED() AND !EMPTY(Season) INTO CURSOR 'SeasonTmp'
SELECT 'SeasonTmp'
LOCATE
IF !EOF()
  SELECT 'SeasonTmp'
  SCAN
    IF !gfSeek('N'+padr('SEASON',10)+ALLTRIM(SeasonTmp.Season) ,'Codes','CCODE_NO')
      SELECT TMPSTR 
      LOCATE
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(SeasonTmp.RepSpSeq))+", Season: "+ ALLTRIM(SeasonTmp.Season)+" is not found in Codes file"+CHR(13)+CHR(10)
    ENDIF
  ENDSCAN
ENDIF

DIMENSION laOrders[1]
laOrders = ''
IF !USED('Scale')
  =gfOpenTable('Scale','Scale')
ENDIF
IF !USED('Style')
  =gfOpenTable('Style','Style')
ENDIF
=gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
=gfOpenTable(oAriaApplication.DataDir+'NOTEPAD',oAriaApplication.DataDir+'NOTEPAD','SH')

llExtendedScale = gfGetMemVar('M_USEEXSSC') 
lnScaleWidth = gfGetMemVar('M_EXTWIDTH')
STORE 0 TO lnClrLen ,lnClrPos ,lnSizeLen ,lnSizePos ,lnMajLen
STORE '' TO lcClrSpr
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
For lnCount = 1 To Alen(laItemSeg,1)
  Do Case
  Case laItemSeg[lnCount,1]='F'
    lnMajLen = Len(laItemSeg[lnCount,3])

  Case laItemSeg[lnCount,1]='C'
    lnClrLen = Len(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = Allt(laItemSeg[lnCount,6])
  Case laItemSeg[lnCount,1]='S'
    lnSizeLen = Len(laItemSeg[lnCount,3])
    lnSizePos = laItemSeg[lnCount,4]
  Endcase
Endfor
SELECT(lcLinesTmp)
LOCATE
SCAN
  STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8
  IF !gfSeek(SUBSTR(&lcLinesTmp..STYLE,1,lnClrPos+lnClrLen-1),'STYLE','STYLE')
    SELECT TMPSTR 
    LOCATE
    IF EOF()
      APPEND BLANK
    ENDIF
    REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(&lcLinesTmp..RepSpSeq))+",Line No."+ALLTRIM(STR(&lcLinesTmp..LineNo))+", Style : "+ ALLTRIM(SUBSTR(&lcLinesTmp..STYLE,1,lnClrPos+lnClrLen-1))+" is not found in Style file"+CHR(13)+CHR(10)
  ELSE
    lcScale = SUBSTR(Style.Scale,1,lnScaleWidth)
    lcStyleScale = ''
    lnScaleSize = 0
    SELECT Scale
    =gfSeek('S'+lcScale,'Scale','Scale')
    SCAN REST WHILE Type+Scale = 'S'+lcScale
      FOR lnC = 1 TO Scale.Cnt
        lcC = STR(lnC,1)
        IF ALLTRIM(Scale.Sz&lcC.) == ALLTRIM(&lcLinesTmp..Size)
          lcStyleScale = Scale.Scale
          lnScaleSize= lnC 
          EXIT
        ENDIF
      ENDFOR
      IF !EMPTY(ALLTRIM(lcStyleScale))
        EXIT
      ENDIF
    ENDSCAN
    IF !EMPTY(lcStyleScale)
      IF llExtendedScale
        *! B611171,1 MMT 07/26/2016 Incorrect Style structure composed in the program [T20160630.0003][Start]
        *lcStyle =  ALLTRIM(SUBSTR(&lcLinesTmp..STYLE,1,lnClrPos+lnClrLen-1))+lcClrSpr+lcStyleScale
        lcStyle =  SUBSTR(&lcLinesTmp..STYLE,1,lnClrPos+lnClrLen-1)+lcClrSpr+lcStyleScale
        *! B611171,1 MMT 07/26/2016 Incorrect Style structure composed in the program [T20160630.0003][End]
      ELSE
        lcStyle =  &lcLinesTmp..STYLE
      ENDIF  
      IF gfSeek(lcStyle ,'Style','Style')
        SELECT (lcLinesTmp)
        SCATTER MEMO MEMVAR
        m.Style = lcStyle 
        m.Season = Style.Season
        m.CDIVISION = STYLE.CDIVISION 
        m.Gros_Price = VAL(m.Price)
        lcSizeNo = STR(lnScaleSize,1)
        m.Qty&lcSizeNo. = &lcLinesTmp..Qty
        IF !SEEK(m.Account + m.Store + STR(m.RepSpSeq ,6)+ m.Style+STR(m.Gros_Price,12,2),lcLineTmpOrd,lcLineTmpOrd)
          SELECT (lcLineTmpOrd)
          APPEND BLANK
          GATHER MEMO MEMVAR
          IF USED(lcLinesNotesTmp) AND RECCOUNT(lcLinesNotesTmp)> 0  AND SEEK(m.Account + m.Store + STR(m.RepSpSeq ,6)+STR(m.LineNO ,6),lcLinesNotesTmp,lcLinesNotesTmp)
            SELECT (lcLinesNotesTmp)
            SCAN REST WHILE Account + Store + STR(RepSpSeq ,6)+STR(LineNO ,6)+STR(NoteLine ,6) = m.Account + m.Store + STR(m.RepSpSeq ,6)+STR(m.LineNO ,6)
              REPLACE Mnotes WITH Mnotes + ALLTRIM(STR(&lcLinesNotesTmp..NoteLine ,6))+" "+&lcLinesNotesTmp..Notes + CHR(13)+CHR(10) IN (lcLineTmpOrd)
            ENDSCAN 
          ENDIF

        ELSE
          REPLACE Qty&lcSizeNo. WITH Qty&lcSizeNo.+&lcLinesTmp..Qty IN (lcLineTmpOrd)
        ENDIF
      ELSE
        SELECT TMPSTR 
        LOCATE
        IF EOF()
          APPEND BLANK
        ENDIF
        REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(&lcLinesTmp..RepSpSeq))+",Line No."+ALLTRIM(STR(&lcLinesTmp..LineNo))+", Style : "+ ALLTRIM(SUBSTR(&lcLinesTmp..STYLE,1,lnClrPos+lnClrLen))+" is not found in Style file"+CHR(13)+CHR(10)
      ENDIF 
    ELSE
      SELECT TMPSTR 
      LOCATE
      IF EOF()
        APPEND BLANK
      ENDIF
      REPLACE mStrRep WITH mStrRep +"RepSpark Sequence No.:"+ALLTRIM(STR(&lcLinesTmp..RepSpSeq))+",Line No."+ALLTRIM(STR(&lcLinesTmp..LineNo))+", Style : "+ ALLTRIM(SUBSTR(&lcLinesTmp..STYLE,1,lnClrPos+lnClrLen))+" Size:"+&lcLinesTmp..Size+" is not found in Scale file"+CHR(13)+CHR(10)
    ENDIF
  ENDIF
ENDSCAN
SELECT TMPSTR 
LOCATE 
IF !EOF()
  DO FORM (oAriaApplication.ClientScreenHome+"SO\soimpre.scx")
  RETURN .F.
ENDIF

IF !USED("ORDLINE")
  =gfOpenTable("ORDLINE","ORDLINE")
ENDIF

IF !USED("ORDHDR")
  =gfOpenTable("ORDHDR","ORDHDR")
ENDIF
IF !useD('BomVar')
  =gfOpenTable(oAriaApplication.DataDir+'BomVar',oAriaApplication.DataDir+'BomVar','SH')
ENDIF
IF !USED('WareHous')
=gfOpenTable(oAriaApplication.DataDir+'WareHous',oAriaApplication.DataDir+'WareHous','SH')
ENDIF
lfCreateOrdTmp()
lfCreateOrders()
IF !EMPTY(laOrders[1])
  IF ALEN(laOrders,1)>1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from order#'+laOrders[1]+' to order#'+laOrders[ALEN(laOrders,1)]+' are created.')
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order#'+laOrders[1]+' is created.')
  ENDIF
  lcRpHDir = ADDBS(lcRpHDir)
  *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][Start]
  *COPY FILE (lcRpHFile) TO (lcRpHDir+JUSTSTEM(lcRpHFile)+'.'+JUSTEXT(lcRpHFile))
  COPY FILE (lcRpHFile) TO (lcRpHDir+JUSTSTEM(lcRpHFile)+"_"+ALLTRIM(DTOS(DATE()))+ALLTRIM(STR(HOUR(CTOT(TIME()))))+ALLTRIM(STR(MINUTE(CTOT(TIME()))))+ALLTRIM(STR(SEC(CTOT(TIME()))))+'.'+JUSTEXT(lcRpHFile))
  *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][End]
  ERASE (lcRpHFile)
  *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][Start]
  *COPY FILE (lcRpLFile) TO (lcRpHDir+JUSTSTEM(lcRpLFile)+'.'+JUSTEXT(lcRpLFile))
  COPY FILE (lcRpLFile) TO (lcRpHDir+JUSTSTEM(lcRpLFile)+"_"+ALLTRIM(DTOS(DATE()))+ALLTRIM(STR(HOUR(CTOT(TIME()))))+ALLTRIM(STR(MINUTE(CTOT(TIME()))))+ALLTRIM(STR(SEC(CTOT(TIME()))))+'.'+JUSTEXT(lcRpLFile))
  *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][End]
  ERASE (lcRpLFile)
  IF !EMPTY(lcRpHNFile) AND FILE(lcRpHNFile)
    *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][Start]
    *COPY FILE (lcRpHNFile) TO (lcRpHDir+JUSTSTEM(lcRpHNFile)+'.'+JUSTEXT(lcRpHNFile))
    COPY FILE (lcRpHNFile) TO (lcRpHDir+JUSTSTEM(lcRpHNFile)+"_"+ALLTRIM(DTOS(DATE()))+ALLTRIM(STR(HOUR(CTOT(TIME()))))+ALLTRIM(STR(MINUTE(CTOT(TIME()))))+ALLTRIM(STR(SEC(CTOT(TIME()))))+'.'+JUSTEXT(lcRpHNFile))
    *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][End]
    ERASE (lcRpHNFile)
  ENDIF
  IF !EMPTY(lcRpLNFile) AND FILE(lcRpLNFile)
    *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][Start]
    *COPY FILE (lcRpLNFile) TO (lcRpHDir+JUSTSTEM(lcRpLNFile)+'.'+JUSTEXT(lcRpLNFile))
    COPY FILE (lcRpLNFile) TO (lcRpHDir+JUSTSTEM(lcRpLNFile)+"_"+ALLTRIM(DTOS(DATE()))+ALLTRIM(STR(HOUR(CTOT(TIME()))))+ALLTRIM(STR(MINUTE(CTOT(TIME()))))+ALLTRIM(STR(SEC(CTOT(TIME()))))+'.'+JUSTEXT(lcRpLNFile))
    *! B611177,1 MMT 08/04/2016 Issue#5-rename the imported repspark files in History folder by date[P20160714.0002 - Issue#5][End]
    ERASE (lcRpLNFile)
  ENDIF
  RETURN .F.
ENDIF

************************************************************
*! Name      : lfCreateOrders
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Create SO function
************************************************************
FUNCTION lfCreateOrders
SELECT WareHous
=gfSeek('')
LOCATE
*! B611186,1 MMT 09/08/2016 Issue#9 - Custom import RepSpark orders does not use default location[P20160714.0002][Start]
LOCATE FOR ldefware 
*! B611186,1 MMT 09/08/2016 Issue#9 - Custom import RepSpark orders does not use default location[P20160714.0002][End]
LOFORMSET =  CREATEOBJECT('Custom')
LOFORMSET.Addproperty('lcdeposittemp','')
LOFORMSET.Addproperty('laEvntTrig[1]','')
LOFORMSET.Addproperty('ActiveMode','A')
LOFORMSET.laEvntTrig[1] =PADR('SOIMPRO',10)
LOFORMSET.lcdeposittemp = gfTempName()
laOrders[1]=''
SELECT(lcHeaderTmp)
LOCATE
SCAN
**************
  STORE '' TO m.cclass,M.Ccurrcode ,m.Cdivision,m.Cordtype,m.ctermcode ,m.CustPO ,;
         m.Order,m.CwareCode,m.GL_Sales,m.LINK_Code, m.Multi,m.Note1,;
         m.priority,m.Season,m.shipvia,m.spcinst ,m.Status ,m.StName ,lcOrder
  store .F. TO m.alt_shpto ,m.lhasnotes,m.MultiPO,m.lfromweb
  STORE '' TO m.Bulk,m.creorder,m.Rep1,m.Rep2,m.Buyer,m.Phone
  STORE 0 TO m.comm1,m.comm2,m.Nexrate,m.Disc,m.Appramt
  STORE 0 TO  m.Book,m.BookAmt,lnLastLNo,m.lastline,m.nExRate,m.NcurrUnit,m.OPenAmt,m.OPen, m.TotAmnt 
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][Start]
  STORE '' TO m.cfaccode
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][End]
  SELECT (lcHeaderTmp)
  SCATTER MEMO MEMVAR 
  =gfSeek('M'+m.Account,'Customer','Customer')
  *B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][Start]
  *m.Entered = oAriaApplication.SystemDate
  *B611180,1 MMT 08/11/2016 Issue#7 - Custom Import RepSpark Orders program does not import dates(Start,Complete,Entered)[P20160714.0002][End]
  M.Ccurrcode = Customer.ccurrcode 
  IF M.Ccurrcode <> oAriaApplication.BaseCurrency
    lnUnit = 0
    m.Nexrate  = gfChkRate('lnUnit' , M.Ccurrcode , m.entered  , .F.) 
    m.NcurrUnit = lnUnit
  ELSE
    m.NcurrUnit = 1
    m.Nexrate  = 1
  ENDIF 
  
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][Start]
  m.cfaccode = Customer.cfaccode 
  *! B611200,1 MMT 10/11/2016 Issues#8 and 10 on Project [P20160714.0002][End]
    
   m.lfromweb = .F.
   m.Rep1 = Customer.SalesRep
   m.comm1 = Customer.Comm
   m.comm2 = Customer.Comm2
   m.Rep2 = Customer.Rep2
   IF !EMPTY(m.SalesRep)
     m.Rep1 = m.SalesRep
   ENDIF
   m.Disc = 0.00
   m.Buyer = customer.buyer
   m.Phone = Customer.Phone1
   m.Appramt = 0
   
   SELECT DISTInCT SEASON FROM (lcLineTmpOrd) WHERE RepSpSeq = m.RepSpSeq  AND Account = m.Account INTO CURSOR 'TmpSeason'
   IF RECCOUNT('TmpSeason') > 1
     m.Season = '*'
   ELSE
     SELECT TmpSeason
     LOCATE
     m.Season = TmpSeason.Season
   ENDIF
   
   SELECT DISTInCT cDivision FROM (lcLineTmpOrd) WHERE RepSpSeq = m.RepSpSeq  AND Account = m.Account INTO CURSOR 'TmpcDivision'
   IF RECCOUNT('TmpcDivision') > 0
     SELECT TmpcDivision
     LOCATE
     m.cDivision = TmpcDivision.cDivision
   ENDIF
   
   SELECT(lcLineTmpOrd)
   COUNT FOR RepSpSeq = m.RepSpSeq  AND Account = m.Account  TO lnLastLNo 
      
   SELECT(lcLineTmpOrd)
   SUM Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,;
      (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8)* Gros_Price ;
       FOR RepSpSeq = m.RepSpSeq  AND Account = m.Account TO M.qty1,M.qty2,;
           m.Qty3,m.Qty4 ,m.Qty5 ,m.Qty6 ,m.Qty7 ,m.Qty8 ,m.TotAmnt 
      
   SELECT (lcHeaderTmp)      
   m.MultiPO  = .F.
   m.Cordtype = 'O'
   m.Order = '' &&gfSequence('ORDER','','',m.cDivision)
   m.CwareCode = WAREHOUS.CwareCode
   = gfSeek('M'+m.Account,'CUSTOMER','CUSTOMER')
   IF EMPTY(ALLTRIM(m.ctermcode))
     m.ctermcode = Customer.ctermcode 
   ENDIF  
   m.GL_Sales = Customer.cslsgllink
   m.LINK_Code = Customer.LINK_Code
   m.Multi = 'N'
   SELECT DISTInCT STORE FROM (lcLineTmpOrd) WHERE RepSpSeq = m.RepSpSeq  AND Account = m.Account AND !EMPTY(Store) INTO CURSOR 'TmpcStore'
   IF RECCOUNT('TmpcStore') > 1
     m.Multi = 'Y'
   ENDIF
   m.Note1 = 'RepSpark seq.:'+ALLTRIM(STR(m.RepSpSeq  ,6))
   m.Priority = Customer.priority 
   IF EMPTY(ALLTRIM(m.shipvia))
     m.shipvia = Customer.shipvia 
   ENDif  
   IF EMPTY(ALLTRIM(m.spcinst))
     m.spcinst = Customer.spcinst 
   ENDIF   
   m.Status = 'O'
   =gfSeek(IIF(!EMPTY(m.Store),"S","M")+m.Account+IIF(!EMPTY(m.Store),m.Store,""),'CUSTOMER','CUSTOMER')
   m.StName = IIF(!EMPTY(m.Store),Customer.StName,"")
   *! B611178,1 MMT 08/10/2016 Issue#6 - Modify custom import RepSpark orders program to use store address[P20160714.0002][Start]
   *m.alt_shpto = .T.
   *! B611178,1 MMT 08/10/2016 Issue#6 - Modify custom import RepSpark orders program to use store address[P20160714.0002][End]
   m.MnotesLines =''
   
   IF SEEK(m.Account + m.Store + STR(m.RepSpSeq ,6) ,lcHeaderNotesTmp,lcHeaderNotesTmp)&&HNO
     SELECT (lcHeaderNotesTmp)
     SCAN REST WHILE  Account + Store + STR(RepSpSeq ,6)+STR(NoteLine ,6)=m.Account + m.Store + STR(m.RepSpSeq ,6) 
       m.MnotesLines = m.MnotesLines +ALLTRIM(STR(&lcHeaderNotesTmp..NoteLine ,6))+" "+&lcHeaderNotesTmp..Notes+CHR(13)+CHR(10)
     ENDSCAN
     m.lhasnotes = .T.
   Else
     m.lhasnotes = .F. 
   ENDIF
   
   m.Book = m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
   m.BookAmt = m.TotAmnt 
   m.lastline  = lnLastLNo 
   m.Flag = 'N'
   m.OPen = m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
   m.OPenAmt =  m.TotAmnt 
   INSERT INTO (lcOrdHdrTmp) FROM MEMVAR
   lcOrder = m.Order
      *Inserting Lines in Ordline File
   SELECT(lcLineTmpOrd)
   lnLine = 1
   SCAN FOR RepSpSeq = m.RepSpSeq  AND Account = m.Account 
     STORE 0 TO m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8
     STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8
     *! B611241,1 MMT 01/05/2017 import Orders from repspark does not update lines commission[T20161205.0014][Start]
     *STORE 0 TO m.comm1,m.comm2
     *! B611241,1 MMT 01/05/2017 import Orders from repspark does not update lines commission[T20161205.0014][End]
     SCATTER MEMO MEMVAR 
     m.note_mem = m.Mnotes
     m.price = m.Gros_price
     m.Account = &lcLineTmpOrd..Account
     m.Order   = lcOrder         
     m.CwareCode = WAREHOUS.CwareCode
     m.store  = &lcLineTmpOrd..store
     m.Cordtype = 'O'
     *m.CustPo = m.Worder
     =gfSeek(&lcLineTmpOrd..Style,'Style','Style')
     *m.Gros_price = M.Price
     m.Cost = Style.TOTCOST
     m.Desc1 = Style.Desc1
     m.Flag = 'N'
     m.Gl_Cost = Style.Link_Code
     m.GL_Sales = Customer.cslsgllink + Style.CSLSGLLINK
     m.Scale = Style.Scale
     m.Season = Style.Season
     m.Book1 = m.Qty1 
     m.Book2 = m.Qty2
     m.Book3 = m.Qty3
     m.Book4 = m.Qty4
     m.Book5 = m.Qty5
     m.Book6 = m.Qty6 
     m.Book7 = m.Qty7
     m.Book8 = m.Qty8
     m.totbook =  m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
     m.totQty =  m.Qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
     m.lineno = lnLine 
     lnLine = lnLine + 1
     INSERT INTO (lcOrdLnTmp) FROM MEMVAR
   ENDSCAN
   llOrdSaved = .F.
   SET ORDER TO oRDLINE IN (lcOrdLnTmp)
   DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
                  WITH .F., 'A', lcOrdHdrTmp,lcOrdLnTmp,.F.,.f.,lcT_BomVar,loFormSet

   IF llOrdSaved
     IF !EMPTY(laOrders[1])
       DIMENSION laOrders[ALEN(laOrders,1)+1]
       laOrders[ALEN(laOrders,1)] = ORDHDR.ORDER
     ELSE
       laOrders[1] = ORDHDR.ORDER
     ENDIF
     *
     IF !EMPTY(m.MnotesLines)
       m.Type = 'B'
       m.Key = ORDHDR.ORDER 
       m.Cdesc = "Notes For Order Number : "+ ORDHDR.ORDER
       mnotes = m.MnotesLines
       SELECT NOTEPAD
       gfAppend('NOTEPAD',.T.)  
       =gfAdd_Info('NOTEPAD')               
     ENDIF 

   ENDIF               
   
   SELECT (lcOrdLnTmp)
   DELETE ALL
   PACK

	*mmt
   SELECT (lcOrdHdrTmp)
   DELETE ALL
   PACK

 ENDSCAN  
 lfSavefiles()
************************************************************
*! Name      : lfvPathDir
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Validate History Directory
************************************************************
FUNCTION lfvPathDir
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][Start]
*IF EMPTY(ALLTRIM(lcRpHDir)) OR !DIRECTORY(ALLTRIM(lcRpHDir))
IF !EMPTY(ALLTRIM(lcRpHDir)) AND !DIRECTORY(ALLTRIM(lcRpHDir))
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][End]
  lcRpHDir =GETDIR('','Select History Directory')
ENDIF
************************************************************
*! Name      : lfvHPath
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Validate Header file
************************************************************
FUNCTION lfvHPath
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][Start]
*IF EMPTY(ALLTRIM(lcRpHFile)) OR !FILE(ALLTRIM(lcRpHFile))
IF !EMPTY(ALLTRIM(lcRpHFile)) AND !FILE(ALLTRIM(lcRpHFile))
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][End]
  lcRpHFile = GETFILE("CSV",'','',1,"Select Header File")
ENDIF
************************************************************
*! Name      : lfvDPath
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Validate lines file
************************************************************
FUNCTION lfvDPath
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][Start]
*IF EMPTY(ALLTRIM(lcRpLFile)) OR !FILE(ALLTRIM(lcRpLFile))
IF !EMPTY(ALLTRIM(lcRpLFile)) AND !FILE(ALLTRIM(lcRpLFile))
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][End]
  lcRpLFile = GETFILE("CSV",'','',1,"Select Detail File")
ENDIF
************************************************************
*! Name      : lfvHNPath
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Validate Header notes file
************************************************************
FUNCTION lfvHNPath
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][Start]
*IF EMPTY(ALLTRIM(lcRpHNFile)) OR !FILE(ALLTRIM(lcRpHNFile))
IF !EMPTY(ALLTRIM(lcRpHNFile)) AND !FILE(ALLTRIM(lcRpHNFile))
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][End]
  lcRpHNFile= GETFILE("CSV",'','',1,"Select Header Notes File")
ENDIF

************************************************************
*! Name      : lfvDNPath
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Validate Detail notes file
************************************************************
FUNCTION lfvDNPath
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][Start]
*IF EMPTY(ALLTRIM(lcRpLNFile)) OR !FILE(ALLTRIM(lcRpLNFile))
IF !EMPTY(ALLTRIM(lcRpLNFile)) AND !FILE(ALLTRIM(lcRpLNFile))
*! B611171,1 MMT 07/26/2016 Stop validating empty path in custom RepSpark SO import prg.[T20160630.0003][End]
  lcRpLNFile = GETFILE("CSV",'','',1,"Select Detail Notes File")
ENDIF


************************************************************
*! Name      : lfwrepwhen
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : OG When function
************************************************************
FUNCTION lfwrepwhen
************************************************************
*! Name      : lfGeCodeNO
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Get Code No. from code description
************************************************************
FUNCTION lfGeCodeNO
LPARAMETERS lcShipViaDesc,lcCodeName
IF !USED('Codes')
  =gfOpenTable("Codes",'cCode_NO')
ENDIF
lcCodeNo = ''
SELECT Codes
=gfSetOrder('cCode_NO')
**! B611171,1 MMT 07/26/2016 Incorrect Style structure composed in the program [T20160630.0003][Start]
*=gfSeek("N"+PADR(lcCodeName,10))
*!*	LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = "N"+PADR(lcCodeName,10) FOR ALLTRIM(UPPER(CDISCREP))=ALLTRIM(UPPER(lcShipViaDesc))
*!*	IF FOUND()
if gfSeek("N"+PADR(lcCodeName,10)+padl(allt(lcShipViaDesc),6,'0'))
*! B611171,1 MMT 07/26/2016 Incorrect Style structure composed in the program [T20160630.0003][End]
  lcCodeNo = Codes.CCODE_NO
ENDIF
RETURN (lcCodeNo)


************************************************************
*! Name      : lfCreateOrdTmp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Create Temp. Order cursors
************************************************************
FUNCTION lfCreateOrdTmp
SELECT OrdLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
llTrade =  gfGetMemVar('M_TRDDISCL')
IF llTrade 
  DIMENSION laFileStru[lnFileStru+5,18]
ELSE
  DIMENSION laFileStru[lnFileStru+4,18]
ENDIF

    laFileStru[lnFileStru+1,1] = 'lContract'
    laFileStru[lnFileStru+1,2] = 'L'
    laFileStru[lnFileStru+1,3] = 1
    laFileStru[lnFileStru+1,4] = 0
    laFileStru[lnFileStru+2,1] = 'cMajor'
    laFileStru[lnFileStru+2,2] = 'C'
    laFileStru[lnFileStru+2,3] = lnMajLen
    laFileStru[lnFileStru+2,4] = 0
    laFileStru[lnFileStru+3,1] = 'cNonMajor'
    laFileStru[lnFileStru+3,2] = 'C'
    laFileStru[lnFileStru+3,3] = lnClrLen
    laFileStru[lnFileStru+3,4] = 0
    laFileStru[lnFileStru+4,1] = 'cMjrScale'
    laFileStru[lnFileStru+4,2] = 'C'
    laFileStru[lnFileStru+4,3] =  lnSizeLen 
    laFileStru[lnFileStru+4,4] = 0
IF llTrade 
  laFileStru[lnFileStru+5,1] = 'TRD_price'
  laFileStru[lnFileStru+5,2] = 'N'
  laFileStru[lnFileStru+5,3] =  12
  laFileStru[lnFileStru+5,4] = 2
ENDIF
FOR lnCount = 1 TO IIF(llTrade ,5,4)
  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
              laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
              laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
              laFileStru[lnFileStru+lnCount,16]
  STORE 0 TO  laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
ENDFOR
DECLARE laIndex[4,2]
laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+DYELOT+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
laIndex[4,1] = 'Order+Store+STYLE+Dyelot+STR(LineNo,6)'
laIndex[4,2] = 'CONFIGLIN'

=gfCrtTmp(lcOrdLnTmp,@laFileStru,@laIndex)
SET ORDER TO TAG 'ORDLINE' IN (lcOrdLnTmp)

SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru,18]
DECLARE laIndex[1,2]
laIndex[1,1] = 'cordtype+order'
laIndex[1,2] = lcOrdHdrTmp
=gfCrtTmp(lcOrdHdrTmp,@laFileStru,@laIndex)


SELECT BomVar
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]

    
laFileStru[lnFileStru+1,1] = 'nRecno'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cStatus'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
FOR lnLoop = 1 to  2
   STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
            laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
            laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
            laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
            laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR
IF USED(lcT_BomVar)
  ZAP IN (lcT_BomVar)
ELSE
 =gfCrtTmp(lcT_BomVar,@laFileStru,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
ENDIF

*!*************************************************************
*! Name      : lfSavefiles
*: Developer : Mariam Mazhar- [MMT]
*: Date      : 07/15/2016
*! Purpose   : Function to save 
*!*************************************************************
FUNCTION lfSavefiles
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == SET("Datasession" )
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  MESSAGEBOX('Saving Process is Rolled Back')
  ThisFormSet.Undo()
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
************************************************************
*! Name      : lfcustmsg
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2016
*! Purpose   : Custom function to check if order is created or not
************************************************************
FUNCTION lfcustmsg
llOrdSaved = .T.