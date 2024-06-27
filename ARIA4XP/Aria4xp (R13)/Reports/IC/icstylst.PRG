*!***********************************************************************
*: Program file       : ICSTYLST
*: Program description: Style master list
*: Module             : Inventory Control (IC)
*: Developer          : Tarek Noaman (TNA)
*: Tracking Job Number: N000529
*: Date               : 3/27/2006
*:**********************************************************************
*: Calls:
*:         Programs   :
*:         Screens    :
*: Global Function    :gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe,
*:                     gfRltFld,gfCodDes,gfOptMsg
*:**********************************************************************
*: Called From:
*:**********************************************************************
*: Passed Parameters  :
*:***************************************************************************
*: Example : DO ICSTYLST
*:***************************************************************************
*:                   Option grid Filter contains
*:1-Title                          is                          lcRPTitle
*:2-Domestic/Imported              Domestic/Imported/Both      lcRPDomImp
*:3-Print Style Picture            YES/NO                      llRpPrnPic
*:4-Style                          In List                     STYLE.CSTYMAJOR
*:5-Primary Fabric                 In List                     STYLE.FABRIC
*:6-Season                         In List                     STYLE.SEASON
*:7-Division                       In List                     STYLE.CDIVISION
*:8-Style Group                    In List                     STYLE.CSTYGROUP
*:9-Status                         In List                     STYLE.STATUS
*:10-Only This Color               In List                     SUBSTR(STYLE.Style,lnClrPo,lnColorLen)
*:***************************************************************************
*:                         Tables Used
*:                        _______________
*:01- Style
*:02- Scale
*:03- Objlink
*:04- Objects
*:***************************************************************************
*Modifications:
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[T20060908.0003]
*B610134,1 MMT 10/31/2012 Report prints repeated lines if print picture is set to Yes[T20121023.0008]
*B611446,1 MHM 11/14/2017 Master style report cannot handle extended size scale case [T20171009.0018]
*B611482,1 AHH 23/11/2017 Style Master List report is repeating colors in case of extended size scale companies [T20171009.0018]
*B611505,1 AHH 09/01/2018 the style master list report prints only the first scale sizes only [T20171009.0018]
*B612627,1 MMT 09/26/2022 Style Master List report gives error if user selected more than 25 item in In List[T20220922.0001]
*E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003]
*:***************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include r:\aria4xp\reports\ic\icstylst.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
lcStTime   = TIME()
llDontPrnt = .F.
lcTime     = gfGetTime()
lnMajLen   = LEN(SUBSTR(lcMajPic,4))
llDummy    = (loOgScroll.llOGFltCh or (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL"))  AND lfCollData()
*B610134,1 MMT 10/31/2012 Report prints repeated lines if print picture is set to Yes[Start]
*!*	IF llRpPrnPic = 'Y' AND UPPER(ALLTRIM(lcRepMode)) <> "TEXT"
*!*	  SELECT FStyle
*!*	  SET RELATION TO
*!*	  SET RELATION TO 'S'+SUBSTR(Style,1,lnMajLen) INTO OBJLINK ADDITIVE
*!*	  SET SKIP TO OBJLINK
*!*	  SELECT OBJLINK
*!*	  SET RELATION TO
*!*	  SET RELATION TO cobject_id INTO OBJECTS  ADDITIVE
*!*	ENDIF
*B610134,1 MMT 10/31/2012 Report prints repeated lines if print picture is set to Yes[End]
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT FStyle
GO TOP
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Record(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTED,oAriaApplication.GetHeaderText("LANG_SELECTED",AHEADERFILE)) +' ' + ALLTRIM(STR(RECCOUNT())) +;
  ' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECIN,oAriaApplication.GetHeaderText("LANG_RECIN",AHEADERFILE))+' ' + ALLTRIM(STR(lnInterval,6,2)) + ' '+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SECO,oAriaApplication.GetHeaderText("LANG_SECO",AHEADERFILE)) NOWAIT
*N000682,1 MMT 02/11/2013 Globalization changes[End]
loOgScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcRepNam')

*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Noaman	(TNA)
*! Date      : 3/27/2006
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData

SET STEP ON 
  *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][Start]
  * CREATE CURSOR FStyle(STYLE C(19),cstymajor C(19),DESC C(20),STATUS C(1),season C(6),cdivision C(6),cstygroup C(6),PATTERN C(10),fabric C(7),cColor M,START D,;
    soldout D,location C(8),nstyweight N(5,2),qty_ctn N(3),pricea N(12,2),priceb N(12,2),pricec N(12,2),commission L,SCALE C(3),prepak C(1))
  IF !USED('STYLEUPC')
    = gfOpenTable('STYLEUPC','STYLEUPC') && STYLE+SIZE
  ENDIF
  
  IF !USED('SCALE')
    = gfOpenTable('SCALE','SCALE') 
  ENDIF
  IF !USED('NOTEPAD')
    = gfOpenTable('NOTEPAD','NOTEPAD') 
  ENDIF
  IF !USED('PROFVALU')
    = gfOpenTable('PROFVALU','PROFILE') 
  ENDIF
  SELECT STYLE
  lnFlds = AFIELDS(laStyFld)
  
  lcNonNeededFlds = "ord1     ,ord2     ,ord3     ,ord4     ,ord5     ,ord6     ,ord7     ,ord8     ,totord   ,"+;
  "wip1     ,wip2     ,wip3     ,wip4     ,wip5     ,wip6     ,wip7     ,wip8     ,totwip   ,"+;
  "stk1     ,stk2     ,stk3     ,stk4     ,stk5     ,stk6     ,stk7     ,stk8     ,totstk   ,"+;
  "alo1     ,alo2     ,alo3     ,alo4     ,alo5     ,alo6     ,alo7     ,alo8     ,totalo   ,"+;
  "shp1     ,shp2     ,shp3     ,shp4     ,shp5     ,shp6     ,shp7     ,shp8     ,totshp   ,"+;
  "ret1     ,ret2     ,ret3     ,ret4     ,ret5     ,ret6     ,ret7     ,ret8     ,totret   ,"+;
  "ra1      ,ra2      ,ra3      ,ra4      ,ra5      ,ra6      ,ra7      ,ra8      ,totra    ,"+;
  "intrans1 ,intrans2 ,intrans3 ,intrans4 ,intrans5 ,intrans6 ,intrans7 ,intrans8 ,totintrn ,"+;
  "nwo1     ,nwo2     ,nwo3     ,nwo4     ,nwo5     ,nwo6     ,nwo7     ,nwo8     ,ntotwo, NSTKVAL"
  lcNonNeededFlds = lcNonNeededFlds + ", cAdd_User, dAdd_Date, cAdd_Time, lLok_stat, cLok_User, dLok_Date, cLok_Time, cEdit_User, dEdit_Date, cEdit_Time,cedt_ver,cadd_ver"
  DIMENSION laDelFlds[1]
  =gfSubStr(lcNonNeededFlds ,@laDelFlds,",")
  IF Type('laDelFlds[1]') <>'U'
    FOR lnA=1 TO ALEN(laDelFlds,1)
      lnP =ASCAN(laStyFld,ALLTRIM(UPPER(laDelFlds[lnA])),1)
      IF lnP>0
        lnPos = ASUBSCRIPT(laStyFld,lnP,1)
        ADEL(laStyFld,lnPos )
      ENDIF  
    ENDFOR
  ENDIF
  DIMENSION lastyfld[alen(lastyfld,1)-alen(ladelFlds,1),18]
  lnFlds = ALEN(laStyFld,1)
  DIMENSION laStyFld[lnFlds +4,18]
  laStyFld[lnFlds +1,1] = 'cColor'
  laStyFld[lnFlds +1,2] = 'M'
  laStyFld[lnFlds +1,3] = 4
  laStyFld[lnFlds +1,4] = 0
  laStyFld[lnFlds +2,1] = 'mnotes'
  laStyFld[lnFlds +2,2] = 'M'
  laStyFld[lnFlds +2,3] = 4
  laStyFld[lnFlds +2,4] = 0
  laStyFld[lnFlds +3,1] = 'CUPC'
  laStyFld[lnFlds +3,2] = 'M'
  laStyFld[lnFlds +3,3] = 4
  laStyFld[lnFlds +3,4] = 0
  laStyFld[lnFlds +4,1] = 'CPRO_VALUE'
  laStyFld[lnFlds +4,2] = 'M'
  laStyFld[lnFlds +4,3] = 4
  laStyFld[lnFlds +4,4] = 0
  
  
  FOR lnCount = 1 TO 4
    STORE '' TO laStyFld[lnFlds +lnCount,7],laStyFld[lnFlds +lnCount,8],laStyFld[lnFlds +lnCount,9],;
              laStyFld[lnFlds +lnCount,10],laStyFld[lnFlds +lnCount,11],laStyFld[lnFlds +lnCount,12],;
              laStyFld[lnFlds +lnCount,13],laStyFld[lnFlds +lnCount,14],laStyFld[lnFlds +lnCount,15],;
              laStyFld[lnFlds +lnCount,16]
    STORE 0 TO  laStyFld[lnFlds +lnCount,17],laStyFld[lnFlds +lnCount,18]
  ENDFOR

  CREATE CURSOR FStyle FROM ARRAY laStyFld
  *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][End]

*B612627,1 MMT 09/26/2022 Style Master List report gives error if user selected more than 25 item in In List[T20220922.0001][Start]
*!*    IF lcRpDomImp <>  'B'
*!*      lcRpExp = lcRpExp + 'AND' + IIF(lcRpDomImp = 'I' , ' !Make' , ' Make' )
*!*    ENDIF
*!*    lcStyMaj  = SPACE(0)
*!*    SELECT STYLE
*!*    SET ORDER TO STYLE
*!*    SCAN FOR EVALUATE(lcRpExp)
*!*      IF (lcStyMaj # STYLE.cstymajor)
*!*        m.cColor = SPACE(0)
*!*      ENDIF
*!*      *B611482,1 AHH 23/11/2017 Style Master List report is repeating colors in case of extended size scale companies [T20171009.0018] [Start]
*!*      *!*    m.cColor = m.cColor + Substr(Style.Style,lnClrPo,lnColorLen) + " "
*!*      IF !(SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen)  $ m.cColor )
*!*        m.cColor = m.cColor + SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen) + " "
*!*      ENDIF
*!*      *B611482,1 AHH 23/11/2017 Style Master List report is repeating colors in case of extended size scale companies [T20171009.0018] [End]
*!*      IF (lcStyMaj # STYLE.cstymajor)
*!*        SCATTER MEMVAR MEMO
*!*        INSERT INTO FStyle FROM MEMVAR
*!*      ELSE
*!*        lcAlias = SELECT(0)
*!*        SELECT FStyle
*!*        REPLACE cColor WITH m.cColor
*!*        SELECT (lcAlias)
*!*      ENDIF
*!*      lcStyMaj = STYLE.cstymajor
*!*    ENDSCAN
  
  
  
  *"STYLE.CSTYMAJOR "
  lcCursorStyle =''
  llSelectStyle = .F.
  lnPosStyle = ASCAN(loOGScroll.laOGvrFlt,"STYLE.CSTYMAJOR")
  IF lnPosStyle > 0
    lnPosStyle = ASUBSCRIPT(loOGScroll.laOGvrFlt,lnPosStyle,1)
    lcCursorStyle= loOGScroll.laOGvrFlt[lnPosStyle,6]
    IF !EMPTY(lcCursorStyle)
      SELECT(lcCursorStyle)
      LOCATE
      IF !EOF()
        llSelectStyle = .T.
      ENDIF
    ENDIF
  ENDIF
  *"STYLE.FABRIC    "
  lcCursorFabric = ''
  llSelectFabric = .F.
  lnPosFaric = ASCAN(loOGScroll.laOGvrFlt,"STYLE.FABRIC")
  IF lnPosFaric > 0
    lnPosFaric = ASUBSCRIPT(loOGScroll.laOGvrFlt,lnPosFaric ,1)
    lcCursorFabric = loOGScroll.laOGvrFlt[lnPosFaric ,6]
    IF !EMPTY(lcCursorFabric)
      SELECT(lcCursorFabric)
      LOCATE
      IF !EOF()
        llSelectFabric = .T.
      ENDIF
    ENDIF
  ENDIF
  *"STYLE.CDIVISION "
  lcDivFile = ''
  lcDivs = ''
  llUseDiv = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGvrFlt,ASCAN(LOOGSCROLL.laOGvrFlt,'STYLE.CDIVISION'),1)
  IF lnPosition > 0
    lcDivs = LOOGSCROLL.laOGvrFlt[lnPosition,6]
    lcDivFile = loogscroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.) AND lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
  ENDIF


  *"STYLE.SEASON    "
  lcSeasonFile = ''
  lcSeasons  = ''
  llUseSeasons = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGvrFlt,ASCAN(LOOGSCROLL.laOGvrFlt,'STYLE.SEASON'),1)
  IF lnPosition > 0
    lcSeasons   = LOOGSCROLL.laOGvrFlt[lnPosition,6]
    lcSeasonFile = loogscroll.gfTempName()
    llUseSeasons = IIF(LEN(lcSeasons  )>0,.T.,.F.) AND lfConvertToCursor(lcSeasons   ,'SEASON',lcSeasonFile )
  ENDIF
  
  *"STYLE.CSTYGROUP "
  lcStyGrpFile = ''
  lcStyGrp  = ''
  llUseStyGrps = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGvrFlt,ASCAN(LOOGSCROLL.laOGvrFlt,'STYLE.CSTYGROUP'),1)
  IF lnPosition > 0
    lcStyGrp  = LOOGSCROLL.laOGvrFlt[lnPosition,6]
    lcStyGrpFile = loogscroll.gfTempName()
    llUseStyGrps= IIF(LEN(lcStyGrp )>0,.T.,.F.) AND lfConvertToCursor(lcStyGrp  ,'CSTYGROUP',lcStyGrpFile)
  ENDIF
  
  *"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)"
  lcColorFile = ''
  lcColors= ''
  llUseColors = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGvrFlt,ASCAN(LOOGSCROLL.laOGvrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
  IF lnPosition > 0
    lcColors= LOOGSCROLL.laOGvrFlt[lnPosition,6]
    lcColorFile= loogscroll.gfTempName()
    llUseColors = IIF(LEN(lcColors)>0,.T.,.F.) AND lfConvertToCursor(lcColors,'COLOR',lcColorFile)
  ENDIF
  
  *"STYLE.STATUS    "
  lcStatusVar= ''
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGvrFlt,ASCAN(LOOGSCROLL.laOGvrFlt,'STYLE.STATUS'),1)
  IF lnPosition > 0
    lcStatusVar = LOOGSCROLL.laOGvrFlt[lnPosition,6]
  ENDIF
  lcStyMaj  = SPACE(0)
  SELECT STYLE
  SET ORDER TO STYLE
  IF llSelectStyle 
    SELECT(lcCursorStyle)
    SCAN
      SELECT STYLE
      IF SEEK(SUBSTR(&lcCursorStyle..KeyExp,1,lnMajLen))
        SCAN REST WHILE Style = SUBSTR(&lcCursorStyle..KeyExp,1,lnMajLen) FOR IIF(lcRpDomImp <>  'B',IIF(lcRpDomImp = 'I' , !Make , Make),.T.) AND ;
          IIF(!EMPTY(lcStatusVar), Style.Status $ lcStatusVar,.T.) AND IIF(llUseColors ,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcColorFile),.T.) AND ;
          IIF(llUseStyGrps,SEEK(STYLE.CSTYGROUP,lcStyGrpFile),.T.) AND IIF(llUseSeasons,SEEK(STYLE.SEASON,lcSeasonFile),.T.) AND ;
          IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND IIF(llSelectFabric ,SEEK(STYLE.FABRIC,lcCursorFabric),.T.)
          IF (lcStyMaj # STYLE.cstymajor)
            m.cColor = SPACE(0)
          ENDIF
          IF !(SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen)  $ m.cColor )
            m.cColor = m.cColor + SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen) + " "
          ENDIF
          *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][Start]
          m.CUPC  =''
          IF gfSEEK(Style.Style,'STYLEUPC')
            =gfSeek('S'+Style.Scale,'SCALE')
            
            FOR ln=1 TO Scale.cnt
               lcN = STR(ln,1)
               IF gfSEEK(Style.Style+lcN ,'STYLEUPC') 
                 m.CUPC  = m.CUPC + IIF(!EMPTY(m.CUPC),",","")+Scale.Sz&lcN.+" : "+ALLTRIM(StyleUPC.cupcnum1)+ALLTRIM(StyleUPC.cupcnum2)+ALLTRIM(StyleUPC.cupcnum3)
               ENDIF
            ENDFOR 
          ENDIF
          m.mnotes = ''
          IF gfSeek('F'+SUBSTR(Style.Style,1,lnMajLen),'NOTEPAD')
            m.mnotes = NOTEPAD.mnotes
          ENDIF
          m.CPRO_VALUE  =""
          IF gfSEEK('ST'+Style.Style,'profvalu')
            SELECT profvalu
            SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'ST'+Style.Style
              m.CPRO_VALUE  = m.CPRO_VALUE  + IIF(!EMPTY(m.CPRO_VALUE),", ","")+ ALLTRIM( GFCODDES(profvalu.CPRO_CODE,'CPRO_CODE '))+" : "+ ALLTRIM(profvalu.cpro_value)
            ENDSCAN
          ENDIF
          *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][End]
          
          IF (lcStyMaj # STYLE.cstymajor)
            SELECT STYLE
  	        SCATTER MEMVAR MEMO
  	        INSERT INTO FStyle FROM MEMVAR
            IF (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL")
              REPLACE cColor WITH SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen) IN FStyle 
            ENDIF
  	      ELSE
          
  	        lcAlias = SELECT(0)
  	        SELECT FStyle
            IF !(oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL")
  	          REPLACE cColor WITH m.cColor
            ENDIF
            *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][Start]
            IF (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL")
              nRec = RECNO()
              SELECT STYLE
              SCATTER MEMVAR MEMO
              INSERT INTO FStyle FROM MEMVAR
              REPLACE cColor WITH SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen) IN FStyle 
              IF BETWEEN(nRec ,1,RECCOUNT('FStyle'))
                GO RECORD nRec IN FStyle 
              ENDIF
            ENDIF
            *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][End]
  	        SELECT (lcAlias)
  	      ENDIF
          lcStyMaj = STYLE.cstymajor
        ENDSCAN
      ENDIF  
    ENDSCAN
  ELSE
    SCAN FOR IIF(lcRpDomImp <>  'B',IIF(lcRpDomImp = 'I' , !Make , Make),.T.) AND ;
      IIF(!EMPTY(lcStatusVar), Style.Status $ lcStatusVar,.T.) AND IIF(llUseColors ,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),lcColorFile),.T.) AND ;
      IIF(llUseStyGrps,SEEK(STYLE.CSTYGROUP,lcStyGrpFile),.T.) AND IIF(llUseSeasons,SEEK(STYLE.SEASON,lcSeasonFile),.T.) AND ;
      IIF(llUseDiv ,SEEK(STYLE.CDIVISION,lcDivFile),.T.) AND IIF(llSelectFabric ,SEEK(STYLE.FABRIC,lcCursorFabric),.T.)
      IF (lcStyMaj # STYLE.cstymajor)
        m.cColor = SPACE(0)
      ENDIF
      IF !(SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen)  $ m.cColor )
        m.cColor = m.cColor + SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen) + " "
      ENDIF
      *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][Start]
      m.CUPC  =''
      IF gfSEEK(Style.Style,'STYLEUPC')
        =gfSeek('S'+Style.Scale,'SCALE')
        
        FOR ln=1 TO Scale.cnt
           lcN = STR(ln,1)
           IF gfSEEK(Style.Style+lcN ,'STYLEUPC') 
             m.CUPC  = m.CUPC + IIF(!EMPTY(m.CUPC),",","")+Scale.Sz&lcN.+" : "+ALLTRIM(StyleUPC.cupcnum1)+ALLTRIM(StyleUPC.cupcnum2)+ALLTRIM(StyleUPC.cupcnum3)
           ENDIF
        ENDFOR 
      ENDIF
      m.mnotes = ''
      IF gfSeek('F'+SUBSTR(Style.Style,1,lnMajLen),'NOTEPAD')
        m.mnotes = NOTEPAD.mnotes
      ENDIF
      m.CPRO_VALUE  =""
      IF gfSEEK('ST'+Style.Style,'profvalu')
        SELECT profvalu
        SCAN REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = 'ST'+Style.Style
          m.CPRO_VALUE  = m.CPRO_VALUE  + IIF(!EMPTY(m.CPRO_VALUE),", ","")+ ALLTRIM( GFCODDES(profvalu.CPRO_CODE,'CPRO_CODE '))+" : "+ ALLTRIM(profvalu.cpro_value)
        ENDSCAN
      ENDIF
      *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][End]     
      IF (lcStyMaj # STYLE.cstymajor)
        SELECT STYLE
        SCATTER MEMVAR MEMO
        INSERT INTO FStyle FROM MEMVAR
        IF (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL")
          REPLACE cColor WITH SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen) IN FStyle 
        ENDIF
      ELSE
        lcAlias = SELECT(0)
        SELECT FStyle
        REPLACE cColor WITH m.cColor
        *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][Start]
        IF (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL")
          nRec = RECNO()
          SELECT STYLE
          SCATTER MEMVAR MEMO
          INSERT INTO FStyle FROM MEMVAR
          REPLACE cColor WITH SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen) IN FStyle 
          IF BETWEEN(nRec ,1,RECCOUNT('FStyle'))
            GO RECORD nRec IN FStyle 
          ENDIF
        ENDIF
        *E304199,1 MMT 06/26/2024 Enhance the report to export more data[P-ERP-20240611.0003][End]

        SELECT (lcAlias)
      ENDIF
      lcStyMaj = STYLE.cstymajor
    ENDSCAN 
  ENDIF
  *B612627,1 MMT 09/26/2022 Style Master List report gives error if user selected more than 25 item in In List[T20220922.0001][End]
  

  *!*************************************************************
  *! Name      : lfCollTime
  *! Developer : Mohamed Atia Badran (MAB)
  *! Date      : 06/04/99
  *! Purpose   : Calcualte spent time in data collection.
  *!*************************************************************
  *! Passed Parameters  : Start collection date,End collection date
  *!*************************************************************
  *! Returns            : Spent time.
  *!*************************************************************
  *! Example   : =lfCollTime()
  *!*************************************************************
FUNCTION lfCollTime
  PARAMETERS lcStart,lcEnd
  lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
  lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
  lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
  lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
  RETURN (lnEnd - lnStart)
  *-- end of lfCollTime.

  *!*************************************************************
  *! Name      : lfMajTtlGet
  *! Developer : AAMER (AHM)
  *! Date      : 03/25/1998
  *! Purpose   : To get the style major segement title
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfMajTtlGet()
  *!*************************************************************
FUNCTION lfMajTtGet
  RETURN gfItemMask("HM")

  *!*************************************************************
  *! Name      : lfNonMaj
  *! Developer : AAMER (AHM)
  *! Date      : 03/25/1998
  *! Purpose   : To get the style nonmajor segement structure
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfNonMaj()
  *!*************************************************************
FUNCTION lfNonMaj
  *-- Compute Free/Color Items in Style Structure. [Begin]
  lnMajSeg  = gfItemMask('SM')  && No. of major segments.
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
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  *lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
  lcColorTt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ONLYTHIS,oAriaApplication.GetHeaderText("LANG_ONLYTHIS",AHEADERFILE)) +' ' + ALLTRIM(lcNonMajT)
  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  *-- Compute Free/Color Items in Style Structure. [End]
  RETURN ''

  *!*************************************************************
  *! Name      : lfwRepWhen
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : Option Grid When function
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfwRepWhen()
  *!*************************************************************
FUNCTION lfwRepWhen
  *-- Check the cost access

  lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
    ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

  *lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
  ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

  *-- Disable/enable Only This colors, Free Segment. [begin]

  DO CASE
    CASE lcFreeClr = 'C'
      laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
      = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
      *laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
      *= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    CASE lcFreeClr = 'F'
      *laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
      *= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
      laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
      = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  ENDCASE
  *-- end of lfwRepWhen.

  *!*************************************************************
  *! Name      : lfAdjPrnArr
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : Prepare the arrays that hold the print by values
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : .....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfAdjPrnArr()
  *!*************************************************************
FUNCTION lfAdjPrnArr
  DIMENSION laRPPrnDsp[2,1],laRPPrnRet[2,1]

  laRPPrnDsp[1,1]=lcMajTtl
  laRPPrnDsp[2,1]=lcNonMajT

  laRPPrnRet[1,1]='S'
  laRPPrnRet[2,1]='C'

  *!*************************************************************
  *! Name      : lfMajPic
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : get major segment picture
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : .....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfMajPic()
  *!*************************************************************
FUNCTION lfMajPic
  lcMajPic = "@! " + gfItemMask("PM")
  RETURN lcMajPic

  *!*************************************************************
  *! Name      : lfvStyle
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : validate style
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : .....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfvStyle()
  *!*************************************************************
FUNCTION lfvStyle

  lcStyle = VARREAD()

  lcTag = ORDER('STYLE')

  SET ORDER TO cStyle IN STYLE

  IF LASTKEY() = 13 AND !MDOWN()
    IF SEEK(&lcStyle.,'Style')
      &lcStyle = STYLE.cstymajor
    ELSE
      &lcStyle = gfStyBrw('M',"","",.F.)
    ENDIF
  ELSE
    &lcStyle = ''
  ENDIF
  SET ORDER TO lcTag IN STYLE

  *!*************************************************************
  *! Name      : lfvFabric
  *! Developer : AAMER (AHM)
  *! Date      : 05/27/1998
  *! Purpose   : validate fabric
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : .....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfvFabric()
  *!*************************************************************
FUNCTION lfvFabric
  lcFabObj = VARREAD()

  lcFab    = &lcFabObj

  llUseByMe = .F.

  IF !USED('FABRIC')
    llUseByMe = .T.
    USE (gcDataDir+'FABRIC') IN 0 SHARE
  ENDIF

  lcTag = ORDER('FABRIC')

  SET ORDER TO fabric IN fabric

  IF LASTKEY() = 13 AND !MDOWN()
    IF SEEK(lcFab,'FABRIC')
      &lcFabObj = fabric.fabric
    ELSE
      = FaBrow(@lcFab,'*')
      &lcFabObj = lcFab
    ENDIF
  ELSE
    &lcFabObj = ''
  ENDIF

  SET ORDER TO fabric IN fabric

  IF llUseByMe
    USE IN fabric
  ENDIF


  *!*************************************************************
  *! Name      : lfsrvSty
  *! Developer : Mohamed Badran (MAB)
  *! Date      : 05/27/98
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
  *B802264,1
FUNCTION lfSRVSty
  PARAMETERS lcParm
  DO CASE
    CASE lcParm = 'S'  && Set code
      *-- open this file in another alias to set order to Style Major
      *-- unique index.
      USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE IN 0
      SELECT STYLE
      SET ORDER TO TAG cStyle
      SET RELATION TO STYLE.STYLE INTO STYLE_X
      GO TOP IN STYLE
    CASE lcParm = 'R'  && Reset code
      USE IN STYLE_X
      SELECT STYLE
      SET ORDER TO TAG STYLE
  ENDCASE
  *-- end of lfsrvSty.

  *!*************************************************************
  *! Name      : lfStySum
  *! Developer : Mohamed Badran (MAB)
  *! Date      : 05/27/98
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
  *B802264,1
FUNCTION lfStySum
  PARAMETERS lcSty,lccomp,lnAddToVar
  PRIVATE lnStyRec
  lnTotcomp = 0

  IF RECCOUNT('STYLE') != 0
    lnStyRec = RECNO('STYLE')
    SELECT STYLE_X
    SUM &lccomp TO lnTotcomp WHILE STYLE = ALLTRIM(lcSty)
    SELECT STYLE
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
  *-- end of lfStySum.

  *!*************************************************************
  *! Name      : lfvBins      *:E301271,1
  *! Developer : Ahmed Salah Shalaby -(SSH)
  *! Date      : 01/07/99
  *! Purpose   : Valid Function for Bins
  *!           :
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code
  *!*************************************************************
  *! Passed Parameters  : String have Pipes,Number of Pieps.
  *!*************************************************************
  *! Returns            : InList Expression like ["AS","BS","CS"]
  *!*************************************************************
  *! Example   : = lfvBins()
  *!*************************************************************
FUNCTION lfvBins
  *N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
  *= gfMover(@laRpSource,@laRpTarget,'Style Bins',.T.,'')
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  *= lfOGMover(@laRpSource,@laRpTarget,'Style Bin',.T.,'')  && call mover function.
  = lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STYLEBIN,oAriaApplication.GetHeaderText("LANG_STYLEBIN",AHEADERFILE)),.T.,'')  && call mover function.
  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  *N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]

  *!*************************************************************
  *! Name      : lfFillBin     *:E301271,1
  *! Developer : Ahmed Salah Shalaby -(SSH)
  *! Date      : 01/07/99
  *! Purpose   : Function to fill bins arrays
  *!           :
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code
  *!*************************************************************
  *! Example   : = lfFillBin()
  *!*************************************************************
FUNCTION lfFillBin
  llUSdBy =gfOpenFile(gcDataDir+'WHSLOC' ,'WHSLOC','SH')
  DIME laRpSource[1,1]
  DIME laRpTarget[1,1]
  SELECT WHSLOC
  SELECT DISTINCT CLOCATION FROM WHSLOC WHERE !EMPTY(CLOCATION)INTO ARRAY laRpSource
  USE IN IIF(llUSdBy,'WHSLOC',0)

  *!*************************************************************
  *! Name      : lfClearRed *:E301271,1
  *! Developer : Ahmed Salah Shalaby -(SSH)
  *! Date      : 01/07/99
  *! Purpose   : Functio to clear read
  *!           :
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Report code
  *!*************************************************************
  *! Example   : = lfClearRed()
  *!*************************************************************
FUNCTION lfClearRed
  CLEAR READ

  *****************************************
FUNCTION lfGetStr

  PRIVATE lcTmpAlias
  lcTmpAlias  = SELECT(0)

  lcSizStr    = ''
  lcPrePakStr = ''
  *****************************
  *B611446,1 MHM 11/14/2017 Master style report cannot handle extended size scale case [T20171009.0018]
  IF gfGetMemvar('M_USEEXSSC')=.T.
    *B611505,AHH 09/01/2018 the style master list report prints only the first scale sizes only [T20171009.0018] [Start]
    lnScaleWidth = gfGetMemvar('M_EXTWIDTH', oAriaApplication.ActiveCompanyID)
    *B611505,AHH 09/01/2018 the style master list report prints only the first scale sizes only [T20171009.0018] [End]
    SELECT SCALE
    *B611505,AHH 09/01/2018 the style master list report prints only the first scale sizes only [T20171009.0018] [Start]
    *!*	    lcscale=SUBSTR(FStyle.SCALE,1,2)
    lcscale=SUBSTR(FStyle.SCALE,1,lnScaleWidth)
    *B611505,AHH 09/01/2018 the style master list report prints only the first scale sizes only [T20171009.0018] [End]

    IF SEEK('S'+lcscale)
      SCAN REST WHILE SCALE  = lcscale
        FOR i = 1 TO SCALE.CNT
          lcstr = ALLTRIM(STR(i))
          lcSizStr = lcSizStr + SCALE.SZ&lcstr + " "
        ENDFOR
      ENDSCAN
    ENDIF
    IF SEEK('P'+lcscale+FStyle.prepak)
      SCAN REST WHILE SCALE  = lcscale
        FOR i = 1 TO SCALE.CNT
          lcstr = ALLTRIM(STR(i))
          lcPrePakStr = lcPrePakStr + SCALE.Pp&lcstr + " "
        ENDFOR
      ENDSCAN
    ENDIF
  ELSE
    *B611446,1 MHM 11/14/2017 Master style report cannot handle extended size scale case [T20171009.0018]
    SELECT SCALE
    IF SEEK('S'+FStyle.SCALE)
      lcSizStr = Sz1 + ' ' + Sz2 + ' ' + Sz3 + ' ' + Sz4 + ' ' + Sz5 + ' ';
        + Sz6 + ' ' + Sz7 + ' ' + Sz8
    ENDIF
    IF SEEK('P'+FStyle.SCALE+FStyle.prepak)
      lcPrePakStr = STR(Pp1) + STR(Pp2) + STR(Pp3) + STR(Pp4) + STR(Pp5) + ;
        STR(Pp6) + STR(Pp7) + STR(Pp8)
    ENDIF
  ENDIF
  SELECT (lcTmpAlias)
  RETURN lcPrePakStr

  *!**************************************************************
  * Name      : lfModeVld
  * Developer : Ramy Mabrouk
  * Date      : 11/24/99
  * Purpose   : Report Mode Validation
  * Job No.   : B# 803531,1
  *!*************************************************************
  * Called from : Option Grid
  *!*************************************************************
  * Passed Parameters  : None
  *!*************************************************************
  * Returns            : None
  *!*************************************************************
  * Example   : =lfModeVld()
  *!*************************************************************
  *-B803531,1 RAMY [start]
FUNCTION lfModeVld
  CLEAR READ
  *-- end of lfModeVld.

  *N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
  *!*************************************************************
  *! Name      : lfCreatExp
  *! Developer : MARIAM MAZHAR (MMT)
  *! Date      : 12/04/2003
  *! Purpose   : Return the selected status in the ReadBox
  *!*************************************************************
FUNCTION lfCreatExp
  IF TYPE('llCallFromScr') = 'L'
    =ACOPY(loOgScroll.laOGFxFlt , laFxExpr)
    =ACOPY(loOgScroll.laOGVrFlt , laVrExpr)
  ENDIF

  *!*************************************************************
  *! Name      : RefreshStatus
  *! Developer : MARIAM MAZHAR (MMT)
  *! Date      : 12/04/2003
  *! Purpose   : Return the selected status in the ReadBox
  *!*************************************************************
  *! Passed Parameters  :
  *!*************************************************************
  *! Returns            :
  *!***************************************************************************
  *! Modification:
  *!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF
  RETURN lcStatusStr
ENDFUNC
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]
*MMT
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 05/17/2022
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'COLOR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

CASE ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0 
CASE ALLTRIM(lcFieldName) = 'CSTYGROUP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0 

ENDCASE
= gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
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
RETURN .T.
*MMT
