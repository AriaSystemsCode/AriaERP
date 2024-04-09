*!********************************************************************
*: Program file  : ALDSCRP.PRG
*: Program desc. : CUSTOMIZED discrepancy report for Low10
*: For screen    : None.
*: System        : ARIA APPAREL SERIES 4XP
*: Module        : ALLOCATION (AL)(C200851)T20070720.0006
*: Developer     : Hassan Ibrahim Ali [HIA]
*: Date          : 09/24/2007
*!********************************************************************
*! Modifications:
*! C200953,1 MMT 02/27/2008 Add Detailed Format to the report[T20080110.0002]
*!********************************************************************

*! C200953,1 MMT 02/27/2008 Add Detailed Format to the report[Start]
IF lcRpChos = 'B'
  llWareSelect =.F.
  lcRpForm  = 'ALDSCRPD'
  lcCursorWare = ""
  lfGetDetData()
  RETURN 
ELSE
  lcRpForm  = 'ALDSCRP'
ENDIF 
*! C200953,1 MMT 02/27/2008 Add Detailed Format to the report[End]


IF !USED('ALDSCRP')
  = gfOpenFile(oAriaApplication.DataDir+'ALDSCRP','ALDSCRP', "SH",'ALDSCRP')
ENDIF

lcTMPFact = loOgScroll.gfTempName()

SELECT ALDSCRP
lcTMPALDSCRP1 = loOgScroll.gfTempName()
= AFIELDS(laFIELDS)

CREATE TABLE (oAriaApplication.WorkDir+lcTMPALDSCRP1) FROM ARRAY laFIELDS
USE IN (lcTMPALDSCRP1)

USE (oAriaApplication.WorkDir+lcTMPALDSCRP1) IN 0 EXCL
SELECT (lcTMPALDSCRP1)
ALTER TABLE (lcTMPALDSCRP1) ALTER COLUMN npack_qty NUMBER(9,0)
ALTER TABLE (lcTMPALDSCRP1) ALTER COLUMN nrcv_qty  NUMBER(9,0)
ALTER TABLE (lcTMPALDSCRP1) ALTER COLUMN nadv_qty  NUMBER(9,0)


ALTER TABLE (lcTMPALDSCRP1) ADD COLUMN nDiff  NUMBER(7,0)
ALTER TABLE (lcTMPALDSCRP1) ADD COLUMN nOut   NUMBER(8,0)
ALTER TABLE (lcTMPALDSCRP1) ADD COLUMN nDscRp NUMBER(8,2)
INDEX ON cFACT+STR(nWEEK,2)+STR(nYEAR,4)+STR(nCALLOFF,6)+STR(nsku,8) TAG(lcTMPALDSCRP1)
USE IN (lcTMPALDSCRP1)

USE (oAriaApplication.WorkDir+lcTMPALDSCRP1) IN 0 SHARED ORDER 1
SELECT (lcTMPALDSCRP1)

*--------------------------------------------------------------------*
SELECT ALDSCRP
lcTMPALDSCRP = loOgScroll.gfTempName()
= AFIELDS(laFIELDS)

CREATE TABLE (oAriaApplication.WorkDir+lcTMPALDSCRP) FROM ARRAY laFIELDS
USE IN (lcTMPALDSCRP)

USE (oAriaApplication.WorkDir+lcTMPALDSCRP) IN 0 EXCL
SELECT (lcTMPALDSCRP)
ALTER TABLE (lcTMPALDSCRP) ALTER COLUMN npack_qty NUMBER(9,0)
ALTER TABLE (lcTMPALDSCRP) ALTER COLUMN nrcv_qty  NUMBER(9,0)
ALTER TABLE (lcTMPALDSCRP) ALTER COLUMN nadv_qty  NUMBER(9,0)


ALTER TABLE (lcTMPALDSCRP) ADD COLUMN nDiff  NUMBER(7,0)
ALTER TABLE (lcTMPALDSCRP) ADD COLUMN nOut   NUMBER(8,0)
ALTER TABLE (lcTMPALDSCRP) ADD COLUMN nDscRp NUMBER(8,2)
INDEX ON cFACT+STR(nWEEK,2)+STR(nYEAR,4)+STR(nCALLOFF,6) TAG(lcTMPALDSCRP)
USE IN (lcTMPALDSCRP)

USE (oAriaApplication.WorkDir+lcTMPALDSCRP) IN 0 SHARED ORDER 1
SELECT (lcTMPALDSCRP)


IF lfFilData()
  SELECT (lcTMPALDSCRP)
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF
SELECT (lcTMPALDSCRP)

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 09/10/2007
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : --
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************

FUNCTION lfwOGWhen

DIMENSION laCallStr[7,4]
laCallStr[1,1] = 'CCALLOFF'
laCallStr[1,2] = 'C'
laCallStr[1,3] = 6
laCallStr[1,4] = 0


laCallStr[2,1] = 'nWeek'
laCallStr[2,2] = 'N'
laCallStr[2,3] = 2
laCallStr[2,4] = 0

laCallStr[3,1] = 'Nyear'
laCallStr[3,2] = 'N'
laCallStr[3,3] = 4
laCallStr[3,4] = 0

laCallStr[4,1] = 'Cdept'
laCallStr[4,2] = 'C'
laCallStr[4,3] = 3
laCallStr[4,4] = 0

laCallStr[5,1] = 'CFact'
laCallStr[5,2] = 'C'
laCallStr[5,3] = 4
laCallStr[5,4] = 0

laCallStr[6,1] = 'nStore'
laCallStr[6,2] = 'N'
laCallStr[6,3] = 4
laCallStr[6,4] = 0

laCallStr[7,1] = 'dcancel'
laCallStr[7,2] = 'D'
laCallStr[7,3] = 8
laCallStr[7,4] = 0


=gfCrtTmp(lcCallTmp,@laCallStr,"STR(NYEAR,4)+STR(NWEEK,2)+CFACT+STR(NSTORE,4)+cCALLOFF",lcCallTmp,.T.)


  RETURN .T.
ENDFUNC

*!*************************************************************
*! Name      : lfFilData
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 09/10/2007
*! Purpose   : Fill the temp file data
*!*************************************************************
*! Called from : Aldscrp main
*!*************************************************************
*! Calls       : --
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFilData()
*!*************************************************************

FUNCTION lfFilData

  IF lnWEEKNO = 0
    lcMessage = 'Week value must be in the range 01 - 53'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    RETURN  .F.
  ENDIF

  IF lnyear = 0
    lcMessage = 'Year value not found in the DB.'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    RETURN  .F.
  ENDIF

  llSelectWarwhous = .F.
  lnPosWarehous = 0

  lnPosWarehous = ASCAN(loOgScroll.laOgFXFlt,"WAREHOUS.CWARECODE")
  IF lnPosWarehous > 0
    lnPosWarehous = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnPosWarehous,1)
    lcCursorWarehous = loOgScroll.laOgFXFlt[lnPosWarehous,6]
    IF !EMPTY(lcCursorWarehous)
      SELECT(lcCursorWarehous)
      LOCATE
      IF !EOF()
        llSelectWarwhous = .T.
        SELECT 0
        SELECT * FROM  (lcCursorWarehous) INTO CURSOR (lcTMPFact) Readwrite
      ENDIF
    ENDIF
  ENDIF
  IF !llSelectWarwhous
    lcMessage = 'Please Select Factory Value.'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    RETURN  .F.
  ELSE
    SELECT (lcTMPFact)
    REPLACE cwarecode WITH SUBSTR(ALLTRIM(cwarecode),3,4) ALL
    REPLACE keyexp WITH SUBSTR(ALLTRIM(keyexp),3,4) ALL
    lcCursorWarehous = lcTMPFact
  ENDIF


  *-------------------------------------------------------------------------------*
  *--Dept
  llSelectDept = .F.
  lnPosDept = 0
  lcDepts   = ''

  lnPosDept = ASCAN(loOgScroll.laOgFXFlt,"LCPURCODE")
  IF lnPosDept > 0
    lnPosDept = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnPosDept,1)
    lcDepts = loOgScroll.laOgFXFlt[lnPosDept,6]
    IF !EMPTY(lcDepts)
      lcDeptCursor = loOgScroll.gfTempName()
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='Dept'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      =gfCrtTmp(lcDeptCursor,@laTempacstru,"Dept",lcDeptCursor,.T.)
      lnStart=1
      lnEnd=AT('|',lcDepts)
      DO WHILE lnEnd <> 0
        SELECT(lcDeptCursor)
        APPEND BLANK

        *REPLACE Dept WITH SUBSTR(gfCodDes(SUBSTR(lcDepts,lnStart,lnEnd-1), 'CPURCODE'),1,4)

        lcDepts = STUFF(lcDepts ,lnStart,lnEnd,"")
        lnEnd=AT('|',lcDepts)
        llSelectDept = .T.
      ENDDO
      IF lnEnd = 0
        SELECT(lcDeptCursor)
        APPEND BLANK
        REPLACE Dept WITH SUBSTR(gfCodDes(lcDepts, 'CPURCODE'),1,4)
        llSelectDept = .T.
      ENDIF
    ENDIF
  ENDIF

  IF !llSelectDept
    lcMessage = 'Please Select Department Value.'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    RETURN  .F.
  ENDIF

  *------------------------------------------------------------------------------------------------*

  &&FILTER PARAMETERS ARE [lnWEEKNO, LNYEAR,LCPURCODE,LcWAREHOUS,lnDiff]
  &&Index expression is 'ALDSCRP' [CFACT+STR(NWEEK,2)+STR(NYEAR,2)+STR(NSTORE,4)+STR(NCALLOFF,6) ]
  SELECT (lcTMPALDSCRP1)
  DELETE ALL
  SELECT (lcTMPALDSCRP )
  DELETE ALL
  lcWEEKNO = STR(lnWEEKNO,2)
  lcWEEKNO = PADL(IIF(SUBSTR(lcWEEKNO,3,1)='0',RIGHT(lcWEEKNO,1),RIGHT(lcWEEKNO,2)),2,' ')
  *-------------------------------------------------------------------------------------------------*


  SELECT ALDSCRP
  SCAN FOR IIF(lnWEEKNO <>0,( STR(nWEEK,2) = lcWEEKNO),.T.) AND IIF(lnyear <> 0,( STR(nYEAR,4) = STR(lnyear,4)),.T.)
    IF llSelectDept
      IF !SEEK(ALDSCRP.cDept,lcDeptCursor)
        LOOP
      ENDIF
    ENDIF
    IF llSelectWarwhous
      *IF !SEEK(ALDSCRP.cFACT,lcCursorWarehous)
      SELECT (lcCursorWarehous)
      GO TOP
      LOCATE FOR &lcCursorWarehous..keyexp = ALDSCRP.cFACT
      IF !FOUND(lcCursorWarehous)
        LOOP
      ENDIF
    ENDIF
    WAIT WINDOW 'Processing Callof # ' + STR(ALDSCRP.nCALLOFF,6) NOWAIT

    SELECT ALDSCRP
    SCATTER MEMVAR MEMO
    lcCALLOFF = PADL(STR(ALDSCRP.nCALLOFF,6),6,' ')
    SELECT (lcTMPALDSCRP1)
    IF SEEK(ALDSCRP.cFACT+STR(ALDSCRP.nWEEK,2)+STR(ALDSCRP.nYEAR,4)+lcCALLOFF  + STR(ALDSCRP.nsku,8))
      REPLACE npack_qty WITH  npack_qty + m.npack_qty,;
      nrcv_qty  WITH  nrcv_qty  + m.nrcv_qty,;
      nadv_qty  WITH  nadv_qty  + m.nadv_qty,;
      nDiff     WITH  ABS(nrcv_qty - nadv_qty)

    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
  ENDSCAN
  SELECT (lcTMPALDSCRP1)
  SCAN FOR IIF(lnWEEKNO <>0,( STR(nWEEK,2) = lcWEEKNO),.T.) AND IIF(lnyear <> 0,( STR(nYEAR,4) = STR(lnyear,4)),.T.)
    IF llSelectDept
      IF !SEEK(&lcTMPALDSCRP1..cDept,lcDeptCursor)
        LOOP
      ENDIF
    ENDIF
    IF llSelectWarwhous
      *IF !SEEK(&lcTMPALDSCRP1..cFACT,lcCursorWarehous)
      SELECT (lcCursorWarehous)
      GO TOP
      LOCATE FOR &lcCursorWarehous..keyexp = &lcTMPALDSCRP1..cFACT
      IF !FOUND(lcCursorWarehous)
        LOOP
      ENDIF
    ENDIF
    WAIT WINDOW 'Processing Callof # ' + STR(&lcTMPALDSCRP1..nCALLOFF,6) NOWAIT

    SELECT (lcTMPALDSCRP1)
    SCATTER MEMVAR MEMO
    lcStyle = ''
    lcSize  = ''
    lncallof_qty = 0
    =lfgetstyle(ALLTRIM(STR(&lcTMPALDSCRP1..nsku)))
    SELECT ALCALOFF
    SET ORDER TO ALCALOFF
    IF SEEK(STR(&lcTMPALDSCRP1..nCALLOFF,6))
      SCAN REST WHILE CCALLOFF+STYLE = STR(&lcTMPALDSCRP1..nCALLOFF,6)
        IF STYLE = lcStyle
          lncallof_qty = lncallof_qty + NVL(ALCALOFF.Qty&lcSize,0)
        ENDIF
      ENDSCAN
    ENDIF

    SELECT (lcTMPALDSCRP )
    lcCALLOFF = PADL(STR(&lcTMPALDSCRP1..nCALLOFF,6),6,' ')
    IF SEEK(&lcTMPALDSCRP1..cFACT+STR(&lcTMPALDSCRP1..nWEEK,2)+STR(&lcTMPALDSCRP1..nYEAR,4)+lcCALLOFF)
      REPLACE npack_qty WITH  npack_qty + m.npack_qty,;
      nrcv_qty  WITH  nrcv_qty  + m.nrcv_qty,;
      nadv_qty  WITH  nadv_qty  + m.nadv_qty,;
      nDiff     WITH  ABS(nrcv_qty - nadv_qty),;
      nOut      WITH  nOut + ABS(lncallof_qty - m.nrcv_qty)
    ELSE
      m.nDiff  = ABS(m.nrcv_qty - m.nadv_qty)
      m.nOut   = ABS(lncallof_qty - m.nrcv_qty)
      APPEND BLANK
      GATHER  MEMVAR MEMO
    ENDIF

    SELECT (lcTMPALDSCRP1)

  ENDSCAN

  SELECT (lcTMPALDSCRP )
  SCAN
    REPLACE nDscRp WITH (((nDiff+nOut) /nadv_qty )*100)
    IF nDscRp < lnDiff
      DELETE
    ENDIF

  ENDSCAN

  IF USED(lcCursorWarehous)
    USE IN (lcCursorWarehous)
  ENDIF

ENDFUNC
*!*************************************************************
*! Name      : lfvWeek
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 09/10/2007
*! Purpose   : Option Grid week validate function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : --
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvweek()
*!*************************************************************

FUNCTION lfvweek
  IF BETWEEN(lnWEEKNO,1,53)
    RETURN .T.
  ELSE
    lcMessage = 'Week value must be in the range 01 - 53'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    RETURN .F.
  ENDIF
ENDFUNC
*!*************************************************************
*! Name      : lfvYear
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 09/10/2007
*! Purpose   : Option Grid - Year validate function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : --
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvyear()
*!*************************************************************

FUNCTION lfvyear

  IF !USED('ALDSCRP')
    = gfOpenFile(oAriaApplication.DataDir+'ALDSCRP','ALDSCRP', "SH",'ALDSCRP')
    &&INDEX EXPR [CFACT+STR(NWEEK,2)+STR(NYEAR,2)+STR(NSTORE,4)+STR(NCALLOFF,6) ]
  ENDIF

  SELECT ALDSCRP
  SET ORDER TO ALDYEAR

  lcYear = ALLTRIM(STR(lnyear,4))
  *lcindex_Year = PADL(IIF(SUBSTR(lcYear,3,1)='0',RIGHT(lcYear,1),RIGHT(lcYear,2)),2,' ')


  IF LEFT(lcYear,2) = '20' AND LEN(ALLTRIM(lcYear))=4 AND SEEK(lcYear) AND BETWEEN(lnyear,01,9999)
    RETURN .T.
  ELSE
    lcMessage = 'Year value not found in the DB.'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    lnyear = 0
    RETURN .F.
  ENDIF
ENDFUNC
*!*************************************************************
*! Name      : lfgetStyle
*! Developer : ABDOU ELGENDI -  (ABD)
*! Date      : 08/28/2003
*! Purpose   : Function to get the style from the spck_lin file.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Prog.
*!*************************************************************
*! Example   : = lfgetStyle()
*!*************************************************************
*!
FUNCTION lfgetstyle
  LPARAMETERS lcSndPckId

  PRIVATE lnAlias , llgetStyle
  llgetStyle = .F.
  IF TYPE('lcSndPckId') = 'U'
    RETURN
  ENDIF
  lnAlias = SELECT(0)

  SELECT Spck_lin
  IF SEEK('S')
    SCAN REST WHILE TYPE+account+pack_id = 'S' FOR lcSndPckId $ ALLTRIM(pack_id)
      lcStyle = Spck_lin.STYLE
      FOR I = 1 TO 8
        *-- Hold the size scale.
        lcSize = STR(I,1)
        IF Spck_lin.Qty&lcSize = 1
          EXIT
        ENDIF
      ENDFOR
      llgetStyle = .T.
      EXIT
    ENDSCAN
  ENDIF

  SELECT(lnAlias)
  RETURN llgetStyle

  *!*************************************************************
  *! Name      : lfvDISC
  *! Developer : Hassan Ibrahim Ali [HIA]
  *! Date      : 09/10/2007
  *! Purpose   : Option Grid - Discrepancy validate function
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Calls       : --
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfvDISC()
  *!*************************************************************
FUNCTION lfvDISC

  IF BETWEEN(lnDiff,0.01,99.99)
    RETURN .T.
  ELSE
    lcMessage = 'Highlight % Difference value out of range [0.01-99.99]'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    lnyear = 0
    RETURN .F.
  ENDIF
ENDFUNC


*! C200953,1 MMT 02/27/2008 Add Detailed Format to the report[Start]
*!*************************************************************
*! Name      : lfvChoose
*: Developer : MAriam Mazhar (MMT)
*: Date      : 02/27/2008
*! Purpose   : refresh grid when user change format
*!*************************************************************
FUNCTION lfvChoose
IF lcRpChos = 'B'
  lcRpForm = 'ALDSCRPD'
ELSE
  lcRpForm = 'ALDSCRP'
ENDIF   
CLEARREAD()

*!*************************************************************
*! Name      : lfGetDetData
*: Developer : MAriam Mazhar (MMT)
*: Date      : 02/27/2008
*! Purpose   : data collection in case of detailed format
*!*************************************************************
FUNCTION lfGetDetData


IF lnWEEKNO = 0
  lcMessage = 'Week value must be in the range 01 - 53'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
  RETURN  .F.
ENDIF

IF lnyear = 0
  lcMessage = 'Year value not found in the DB.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
  RETURN  .F.
ENDIF


lcDataFile = loogscroll.gfTempName()
lfCrtTemp()


llUsePurCode  = .F.
lcCodSelFile = ''
lnCodPos = ASCAN(loOgScroll.laOgFXFlt,"LCPURCODE")
IF lnCodPos > 0 
  lnCodPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnCodPos,1)
  lcCodSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnCodPos,6]),loOgScroll.laOgFXFlt[lnCodPos,6],'')
  IF !EMPTY(lcCodSel) 
    lcCodSelFile = loOGScroll.gfTempName()
    llUsePurCode = IIF(LEN(lcCodSel )>0,.T.,.F.) AND lfConvertToCursor(lcCodSel,'CPURCODE',lcCodSelFile)
  ENDIF   
ENDIF   

IF !llUsePurCode 
  lcMessage = 'Please Select Department Value.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
  RETURN  .F.
ENDIF   

llWareSelect = .F.
lcCursorWare = ""
lnWarePos = ASCAN(loOgScroll.laOgFXFlt,"WAREHOUS.CWARECODE")
IF lnWarePos >0 
  lnWarePos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnWarePos,1)
  lcCursorWare = loOgScroll.laOgFxFlt[lnWarePos,6]
ENDIF       
IF !EMPTY(lcCursorWare)
  SELECT(lcCursorWare)
  LOCATE 
  IF !EOF()
    llWareSelect = .T.
  ELSE
    lcMessage = 'Please Select Factory Value.'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
    RETURN  .F.   
  ENDIF 
ENDIF 
IF !llWareSelect
  lcMessage = 'Please Select Factory Value.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
  RETURN  .F.   
ENDIF 

llCallSelect = .F.
lcCursorCall = ""
lnCallPos = ASCAN(loOgScroll.laOgFXFlt,"ALCALOFF.CCALLOFF")
IF lnCallPos >0 
  lnCallPos = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnCallPos ,1)
  lcCursorCall= loOgScroll.laOgFxFlt[lnCallPos ,6]
ENDIF       
IF !EMPTY(lcCursorCall) AND USED(lcCursorCall)
  SELECT(lcCursorCall)
  LOCATE 
   IF !EOF()
    llCallSelect = .T.
  ENDIF 
ENDIF 

IF !USED('ALDSCRP')
  = gfOpenFile(oAriaApplication.DataDir+'ALDSCRP','ALDYEAR', "SH",'ALDSCRP')
ENDIF

IF !USED('ALCalOf')
  = gfOpenFile(oAriaApplication.DataDir+'ALCalOff','ALCalOff', "SH",'ALCalOf')
ENDIF

IF !USED('spck_lin')
  = gfOpenFile(oAriaApplication.DataDir+'spck_lin','spck_lin', "SH",'spck_lin')
ENDIF

IF !USED('SCALE')
  = gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE', "SH",'SCALE')
ENDIF

IF !USED('STYLE')
  = gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE', "SH",'STYLE')
ENDIF


IF !USED('PIKLINE')
  = gfOpenFile(oAriaApplication.DataDir+'PIKLINE','PIKLINE', "SH",'PIKLINE')
ENDIF



IF !USED('almnfhst')
  = gfOpenFile(oAriaApplication.DataDir+'almnfhst','ALMNFHST', "SH",'ALMNFHST')
ENDIF


SELECT ALDSCRP
SET ORDER TO ALDYEAR
=SEEK(STR(lnyear ,4)+STR(lnWEEKNO ,2))
SCAN REST WHILE STR(NYEAR,4)+STR(NWEEK,2)+CFACT+STR(NSTORE,4)+STR(NCALLOFF,6) =  STR(lnyear ,4)+STR(lnWEEKNO ,2) FOR;   
         IIF(llCallSelect ,SEEK(PADl(ALLTRIM(STR(ALDSCRP.nCallOFf,6)),6,'0') ,lcCursorCall),.T.) AND ;
         IIF(llUsePurCode  ,SEEK(ALDSCRP.cDept,lcCodSelFile),.T.)

  WAIT WINDOW 'Processing Callof # ' + STR(ALDSCRP.nCALLOFF,6) NOWAIT
  SELECT ALDSCRP
  m.cDept     = ALDSCRP.cDept
  m.CWeek     = STR(ALDSCRP.Nweek,2) 
  m.CYear     = STR(ALDSCRP.nYear,4) 
  m.CalOfNo   = PADl(ALLTRIM(STR(ALDSCRP.nCallOFf,6)),6,'0') 
  =SEEK(m.CalOfNo,'ALCalOf','ALCALOFF')
  m.CallDate  = ALCalOf.Dadd_date
  m.CWARECODE = ALCalOf.Cwarecode
  m.SKU       = ALDSCRP.nsku 
  m.Style     = aldscrp.cstymajor 
  m.Colour    = aldscrp.cclr_desc 
  m.SIZE      = aldscrp.csz_desc
  m.STORE     = aldscrp.nstore 
  m.Advised   = aldscrp.nadv_qty 
  m.Received  = aldscrp.nrcv_qty 
  m.CanDate   = aldscrp.dcancel 
  
  lcStyle = ''
  SELECT spck_lin
  =SEEK('S')
  LOCATE REST WHILE TYPE+account+pack_id = 'S' FOR ALLTRIM(STR(m.SKU)) $ ALLTRIM(pack_id)
  IF FOUND()
    lcStyle = spck_lin.Style
    lnReqSz =''
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      IF spck_lin.Qty&lcI = 1
        lnReqSz = lnI
        EXIT 
      ENDIF 
    ENDFOR 
    
    m.OrgQty = 0
    m.SntQty = 0
    m.PIKTKT = ''
    m.INVOICE = ''
    m.INVDATE = {}
    m.TruRef  = ''  
    m.Ndiff  = 0
    SELECT ALCalOf
    SET ORDER TO CALLDEPT   
    IF SEEK(m.CalOfNo + Str(aldscrp.nstore ,4))
      LOCATE REST WHILE CCALLOFF+CMSSTORE+CDEPOT+STYLE = m.CalOfNo + Str(aldscrp.nstore ,4) FOR IIF(llWareSelect ,SEEK(ALCalOf.Cwarecode,lcCursorWare),.T.)
      IF !FOUND()
        LOOP 
      ENDIF 
      =SEEK(m.CalOfNo + Str(aldscrp.nstore ,4))
      SCAN REST WHILE CCALLOFF+CMSSTORE+CDEPOT+STYLE = m.CalOfNo + Str(aldscrp.nstore ,4) FOR Style = lcStyle ;
                    AND  IIF(llWareSelect ,SEEK(ALCalOf.Cwarecode,lcCursorWare),.T.)
      
      
        m.CWARECODE = ALCalOf.Cwarecode
        lcReqSz = STR(lnReqSz ,1)
        m.OrgQty    = ALCalOf.norigqty&lcReqSz 
        m.SntQty    = ALCalOf.qty&lcReqSz 
        m.PIKTKT    = ALCalOf.piktkt 
        =SEEK(m.PIKTKT,'PIKLINE')
        m.INVOICE   = PIKLINE.INVOICE   
        m.INVDATE   = PIKLINE.INVDATE
        SELECT ALMNFHST
        LOCATE FOR ALMNFHST.ccalloff = ALCalOf.ccalloff ;
         AND ALMNFHST.Cdept = m.cDept AND ALMNFHST.cmsstore = STR(m.STORE ,4) AND ALMNFHST.style = lcStyle 
        IF FOUND()
          m.TruRef    =ALMNFHST.ctrkref 
        ELSE
          m.TruRef    = ''  
        ENDIF   
        m.Ndiff  = m.Advised - m.SntQty 
        SELECT ALCalOf
      ENDSCAN 
    ELSE
      *m.CWARECODE = ALDSCRP.CFact
      LOOP 
    ENDIF   
    INSERT INTO (lcDataFile) FROM MEMVAR 
  ENDIF 
ENDSCAN  

SELECT (lcDataFile)
LOCATE 

loOGScroll.cCROrientation='L'
loOGScroll.cCRPapersize ='A4'

IF !EOF() 
  DO gfDispRe WITH EVALUATE('lcRpForm')
ELSE
 =gfModalGen('TRM00052B40011','ALERT')
 RETURN .F.
ENDIF   

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 02/27/2008
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  CASE   ALLTRIM(lcFieldName) = 'CPURCODE'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 4 
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
    REPLACE &lcFieldName  WITH SUBSTR(gfCodDes(SUBSTR(lcValuesToConvert,lnStart,lnEnd-1), 'CPURCODE'),1,4)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH  SUBSTR(gfCodDes(lcValuesToConvert , 'CPURCODE'),1,4)
  ENDIF 
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : lfCrtTemp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 02/27/2008
*! Purpose   : create tmp file
*!*************************************************************
FUNCTION lfCrtTemp
DIMENSION laFileStr[21,4]

laFileStr[1,1] = 'CWARECODE' 
laFileStr[1,2] = "C"
laFileStr[1,3] = 6
laFileStr[1,4] = 0


laFileStr[2,1] = 'cDept' 
laFileStr[2,2] = "C"
laFileStr[2,3] = 4
laFileStr[2,4] = 0

laFileStr[3,1] = 'CWeek' 
laFileStr[3,2] = "C"
laFileStr[3,3] = 2
laFileStr[3,4] = 0


laFileStr[4,1] = 'CYear' 
laFileStr[4,2] = "C"
laFileStr[4,3] = 4
laFileStr[4,4] = 0

laFileStr[5,1] = 'CalOfNo' 
laFileStr[5,2] = "C"
laFileStr[5,3] = 6
laFileStr[5,4] = 0

laFileStr[6,1] = 'CallDate' 
laFileStr[6,2] = "D"
laFileStr[6,3] = 8
laFileStr[6,4] = 0

laFileStr[7,1] = 'SKU' 
laFileStr[7,2] = "N"
laFileStr[7,3] = 8
laFileStr[7,4] = 0

laFileStr[8,1] = 'Style' 
laFileStr[8,2] = "C"
laFileStr[8,3] = 19
laFileStr[8,4] = 0

laFileStr[9,1] = 'Colour' 
laFileStr[9,2] = "C"
laFileStr[9,3] = 20
laFileStr[9,4] = 0

laFileStr[10,1] = 'Size' 
laFileStr[10,2] = "C"
laFileStr[10,3] = 6
laFileStr[10,4] = 0

laFileStr[11,1] = 'Store' 
laFileStr[11,2] = "N"
laFileStr[11,3] = 4
laFileStr[11,4] = 0

laFileStr[12,1] = 'Advised' 
laFileStr[12,2] = "N"
laFileStr[12,3] = 6
laFileStr[12,4] = 0

laFileStr[13,1] = 'Received' 
laFileStr[13,2] = "N"
laFileStr[13,3] = 6
laFileStr[13,4] = 0

laFileStr[14,1] = 'CanDate' 
laFileStr[14,2] = "D"
laFileStr[14,3] = 8
laFileStr[14,4] = 0

laFileStr[15,1] = 'OrgQty' 
laFileStr[15,2] = "N"
laFileStr[15,3] = 6
laFileStr[15,4] = 0

laFileStr[16,1] = 'SntQty' 
laFileStr[16,2] = "N"
laFileStr[16,3] = 6
laFileStr[16,4] = 0

laFileStr[17,1] = 'PIKTKT' 
laFileStr[17,2] = "C"
laFileStr[17,3] = 6
laFileStr[17,4] = 0

laFileStr[18,1] = 'INVOICE' 
laFileStr[18,2] = "C"
laFileStr[18,3] = 6
laFileStr[18,4] = 0

laFileStr[19,1] = 'INVDATE' 
laFileStr[19,2] = "D"
laFileStr[19,3] = 8
laFileStr[19,4] = 0

laFileStr[20,1] = 'TruRef' 
laFileStr[20,2] = "C"
laFileStr[20,3] = 20
laFileStr[20,4] = 0

laFileStr[21,1] = 'Ndiff' 
laFileStr[21,2] = "N"
laFileStr[21,3] = 6
laFileStr[21,4] = 0

 = gfCrtTmp(lcDataFile ,@laFileStr,"CalOfNo+Style+Colour+STR(SKU,8)" ,lcDataFile)

*!*************************************************************
*! Name      : lfSrvCall 
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/03/2008
*! Purpose   : Set Call off num
*!*************************************************************
FUNCTION lfSrvCall 
PARAMETERS lcStRest
IF lcStRest = 'S'
  IF USED(lcCallTmp)
    SELECT(lcCallTmp)
    ZAP 
  ENDIF 

  IF !USED('ALDSCRP')
    = gfOpenFile(oAriaApplication.DataDir+'ALDSCRP','ALDYEAR', "SH",'ALDSCRP')
  ELSE
    SELECT ALDSCRP
    SET ORDER TO ALDYEAR
  ENDIF

  SELECT ALDSCRP
  =SEEK(STR(lnYear,4)+STR(lnWeekNo,2))
  SCAN REST WHILE  STR(NYEAR,4)+STR(NWEEK,2)+CFACT+STR(NSTORE,4)+STR(NCALLOFF,6) = STR(lnYear,4)+STR(lnWeekNo,2)
    SCATTER MEMO MEMVAR 
    IF !SEEK(STR(m.NYEAR,4)+STR(m.NWEEK,2)+m.CFACT+STR(m.NSTORE,4)+PADl(ALLTRIM(STR(m.nCallOFf,6)),6,'0') ,lcCallTmp)
      SELECT (lcCallTmp)
      APPEND BLANK
      m.CCALLOFF = PADl(ALLTRIM(STR(m.nCallOFf,6)),6,'0')
      GATHER MEMO MEMVAR 
    ENDIF 
  ENDSCAN 
ENDIF   
*! C200953,1 MMT 02/27/2008 Add Detailed Format to the report[End]