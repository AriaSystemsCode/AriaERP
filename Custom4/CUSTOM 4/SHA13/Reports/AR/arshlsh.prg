*:***************************************************************************
*: Program file  : ARSHLSH.PRG
*: Program desc. : Custom Shipping Log Report for SHA13 to Print the order complete date. 
*: Module        : Accounts Receivable (AR)
*: Developer     : Mariam Mazhar(MMT)
*: Tracking Job Number: C201640 [T20140808.0003]
*: Date          : 10/13/2014
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfCodDes,gfGetMemVar,gfBrows,gfStyBrw,CusBrowM,
*:                         gfItemMask,gfCrtTmp.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARSHLOG
*:***************************************************************************
*: Modification: 
*:**************************************************************************
*----------------------- Report Code Begin -----------------------------

#INCLUDE R:\Aria4xp\reports\ar\arshlog.H
PRIVATE lcExactCas

lcStTime   = TIME()    && Time in which we start collect data.
IF !USED("ORDHDR")
  =gfOpenTable("ORDHDR","ORDHDR",'SH')
ENDIF
*--Declare variable if coming from standard.
IF TYPE("llTrigFG")="U"
  llTrigFG=.F.
ENDIF
*--Use variable llOGFltCh that detect OG filter changes.
IF llClearFn OR loOGScroll.llOGFltCh
  llClearFn  = .F.
  lcLastTag  = ''     && to fill index field with its corresponding data.
  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.
  *--if you have previous data clear workfile then recreate it.
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF
    =lfWorkFile()
  ENDIF

  SELECT INVLINE
  lcNewExp=""
  IF LEN(loOGScroll.lcRpExp) > 1
    lcExp = loOGScroll.lcRpExp
    lnOccur = OCCURS(' AND',lcExp)
    IF lnOccur > 0
      FOR lnCount = 1 TO lnOccur + 1
        lnStart = IIF(lnCount = 1 , 1 , ATC(' AND',lcExp,lnCount-1) + 5)
        lnEnd = IIF(lnCount = lnOccur + 1,LEN(lcExp),ATC('AND',lcExp,lnCount))
        lnLength = lnEnd - lnStart +IIF(lnCount = lnOccur + 1,1,0)
        lcTake = SUBSTR(lcExp,lnStart,lnLength)
        lnoccurs=ATC('INLIST',lcTake)
        lnSeaOcurr=ATC('SEASON',lcTake)
        lnDivOcurr=ATC('CDIVISION',lcTake)
        lnGrpOcurr=ATC('CSTYGROUP',lcTake)
        lnLocOcurr=ATC('CWARECODE',lcTake)
        lnCurOcurr=ATC('CCURRCODE',lcTake)
        lnStatOcurr=ATC('CADDRESS4',lcTake)
        IF lnoccurs > 0
          IF  (lnSeaOcurr > 0 OR lnDivOcurr > 0 OR lnGrpOcurr > 0 OR lnLocOcurr > 0 OR lnCurOcurr > 0 OR lnStatOcurr>0 )
            lcTake = ""
          ELSE
            lcTake = SUBSTR(lcExp,lnStart,lnLength)
          ENDIF
        ENDIF
        IF !EMPTY(lcNewExp)
          IF !EMPTY(lcTake)
            lcNewExp = lcNewExp + ' .AND. '+ lcTake
          ENDIF
        ELSE
          lcNewExp = lcTake
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  lcRpExp = IIF(EMPTY(lcNewExp),loOGScroll.lcRpExp,lcNewExp)
  *--Create temp files for Division,Season,Location,style Group,currency [BEGIN]
   *--Make Temp File For Selected States
  lcstatCursor =""
  lnState = ASCAN(loOgScroll.laOgVrFlt,"CUSTOMER.CADDRESS4")
  lnState = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnState ,1)
  lcStates= loOgScroll.laOgVrFlt[lnState ,6]
  IF !EMPTY(lcStates)
    IF lnState > 0
      lcstatCursor  = loOgScroll.gfTempName() &&Cursor Hold Selected Divisions
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CADDRESS4'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcstatCursor ,@laTempacstru,"CADDRESS4",lcstatCursor ,.T.)
      IF !EMPTY(lcStates)
        lnStart=1
        lnEnd=AT('|',lcStates)
        DO WHILE lnEnd <> 0
          SELECT(lcstatCursor )
          APPEND BLANK
          REPLACE CADDRESS4 WITH SUBSTR(lcStates,lnStart,lnEnd-1)
          lcStates= STUFF(lcStates,lnStart,lnEnd,"")
          lnEnd=AT('|',lcStates)
        ENDDO
        IF lnEnd = 0
          SELECT(lcstatCursor )
          APPEND BLANK
          REPLACE CADDRESS4 WITH lcStates
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcstatCursor )
    SET ORDER TO TAG (lcstatCursor )
  ENDIF
  *--Make Temp File For Selected invoice Divisions
  lcDivCursor =""
  lnInvDivision = ASCAN(loOgScroll.laOgFxFlt,"INVHDR.CDIVISION")
  lnInvDivision = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnInvDivision,1)
  lcDivisions= loOgScroll.laOgFxFlt[lnInvDivision,6]
  IF !EMPTY(lcDivisions)
    IF lnInvDivision > 0
      lcDivCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Divisions
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CDIVISION'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
      IF !EMPTY(lcDivisions)
        lnStart=1
        lnEnd=AT('|',lcDivisions)
        DO WHILE lnEnd <> 0
          SELECT(lcDivCursor)
          APPEND BLANK
          REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
          lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcDivisions)
        ENDDO
        IF lnEnd = 0
          SELECT(lcDivCursor)
          APPEND BLANK
          REPLACE CDIVISION WITH lcDivisions
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcDivCursor)
    SET ORDER TO TAG (lcDivCursor)
  ENDIF

  *--Make Temp File For Selected Invoice Seasons
  lcSeaCursor =""
  lnInvSeason = ASCAN(loOgScroll.laOgFxFlt,"INVLINE.SEASON")
  lnInvSeason = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnInvSeason,1)
  lcSeasons= loOgScroll.laOgFxFlt[lnInvSeason,6]
  IF !EMPTY(lcSeasons)
    IF lnInvSeason > 0
      lcSeaCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='SEASON'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcSeaCursor,@laTempacstru,"SEASON",lcSeaCursor,.T.)
      IF !EMPTY(lcSeasons)
        lnStart=1
        lnEnd=AT('|',lcSeasons)
        DO WHILE lnEnd <> 0
          SELECT(lcSeaCursor)
          APPEND BLANK
          REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
          lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcSeasons)
        ENDDO
        IF lnEnd = 0
          SELECT(lcSeaCursor)
          APPEND BLANK
          REPLACE SEASON WITH lcSeasons
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF !EMPTY(lcSeaCursor)
    SET ORDER TO TAG (lcSeaCursor)
  ENDIF

  *--Make Temp File For Selected Invoice Seasons
  lcHdrSeaCursor =""
  lnInvHdrSeason = ASCAN(loOgScroll.laOgFxFlt,"INVHDR.SEASON")
  lnInvHdrSeason = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnInvHdrSeason,1)

  lcHdrSeasons= loOgScroll.laOgFxFlt[lnInvHdrSeason,6]
  IF !EMPTY(lcHdrSeasons)
    IF lnInvHdrSeason > 0
      lcHdrSeaCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='SEASON'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcHdrSeaCursor,@laTempacstru,"SEASON",lcHdrSeaCursor,.T.)
      IF !EMPTY(lcHdrSeasons)
        lnStart=1
        lnEnd=AT('|',lcHdrSeasons)
        DO WHILE lnEnd <> 0
          SELECT(lcHdrSeaCursor )
          APPEND BLANK
          REPLACE SEASON WITH SUBSTR(lcHdrSeasons,lnStart,lnEnd-1)
          lcHdrSeasons= STUFF(lcHdrSeasons,lnStart,lnEnd,"")
          lnEnd=AT('|',lcHdrSeasons)
        ENDDO
        IF lnEnd = 0
          SELECT(lcHdrSeaCursor )
          APPEND BLANK
          REPLACE SEASON WITH lcHdrSeasons
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF !EMPTY(lcHdrSeaCursor )
    SET ORDER TO TAG (lcHdrSeaCursor )
  ENDIF

  *--Make Temp File For Selected Invoice Locations
  lcLocCursor =""
  lnInvLocation = ASCAN(loOgScroll.laOgFxFlt,"INVHDR.CWARECODE")
  lnInvLocation = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnInvLocation,1)
  lcLocs= loOgScroll.laOgFxFlt[lnInvLocation,6]
  IF !EMPTY(lcLocs)
    IF lnInvLocation > 0
      lcLocCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CWARECODE'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcLocCursor,@laTempacstru,"CWARECODE",lcLocCursor,.T.)
      IF !EMPTY(lcLocs)
        lnStart=1
        lnEnd=AT('|',lcLocs)
        DO WHILE lnEnd <> 0
          SELECT(lcLocCursor)
          APPEND BLANK
          REPLACE CWARECODE WITH SUBSTR(lcLocs,lnStart,lnEnd-1)
          lcLocs = STUFF(lcLocs ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcLocs)
        ENDDO
        IF lnEnd = 0
          SELECT(lcLocCursor)
          APPEND BLANK
          REPLACE CWARECODE WITH lcLocs
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF !EMPTY(lcLocCursor)
    SET ORDER TO TAG (lcLocCursor)
  ENDIF

  *--Make Temp File For Selected Invoice Style Group Codes
  lcGrpCursor =""
  lnInvGrp = ASCAN(loOgScroll.laOgFxFlt,"STYLE.CSTYGROUP")
  lnInvGrp = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnInvGrp,1)
  lcStyGrps= loOgScroll.laOgFxFlt[lnInvGrp,6]
  IF !EMPTY(lcStyGrps)
    IF lnInvGrp > 0
      lcGrpCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Style Groups
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CSTYGROUP'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcGrpCursor,@laTempacstru,"CSTYGROUP",lcGrpCursor,.T.)
      IF !EMPTY(lcStyGrps)
        lnStart=1
        lnEnd=AT('|',lcStyGrps)
        DO WHILE lnEnd <> 0
          SELECT(lcGrpCursor)
          APPEND BLANK
          REPLACE CSTYGROUP WITH SUBSTR(lcStyGrps,lnStart,lnEnd-1)
          lcStyGrps = STUFF(lcStyGrps ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcStyGrps)
        ENDDO
        IF lnEnd = 0
          SELECT(lcGrpCursor)
          APPEND BLANK
          REPLACE CSTYGROUP WITH lcStyGrps
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF !EMPTY(lcGrpCursor)
    SET ORDER TO TAG (lcGrpCursor)
  ENDIF

  *--Make Temp File For Selected Invoice Currencies
  lcCurCursor =""
  lnInvCurr = ASCAN(loOgScroll.laOgFxFlt,"INVHDR.CCURRCODE")
  lnInvCurr = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnInvCurr,1)
  lcCurrs= loOgScroll.laOgFxFlt[lnInvCurr,6]
  IF !EMPTY(lcCurrs)
    IF lnInvCurr > 0
      lcCurCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Currenciess
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CCURRCODE'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcCurCursor,@laTempacstru,"CCURRCODE",lcCurCursor,.T.)
      IF !EMPTY(lcCurrs)
        lnStart=1
        lnEnd=AT('|',lcCurrs)
        DO WHILE lnEnd <> 0
          SELECT(lcCurCursor)
          APPEND BLANK
          REPLACE CCURRCODE WITH SUBSTR(lcCurrs,lnStart,lnEnd-1)
          lcCurrs = STUFF(lcCurrs ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcCurrs)
        ENDDO
        IF lnEnd = 0
          SELECT(lcCurCursor)
          APPEND BLANK
          REPLACE CCURRCODE WITH lcCurrs
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcCurCursor)
    SET ORDER TO TAG (lcCurCursor)
  ENDIF
  *--Create temp files for Division,Season,Location,style Group,currency [END]
  *--Allow void invoice printing
  IF llTrigFG
    lcRpExp    = STRTRAN(lcRpExp,'INVHDR.STATUS <> "V" AND','')
  ENDIF
  lcRpExp    = STRTRAN(lcRpExp,'INVLINE.','')
  lcRpExp    = [invoice+STR(lineno,6) = '' AND ] + lcRpExp

  SELECT InvLine
  SET ORDER TO InvLine
  SET RELATION TO Invoice INTO InvHdr, Style INTO Style,;
                  IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer
  *-- Scan to fill Temp. File with filter data.

  SCAN FOR &lcRpExp
    IF !EMPTY(lcstatCursor )
      IF !SEEK(substr(CUSTOMER.CADDRESS4,1,6),lcstatCursor )
        LOOP
      ENDIF
    ENDIF
    IF !EMPTY(lcDivCursor)
      IF !SEEK(INVHDR.CDIVISION,lcDivCursor)
        LOOP
      ENDIF
    ENDIF

    IF !EMPTY(lcSeaCursor)
      IF !SEEK(Style.SEASON,lcSeaCursor)
        LOOP
      ENDIF
    ENDIF


    IF !EMPTY(lcHdrSeaCursor)
      IF !SEEK(INVHDR.SEASON,lcHdrSeaCursor)
        LOOP
      ENDIF
    ENDIF


    IF !EMPTY(lcLocCursor)
      IF !SEEK(INVHDR.CWARECODE,lcLocCursor)
        LOOP
      ENDIF
    ENDIF
    IF !EMPTY(lcGrpCursor)
      IF !SEEK(STYLE.CSTYGROUP,lcGrpCursor)
        LOOP
      ENDIF
    ENDIF
    IF !EMPTY(lcCurCursor)
      IF !SEEK(INVHDR.CCURRCODE,lcCurCursor)
        LOOP
      ENDIF
    ENDIF
    SCATTER MEMVAR MEMO
    IF InvHdr.STATUS="V"
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,;
                 m.Qty8,m.TotQtY,m.price
    ENDIF

    m.custpo    = INVHDR.custpo
  	m.StyDesc   = Style.Desc1
  	m.BtName    = Customer.BtName
  	m.cAddress4 = Customer.cAddress4
  	m.Desc      = Style.Desc
  	m.cStyGroup = STYLE.cStyGroup
    m.Complete = IIF(gfSeek("O"+INVLINE.ORDER,'ORDHDR','ORDHDR'),ORDHDR.COMPLETE,{})
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDSCAN  && end Scan to fill Temp. File with filter data.
  SELECT InvLine
  SET ORDER TO
  SET RELATION TO
ENDIF

SELECT (lcWorkFile)
IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
SELECT (lcWorkFile)
SET RELATION TO Invoice INTO InvHdr, Style INTO Style,;
                IIF(EMPTY(Store),'M','S')+Account+Store INTO Customer
*-- ReIndex work file if first time collect data or user change sort By.
IF !(lcRpIndTag == lcLastTag)
  lcLastTag = lcRpIndTag
  REPLACE ALL cTempKey WITH EVALUATE(lcRpIndTag)
ENDIF
GO TOP
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.

WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_SelectMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_SelectMsg",AHEADERFILE))+' '+;
            ALLTRIM(STR(RECCOUNT(lcWorkFile))) +' '+;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_RecInMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_RecInMsg",AHEADERFILE))+' ' +;
            ALLTRIM(STR(lnInterval,6,2)) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_SecondMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_SecondMsg",AHEADERFILE)) NOWAIT

DO gfDispRe WITH EVAL('lcRpForm')
RETURN
*----------------------- Report Code End -----------------------------
*-- Function Section
*-------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : 1- Get state title.
*!           : 2- Know in which country we are.
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : Country state title.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStitle

lcWorkArea = SELECT()
PRIVATE lcSelectCommand,lnResult,lcSelectCommand1,lnResult1
lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult >= 1
  IF !USED('SYCINT')
    =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')
  ENDIF
   lcSelectCommand1=[SELECT CCONT_CODE,CPART4LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=SYCCOMP.CCONT_CODE]
   lnResult1 = oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult1 >= 1
    llCanada  = 'CAN' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
	llEngland = 'ENG' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
	SELECT (lcWorkArea)
	RETURN (SYCINT.CPART4LAB)
  ENDIF
ENDIF
SELECT (lcWorkArea)

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Option Grid When function
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
IF TYPE('lcLastTag') = 'N'
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.
  SET ORDER TO INVHDR IN INVHDR
  SET ORDER TO INVLINE IN INVLINE
  SET ORDER TO STYLE IN STYLE

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    lnCurrPos  = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.CCURRCODE'),1)
    loOGScroll.laOGFxFlt[lnCurrPos,6] = oAriaApplication.BaseCurrency
    = LFOGSHOWGET('LAOGFXFLT[' + ALLTRIM(STR(LNCURRPOS)) + ',6]')  && SHOW GET OBJECT .
  ENDIF

  IF llMultLoc
    SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  ENDIF

  lnSelectedAlias=SELECT()

  DIMENSION laTempStru[1,18]
  laTempStru = ''
  SELECT INVLINE
  =AFIELD(laTempStru)
  lnTempStru=ALEN(laTempStru,1)

  DIMENSION laTempStru[lnTempStru+9, 18]

  *-- cTempKey :  field used in all sort by cases as the master key .

  laTempStru[lnTempStru+1,1] = 'cTempKey'
  laTempStru[lnTempStru+1,2] = 'C'
  laTempStru[lnTempStru+1,3] = 55
  laTempStru[lnTempStru+1,4] = 0

  laTempStru[lnTempStru+2,1] = 'cState'
  laTempStru[lnTempStru+2,2] = 'C'
  laTempStru[lnTempStru+2,3] = 30
  laTempStru[lnTempStru+2,4] = 0

  LATEMPSTRU[LNTEMPSTRU+3, 1] = 'custpo'
  LATEMPSTRU[LNTEMPSTRU+3, 2] = 'C'
  LATEMPSTRU[LNTEMPSTRU+3, 3] = 15
  LATEMPSTRU[LNTEMPSTRU+3, 4] = 0

  LATEMPSTRU[LNTEMPSTRU+4, 1] = 'StyDesc'
  LATEMPSTRU[LNTEMPSTRU+4, 2] = 'C'
  LATEMPSTRU[LNTEMPSTRU+4, 3] = 60
  LATEMPSTRU[LNTEMPSTRU+4, 4] = 0

  LATEMPSTRU[LNTEMPSTRU+5, 1] = 'BtName'
  LATEMPSTRU[LNTEMPSTRU+5, 2] = 'C'
  LATEMPSTRU[LNTEMPSTRU+5, 3] = 30
  LATEMPSTRU[LNTEMPSTRU+5, 4] = 0

  LATEMPSTRU[LNTEMPSTRU+6, 1] = 'cAddress4'
  LATEMPSTRU[LNTEMPSTRU+6, 2] = 'C'
  LATEMPSTRU[LNTEMPSTRU+6, 3] = 30
  LATEMPSTRU[LNTEMPSTRU+6, 4] = 0

  LATEMPSTRU[LNTEMPSTRU+7, 1] = 'Desc'
  LATEMPSTRU[LNTEMPSTRU+7, 2] = 'C'
  LATEMPSTRU[LNTEMPSTRU+7, 3] = 20
  LATEMPSTRU[LNTEMPSTRU+7, 4] = 0

  LATEMPSTRU[LNTEMPSTRU+8, 1] = 'cStyGroup'
  LATEMPSTRU[LNTEMPSTRU+8, 2] = 'C'
  LATEMPSTRU[LNTEMPSTRU+8, 3] = 6
  LATEMPSTRU[LNTEMPSTRU+8, 4] = 0

  LATEMPSTRU[LNTEMPSTRU+9, 1] = 'Complete'
  LATEMPSTRU[LNTEMPSTRU+9, 2] = 'D'
  LATEMPSTRU[LNTEMPSTRU+9, 3] = 8
  LATEMPSTRU[LNTEMPSTRU+9, 4] = 0

  FOR  lnInc=7 TO 18
    STORE SPACE(1) TO laTempStru[lnTempStru+1,lnInc], laTempStru[lnTempStru+2,lnInc]

    STORE SPACE(1) TO LATEMPSTRU[LNTEMPSTRU+3, LNINC],;
          LATEMPSTRU[LNTEMPSTRU+4, LNINC], LATEMPSTRU[LNTEMPSTRU+5, LNINC], LATEMPSTRU[LNTEMPSTRU+6, LNINC],;
          LATEMPSTRU[LNTEMPSTRU+7, LNINC], LATEMPSTRU[LNTEMPSTRU+8, LNINC], LATEMPSTRU[LNTEMPSTRU+9, LNINC]
  ENDFOR
  =lfWorkFile()
  SELECT(lnSelectedAlias)
ENDIF  && END IF you first time enter when function.

*!*************************************************************
*! Name      : lfWorkFile
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Create work File.
*!*************************************************************
*! Parameters  : File name,File structure,Tag expression,Tag name
*!               ,Logic cursor = .T.(create cursor)
*!                             = .F.(create file)
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfWorkFile
gfCrtTmp(lcWorkFile,@laTempStru,"cTempKey",lcWorkFile,.F.)

*!*************************************************************
*! Name      : lfwOldVal
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

*!*************************************************************
*! Name      : lfvAcc
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Validate function for the Customer Account field
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfvAcc

PRIVATE lcItsName , lcItsVal , llObjRet
lcItsName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvLoc
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Validate location Code field
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvLoc
PRIVATE lcObjName , lcObjVal

lcObjName = OGSYS18()               && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the location he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvStates
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Validate the state code
*!*************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvStates
PRIVATE lcStateObj , lcStateVal

lcStateObj = OGSYS18()                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = EVALUATE(OGSYS18())  && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the state code he entered is not in the file.
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR(ALLTRIM(lcStateVal),6)+'N'+'STATE','CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = ALLTRIM(lcSTitle) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_StCodes,oAriaApplication.GetHeaderText("LANG_Arshlog_StCodes",AHEADERFILE))
   lcBrfields = [cCode_No :H=  ALLTRIM(lcSTitle) + ']+;
                IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_StCode,oAriaApplication.GetHeaderText("LANG_Arshlog_StCode",AHEADERFILE))+;
                [',cDiscrep :H= ']+;
                IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Description,oAriaApplication.GetHeaderText("LANG_Arshlog_Description",AHEADERFILE))+[' :30]
   IF gfBrows('FOR cdefcode+cfld_name+ccode_no+cdiscrep = ;
     "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')

     lcStateVal = laCodeFld[1]
   ENDIF
   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF
*-- If user type inValid code does not have ? Accept it.
lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
&lcStateObj = lcStateVal

*!*************************************************************
*! Name      : lfCollTime
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns   : Spent time.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

*!*************************************************************
*! Name      : lfClearRep
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.    && If you run filter you must create cursor again.
*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF

*!*************************************************************
*! Name      : lfEvalVars
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Fill Default values used in both OG and Report.
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return    : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfEvalVars

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lcStyGrp    = lcStyMajor + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Group,oAriaApplication.GetHeaderText("LANG_Arshlog_Group",AHEADERFILE))

*-- Evaluate sort by arrays. [Begin]
DIMENSION laSortDesc[4,1],laSortVal[4,1]
laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Account,oAriaApplication.GetHeaderText("LANG_Arshlog_Account",AHEADERFILE))

laSortDesc[2] = lcStyMajor
laSortDesc[3] = lcStyGrp
laSortDesc[4] = lcSTitle       && State variable Title

laSortVal[1] = 'A'
laSortVal[2] = 'S'
laSortVal[3] = 'G'
laSortVal[4] = 'T'
*-- Evaluate sort by arrays. [End]

*-- if multi currency evaluate currency arrays [Begin]
IF llMultCurr
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*-- if multi currency evaluate currency arrays [Begin]

*-- Evaluate non Segment values [begin]

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
    lcColorTlt = PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3]))
    lnColorLen = LEN(lcNonMajPi)
    EXIT

  ENDIF  && end If you Find segment of Color Type.

ENDFOR    && end Loop Around Non Major elements.
*-- Compute Free/Color Items in Style code Structure. [End]
*-- Evaluate non Segment values [end]

*-- Fill default sort options... [Begin]
lcRpSortBy = 'A'
llRpShowSz = .F.
=lfvSortBy()
*-- Fill default sort options... [End]

*!*************************************************************
*! Name      : lfvShowSz
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Valid function for show sizes .
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvShowSz

lcRpForm = IIF(llRpShowSz,'ARSHLSHS','ARSHLSHN')
= lfRepPltFr(lcRpForm)
=lfvSortBy(.T.)

*!*************************************************************
*! Name      : lfvSortBy
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Rise change index flag to reindex temp cursor.
*!*************************************************************
*! Parameters  : Show Details of sizes or not
*!                  llDetonly=.T. (show sizes break down)
*!                  llDetOnly=.F. (Don't show sizes break down)
*!*************************************************************
*! Returns   : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvSortBy
PARAMETERS llDetOnly
DO CASE
  CASE lcRpSortBy = 'S'		&& Sort by Style Case
    IF !llDetOnly
      lcRpHedTlt = ALLTRIM(lcStyMajor)
    ENDIF
    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)
      IF !llDetOnly
        lcRpIndTag = [STYLE + ACCOUNT + INVOICE]
        lcRpGroup1   = [STYLE]
        lcRpGrpHd1   = [lcStyTitle + ' : ' + Style + ' - ' + StyDesc]
        lcRpGrpFt1   = ['*   ' + Style + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
        STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
      ENDIF
      IF llRpShowSz
        lcRpRepHed  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))   + SPACE(1) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))        + SPACE(4)  + PADR(ALLTRIM(lcSTitle),6)
        lcRpDetLin  = [Account + SPACE(1) +  PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(cAddress4),6)]
      ELSE
        lcRpRepHed =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))  + ;
                      SPACE(1) +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Name,oAriaApplication.GetHeaderText("LANG_Arshlog_Name",AHEADERFILE))+;
                      SPACE(27)                  +;
                      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))       +;
                      SPACE(4) + PADR(ALLTRIM(lcSTitle),16)
         lcRpDetLin  = [Account + SPACE(1) + PADR(btname,30) + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(cAddress4),16)]
       ENDIF
    ELSE  && else style non major has color segment.
      IF !llDetOnly
        lcRpIndTag = [SUBSTR(STYLE,1,LEN(lcMajorPic)) + SUBSTR(STYLE,lnNonMajSt,lnColorLen) +;
                      ACCOUNT + INVOICE]
        lcRpGroup1   = [SUBSTR(STYLE,1,LEN(lcMajorPic))]
        lcRpGroup2   = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
        *for report adjustment[start]
        lcRpGrpHd1   = [PADR(lcStyMajor,19) + ': ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)] &&AYM
        lcRpGrpHd2   = [PADR(lcColorTlt,19) + ': ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                      ' - ' + StyDesc]
        * for report adjustment.[end]
        lcRpGrpFt1   = ['*   ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
		lcRpGrpFt2   = ['**  ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
        STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
      ENDIF
      IF llRpShowSz
        lcRpRepHed  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+;
                      ' '      +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+;
                      SPACE(4)   +PADR(ALLTRIM(lcSTitle),6)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account  + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(cAddress4),6) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ELSE
        lcRpRepHed  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+' '+;
                      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Name,oAriaApplication.GetHeaderText("LANG_Arshlog_Name",AHEADERFILE))+SPACE(27)                     +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+SPACE(4)   +PADR(ALLTRIM(lcSTitle),9)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account + ' ' + PADR(btname,30) + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(cAddress4),9) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ENDIF
    ENDIF  && end if style non major does not has color segment.
  CASE lcRpSortBy = 'G'		&& Sort by Style Group Case
    IF !llDetOnly
      lcRpHedTlt = ALLTRIM(lcStyMajor) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Group,oAriaApplication.GetHeaderText("LANG_Arshlog_Group",AHEADERFILE))
      lcRpGrpHd1   = [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Group,oAriaApplication.GetHeaderText("LANG_Arshlog_Group",AHEADERFILE))+['+'            ' + ' : ' + PADR(LEFT(cTempKey,6),19) +;
                       ' - ' + gfCodDes(LEFT(cTempKey,6),"CSTYGROUP")]
      lcRpGrpFt1   = ['*'+space(2) + PADR(LEFT(cTempKey,6),19) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
    ENDIF
    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)
      IF !llDetOnly
        lcRpIndTag = [cStyGroup + STYLE + ACCOUNT + INVOICE]
        lcRpGroup1   = [LEFT(cTempKey,6)]
        lcRpGroup2   = [STYLE]
        lcRpGrpHd2   = [PADR(lcStyTitle,19) + ': ' + PADR(Style,19) + ' - ' + StyDesc]&&AYM
        lcRpGrpFt2   = ['**  ' + PADR(Style,19) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
        STORE '' TO lcRpGroup3,lcRpGrpHd3,lcRpGrpFt3
      ENDIF
      IF llRpShowSz
        lcRpRepHed =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))   + SPACE(1) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))       + SPACE(4) + PADR(ALLTRIM(lcSTitle),6)
        lcRpDetLin  = [Account + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(cAddress4),6)]
      ELSE
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))   + SPACE(1) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Name,oAriaApplication.GetHeaderText("LANG_Arshlog_Name",AHEADERFILE)) + SPACE(27)                  + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))       + SPACE(4) + PADR(ALLTRIM(lcSTitle),16)
        lcRpDetLin  = [Account + SPACE(1) + PADR(btname,30) + SPACE(1) + PADR(Store,8) + SPACE(1) + PADR(ALLTRIM(cAddress4),16)]
      ENDIF
    ELSE  && else style non major has color segment.
      IF !llDetOnly
        lcRpIndTag = [cStyGroup + SUBSTR(STYLE,1,LEN(lcMajorPic)) +;
                                        SUBSTR(STYLE,lnNonMajSt,lnColorLen) +;
                                        ACCOUNT + INVOICE]
        lcRpGroup1   = [LEFT(cTempKey,6)]
        lcRpGroup2   = [SUBSTR(STYLE,1,LEN(lcMajorPic))]
        lcRpGroup3   = [SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
        * for adjustment report[start]
        lcRpGrpHd2   = [PADR(lcStyMajor,19) + ': ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19)]&&AYM
        lcRpGrpHd3   = [PADR(lcColorTlt,19) + ': ' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19) +;
                          ' - ' + StyDesc]
        lcRpGrpFt2   = ['**  ' + PADR(SUBSTR(STYLE,1,LEN(lcMajorPic)),19) +']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE)) +[']
        lcRpGrpFt3   = ['***' + PADR(SUBSTR(STYLE,lnNonMajSt,lnColorLen),19)+'] + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
 
      ENDIF
      IF llRpShowSz
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+' '     +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+SPACE(4)   +PADR(ALLTRIM(lcSTitle),6)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(cAddress4),6) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
      ELSE
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Name,oAriaApplication.GetHeaderText("LANG_Arshlog_Name",AHEADERFILE))+SPACE(27)                     +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+SPACE(4)   +PADR(ALLTRIM(lcSTitle),9)+SPACE(1)        +PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  = [Account + ' ' + PADR(btname,30) + ' ' + PADR(Store,8) +' '+PADR(ALLTRIM(cAddress4),9) + ' ' +SUBSTR(STYLE,lnNonMajSt,6)]
       ENDIF
    ENDIF  && end if style non major does not has color segment.

  CASE lcRpSortBy = 'A'		&& Sort by Account Case
    IF !llDetOnly
      lcRpIndTag = [ACCOUNT + STORE + INVOICE]
      lcRpGroup1   = [ACCOUNT]
      lcRpGrpHd1   = [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Account,oAriaApplication.GetHeaderText("LANG_Arshlog_Account",AHEADERFILE))+[' + ' : ' + Account + ' - ' + BtName]
      lcRpGrpFt1   = ['*   ' + Account + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3
    ENDIF

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)

      IF llRpShowSz
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)
        lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(cAddress4),6) +' '+ Style]
      ELSE
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyTitle,19)+' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Description,oAriaApplication.GetHeaderText("LANG_Arshlog_Description",AHEADERFILE))
        *Use Style Long Description Where Possible
        *Print the style description from the INVLINE file instead of the STYLE file. [start]
        lcRpDetLin  = [Store+ ' '   +PADR(ALLTRIM(cAddress4),6) +' '+ Style + ' ' + &lcWorkFile..Desc1]
        *Print the style description from the INVLINE file instead of the STYLE file. [end]
      ENDIF

    ELSE
      IF llRpShowSz

        * Adjust Variable lcRpRepHed & Take Space From Addreess4 To Give Space To Color To Print. [ Begin ]
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,10)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(cAddress4),4) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
        * Adjust Variable lcRpRepHed & Take Space From Addreess4 To Give Space To Color To Print. [ End ]

      ELSE
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+PADR(ALLTRIM(lcSTitle),6)           +' '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Description,oAriaApplication.GetHeaderText("LANG_Arshlog_Description",AHEADERFILE))
        * Use Style Long Description Where Possible
        * Print the style description from the INVLINE file instead of the STYLE file. [start]
        lcRpDetLin  =[Store+ ' '   +PADR(ALLTRIM(cAddress4),6) +' '+ PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + &lcWorkFile..Desc1]
        * Print the style description from the INVLINE file instead of the STYLE file. [end]
      ENDIF

    ENDIF

  CASE lcRpSortBy = 'T'		&& Sort by State Case

    IF !llDetOnly
      lcRpHedTlt = ALLTRIM(lcSTitle)
      lcRpIndTag = [cAddress4 + ACCOUNT + INVOICE]
      lcRpGroup1   = [LEFT(cTempKey,30)]
      lcRpGrpHd1   = [lcSTitle + ' : ' + ALLTRIM(cAddress4)]
      lcRpGrpFt1   = ['*   ' + PADR(ALLTRIM(cAddress4),20) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_TotalMsg,oAriaApplication.GetHeaderText("LANG_Arshlog_TotalMsg",AHEADERFILE))+[']
      STORE '' TO lcRpGroup2,lcRpGroup3,lcRpGrpHd2,lcRpGrpHd3,lcRpGrpFt2,lcRpGrpFt3

    ENDIF

    *-- if style non major does not has color segment.
    IF EMPTY(lcFree_Clr)

      IF llRpShowSz
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+' '        +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+ PADR(lcStyTitle,19)
        lcRpDetLin  = [Account + ' ' + Store+ ' '   +  Style]
      ELSE
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+' '        +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Name,oAriaApplication.GetHeaderText("LANG_Arshlog_Name",AHEADERFILE))+ SPACE(11)                 + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+ PADR(lcStyTitle,19)+' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Description,oAriaApplication.GetHeaderText("LANG_Arshlog_Description",AHEADERFILE))
        lcRpDetLin  = [Account + ' ' + PADR(BtName,14) +  ' '  + Store+ ' '   +  Style + ' ' + PADR(Desc,12)]
      ENDIF

    ELSE
      IF llRpShowSz
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+' '        +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6)
        lcRpDetLin  =[Account + ' '  + Store +' '   + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen)]
      ELSE
        lcRpRepHed = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Acct,oAriaApplication.GetHeaderText("LANG_Arshlog_Acct",AHEADERFILE))+' '        +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Name,oAriaApplication.GetHeaderText("LANG_Arshlog_Name",AHEADERFILE))+ SPACE(11)                 +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Store,oAriaApplication.GetHeaderText("LANG_Arshlog_Store",AHEADERFILE))+'    '+ PADR(lcStyMajor,12)                           + ' ' + PADR(ALLTRIM(lcColorTlt),6) + ' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arshlog_Description,oAriaApplication.GetHeaderText("LANG_Arshlog_Description",AHEADERFILE))
        lcRpDetLin  =[Account + ' ' + PADR(BtName,14) +  ' '   + Store +' '   + PADR(ALLTRIM(PADR(Style,LEN(lcMajorPic))),12) + ' ' + SUBSTR(STYLE,lnNonMajSt,lnColorLen) + ' ' + PADR(Desc,12)]
      ENDIF
    ENDIF
ENDCASE

*!*************************************************************
*! Name      : lfvOGStyle
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Parameters : ....
*!*************************************************************
*! Return    : ....
*!*************************************************************
*!Modification : ....
*!*************************************************************
FUNCTION lfvOGStyle
PRIVATE lnCurSelct,lcStyOrder

lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*!*************************************************************
*! Name      : lfsrAcc
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.
*!*************************************************************
*! Name      : lfSRVSty
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    LOCATE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*: Developer : Mariam Mazhar(MMT)
*: Date      : 10/13/2014
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

LOCAL lnAlias
lnAlias = SELECT()

lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)

lnTotcomp = 0
SELECT Style_X
SET ORDER TO Style
IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF

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

SELECT(lnAlias)

RETURN INT(lnTotcomp)

*-- end of lfStySum.

