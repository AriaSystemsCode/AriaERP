*:************************************************************
*: Program file  : SOUNFRM.PRG
*: Program desc. : Custom screen for DIR03 for Uniform data entry
*: For screen    : SOUNFRM.SPR
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (SO).
*:     Developer : TMI - TAREK MOHAMMED IBRAHIM
*: Date          : 11/29/2004
*: Entry         : C # 123847,1
*:************************************************************
*-- laData     Array that hold the data of the basefile
*-- laScrMode  Array that hold the screen modes

EXTERNAL ARRAY laData, laKeyField, laDefProc 
DIMENSION laData[1],laScrMode[4],laKeyField[1,4],;
          laOldLink[3]

laKeyField[1,1] = 'UCODE'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'UNIFORM'
laKeyField[1,4] = 1

*- Global processes
laDefProc[3] = .F.                 && Use "lpNxtScr" to skip to next record
laDefProc[4] = .F.                 && Use "lpPrvScr" to skip to next record
laDefproc[7] = .F.                 && This is to Enable local delete
laDefproc[9] = .F.                 && This is to Enable local save

STORE 0 TO lnStyleWid,lnMrk,lnClrPos,lnClrLen,;
           lnLastMode, laOldLink
PRIVATE lcSvProc

*- Get style major lenght
lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)

lcIMjrPt  = gfItemMask('PI')
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)

*- Get color start and lenght
=lfGetClrD()

*- Browse variable
llBrowse = .F.

*- Entitlement types Combo 
DIMENSION laEntType[3]
lnEntType = 1
laEntType[1] = 'Units '
laEntType[2] = 'Value '
laEntType[3] = 'Points'

*- Initialize state variables for the screen
STORE '' TO lcKeyStat, lcObjStat,;
            lcWinCh0, lcWinCh1, lcWinCh2, lcBrWin, lcTmpUnfrm,lcUnfrm,;
            lcOldValue
lcBrTtl    = 'List Uniform Styles'

IF !gfSetup()
  RETURN
ENDIF  


IF !WEXIST(gcBaseWind)
  
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')  
  =gfOpenFile(gcDataDir+'UNIFORM','UNIFORMLN','SH')  
  =gfOpenFile(gcDataDir+'EMPLOYEE','UCODE','SH')  

  lcScFields = 'UCODE, UDESC, DSTART, DEND, NPERIOD, TYPE'
  
  lcWinCh0 = gfTempName()
  lcWinCh1 = gfTempName()
  lcWinCh2 = gfTempName()
  lcBrWin  = gfTempName()
  
  lcTmpUnfrm = gfTempName()
   
  SELECT UNIFORM
  SCATTER FIELDS &lcScFields TO laData BLANK
  lnEntType = 1

  *- Create temp file for uniform browse
  =lfCrTmpFl()
  
  *- Initialize screen modes array
  laScrMode    = .F.
  laScrMode[1] = .T.
  
  lcUnfrm  = gfTempName()
  lcBaseFile = lcUnfrm
  
  *- Fill temp base file with uniform codes ( distinct )
  =lfFilTmpBs()

ENDIF

SELECT &lcTmpUnfrm
SET RELATION TO PADR(CSTYMAJOR,lnStyleWid) INTO STYLE

PUSH KEY                                     && To save the the current on key label
ON KEY LABEL ALT+C ACTIVATE WINDOW (lcBrTtl)
ON KEY LABEL ALT+A DO lfvAdd 
DO (gcScrDir + gcWinAppl + '\SOUNFRM.SPR')   && calling the screen ICSKU
RELEASE WINDOW (lcBrTtl)                  && Release the browse window ( this is a must )
POP KEY                                      && To Restore the previous assignments for on key label

*-- If we realy quitting the screen 
IF glQuitting

  *- Quitting routine  
  IF USED(lcTmpunfrm)
    USE IN &lcTmpunfrm
  ENDIF
  ERASE (gcWorkDir+lcTmpunfrm+'.DBF')
  ERASE (gcWorkDir+lcTmpunfrm+'.CDX')

  IF USED(lcUnfrm)
    USE IN &lcUnfrm
  ENDIF
  ERASE (gcWorkDir+lcUnfrm+'.DBF')
  ERASE (gcWorkDir+lcUnfrm+'.CDX')
 
ENDIF  

*:**************************************************************************
*:* Name        : lfCrTmpFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Create temp file for uniform browse
*:***************************************************************************
FUNCTION lfCrTmpFl
PRIVATE lnSlct,laStru,lnPos,lnLen
lnSlct = SELECT()

SELECT UNIFORM
=AFIELDS(laStru)

*- Check this record is deleted
lnLen = ALEN(laStru,1)+1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'LDEL'
laStru[lnLen,2] = 'L'
laStru[lnLen,3] = 1
laStru[lnLen,4] = 0

*- Add a field for color description
lnLen = ALEN(laStru,1)+1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'CLRDESC'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 30
laStru[lnLen,4] = 0

*- Add a field for Style Desc.
lnLen = ALEN(laStru,1)+1
DIMENSION laStru[lnLen,4]
laStru[lnLen,1] = 'StyleDesc'
laStru[lnLen,2] = 'C'
laStru[lnLen,3] = 30
laStru[lnLen,4] = 0

CREATE TABLE (gcWorkDir+lcTmpUnfrm) FROM ARRAY laStru
INDEX ON NLINE TAG &lcTmpUnfrm

SELECT (lnSlct)
*-- end of lfCrTmpFl.

*:**************************************************************************
*:* Name        : lfFilTmpBs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Fill temp base file
*:***************************************************************************
FUNCTION lfFilTmpBs
PRIVATE laStru
DIMENSION laStru[1,4]

SELECT UNIFORM
=AFIELDS(laStru)
CREATE TABLE (gcWorkDir+lcUnfrm) FROM ARRAY laStru
INDEX ON UCODE TAG &lcUnfrm

SELECT UNIFORM
SCAN
  IF !SEEK(UNIFORM.UCODE,lcUnfrm)
    SELECT &lcUnfrm
    APPEND BLANK
    REPLACE UCODE WITH UNIFORM.UCODE ;
            UDESC WITH UNIFORM.UDESC ;
            TYPE  WITH UNIFORM.TYPE
  ENDIF
ENDSCAN

*:**************************************************************************
*:* Name        : lpShow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Handling the screen mode
*:***************************************************************************
PROCEDURE lpShow

PRIVATE lnSlct
lnSlct = SELECT(0)

*- Locate the pointer in the temp base file
=SEEK(laData[1],lcUnfrm)

STORE 'DISABLE' TO lcKeyStat, lcObjStat
SHOW GET pbAdd DISABLE
SHOW GET pbDel DISABLE
SHOW GET pbSv DISABLE
SHOW GET pbClr DISABLE
SHOW GET pbClo ENABLE

*- Disable standard browse button
SHOW GET pbBrws DISABLE

DO CASE
  *-- Select mode
  CASE laScrMode[1]
    
    lcKeyStat = 'ENABLE'

    SELECT UNIFORM
    SCATTER FIELDS &lcScFields TO laData BLANK
    lnEntType = 1

    *- Create temp file for uniform browse
    SELECT &lcTmpUnfrm
    ZAP

    *-Browse table
    =lfActBr()
     
    *-Save current mode as the last mode
    lnLastMode = 1

  *-- View mode
  CASE laScrMode[2]

    SHOW GET pbClr ENABLE

    SELECT UNIFORM
    =SEEK(laData[1])
    SCATTER FIELDS &lcScFields TO laData
    lnEntType = AT(TYPE,'UVP')
    _CUROBJ = OBJNUM(pbCls)
    
    *- refill the cursor if last mode was edit mode
    IF lnLastMode = 3
      =lfPopupTmp()
    ENDIF

    *-Save current mode as the last mode
    lnLastMode = 2

  *-- Edit mode
  CASE laScrMode[3]

    lcObjStat = 'ENABLE'
    SHOW GET pbAdd ENABLE
    SHOW GET pbDel ENABLE
    SHOW GET pbClr ENABLE
    SHOW GET pbSv ENABLE

    SELECT UNIFORM
    =SEEK(laData[1])
    SCATTER FIELDS &lcScFields TO laData
    lnEntType = AT(TYPE,'UVP')
    
    *-Save current mode as the last mode
    lnLastMode = 3

  *-- Add mode
  CASE laScrMode[4]

    lcObjStat = 'ENABLE'
    SHOW GET pbClr ENABLE
    SHOW GET pbAdd ENABLE
    SHOW GET pbSv ENABLE
    
    *-Save current mode as the last mode
    lnLastMode = 4

ENDCASE

*-Browse table
=lfActBr() .AND. lfwBrows()

=lfRefresh()

SELECT(lnSlct)

*:**************************************************************************
*:* Name        : lfvData_1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on UCODE field
*:***************************************************************************
FUNCTION lfvData_1
 
PRIVATE lnSlct,lcModalMes,lnResp
lnSlct = SELECT(0)

SELECT UNIFORM

*-               if user entered the chrachter ? / \ , the call browse
IF llBrowse .OR. CHRTRAN(laData[1],'?/\','') <> laData[1]
  
  *- Brow button is clicked  
  =lfUnfrmBr()

ELSE

  *- uniform code is keyed
  IF !EMPTY(laData[1])
    IF !SEEK(laData[1],'UNIFORM'))
      *- Msg : 00001  = >   ð is not found in the data file.
      *- Btn : 42003  = >  \<Add;\<Browse;\?\<Reenter
      lcModalMes = 'Uniform Code : ' + ALLTRIM(laData[1])+' '
      lnResp = gfModalGen('QRM00001B42003','DIALOG',lcModalMes)

      DO CASE
      CASE lnResp = 1  && Add
        laData[3] = gdSysDate
        laData[5] = 12
        
        laScrMode    = .F.
        laScrMode[4] = .T.
        SHOW GETS
  
      CASE lnResp = 2  && Browse
        =lfUnfrmBr()
  
      CASE lnResp = 3  && Reenter
        laData[1] = SPACE(12)
        _CUROBJ = OBJNUM(laData[1])
  
      ENDCASE
    
    ELSE
    
      =lfPopupTmp()
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
      
    ENDIF
  ENDIF
  
ENDIF

SELECT (lnSlct)

*:**************************************************************************
*:* Name        : lfUnfrmBr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Browse uniform codes
*:***************************************************************************
FUNCTION lfUnfrmBr
PRIVATE laTemp,lcFieldsNam,lcBrFields

llBrowse = .F.

SELECT &lcUnfrm
GO TOP

DIMENSION laTemp[2]
latemp = ''
lcFieldsNam = "UCODE,UDESC"
lcBrFields = "UCODE :H='Uniform Code',"+;
             "UDESC :H='Description',"+;
             "TYPE  :H='Type'"
PUSH KEY
ON KEY
PRIVATE lcFile_ttl,lcWinTitl
=Ariabrow('',' Uniform Code File Browse',gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
             lcFieldsNam,'laTemp')
POP KEY

IF !EMPTY(laTemp[1])
  IF SEEK(laTemp[1],'UNIFORM')
    laData[1] = laTemp[1]
  
    =lfPopupTmp()
    
    *- Locate the pointer in the temp base file
    =SEEK(laData[1],lcUnfrm)
    
    SHOW GET laData[1]
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
ELSE  
  laData[1] = ' '  
  _CUROBJ = OBJNUM(laData[1])
ENDIF
*-- end of lfUnfrmBr.

*:**************************************************************************
*:* Name        : lfPopupTmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Popup the temp file lctmpunfrm with data from uniform file
*:***************************************************************************
FUNCTION lfPopupTmp

SELECT &lcTmpunfrm
ZAP

SELECT UNIFORM
IF SEEK(laData[1],'UNIFORM')
  SCAN REST WHILE UCODE+STR(NLINE,2) = laData[1]
    SCATTER MEMVAR
    INSERT INTO &lcTmpunfrm FROM MEMVAR
    DO lfBlnkFlds
  ENDSCAN
ENDIF

SELECT &lcTmpunfrm
GO TOP

SELECT UNIFORM
=SEEK(laData[1],'UNIFORM')
SCATTER FIELDS &lcScFields TO laData
lnEntType = AT(TYPE,'UVP')

*-- end of lfPopupTmp.

*:**************************************************************************
*:* Name        : lfBlnkFlds
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Blank fields NLINK1,NLINK2 if = 0
*:***************************************************************************
FUNCTION lfBlnkFlds

    SELECT &lcTmpunfrm
    IF NLINK1 = 0
      BLANK FIELD NLINK1
    ENDIF
    IF NLINK2 = 0
      BLANK FIELD NLINK2
    ENDIF
*/*     IF NLINK3 = 0
*/*       BLANK FIELD NLINK3
*/*     ENDIF

*-- end of lfBlnkFlds.

*:**************************************************************************
*:* Name        : lfvData_3
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on DATE CREATED field
*:***************************************************************************
FUNCTION lfvData_3
PRIVATE ldStart,lnPeriod,ldEnd
ldStart = laData[3]
lnPeriod= laData[5]
ldEnd   = laData[4]

=lfUpdDates(@ldStart,@lnPeriod,@ldEnd)

laData[3] = ldStart
laData[5] = lnPeriod
laData[4] = ldEnd   
SHOW GET laData[3]
SHOW GET laData[5]
SHOW GET laData[4]

=lfRefresh()  


*-- end of lfvData_3.

*:**************************************************************************
*:* Name        : lfvData_5
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on PERIOD field
*:***************************************************************************
FUNCTION lfvData_5
IF laData[5] < 0
  laData[5] = lcOldValue
ENDIF  

PRIVATE ldStart,lnPeriod,ldEnd
ldStart = laData[3]
lnPeriod= laData[5]
ldEnd   = laData[4]

=lfUpdDates(@ldStart,@lnPeriod,@ldEnd)

laData[3] = ldStart
laData[5] = lnPeriod
laData[4] = ldEnd   
SHOW GET laData[3]
SHOW GET laData[5]
SHOW GET laData[4]

*-- end of lfvData_5.

*:**************************************************************************
*:* Name        : lfvEntTyp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/05/2004
*:* Purpose     : Entitlement type valid function
*:***************************************************************************
FUNCTION lfvEntTyp

*- Refresh the browse with the changed type , provided that it is not assigned to any employee
IF lnEntType <> lcOldValue 
  IF !SEEK(laData[1],'EMPLOYEE')
    =lfActBr()
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not change type, Uniform code is already assigned to an employee')
    lnEntType = lcOldValue 
    SHOW GET lnEntType
  ENDIF
ENDIF

*-- end of lfvEntTyp.

*:**************************************************************************
*:* Name        : lfUpdDates
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on PERIOD field
*:***************************************************************************
FUNCTION lfUpdDates
PARAMETERS ldStart,lnPeriod,ldEnd
PRIVATE lnDay,lnMnth,lnYr

ldEnd = {}
IF lnPeriod > 0
  ldEnd = ldStart-1
  lnYr = YEAR(ldStart)
  lnMnth = MONTH(ldStart)
  FOR lnI = 0 TO lnPeriod-1
    ldEnd = ldEnd + lfMnthDays(lnMnth+lnI,lnYr)
    IF lnMnth+lnI = 12
      lnMnth =  - lnI
      lnYr = lnYr + 1
    ENDIF
  ENDFOR
ENDIF

*-- end of lfUpdDates.

*:**************************************************************************
*:* Name        : lfMnthDays
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/12/2004
*:* Purpose     : return month days count
*:***************************************************************************
FUNCTION lfMnthDays
PARAMETERS lnMnth,lnYr
PRIVATE lnDays

DO CASE
CASE INLIST(lnMnth,1,3,5,7,8,10,12)
  lnDays = 31
CASE INLIST(lnMnth,4,6,9,11)
  lnDays = 30
CASE lnMnth = 2
  lnDays = 29-IIF(MOD(lnYr,4)>0,1,0)
ENDCASE

RETURN lnDays
*-- end of lfMnthDays.

*:**************************************************************************
*:* Name        : lfActBr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Browse detail function
*:***************************************************************************
FUNCTION lfActBr
PRIVATE lcBrFlds ,lcBrTitle,lcKey,lnSlct,lcWhn,lcStyWhn

lnSlct = SELECT(0)

SELECT &lcTmpUnfrm
lnMrk = RECNO(lcTmpUnfrm)
lcStyWhn = IIF(laScrMode[3].OR.laScrMode[4],'.T.','.F.')
lcWhn = IIF(laScrMode[3].OR.laScrMode[4],'.T.','.F.') + ' .AND. !EMPTY(CSTYMAJOR)'
*/*lcBrFlds = "lcMrk=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
*/*             "NLINE      :R:H='Line No'       :6 :W=.F. ,"+;
*/*             "CSTYMAJOR    :H='Style'        :15 :P=REPL('!',lnStyleWid):V=lfvBrSty() :W=&lcStyWhn ,"+;
*/*             "Style.Desc :R:H='Desc'         :20 :W=.F. ,"+;
*/*             "COLOUR       :H='CLR'          :5  :P=REPL('!',lnClrLen)  :V=lfvBrClr() :W=&lcWhn,"+;
*/*             "CLRDESC=gfCodDes(COLOUR,'COLOR')   :H='Colour Desc.' :15 :W=.F.,"+;
*/*             IIF(lnEntType=3,"PNTSVAL      :H='Points value' :V=lfvChkPstv('PNTSVAL') :W=&lcWhn :10  ," , '')+;
*/*             "ENTITLEMNT   :H='Entitlement'  :9  :P='99'+IIF(lnEntType=2,'.99','') :V=lfvChkPstv('ENTITLEMNT') :W=&lcWhn,"+;
*/*             "NLINK1       :H='Link'         :3  :V=lfvChkLn(1) :W=&lcWhn,"+;
*/*             "NLINK2       :H='to'           :3  :V=lfvChkLn(2) :W=&lcWhn,"+;
*/*             "NLINK3       :H='Line '        :3  :V=lfvChkLn(3) :W=&lcWhn"

lcBrFlds = "lcMrk=IIF(lnMrk=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "NLINE      :R:H='Line No'       :6 :W=.F. ,"+;
             "CSTYMAJOR    :H='Style'        :15 :P=REPL('!',lnStyleWid):V=lfvBrSty() :W=&lcStyWhn ,"+;
             "Style.Desc :R:H='Desc'         :20 :W=.F. ,"+;
             "COLOUR       :H='CLR'          :5  :P=REPL('!',lnClrLen)  :V=lfvBrClr() :W=&lcWhn,"+;
             "CLRDESC=gfCodDes(COLOUR,'COLOR')   :H='Colour Desc.' :15 :W=.F.,"+;
             IIF(lnEntType=3,"PNTSVAL      :H='Points value' :V=lfvChkPstv('PNTSVAL') :W=&lcWhn :10  ," , '')+;
             "ENTITLEMNT   :H='Entitlement'  :9  :P='99'+IIF(lnEntType=2,'.99','') :V=lfvChkPstv('ENTITLEMNT') :W=&lcWhn,"+;
             "NLINK1       :H='Link'         :3  :V=lfvChkLn(1) :W=&lcWhn,"+;
             "NLINK2       :H='Range'        :5  :V=lfvChkLn(2) :W=&lcWhn "


BROWSE FIELDS &lcBrFlds;
       LOCK 0  ;
       NOAPPEND;
       NOCLEAR ;
       NODELETE;
       NOMENU  ;
       NOWAIT  ;
       SAVE    ;
       WHEN lfwBrows();
       VALID :F lfvBrow();         
       TITLE (lcBrTtl) ;
       WINDOW (lcWinCh1) ;
       IN WINDOW (gcBaseWind)

=lfRefresh(lcBrTtl)
SELECT (lnSlct)

*:**************************************************************************
*:* Name        : lfwActBr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/17/2005
*:* Purpose     :  When fn. for fields for uniform browse file
*:***************************************************************************
FUNCTION lfwActBr
RETURN &lcWhn 

*-- end of lfwActBr.

*:**************************************************************************
*:* Name        : lfwBrows
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/07/2004
*:* Purpose     : Refresh record pointer.
*:***************************************************************************
FUNCTION lfwBrows

lnMrk  = RECNO(lcTmpUnfrm)
SHOW WINDOW (lcBrTtl) REFRESH
*/* SCATTER FIELDS NLINK1,NLINK2,NLINK3 TO laOldLink
SCATTER FIELDS NLINK1,NLINK2 TO laOldLink

*:**************************************************************************
*:* Name        : lfvBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Valid fn. for browse
*:***************************************************************************
FUNCTION lfvBrow

*-- end of lfvBrow.

*:**************************************************************************
*:* Name        : lfvBrSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/07/2004
*:* Purpose     : Valid for style in Uniform browse detail 
*:***************************************************************************
FUNCTION lfvBrSty
PRIVATE lcSty

*- 	Entitlement should default to the first colour of the style selected 
*-  and should show the colour description and not the colour code. 

SELECT &lcTmpUnfrm
lcSty = PADR(CSTYMAJOR,lnStyleWid)
IF !EMPTY(lcSty) .AND. !SEEK(lcSty,'STYLE')
  SELECT STYLE
  lcSty = gfStyBrw('M',ALLT(lcSty),'',.F.)
  SELECT &lcTmpUnfrm
  REPLACE CSTYMAJOR WITH lcSty
ENDIF

*/* IF !SEEK(lcSty+lcSepart+PADR(COLOUR,lnClrLen),'STYLE')
*/*   REPLACE COLOUR WITH ''
*/* ENDIF

IF !EMPTY(lcSty)
  =SEEK(lcSty,'STYLE')
  REPLACE StyleDesc WITH STYLE.DESC ;
          COLOUR    WITH SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen) ;
          CLRDESC   WITH gfCodDes(COLOUR,'COLOR')
ENDIF
*-- end of lfvBrSty.

*:**************************************************************************
*:* Name        : lfvBrClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Valid function for color browse
*:***************************************************************************
FUNCTION lfvBrClr
PRIVATE lcClr,lcSty
SELECT &lcTmpunfrm

lcClr = PADR(ALLT(COLOUR),lnClrLen)
lcSty = PADR(&lcTmpUnfrm..CSTYMAJOR,lnStyleWid)+lcSepart

IF EMPTY(lcClr) .OR. !SEEK(lcSty+lcClr,'STYLE')
  lcClrDesc = ''
  lcClr = lfLstClrs(lcSty)
  REPLACE COLOUR  WITH lcClr ;
          CLRDESC WITH lcClrDesc
ENDIF
*-- end of lfvBrClr.

*:**************************************************************************
*:* Name        : lfvChkPstv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Check positive 
*:***************************************************************************
FUNCTION lfvChkPstv
PARAMETERS lcFldname
PRIVATE lnRecno,lnLine,lnEnt
IF &lcFldname < 0
  REPLACE (lcFldname) WITH 0
ENDIF  

lnRecno = RECNO()
lnLine = NLINE
lnEnt = ENTITLEMNT
GO TOP
SCAN
*/*   IF INLIST(lnLine,NLINK1,NLINK2,NLINK3)
*/*     REPLACE ENTITLEMNT WITH lnEnt
*/*   ENDIF
  IF BETWEEN(lnLine,NLINK1,NLINK2)
    REPLACE ENTITLEMNT WITH lnEnt
  ENDIF
ENDSCAN

GOTO (lnRecno)

*-- end of lfvChkPstv.

*:**************************************************************************
*:* Name        : lfvChkLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Check line refrence exits
*:***************************************************************************
*C123847,1  TMI [Start] change the way the function work
FUNCTION X_lfvChkLn
*C123847,1  TMI [End  ] 
PARAMETERS lnLn,llFrmCod
PRIVATE lnRecno,lcLn,lnLine,llFound,;
        lnI,lcI,lnCurrLn,lcTmpLnk,lcLnk,;
        lnJ,lcJ,llOtherLn,lnOtherLnA,lnOtherLnB,lnGoLn
lnRecno = RECNO()

lcLn   = STR(lnLn,1)
lnLine = NLINK&lcLn

IF !llFrmCod
  *- if no changes occured , exit the function
  IF laOldLink[lnLn] = NLINK&lcLn
    RETURN
  ENDIF
ENDIF

lnCurrLn = NLINE     
lcLnk = ''
llFound = .F.
IF !EMPTY(lnLine)
    
    *- Add a refrence of a line
  
  *- Check here that the line you enter is actually exists in the file , and no other linked lines 
  *- in the same line refer to the same line
  lnOtherLnA = IIF(lnLn=1,NLINK2,NLINK1)
  lnOtherLnB = IIF(lnLn=3,NLINK2,NLINK3)
  llOtherLn = IIF(lnOtherLnA=0,.T.,lnOtherLnA<>lnLine) .AND. IIF(lnOtherLnB=0,.T.,lnOtherLnB<>lnLine)
  IF lnLine<>NLINE .AND. llOtherLn .AND. SEEK(lnLine)
    FOR lnI = 1 TO 3
      lcI = STR(lnI,1)
      IF NLINK&lcI = 0
        REPLACE NLINK&lcI WITH lnCurrLn ;
                LNK&lcI   WITH STR(lnCurrLn,2)+lcLn
        lcLnk = STR(NLINE,2)+lcI
        llFound = .T.
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*- If there is any old link , remove it
GOTO (lnRecno)
IF !EMPTY(LNK&lcLn)
  lnGoLn = VAL(LEFT(LNK&lcLn,2))
  lcI    = RIGHT(LNK&lcLn,1)
  =SEEK(lnGoLn)
  BLANK FIELDS NLINK&lcI, LNK&lcI
ENDIF

*- Establish a refrence for the new link
GOTO (lnRecno)
REPLACE LNK&lcLn WITH lcLnk
IF !llFound
  BLANK FIELDS NLINK&lcLn
ENDIF

*- Update old Value array
SCATTER FIELDS NLINK1,NLINK2,NLINK3 TO laOldLink

IF !llFrmCod
  *- if line 1 , 2 are linked and if 1 , 3 are also linked , then 2,3 must be linked
  *- so call the function will call itself to update other lines "llFrmCod is .T."
  DIMENSION laOtLnks[3,2]  && this array holds the three pairs (NLINK1,NLINK2),(NLINK1,NLINK3),(NLINK2,NLINK3)
  laOtLnks[1,1] = NLINK1
  laOtLnks[1,2] = NLINK2
  laOtLnks[2,1] = NLINK1
  laOtLnks[2,2] = NLINK3
  laOtLnks[3,1] = NLINK2
  laOtLnks[3,2] = NLINK3
  FOR lnI = 1 TO 3
    IF laOtLnks[lnI,1]<>0 .AND. laOtLnks[lnI,2]<>0
      =SEEK(laOtLnks[lnI,1])
      IF !INLIST(laOtLnks[lnI,2],NLINK1,NLINK2,NLINK3)
        FOR lnJ = 1 TO 3
          lcJ = STR(lnJ,1)
          IF NLINK&lcJ = 0
            REPLACE NLINK&lcJ WITH laOtLnks[lnI,2]
            =lfvChkLn(lnJ,.T.)          
          ENDIF
        ENDFOR
      ENDIF
    ENDIF
  ENDFOR
  
  SELECT &lcTmpunfrm
  GOTO (lnRecno)
ENDIF
*-- end of x_lfvChkLn.
*:**************************************************************************
*:* Name        : lfvChkLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Check line refrence exits
*:***************************************************************************
FUNCTION lfvChkLn
PARAMETERS lnLn
PRIVATE lnRecno,lcLn,lnLine,llFound,;
        lnI,lcI,lnCurrLn,lcTmpLnk,lcLnk,;
        lnJ,lcJ,llOtherLn,lnOtherLnA,lnOtherLnB,lnGoLn,;
        llReturn,laLnk
llReturn = .F.        
lnRecno = RECNO()

lcLn   = STR(lnLn,1)
lnLine = NLINK&lcLn

*- if no changes occured , exit the function
IF laOldLink[lnLn] = NLINK&lcLn
  *- Nothing to Do   
  SCATTER FIELDS NLINK1,NLINK2 TO laOldLink
  RETURN
ENDIF  
  
*- If start is greater than end , exit the function and restore values
IF NLINK1*NLINK2>0 .AND. NLINK1 >= NLINK2
  REPLACE NLINK&lcLn WITH laOldLink[lnLn]
  IF NLINK&lcLn=0
    BLANK FIELDS NLINK&lcLn
  ENDIF  
  SCATTER FIELDS NLINK1,NLINK2 TO laOldLink
  RETURN
ENDIF  
  
*- if the line if not found 
IF lnLine>0
  LOCATE FOR NLINE = lnLine
  IF !FOUND()
    GOTO (lnRecno)
    REPLACE NLINK&lcLn WITH laOldLink[lnLn]
    IF NLINK&lcLn=0
      BLANK FIELDS NLINK&lcLn
    ENDIF  
    SCATTER FIELDS NLINK1,NLINK2 TO laOldLink
    RETURN
  ENDIF
  GOTO (lnRecno)
ENDIF

*- Can not accept negative values
IF NLINK&lcLn<0 
  BLANK FIELDS NLINK&lcLn
  SCATTER FIELDS NLINK1,NLINK2 TO laOldLink
  RETURN
ENDIF  

IF NLINK&lcLn = 0 .AND. laOldLink[lnLn]>0
  BLANK FIELDS NLINK1,NLINK2 FOR BETWEEN(NLINE,laOldLink[1],laOldLink[2])
  GOTO (lnRecno)
  SCATTER FIELDS NLINK1,NLINK2 TO laOldLink
  RETURN
ENDIF  

*- Update the link from nlink1 to nlink2
IF NLINK1<>0 .AND. NLINK2<>0
  lnLINK1 = NLINK1
  lnLINK2 = NLINK2
  LOCATE FOR NLINE = lnLINK1
  SCATTER FIELDS NLINK1,NLINK2 TO laLnk
  BLANK FIELDS NLINK1,NLINK2 FOR BETWEEN(NLINE,laLnk[1],laLnk[2]) .OR. ;
                                 BETWEEN(NLINE,laOldLink[1],laOldLink[2]) .OR. ;
                                 (NLINK1>NLINE .OR. NLINE>NLINK2)

  LOCATE 
  REPLACE NLINK1 WITH lnLINK1 ;
          NLINK2 WITH lnLINK2 ;
          FOR BETWEEN(NLINE,lnLink1,lnLink2)
  
ENDIF

*- Back to current line , *- Update old Value array
GOTO (lnRecno)
SCATTER FIELDS NLINK1,NLINK2 TO laOldLink

*-- end of lfvChkLn.

*:**************************************************************************
*:* Name        : lpSavscr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Make local save
*:***************************************************************************
PROCEDURE lpSavscr
PRIVATE lnSlct,lcDeleted
lnSlct = SELECT()

*- if llCSave is set to .F. this prevent gpCpSave to save user defined fields 
llCSave = .F.   

*-...
*-..
*-.

*- check data is ok to save
IF !lfSavOk()
  RETURN
ENDIF  

*-...
*-..
*-.

*- When saving in edit mode , check the deleted record , 
*-           if in Add mode pack the temp file
SELECT &lcTmpUnfrm
IF laScrMode[4]
  PACK
  
  *- Update base file for new added codes
  IF !SEEK(laData[1],lcUnfrm)
    INSERT INTO &lcUnfrm (UCODE,UDESC,TYPE) VALUES (laData[1],laData[2],laEntType[lnEntType])
  ENDIF
  
ELSE
  lcDeleted = SET('DELETED')
  SET DELETED OFF
  REPLACE LDEL WITH .T. FOR DELETED()
  RECALL ALL
  SET DELETED &lcDeleted
ENDIF  

SELECT &lcTmpUnfrm
GO TOP
SCAN
  SCATTER MEMVAR
  IF !SEEK(UCODE+STR(NLINE,2),'UNIFORM')
    SELECT UNIFORM
    APPEND BLANK
  ELSE
    IF LDEL
      SELECT UNIFORM
      DELETE
      LOOP
    ENDIF
  ENDIF  

  m.UDESC      = laData[2]
  m.TYPE       = laEntType[lnEntType]
  m.DSTART     = laData[3]
  m.DEND       = laData[4]
  m.NPERIOD    = laData[5]

  SELECT UNIFORM
  GATHER MEMVAR

  =lfAudtFlds()
ENDSCAN

SELECT &lcTmpunfrm
ZAP

SELECT UNIFORM
IF laScrMode[4]
  laScrMode    = .F.
  laScrMode[1] = .T.
ELSE
  laScrMode    = .F.
  laScrMode[2] = .T.
ENDIF
SHOW  GETS

SELECT (lnSlct)

*:**************************************************************************
*:* Name        : lfAudtFlds
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Update audit fields
*:***************************************************************************
FUNCTION lfAudtFlds
IF laScrMode[4]
  REPLACE CADD_USER WITH gcUser_ID ;
          CADD_TIME WITH TIME()    ;
          DADD_DATE WITH gdSysDate
ELSE
  REPLACE CEDIT_USER WITH gcUser_ID ;
          CEDIT_TIME WITH TIME()    ;
          DEDIT_DATE WITH gdSysDate
ENDIF
*-- end of lfAudtFlds.

*:**************************************************************************
*:* Name        : lfSavOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Check data is ok to save 
*:***************************************************************************
FUNCTION lfSavOk
PRIVATE llGoSv, laRepeated, lnCnt,laLink

*- Check the uniform data is OK to save
SELECT &lcTmpunfrm
IF EMPTY(laData[2])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Missing Uniform Description, Can not Save.')
  _CUROBJ = OBJNUM(laData[2])
  RETURN .F.
ENDIF
IF EMPTY(laData[3])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Missing Date Created, Can not Save.')
  RETURN .F.
ENDIF
IF EMPTY(laData[5])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Missing Period, Can not Save.')
  RETURN .F.
ENDIF

lnCnt = 0
COUNT TO lnCnt
IF lnCnt = 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No record found, Can not Save.')
  RETURN .F.  
ENDIF

*- Check missing fields
llGoSv = .T.
SCAN  
  DO CASE
  CASE EMPTY(CSTYMAJOR)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Missing Style in Line '+STR(NLINE,2) + ', Can not Save.' )
    llGoSv = .F.
  CASE EMPTY(COLOUR)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Missing colour in Line '+STR(NLINE,2) + ', Can not Save.' )
    llGoSv = .F.
  CASE TYPE = 'P' .AND. PNTSVAL = 0    
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Missing POINTS VALUE in Line '+STR(NLINE,2) + ', Can not Save.' )
    llGoSv = .F.
  CASE EMPTY(ENTITLEMNT)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Missing ENTITLEMNT in Line '+STR(NLINE,2) + ', Can not Save.' )
    llGoSv = .F.
  ENDCASE
  IF !llGoSv
    EXIT
  ENDIF
ENDSCAN

IF !llGoSv
  RETURN .F. 
ENDIF

*- Check no repeated styles
SELECT &lcTmpunfrm
GO TOP
DIME laRepeated[1]
laRepeated = ' '
_TALLY = 0
SELECT CSTYMAJOR,COLOUR,COUNT(*) AS CNT ;
 FROM &lcTmpunfrm ;
 GROUP BY CSTYMAJOR,COLOUR ;
 HAVING CNT>1 ;
 INTO ARRAY laRepeated
IF _TALLY > 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Same Style and Colour in two different lines, Can not Save.')
  RETURN .F.
ENDIF   

*- If type is POINTS, do not allow the field PNTSVAL to be zero
IF lnEntType = 3
  SELECT &lcTmpunfrm
  GO TOP
  LOCATE FOR PNTSVAL = 0
  IF FOUND()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'If type is POINTS, do not allow the field PNTSVAL to be zero.')
    RETURN .F.
  ENDIF
ENDIF

*/* *- Check linked styles are linked correctly
*/* *- That is if we have 3 styles 
*/* *- and if 2 styles are linked then if a 3rd style is linked to either
*/* *- of the styles then it must be linked to both of them.
*/* PRIVATE lnSum,lnEnt
*/* SELECT NLINE,NLINK1,NLINK2,NLINK3 FROM &lcTmpunfrm INTO ARRAY laLink
*/* FOR lnCnt = 1 TO ALEN(laLink,1)
*/*   =SEEK(laLink[lnCnt,1])
*/*   lnSum = NLINE+NLINK1+NLINK2+NLINK3
*/*   lnEnt = ENTITLEMNT
*/*   lnPNTSVAL = PNTSVAL
*/*   GO TOP
*/*   SCAN FOR INLIST(NLINE,laLink[lnCnt,2],laLink[lnCnt,3],laLink[lnCnt,4])
*/*     IF NLINE+NLINK1+NLINK2+NLINK3 <> lnSum
*/*       =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Linked styles are Not linked correctly.')
*/*       RETURN .F.
*/*     ENDIF
*/*     IF lnEnt <> ENTITLEMNT
*/*       =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Entitlement number can not differs for linked styles.')
*/*       RETURN .F.
*/*     ENDIF
*/* **    IF lnPNTSVAL <> PNTSVAL
*/* **      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'PNTSVAL number can not differs for linked styles.')
*/* **      RETURN .F.
*/* **    ENDIF
*/*   ENDSCAN
*/* ENDFOR  

SELECT &lcTmpunfrm
GO TOP
LOCATE FOR (NLINK1=0 .AND. NLINK2>0) .OR. (NLINK1>0 .AND. NLINK2=0)
IF FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Linked styles are Not linked correctly.')
  RETURN .F.
ENDIF  

*- Check linked styles are linked correctly
*- That is if we have 3 styles 
*- and if 2 styles are linked then if a 3rd style is linked to either
*- of the styles then it must be linked to both of them.
PRIVATE lnSum,lnEnt
SELECT NLINK1,NLINK2 FROM &lcTmpunfrm INTO ARRAY laLink
FOR lnCnt = 1 TO ALEN(laLink,1)
  IF laLink[lnCnt,1]>0 .AND. laLink[lnCnt,2]>0
    LOCATE
    =SEEK(laLink[lnCnt,1])
    lnEnt = ENTITLEMNT
    LOCATE
    SCAN FOR BETWEEN(NLINE,laLink[lnCnt,1],laLink[lnCnt,2])
      IF lnEnt <> ENTITLEMNT
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Entitlement number can not differs for linked styles.')
        RETURN .F.
      ENDIF
    ENDSCAN
  ENDIF
ENDFOR  

RETURN .T.
*-- end of lfSavOk.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 12/08/2004
*! Purpose   : To make local delete.
*!*************************************************************
PROCEDURE lpDelScr
PRIVATE lnSlct
lnSlct = SELECT()
*- add here code to delete a uniform provided no employee has assigned it 
IF SEEK(laData[1],'EMPLOYEE')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not delete this uniform code, it is already assigned to some employees.')
  RETURN 
ELSE  
  SELECT UNIFORM
  =SEEK(laData[1],'UNIFORM')
  DELETE REST WHILE UCODE+STR(NLINE,2) = laData[1]
  
  SELECT &lcTmpunfrm
  ZAP
  
  *- Refresh browse.
  =lfActBr()
  
  *- Delete uniform from temp base file 
  IF SEEK(laData[1],lcUnfrm)
    SELECT &lcUnfrm
    DELETE
  ENDIF

  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF  
SELECT (lnSlct)

*:**************************************************************************
*:* Name        : lfOldValue
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/07/2004
*:* Purpose     : Save old value of a field in "lcOldValue" variable
*:***************************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))

*:**************************************************************************
*:* Name        : lpNxtScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/29/2004
*:* Purpose     : called from global function to skip to next record
*:***************************************************************************
*:* Called from : gfCPNext IN ARIA27.PRG
*:***************************************************************************
FUNCTION lpNxtScr
laData[1] = &lcUnfrm..UCODE

*- Locate the pointer in the temp base file
=SEEK(laData[1],lcUnfrm)

=lfPopupTmp() 
*-- end of lpNxtScr.


*:**************************************************************************
*:* Name        : lpPrvScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/29/2004
*:* Purpose     : Skip to previous record
*:***************************************************************************
*:* Called from : gfCPPrvis in ARIA27.PRG
*:***************************************************************************
FUNCTION lpPrvScr
laData[1] = &lcUnfrm..UCODE

*- Locate the pointer in the temp base file
=SEEK(laData[1],lcUnfrm)

=lfPopupTmp()
*-- end of lpPrvScr.


*-------------------------*  Valid functions for the screen  *-------------------------*
*:**************************************************************************
*:* Name        : lfvAdd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Add a new line function
*:***************************************************************************
FUNCTION lfvAdd
PRIVATE lnSlct,lnCnt

lnSlct = SELECT()
SELECT &lcTmpUnfrm
GO BOTTOM
lnCnt = NLINE
APPEND BLANK
REPLACE NLINE   WITH lnCnt+1   ;
        UCODE   WITH laData[1] ;
        UDESC   WITH laData[2] ;
        TYPE    WITH laEntType[lnEntType] ;
        DSTART  WITH laData[3] ;
        DEND    WITH laData[4] ;
        NPERIOD WITH laData[5]

SHOW GET pbSv  ENABLE
SHOW GET pbDel ENABLE

*- Refresh browse
_CUROBJ = OBJNUM(pbAdd)
SHOW GET pbADd DISABLE
ACTIVATE WINDOW (lcBrTtl)
=lfwBrows()
KEYBOARD '{BACKTAB}'
SHOW GET pbADd ENABLE

=lfRefresh()


*-- end of lfvAdd.
*:**************************************************************************
*:* Name        : lfvDel 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Delete function
*:***************************************************************************
FUNCTION lfvDel
PRIVATE lnSlct,laCnt
lnSlct = SELECT()
SELECT &lcTmpUnfrm

IF !BOF() .AND. !EOF()
*/*   IF NLINK1<>0 .OR. NLINK2<>0 .OR. NLINK3<>0
  IF NLINK1<>0 .OR. NLINK2<>0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This line is linked to other lines, remove link first!')
    RETURN
  ENDIF
  DELETE
  SHOW GET pbSv ENABLE
  
  SKIP -1
  IF BOF()
    GO TOP
    IF EOF()
      SHOW GET pbDel DISABLE
    ENDIF
  ENDIF

  *- Refresh browse
  =lfwBrows()

ENDIF

SELECT (lnSlct)
*-- end of lfDel.
*:**************************************************************************
*:* Name        : lfvSav
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Save function
*:***************************************************************************
FUNCTION lfvSav
DO lpSavScr

*-- end of lfSav.
*:**************************************************************************
*:* Name        : lfvClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Clear function
*:***************************************************************************
FUNCTION lfvClr
PRIVATE lnResp
lnResp = 1
*- Show the message if not view mode
IF laScrMode[3] .OR. laScrMode[4]
  lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure to undo all changes and go to Select Mode?')
ENDIF
IF lnResp = 1
  =lfClearLck()
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF  

*-- end of lfClr.
*:**************************************************************************
*:* Name        : lfClo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2004
*:* Purpose     : Close function
*:***************************************************************************
FUNCTION lfvClo
PRIVATE lnResp
lnResp = 1
*- Show the message if  Add/Edit mode , 
IF laScrMode[3] .OR. laScrMode[4]
  lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure close the screen and lose current changes?')
ENDIF
IF lnResp = 1  
  =lfClearLck()
  glQuitting = .T.
  CLEAR READ
ENDIF
*-- end of lfClo.

*:**************************************************************************
*:* Name        : lfClearLck
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Clear locks
*:***************************************************************************
FUNCTION lfClearLck
SELECT UNIFORM
IF SEEK(laData[1])
  SCAN REST WHILE UCODE+STR(NLINE,2) = laData[1]
    =gfObj_Lock(.F.)
  ENDSCAN
ENDIF

*-- end of lfClearLck.
*:**************************************************************************
*:* Name        : lfBrBack
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*! Purpose      : TO Assign functions to some keys to not affect the browse
*:***************************************************************************

FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key


*-- Set the global flag "glFromBrow" to true only if one of the
*-- screen's browses is active.
glFromBrow = INLIST(WONTOP(),lcBrTtl)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF WONTOP() = lcBrTtl
  ON KEY LABEL TAB        DO lfBrTab
  ON KEY LABEL BACKTAB    DO lfBrBack
ENDIF  
*:**************************************************************************
*:* Name        : lfBrBack
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*! Purpose      : Clearing the previous trapping
*:***************************************************************************

FUNCTION lfClrTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 


*-- If going out of one of the browses, call the global function
*-- to stop the browse and reset the global flag.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the 
*-- trapped keys.
IF WONTOP () <> lcBrTtl
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF
SHOW GET pbClo ENABLE

*:**************************************************************************
*:* Name        : lfBrBack
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*! Purpose      : Trap the Tab Key
*:***************************************************************************
FUNCTION lfBrTab

ON KEY LABEL TAB

ACTIVATE WINDOW (lcWinCh2) 

*:**************************************************************************
*:* Name        : lfBrBack
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Trap the BackTab Key
*:***************************************************************************
FUNCTION lfBrBack
ON KEY LABEL BACKTAB

ACTIVATE WINDOW gwcContrl1
_CUROBJ = OBJNUM(pbCls)

*!*************************************************************
*! Name      : lfGetClrD
*! Developer : TMI
*! Date      : 04/03/2001
*! Purpose   : To get color position also color length
*!*************************************************************
*! Called from : ICSLCAT1.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetClrD
*!*************************************************************
FUNCTION lfGetClrD
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
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfLstClrs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Get List of Colors of a style from style.dbf
*:***************************************************************************
FUNCTION lfLstClrs
PARAMETERS lcSty
PRIVATE   lcBrWinName ,lcBrClrs     ,lcClr,;
        lnWinWidth, lnWinHight, lnSelRec,lcTmpClr,;
        lnSlct
        
lnSlct = SELECT()

lcTmpClr = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpClr) (COLOR C(lnClrLen),CLRDESC C(30))
INDEX ON COLOR TAG &lcTmpClr
SELECT STYLE
=SEEK(lcSty)
SCAN REST WHILE STYLE = lcSty
  lcClr = SUBSTR(STYLE,lnClrPos,lnClrLen)
  IF !SEEK(lcClr,lcTmpClr)
    INSERT INTO &lcTmpClr VALUES (lcClr , gfCodDes(lcClr,'COLOR'))
  ENDIF
ENDSCAN

lcBrClrs     = 'Colour List of Style :' + SUBSTR(&lcTmpunfrm..CSTYMAJOR,1,lnStyleWid)
lcBrWinName = gfTempName()

lnWinWidth = 60
lnWinHight = 20

lnSelRec = 1


DEFINE WINDOW (lcBrWinName);
       AT 0, 0 SIZE lnWinHight, lnWinWidth;
       FONT "MS Sans Serif", 9 ;
       FLOAT ;
       NOCLOSE ;
       SHADOW ;
       NOMINIMIZE ;
       SYSTEM ;
       COLOR SCHEME 10  	   
MOVE WINDOW (lcBrWinName) CENTER
PUSH KEY
ON KEY
ON KEY LABEL ENTER DO lpSelOnEnt WITH lcBrClrs

SELECT &lcTmpClr
GO TOP
BROWSE FIELDS Color :H='Colour',;
              CLRDESC :H='Description';
       WINDOW (lcBrWinName);
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;
       TITLE lcBrClrs

RELEASE WINDOW (lcBrWinName)
ON KEY
POP KEY

lcRetClr = &lcTmpClr..COLOR
lcClrDesc = &lcTmpClr..CLRDESC
USE IN &lcTmpClr
ERASE (gcWorkDir+lcTmpClr+'.DBF')
ERASE (gcWorkDir+lcTmpClr+'.CDX')

SELECT (lnSlct)
RETURN lcRetClr

*-- end of lfLstClrs.