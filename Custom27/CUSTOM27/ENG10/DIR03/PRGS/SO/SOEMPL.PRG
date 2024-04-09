*:************************************************************
*: Program file  : SOEMPL.PRG
*: Program desc. : Employee/Wearer File:
*:               : This screen allows the user to give details of each Employee 
*:               : that works for them and what Uniform Code they are entitled to.
*: For screen    : SOEMPL.SPR
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (SO).
*:     Developer : TMI - TAREK MOHAMMED IBRAHIM
*: Date          : 12/09/2004
*: Entry         : C # 123847,1
*:************************************************************
*-- laData     Array that hold the data of the basefile
*-- laScrMode  Array that hold the screen modes

EXTERNAL ARRAY laData   ,laKeyField, laDefProc 
DIMENSION      laData[1],laKeyField[3,4],laScrMode[4]

laDefProc[3] = .F.                 && Use "lpNxtScr" to skip to next record
laDefProc[4] = .F.                 && Use "lpPrvScr" to skip to next record
laDefproc[9] = .F.                 && This is to Enable local save

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'EMPLOYEE'
laKeyField[1,4] = 1

laKeyField[1,1] = 'laData[2]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'EMPLOYEE'
laKeyField[1,4] = 2

laKeyField[1,1] = 'laData[3]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'EMPLOYEE'
laKeyField[1,4] = 3

*- Browse variable
llBrowse = .F.

*- Initialize state variables for the screen
PRIVATE     lcSvProc, lcWinCh1, lcUnfrm,lcTmpEmpl,lcEntUsed,lcOldValue,lcUniDesc,lcStoDesc
STORE '' TO lcSvProc, lcWinCh1, lcUnfrm,lcTmpEmpl,lcEntUsed,lcOldValue,lcUniDesc,lcStoDesc

=gfOpenFile(gcDataDir+'UNIFORM' ,'UNIFORMLN','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER' ,'SH')
=gfOpenFile(gcDataDir+'STYHIST' ,'STYHIST'  ,'SH')
=gfOpenFile(gcDataDir+'TRNHIST' ,'TRNHIST'  ,'SH')
=gfOpenFile(gcDataDir+'ORDHDR'  ,'ORDHDR'   ,'SH')
=gfOpenFile(gcDataDir+'STYLE'   ,'CSTYLE'   ,'SH')
=gfOpenFile(gcDataDir+'SCALE'   ,'SCALE'    ,'SH')
=gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE' ,'SH')
=gfOpenFile(gcDataDir+'ORDLINE' ,'ORDLINE'  ,'SH')

IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind) 
  
  lcBrFields = "Account   :R :H='Account'          :16," +;
               "STORE     :R :H='Store'            :19," +;
               "EMPLOYEE  :R :H='Employee Code'    :16," +;
               "EMPNAME   :R :H='Employee Name'    :30," +;
               "UCODE     :R :H='Uniform Code'     :12" 

  lcScFields = 'ACCOUNT   ,STORE     ,EMPLOYEE  ,EMPNAME   ,EMPADD1   ,' +;
               'EMPADD2   ,EMPADD3   ,EMPADD4   ,EMPADD5   ,PAYROLLNO ,' +;
               'SITE_NO   ,DSTART    ,UCODE     ,NPERIOD   ,DEND      '
  
  lcWinCh1  = gfTempName()
  lcTmpEmpl = gfTempName()
  lcUnfrm   = gfTempName()
  lcEntUsed = gfTempName()
  =lfFilTmpBs()
  
  SELECT EMPLOYEE
  SCATTER FIELDS &lcScFields TO laData BLANK
  
ENDIF

*- Variable to disable '\<Edit Ent. Used Qty.' menu option
llTrnFound = .F.

PUSH KEY                                     && To save the the current on key label
DO (gcScrDir + gcWinAppl + '\SOEMPL.SPR')    && calling the screen "SOEMPL"
POP KEY                                      && To Restore the previous assignments for on key label

*-- If we realy quitting the screen 
IF glQuitting

  *- Quitting routine
  IF USED(lcUnfrm)
    USE IN &lcUnfrm
  ENDIF
  ERASE (gcWorkDir+lcUnfrm+'.DBF')
  ERASE (gcWorkDir+lcUnfrm+'.CDX')

  IF USED(lcTmpEmpl)
    USE IN &lcTmpEmpl
  ENDIF
  ERASE (gcWorkDir+lcTmpEmpl+'.DBF')
  ERASE (gcWorkDir+lcTmpEmpl+'.CDX')

  IF USED(lcEntUsed)
    USE IN &lcEntUsed
  ENDIF
  ERASE (gcWorkDir+lcEntUsed+'.DBF')
  ERASE (gcWorkDir+lcEntUsed+'.CDX')
ENDIF  
RELEASE PAD _INQUIRY OF _MSYSMENU

*:**************************************************************************
*:* Name        : lpShow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Handling the screen mode
*:***************************************************************************
PROCEDURE lpShow
PRIVATE lnSlct
lnSlct = SELECT(0)

*- set the Variable to disable '\<Edit Ent. Used Qty.' menu option
llTrnFound = IIF(laScrMode[4],.F., SEEK(laData[1]+laData[2]+laData[3],'TRNHIST') )

DO CASE
  *-- Select mode
  CASE laScrMode[1]
    
    IF USED(lcEntUsed)
      SELECT &lcEntUsed
      ZAP
    ENDIF
    
    STORE '' TO lcStoDesc, lcUniDesc
    SELECT EMPLOYEE
    SCATTER FIELDS &lcScFields TO laData BLANK
    _CUROBJ = OBJNUM(laData[1])
    
  *-- View mode
  CASE laScrMode[2]
    
    IF USED(lcEntUsed)
      SELECT &lcEntUsed
      ZAP
    ENDIF

    SELECT EMPLOYEE
    SCATTER FIELDS &lcScFields TO laData
    _CUROBJ = OBJNUM(pbCls)
    
    *- If this employee has ordered some styles , do not allow to delete him
    laCtrStat[8] = IIF( SEEK(laData[1]+laData[2]+laData[3],'STYHIST') , 'DISABLE' , 'ENABLE' )
    SHOW GET pbDlt &laCtrStat[8]

  *-- Edit mode
  CASE laScrMode[3]
    *--     
    
  *-- Add mode
  CASE laScrMode[4]
    *-- 
    
ENDCASE

*:**************************************************************************
*:* Name        : lfvData_1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on UCODE field
*:***************************************************************************
FUNCTION lfvData_1
PRIVATE xAccount
IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK('M'+laData[1],'CUSTOMER'))
  xAccount = laData[1]
  SELECT CUSTOMER
  PRIVATE lcBrFields, lcBrowTitle  
  DO CUSBROWM WITH xAccount
  laData[1] = xAccount
ENDIF

IF EMPTY(laData[1])
  laData[2] = ''
*  _CUROBJ = OBJNUM(laData[1])
ENDIF  

=lfRefresh()
llBrowse = .F.

*:**************************************************************************
*:* Name        : lfvData_2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on DATE CREATED field
*:***************************************************************************
FUNCTION lfvData_2
PRIVATE xStore,lcOldAcc

lcStoDesc = ''
IF !EMPTY(laData[2]) .OR. llBrowse
  IF SEEK('S'+laData[1]+laData[2],'CUSTOMER')
    lcStoDesc = CUSTOMER.STNAME
  ELSE
    xStore   = laData[2]
    IF !CUSBROWS(laData[1],.T.)
      STORE SPACE(8) TO xStore
    ENDIF
    laData[2] = xStore
    lcStoDesc = IIF(!empty(xStore),CUSTOMER.STNAME,'')
  ENDIF  
ENDIF
=lfRefresh()
llBrowse = .F.

*-- end of lfvData_2.

*:**************************************************************************
*:* Name        : lfvData_3
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Validation on DATE CREATED field
*:***************************************************************************
FUNCTION lfvData_3
PRIVATE lcModalMes

IF !llBrowse .AND. EMPTY(laData[3])
  RETURN
ENDIF
IF CHRTRAN(laData[3],'?/\','') <> laData[3]
  llBrowse = .T.
ENDIF
IF llBrowse
  =lfBrowEmp()
  IF !EMPTY(laData[3])
    =SEEK(laData[1]+laData[2]+laData[3],'EMPLOYEE')
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
ELSE
  IF !EMPTY(laData[3])
    IF SEEK(laData[1]+laData[2]+laData[3],'EMPLOYEE')
      
      SELECT EMPLOYEE
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
      
    ELSE
      
      IF !EMPTY(laData[1])
        lcModalMes = 'Employee Code : ' + ALLTRIM(laData[1])+' '
        lnResp = gfModalGen('QRM00001B42003','DIALOG',lcModalMes)
      ELSE
        && Add 1 to the response since the buttons 00014 are only two while 42003 are three
        lnResp = gfModalGen('QRM00000B00014',.F.,.F.,.F.,'Can not Select/Add an employee without Account Code.') + 1   
      ENDIF
      
      DO CASE
      CASE lnResp = 1 && Add
        laScrMode    = .F.
        laScrMode[4] = .T.
        SHOW GETS
        
      CASE lnResp = 2 && Browse
        =lfBrowEmp()
        IF !EMPTY(laData[3])
          laScrMode    = .F.
          laScrMode[2] = .T.
          SHOW GETS
        ENDIF
          
        
      CASE lnResp = 3 && Reenter
        laData[3] = ' '    
        _CUROBJ = OBJNUM(laData[3])
    
      ENDCASE
    ENDIF  
  
  ENDIF
ENDIF  

*- If this employee has ordered some styles , do not allow to delete him
laCtrStat[8] = IIF( SEEK(laData[1]+laData[2]+laData[3],'STYHIST') , 'DISABLE' , 'ENABLE' )
SHOW GET pbDlt &laCtrStat[8]

=lfRefresh()
llBrowse = .F.

*- set the Variable to disable '\<Edit Ent. Used Qty.' menu option
llTrnFound = IIF(laScrMode[4],.F., SEEK(laData[1]+laData[2]+laData[3],'TRNHIST') )

*:**************************************************************************
*:* Name        : lfBrowEmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Get employee from a browse
*:***************************************************************************
FUNCTION lfBrowEmp
PRIVATE laTemp,lcFieldsNam,lcBrFields

SELECT ACCOUNT,STORE,EMPLOYEE,EMPNAME,UCODE ;
  FROM EMPLOYEE ;
  WHERE ACCOUNT = ALLTRIM(laData[1]) .AND. STORE = ALLTRIM(laData[2]) ;
  INTO TABLE (gcWorkDir+lcTmpEmpl)
INDEX ON ACCOUNT+STORE+EMPLOYEE TAG &lcTmpEmpl
GO TOP
DIMENSION laTemp[4]
latemp = ''
lcFieldsNam = 'ACCOUNT,STORE,EMPLOYEE,EMPNAME'
lcBrFields  = 'ACCOUNT :H="Account",'+;
              'STORE   :H="Store",'+;
              'EMPLOYEE:H="Employee Code",'+;
              'EMPNAME :H="Employee Name",'+;
              'UCODE   :H="Uniform Code Assinged"'
PUSH KEY
ON KEY
PRIVATE lcFile_ttl,lcWinTitl
=Ariabrow('','Employees',gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
             lcFieldsNam,'laTemp')
POP KEY
laData[3] = ''
IF !EMPTY(laTemp[1])
  SELECT EMPLOYEE
  =SEEK(laTemp[1]+laTemp[2]+laTemp[3],'EMPLOYEE')
  laData[1] = laTemp[1]
  laData[2] = laTemp[2]
  laData[3] = laTemp[3]
  laData[4] = laTemp[4]
ENDIF
*-- end of lfBrowEmp.

*:**************************************************************************
*:* Name        : lfvUniform
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Valid fn. for uniform code
*:***************************************************************************
FUNCTION lfvUniform
PRIVATE laTemp,lcFieldsNam,lcBrFields,lcStat

IF !llBrowse .AND. EMPTY(laData[13])
  RETURN
ENDIF

IF CHRTRAN(laData[13],'?/\','') <> laData[13]
  llBrowse = .T.
ENDIF

IF llBrowse .OR. !SEEK(laData[13],'UNIFORM')

  SELECT &lcUnfrm
  GO TOP
  
  DIMENSION laTemp[2]
  latemp = ''
  lcFieldsNam = "UCODE,UDESC"
  lcBrFields = "UCODE   :H='Uniform Code',"+;
               "UDESC   :H='Description' ,"+;
               "TYPE    :H='Type'        ,"+;
               "NPERIOD :H='Period'      ,"+;
               "DSTART  :H='Start Date'  ,"+;
               "DEND    :H='End Date'    "
  PUSH KEY
  ON KEY
  PRIVATE lcFile_ttl,lcWinTitl
  =Ariabrow('','Uniform Code File Browse',gnbrfsrow1, gnbrfscol1,gnbrfsrow2, gnbrfscol2, '','',;
               lcFieldsNam,'laTemp')
  POP KEY
  
  laData[13] = ' '
  IF !EMPTY(laTemp[1])
    IF SEEK(laTemp[1],'UNIFORM')
      laData[13] = laTemp[1]
      lcUniDesc  = laTemp[2]
      SHOW GET laData[13]
      SHOW GET lcUniDesc 
    ENDIF
  ENDIF

ELSE

  =SEEK(laData[13],'UNIFORM')
  lcUniDesc = UNIFORM.UDESC
  
ENDIF

IF !EMPTY(laData[13])
  =SEEK(laData[13],'UNIFORM')
  *- If start date is entered befor do not change it
  IF EMPTY(laData[12])
    laData[12] = UNIFORM.DSTART
  ENDIF
  IF EMPTY(laData[14])
    laData[14] = UNIFORM.NPERIOD
  ENDIF
  IF EMPTY(laData[15])
    laData[15] = UNIFORM.DEND
  ENDIF
  
  SHOW GET laData[12]
  SHOW GET laData[14]
  SHOW GET laData[15]
  
  *- This line applies in sogtemp screen only
  lcStat = IIF(UNIFORM.TYPE='U','ENABLE','DISABLE')
  SHOW GET pbStyHist &lcStat
  
ENDIF

=lfRefresh()  
llBrowse = .F.

*-- end of lfvUniform.


*:**************************************************************************
*:* Name        : lfFilTmpBs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/09/2004
*:* Purpose     : Fill uniform code file as distinct ucode's
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
    SCATTER MEMVAR
    INSERT INTO &lcUnfrm FROM MEMVAR
  ENDIF
ENDSCAN
 
*:**************************************************************************
*:* Name        : lfvData_12
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/11/2004
*:* Purpose     : Valid fn. for period start date
*:***************************************************************************
FUNCTION lfvData_12
*=lfUpdDates(@laData[12],@laData[14],@laData[15])

PRIVATE ldStart,lnPeriod,ldEnd
ldStart = laData[12]
lnPeriod= laData[14]
ldEnd   = laData[15]

=lfUpdDates(@ldStart,@lnPeriod,@ldEnd)

laData[12] = ldStart
laData[14] = lnPeriod
laData[15] = ldEnd   
SHOW GET laData[12]
SHOW GET laData[14]
SHOW GET laData[15]
=lfRefresh()

*-- end of lfvData_12.

*:**************************************************************************
*:* Name        : lfvData_12
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/11/2004
*:* Purpose     : Valid fn. for period 
*:***************************************************************************
FUNCTION lfvData_14
IF laData[14] < 0
  laData[14] = 0
ENDIF  
*=lfUpdDates(@laData[12],@laData[14],@laData[15])

PRIVATE ldStart,lnPeriod,ldEnd
ldStart = laData[12]
lnPeriod= laData[14]
ldEnd   = laData[15]

=lfUpdDates(@ldStart,@lnPeriod,@ldEnd)

laData[12] = ldStart
laData[14] = lnPeriod
laData[15] = ldEnd   
SHOW GET laData[12]
SHOW GET laData[14]
SHOW GET laData[15]
=lfRefresh()



*-- end of lfvData_14.

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

SHOW GET ldStart
SHOW GET lnPeriod
SHOW GET ldEnd

=lfRefresh()  

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
lnDays = 0

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
*:* Name        : lpSavscr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2004
*:* Purpose     : Make local save
*:***************************************************************************
PROCEDURE lpSavscr
PRIVATE lnSlct
lnSlct = SELECT()

DO CASE
CASE EMPTY(laData[4])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty Employee Name, Can not save')
  _CUROBJ = OBJNUM(laData[4])
  llCSave = .F.   
CASE EMPTY(laData[12])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty Start Date, Can not save')
  _CUROBJ = OBJNUM(laData[12])
  llCSave = .F.   
CASE EMPTY(laData[14])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty Calculation Period, Can not save')
  _CUROBJ = OBJNUM(laData[14])
  llCSave = .F.   
CASE EMPTY(laData[15])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Empty End Date, Can not save')
  _CUROBJ = OBJNUM(laData[15])
  llCSave = .F.   
ENDCASE  

IF llCSave
  *- Save Entitlment Used field in StyHist file
*/*   IF laScrMode[4]
    =lfSvUsed()
*/*   ENDIF
  
  *- Update user fields ( since all screen fields are user fields )
  =lfUpUsrFld()
  
  SELECT EMPLOYEE 
  IF laScrMode[4]
    APPEND BLANK
  ELSE
    =SEEK(laData[1]+laData[2]+laData[3],'EMPLOYEE')
  ENDIF
  GATHER FIELDS &lcScFields FROM laData
  =lfAudtFlds()
ENDIF

*:**************************************************************************
*:* Name        : lfSvUsed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/14/2004
*:* Purpose     : Save Entitlement used qty in StyHist file
*:***************************************************************************
FUNCTION lfSvUsed
PRIVATE lnSlct
lnSlct = SELECT()

IF USED(lcEntUsed)
  SELECT &lcEntUsed
  LOCATE
  SCAN FOR ACCOUNT+STORE+EMPLOYEE+CSTYMAJOR+COLOUR = laData[1]+laData[2]+laData[3] ; 
           AND NUSED > 0
    SCATTER MEMVAR
    IF !SEEK(laData[1]+laData[2]+laData[3]+CSTYMAJOR+COLOUR,'STYHIST')
      INSERT INTO STYHIST FROM MEMVAR
    ELSE
      SELECT STYHIST
      REPLACE NUSED WITH M.NUSED
    ENDIF
  ENDSCAN
  
  SELECT &lcEntUsed
  ZAP
ENDIF  

SELECT (lnSlct)
*-- end of lfSvUsed.

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
*:* Name        : lfUpUsrFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/11/2004
*:* Purpose     : *- Update user fields
*:***************************************************************************
FUNCTION lfUpUsrFld
PRIVATE lnI,laFlds,lnPos
*DIMENSION laFlds[15]  
DIMENSION laFlds[OCCURS(',',lcScFields)+1]  

=gfSubStr(lcScFields,@laFlds,',')
FOR lnI = 1 TO ALEN(laFlds)
  lnPos = ASCAN(laUsrFields,PADR(laFlds[lnI],10) )
  IF lnPos>0
    lnPos = ASUBSCRIPT( laUsrFields , lnPos , 1 )
    laUsrFields[lnPos,6] = laData[lnI]
  ENDIF
ENDFOR

*-- end of lfUpUsrFld.

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
=SEEK('S'+EMPLOYEE.ACCOUNT+EMPLOYEE.STORE,'CUSTOMER')
lcStoDesc = CUSTOMER.STNAME 

lcUniDesc = ''
IF !EMPTY(EMPLOYEE.UCODE)
  =SEEK(EMPLOYEE.UCODE,'UNIFORM')
  lcUniDesc = UNIFORM.UDESC
ENDIF   

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
=SEEK('S'+EMPLOYEE.ACCOUNT+EMPLOYEE.STORE,'CUSTOMER')
lcStoDesc = CUSTOMER.STNAME 

lcUniDesc = ''
IF !EMPTY(EMPLOYEE.UCODE)
  =SEEK(EMPLOYEE.UCODE,'UNIFORM')
  lcUniDesc = UNIFORM.UDESC
ENDIF   
*-- end of lpPrvScr.

*:**************************************************************************
*:* Name        : lfActPad
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/12/2004
*:* Purpose     : define the option pad for the screen
*:***************************************************************************
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Style History'       SKIP FOR laScrMode[1]
DEFINE BAR 2  OF _INQURYPOP PROMPT '\<Transaction History' SKIP FOR laScrMode[1]
*/* *DEFINE BAR 3  OF _INQURYPOP PROMPT '\<Edit Ent. Used Qty.' SKIP FOR ;
*/*            laScrMode[1] .OR. laScrMode[2] .OR. laScrMode[3] .OR. EMPTY(laData[13])
DEFINE BAR 3  OF _INQURYPOP PROMPT '\<Edit Ent. Used Qty.' SKIP FOR ;
                       laScrMode[1] .OR. laScrMode[2] .OR. llTrnFound
ON SELECTION POPUP _INQURYPOP DO lpvInquiry

RETURN glReadWhen

*:**************************************************************************
*:* Name        : lpvInquiry
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/12/2004
*:* Purpose     : Run the selected option from option menu pad
*:***************************************************************************
FUNCTION lpvInquiry
PRIVATE lcKey,lcBrTtl,lcFile,lcBrFields,lcResource,lcStyHist
lcResource = SET('RESOURCE')
SET RESOURCE OFF
PUSH KEY
ON KEY
PRIVATE lcFile_ttl,lcWinTitl

DO CASE
CASE BAR() = 1      && Style History

  SELECT STYHIST
  SET RELATION TO 

  lcKey       = laData[1] + laData[2] + laData[3]
  lcBrTtl     = 'Style History' 

  *C123847,3  TMI [Start] 
  *lcBrFields  = "CSTYMAJOR                  :H='Style'            ," +;
                "COLOUR                     :H='Colour'           ," +;
                "STYLE.DESC                 :H='Style Desc'       ," +;
                "ENTITLEMNT                 :H='Entitlement'      ," +;
                "NUSED                      :H='Entitlement Used' ," +;
                "nBalance= ENTITLEMNT-NUSED :H='Balance to Use'   ," +;
                "DLASTORD                   :H='Last Order Date'  ," +;
                "ORDHDR.CCURRCODE           :H='Currency Code'    ," +;
                "ORDHDR.NEXRATE             :H='exchange rate'     " 
  lcBrFields  = "NLINE            :H='L#'               ," +;
                "NLINK1           :H='Lnk'              ," +;
                "NLINK2           :H='Lnk'              ," +;
                "CSTYMAJOR        :H='Style'            ," +;
                "COLOUR           :H='Colour'           ," +;
                "STYLE.DESC       :H='Style Desc'       ," +;
                "ENTITLEMNT       :H='Entitlement'      ," +;
                "NUSED            :H='Entitlement Used' ," +;
                "nBalance         :H='Balance to Use'   ," +;
                "DLASTORD         :H='Last Order Date'  ," +;
                "ORDHDR.CCURRCODE :H='Currency Code'    ," +;
                "ORDHDR.NEXRATE   :H='exchange rate'     " 
  *C123847,1  TMI [End  ] 

  lcStyHist = gfTempName()
  SELECT *,0000.00 AS nBalance,00 AS NLINE,00 AS NLINK1,00 AS NLINK2 ;
    FROM STYHIST ;
    WHERE ACCOUNT+STORE+EMPLOYEE = lcKey ;
    INTO TABLE (gcWorkDir+lcStyHist)    
  INDEX ON NLINE            TAG NLINE
  INDEX ON CSTYMAJOR+COLOUR TAG &lcStyHist ADDITIVE
  
  =lfEntlUsed()
  
  SET ORDER TO TAG NLINE
  SET RELATION TO CSTYMAJOR INTO STYLE, 'O'+ORDER INTO ORDHDR
  LOCATE
  =ARIABROW('',lcBrTtl,gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2)
  
  USE IN &lcStyHist
  ERASE (gcWorkDir+lcStyHist+'.DBF')
  ERASE (gcWorkDir+lcStyHist+'.CDX')

CASE BAR() = 2      && Transaction History

  lcKey       = laData[1] + laData[2] + laData[3]
  lcBrTtl     = 'Transaction History' 

  IF !USED('RETHDR')
    =gfOpenFile(gcDataDir+'INVHDR','INVHDR','SH')
  ENDIF
  IF !USED('RETHDR')
    =gfOpenFile(gcDataDir+'RETHDR','RETHDR','SH')
  ENDIF
  SET ORDER TO INVHDR IN INVHDR
  SET ORDER TO RETHDR IN RETHDR
  
*  lcBrFields  = "cTyp = IIF(TRANTYPE='I','Invoice','Credit Memo'):H='Trans Type'                , " +;
                "DTRANDT   :H='Trans Date'                , " +;
                "TRANNO    :H='Trans No'                  , " +;
                "CSTYMAJOR :H='Style '                    , " +;
                "cClr=gfCodDes(COLOUR,'COLOR'):H='Colour Desc'               , " +;
                "CSZ       :H='Size Desc'                  "
  lcBrFields  = "cTyp = IIF(TRANTYPE='I','Invoice','Credit Memo'):H='Trans Type'                , " +;
                "DTRANDT   :H='Trans Date'                , " +;
                "TRANNO    :H='Trans No'                  , " +;
                "STYLE     :H='Style '                    , " +;
                "QTY1      :H='Size1' :R,"+;
                "QTY2      :H='Size2' :R,"+;
                "QTY3      :H='Size3' :R,"+;
                "QTY4      :H='Size4' :R,"+;
                "QTY5      :H='Size5' :R,"+;
                "QTY6      :H='Size6' :R,"+;
                "QTY7      :H='Size7' :R,"+;
                "QTY8      :H='Size8' :R,"+;
                "TOTQTY    :H='No of Units' :R,"+;
                "cValBas=IIF(TRANTYPE='I', INVHDR.TOTALCHG*INVHDR.NEXRATE/INVHDR.NCURRUNIT,RETHDR.TOTCREDIT*RETHDR.NEXRATE/RETHDR.NCURRUNIT ) :H='Value base currency' ,"+;
                "cCurr  =IIF(TRANTYPE='I', INVHDR.CCURRCODE, RETHDR.CCURRCODE) :H='Currency of transaction', "+;
                "cRate  =IIF(TRANTYPE='I', INVHDR.NEXRATE  , RETHDR.NEXRATE  ) :H='Exchange Rate' ,"+;                
                "cValFrn=IIF(TRANTYPE='I', INVHDR.TOTALCHG , RETHDR.TOTCREDIT) :H='Value Foreign currency' ,"+;
                "cUnit  =IIF(TRANTYPE='I', INVHDR.NCURRUNIT, RETHDR.NCURRUNIT) :H='Currency unit value' "

  SELECT TRNHIST
  SET RELATION TO   
  SET RELATION TO TRANNO INTO INVHDR , TRANNO INTO RETHDR
  LOCATE  
  =ARIABROW([lcKey],lcBrTtl,gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2)

CASE BAR() = 3
  =lfEdtUsed()
  
ENDCASE

POP KEY
SET RESOURCE &lcResource
*-- end of lpvInquiry.

*:**************************************************************************
*:* Name        : lfEntlUsed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/02/2005
*:* Purpose     : Get balance to use for a style linked or not
*:***************************************************************************
FUNCTION lfEntlUsed
PRIVATE lnRecno,lnSlct
lnSlct = SELECT()

=SEEK(EMPLOYEE.UCODE,'UNIFORM')
SELECT UNIFORM
DO WHILE !EOF('UNIFORM') .AND. UNIFORM.UCODE = EMPLOYEE.UCODE
  SELECT UNIFORM
  lnLnk1 = IIF(NLINK1<>0,NLINK1,NLINE)
  lnLnk2 = IIF(NLINK2<>0,NLINK2,NLINE)
  lnUsed = 0
  lnRecno = RECNO()
  SCAN REST WHILE UCODE+CSTYMAJOR+COLOUR = EMPLOYEE.UCODE .AND. BETWEEN(NLINE,lnLnk1,lnLnk2)
    =SEEK(CSTYMAJOR+COLOUR,lcStyHist)
    lnUsed = lnUsed + &lcStyHist..NUSED
    SELECT &lcStyHist
    REPLACE NLINE  WITH UNIFORM.NLINE  ;
            NLINK1 WITH UNIFORM.NLINK1 ;       
            NLINK2 WITH UNIFORM.NLINK2
  ENDSCAN

  SELECT UNIFORM
  GOTO (lnRecno)
  SCAN REST WHILE UCODE+CSTYMAJOR+COLOUR = EMPLOYEE.UCODE .AND. BETWEEN(NLINE,lnLnk1,lnLnk2)
    =SEEK(CSTYMAJOR+COLOUR,lcStyHist)
    SELECT &lcStyHist
    REPLACE NBALANCE WITH ENTITLEMNT - lnUsed
  ENDSCAN
ENDDO

SELECT (lnSlct)
*-- end of lfEntlUsed.

*:**************************************************************************
*:* Name        : lfEdtUsed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/13/2004
*:* Purpose     : Edit used qty in the browse of style history file
*:***************************************************************************
FUNCTION lfEdtUsed
PRIVATE lcUsed

IF EMPTY(laData[13])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No uniform code assigned to this employee.')
  RETURN
ENDIF

=SEEK(laData[13],'UNIFORM')
IF UNIFORM.TYPE <> 'U'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can Only edit used qty for uniforms with "U" type.')
  RETURN
ENDIF 

*/* IF laScrMode[4] .AND. !EMPTY(laData[13])
IF !EMPTY(laData[13])

*/*   IF !USED(lcEntUsed) .OR. !SEEK(laData[1]+laData[2]+laData[3],lcEntUsed)
*/*     *- Create temp file to update used qty 
*/*     SELECT STYHIST
*/*     =AFIELDS(laStyHist)
*/*     CREATE TABLE (gcWorkDir+lcEntUsed) FROM ARRAY laStyHist
*/*     INDEX ON ACCOUNT+STORE+EMPLOYEE+CSTYMAJOR+COLOUR TAG &lcEntUsed

  IF !USED(lcEntUsed)
    *- Create temp file to update used qty 
    SELECT STYHIST
    =AFIELDS(laStyHist)
    CREATE TABLE (gcWorkDir+lcEntUsed) FROM ARRAY laStyHist
    INDEX ON ACCOUNT+STORE+EMPLOYEE+CSTYMAJOR+COLOUR TAG &lcEntUsed
  ENDIF
  IF !SEEK(laData[1]+laData[2]+laData[3],lcEntUsed)
    SELECT &lcEntUsed
    ZAP

    SELECT UNIFORM
    =SEEK(laData[13])
    SCAN REST WHILE UCODE+STR(NLINE,2) = laData[13]
      
      *- locate the style record in styhist file 
      =SEEK(laData[1]+laData[2]+laData[3]+UNIFORM.CSTYMAJOR+UNIFORM.COLOUR,'STYHIST')
      
      SELECT &lcEntUsed
      APPEND BLANK
      REPLACE ACCOUNT    WITH laData[1] ;
              STORE      WITH laData[2] ;
              EMPLOYEE   WITH laData[3] ;
              CSTYMAJOR  WITH UNIFORM.CSTYMAJOR  ;
              COLOUR     WITH UNIFORM.COLOUR     ;
              ENTITLEMNT WITH UNIFORM.ENTITLEMNT ;
              NUSED      WITH STYHIST.NUSED
      =lfAudtFlds()               
    ENDSCAN
  ENDIF
  
  SELECT &lcEntUsed
  LOCATE

  PRIVATE   lcBrWinName ,lcBrTtl ,lnWinWidth, lnWinHight, lnSelRec        
  
  lcBrTtl  = 'Uniform ' + laData[13] + ' Style list'
  lcBrWinName = gfTempName()
  
  lnWinWidth = 78
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
  ON KEY LABEL ENTER DO lpSelOnEnt WITH lcBrTtl
  
  lcUsed = IIF(UNIFORM.TYPE='U','Units',IIF(UNIFORM.TYPE='V','Value','Points'))
  GO TOP
  BROWSE FIELDS CSTYMAJOR :R:H='Style',;
                COLOUR    :R:H='Colour',;
                ENTITLEMNT:R:H='Entitlment',;
                NUSED       :H='Ent.Used '+lcUsed :P=IIF(UNIFORM.TYPE='V','99.99','99') ; 
         WINDOW (lcBrWinName);
         LOCK 0;
         NOMENU;         
         NOAPPEND;
         NODELETE;
         TITLE lcBrTtl
  
  RELEASE WINDOW (lcBrWinName)
  ON KEY
  POP KEY

ENDIF
*-- end of lfvEdtUsed.
