*!********************************************************************
*: Program file  : ALREG.PRG (REFERE TO C126785)
*: Program desc. : Regions AND States SCREEN FOR NORTH BAY .
*:               :  
*: For screen    :  ALREG
*:         System: ARIA 2.7
*:         Module: AL
*:      Developer: EHAB ISMAIL HAMED MOHAMED . EIH
*!********************************************************************
*!*: Modification
*!********************************************************************

EXTERNAL ARRAY laDefProc,laKeyField , laCtrStat
lcGroup    = SPACE(06)     && Variable for getting the Style Group
*-- Initializing the necessary variables
lnFileStru = 0
STORE "" TO laGrp, laBrowArr,laCodinfo , laKeyField , TempCursor , ;
            TempHdr , TmpFile, lcWindE, lcWindD,lcWindC,lcWindB,lcScFields
STORE 1 TO lnGrp

DIMENSION laScrMode[4],laBrowArr[1],laKeyField[1,4] , laData[5]
llAlowNew = .T.
STORE .F. TO laScrMode
laScrMode[1] = .T.

llClose    = .T.           && Variable to check the status of pbClose
llBrowse   = .F.           && Variable for browsing.
lcBrTit    = "Regions & States" 
lcBrowBmp  = (GCBmpHome + "ExtKey.BMP")

glFromBrow = .F.

*-- To force the control pannel to call the local save and close
*-- procedures (lpNxtScr ,lpPrvScr ,lpSavScr, lpClsScr ,lpDelScr )
laCtrStat[10] = "DISABLE"

laDefProc[04] = .F.   &&lpPrvScr
laDefProc[07] = .F.   &&lpSavScr
laDefProc[09] = .F.   &&lpClsScr
laDefProc[10] = .F.   &&lpDelScr

*-- The screen keys
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.T.
laKeyField[1,3] = "cregion"
laKeyField[1,4] = 1

IF !GFSETUP()
  RETURN
ENDIF

lcBrFields =  [cregion:20:H="Region",cstate:3:H="State"]

IF !WEXIST(gcBaseWind)
  *-- Screen fields variable, to be used to build the laData array.
  lcScFields = " cregion , cstate , corderno "

  lcWindB    = gfTempName()
  lcWindC    = gfTempName()
  lcWindD    = gfTempName()
  lcWindE    = "GwcContrl1"
  TmpFile    = gfTempName()  
  TmpFile1   = gfTempName()  
  
  SELECT piktkreg
  SET ORDER TO TAG Region 
  =AFIELDS(laFileStru)
  
  *--- To Store Operation Code N --> no operation        E --> Edite Operation 
  *---                         D --> Delete operation    A --> Append Operation   
  
  DIMENSION laFileStru[4 , 4]
  laFileStru [4  ,  1]	= 'COPER'
  laFileStru [4  ,  2]	= 'C'
  laFileStru [4  ,  3]	= 1
  laFileStru [4  ,  4]	= 0


  CREATE TABLE (GcWorkDir+TmpFile) FROM ARRAY laFileStru
  INDEX ON cregion+cState+cOrderNo TAG(TmpFile)
  INDEX ON cState+cOrderNo TAG(TmpFile1)
  SELECT (TmpFile)
  SET ORDER TO TAG &TmpFile
  SCATTER FIELDS &lcScFields TO laData BLANK 
  SELECT (TmpFile) 
ELSE

ENDIF

DO (GcScrDir+"AL\ALREG.SPX")
RELEASE WINDOW (lcBrTit)

*-- Functions and Procedures :
*:***************************************************************************
*: Name       : lpShow
*: Developer  : EHAB ISMAIL HAMED - EIH
*: Date       : 05/09/2005
*: Purpose    : Screen modes controlling procedure.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : DO lpShow
*:***************************************************************************
PROCEDURE lpShow

DO CASE
  *                ---> S E L E C T  M O D E <---
  CASE laScrMode[1]
    llClose = .T.
    STORE '' TO laData[1] , laData[2]  ,laData[3]
    SELECT (TmpFile) 
    ZAP
    =lfBrows()
    SHOW WINDOW (lcBrTit)  REFRESH SAME
    SHOW GET ladata[1]     ENABLE    
    SHOW GET ibReg         ENABLE
    SHOW GET ladata[2]     DISABLE     
    SHOW GET ladata[3]     DISABLE     
    SHOW GET ibState       DISABLE     
    SHOW GET pbBrws        DISABLE     
    
  *******          -->V I E W     M O D E <--
  CASE laScrMode[2]
    llClose = .T.
    SHOW WINDOW (lcBrTit) REFRESH SAME
    SHOW GET pbBrws  DISABLE  
      
  *                ---->E D I T  M O D E <----
  CASE laScrMode[3]
    *STORE '' TO ladata[2] ,ladata[3] 
    SHOW GET ladata[1] DISABLE     
    SHOW GET ladata[2] DISABLE
    SHOW GET ladata[3] DISABLE 
    SHOW GET ibState   DISABLE
         

  *                   ----> A D D  M O D E <----
  CASE laScrMode[4] 
    SELECT (TmpFile) 
    ZAP
    =lfBrows()
    STORE '' TO laData[1] , laData[2]  ,laData[3]
    SHOW GET ladata[2] ENABLE     
    SHOW GET ladata[3] ENABLE     
    SHOW GET ibState   ENABLE     
    _CUROBJ = OBJNUM(ladata[1])
ENDCASE  
*!*************************************************************
*! Name      : lfvReg
*! Developer : Ehab Ismail Hamed
*! Date      : 05/09/2005
*! Purpose   : To validate the Region .
*!*************************************************************
*! Example            : =lfvReg()
*!*************************************************************
FUNCTION lfvReg

IF laScrMode[4]    && ADD MODE
  IF !EMPTY(laData[1]) 
    IF SEEK(laData[1]) 
      lcMsg = 'This region already exists  ' 
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
      STORE '' TO laData[1] , laData[2]  ,laData[3]
      SHOW GETS
      _CUROBJ = OBJNUM(ladata[1])
      RETURN
    ENDIF
    *EIH TESTING IF REGION EXIST IN MASTER FILE
    SELECT piktkreg
    SET ORDER TO TAG REGION
    IF SEEK(laData[1]) 
      lcMsg = 'This region already exists  ' 
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
      STORE '' TO laData[1] , laData[2]  ,laData[3]
      SHOW GETS
      _CUROBJ = OBJNUM(ladata[1])
      SELECT (TmpFile)
      RETURN
    ELSE
      SHOW GET ladata[2] ENABLE     
      SHOW GET ladata[3] ENABLE     
      SHOW GET ibState   ENABLE     
      SHOW GET ladata[1] DISABLE    
      SHOW GET ibReg     DISABLE        
      _CUROBJ = OBJNUM(ladata[2])
    ENDIF
  ELSE
    lcMsg = 'Invalid Region value ' 
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
    _CUROBJ = OBJNUM(ladata[1])
    RETURN
  ENDIF 
ELSE
  IF !llBrowse .AND. EMPTY(ladata[1])
    RETURN
  ENDIF
  
  IF llBrowse OR LEFT(ladata[1],1) = "?"
    *-- To browse all the existing regions
    =lfRegBrow()
    *-- Rebrowse the REGIONS
    =lfBrows()
    llBrowse = .F.
    IF EMPTY(ladata[1])
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
    SELECT piktkreg
    =SEEK(laData[1]) 
    SCAN REST WHILE cregion+cstate+corderno = laData[1]
      SCATTER MEMVAR
      m.COPER = 'N'
      INSERT INTO (TmpFile) FROM MEMVAR
    ENDSCAN
  ELSE
    SELECT (TmpFile)
    IF !SEEK(ladata[1])
      ZAP
      SELECT piktkreg
      IF SEEK(laData[1]) 
        SCAN REST WHILE cregion+cstate+corderno = laData[1]
          SCATTER MEMVAR
          m.COPER = 'N'
          INSERT INTO (TmpFile) FROM MEMVAR
        ENDSCAN
        =lfBrows()
        llBrowse = .F.
        SHOW GET ladata[1] DISABLE    
        SHOW GET ibReg     DISABLE    
        STORE .F. TO laScrMode
        STORE .T. TO laScrMode[2]
        SHOW GETS
        SELECT (TmpFile)
        RETURN
      ENDIF
      *-- To browse all the existing regions
      =lfRegBrow()
      *-- Rebrowse the REGIONS
      =lfBrows()
      llBrowse = .F.
      IF EMPTY(ladata[1])
        _CUROBJ = _CUROBJ
        RETURN
      ENDIF
    ENDIF
    SELECT piktkreg
    =SEEK(laData[1]) 
    SCAN REST WHILE cregion+cstate+corderno = laData[1]
      SCATTER MEMVAR
      m.COPER = 'N'
      INSERT INTO (TmpFile) FROM MEMVAR
    ENDSCAN
    
  ENDIF

  IF !EMPTY(ladata[1]) 
     llBrowse = .F.
  ENDIF
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[2]
  SHOW GETS
ENDIF
*!*************************************************************
*! Name      : lfBrows
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : To brows the selected of Order.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfBrows
PRIVATE lnAlias
lcStatName = ''
lnAlias = SELECT()
lcBrFlds = [cstate:6:H="State", lcStatName=gfCodDes(cState  , 'STATE' ):30:H="State"]
SELECT (TmpFile)
LOCATE
BROWSE FIELDS &lcBrFlds;
       WINDOW    (lcwindC)  ;
       IN WINDOW (GcBaseWind) ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       WHEN lfwBrows()   ; 
       VALID :F lfvBrows();
       TITLE lcBrTit
SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfRegBrow
*! Developer : EHAB ISMAIL HAMED - EIH
*! Date      : 05/05/2005
*! Purpose   : To brows the REGIONS.
*!*************************************************************
*! Example            : =lfRegBrow()
*!*************************************************************
FUNCTION lfRegBrow

SELECT piktkreg
SET ORDER TO Uniqreg
GOTO TOP
IF EOF()
  =gfDialog("I","No records to display")
  ladata[1]  = SPACE(5)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lcBrFields = [cregion:20:H="Region"]
ladata[1] = IIF(ARIABROW('',"Regions",.F.,.F.,.F.,.F.,'','','cregion','laBrowArr'),cregion,SPACE(5))
SELECT piktkreg
SET ORDER TO Region


*!*************************************************************
*! Name      : lfvNew
*! Developer : Ehab Ismail Hamed - EIH
*! Date      : 05/09/2005
*! Purpose   : To validate the New push button.
*!*************************************************************
*! Example            : =lfvNew()
*!*************************************************************
FUNCTION lfvNew
STORE '' TO laData[2], laData[3]
SHOW GET ladata[2] ENABLE
SHOW GET ladata[3] ENABLE 
SHOW GET ibState   ENABLE
SHOW GET pbNew    DISABLE
SHOW GET pbRemove DISABLE
_CUROBJ = OBJNUM(ladata[2])

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Khalid Mohi El-Din
*! Date      : 05/25/1998
*! Purpose   : To validate the Remove push button.
*!*************************************************************
*! Example            : =lfvremove()
*!*************************************************************
FUNCTION lfvRemove
SELECT(TmpFile)
REPLACE COPER WITH 'D'
llCUpDate = .T.
DELETE
GOTO TOP
IF EOF()
  SHOW GET pbRemove DISABLE
ENDIF
_CUROBJ = OBJNUM(ibTabs)
=lfBrows()


*!*************************************************************
*! Name      : lpSavScr
*! Developer : EHAB ISMAIL HAMED - EIH
*! Date      : 05/09/2005
*! Purpose   : local save button procedure
*!*************************************************************
*! Example            : =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
SELECT(TmpFile)
SET DELETE OFF

SCAN FOR COPER <> 'N'
  DO CASE
    CASE &TmpFile..COPER = 'E'   && EDIT Operation
      SELECT piktkreg
      IF SEEK(&TmpFile..CREGION + &TmpFile..CSTATE ) 
        REPLACE piktkreg.CSTATE   WITH &TmpFile..CSTATE , ;
                piktkreg.CORDERNO WITH &TmpFile..CORDERNO
      ENDIF

    CASE &TmpFile..COPER = 'D'            && DELETE Operation
      SELECT piktkreg
      IF SEEK(&TmpFile..CREGION + &TmpFile..CSTATE ) 
        DELETE 
      ENDIF
    
    CASE &TmpFile..COPER = 'A'            && APPEND Operation
      SCATT MEMVAR MEMO
      SELECT piktkreg
      APPEND BLANK
      GATHER MEMVAR MEMO
  ENDCASE
  SELECT(TmpFile)    
ENDSCAN
SELECT(TmpFile)    
REPLACE ALL &TmpFile..COPER WITH 'N'
SET DELETE ON


lascrmode = .F.
lascrmode[2] = .T.

SHOW GETS

*!*************************************************************
*! Name      : lfReadAct
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Example            :  lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTit)
ON KEY LABEL ESC 
*!*************************************************************
*! Name      : lfvDeact
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : The READ Deactivate function of screen NAP500
*!*************************************************************
*! Example            :  lfvDeact()
*!*************************************************************
FUNCTION lfvDeact
ON KEY LABEL ALT+B 
ON KEY LABEL ESC 

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvActBrow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : Valid function for the browsing button (ibGroup)
*!*************************************************************
*! Example            :  lfvActBrow()
*!*************************************************************
FUNCTION lfvActBrow
PARAMETERS lcObjName
llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)


*!*************************************************************
*! Name      : lpDelScr
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 05/09/2005
*! Purpose   : Valid function for the popup lagrp  (ibGroup)
*!*************************************************************
*! Example            :  lpDelScr()
*!*************************************************************
FUNCTION lpDelScr

SELECT (TmpFile)
LOCATE
REPLACE FOR CREGION = ladata[1]  COPER WITH   'D'

DELETE ALL FOR CREGION = ladata[1]  
LOCATE

SELECT piktkreg
REPLACE all FOR CREGION = ladata[1]  CREGION WITH  " "
DELETE ALL FOR CREGION = " "
LOCATE

STORE .F. TO laScrMode
STORE .T. TO laScrMode[1]

SHOW GETS


*:***************************************************************************
*: Name       : lfvBrows
*! Developer  : Ahmed Mohamed_El_Anwar Abd_El_Fatth (AME)   
*! Date       : 30/04/2000
*: Purpose    : "Valid" function of the Dept. browse.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfvBrows()
*:***************************************************************************
FUNCTION lfvBrows

IF WONTOP() # lcBrTit AND glFromBrow
  = gfStopBrow()
ENDIF

*:***************************************************************************
*: Name       : lfwBrows
*! Developer  : Ahmed Mohamed_El_Anwar Abd_El_Fatth (AME)   
*! Date       : 30/04/2000
*: Purpose    : "WHEN" function of the Dept. browse.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfwBrows()
*:***************************************************************************
FUNCTION lfwBrows

IF WEXIST(lcBrTit)
  SHOW WINDOW (lcBrTit) REFRESH SAME
  =lfRefresh(lcBrTit)
  ladata[2]  = &TmpFile..cstate 
  ladata[3]  = &TmpFile..corderno
  SHOW GET ladata[2] DISABLE
  SHOW GET ladata[3] DISABLE
ENDIF


*!*************************************************************
*! Name      : lpNxtScr
*! Developer : EHAB ISMAIL HAMED - EIH
*! Date      : 05/09/2005
*! Purpose   : local Next button procedure
*!*************************************************************
*! Example            : =lpNxtScr()
*!*************************************************************
FUNCTION lpNxtScr
SKIP
IF EOF()
  GO BOTTOM
ENDIF
ladata[1]  = &TmpFile..cregion 
ladata[2]  = &TmpFile..cstate 
ladata[3]  = &TmpFile..corderno


*!*************************************************************
*! Name      : lpPrvScr
*! Developer : EHAB ISMAIL HAMED - EIH
*! Date      : 05/09/2005
*! Purpose   : local Previous button procedure
*!*************************************************************
*! Example            : =lpPrvScr()
*!*************************************************************
FUNCTION lpPrvScr
SKIP -1
IF BOF()
  GO TOP
ENDIF
ladata[1]  = &TmpFile..cregion 
ladata[2]  = &TmpFile..cstate 
ladata[3]  = &TmpFile..corderno

*!*************************************************************************
*! Name      : lfvStat
*! Developer : EHAB ISMAIL HAMED - EIH
*! Date      : 05/09/05    
*! Purpose   : Valid function for editing state
*!*************************************************************************
*! Returns   :  
*:*************************************************************************
FUNCTION lfvStat
IF !llBrowse .AND. EMPTY(ladata[2])
   lcMsg = 'Invalid State value ' 
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  _CUROBJ = OBJNUM(ladata[2])
   RETURN
ENDIF


IF llBrowse OR LEFT(ladata[2],1) = "?"
  *-- To browse all the existing regions
  llBrowse = .T.
  =lfStAtBrow()
  *-- Rebrowse the REGIONS
  llBrowse = .F.
  IF EMPTY(ladata[2])
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
ENDIF


IF !EMPTY(ladata[2])
  IF !USED('CODES')
    = gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
  ENDIF
  SELECT CODES
  LOCATE
  =SEEK('N'+'STATE     '+ladata[2]) 
  IF !FOUND()
    *-- To browse all the existing STATES
    llBrowse = .T.
    =lfStAtBrow()
    *-- Rebrowse the REGIONS
    llBrowse = .F.
    IF EMPTY(ladata[2])
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF
ENDIF  



IF !EMPTY(laData[2])
  SELECT (TmpFile)
  lnCurRec = RECNO()
  SET ORDER TO TAG &TmpFile1
  IF SEEK(laData[2])  
    lcMsg = 'The State  ' +laData[2]+' exist in Region  '+ &TmpFile..cregion 
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
    LOCATE
    laData[2] = ''
    laData[3] = ''
    _CUROBJ = OBJNUM(ladata[2])
    SET ORDER TO TAG &TmpFile
    RETURN
  ENDIF
  SET ORDER TO TAG &TmpFile
  *-- EIH Testin if the region exists in region master file
  SELECT piktkreg
  SET ORDER TO TAG State
  IF SEEK(laData[2])  
    lcMsg = 'The State  ' +laData[2]+' exist in Region  ' + piktkreg.cregion 
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
    LOCATE
    laData[2] = ''
    laData[3] = ''
    _CUROBJ = OBJNUM(ladata[2])
    SET ORDER TO TAG Region 
    SELECT (TmpFile)
    RETURN                     &&End of Test
  ELSE
    SELECT (TmpFile)
    APPEND BLANK
    REPLACE cregion WITH laData[1],;
            cState  WITH laData[2],;
            cOper   WITH 'A'
    lnCurRec = RECNO()
  ENDIF
  SELECT (TmpFile)
  SET ORDER TO TAG &TmpFile
  SHOW GET laData[1] DISABLE
  =lfBrows()
  IF BETWEEN(lnCurRec ,1,RECCOUNT()) 
   	GOTO lnCurRec
  ENDIF
ENDIF

*!*************************************************************************
*! Name      : lfvOrd
*! Developer : EHAB ISMAIL HAMED - EIH
*! Date      : 05/09/05
*! Purpose   : Valid function for editing state
*!*************************************************************************
*! Returns   :  
*:*************************************************************************
FUNCTION lfvOrd

IF !EMPTY(laData[3])
  SELECT (TmpFile)
  lnCurRec = RECNO()
  LOCATE FOR CREGION = laData[1] AND CORDERNO = laData[3]
  IF FOUND()
    lcMsg = 'This order number exist for that region ' 
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
    STORE '' TO laData[3]
    _CUROBJ = OBJNUM(ladata[3])
    IF BETWEEN(lnCurRec ,1,RECCOUNT()) 
     	GOTO lnCurRec
    ENDIF
    RETURN
  ENDIF
  IF BETWEEN(lnCurRec ,1,RECCOUNT()) 
   	GOTO lnCurRec
  ENDIF
  SHOW GET pbNew    ENABLE
  SHOW GET pbRemove ENABLE
  IF cOper <> 'A'
    REPLACE CORDERNO WITH laData[3] ,;
            cOper WITH 'E'          
  ELSE
    REPLACE corderno WITH ladata[3] 
  ENDIF
  _CUROBJ = OBJNUM(pbNew)
ELSE
  lcMsg = 'Invalid printing order value ' 
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  _CUROBJ = OBJNUM(ladata[3])
  RETURN
ENDIF
*!*************************************************************************
*! Name      : lfStAtBrow
*! Developer : Ehab Ismail Hamed
*! Date      : 05/09/05
*! Purpose   : Valid function for editing state
*!*************************************************************************
*! Returns   :  
*:*************************************************************************
FUNCTION lfStAtBrow

lnAlias=SELECT(0)

= gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
SELECT(lnAlias)
IF llbrowse
  SELECT CODES
  LOCATE
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcFile_Ttl = "State"
  lcBrFields = "CCode_No :R :H= 'State'," +;
               "cdiscrep :R :H= 'Name'" 
   lcFilter = "'NSTATE' FOR crltfield = 'N'"
  =gfBrows(lcFilter,'CCode_No','laTemp')
  lcBrFields = lcSavBrFld
  Ladata[2]=LEFT(laTemp[1],3)
ENDIF

SELECT(lnAlias)

*---------------------------   Last Line ------------------------------*


