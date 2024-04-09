*:**********************************************************************************************
*: Program file  : SOPACKS.PRG
*: Program desc. : Screen to enable the user (Cathy Daniels) to fill his custom PACKS file
*:               : with Sales and Production Dates for specific valid years.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 12/10/2000
*:       Refer to: C(102100)
*:**********************************************************************************************
*:Modificatoins:
*:B#604141,1 SSH Allow the user to select one of the deleted packs when generate new one
*:**********************************************************************************************
*-----------------Intiliaze needed variables
*--laScrMode && Screen Modes.
*--laPacks   && Packs Popup.
*--llEdit    && Will the Edit button be enabled?
*--llClose   && Is the button's caption Close or Cancel?
*--lcOrgEsc  && Holds old Escape's ON("KEY")
DIMENSION laScrMode[4],laPacks[1]
EXTERNAL ARRAY laData
laPacks = ' '
*:B#604141,1 SSH Variable declaration
DIMENSION laDeleted[1]
laDeleted = ''
lnNewOrDel = 2
lcQuestion = ""
lnPacks    = 1
*:B#604141,1 SSH [END]
STORE .F. TO laScrMode
laScrMode[1] = .T.
STORE .T. TO llEdit,llClose,llAlowNew
*-----------------Open Files
=lfOpenFiles()
laDefProc[7]  = .F.     && Cancel/Uncancel procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)
llcSave = .F.
IF !gfSetup()
  RETURN
ENDIF

lcScFields = 'cYear,dSale_date,dProd_date,cPack'
SCATTER FIELDS &lcScFields TO laData BLANK
STORE '' TO laUsrFields
lcOrgEsc = ON("KEY","ESCAPE")
ON KEY LABEL ESCAPE DO lpClose
*----------------Call the packs screen.
DO (gcScrDir+"SOPACKS.SPX")

ON KEY LABEL ESCAPE &lcOrgEsc

*!*************************************************************
*! Name      : lfOpenFiles
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Open Files.
*!*************************************************************
FUNCTION lfOpenFiles

=gfOpenFile(gcDataDir+'Fishd',gcDataDir+'Compfyear','SH')
=gfOpenFile(gcDataDir+'Packs',gcDataDir+'Cyear','SH')


*!*************************************************************
*! Name      : lfvYear
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Valid Year field.
*!*************************************************************
FUNCTION lfvYear

IF MDOWN()
  RETURN
ENDIF  

IF laData[1] <> 0 
  laData[1] = STR(laData[1],4)
  *--If the user entered any record for the entered year in the packs file, then it's valid
  IF !SEEK(laData[1],'packs')
    *--If entered year exists in fishd file, then it's valid
    *IF !SEEK(laData[1],'fishd')
    *   = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'You must enter a valid year.')
    *   _CUROBJ = OBJNUM(laData[1])
    *   laData[1] = VAL(laData[1])
    *   RETURN
    *ELSE
      *--Ask him if he wants to add packs for this Year
      *IF gfModalGen('TRM00000B32000',.F.,.F.,.F.,"Year : " + laData[1] + ' does not exist. Do you want to add it?') = 1
      IF  gfModalGen('TRM00000B32000',.F.,.F.,.F.,"There are no packs for this year. Do you want to add packs?") = 1
        lnPacks = 1
        laPacks[lnPacks] = "01"
        STORE .F. TO laScrMode
        laScrMode[4] = .T.
        DO GPCTRLSHOW
        DO lpShow
        RETURN
      ELSE
        _CUROBJ = OBJNUM(laData[1])
        laData[1] = VAL(laData[1])
        RETURN
      ENDIF
    *ENDIF
  ENDIF
  *--Get all packs for the entered year if any.
  SELECT PACKS
  *--Increase speed
  SET ORDER TO
  SELECT CPACK FROM PACKS WHERE cyear+CPACK = laData[1] ORDER BY CPACK INTO ARRAY laPacks
  SET ORDER TO TAG Cyear
  *--If there any pack for the entered year Let Edit button be Enabled.
  llEdit = (ALEN(laPacks,1) > 0 AND !EMPTY(laPacks[1]))
  *--Go through View Mode.
  STORE .F. TO laScrMode
  laScrMode[2] = .T.
  lnPacks = 1
  *--Get the first pack's Information.
  =SEEK(laData[1]+laPacks[1],'Packs')
  laData[2] = Packs.dSALE_DATE
  laData[3] = Packs.dprod_date
  *--b
  DO GPCTRLSHOW
  DO lpShow
ENDIF  

*!*************************************************************
*! Name      : lfvNew
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Valid New push button.
*!*************************************************************
FUNCTION gfvCpNew

*--Has the user entered the 52 records? If yes don't allow adding new recoeds.
SELECT PACKS
*--Set the index DESC to get the last enterd pack.
SET ORDER TO TAG cYear DESC
lnNewOrDel = 2
IF SEEK(laData[1])
  *IF VAL(CPACK) <> 52
  *:B#604141,1 SSH Get the deleted packs
  llDummy = lfGetDel(CPACK)
  IF !EMPTY(laDeleted[1])
    lcQuestion = 'Deleted packs have been detected. Do you want to select one of them?'
    lnNewOrDel = gfModalGen('TRM00000B32000',.F.,.F.,.F.,lcQuestion)
    IF lnNewOrDel = 1
      DIMEN laPacks[ALEN(laDeleted,1)]
      =ACOPY(laDeleted,laPacks)
      lnPacks = ALEN(laPacks,1)
      STORE .F. TO laScrMode
      laScrMode[4] = .T.
      DO GPCTRLSHOW
      DO lpShow
    ENDIF
  ENDIF
  *:B#604141,1 SSH Create new packs if the user select no
  IF lnNewOrDel = 2 .AND. VAL(CPACK) < 52
  *:B#604141,1 SSH [END]
    DIMENSION laPacks[ALEN(laPacks,1)+1]
    lnPacks = ALEN(laPacks,1)
    laPacks[lnPacks] = IIF(SUBSTR(CPACK,1)='0' AND SUBSTR(CPACK,2)<>'9','0'+STR(VAL(CPACK)+1,1),STR(VAL(CPACK)+1,2))
    STORE .F. TO laScrMode
    laScrMode[4] = .T.
    DO GPCTRLSHOW
    DO lpShow
  ELSE
    IF lnNewOrDel = 2
      = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Packs cannot exceed 52.')
      RETURN
    ENDIF
  ENDIF
ELSE
  laPacks[lnPacks] = "01"
  STORE .F. TO laScrMode
  laScrMode[4] = .T.
  DO GPCTRLSHOW
  DO lpShow
ENDIF  
SET ORDER TO TAG cYear ASCE

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Valid Delete push button.
*!*************************************************************
FUNCTION lpDelScr
*-- 'Are you sure you want to delete this record?','\!\<Yes;\?\<No'
SELECT PACKS
DELETE
*--Delete it from the packs array that fills the Packs popup.
=ADEL(laPacks,lnPacks)
lnPackNo = MAX(ALEN(laPacks,1)-1,1)
DIMENSION laPacks[lnPackNo]
*--Popup element no will be either the same as deleting an element just push all elemnts up
*-- so it will take the value, or Decrease by one if we deleted the last one.
lnPacks =MIN(ALEN(laPacks,1),lnPacks)
IF ALEN(laPacks,1) = 1 AND TYPE('laPacks[1]') = 'L'
  laPacks = ' '
  lnPacks = 1
ENDIF
IF !EMPTY(laPacks)
  =SEEK(laData[1]+laPacks[lnPacks],'Packs')
  laData[2] = dSALE_DATE
  laData[3] = dprod_date
  SHOW GET laData[2]
  SHOW GET laData[3]
  SHOW GET lnPacks
  STORE .F. TO laScrMode
  laScrMode[2] = .T.
  DO GPCTRLSHOW
  DO lpShow
ELSE
  *--User deleted the lastb pack for the current year
  STORE .F. TO laScrMode
  laScrMode[1] = .T.
  DO GPCTRLSHOW
  DO lpShow
ENDIF  

*:***************************************************************************
*: Name       : lpShow
*: Developer  : Adel Mohammed El Gazzar (ADEL)
*: Date       : 12/10/2000
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
  *-- S E L E C T  M O D E
  CASE laScrMode[1]
    llClose = .T.
    STORE 0 TO laData[1],lnPacks
    DIMENSION laPacks[1]
    laPacks = ' ' 
    STORE {} TO laData[2],laData[3]
    STORE .T. TO llEdit
    SHOW GET laData[1] ENABLE
    SHOW GET lnPacks   DISABLE
    SHOW GET laData[2] DISABLE
    SHOW GET laData[3] DISABLE
    _CUROBJ = OBJNUM(laData[1])
    SHOW GET pbCpNew DISABLE
  *-- V I E W      M O D E
  CASE laScrMode[2]
    llClose = .T.
    llEdit = (ALEN(laPacks,1) > 0 AND !EMPTY(laPacks[1]))
    SELECT PACKS
    *--Increase speed
    SET ORDER TO
    SELECT CPACK FROM PACKS WHERE cyear+CPACK = laData[1] ORDER BY CPACK INTO ARRAY laPacks
    SET ORDER TO TAG Cyear
    *--If there any pack for the entered year Let Edit button be Enabled.
    llEdit = (ALEN(laPacks,1) > 0 AND !EMPTY(laPacks[1]))
    lnPacks = IIF(lnPacks = 0,1,lnPacks)
    *--Get the first pack's Information.
    laData[1] = Packs.cYear
    laData[2] = Packs.dSALE_DATE
    laData[3] = Packs.dprod_date
    lnPacks   = ASCAN(laPacks,Packs.cPack)
    SHOW GET laData[1] DISABLE
    SHOW GET lnPacks   ENABLE
    SHOW GET laData[2] DISABLE
    SHOW GET laData[3] DISABLE
    SHOW GET pbCpNew ENABLE
  *--E D I T       M O D E
  CASE laScrMode[3]
    llClose = .F.
    SHOW GET lnPacks   DISABLE
    SHOW GET laData[2] ENABLE
    SHOW GET laData[3] ENABLE
    _CUROBJ = OBJNUM(laData[2])
  *-- A D D        M O D E
  CASE laScrMode[4] 
    llClose = .F.
    STORE {} TO laData[2],laData[3]
    IF lnNewOrDel = 2
      SHOW GET lnPacks   DISABLE
    ELSE
      SHOW GET lnPacks   ENABLE
    ENDIF  
    SHOW GET laData[2] ENABLE
    SHOW GET laData[3] ENABLE
    _CUROBJ = OBJNUM(laData[2])
ENDCASE  
SHOW GET pbUsrField DISABLE

*!*************************************************************
*! Name      : lfvPacks
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Valid Packs Popup.
*!*************************************************************
FUNCTION lfvPacks

=SEEK(laData[1]+laPacks[lnPacks],'Packs')
laData[2] = Packs.dSALE_DATE
laData[3] = Packs.dprod_date
SHOW GET laData[2]
SHOW GET laData[3]

*!*************************************************************
*! Name      : lpClose
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Process Escape button.
*!*************************************************************
PROCEDURE lpClose

=gfcpclose()

*!*************************************************************
*! Name      : gfcpclose
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Valid Close\Cancel push button.
*!*************************************************************
FUNCTION gfcpclose

IF llClose
  REPLACE lLok_Stat WITH .F. , ;   
        cLok_User WITH ""  , ;
        dLok_Date WITH {}  , ;
        cLok_Time WITH ""
  CLEAR READ
ELSE
  llChange = (Packs.dSALE_DATE<> laData[2] OR Packs.dprod_date <> laData[3])
  IF llChange OR laScrMode[4]
    IF gfDialog('I','Are you sure you want to cancel and lose all changes.','\<Yes;\<No' )=1)
      laData[2] = Packs.dSALE_DATE
      laData[3] = Packs.dprod_date
      IF laScrMode[4]
        *--Delete it from the packs array that fills thge Packs popup.
        =ADEL(laPacks,lnPacks)
        DIMENSION laPacks[MAX(ALEN(laPacks,1)-1,1)]
        lnPacks =MAX(lnPacks-1,1)
        IF lnPacks  = 1 AND TYPE('laPacks[1]') = 'L'
          laPacks[lnPacks] = ' ' 
        ENDIF
        *--Go to Select Mode.
        STORE .F. TO laScrMode
        laScrMode[1] = .T.
        DO GPCTRLSHOW
        DO lpShow
        RETURN
      ENDIF  
      SHOW GET laData[2]
      SHOW GET laData[3]
      SHOW GET lnPacks
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
      DO GPCTRLSHOW
      DO lpShow
      REPLACE lLok_Stat WITH .F. , ;   
        cLok_User WITH ""  , ;
        dLok_Date WITH {}  , ;
        cLok_Time WITH ""
    ELSE
      _CUROBJ = OBJNUM(laData[2])  
    ENDIF
  ELSE
    STORE .F. TO laScrMode
    laScrMode[2] = .T.
    DO GPCTRLSHOW
    DO lpShow
    REPLACE lLok_Stat WITH .F. , ;   
        cLok_User WITH ""  , ;
        dLok_Date WITH {}  , ;
        cLok_Time WITH ""
  ENDIF      
ENDIF

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Valid Save push button.
*!*************************************************************
FUNCTION lpSavScr

llcSave = .T.
SELECT PACKS
IF laData[2] = {} OR laData[3] = {}
  lcMessage = IIF(laData[2] = {},'Sales date must be entered.','Production date must be entered.')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,lcMessage)
  IF laData[2] = {}
    _CUROBJ = OBJNUM(laData[2])
  ELSE
    _CUROBJ = OBJNUM(laData[3])
  ENDIF  
  llcSave = .F.
  RETURN
ENDIF
*--Validate Sales and Production dates
lcKey = EVAL(KEY())
=SEEK(laData[1])
LOCATE REST WHILE cyear+CPACK = laData[1] FOR (dSALE_DATE = laData[2] AND dprod_date = laData[3])
IF FOUND() AND IIF(laScrMode[3],EVAL(KEY()) <> lcKey,.T.)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Sales date and production date already exist for year : ' + laData[1];
               + '  Pack# : ' + cPack+'.')
  _CUROBJ = OBJNUM(laData[2])
  =SEEK(lcKey)
  llcSave = .F.
  RETURN
ENDIF
=SEEK(laData[1])
LOCATE REST WHILE cyear+CPACK = laData[1] FOR dSALE_DATE = laData[2]
IF FOUND() AND IIF(laScrMode[3],EVAL(KEY()) <> lcKey,.T.)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Sales date already exists for year : ' + laData[1]+'.')
  _CUROBJ = OBJNUM(laData[2])
  =SEEK(lcKey)
  llcSave = .F.
  RETURN 
ENDIF
=SEEK(laData[1])
LOCATE REST WHILE cyear+CPACK = laData[1] FOR dprod_date = laData[3]
IF FOUND() AND IIF(laScrMode[3],EVAL(KEY()) <> lcKey,.T.)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Production date already exists for year: ' + laData[1]+'.')
  _CUROBJ = OBJNUM(laData[3])
  =SEEK(lcKey)
  llcSave = .F.
  RETURN
ENDIF
=SEEK(lcKey)
IF laScrMode[4]  
  APPEND BLANK
ENDIF
REPLACE CPACK      WITH laPacks[lnPacks],;
        dSALE_DATE WITH laData[2],;
        dprod_date WITH laData[3],;
        cYear      WITH laData[1]
REPLACE lLok_Stat WITH .F. , ;   
        cLok_User WITH ""  , ;
        dLok_Date WITH {}  , ;
        cLok_Time WITH ""
        
        
        
STORE .F. TO laScrMode
laScrMode[2] = .T.
DO GPCTRLSHOW
DO lpShow

*!*************************************************************
*! Name      : lfvPackNo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Made fo Cathy Danields
*!*************************************************************
FUNCTION lfvPackNo

*!*************************************************************
*! Name      : lfwCutWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/11/2000
*! Purpose   : Made fo Cathy Danields
*!*************************************************************
FUNCTION lfwCutWhen


*!*************************************************************
*! Name      : lfGetDel
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 12/11/2000
*! Purpose   : Made fo Cathy Danields
*!*************************************************************
*:B#604141,1 SSH Create new packs if the user select no
FUNCTION lfGetDel
PARAMETER lcLastPack

PRIVATE lnOldAls , lcCurr , lnCurr

lnOldAls = SELECT(0)
lcCurr   = ""
lnCurr   = 1
DIMENSION laDeleted[1]
laDeleted[1] = ''
lcToRet = ""
lcNext  = ""
SELECT PACKS
SET ORDER TO Cpack
FOR lnPack = 1 TO VAL(lcLastPack)
  IF !SEEK(PADL(ALLTRIM(STR(lnPack)),2,"0")+laData[1])
    laDeleted[lnCurr] = PADL(ALLTRIM(STR(lnPack)),2,"0")
    lnCurr = lnCurr + 1
    DIMENSION laDeleted[lnCurr]
    laDeleted[lnCurr] = ""
  ENDIF
ENDFOR
IF lnCurr<>1 .AND. laDeleted[lnCurr] = ""
  DIMENSION laDeleted[lnCurr-1]
ENDIF
SELECT PACKS
SET ORDER TO TAG cYear DESC
SELECT(lnOldAls)
*:B#604141,1 SSH Create new packs if the user select no