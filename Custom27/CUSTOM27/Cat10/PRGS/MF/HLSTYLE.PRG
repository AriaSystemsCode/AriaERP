*:****************************************************************
*: Program file  : HLSTYLE.PRG
*: Program desc. : Add,Change,Delete and Inquire Styles.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 12/18/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lpShow     , lfvData_1 , lfvData_3
*:               :            : lfvData_14 , lfvChfwt  , lfvConst
*:               :            : lfvGrmnt   , lfvCutAt  , lfvSewAt
*:               :            : lfvImport  , lfvCvs    , lfvPicture
*:               :            : lfvCost    , lfvDoz    , lfvCancel
*:               :            : lfvData_40 , lfOpenFls , lfTmp2Mast
*:               -----------------------------------------------
*:               : PROCEDURE  : lpSavScr   , lpDelScr  , lfShow
*:               :            : lpClose
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lnRecNo    :- Variable to hold the Record No.
*-- lnLinNo    :- Variable To hOld the Line No.
*-- lnFcount   :- Variable To hOld the File Count
*-- llNewPic   :- Variable Hold True if there is a new picture select
*-- laChfWtCod :- Chief Weight Code.
*-- laConstCod :- Construction Code.
*-- laGrmntCod :- Garment Description Code.
*-- laCutAtCod :- Cut At Code.
*-- laSewAtCod :- Sew At Code.
*-- lcImgDir   :- The Aria Application system Image Directory.
*-- lcAASDir   :- The Aria Application system Data Directory.
*-- llCodes    :- Flage Hold True In case we open the codes file from AAS.
*-- lcCodes    :- Variable Hold the temp name for codes file.
*-- llStyle    :- Flage Hold True In case we open the Style file from AAS.
*-- llFABRIC   :- Flage Hold True In case we open the FABRIC file from AAS.
*-- llCuttkth  :- Flage Hold True In case we open the Cuttkth file from AAS.
*-- llCTKTBOM  :- Flage Hold True In case we open the CTKTBOM file from AAS.
*-- llBom      :- Flage Hold True In case we open the BOM file from AAS.
*-- llAPVENDOR :- Flage Hold True In case we open the VENDOR file from AAS.
*-- llOBJECTID :- Flage Hold True In case we open the OBJECTID file from AAS.
*-- llOBJLINK  :- Flage Hold True In case we open the OBJLINK file from AAS.
*-- End Declaration variables.

EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode


*-- Array to hold the description & codes for.
DECLARE laCodes[5,10]   , laChfWtCod[1,2] , laConstCod[1,2] ,;
        laGrmntCod[1,2] , laCutAtCod[1,2] , laSewAtCod[1,2] ,;
        laData[1]       , laKeyField [1,4]


STORE ''  TO lc_Temp   ,lcIndExp   ,lcPicName ,lcLastPic,lcNewPic,lcTempCur,;
             laCodes   ,laChfWtCod ,laConstCod,laGrmntCod ,;
             laCutAtCod,laSewAtCod ,lcScFields , lcModal  ,;
             lcImgDir  , lcAASDir  , lcCodes

STORE 0   TO lnLinNo ,lnFcount ,lnMissy  ,lnlarge  ,rbMisLrg 
STORE .F. TO llFirst , llUpdCvs,llPic    ,llNewPic ,cbPrntCut ,;
          llCodes    , llStyle ,llFABRIC   , llCuttkth,llCTKTBOM,;
          llBom      , llAPVENDOR , llOBJECTID, llOBJLINK

STORE 1   TO lnRecNo , lnChfWtCod , lnConstCod , lnGrmntCod,;
             lnCutAtCod,lnSewAtCod

laDefProc[7]  = .F.                       && Use custom Delete procedure  
laDefProc[9]  = .F.                       && Use custom Save procedure

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .T.
laKeyField[1,3] = 'CSTYNUM'
laKeyField[1,4] = 1

IF !gfSetup()
  RETURN
ENDIF  
lcCodes   = gfTempName()
lc_Temp   = gfTempName()
lcTempCur = gfTempName()

lcBrFields = [cstynum:H="Style number ", cstyseacd:H="Style season code", cstyrang:H="Style range",cstrangds:H="Range description"]

SELECT hlsetup

*-- Assing the Path of AAS Directory to lcImgDir.
lcImgDir = ALLTRIM(hlsetup.cSetImgDr)+;
           IIF(RIGHT(ALLTRIM(hlsetup.cSetImgDr),1)<>'\','\','')

*-- Assing the Path of AAS Directory to lcAAsDir.
lcAASDir  = ALLTRIM(hlsetup.cSetArDir)   && Store the AAS directory


IF !WEXIST(gcBaseWind)
  *-- Codes For Chief Weight Code , Construction Code & Garment Description Code.
  laCodes[1,1]  = 'CTSUCHWCD'
  laCodes[1,2]  = 'laChfWtCod'
  laCodes[1,3]  = 'lnChfWtCod'
  laCodes[1,4]  = ''
  laCodes[1,5]  = .F.
  laCodes[1,6]  = .F.
  laCodes[1,10] = 'CTSUCHWCD'
 
  laCodes[2,1]  = 'CTSUCONCD'
  laCodes[2,2]  = 'laConstCod'
  laCodes[2,3]  = 'lnConstCod'
  laCodes[2,4]  = ''
  laCodes[2,5]  = .F.
  laCodes[2,6]  = .F.
  laCodes[2,10] = 'CTSUCONCD'

  laCodes[3,1]  = 'CTSUGARCD'
  laCodes[3,2]  = 'laGrmntCod'
  laCodes[3,3]  = 'lnGrmntCod'
  laCodes[3,4]  = ''
  laCodes[3,5]  = .F.
  laCodes[3,6]  = .F.
  laCodes[3,10] = 'CTSUGARCD'

  laCodes[4,1]  = 'CSTYCUTAT'
  laCodes[4,2]  = 'laCutAtCod'
  laCodes[4,3]  = 'lnCutAtCod'
  laCodes[4,4]  = ''
  laCodes[4,5]  = .F.
  laCodes[4,6]  = .F.
  laCodes[4,10] = 'CSTYCUTAT'

  laCodes[5,1]  = 'CSTYSEWAT'
  laCodes[5,2]  = 'laSewAtCod'
  laCodes[5,3]  = 'lnSewAtCod'
  laCodes[5,4]  = ''
  laCodes[5,5]  = .F.
  laCodes[5,6]  = .F.
  laCodes[5,10] = 'CSTYSEWAT'

  lcScFields = 'CSTYNUM   , CSTYRANG  , CSTYMSLRG , MSTYDESC  , CSTYFABCN ,'+;
               'CTSUCHWCD , CTSUCONCD , CTSUGARCD , NTSUDTRAT , CTSUQTCAT ,'+;
               'CTSUSUFIX , NSTYCUTCO , CSTYCUTAT , LSTYDT807 , NSTYSEWCO ,'+;
               'NSTY_OQC  , NSETLBFRT , CSTYMRKNO , NSTYFABCO , NSTYFABYA ,'+;
               'NSETINFRT , NSTYFRTNO , NSETFRTSO , CSTYSEACD , CSTRANGDS ,'+;
               'NSTYTOTUS , NSTYTDUTI , NSTYTOT   , MSTYDESCB , NSTYNETDD ,'+;
               'NSTYFABCB , NSTYFABCC , NSTYFABCD , NSTYFABYB , NSTYFABYC ,'+;
               'NSTYFABYD , NSTYPROF  , CSTYSEWAT , NSTYMINUT , lPrintCut'
               
  SELECT HLCSTYLE
  SCATTER FIELDS &lcScFields Memo TO laData BLANK
ENDIF

PUSH KEY
DO (gcScrDir+gcAct_Appl+"\Hlstyle.SPX")
POP KEY


IF glQuitting 

  IF USED('&lc_Temp')
    USE IN &lc_Temp
    ERASE &gcWorkDir.&lc_Temp+'.DBF'
    ERASE &gcWorkDir.&lc_Temp+'.CDX'
    ERASE &gcWorkDir.&lc_Temp+'.FPT'
  ENDIF  

  *-- Close the CODE File.
  IF llCodes
    = gfCloseFile('CODES')
  ENDIF
  
  *-- Close the STYLE File.
  IF llStyle
    = gfCloseFile('STYLE')
  ENDIF

  *-- Close the FABRIC File.
  IF llFABRIC
    = gfCloseFile('FABRIC')
  ENDIF
  
  *-- Close the CUTTKTH File.
  IF llCuttkth
    = gfCloseFile('CUTTKTH')
  ENDIF

  *-- Close the CTKTBOM File.
  IF llCTKTBOM
    = gfCloseFile('CTKTBOM')
  ENDIF

  *-- Close the BOM File.
  IF llBom
    = gfCloseFile('BOM')
  ENDIF   

  *-- Close the VENDOR File
  IF llAPVENDOR
    = gfCloseFile('APVENDOR')
  ENDIF   
  
  *-- Close the OBJECTID File
  IF llOBJECTID
    = gfCloseFile('OBJECTID')
  ENDIF
  
  *-- Close the OBJLINK File
  IF llOBJLINK
    = gfCloseFile('OBJLINK') 
  ENDIF
  
  
ENDIF
RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/19/2001
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters:  None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpShow
*:*************************************************************
*:
PROCEDURE lpShow


EXTERNAL ARRAY laScrMode

rbMisLrg = AT(laData[3],"ML")

rbCut807 = IIF(laData[14],1,2)

cbPrntCut= laData[40]

DO CASE 

  ***--- S E L E C T   M O D E ---***    
  CASE laScrMode[1]
    IF USED(lcTempCur) 
      SELECT (lcTempCur)
      ZAP
    ENDIF
    *-- Chief Weight Code.
    STORE 1 To lnChfWtCod , lnConstCod , lnGrmntCod,;
               lnCutAtCod,lnSewAtCod 

    lnChfwtCod = ASCAN(laChfwtCod,laData[06])/2
    lnConstCod = ASCAN(laConstCod,laData[07])/2
    lnGrmntCod = ASCAN(laGrmntCod,laData[08])/2
    lnCutAtCod = ASCAN(laCutAtCod,laData[13])/2
    lnSewAtCod = ASCAN(laSewAtCod,laData[38])/2
               
    SHOW GET ladata[1]     ENABLE
    _CUROBJ = OBJNUM(laData[1])
    
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]

    *-- Chief Weight Code.
    SELECT HLCSTYLE
    =gfwCodePop(@laCodes,'CTSUCHWCD','N')
    =gfwCodePop(@laCodes,'CTSUCHWCD','L')

    *-- Construction Code.
    =gfwCodePop(@laCodes,'CTSUCONCD','N')
    =gfwCodePop(@laCodes,'CTSUCONCD','L')
    

    *-- Garment Description Code.
    =gfwCodePop(@laCodes,'CTSUGARCD','N')
    =gfwCodePop(@laCodes,'CTSUGARCD','L')

    *-- Cut At Code.
    =gfwCodePop(@laCodes,'CSTYCUTAT','N')
    =gfwCodePop(@laCodes,'CSTYCUTAT','L')

    *-- Sew At Code.
    =gfwCodePop(@laCodes,'CSTYSEWAT','N')
    =gfwCodePop(@laCodes,'CSTYSEWAT','L')

    lnChfwtCod = ASCAN(laChfwtCod,laData[06])/2
    lnConstCod = ASCAN(laConstCod,laData[07])/2
    lnGrmntCod = ASCAN(laGrmntCod,laData[08])/2
    lnCutAtCod = ASCAN(laCutAtCod,laData[13])/2
    lnSewAtCod = ASCAN(laSewAtCod,laData[38])/2
                 
                 
    lcPicName=""             
    llNewPic = .F.
    SHOW GET pbCvs     ENABLE
    SHOW GET pbPicture ENABLE
    SHOW GET pbYard    ENABLE
    SHOW GET pbDoz     ENABLE

  ***--- E D I T   M O D E ---***    
  CASE laScrMode[3]
    SHOW GET laData[1] DISABLE


  ***--- A D D   M O D E ---***
  CASE laScrMode[4]
   *- Abdou
   STORE 1 To lnChfWtCod , lnConstCod , lnGrmntCod,;
              lnCutAtCod,lnSewAtCod 

   
    *-- New name hlsetup ==> hlsetup File.
    laData[12] = HLSETUP.NSETCTDFC
    laData[21] = HLSETUP.NSETINFRT
    laData[23] = HLSETUP.NSETFRTSO
    laData[17] = HLSETUP.nSetlbFrt

    
    SHOW GET laData[12]
    SHOW GET laData[21]
    SHOW GET laData[23]
    SHOW GET laData[01] DISABLE
ENDCASE


IF EMPTY(laChfWtCod)
  SHOW GET puChfWt DISABLE
ENDIF  

IF EMPTY(laConstCod)
  SHOW GET puConst DISABLE
ENDIF

IF EMPTY(laGrmntCod)
  SHOW GET puGrmnt DISABLE
ENDIF

IF EMPTY(laCutAtCod)  
  SHOW GET puCutAt DISABLE
ENDIF

IF EMPTY(laSewAtCod)  
  SHOW GET puSewAt DISABLE
ENDIF
  
IF laScrMode[1]
  SHOW GET pbZoom DISABLE
ELSE
  SHOW GET pbZoom ENABLE
ENDIF

SHOW GET laData[22]

*-- End Of lpShow.
*:*************************************************************
*: Name      : lfvData_1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate laData[1]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_1()
*:*************************************************************
*:
FUNCTION lfvData_1

IF !EMPTY(ALLTRIM(laData[1]))
  laData[1] = IIF(laData[1] = '?','',laData[1])
  LOCATE
  =gfSeekRec()
ENDIF

llPic  = .F.
llFirst=.T.

*-- End Of lfvData_1.
*:*************************************************************
*: Name      : lfvData_3
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate   Missy/Large.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_3()
*:*************************************************************
*:
FUNCTION lfvData_3
PRIVATE  lnAlias

lnAlias = SELECT (0)
IF laScrMode[3]          && Edit Mode
  SELECT HLCTSUSA
  IF SEEK (laData[6]+laData[7]+laData[8])
    lnMissy   = HLCTSUSA.nTsufrtnm
    lnlarge   = HLCTSUSA.nTsufrtnl
  ELSE
    lnMissy   = 0
    lnlarge   = 0
  ENDIF
ENDIF    

SELECT hlcStyle

laData[3] = SUBSTR("ML",rbMisLrg,1)
laData[22]= IIF(rbMisLrg = 0,0,IIF(laData[3]="M",lnMissy,lnlarge))

SHOW GET laData[22]

SELECT (lnAlias)

*-- End OF lfvData_3.
*:*************************************************************
*: Name      : lfvData_14
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate  Cutting Dutiable/807 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_14()
*:*************************************************************
*:
FUNCTION lfvData_14
 
laData[14] = (rbCut807 = 1)

*-- End OF lfvData_14.
*:*************************************************************
*: Name      : lfvChfwt
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate Chief Weight Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvChfwt()
*:*************************************************************
*:
FUNCTION lfvChfwt

IF lnChfWtCod <> 0 .AND. !( PADR(ALLTRIM(laData[6]),6) == PADR(ALLTRIM(laChfWtCod[lnChfWtCod,2]),6))
  laData[6] = PADR(ALLTRIM(laChfWtCod[lnChfWtCod,2]),6)
ENDIF

STORE 0 TO laData[22] , lnMissy , lnLarge
llupdCvs   = .F.
SHOW GET laData[22]

*-- End OF lfvChfwt
*:*************************************************************
*: Name      : lfvConst
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate Construction Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvConst()
*:*************************************************************
*:
FUNCTION lfvConst

IF lnConstCod <> 0 .AND. !( PADR(ALLTRIM(laData[7]),6) == PADR(ALLTRIM(laConstCod[lnConstCod,2]),6))
  laData[7] = PADR(ALLTRIM(laConstCod[lnConstCod,2]),6)
ENDIF


STORE 0 TO laData[22] , lnMissy , lnLarge
llupdCvs   = .F.
SHOW GET laData[22]

SHOW GET laData[22] 

*-- End OF lfvConst.
*:*************************************************************
*: Name      : lfvGrmnt
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate Garment Description Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvGrmnt()
*:*************************************************************
*:
FUNCTION lfvGrmnt
PRIVATE lnAlias

lnAlias = SELECT (0)

IF  lnGrmntCod = 0 
  RETURN
ENDIF

IF  !( PADR(ALLTRIM(laData[8]),6) == PADR(ALLTRIM(laGrmntCod[lnGrmntCod,2]),6))
  laData[8] = PADR(ALLTRIM(laGrmntCod[lnGrmntCod,2]),6)
ENDIF


IF !EMPTY(laData[8])
  SELECT HLCTSUSA
  IF SEEK (laData[6]+laData[7]+laData[8])
    laData[09] = HLCTSUSA.NTSUDTRAT
    laData[10] = HLCTSUSA.CTSUQTCAT
    laData[11] = HLCTSUSA.CTSUSUFIX
    lnMissy    = HLCTSUSA.nTsufrtnm
    lnlarge    = HLCTSUSA.nTsufrtnl
    laData[22] = IIF(rbMisLrg = 0,0,IIF(laData[3]="M",lnMissy,lnlarge))
    SHOW GET laData[09]
    SHOW GET laData[10]
    SHOW GET laData[11]
    SHOW GET laData[22]
    SHOW GET rbMisLrg ENABLE
  ELSE
    *- Message Text   :- Codes not found in the Code File.  Cannot import any data for this codes.
    *- Message No.    :- 38216.
    *- Buttom Message :- \!\<Retry;\?\<Cancel
    *- Buttom Number  :- 36005.
    IF gfModalGen('INM38216B36005','DIALOG') = 1

      *-- Chief Weight Code.
      =gfwCodePop(@laCodes,'CTSUCHWCD','N')
      
      *-- Construction Code.
      =gfwCodePop(@laCodes,'CTSUCONCD','N')

      *-- Garment Description Code.
      =gfwCodePop(@laCodes,'CTSUGARCD','N')

      
      STORE SPACE(6) TO laData[6],laData[7],laData[8]
      SHOW GET lnChfWtCod
      SHOW GET lnConstCod
      SHOW GET lnGrmntCod

      _CUROBJ=OBJNUM(lnChfWtCod)
      SELECT (lnAlias)
      RETURN .F.
    ELSE
      laData[09] = 0.000
      laData[10] = ' '
      laData[11] = ' '
      lnMissy    = 0
      lnlarge    = 0
      laData[22] = 0
      SHOW GET laData[9]
      SHOW GET laData[10]
      SHOW GET laData[11]
      SHOW GET laData[22]
    ENDIF 
  ENDIF
  
  _CUROBJ=OBJNUM(lnCutAtCod)
  SHOW GET lnCutAtCod
ENDIF

llupdCvs = .F.

SELECT (lnAlias)

*-- End OF lfvGrmnt.
*:*************************************************************
*: Name      : lfvCutAt
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate Cut At Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCutAt()
*:*************************************************************
*:
FUNCTION lfvCutAt

IF lnCutAtCod <> 0 .AND. !( PADR(ALLTRIM(laData[13]),6) == PADR(ALLTRIM(laCutAtCod[lnCutAtCod,2]),6))
  laData[13] = PADR(ALLTRIM(laCutAtCod[lnCutAtCod,2]),6)
ENDIF


*-- End Of lfvCutAt
*:*************************************************************
*: Name      : lfvSewAt
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate Sew At Code.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvSewAt()
*:*************************************************************
*:
FUNCTION lfvSewAt

IF lnSewAtCod <> 0 .AND. !( PADR(ALLTRIM(laData[38]),6) == PADR(ALLTRIM(laSewAtCod[lnSewAtCod,2]),6))
  laData[38] = PADR(ALLTRIM(laSewAtCod[lnSewAtCod,2]),6)
ENDIF

*-- End OF lfvSewAt
*:*************************************************************
*: Name      : lfvImport
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Import style from AAS system to HLC system.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvImport ()
*:*************************************************************
*:
FUNCTION lfvImport
PRIVATE lnAlias

lnAlias = SELECT (0)


lcCuttkt  = " "

*- Message Text   :- Are you sure you want to import data from Aria27 system ?
*- Message No.    :- 38230.
*- Buttom Message :- \<Yes;\<No
*- Buttom Number  :- 38006.
IF gfModalGen('INM38230B38006','DIALOG') = 2
  SHOW GET pbImport DISABLE
  _CUROBJ=OBJNUM(laData[24])
  RETURN .F.
ELSE
  IF !lfOpenFls('IMPORT')
    RETURN .F.
  ENDIF

  IF EMPTY(laData[24])
    lcTempFile = gfTempName()
    CREATE TABLE (lcTempFile) (ccode_no C(6) , cdiscrep C(30) ,crltd_vlu C(50))

    SELECT (lcCodes)
    lcOldOrder = ORDER()
    SET ORDER TO Ccode_no
    = SEEK('N'+'SEASON')
    SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'SEASON';
      FOR EMPTY(crltd_nam) .OR. ALLTRIM(crltd_nam) = 'CSEALNAME'
      IF ALLTRIM(crltd_nam) = 'CSEALNAME'
        SELECT (lcTempFile)
        APPEND BLANK
        REPLACE ccode_no  WITH &lcCodes..ccode_no ,;
                crltd_vlu WITH &lcCodes..crltd_vlu
      ELSE
        SELECT (lcTempFile)
        REPLACE cdiscrep  WITH &lcCodes..cdiscrep
      ENDIF
      SELECT (lcCodes)
    ENDSCAN     
    *-- Return to old index.
    SELECT CODES
    SET ORDER TO &lcOldOrder
    SELECT (lcTempFile)      
    LOCATE
    DIMENSION laTemp[2]
    lcSavBrow  = lcBrFields
    lcBrFields = "ccode_no:H='Codes',cdiscrep:H='Description',cRltd_vlu:H='Season Long Description'"
    = gfbrows(.F.,'cCode_no,cRltd_vlu','laTemp','Season Code')
    lcBrFields = lcSavBrow
    IF !EMPTY(laTemp[1])
      laData[24]= PADR(laTemp[1],3)
    ENDIF  
    SHOW GET laData[24]
    _CUROBJ=OBJNUM(rbMisLrg)  
  ELSE
    IF !EMPTY(laData[24])  
      laData[24]=UPPER(ALLTRIM(LEFT(laData[24],3)))
    ELSE  
      *- Message Text   :- Season xxx not Found in the Aria27 Code File.
      *- Message Text   :- Cannot Import any data
      *- Message No.    :- 38232.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      = gfModalGen('INM38232B00000','ALERT','Season '+laData[24])
      SELECT hlcStyle
      laData[24]=" "
      _CUROBJ=OBJNUM(laData[24])
      RETURN .F.
    ENDIF  
  ENDIF  

  IF USED('&lcTempFile')
    USE IN &lcTempFile
    ERASE &gcWorkDir.&lcTempFile+'.DBF'
    ERASE &gcWorkDir.&lcTempFile+'.CDX'
    ERASE &gcWorkDir.&lcTempFile+'.FPT'
  ENDIF  

  *-- Tacking the first 3 characters and check in the codes file.
  SELECT (lcCodes)
  lcOldOrder = ORDER ()
  SET ORDER TO Ccode_no
  
  IF SEEK ('N'+'SEASON    '+laData[24])
    *-- Replace Style Range with CODE.cData ***
    LOCATE WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'SEASON    '+laData[24] ;
      FOR !EMPTY (cdiscrep)
    ladata[2] = &lcCodes..cdiscrep
    SET ORDER TO &lcOldOrder
    *-- Replace Range Discription with CODE.SEALNAME.
    IF TYPE('laTemp') = 'U'
     SELECT (lcCodes)
     lcOldOrder = ORDER()
     SET ORDER TO Ccode_no
     *-- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
     *-- We will add Space 30 for description and 3 also for the rest of the season.
      = SEEK('N'+'SEASON    '+laData[24]+SPACE(33)+'CSEALNAME ')
      laData[25] = ALLTRIM(&lcCodes..crltd_vlu)
      SET ORDER TO lcOldOrder      
    ELSE
      laData[25] = laTemp[2]    
    ENDIF

    
    SELECT STYLE
    lcStlDiv = " "
    *-- Seek in the STYLE file with UESR ENTERED SEASON CODE ***
    *-- +UESR ENTERED STYLE NUMBER ***
    IF SEEK (laData[24]+'-'+laData[1])
      llPic = .T.
      *-- Replace STYLE DISC WITH SDESC1,SDESC2,SDESC3,SDESC4,SDESC5.
      laData[4] =   Style.msalesdesc

      *-- Get the Picture for the Import style.
      lcPicName = lcImgDir+ALLTRIM(laData[24])+;
                  '\'+ALLTRIM(laData[24])+'.BMP'
     = lfvAAS ()
      *-- Get the Divisioin from the Import style.
      lcStlDiv  = STYLE.CDIVISION
   
      IF lcStlDiv = hlsetup.CSETASDVM
        laData[3] = "M"
        rbMisLrg  = 1
      ELSE 
        IF lcStlDiv = hlsetup.CSETASDVL
          laData[3] = "L"
          rbMisLrg  = 2
        ELSE 
          laData[3] = " "
          laData[22]= 0
        ENDIF  
      ENDIF  
      SHOW GET rbMisLrg 
      SHOW GET laData[22]


      
      *-- In the Old Code , we was use the item File but Now we will use
      *-- The Fabric File.
      SELECT Fabric
     
      *-- Seek in ITEM file in the Old Code and we now will seek in the fabric file.
      IF !EMPTY(STYLE.Fabric) .AND. SEEK(STYLE.Fabric) .AND. !EMPTY(Fabric.Content)
        *-- Replace Fabric Content with ITEM.Content.
        laData[5]=ALLTRIM(Fabric.Content) 
      ENDIF

      SELECT CUTTKTH
      *-- Seek in CUTTKTH file with the season code and style number.
      IF SEEK (laData[24]+'-'+laData[1])
        *--Store the CUTTKT in lcCuttkt.
        lcCuttkt=ALLTRIM(CUTTKTH.CUTTKT)  
        SELECT CTKTBOM
        *-- Seek in CTKTBOM file with lcCuttkt+"1".
        
        *-- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
        IF SEEK ('M'+lcCuttkt+'1')
          LOCATE REST WHILE (cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot) = ('M'+lcCuttkt+'1');
                      FOR "SELF" $ DESC

          IF FOUND()
            *-- Replace Marker Number with CTKTBOM.Marker.
            laData[18] = ALLTRIM(cTktBom.cMarker)
          ELSE
            *-- If you are not able to find the MARKER NUMBER from
            *-- CTKTBOM file then we have to look in the BOM file.
            
            SELECT BOM
            *-- Seek in CUTTKTH file with the season code and style number.
            = SEEK (PADR(laData[24]+'-'+laData[1],19)+'1')
            IF FOUND()
              LOCATE REST WHILE (citmmajor+typ+citmmask+mfgcode+item+iclr)=;
                         (PADR(laData[24]+'-'+laData[1],19) + '1');
                     FOR "SELF" $ DESC   
                IF FOUND()
                  *-- Replace Marker Number with BOM.Marker.
                  laData[18]=ALLTRIM(BOM.cMarker)
                ENDIF
            ENDIF 
          ENDIF
        ENDIF
      ENDIF
      laData[20]= 0
      laData[34]= 0
      laData[35]= 0
      laData[36]= 0
      lnCount   = 0

     *-- Scan for USER ENTERED SEASON+USER ENTERED STYLE+"1".
     SELECT BOM
     *-- citmmajor+typ+citmmask+mfgcode+item+iclr
      =  SEEK (PADR(laData[24]+'-'+laData[1],19)+'1')
      SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr =;
                     PADR(laData[24]+'-'+laData[1],19) + '1'.AND. lnCount < 4
        *-- Replace Fabric Yards/Doz with BOM.TOTQTY.
        lcArrElm="laData["+ALLTRIM(STR(20+;
                            IIF(lnCount=0,0,13+lnCount)))+']'
        &lcArrElm = BOM.NBOMTOTQTY
        lnCount = lnCount + 1
      ENDSCAN
    ELSE
      *- Message Text   :- Style xxx not Found in the Aria27 Code File.
      *- Message Text   :- Cannot Import any data.
      *- Message No.    :- 38232.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      = gfModalGen('INM38232B00000','ALERT','Style '+laData[24]+'/'+laData[1])
      STORE '' TO laData[2] , laData[25] , laData[24]
      SELECT hlcStyle
      _CUROBJ=OBJNUM(laData[24])
      RETURN .F.
    ENDIF  
  ELSE
    SET ORDER TO &lcOldOrder
    *- Message Text   :- Season xxx not Found in the Aria27 Code File.
    *- Message Text   :- Cannot Import any data
    *- Message No.    :- 38232.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM38232B00000','ALERT','Season '+laData[24])
    SELECT hlcStyle
    laData[24]=" "
    _CUROBJ=OBJNUM(laData[24])
    RETURN .F.
  ENDIF    
ENDIF 

SHOW GET laData[2]
SHOW GET laData[4]
SHOW GET laData[5]
SHOW GET laData[18]
SHOW GET laData[20]
SHOW GET laData[24]
SHOW GET laData[25]

_CUROBJ = OBJNUM(rbMisLrg)

SELECT hlcStyle

SELECT (lnAlias)

*-- End OF lfvImport.
*:*************************************************************
*: Name      : lfvCvs
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCvs ()
*:*************************************************************
*:
FUNCTION lfvCvs
PRIVATE lnAlias

lnAlias = SELECT (0)
IF !lfOpenFls('CVS')
  SELECT (lnAlias)
  RETURN .F.
ENDIF

IF llFirst .OR. laScrMode[2] .OR. !llUpdCvs
  SELECT HLCVSLIN  
  SET ORDER TO TAG Cstynum
  lcIndExp=SYS(14,VAL(SYS(21)))
  lcTagExp= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')
 
  SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus',SPACE(49) AS 'Item_Key';
    FROM &gcDataDir.HLCVSLIN;
   WHERE ALLTRIM(HLCVSLIN.CSTYNUM) = ALLTRIM(laData[1]);
    INTO DBF &gcWorkDir.&lc_Temp;
   ORDER BY CSTYNUM
 
  SELECT (lc_Temp)
  INDEX ON &lcIndExp.&lcTagExp
  INDEX ON Item_Key  TAg Item_Key ADDITIVE
  SET ORDER TO Cstynum
  COUNT FOR CSTYNUM <> CHR(255) .AND. !DELETED() TO lnLinNo
ELSE
  SELECT (lc_Temp)
  COUNT FOR CSTYNUM <> CHR(255) .AND. !DELETED() TO lnLinNo
ENDIF


*-- Seperate Program.
DO (gcAppHome +gcAct_Appl+"\hlChdln.PRG")
llFirst = .F.

SELECT (lnAlias)

*-- End Of lfvCvs.
*:*************************************************************
*: Name      : lfvPicture
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to get the Picture
*:*************************************************************
*: Calls     : Program "HLGETPIC"
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvPicture()
*:*************************************************************
*:
FUNCTION lfvPicture

DO (gcAppHome +gcAct_Appl+"\HLGETPIC.PRG")

*-- End OF lfvPicture.
*:*************************************************************
*: Name      : lpSavScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Save The ladata into Base file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : Do lpSavScr
*:*************************************************************
*:
* This procedure replaces the default Save procedure for the push button
* "SAVE"
PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode    

*-- This code to delete the old recored.
IF USED(lcTempCur) .AND. !EOF(lcTempCur)
  SELECT  (lcTempCur)
  IF !EMPTY(gpic_sty)
    IF laScrMode[3]
      SELECT hlcStyle
      DELETE 
    ENDIF
    SELECT hlcStyle
    APPEND FROM (gcWorkDir+lcTempCur)
  ENDIF  
  SELECT hlcStyle
ELSE  
  SELECT hlcStyle
  IF laScrMode[4]   && Add mode
     APPEND BLANK
  ENDIF
ENDIF

SELECT hlcStyle
GATHER FROM laData FIELDS &lcScFields MEMO


=gfAdd_Info()

IF llUpdCvs
  =lFTMP2MAST('HLCVSLIN',lc_Temp)
  llUpdCvs = .F.
ENDIF

SELECT hlcStyle

*-- End Of lpSavScr.
*:*************************************************************
*: Name      : lpDelScr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : delete The ladata into Base file.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : Do lpDelScr
*:*************************************************************
*:
* This procedure replaces the default Delete procedure for the push button
* "DELETE"
PROCEDURE lpDelScr

EXTERNAL ARRAY laScrMode

SELECT HLCVSLIN

*-- Delete all Style Number for this Style
DELETE FOR cStyNum = laData[1]

*-- Delete the user record from the user file
SELECT HLCSTYLE
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
GATHER FROM laData FIELDS &lcScFields MEMO 
DELETE
laScrMode    = .F.
laScrMode[1] = .T.


*-- End OF lpDelScr.
*:*************************************************************
*: Name      : lfvCost
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to get the Fabric Cost / Yard...
*:*************************************************************
*: Calls     : Screen "HlFabCo" .
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCost()
*:*************************************************************
*:
FUNCTION lfvCost

lnFabCo1   = laData[19]
lnFabCo2   = laData[31]
lnFabCo3   = laData[32] 
lnFabCo4   = laData[33] 
llWasSelec = .T.

PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\HlFabCo.SPX")
POP KEY

IF !llWasSelect
  laData[19] = lnFabCo1
  laData[31] = lnFabCo2
  laData[32] = lnFabCo3
  laData[33] = lnFabCo4
ENDIF  

*-- End OF lfvCost.
*:*************************************************************
*: Name      : lfvDoz
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to get the Fabric Yards / Doz...
*:*************************************************************
*: Calls     : Screen "hlFabYa" .
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvDoz()
*:*************************************************************
*:
FUNCTION lfvDoz

lnFabCo1   = laData[20]
lnFabCo2   = laData[34]
lnFabCo3   = laData[35] 
lnFabCo4   = laData[36] 
llWasSelec = .T.

PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\hlFabYa.SPX")
POP KEY


IF !llWasSelect
  laData[20] = lnFabCo1
  laData[34] = lnFabCo2
  laData[35] = lnFabCo3
  laData[36] = lnFabCo4
ENDIF  
*-- End Of lfvDoz.
*:*************************************************************
*: Name      : lfShow
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Show Fabric Cost / Yard Screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfShow()
*:*************************************************************
*:
PROCEDURE lfShow

EXTERNAL ARRAY laScrMode

IF laScrMode[2]                     && View Mode
  SHOW GETS DISABLE ONLY
  SHOW GET pbOk ENABLE
ELSE                                && Edit or Add or Select Mode
  SHOW GETS ENABLE ONLY
ENDIF

*-- End OF lfShow.
*:*************************************************************
*: Name      : lfvCancel
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCancel()
*:*************************************************************
*:
FUNCTION lfvCancel

llWasSelec = .F.

*-- End OF lfvCancel.
*:*************************************************************
*: Name      : lfvData_40
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Validate laData[40]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_40()
*:*************************************************************
*:
FUNCTION lfvData_40
 
laData[40] = cbPrntCut

*-- End Of lfvData_40.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*
PROCEDURE lpClose

IF laScrMode [3] .OR. laScrMode [4]
  _CUROBJ = OBJNUM(pbCancel)
ELSE
  _CUROBJ = OBJNUM(pbOk)
ENDIF  
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 11/19/2001
*: Purpose   : Function to Open need files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: lcParam Hold :-
*:           :"IMPORT" ==> to open files need fro Import Function
*:           :"CVS" ==> to open files need fro CVS Function
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfOpenFls()
*:*************************************************************
*:
FUNCTION lfOpenFls
PARAMETER lcParam
Private lnAlias

IF TYPE ('lcParam') = 'U' .OR. TYPE ('lcParam') # 'C'
  RETURN
ENDIF
lnAlias = SELECT(0)

IF lcParam = 'IMPORT'
  IF !USED(lcCodes)
    IF FILE('&lcAASDir.CODES.DBF')
       llCodes = gfOpenFile(lcAASDir+'CODES',lcAASDir+'CODES','SH',@lcCodes,.T.)
    ELSE
      *- Message Text   :- Aria27 Code file not found. Cannot import any Data.
      *- Message Text   :- Please check your Aria27 Directory Path.
      *- Message No.    :- 38231.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      = gfModalGen('INM38231B00000','ALERT',"CODE")
      SHOW GET pbImport DISABLE
      _CUROBJ=OBJNUM(laData[24])
      SELECT (lnAlias)
      RETURN .F.
    ENDIF
  ENDIF

  IF !USED('STYLE')        && Open the STYLE File
    IF FILE('&lcAASDir.STYLE.DBF')
      llStyle = gfOpenFile(lcAASDir+'STYLE',lcAASDir+'STYLE','SH')  
    ELSE
      *- Message Text   :- Aria27 STYLE file not found. Cannot import any Data.
      *- Message Text   :- Please check your Aria27 Directory Path
      *- Message No.    :- 38216.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      = gfModalGen('INM38231B00000','ALERT',"STYLE")
      SHOW GET pbImport DISABLE
      _CUROBJ=OBJNUM(laData[24])
      SELECT (lnAlias)
      RETURN .F.
    ENDIF  
  ENDIF

  IF !USED('CUTTKTH')      && Open the CUTTKTH File
    IF FILE('&lcAASDir.CUTTKTH.DBF')
      llCuttkth = gfOpenFile(lcAASDir+'CUTTKTH',lcAASDir+'CUTTKTHS','SH')  
    ELSE
      *- Message Text   :- Aria27 CUTTKTH file not found. Cannot import any Data.
      *- Message Text   :- Please check your Aria27 Directory Path
      *- Message No.    :- 38216.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      = gfModalGen('INM38231B00000','ALERT',"CUTTKTH")
      SHOW GET pbImport DISABLE
      _CUROBJ=OBJNUM(laData[24])
      SELECT (lnAlias)
      RETURN .F.
    ENDIF  
  ENDIF  

  IF !USED('CTKTBOM')      && Open the CTKTBOM File
    IF FILE('&lcAASDir.CTKTBOM.DBF')
      llCtktBom = gfOpenFile(lcAASDir+'CTKTBOM',lcAASDir+'CTKTBOM','SH')  
    ELSE
      *- Message Text   :- Aria27 CTKTBOM file not found. Cannot import any Data.
      *- Message Text   :- Please check your Aria27 Directory Path
      *- Message No.    :- 38231.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      = gfModalGen('INM38231B00000','ALERT',"CTKTBOM")
      SHOW GET pbImport DISABLE
      _CUROBJ=OBJNUM(laData[24])
      SELECT (lnAlias)
      RETURN .F.
    ENDIF  
  ENDIF  

ELSE

  IF !USED('APVENDOR')      && Open the VENDOR File
    IF FILE('&lcAASDir.APVENDOR.DBF')
      llAPVENDOR = gfOpenFile(lcAASDir+'APVENDOR',lcAASDir+'Vencode','SH')  
    ELSE
      *- Message Text   :- Aria27  APVENDOR file not found. Cannot import any Data.
      *- Message Text   :- Please check your Aria27 Directory Path
      *- Message No.    :- 38216.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      = gfModalGen('INM38231B00000','ALERT',"APVENDOR")
      SELECT (lnAlias)
      RETURN .F.
    ENDIF  
  ENDIF  

ENDIF

IF !USED('FABRIC')         && Open the FABRIC File
  IF FILE('&lcAASDir.FABRIC.DBF')
    llFABRIC = gfOpenFile(lcAASDir+'FABRIC',lcAASDir+'FABRIC','SH')
  ELSE
    *- Message Text   :- Aria27 FABRIC file not found. Cannot import any Data.
    *- Message Text   :- Please check your Aria27 Directory Path
    *- Message No.    :- 38216.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM38231B00000','ALERT',"FABRIC")
    SHOW GET pbImport DISABLE
    _CUROBJ=OBJNUM(laData[24])
    SELECT (lnAlias)
    RETURN .F.
  ENDIF  
ENDIF


IF !USED('BOM')          && Open the BOM File
  IF FILE('&lcAASDir.BOM.DBF')
    llBom = gfOpenFile(lcAASDir+'BOM',lcAASDir+'BOM','SH')    
  ELSE
    *- Message Text   :- Aria27 BOM file not found. Cannot import any Data.
    *- Message Text   :- Please check your Aria27 Directory Path
    *- Message No.    :- 38216.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM38231B00000','ALERT',"BOM")
    SHOW GET pbImport DISABLE
    _CUROBJ=OBJNUM(laData[24])
    SELECT (lnAlias)
    RETURN .F.
  ENDIF  
ENDIF  

*-- End Of lfOpenFls
*:*************************************************************
*: Name      : lfTmp2Mast
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Update Or Add records from temp File to master file 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*:
*: Function to update records of master file from temp file 
*: Temp file has to have 2 additional fields nRecNo and cStatus to hold
*: the phisical records no and the action status done to this record
*:
FUNCTION lfTmp2Mast

PARAMETERS lcMastFile,lcTempFile

PRIVATE    lcMastFile,lcTempFile,lcSavAlias

lcSavAlias = SELECT(0)
lcSourcDir = IIF (UPPER(LEFT(lcMastFile,2))="SY",gcSyshome,gcDatadir)

lcSaveDel = SET ('DELETE')
SET DELETE OFF

SELECT (lcTempFile)
*** Scan through all the Added,Modified or Deleted records
SCAN FOR cStatus <> 'S'
  DO CASE 
    *** New added record   
    CASE cStatus = 'A'                    
      SCATTER MEMVAR MEMO
      SELECT  (lcMastFile)  
      IF SEEK(' ')                        && Chek if there is empty 
        RECALL                            && Deleted records to recall
        GATHER MEMVAR MEMO
      ELSE  
        INSERT INTO &lcSourcDir.&lcMastFile FROM MEMVAR
      ENDIF  
      =gfAdd_Info()
    *** Record was modified
    CASE cStatus = 'M'                    
      SCATTER MEMVAR MEMO                 && Collect data from temp
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo
      GATHER  MEMVAR MEMO                 && Replace master data 

    *** Record was deleted
    CASE DELETED()
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo

      SCATTER MEMVAR MEMO BLANK           && Empty the record befor
      GATHER  MEMVAR MEMO                 && delete it

      DELETE                              && Delete recored not in temp
  ENDCASE

  SELECT  (lcTempFile)
  REPLACE cStatus WITH "S"
ENDSCAN  

SET DELETE &lcSaveDel
SELECT (lcSavAlias)

*-- End Of lfTmp2Mast.
*:*************************************************************
*: Name      : lfvAAS
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/31/2001
*: Purpose   : Zoom In the Picture.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvAAS ()
*:*************************************************************
*
FUNCTION lfvAAS

*-- Seek with the type and Style in the Object Link file , if the 
*-- User enter style Picture in the AAS I will get it
*-- Also I will get the First Picture 

IF USED(lcTempCur)
  SELECT (lcTempCur)
  DELETE
ELSE
  Create TABLE (gcWorkDir+lcTempCur) (gPic_Sty G )
  SELECT (lcTempCur)
  APPEND BLANK
ENDIF


IF !USED('OBJECTS')        && Open the OBJECTS File.
  IF FILE('&lcAASDir.OBJECTS.DBF')
    llOBJECTID = gfOpenFile(lcAASDir+'OBJECTS',lcAASDir+'OBJECTID','SH')  
  ELSE
    *- Message Text   :- Aria27 OBJECTS file not found. Cannot import any Data.
    *- Message Text   :- Please check your Aria27 Directory Path
    *- Message No.    :- 38231.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM38231B00000','ALERT',"OBJECTS")
    RETURN .F.
  ENDIF  
ENDIF

IF !USED('OBJLINK')        && Open the OBJLINK File.
  IF FILE('&lcAASDir.STYLE.DBF')
    llOBJLINK = gfOpenFile(lcAASDir+'OBJLINK',lcAASDir+'Objlnkty','SH')  
  ELSE
    *- Message Text   :- Aria27 OBJLINK file not found. Cannot import any Data.
    *- Message Text   :- Please check your Aria27 Directory Path
    *- Message No.    :- 38216.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM38231B00000','ALERT',"OBJLINK")
    RETURN .F.
  ENDIF  
ENDIF

SELECT OBJLINK
IF SEEK ('S'+laData[24]+'-'+laData[1])
  SELECT (lcTempCur)
  ZAP
  lcPicName = lcImgDir+ALLTRIM(laData[24])+;
              '\'+ALLTRIM(laData[24])+'.BMP'

   lcObjectid = OBJLINK.cObject_id
    SELECT (lcTempCur)

    SELECT gobject AS gpic_sty           ; 
    FROM   &lcAASDir.OBJECTS             ;
    WHERE OBJECTS.cobject_id = lcObjectid;
    INTO TABLE (gcWorkDir+lcTempCur)
    lcNewPic  = lcPicName
    llNewPic  = .T.
ELSE
  *- Message Text   :- There is no image for style number XXX
  *- Message Text   :- in Aria27 Directory,Cannot Import any picture.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ',;
  'There is no image for style number '+ladata[1]+' in Aria27 Directory,Cannot Import any picture.')
  SELECT (lcTempCur)
ENDIF

*-- End Of lfvAAS.
*:*************************************************************

