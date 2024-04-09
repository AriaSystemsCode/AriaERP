*:****************************************************************
*: Program file  : ICCVSTY.PRG
*: Program desc. : Convrert Styles File In 2.6 to 2.7
*: System        : Aria Apparel System (A27).
*: Module        : Inventory Control (IC).
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200238
*: Date          : 10/07/2001.
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfGetDir , lfvOK , lfChckFls
*:               :            : lfOpenFls , lfClosFls
*:               -----------------------------------------------
*:               : PROCEDURE  : lpProcess , 
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*:C#200238,1          : 
*:****************************************************************
*:Modifications :
*:C#200238,4 ABD 10/24/2001 Supplementary Requirements to 
*:C#200238,4 ABD            original specifications.
*:****************************************************************
*
*-- Define Variables  [Begin]
*-- lcMulPath   :- Hold the Path Directry.
*-- lcOldStyle  :- Hold the style from Aria26.
*-- llOldStyle  :- True if old style from Aria26 was open from this program.
*-- llStyle     :- True if old style file from Aria27 was open from this program.
*-- llStydye    :- True if old Stydye file from Aria27 was open from this program.
*-- llicstyhst  :- True if old icStyhst file from Aria27 was open from this program.
*-- llNote      :- Hold true if we open notes file from this program.
*-- llNote1     :- Hold true if we open cksNote from this program.
*-- llNote2     :- Hold true if we open cklNote from this program.
*-- llMScale    :- Hold true in case Company extended size scale.
*-- llcklabel   :- Hold true in cae we open cklabel file in this program.
*-- llicdesign  :- Hold true in case we open icdesign file in this program.

lcMulPath = FULLPATH('')
STORE '' To  lcOldStyle
STORE .F. To llOldStyle  , llStyle , llStydye , llicstyhst , llMScale ,;
             llNote       , llNote1    ,llNote2 , llCKSTYDET , llcklabel,;
             llicdesign


*-- Define Variables  [End]
lcOldStyle = gfTempName()


*--Use extended size scale.  
llMScale   = gfGetMemVar('M_USEEXSSC')

IF !llMScale
  *- Message Text   :- The system is not set up to use extended size scale
  *- Message Text   :- can not complete the process.
  *- Message Number :- 00000
  *- button message :- OK
  *- button Number  :- 00000
  = gfModalGen("TRM000000B00000","DIALOG",'','',"The system is not set up to use extended size scale;"+;
                " can not complete the process.")
  RETURN
ENDIF



PUSH KEY
ON KEY
ON KEY LABEL ESC CLEAR READ

*-- Call The Converting Screen.
DO (gcScrDir + gcWinAppl +'\ICCVSTY.SPX')
POP KEY


RETURN

*-- End Of Coding.
*!*************************************************************
*! Name      : lfGetDir
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 10/07/2001
*! Purpose   : To get the 'directory' push button
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : = lfGetHdr ()
*!**************************************************************
*
FUNCTION lfGetDir


lcMulPath = GETDIR(lcMulPath,'Select old files directory.')

=lfRefresh()

*-- End Of lfGetDir.
*!*************************************************************
*! Name      : lfvOK
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 10/07/2001
*! Purpose   : To convert the style file from 2.6 into 2.7
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : = lfvOK ()
*!**************************************************************
*
FUNCTION lfvOK


IF EMPTY(ALLTRIM(lcMulPath))
  = gfModalGen("TRM000000B00000","DIALOG",'','',"You should select specific directory. Can not proceed.")
  RETURN
ENDIF


*-- Function to check on the old style files before doing the style conversions.
IF !lfChckFls ()
  RETURN
ENDIF

*-- Open needed files , "Old Style and new style and stydye ,and Icstyhst"
IF !lfOpenFls ()
  WAIT WINDOW 'Some files not found or can not be opened, can not complete.' NOWAIT
  RETURN
ENDIF

SELECT STYLE
lcMessage = ''
IF !EOF()
  lcMessage =  'Style file is not empty;'
ENDIF
*- Message Text   :- Are you sure you want to proceed.
*- Message Number :- 00000
*- button message :- \<Proceed;\?\<Cancel 
*- button Number  :- 40003
IF gfModalGen("TRM000000B40003","DIALOG",'','',lcMessage+" Are you sure you want to proceed.") = 2
  RETURN
ENDIF



*-- Start converting
Do lpProcess

*- Message Text   :- process completed successfully.
*- Message Number :- 42219.
*- button message :- Ok
*- button Number  :- 00000
= gfModalGen("QRM42219B00000","DIALOG")

*-- Close open file
= lfClosFls ()

CLEAR READ

*- End Of lfvOK.
*!*************************************************************
*! Name      : lfChckFls
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 10/07/2001
*! Purpose   : Function to check on the Old style and style , stydye
*!           : Icstyhst files before doing the style conversions
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : = lfChckFls ()
*!**************************************************************
*
FUNCTION lfChckFls

DO CASE
  CASE !EMPTY(lcMulPath) .AND. !FILE(lcMulPath+'STYLE.DBF') 
    *-- Check to found style file in selected directry.
    *- Message Text   :- "Style.DBF not found in XX , Can not proceed".
    *- Message Number :- 000000.
    *- button message :- OK.
    *- button Number  :- 00000
    = gfModalGen("TRM000000B00000","DIALOG",'','',"Style.DBF not found in ";
     + ALLTRIM(lcMulPath)+". Can not proceed.")
    RETURN .F.

  CASE !EMPTY(lcMulPath) .AND. !FILE(lcMulPath+'CKSNOTE.DBF') 
    *-- Check to found Note file 1 in selected directry.
    *- Message Text   :- "CKSNOTE.DBF not found in XX , Can not proceed".
    *- Message Number :- 000000.
    *- button message :- OK.
    *- button Number  :- 00000
    = gfModalGen("TRM000000B00000","DIALOG",'','',"CKSNOTE.DBF not found in ";
     + ALLTRIM(lcMulPath)+". Can not proceed.")
    RETURN .F.

  CASE !EMPTY(lcMulPath) .AND. !FILE(lcMulPath+'CKLNOTE.DBF') 
    *-- Check to found style file in selected directry.
    *- Message Text   :- "CKLNOTE.DBF not found in XX , Can not proceed".
    *- Message Number :- 000000.
    *- button message :- OK.
    *- button Number  :- 00000
    = gfModalGen("TRM000000B00000","DIALOG",'','',"CKLNOTE.DBF not found in ";
     + ALLTRIM(lcMulPath)+". Can not proceed.")
    RETURN .F.

  CASE !EMPTY(lcMulPath) .AND. !FILE(lcMulPath+'CKSTYDET.DBF') 
    *-- Check to found style file in selected directry.
    *- Message Text   :- "CKLNOTE.DBF not found in XX , Can not proceed".
    *- Message Number :- 000000.
    *- button message :- OK.
    *- button Number  :- 00000
    = gfModalGen("TRM000000B00000","DIALOG",'','',"CKLNOTE.DBF not found in ";
     + ALLTRIM(lcMulPath)+". Can not proceed.")
    RETURN .F.

  CASE !EMPTY(lcMulPath) .AND. !FILE(lcMulPath+'CKLABEL.DBF') 
    *-- Check to found style file in selected directry.
    *- Message Text   :- "CKLABEL.DBF not found in XX , Can not proceed".
    *- Message Number :- 000000.
    *- button message :- OK.
    *- button Number  :- 00000
    = gfModalGen("TRM000000B00000","DIALOG",'','',"CKLABEL.DBF not found in ";
     + ALLTRIM(lcMulPath)+". Can not proceed.")
    RETURN .F.

ENDCASE


*-- End Of lfChckFls.
*!*************************************************************
*! Name      : lfOpenFls
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 10/07/2001
*! Purpose   : Open Needed files.
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : = lfOpenFls ()
*!**************************************************************
*
FUNCTION lfOpenFls

*-- open old style 26
IF !llOldStyle
  llOldStyle = gfOpenFile(lcMulPath+'STYLE',lcMulPath+'STYLE','SH',@lcOldStyle,.T.)
ENDIF

*-- Open 1st file hold note pad.
IF !llNote1
   llNote1 = gfOpenFile(lcMulPath+'CKSNOTE',lcMulPath+'CKSNOTE','SH')
ENDIF

*-- Open 2nd file hold note pad.
IF !llNote2
   llNote2 = gfOpenFile(lcMulPath+'CKLNOTE',lcMulPath+'CKLNOTE','SH')  
ENDIF

IF !llCKSTYDET
 llCKSTYDET = gfOpenFile(lcMulPath+'CKSTYDET',lcMulPath+'CKSTYDET','SH')  
ENDIF

*C#200238,4 ABD - Open New files. [Begin]
IF !llcklabel
  llcklabel  = gfOpenFile(lcMulPath+'CKLABEL',lcMulPath+'CKLABEL','SH')  
ENDIF

IF !llicdesign
  llicdesign = gfOpenFile(gcDataDir+'ICDESIGN',gcDataDir+'ICDESIGN','SH')
ENDIF
*C#200238,4 ABD - [End]

*-- Open Style 27
IF !llStyle
  llStyle = gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
ENDIF

*-- Open Stydye 27
IF !llStydye
  llStydye = gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF

*-- Open styhist 27
IF !llicstyhst
  llicstyhst = gfOpenFile(gcDataDir+'ICSTYHST',gcDataDir+'STYHST','SH')
ENDIF

IF !llNote
  llNote = gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')
ENDIF

RETURN 

*-- End OF lfOpenFls.
*!*************************************************************
*! Name      : lfClosFls
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 10/07/2001
*! Purpose   : Close Style And stydye file.
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : = lfClosFls ()
*!**************************************************************
*
FUNCTION lfClosFls

*-- Old Style
IF llOldStyle
  = gfCloseFile(lcOldStyle)
ENDIF


IF llNote1
  = gfCloseFile('CKSNOTE')
ENDIF

*C#200238,4 ABD - Close open files that open before that. [Begin]
IF llcklabel
  = gfCloseFile('CKLABEL')
ENDIF

IF llicdesign
  = gfCloseFile('ICDESIGN')
ENDIF

*C#200238,4 ABD - [End]

IF llNote2
  = gfCloseFile('CKLNOTE')
ENDIF

IF llCKSTYDET
  = gfCloseFile('CKSTYDET')
ENDIF

*-- Style 27
IF llStyle
  = gfCloseFile('STYLE')
ENDIF

*-- Stydye 27
IF llStydye 
  = gfCloseFile('STYDYE')
ENDIF

*-- icStyhst 27
IF llicstyhst
  = gfCloseFile('ICSTYHST')
ENDIF

*-- Close note pad file in 27
IF llNote
  = gfCloseFile('NOTEPAD')
ENDIF

*-- End OF lfClosFls.
*!*************************************************************
*! Name      : lpProcess
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 10/07/2001
*! Purpose   : Start Convert styles from Aria26 to Aria 27
*!*************************************************************
*! Calls     : None
*!**************************************************************
*! Returns   : None
*!**************************************************************
*! Example   : DO lpProcess
*!**************************************************************
*
PROCEDURE lpProcess
PRIVATE lcStyle , lcScale , lcSepart , lnRecNo ,lcNote1 ,lcNote2;
        lcNumber

STORE '' TO lcStyle , lcScale , lcSepart ,lcNote1 ,lcNote2 , lcNumber
STORE 1 TO lnRecNo

*-- Get the seprator 
DECLARE laItemSeg[1]
STORE 0 To laItemSeg
= gfItemMask(@laItemSeg)
lcSepart1 = ALLTRIM(laItemSeg[1,6])
lcSepart2 = ALLTRIM(laItemSeg[2,6])
SELECT (lcOldStyle)
LOCATE
*-- Scan For Scale 1,3,A only
SCAN FOR Scale $ '13A'
  STORE 0 To lnPriceA
  *-- Check For scale.
  lnCount = IIF(Scale = '1',4,IIF(Scale ='3',3,2))
  
  FOR I = 1 To lnCount
    IF I = 1
      lnPriceA = &lcOldStyle..Pricea
    ENDIF
    DO CASE
      CASE lnCount = 4
        lcScale = 'A0'+STR(I,1)
      CASE lnCount = 3
        lcScale = 'B0'+STR(I,1)
      CASE lnCount = 2
        IF I = 1 
          lcScale = 'CJR'
        ELSE
          lcScale = 'CSR'
        ENDIF
    ENDCASE

    
    lcStyle = IIF(lnCount = 3 ,PADR(SUBSTR(ALLTRIM(Style),1,LEN(ALLTRIM(Style))-1),8),PADR(ALLTRIM(Style),8)) ;
           + lcSepart1 + Color +lcSepart2+ lcScale
    WAIT WINDOW 'Update Style .. :- '+lcStyle NoWait
    SELECT STYLE
    APPEND BLANK
    REPLACE STYLE      WITH lcStyle                 ,;
            CSTYMAJOR  WITH IIF(lnCount = 3 ,PADR(SUBSTR(ALLTRIM(Style),1,LEN(ALLTRIM(Style))-1),8),&lcOldStyle..STYLE); 
            Cdye_flg   WITH &lcOldStyle..Dyelot     ,;
            Pattern    WITH &lcOldStyle..Pattern    ,;
            Status     WITH &lcOldStyle..Status     ,;
            CstyGroup  WITH &lcOldStyle..Group      ,;
            Scale      WITH lcScale                 ,;
            PrePak     WITH &lcOldStyle..PrePak     ,;
            Desc       WITH &lcOldStyle..Desc       ,;
            Desc1      WITH &lcOldStyle..Desc1      ,;
            Season     WITH &lcOldStyle..Season     ,;
            NstyWeight WITH &lcOldStyle..Weight     ,;
            Qty_Ctn    WITH &lcOldStyle..Qty_Ctn    ,;
            Commission WITH &lcOldStyle..Commission ,;
            Link_Code  WITH IIF(EMPTY(&lcOldStyle..Link_code) .OR. &lcOldStyle..Link_code='DEF','DEFDEF',m.Link_code),;
            Make       WITH &lcOldStyle..Make       ,;
            NmCost1    WITH &lcOldStyle..NmCost1    ,;
            NmCost2    WITH &lcOldStyle..NmCost2    ,;
            NmCost3    WITH &lcOldStyle..NmCost3    ,;
            NmCost4    WITH &lcOldStyle..NmCost4    ,;
            NmCost5    WITH &lcOldStyle..NmCost5
    
    REPLACE NiCost1    WITH &lcOldStyle..NiCost1    ,;
            NiCost2    WITH &lcOldStyle..NiCost2    ,;
            NiCost3    WITH &lcOldStyle..NiCost3    ,;
            NiCost4    WITH &lcOldStyle..NiCost4    ,;
            NiCost5    WITH &lcOldStyle..NiCost5    ,;
            cPriceCur  WITH 'GBP'                   ,;
            CdutyCur   WITH 'GBP'                   ,;
            nPrCost2   WITH &lcOldStyle..nPrCost2   ,;
            nPrCost3   WITH &lcOldStyle..nPrCost3   ,;
            nPrCost4   WITH &lcOldStyle..nPrCost4   ,;
            nPrCost5   WITH &lcOldStyle..nPrCost5   ,;
            TotCost    WITH &lcOldStyle..TotCost    ,;
            Ave_Cost   WITH &lcOldStyle..Ave_Cost   ,;
            SoldOut    WITH &lcOldStyle..SoldOut    ,;
            Start      WITH &lcOldStyle..Start      ,;
            Fabric     WITH &lcOldStyle..Fabric     ,;
            Pricea     WITH &lcOldStyle..Pricea     ,;
            Priceb     WITH &lcOldStyle..Priceb     ,;
            Pricec     WITH &lcOldStyle..Pricec     ,;
            Marka      WITH &lcOldStyle..Marka      ,;
            Markb      WITH &lcOldStyle..Markb      ,;
            Markc      WITH &lcOldStyle..Markc      ,;
            nCurrRate  WITH &lcOldStyle..nCurrRate 
                
    REPLACE nFrgnprice WITH &lcOldStyle..nFrgnprice ,;
            Lupcyn     WITH &lcOldStyle..Lupcyn     ,;
            cConsinfo1 WITH &lcOldStyle..cConsinfo1 ,;
            cConsinfo2 WITH &lcOldStyle..cConsinfo2 ,;
            nsugretPri WITH &lcOldStyle..nsugretPri ,;
            Content1   WITH &lcOldStyle..Content1   ,;
            Content2   WITH &lcOldStyle..Content2   ,;
            CdefWare   WITH 'MAIN'                  ,;
            cTaxCode   WITH &lcOldStyle..cTaxCode   ,;
            nTaxBreak  WITH &lcOldStyle..nTaxBreak  ,;
            cslsglLink WITH 'DEF'                   ,;
            LdetCost   WITH .T.                     ,;
            cDivision  WITH &lcOldStyle..Division   ,;
            lTaxable   WITH .F.                     ,;
            Cadd_User  WITH gcUSer_Id               ,;
            Cadd_Time  WITH TIME()                  ,;
            Dadd_Date  With gdSysDate
                
    *-- Update the Custom Field
    IF SEEK(SUBSTR(Style,1,8),'CKSTYDET')
      *C#200238,4 ABD - Update the custom fields. [Begin]
      *REPLACE Priclevel WITH CKSTYDET.PriceCode
      REPLACE Priclevel WITH CKSTYDET.PriceCode ,;
              label_code WITH CKSTYDET.labelcode
      *C#200238,4 ABD - [End]
    ENDIF
  
    *-- Update the Style History file.
    SELECT ICSTYHST
    lcFisyear = '2000'
    FOR X = 1 To 3
      APPEND BLANK
      REPLACE STYLE      WITH lcStyle   ,;
              cFisfyear  WITH lcFisyear ,;
              Cadd_User  WITH gcUSer_Id ,;
              Cadd_Time  WITH TIME()    ,;
              Dadd_Date  WITH gdSysDate
      lcFisyear = STR(Val(lcFisyear) + 1 ,4)
    ENDFOR
    *-- Update the Style Dye file.
    SELECT STYDYE
    APPEND BLANK
    REPLACE STYLE       WITH lcStyle                 ,;
            cWareCode   WITH 'MAIN'                  ,;
            Dyelot      WITH &lcOldStyle..Dyelot     ,;
            Ave_Cost    WITH &lcOldStyle..Ave_Cost   ,;
            gl_link     WITH IIF(EMPTY(&lcOldStyle..Link_code) .OR. &lcOldStyle..Link_code='DEF','DEFDEF',m.Link_code),;
            cAdd_User   WITH gcUser_id               ,;
            cAdd_Time   WITH TIME()                  ,;
            dAdd_Date   WITH gdSysDate
    SELECT (lcOldStyle)
  ENDFOR
  
  *-- Update the ICDESIGN File for style has either a "P"
  *-- Or "J" in the First Character.
  IF SUBSTR(lcStyle,1,1) $ 'JP' 
    SELECT ICDESIGN
    APPEND BLANK
    REPLACE cDsgncode  WITH lcStyle     ,;
            cDsgnname  WITH Style.desc  ,;
            Season     WITH Style.season,;
            nDsgnPrc   WITH lnPriceA    ,;
            cDsgnClr   WITH Style.cConsInfo1
  ENDIF
  SELECT (lcOldStyle)
  
ENDSCAN

*-- Update the Notes file.
SELECT CKSNOTE
SCAN
  IF SEEK(SUBSTR(Code,1,8),'STYLE')
    WAIT WINDOW 'Update notepad ..: ' + Code NOWAIT
    STORE '' TO lcNote1 , lcNote2
    FOR lnRecNo = 1 TO 6
      lcNumber  =  STR(lnRecNo,1)
      lcNote1   = lcNote1 + sNote&lcNumber + CHR(13)
    ENDFOR
    
  
    *-- Get the other notes from the other file.
    lnRecNo  = 1
    lcNumber = ''
    IF SEEK (CKSNOTE.Code,'CKLNOTE')
      FOR lnRecNo = 1 TO 20
        lcNumber  =  ALLTRIM(STR(lnRecNo,2))
        lcNote2 = lcNote2 +  CKLNOTE.Lnote&lcNumber + CHR(13)
      ENDFOR
    ENDIF
   
    SELECT NOTEPAD
    APPEND BLANK
    REPLACE TYPE   WITH 'F'            ,;
            KEY    WITH CKSNOTE.Code   ,;
            cDesc  WITH "Notes For Style Number : "+CKSNOTE.Code;
            Mnotes WITH lcNote1 +  CHR(13) + lcNote2
     
     
    SELECT STYLE 
    REPLACE lhasnotes WITH .T.
    SELECT CKSNOTE
  ENDIF  
ENDSCAN

SELECT cklabel
SCAN
  SELECT NOTEPAD
  APPEND BLANK
  REPLACE TYPE   WITH 'T'            ,;
          KEY    WITH cklabel.Code   ,;
          cDesc  WITH "Label Details from Aria 2.6",;
          Mnotes WITH cklabel.label1 +  CHR(13) + cklabel.label2 + CHR(13) +cklabel.label3
ENDSCAN



WAIT CLEAR
*-- End Of lpProcess.
*!**************************************************************