*:----------------------------------------------------------------
*: Program file        : SMFRMCD.PRG
*: Program description : Form Codes
*: For screen          : SMFRMCD.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SM)
*: Developer Name      : Hossam El Etreby (HDM)
*: Tracking Job Number : E300871
*: Last modi           : 09/22/1998
*:----------------------------------------------------------------
*: Calls               : SMFRMCD.SPR
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO SMFRMCD
*:----------------------------------------------------------------
*: Modification
*E301065,1 AMM    11/22/1998 Add any custom forms added to the system files 
*E301065,1 AMM               to the dbfs' files and fix some bugs
*E301076,1 AMM    12/08/1998 Adjust the program to work properly with the form settings
*E301098,1 HESHAM 12/16/1998 Get company data path using gfGetDataDir(..)
*B602588,1 HESHAM 02/24/1999 Stop Browsing in browse valivation method
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*B602658,5 AMM 03/17/99 Fix the bug of table not ordered
*B602834,1 HDM 06/06/1999 Clear Tab key Traping
*E301251,1 HDM 06/08/1999 Show only records in FrmCdHd file that has lDisplay = .T.
*B603735,1 AMM 07/09/2000 Update the data files with standard records as well as custom records
*B603819,1    SSE 08/13/2000 Update FormCddt with SyfrmCdd each time user runs Form Codes screen.        
*E037351,1 HBG 23/12/2003 Add new field to SYREPUVR to differentiate between records belong to
*E037351,1                Aria27 and records belong to Aria4.
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*:----------------------------------------------------------------
EXTERNAL ARRAY laCtrStat, laDefProc, laScrMode

*-- laCtrStat : holds enable or disable state of tool bar buttons
*-- laDefProc : override default toolbar procedures
*-- laCompany holds company information to be displayed in the companies 
*-- popup

DIMENSION laCompany[1,2], laScrMode[4], laForms[1]

*-- To force the control pannel to call the local save procedure(lpSavScr)
laDefProc[9] = .F.

*E301077,78 Hesham (Start)
DIMENSION laSettings[3,2]
*STORE '' TO lcFrmCdCh0, lcFrmCdCh1 , lcFrmCdCh2, lc_FrmCdHd             ,;
		    puCompName, lcFrmName  , lcFrmDes  , lcDataDir , lnArLoop   ,;
		    lcCurComp , lcFormMaj  , lcFrmDtl  , lcFrmHdr  , lcPrevComp ,;
            cMarker   , llPrntComp , llFactor  , llPrntIns 
STORE '' TO lcFrmCdCh0, lcFrmCdCh1 , lcFrmCdCh2, lc_FrmCdHd             ,;
		    puCompName, lcFrmName  , lcFrmDes  , lcDataDir , lnArLoop   ,;
		    lcCurComp , lcFormMaj  , lcFrmDtl  , lcFrmHdr  , lcPrevComp ,;
            cMarker   , llPrntComp , llFactor  , llPrntIns ,laSettings,lcPuCompName

*E301077,78 Hesham (End)            
*E301076,1 AMM Not used any more.
*STORE .F. TO llSupress
*E301076,1 AMM
lcBrowTtl = "Forms Codes"
*E301065,1 AMM Initialize these variables, to adjust display in select mode.
STORE SPACE(0) TO laForms
lcFrmName  = "Form Name"
*E301065,1 AMM END
*-- Define these variables for the option grid

*E301065,1 AMM START, Don't hard code these variables, get it from 
*E301065,1 AMM SYREPUVR instead
*DIMENSION laSettings[3,2]
*laSettings[1,1] = 'llPrntComp'
*laSettings[2,1] = 'llFactor'
*laSettings[3,1] = 'llPrntIns'
lnVarNum = 0
llOpRep = gfOpenFile(gcSysHome+'SYREPUVR','Fld_Name','SH')
=SEEK('SMFRMCD')

SCAN WHILE cRep_Id = 'SMFRMCD' FOR lAskRunT .AND. lDispOG
  *E037351,1 HBG 23/12/2003 Scan for records belong to Aria27 only [Begin]
  IF !EMPTY(CVER) AND CVER <> 'A27'
    LOOP
  ENDIF
  *E037351,1 [End]
  lnVarNum = lnVarNum + 1
  DIMENSION laSettings[lnVarNum,2]
  laSettings[lnVarNum,1] = SYREPUVR.mFld_Name
  *E301076,1 AMM Initialiaze the setting variables.
  DO CASE
    CASE SYREPUVR.cData_typ = 'L'
      STORE .F. TO (laSettings[lnVarNum,1])
    CASE SYREPUVR.cData_typ = 'C'
      STORE SPACE(0) TO (laSettings[lnVarNum,1])
    CASE SYREPUVR.cData_typ = 'D'
      STORE {} TO (laSettings[lnVarNum,1])
    CASE SYREPUVR.cData_typ = 'N'
      STORE 0 TO (laSettings[lnVarNum,1])
  ENDCASE
  *E301076,1 AMM end
ENDSCAN

IF llOpRep .AND. USED('SYREPUVR')
  USE IN SYREPUVR
ENDIF
RELEASE lnVarNum
RELEASE llOpRep
*E301065,1 AMM end

*-- Save the original Escape button trapping in this variable
*-- just to re-load it when going out of the browse window.
lcEscTrap  = ON("KEY","ESC")

IF !gfSetup()
  RETURN
ENDIF  

*-- Permanently disable the following tool bar buttons
*-- New    : laCtrStat[05]
*-- Print  : laCtrStat[06]
*-- Delete : laCtrStat[08] 
*-- Browse : laCtrStat[10]
STORE 'DISABLE' TO laCtrStat[05], laCtrStat[06], laCtrStat[08], laCtrStat[10]

*-- Child window variable names

IF !WEXIST(gcBaseWind)

  *-- Temporary form codes files
  lc_FrmCdHd = gfTempName()			&& Cursor name for a temporary FORMCDHD
  lcFrmDtl   = gfTempName()		    && Alias name for FORMCDDT
  lcFrmHdr   = gfTempName()			&& Alias name for FORMCDHD
  
  IF !lfReadComp()
    glQuitting = .T.
    RETURN
  ELSE
    *-- Child window names
    lcFrmCdCh0 = gfTempName()
    lcFrmCdCh1 = gfTempName()
    lcFrmCdCh2 = gfTempName()

    *-- Temporary file created
    
    CREATE CURSOR (lc_FrmCdHd);
      (cFormMaj C(6), cFormMjDes C(30), cFormCdDes C(30), cCurForm C(2)   ,;
       mFormSets M  , cStatus C(1)    , nRecNo N(4)     , CADD_USER C(10) ,;
       DADD_DATE D(8) , CADD_TIME C(11) , LLOK_STAT L , CLOK_USER C(10)   ,;
       DLOK_DATE D(8) , CLOK_TIME C(8) , CFORMID C(2) , LDISPLAY L)
    
    *-- If there is no active company, add a bar
    *-- 'Select a company' =>SELECT Mode
    *-- and make it the active bar
    *-- else, active bar = active company ==> VIEW Mode

    DIMENSION laCompany[ALEN(laCompany,1) + 1, 2]
    = AINS(laCompany,1)
    laCompany[1,1] = "Select a company"
    laCompany[1,2] = "Select a company"

    IF EMPTY(gcAct_Comp)
      puCompName   = laCompany[1,1]
    ELSE
      = lfGetActCo()
      = lfOpenDbf()
      *E301065,1 AMM Check if there are custom forms added to the system files
      =lfchkCFrm()
      *E301065,1 AMM end

      llNoShow     = .F.
      laScrMode    = .F.
      laScrMode[2] = .T.
    ENDIF
    lcPuCompName     = puCompName 
  ENDIF  
ELSE
*  IF EMPTY(gcAct_Comp)
*    puCompName   = laCompany[1,1]
*  ELSE
     puCompName = lcPuCompName 
    *= lfGetActCo()
    = lfOpenDbf()
    llNoShow     = .F.
*    laScrMode    = .F.
*    laScrMode[2] = .T.
*  ENDIF
ENDIF  

PUSH MENU _MSYSMENU

*-- Define a menu bar just to handle the tab key behaviour
DEFINE BAR 100 OF _MSYSMENU PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF _MSYSMENU ACTIVATE WINDOW (lcBrowTtl)

lcDelPadNm = SPACE(0)
lcBarPrmpt = '\<Delete'
lnDelBarNo = lfGtDelBar(@lcBarPrmpt)
DEFINE BAR lnDelBarNo OF (lcDelPadNm) PROMPT (lcBarPrmpt)
SET SKIP OF BAR lnDelBarNo OF (lcDelPadNm) .T.

*--- Just Run the Screen
DO (gcScrDir + gcWinAppl + '\SMFRMCD.SPR')

RELEASE WINDOW (lcBrowTtl)
POP MENU _MSYSMENU

*--HDM B602834,1 [Start] Clear Tab key Traping
ON KEY LABEL ESC &lcEscTrap
ON KEY LABEL TAB
ON KEY LABEL BACKTAB 
*--HDM B602834,1 [End]
******
*lcSkipExp = IIF(EMPTY(SYDOBJCT.mskipexpr),[],[SKIP FOR ]+;
                        ALLTRIM(SYDOBJCT.mskipexpr))
*lcHotKey  = IIF(EMPTY(cSub_hKey),[],[KEY ]+ALLTRIM(cSub_hKey))   
*DEFINE BAR lnDelBarNo OF (lcDelPadNm) PROMPT (lcMenProm)
******

*-- If completely exiting the screen, close and erase temporary file
IF USED(lcFrmHdr)
  USE IN (lcFrmHdr)
ENDIF
IF USED(lcFrmDtl)
  USE IN (lcFrmDtl)
ENDIF
IF glQuitting
  IF USED(lc_FrmCdHd)
    USE IN (lc_FrmCdHd)
  ENDIF
ENDIF

*------------------------------------------------------------------------
*!*************************************************************
*! Name      : lpShow
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Handle The special case for screen modes
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpShow
*!*************************************************************

PROCEDURE lpShow
*-- Called from gpCtrlShow which is the SHOW procedure of the READ CYCLE,
*-- Added by the generated code of the tool bar to the SPR 

DO CASE
  CASE laScrMode[1] && Select mode
    SELECT (lc_FrmCdHd)
    BLANK ALL
    DELETE ALL
    lcFrmName  = "Form Name"
    puCompName = laCompany[1,1]
    puForms    = 0 
    SHOW GET puCompName
    SHOW GET puForms
    SHOW WINDOW (lcBrowTtl) REFRESH
*    puForms = "N/A"
*    SHOW GET puForms
    llNThing  = lfRefresh()

  CASE laScrMode[2] && View Mode
    SELECT (lc_FrmCdHd)
    BLANK ALL
    DELETE ALL
    
    SELECT (lcFrmHdr)
    SCAN
      SCATTER MEMVAR MEMO
      m.nRecNo = RECNO()
      SELECT (lcFrmDtl)
      m.cFormCdDes = IIF(SEEK(m.cFormMaj+m.cCurForm), cFormCdDes, "")
      m.cStatus    = 'S'
      *-- Add The selected values to cursor
      INSERT INTO (lc_FrmCdHd) FROM MEMVAR
    ENDSCAN
    *-- U Can browse the selected through the cursor
    SELECT (lc_FrmCdHd)
    GOTO TOP
    *SHOW WINDOW (lcBrowTtl) REFRESH SAME
    =lfWhenBrow()

  CASE laScrMode[3] && Edit mode
    SELECT (lc_FrmCdHd)
    BLANK ALL
    DELETE ALL
    
    SELECT (lcFrmHdr)
    SCAN
      SCATTER MEMVAR MEMO
      m.nRecNo = RECNO()
      
      SELECT (lcFrmDtl)
      m.cFormCdDes = IIF(SEEK(m.cFormMaj+m.cCurForm), cFormCdDes, "")
      m.cStatus    = 'S'
      INSERT INTO (lc_FrmCdHd) FROM MEMVAR
    ENDSCAN
    
    SELECT (lc_FrmCdHd)
    GOTO TOP
    *SHOW WINDOW (lcBrowTtl) REFRESH SAME
    =lfWhenBrow()
ENDCASE

*-- Permanently disable the following tool bar buttons
SHOW GET pbCpNew   DISABLE
SHOW GET pbcpPrint DISABLE
SHOW GET pbDlt     DISABLE
SHOW GET pbBrws    DISABLE

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfWhenBrow
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Handles The Movement of the arow keys inside the
*!             Browse 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfWhenBrow()
*!*************************************************************

FUNCTION lfWhenBrow

*--HDM E301251,1 [Start] Define variable to hold form special settings
*PRIVATE lcCurForm
PRIVATE lcCurForm , lcFormId
*--HDM E301251,1 [End]

DIMENSION laForms[1]

laForms[1] = SPACE(0)
lcCurForm  = &lc_FrmCdHd..cFormMaj
*--HDM E301251,1 [Start] Read the special setting variable and filter to get its form

lcFormId   = &lc_FrmCdHd..CFORMID

*SELECT cFormCode + " - " + cFormCdDes,  cFormCode ;
  FROM (lcFrmDtl) ;
  WHERE cFormMaj+cFormCode = lcCurForm ;
  INTO ARRAY laForms

*-- Add CformMaj to the array as 3rd Element

SELECT cFormCode + " - " + cFormCdDes,  cFormCode,cFormMaj ;
  FROM (lcFrmDtl) ;
  WHERE cFormMaj+cFormCode = lcCurForm ;
  AND CFORMID = PADR(lcFormId,2);
  INTO ARRAY laForms
*--HDM E301251,1 [End]


*E301065,1 AMM make this condition not to cause a bug.
*puForms    = ASUBSCRIPT(laForms, ASCAN(laForms,&lc_FrmCdHd..cCurForm), 1)
IF ASCAN(laForms,&lc_FrmCdHd..cCurForm) # 0
  puForms    = ASUBSCRIPT(laForms, ASCAN(laForms,&lc_FrmCdHd..cCurForm), 1)
ELSE
  puForms = 1
ENDIF

*E301065,1 AMM END	
SHOW GET puForms
*E301076,1 AMM Not used any more
*llSupress = IIF(cFormMaj = 'ARPINV', .F. , .T.)
*E301076,1 AMM
lcFrmName  = ALLTRIM(&lc_FrmCdHd..cFormMjDes)
llNThing   = lfRefresh()
lnRecNo   = RECNO()
SHOW WINDOW (lcBrowTtl) REFRESH SAME

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfBrowse
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Browses the TEMP Cursor Recopds
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfBrowse()
*!*************************************************************

FUNCTION lfBrowse

lnRecNo  = RECNO()

SELECT (lc_FrmCdHd)
*--HESHAM B602588,1 Force valid function to stop browsing(Start)
*BROWSE FIELDS cMarker=IIF(RECNO()=lnRecNo,'>',' ') :R:H = " ",;
*              &lc_FrmCdHd..cFormMjDes :R:H = "Form Name"     ,;
*              &lc_FrmCdHd..cCurForm   :R:H = "Form Code"     ,;
*              &lc_FrmCdHd..cFormCdDes :R:H = "Description"    ;
*              WINDOW (lcFrmCdCh1)                           ;
*              IN WINDOW (gcBaseWind)                        ;
*              TITLE (lcBrowTtl) LOCK 0                      ;
*              WHEN lfWhenBrow()                             ;
*              NOWAIT SAVE NOEDIT NOCLEAR                    ;
*              NOAPPEND NODELETE NOMENU

*BROWSE FIELDS cMarker=IIF(RECNO()=lnRecNo,'>',' ') :R:H = " ",;
              &lc_FrmCdHd..cFormMjDes :R:H = "Form Name"     ,;
              &lc_FrmCdHd..cCurForm   :R:H = "Form Code"     ,;
              &lc_FrmCdHd..cFormCdDes :R:H = "Description"    ;
              WINDOW (lcFrmCdCh1)                           ;
              IN WINDOW (gcBaseWind)                        ;
              TITLE (lcBrowTtl) LOCK 0                      ;
              WHEN lfWhenBrow()                             ;
              VALID :F lfvBrows()                           ;  
              NOWAIT SAVE NOEDIT NOCLEAR                    ;
              NOAPPEND NODELETE NOMENU
*MANRENEE Removed FOR LDISPLAY
BROWSE FIELDS cMarker=IIF(RECNO()=lnRecNo,'>',' ') :R:H = " ",;
              &lc_FrmCdHd..cFormMjDes :R:H = "Form Name"     ,;
              &lc_FrmCdHd..cCurForm   :R:H = "Form Code"     ,;
              &lc_FrmCdHd..cFormCdDes :R:H = "Description"    ;
              WINDOW (lcFrmCdCh1)                           ;
              IN WINDOW (gcBaseWind)                        ;
              TITLE (lcBrowTtl) LOCK 0                      ;
              WHEN lfWhenBrow()                             ;
              VALID :F lfvBrows()                           ;  
              NOWAIT SAVE NOEDIT NOCLEAR                    ;
              NOAPPEND NODELETE NOMENU

*--HESHAM B602588,1 Force valid function to stop browsing(End)


*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfvCompany
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Gets the selected company Data Files Path AND 
*!             opens them
*!*************************************************************
*! Calls     : lfOpenDbf
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvCompany()
*!*************************************************************

FUNCTION lfvCompany

*-- When a company is selected, switch to view mode:
IF lcPrevComp <> puCompName AND puCompName <> "Select a company"
  laScrMode    = .F.
  laScrMode[2] = .T.
  lcPuCompName = puCompName
  IF !lcCurComp = puCompName
    *-- When Any other item is selected just get it's data source
    FOR lnArLoop2 = 1 To ALEN(laCompany, 1)
      IF ALLTRIM(laCompany[lnArLoop2,1]) = ALLTRIM(puCompName)
        lcDataDir        = ALLTRIM(laCompany[lnArLoop2,2])
        lcCurComp        = ALLTRIM(laCompany[lnArLoop2,1])
        EXIT
      ENDIF
    ENDFOR
    = lfOpenDbf()
    *E301065,1 AMM start, Check if there is any custom forms added
    =lfchkCFrm()
    *E301065,1 AMM end
    SHOW GETS
    ACTIVATE WINDOW (lcBrowTtl)

  ENDIF
ELSE
  laScrMode    = .F.
  laScrMode[1] = .T.
ENDIF
*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfReadComp
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Reads the available companies from SYCCOMP
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfReadComp()
*!*************************************************************

FUNCTION lfReadComp
PRIVATE llRetVal

*-- Fill the Companies array from SYCCOMP
*B602588,1 Hesham (Start)
*SELECT cComp_ID +' - ' + cCom_Name, cCom_DDir;
  FROM (gcSysHome + 'SYCCOMP');
  INTO ARRAY laCompany;
 ORDER BY cComp_ID
*B602588,1 Hesham (Start) 
*B602658,5 AMM The file alerady opened by program files
*llCompUse=gfSysOpen(gcSysHome+'SYCCOMP')
*B602658,5 AMM end
*B602588,1 Hesham (End) 
SELECT cComp_ID +' - ' + cCom_Name,PADR(gfGetDataDir(ALLT(cCom_DDir)),LEN(cCom_dDir));
  FROM (gcSysHome + 'SYCCOMP');
  INTO ARRAY laCompany;
 ORDER BY cComp_ID
*E301098,1 Hesham End)
*-- Check if any company exists at all
IF _TALLY = 0
  *-- Message 
  *** No companies available. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00189B00000","DIALOG","companies")
  llRetVal = .F.
ELSE
  llRetVal = .T.
ENDIF
*B602588,1 Hesham (Start) 
*B602658,5 AMM This file is withen the program files, don't close it.
*USE IN IIF(llCompUse,'SYCCOMP',0)
*B602658,5 AMM end
*B602588,1 Hesham (End) 
RETURN llRetVal

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfGetActCo
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Gets the active company And makes it the selected
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGetActCo()
*!*************************************************************

FUNCTION lfGetActCo
PRIVATE lcTemp , lnActLen

*-- The active Company String length
lnActLen = LEN(gcAct_Comp)

*-- Loop through the array to find the active company Name according to
*-- String comparison.
FOR lnArLoop = 1 To ALEN(laCompany, 1)
  lcTemp = SUBSTR(laCompany[lnArLoop,1], 1, lnActLen)
  IF ALLTRIM(lcTemp) = ALLTRIM(gcAct_Comp)
    puCompName        = laCompany[lnArLoop,1]
    lcCurComp         = laCompany[lnArLoop,1]
    EXIT
  ENDIF
ENDFOR

*-- Return the Selected company Data directory
lcDataDir = ALLTRIM(laCompany[lnArLoop , 2])

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfOpenDbf
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Open the Data Files
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfOpenDbf()
*!*************************************************************

FUNCTION lfOpenDbf

*-- Open The database file
IF USED(lcFrmDtl)
  USE IN (lcFrmDtl)
ENDIF

IF USED(lcFrmHdr)
  USE IN (lcFrmHdr)
ENDIF

USE (lcDataDir + 'FORMCDHD')  AGAIN ALIAS (lcFrmHdr);
	IN 0 ORDER TAG FORMCDHD

USE (lcDataDir + 'FORMCDDT') AGAIN ALIAS (lcFrmDtl);
	IN 0 ORDER TAG FORMCDDT

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfvFormCode
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Update the cursor with the selected code
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvFormCode()
*!*************************************************************

FUNCTION lfvFormCode

SELECT (lc_FrmCdHd)
REPLACE cCurForm   WITH laForms[puForms,2]            ,;
 	    cFormCdDes WITH SUBSTR(laForms[puForms,1], 6) ,;
 	    cStatus    WITH 'M'

*MANRENEE Start
*-- E301251,1 HDM [Start] We have to replace all hidden sub froms to be up to date
*--                       as its main form
*SCAN FOR CFORMMAJ = laForms[puForms,3] AND !LDISPLAY
*  REPLACE cCurForm   WITH laForms[puForms,2]            ,;
*          cFormCdDes WITH SUBSTR(laForms[puForms,1], 6) ,;
*          cStatus    WITH 'M'
  
*ENDSCAN
*MANRENEE End
*-- E301251,1 HDM [End]

*-- Update the cursor with the audit values
= gfAdd_Info(lc_FrmCdHd)

SHOW WINDOW (lcBrowTtl) REFRESH SAME

*-------------------------------------------------------------
*!*************************************************************
*! Name      : lfwCoName
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Sets a variable with the previouse company
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfwCoName()
*!*************************************************************

FUNCTION lfwCoName
lcPrevComp = puCompName

* --------------------------------------------------
*!*************************************************************
*! Name      : lfGoToNav
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Moves the Focus to the Settings Window
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGoToNav()
*!*************************************************************

FUNCTION lfGoToNav

IF SYS(18) = UPPER('pbLayout')
  =lfGoToFlds()
*ELSE
*  ACTIVATE WINDOW (lcFrmCdCh2)
*  _CUROBJ = OBJNUM(puForms)
ENDIF
* --------------------------------------------------
*!*************************************************************
*! Name      : lfGoToFlds
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Moves the Focus to the Company Selection Window
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGoToFlds()
*!*************************************************************

FUNCTION lfGoToFlds

ACTIVATE WINDOW (lcFrmCdCh0)
_CUROBJ = OBJNUM(puCompName)

* --------------------------------------------------
*!*************************************************************
*! Name      : lfTrap
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : READ CYCLE Deactivate function (for the browse trapping)
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfTrap()
*!*************************************************************

FUNCTION lfTrap

IF WONTOP() = (lcBrowTtl)
  glFromBrow  = .T.
  ON KEY LABEL ESCAPE      lnDummy = 1
  ON KEY LABEL TAB         DO lfGoToNav
  ON KEY LABEL BACKTAB     DO lfGoToFlds
ENDIF
RETURN .T.

* --------------------------------------------------
*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : For Clearing the browse trapping When Going Out
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************

FUNCTION lfClrTrap

IF glFromBrow
  ON KEY LABEL ESC &lcEscTrap
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB 
  =gfStopBrow()
ENDIF


* --------------------------------------------------
*!*************************************************************
*! Name      : lpSavScr
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : My own Saving
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************

PROCEDURE lpSavScr
= gftmp2Mast(lcFrmHdr ,lc_FrmCdHd)
laScrMode    = .F.
laScrMode[2] = .T.

* --------------------------------------------------
*!*************************************************************
*! Name      : lfGtDelBar
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : To get the Delete BAR Number to disable it
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGtDelBar()
*!*************************************************************

FUNCTION lfGtDelBar
PARAMETERS lcBarPrmpt

*-- Open the system menu file.
USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
IF SEEK("GFCPDELETE", "MenuFile")
  lcDelPadNm = UPPER(ALLTRIM(MenuFile.cMstr_Nam))
  lnRetVal   = VAL(MenuFile.cBar_Pos)
  lcBarPrmpt = ALLTRIM(MenuFile.cSub_Prpt)
ELSE
  *-- The default values.
  lcDelPadNm = "P03PU03"
  lnRetVal   = 10
  lcBarPrmpt = '\<Delete'
ENDIF
USE IN MenuFile
RETURN (lnRetVal)
* --------------------------------------------------
*!*************************************************************
*! Name      : lfVSetings
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Generate the option grid with the saved values
*!*************************************************************
*! Calls     : lfSavDefVr AND lfGetDefVr
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfVSetings()
*!*************************************************************

FUNCTION lfVSetings

SELECT (lc_FrmCdHd)
REPLACE cStatus WITH 'M'

= lfGetDefVr()

*-- Call the option grid Function
= gfOpGrid("SMFRMCD",.T.)
= lfSavDefVr()
     
* ---------------------------------------------
*!*************************************************************
*! Name      : lfRepShow
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Customize the option grid
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfRepShow()
*!*************************************************************

FUNCTION lfRepShow

SHOW GET pbOGSave DISABLE
SHOW GET pbRun    DISABLE

* --------------------------------------------------
*!*************************************************************
*! Name      : lfGetDefVr
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Get the saved values for the option grid
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfGetDefVr()
*!*************************************************************
 
FUNCTION lfGetDefVr
PRIVATE lnCount

SELECT (lc_FrmCdHd)
IF !EMPTY(&lc_FrmCdHd..mFormSets)
  RESTORE FROM MEMO mFormSets ADDITIVE
  FOR lnCount = 1 TO ALEN(laSettings,1)
    &laSettings[lnCount, 1] = laSettings[lnCount, 2]
  ENDFOR
ENDIF

* --------------------------------------------------
*!*************************************************************
*! Name      : lfSavDefVr
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Save the values to the TEMP File
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfSavDefVr()
*!*************************************************************

FUNCTION lfSavDefVr
PRIVATE lnCount

FOR lnCount = 1 TO ALEN(laSettings,1)
  laSettings[lnCount, 2] = &laSettings[lnCount, 1] 
ENDFOR

*-- Save the settings into the memo field inside the DBF
SELECT (lc_FrmCdHd)
SAVE TO MEMO mFormSets;
     ALL LIKE laSettings


* --------------------------------------------------
*!*************************************************************
*! Name      : lfvLayout
*! Developer : Hossam El Etreby
*! Date      : 26/07/1998
*! Purpose   : Run the layout preview considering the selected form
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfvLayout()
*!*************************************************************
FUNCTION lfvLayout
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT (lcFrmDtl)
*E301065,1 AMM if there is not layout exist, don't display an empty report
=SEEK(&lc_FrmCdHd..cFormMaj + &lc_FrmCdHd..cCurForm)
IF EMPTY(gfRmBmp)
  *-- Message
  *-- Invoice form layout is not available.
  *--          < OK >
  =gfModalGen("INM00341B00000","DIALOG",ALLTRIM(&lc_FrmCdHd..cFormMjDes))
ELSE
*E301065,1 AMM end

  REPORT FORM (gcRepHome+gcWinAppl+"\SMFRMCD.FRX")       ;
       FOR                                             ;
         &lc_FrmCdHd..cFormMaj + &lc_FrmCdHd..cCurForm ;
       = &lcFrmDtl..cFormMaj   + &lcFrmDtl..cFormCode  ;
       PREVIEW

  SELECT (lnCurAlias)
ENDIF
* --------------------------------------------------

*!*************************************************************
*! Name      : lfchkCFrm
*! Developer : Ahmed Ibrahim
*! Date      : 11/22/1998
*! Purpose   : Check if there is any custom forms added to the system files
*! Reference : *E301065,1
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfchkCFrm()
*!*************************************************************
FUNCTION lfchkCFrm

*B603819,1 Define an array for SyfrmCdd fields Structure [Begin]
PRIVATE laSyFrmCdd , lnAlias
*B603819,1 Define an array for SyfrmCdd fields Structure [Begin]

*E301065,1 AMM Open necessary files and set relations.
lcTmpFDT = gfTempName()
llOpSYREP = gfOpenFile(gcSysHome+'SYDREPRT','CREP_ID','SH')

*E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

IF USED('SYDREPRT')
  SET FILTER TO SYDREPRT.cVer<>"A40" 
ENDIF

*E038033,1 HMA [END]
USE (gcSysHome+'SYFRMCDD') ORDER TAG UPGRDLVL AGAIN ALIAS (lcTmpFDT) IN 0
*B603735,1 AMM open the SYFORMCDH with another alias
lcTmpFHD = gfTempName()
USE (gcSysHome+'SYFRMCDH') ORDER TAG Formcdhd AGAIN ALIAS (lcTmpFHD) IN 0
*B603735,1 AMM end
SELECT (lcTmpFDT)
SET RELATION TO cformmaj+cformcode INTO (lcFrmDtl) ADDITIVE
SET RELATION TO cRep_Id INTO SYDREPRT ADDITIVE
*B603735,1 AMM set relation
SET RELATION TO cformmaj+cformID INTO (lcTmpFHD) ADDITIVE
*B603735,1 AMM Remove the condition of checking custom records
*=SEEK('U')
*E301065,1 AMM Scan on the custom forms.
*SCAN WHILE cUpgrdLvl= 'U' FOR EOF(lcFrmDtl)
 
*B603819,1 Scan loop on all records [Begin]
=AFIELDS(laSyFrmCdd)
*SCAN FOR EOF(lcFrmDtl)
SCAN
*B603819,1 Scan loop on all records [End]

*B603735,1 AMM end
  IF SEEK(SUBSTR(puCompName,1,2),'SYCCOMP') .AND. !EOF('SYDREPRT')
    *E301065,1 AMM Get installed modules of the company selected.
    lcInstMod = SYCCOMP.mComp_Mdl
    IF !EMPTY(SYDREPRT.mCallMods)
      lnRel = OCCURS('|',SYDREPRT.mCallMods)
      DIMENSION laCalMdls[lnRel+1]
      laCalMdls = ''
      *B602658,5 AMM the separator is "," not "|"
      *=gfSubStr(SYDREPRT.mCallMods,@laCalMdls,"|")
      =gfSubStr(SYDREPRT.mCallMods,@laCalMdls,",")
      *B602658,5 AMM end
      *E301065,1 AMM Check if any required module installed, add a record 
      *E301065,1 AMM in the dbfs directory.      
      FOR lnC=1 TO ALEN(laCalMdls)
        IF laCalMdls[lnC] $ lcInstMod      
          SCATTER MEMVAR MEMO

          *B603819,1 If record is not found in Data file [Begin]
          IF EOF(lcFrmDtl)
            *INSERT INTO (lcFrmDtl) FROM MEMVAR
            *-- we 've replaced Append From instead of Insert into (for the General Field) to be copied            
            lnAlias = SELECT()
            SELECT (lcFrmDtl)
            APPEND FROM (gcSysHome+'SYFRMCDD') FOR cFormMaj + cFormCode = m.cFormMaj+m.cFormCode
            SELECT (lnAlias)
          *B603819,1 If record is not found in Data file [End]        

          
          *B603819,1 Else record is found update it [Begin]
          ELSE
            =RLOCK()
            *-- For loop around all fields of SyFrmcdd
            FOR lnC = 1 TO ALEN(laSyFrmCdd,1)
              *-- if Type of field is Char , Numeric , Logical , Memo
              IF laSyFrmCdd[lnC,2] $ 'CNLM' 
                REPLACE &lcFrmDtl..&laSyFrmCdd[lnC,1] WITH m.&laSyFrmCdd[lnC,1]
              ENDIF
              *-- Endif of Type of field is Char , Numeric , Logical , Memo
            ENDFOR
            UNLOCK
          ENDIF
          *B603819,1 Else record is found update it [End]
          
          *B603735,1 AMM If a new record, add it to the FORMCDHD file
          IF !EOF(lcTmpFHD) .AND. !SEEK(&lcTmpFHD..cformmaj+&lcTmpFHD..cformID,lcFrmHdr)
            SELECT (lcTmpFHD)
            SCATTER MEMVAR MEMO
            INSERT INTO (lcFrmHdr) FROM MEMVAR
          ENDIF
          *B603735,1 AMM end
          EXIT      
        ENDIF
      ENDFOR        
    ENDIF
  ENDIF
ENDSCAN

IF USED(lcTmpFDT)
  USE IN (lcTmpFDT)
ENDIF
IF llOpSYREP .AND. USED('SYDREPRT')
  USE IN SYDREPRT
ENDIF
*B603735,1 AMM
IF USED(lcTmpFHD)
  USE IN (lcTmpFHD)
ENDIF
*B603735,1 AMM end


*!*****************************************************************
*! Function: lfvBrows
*!*****************************************************************
*! Author : Hesham
*!*****************************************************************
*! Purpose : Stop browse case activating another screen
*!*****************************************************************
*! DUE TO Bug# 602588
*!*****************************************************************

FUNCTION lfvBrows
IF WONTOP(lcBrowTtl)
  glFromBrow = .T.
ELSE
  = gfStopBrow()
  glFromBrow = .F.  
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
    CLEAR READ
    RETURN TO SMFRMCD.SPR
  ENDIF  
ENDIF  
