*:************************************************************************
*: Program file  : SMNTTMP.PRG
*: Program desc. : NotePad Templates
*: Module        : System Manager
*:         System: Aria Apparel System
*:      Developer: GEHAN 
*:************************************************************************
*: Calls 		 : SMNTTMP.SPR
*:      Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*:*************************************************************
*-- Parameter list (optional)
*-- Variable declarations
STORE '' TO lcScFields, lcTmpFl , lcNtPth, lcPrevComp,;
			lcChWin0  , lcChWin1, lcChWin2,lcNotTmpFl,;
			lcBrow_Ttl,lcWindTitl,lnArLoop,lcCurComp, lcOldVal   
STORE 0 TO puComp
lcTmpStat = 'DISABLE'

DIMENSION laComp[1]


IF !gfSetup()    
  RETURN
ENDIF  
glFromBrow = .F.


*-- Disable browse and delete button in the tool bar
*laCtrStat[ 8] : delete button
*laCtrStat[10] : browse button
*laCtrStat[ 7] : edit button
STORE "DISABLE" TO laCtrStat[8], laCtrStat[10], laCtrStat[7]

*-- Holds the save button to use lpSavScr Procdure
*-- Create temporary file
=lfCrTmpFl()

*-- fields list to be scattered to LADATA array
*lcScFields = 'Capp_id,Cdlobjtyp,Cdlobjid,Mdlobj'
*-- lcKeyType = 'T' For templates.
*-- lcKeyType = 'X' For Account General purpose letters.
*-- lcKeyType = 'Y' For  Vendor General purpose letters.
IF (TYPE('lcKeyType') $ 'UL')
  lcKeyType = 'T'
ENDIF

*-- lcKeyType = 'T' For templates.
*-- lcKeyType = 'X' For Account General purpose letters.
*-- lcKeyType = 'Y' For  Vendor General purpose letters.
*-- Fill viarables according to the paramter.
*lcBrow_Ttl = 'Templates'
*lcWindTitl = 'Notepad Templates'   

lcBrow_Ttl = IIF(lcKeyType = 'T','Templates','General Purpose Letters')
DO CASE
  CASE lcKeyType = 'T'
    lcWindTitl = 'Notepad Templates'   
  CASE lcKeyType = 'X'
    lcWindTitl = 'Customer General Purpose Letters'
  CASE lcKeyType = 'Y'
    lcWindTitl = 'Vendor General Purpose Letters' 
ENDCASE


*-- The first time we run the screen, 
IF !WEXIST(gcBaseWind)
  IF !lfMakArry()  
    RETURN
  ENDIF  
  *-- Initialize some variables
  *-- Create window names
  lcChWin0   = gfTempName()
  lcChWin1   = gfTempName()
  lcChWin2   = gfTempName()  
  lcNotTmpFl = gfTempName()  
  IF EMPTY(gcAct_Comp)
    puComp   =1
  ELSE
    =lfGetActCo()
    =lfOpenDbf()
    llNoShow  = .F.
  ENDIF
ELSE
  *-- Reinitialize popup variables according to the restored laData elements.
   =lfwBrWhen()
ENDIF  

*--Trapping key
PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrow_Ttl)
DO (gcScrDir + gcWinAppl + '\SMNTTMP.SPX')
RELEASE WINDOW (lcBrow_Ttl)
POP KEY

IF glQuitting
  *-- Erase temp files
  =lfDlTmpFl()
ENDIF  

*--------------------------------------------------------------------------*
FUNCTION lpShow
*--------------------------------------------------------------------------*
*-- Disable browse and delete button in the tool bar
SHOW GET pbBrws DISABLE
SHOW GET pbDlt  DISABLE
SHOW GET pbEdt  DISABLE
DO CASE
  *-- If select mode
  CASE laScrMode[1]
    SELECT (lcTmpFl)
    ZAP
    puComp=1
    lcTmpCod=SPACE(20)
    lcTmpDesc=SPACE(35)
    *SHOW GET lcTmpCod 
    SHOW GET pbRemove  DISABLE
    SHOW GET pbNotes   DISABLE
    SHOW GET lcTmpDesc DISABLE
    =lfBrowse()
 *-- If view mode
  CASE laScrMode[2]
    lcTmpCod=SPACE(20)
    lcTmpDesc=SPACE(35)
    SHOW GET lcTmpCod DISABLE
    SHOW GET pbRemove  &lcTmpStat
    SHOW GET pbNotes   &lcTmpStat
    SHOW GET lcTmpDesc &lcTmpStat
    RELEASE WINDOW (lcBrow_Ttl)
    =lfBrowse()
  *-- If edit
  *CASE laScrMode[3]
  *  =lfwBrWhen()
  *  SHOW GET lcTmpCod DISABLE
  *  SHOW GET lcTmpDesc ENABLE
  *  RELEASE WINDOW (lcBrow_Ttl)
  *  =lfBrowse()
      
  *-- If add mode
  *CASE laScrMode[4]
ENDCASE


*!*************************************************************
*! Name      : lfBrowse
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Browse function
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfBrowse()
*!*************************************************************
FUNCTION lfBrowse
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

IF laScrMode[2]
  SELECT (lcNotTmpFl)
  GO TOP
ELSE
 SELECT (lcTmpFl)
 GO TOP
ENDIF

*-- Fill viarables according to the paramter.
*lcBrow_Ttl = 'Templates'
*lcWindTitl = 'Notepad Templates'   
lcBrow_Ttl = IIF(lcKeyType = 'T','Templates','General Purpose Letters')

BROWSE  FIELDS Key:H="Template":35,cDesc:H="Description":100;
	    KEY lcKeyType	 ;
		NOMENU			 ;         
    	NOAPPEND         ;
    	NOEDIT           ;
    	NODELETE         ;
		SAVE			 ;
		NOWAIT			 ;
		NOCLEAR			 ;
		WINDOW (lcChWin1);
	    IN WINDOW (gcBaseWind);
	    TITLE (lcBrow_Ttl)  ;
	    WHEN lfwBrWhen()

SELECT (lnCurAlias)

*-------------------------------------------------------------------------*
*!*************************************************************
*! Name      : lfwBrWhen
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Dispalys current record in the browse window in two other variables
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfwBrWhen()
*!*************************************************************

FUNCTION lfwBrWhen
*--Displayes the current record in the browse window in the two variables
*-- Selecting Master file in view mode 
*-- Selecting Temporary file(Empty File) in Select mode
IF laScrMode[2]
  SELECT (lcNotTmpFl)
  SET ORDER TO TAG NotePad  
  lcTmpCod=KEY
  lcTmpDesc=CDESC
ELSE
  SELECT(lcTmpFl)
  lcTmpCod  = KEY
  lcTmpDesc = CDESC
ENDIF
*Ren
*SHOW GET lcTmpCod
*SHOW GET lcTmpDesc
IF EMPTY(key)
  lcTmpStat = 'DISABLE'
  SHOW GET lcTmpCod  DISABLE
  SHOW GET pbRemove  DISABLE
  SHOW GET pbNotes   DISABLE
  SHOW GET lcTmpDesc DISABLE
*Ren
ELSE
  lcTmpStat = 'ENABLE'
  SHOW GET lcTmpCod  DISABLE
  SHOW GET pbRemove  ENABLE
  SHOW GET pbNotes   ENABLE
  SHOW GET lcTmpDesc ENABLE
*Ren
ENDIF    
*!*************************************************************
*! Name      : lfNew
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Adding a New Record
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfNew()
*!*************************************************************

FUNCTION lfvNew
*--Allows the addition of a new record in the notepad file 	
*-------------------------------------------------------------------------*
lcTmpCod  = SPACE(20)
lcTmpDesc = SPACE(35)
_CUROBJ = OBJNUM(lcTmpCod)
SHOW GET lcTmpCod ENABLE
*Ren
*SHOW GET lcTmpDesc
SHOW GET lcTmpDesc DISABLE
*Ren end

*!*************************************************************
*! Name      : lfRemove
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Deleting the current record
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   : lfRemove()
*!*************************************************************

FUNCTION lfvRemove
*--Allows the deletion of a current selected record in the notepad file 	
*-------------------------------------------------------------------------*
IF gfModalGen("QRM00007B00007","ALERT") = 1
*-- Marks records as deleted when pressing remove button
 IF !EOF()
   DELETE
   *-- Clears the data in two variables 
   lcTmpCod  = SPACE(20)
   lcTmpDesc = SPACE(35)
   SHOW GET lcTmpCod 
   SHOW GET lcTmpDesc
  SHOW WINDOW (lcBrow_Ttl) REFRESH
 ENDIF 
ENDIF
*-- IF KEY IS EMPTY (BROWSE WINDOW IS EMPTY)
IF EMPTY(key)
  lcTmpStat = 'DISABLE'
  SHOW GET pbRemove  DISABLE
  SHOW GET pbNotes   DISABLE
  SHOW GET lcTmpDesc DISABLE
ENDIF    

*-------------------------------------------------------------------------*
*!*************************************************************
*! Name      : lfCrTmpFl
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Creates an Empty Template File
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfCrTmpFl()
*!*************************************************************

FUNCTION lfCrTmpFl
*-->> When entering the prog
*-- Create a temp File in the work dir(gcWorkDir)	
*--Using the global function gfTempName 
*-- Creates an index for the file
*-- Sets the table order to this index 
lcTmpFl = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpFl) (TYPE C(1),;
       KEY C(20),CDESC C(35),MNOTES M(10))
INDEX ON TYPE+KEY TAG (lcTmpFl) ADDITIVE
SET ORDER TO TAG (lcTmpFl)  
  
*-------------------------------------------------------------------------*    
*!*************************************************************
*! Name      : lfDlTmpFl
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Deletes Template file,its(.CDX) & (.FPT) Files
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfDlTmpFl()
*!*************************************************************

 FUNCTION lfDlTmpFl
 *-->> When quiting the prog
 *--Closes temp file to delete it before quitting the system
 *-- Delete also the index(.CDX) file
 *-- Delete memo file (.FPT) file
IF USED (lcTmpFl)
  USE IN (lcTmpFl) 
ENDIF
ERASE (gcWorkDir + lcTmpFl + '.DBF')
ERASE (gcWorkDir + lcTmpFl + '.FPT')
ERASE (gcWorkDir + lcTmpFl + '.CDX')  
  
*-------------------------------------------------------------------------*    
*!*************************************************************
*! Name      : lfMakArry
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Makes the Array to fill the Company PopUp with.
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfMakArry()
*!*************************************************************
FUNCTION lfMakArry
*-- Opens the file needed to fill our array (laComp) with 3 fields from it
*-- Then using this array (laComp) to fill the puComp popup 
PRIVATE lnCurAlias, llRetVal
lnCurAlias = SELECT(0)   && save current work ara
=gfOpenFile(gcSysHome+'SYccomp', 'Ccomp_ID', 'SH')
SELECT 	Ccomp_Id + " - " + Ccom_Name,Ccom_Ddir ;
  FROM 	SYccomp      ;
  INTO 	ARRAY laComp ;
 ORDER  BY Ccom_Name
IF _TALLY = 0
  *-- Message 
  *** No companies available. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00189B00000","DIALOG","companies")
  llRetVal = .F.
ELSE
  llRetVal = .T.
  DIMENSION laComp[ALEN(laComp,1) +1, 2]
  =AINS(laComp, 1)
  laComp[1,1] = "Select a company"
  laComp[1,2] = "Select a company"
  puComp = 1
ENDIF  
SELECT (lnCurAlias)
RETURN llRetVal

*-------------------------------------------------------------------------*    
*!*************************************************************
*! Name      : lfvPuComp
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Checks the value of the Company PopUp
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  lfvPuComp()
*!*************************************************************

FUNCTION lfvPuComp
*-- Checks that selected company is different then the last one & not("Select a Company")
IF lcPrevComp <> laComp[puComp,1] AND puComp <> 1
  =lfOpenDbf()
  SHOW GETS
ENDIF
*!*************************************************************
*! Name      : lfOpenDbf
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Opens the Dbf of the Selected Company
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :lfOpenDbf()

FUNCTION lfOpenDbf
*-- Passes to a variable the directory of the dbfs of 
*--The selected company file name (NOTEPAD)
lcNtPth  = gfGetDataDir(ALLTRIM (laComp[puComp,2])) +"NotePad"
IF USED(lcNotTmpFl)
  USE IN (lcNotTmpFl)
  *=gfCloseFile("NOTEPAD")
ENDIF
=gfOpenFile(lcNtPth, "NOTEPAD", "SH", @lcNotTmpFl, .T.)
lcTmpStat = IIF(SEEK(lcKeyType), 'ENABLE', 'DISABLE')
laScrMode    = .F.
laScrMode[2] = .T.
*---------------------------------------------------------------------------*
FUNCTION lfwCoName
*-- passses the value of the previously selected company to variable(lcPrevComp)
lcPrevComp = laComp[puComp,1]
*!*************************************************************
*! Name      : lfvDesc
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Updates the cDesc field in NotePad file.
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :lfvDesc()

FUNCTION lfvDesc
PRIVATE lnCurAlias
IF lcTmpDesc <> lcOldVal
  lcTmpDesc = PADR(ALLTRIM(lcTmpDesc), 35)
  *-- When quitting the description variable
  lnCurAlias = SELECT(0)
  SELECT (lcNotTmpFl)
  REPLACE cDesc WITH lcTmpDesc
  SELECT (lnCurAlias)
  SHOW WINDOW (lcBrow_Ttl) REFRESH
ENDIF
 
*---------------------------------------------------------------------------* 
*!*************************************************************
*! Name      : lfvNotes
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Opens the NotePad prg with the Memos of the selected Record
*!*************************************************************
*! Calls     :  NotePad Prg
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :lfvNotes()

FUNCTION lfvNotes
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
*-- NOTEPAD button that runs notepad program
lcNtPth=ALLTRIM (laComp[puComp,2])
=NOTEPAD(lcKeyType, &lcNotTmpFl..KEY,.F.,lcNtPth)
SELECT (lnCurAlias)
*---------------------------------------------------------------------------* 
*!*************************************************************
*! Name      : lfGetActCo
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Sets the popup value on the Global Active Company
*!*************************************************************
*! Calls     :  None.
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :lfGetActCo()

FUNCTION lfGetActCo
*--Gets the Active Company & select it
PRIVATE lcTemp , lnActLen

*-- The active Company String length
lnActLen = LEN(gcAct_Comp)

*-- Loop through the array to find the active company Name according to
FOR lnArLoop = 1 To ALEN(laComp, 1)
  IF LEFT(laComp[lnArLoop,1],2) = gcAct_Comp
   puComp = IIF(lnArLoop =0, 0, AELEMENT(laComp,lnArLoop))
   EXIT
  ENDIF
ENDFOR
*---------------------------------------------------------------------------* 
*!*************************************************************
*! Name      : lfvTmpCod
*! Developer : Gehan
*! Date      : 03/25/1999
*! Purpose   : Validates the Key of the newly added records
*!*************************************************************
*! Calls     :  None
*!*************************************************************
*! Parameters:  None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :lfvTmpCod()

FUNCTION lfvTmpCod
lcTmpCod = PADR(ALLTRIM(lcTmpCod), 20)
IF !EMPTY(lcTmpCod)
  *-- Validates key   
  IF !SEEK(lcKeyType+lcTmpCod)
    *-- Inserts the newly added records in the temp file 
    INSERT INTO (lcNotTmpFl);
     	    (TYPE, Key);
  	  VALUES(lcKeyType, lcTmpCod)
  	  
    SHOW GET lcTmpCod  DISABLE     
    *Ren comment the IF..ENDIF
*    IF lcTmpStat = 'DISABLE'
      lcTmpStat = 'ENABLE'
      SHOW GET pbRemove  ENABLE
      SHOW GET pbNotes   ENABLE
      SHOW GET lcTmpDesc ENABLE
 *   ENDIF  
    SHOW WINDOW (lcBrow_Ttl) REFRESH
  ELSE 
*--Message terminate an already existing key
   =gfModalGen("TRM00352B00000","DIALOG",ALLTRIM(lcKeyType))
 *--Empty the Code & Descrip.
    lcTmpCod = SPACE(20)
    *SHOW GET  lcTmpCod ENABLE     
    lcTmpCod=KEY
  ENDIF
ELSE
 *--Message to enter Code
 *Ren
 * wait window "Please Enter the Code"
 SHOW GET lcTmpCod &lcTmpStat
ENDIF   

FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))

FUNCTION lfAct
IF glFromBrow
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfDeact
*! Developer : Gehan
*! Date      : 05/27/99
*! Purpose   : Controls the Tab key when browse window is deactivated
*!*************************************************************
*! Calls     : lpTab.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   : lfDeact() 
*!*************************************************************

FUNCTION lfDeact
IF WONTOP()=lcBrow_Ttl
  glFromBrow = .T.
  ON KEY LABEL TAB         DO lpTab WITH lcChWin2, OBJNUM(lcTmpCod)
  IF laScrMode[1] 
    ON KEY LABEL BACKTAB   DO lpTab WITH 'lcChWin1, OBJNUM(puComp)
  ELSE
    ON KEY LABEL BACKTAB   DO lpTab WITH 'gwcContrl1', OBJNUM(pbCls)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Renee
*! Date      : 07/01/1996
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'ORD100P', 'pbApply'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lnObjNum
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum








*---------------------------------------------------------------------------* 