*:************************************************************************
*: Program file  : ARIA.PRG
*: Program desc. : Main Starting Program
*:         System: Aria advantage series Ver. 3.0
*:         Module: Main system 
*:           Date: 1993-2002 
*:************************************************************************
*: Parameters lcSysFiles  Path of system files directory to be used
*:************************************************************************
*: Modifications : 
*: B037224,1 MAH Set developmnet env.
*: N119812,1 HBG 16/11/2003 Modify Function "gpStDyBrow" & "SDYEBROW" by adding new parameter 
*: N119812,1				with .T. or .F. to check if use style configuration 
*: N037377,1 AMH 01/04/2004 Add new function to browse the cost sheet ID.
*: N119813,1 AMH 01/06/2004 Modify the gfSheetItem function to work with SQL tables and 7 cost elements.
*! N037401,1 HBG 02/11/2004 Call configuration screen from gfConfgBrw function 
*: N037782,1 AMH 04/05/2004 Modify the gfItemMask function to get style and material code structure.
*! B037981,1 MAH 04/17/2004 Toolbar is active while aria message displayed.
*! N119687,1 WSH 04/21/2004 1) Chnge the title of scale browse from "Scale" to "Size Scale" 
*! 				 04/27/2004 2) Fix Bug in Open To Sell Browse gfOTSDisp()
*! N038038,1 SMM 05/05/2004 Filling an array with the filter expression for each filter
*! E038150,1 AMH 06/14/2004 Consider the inventory types in gfSheetItem function.
*! B038241,1 AKA 06/29/2004 Update the GLLink Code with DEFDEF with the default Style Link Code [Start]
*! B038253,1 AMH 06/30/2004 Consider the Dyelot in browse gfOTSDisp function.
*! B038229,1 MAH 07/12/2004 Fox icon appears for a while.
*! E038245,1 WSH 07/18/2004 Remove Quantity fields from Item file and Calculate Them from ItemLoc file.
*! B038282,1 SMM 07/12/2004 Missing function
*! E038220,1 WSH 07/28/2004 Create Global Function to Get UOM Data from UOM SQL file.
*! B123842,1 WSH 08/05/2004 Browse Locations Using Aria Global Browse Screen.
*! B123704,1 SMM 08/09/2004 Add gfSoftSeek that opens the ..\sy\softseek.scx.  
*! N120718,1 HBG 08/10/2004 FIX bug variable 'FOR' not found [Begin]
*! E038428,1 MAH 08/23/2004 The color background is not clear.
*! B038457,1 SMM 09/01/2004 Fixed error If there are spaces in the path an error occured 
*! N038470,1 SMM 09/06/2004 Add function gfConvertDataType 
*! E121005,1 WSH 09/12/2004 Remove the '?' from the "lcItem" to locaate right.
*! E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*! N037249,1 SMM 09/25/2004 Add function gfVldUsrFld
*! E037237,2 MAH 10/01/2004 Enhance the interface of color scheme.
*! B038623,1 MAH 10/12/2004 Fix preference problems.
*! E124065,1 AMH 10/27/2004 Add color name to style and material browse.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*! B125765,1 HMA 12/21/2004 assign initial value to the variable llReturn in(gfUserPriv).
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*! E037885,4 MAH 02/16/2005 Remove the host form.
*! E037885,4 MAH 02/17/2005 Remove the host form.
*! B039071,1 MAH 02/24/2005 Close Group Problem.
*! B999999,1 MAH 03/01/2005 Fix apply filter problem.
*! B039082,1 WSH 03/02/2005 Enhance ItemBrowse Function and Display Fabric Pictures.
*! B999999,1 MMT 03/14/2005 Define a public array for temp. files
*! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values.
*! B039255,1 KHM 04/27/2005 Fix bug of alias sydfieldtmp is not found
*! B127465,1 SMM 05/08/2005 Invalid Email [START]
*! N037773,1 WSH 05/17/2005 Declare variables as local as they may cause an infinite loop in "gfVldUsrFld" function.
*! B128044,1 WSH 05/18/2005 Fix bug of displaying only one color while browsing Fabrics from Material PO.
*! B128052,1 AMH 05/31/2005 Fix bug of long time to log in.
*! B128052,1 MAH 06/05/2005 Takes a long time to log in.
*! E037241,2 MAH 04/17/2005 Browse User Defined Sort.
*! N037635,1 WSH 07/04/2005 Don't remove the passed OnSelect function if it is not a methiod in a FormSet Object.
*! B039435,1 AMH 07/07/2005 Fix bug of there is quat in the item value.
*! B039435,1 MAH Fix bug of there is a quote in the field value.
*! B128950,1 AMH 07/18/2005 Fix bug of variable not found when browse.
*! B128949,1 WSH 08/07/2005 Read Total Quantity Fields from Item File.
*! N039625,1 MAH 08/24/2005 Enhance Production Schadule Process.
*! B131608,1 WSH 05/03/2006 Prepare to use gfAddBrwDy instead of gfItmDyBrw in Item Inventory.
*! B040211,1 ASM 05/10/2006 Bug in the Global Browse
*! B132026,1 WSH 05/16/2006 Modify gfPhoneTem to allow getting Phone Template for any Country.
*! B132139,1 WSH 05/18/2006 Modify gfOTSDisp() function to work with SQL files.
*! B040241,1 ASM 05/18/2006 Bug in gfReplace, fixed in gfGetAlias
*! B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File (T20060914.0001)
*! B607915,1 MMT 12/28/2006 bug of error in Po screen while save
*! N000587,1 HBG 02/22/2007 Ignore non costing elements in gfSheetItem function (T20061108.0002)
*! B608122,1 TMI 06/13/2007 Allow gfSequence function to deal with the SQL server side tables (T20070524.0013)
*! B608139,1 WAM 06/21/2007 Change function gfCrdtBrow to browse credit memos from SQL tables not from FOX tables
*! B608180,1 TMI 07/29/2007 Allow for same fabric color to added more than once in BOMLINE file with differenct operation of totcost (T20070627.0008 )
*! B608156,1 SSH 07/29/2007 Allow the user to unlock record from history options
*! B608223,1 MMT 08/16/2007 fix bug of wrong unit cost saved in Ctktbom[T20070806.0018]
*! B608245,1 MMT 08/28/2007 fix bug of not browsing stores orders[T20070815.0020]         
*! B608376,1 WAM 12/09/2007 Fix calculation of CT cost sheet required quantity.
*! N000587,1 WAM 12/10/2007 Get foreign and equivalent cost from PO cost sheet lines
*! B608402,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [End]
*! B608546,1 TMI 05/08/2008 assign the value of the optin lcBrPushB to tcOptions and then use the last in the browse.scx screen to not trim the sent key
*                      in case of fox tables as this causes problems when keying in the browse screen is the keyed field in the second key in the 
*                      current order and the first key has spece in its right
*! B608644,1 MMT 08/05/2008 Fix bug of error in gfAddUserInfo due data session change[T20080603.0010]
*! B608542,1 WAM 08/14/2008 Get material cost based on latest exchange rates  TICKET#[T20080729.0001] [Start]
*! B608684,1 MMT 09/10/2008 call gfStyBrw when browse style file in option grid[T20080820.0006]      
*! E302555,1 MMT 09/18/2008 Add Syustatc File For Aria4 and count A4 users     [T20070202.0001]
*! B608749,1 WAM 12/02/2008 while creating a PO cost sheet, increament the NLINENO field in BOMLINE when the value of 
*! B608749,1 WAM 12/02/2008 the unique key repeated.
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS
*! B608833,1 HES 04/01/2009 Add Standared Fields for POMLINE File   [T20081001.0001]
*! B608884,1 WAM 06/02/2009 Include Actualized PO in OTS calculation [T20090512.0001]
*! E302618,1 MMT 06/17/2009 Add New GL Categories 029,030 For HST,PST Taxes[T20090605.0009]
*! B608900,1 WAM 06/21/2009 Fix filter expression for logic fields [T20090617.0115]
*! B608902,1 MMT 06/22/2009 Fix bugs of not updating PO Cost Sheet when edit PO[T20090612.0013]
*! B609060,1 MMT 10/25/2009 Fix bug of 'gpExit.Prg' is not Found{T20090911.0005}
*! C201210,1 MMT 01/11/2010 Add New Parameters to Style Configuration screen[T20090421.0032]
*! C201210,2 MMT 02/09/2010 Add New Parameters to Style Configuration screen for all Scale case[T20090421.0032]
*! B609140,1 MMT 02/10/2010 Fix bug of error while cancelling Sales order [T20100129.0001]
*! B609159,1 MMT 03/01/2010 CHKPRD open cursor from syccomp when called & doesn't close [T20080717.0001]
*! B609159,2 MMT 03/17/2010 CHKPRD open cursor from syccomp when called & doesn't close [T20080717.0001]
*! B609207,1 MMT 04/12/2010 Error when more than one user try to open Rolls screen from PO Rec. Screen[T20100330.0014]
*! E302692,1 TMI 05/10/2010 Split the Aria.exe [T20100418.0003]
*! B609246,1 MMT 05/13/2010 Fix problem of Wrong unit Qty in Ctktbom file when PO edited[T20100316.0001]
*! B609253,1 MMT 05/17/2010 Release variables from gfSheetItem Function after usage[T20100427.0002]
*! B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[T20100518.0026]
*!* B609356,1 SMA 07/27/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*! B609424,1 MMT 10/11/2010 Fix bug of Error in Rebalance program (PO module)[T20100831.0014]
*! B609498,1 MMT 01/13/2010 Fix bug of wrong estimated cost for style comp. in PO Screen[T20101101.0001]
*! B609507,1 TMI 01/20/2011 replace the SEEK command with gfSeek for the shpmthdr file [T20101108.0012]
*! B609526,1 MMT 02/17/2011 Error in Automatic allocation screen OG[T20110201.0005]
*! B609533,1 SAB 02/22/2011 erro prevents adding material to PO cost sheet when  color starts with * [T20110211.0003]
*! B609572,1 MMT 04/19/2011 Fix bug of wrong Costing elements desc. in case of detail cost No Styles[T20110405.0007]
*! B609487,1 WAM 01/23/2011 Display System updates [T20110109.0001]
*! E302857,1 HES 02/10/2011 Avoid 'X:\' Fixed Path [T20110206.0017]
*! B609677,1 MMT 10/03/2011 Fix bug of wrong scale in SO screen after adding style to location [T20110808.0008]
*! B609711,1 MAH 11/23/2011 Separate Business From Aria.exe Begin [T20110803.0001]
*:************************************************************************
LPARAMETERS lcSysFiles

* B038229,1 MAH Show Aria title bar instead of "Microsoft Visual FoxPro"
_SCREEN.Caption = "Aria 4 XP"
* B038229,1 MAH End

CLEAR
*ON ERROR Do gfErrorTrap
DEACTIVATE WINDOW "Project Manager"
*CLEAR ALL CLASS
*_screen.WindowState = 2 
*_VFP.Visible = .T.

*-- Include the .H file
#INCLUDE R:\ARIA4XP\PRGS\SY\ARIA.H

*-- All public vars will be released as soon as the application
*-- object is created. 
PUBLIC gcBaseWind, gnProgCopy, glInitMenu,gnMaxUsers,gcPrmtMdls,gcAct_Key,laCtrStat
PUBLIC glToolActv,glMsgREm,glUser_tsk,glAutoAdd,GlLog_Requ,glSys_log,GLCHNGPASS,gcAct_Comp

*! N039625,1 MAH 08/24/2005 [BEGIN]
PUBLIC gcAct_Appl
*! N039625,1 MAH 08/24/2005 [END]

*--E000000,1 Hesham (Start)
*PUBLIC oAriaApplication,gcPlatForm,gcLicence,gcCompName,lcProgName,glErrorHan
PUBLIC oAriaApplication,gcPlatForm,gcLicence,gcCompName,lcProgName,glErrorHan,gcUser_lvl,glLockSys
*--E000000,1 Hesham (End)

*--B999999,1 MMT 03/14/2005 Define a public array for temp. files[Start]
PUBLIC laPrgTemps[1,1],gcCurResouce
DIMENSION laPrgTemps[1,1]
*--B999999,1 MMT 03/14/2005 Define a public array for temp. files[End]

STORE .F. TO glToolActv,glMsgREm,glUser_tsk,glAutoAdd,GlLog_Requ,glSys_Log,glErrorHan,GLCHNGPASS
STORE "  " TO gcAct_Comp,gcLicence,gcCompName
*--E000000,1 Hesham (Start)
gcUser_lvl = "A"
glLockSys = .F.
*--E000000,1 Hesham (End)
llAddRec = .T.
*--Browse coordinates for (Full or Half screen view).
PUBLIC gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,gnBrHSRow1,gnBrHSCol1,gnBrHSRow2,gnBrHSCol2 
STORE 0 TO gnBrFSRow1,gnBrFSCol1,gnBrHSCol1
STORE SCOLS()-1 TO gnBrFSCol2,gnBrHSCol2
gnBrFSRow2 = SROWS()-3   
gnBrHSRow1 = SROWS()/2                       
gnBrHSRow2 = SROWS()/2-3                     
gcact_appl=''
gnMaxUsers= 0
PUBLIC gnLockTry
gnLockTry = -2                 && No. of attemptes to lock files or records  

lcCurrentProcedure = UPPER(SYS(16,1))
* B037224,1 Set developmnet env.
*lcCurrentProcedure = STRTRAN(lcCurrentProcedure, "\PRGS", "")
lcCurrentProcedure = STRTRAN(lcCurrentProcedure, "\PRGS\SY", "")
* B037224,1 END
lnPathStart        = AT(":",lcCurrentProcedure)- 1
lnLenOfPath        = RAT("\", lcCurrentProcedure) - (lnPathStart)
lcCurPath          = SUBSTR(lcCurrentProcedure, lnPathStart, lnLenofPath)
SET DEFAULT TO (lcCurPath)


*B609711,1 MAH 11/23/2011 Separate Business From Aria.exe Begin 
LOCAL lcAriaMainPath
lcAriaMainPath = "SET PROCEDURE TO " + lcCurPath + "\PRGS\SY\ariamain.fxp ADDITIVE "
&lcAriaMainPath.
*B609711,1 MAH 11/23/2011 End


*! B128052,1 MAH 06/05/2005 [BEGIN]
= gfCreatePerfLog(lcCurPath)
= gfAddPerfLog("LOGIN", "ARIA_PRG", "START", "", .T.)
*! B128052,1 MAH 06/05/2005 [END]

SET CLASSLIB TO

lcClasPath  = lcCurPath + "\CLASSES\"
lnClasses   = ADIR(laClasses, lcClasPath + "*.*", "D")
FOR lnClass = 1 TO lnClasses
  IF "D" $ laClasses[lnClass,5] AND !INLIST(laClasses[lnClass,1], ".", "..")
    = lpSetLibs (lcClasPath + laClasses[lnClass,1])
  ELSE
    = lpSetFile (lcClasPath, laClasses[lnClass,1])
  ENDIF
ENDFOR
RELEASE lcClass, lnClasses, lnClass, lcClasPath, lnLenOfPath, lnPathStart, lcCurrentProcedure

*! B128052,1 MAH 06/05/2005 [BEGIN]
= gfAddPerfLog("LOGIN", "ARIA_PRG", "AFTER_SET_CLASS_LIB", "")
*! B128052,1 MAH 06/05/2005 [END]

*-- Hesham (start)
*-- pass the system files directory passed from command line to the ariaapplication
*-- object
*oAriaApplication = CREATEOBJECT("AriaApplication")


*! E038142, 2 MAH 08/31/2004 Create temp oAriaApplication to hold DBEngines constanct becasue its used while init 
*!                           AriaApplication Class [BEGIN]
CREATE CURSOR Temp1(cSQLDBID c(3), cFOXDBID c(3), cNativeDBID c(6))
APPEND BLANK
REPLACE cSQLDBID WITH 'SQL', ;
        cFOXDBID WITH 'FOX', ;
        cNATIVEDBID WITH 'NATIVE'
SCATTER FIELDS cSQLDBID, cFOXDBID, cNativeDBID NAME oAriaApplication
*! E038142, 2 MAH 08/31/2004 [END]

oAriaApplication = CREATEOBJECT("AriaApplication",IIF(TYPE('lcSysFiles')='C',lcSysFiles,.F.))

*E302692,1 [T20100418.0003] Split the Aria.exe TMI 5/10/2010 [START] 
** set the path to the folders aria4xp\Screens\SY and aria4xp\Prgs\SY
** all screens and programs in either folder will be seen to all of aria programs and could be 
** called without putting the path to the needed file to call
LOCAL lcPath
lcPath = oAriaApplication.Screenhome+'SY\;'+oAriaApplication.Programhome+'SY\'
SET PATH TO (lcPath) ADDITIVE
RELEASE lcPath
*E302692,1 [T20100418.0003] Split the Aria.exe TMI 5/10/2010 [ END ] 


*! B128052,1 MAH 06/05/2005 [BEGIN]
= gfAddPerfLog("LOGIN", "ARIA_PRG", "AFTER_CREATE_ARIA_APPLICATION", "")
*! B128052,1 MAH 06/05/2005 [END]

*-- Hesham (End)
IF TYPE('oAriaApplication') = "O"
  oAriaApplication.Do()

  *! B128052,1 MAH 06/05/2005 [BEGIN]
  = gfAddPerfLog("LOGIN", "ARIA_PRG", "AFTER_CALL_APPLICATION_DO", "")
  *! B128052,1 MAH 06/05/2005 [END]

  oAriaApplication.oToolBar = null
  oAriaApplication = null
ENDIF

*! B128052,1 MAH 06/05/2005 [BEGIN]
= gfAddPerfLog("LOGIN", "ARIA_PRG", "END", "")
*! B128052,1 MAH 06/05/2005 [END]

SET CLASSLIB TO
*CLEAR CLASSLIB MAIN
CLEAR CLASSLIB CLASSES\MAIN
CLEAR DLLS
RELEASE ALL EXTENDED
CLEAR ALL


* ------------------------------------------------------------------------------

PROCEDURE lpSetLibs
LPARAMETERS lcPathToSearch
PRIVATE lnClasses, lnClass, laClasses

lcPathToSearch = IIF(RIGHT(lcPathToSearch,1)="\", lcPathToSearch, lcPathToSearch+"\")
lnClasses   = ADIR(laClasses, lcPathToSearch + "*.VCX")
FOR lnClass = 1 TO lnClasses
  = lpSetFile (lcPathToSearch, laClasses[lnClass,1])
ENDFOR

* ------------------------------------------------------------------------------

PROCEDURE lpSetFile
LPARAMETERS lcPath, lcClassFileName 
PRIVATE lcClass, lcCommand

IF UPPER(RIGHT(lcClassFileName, 3)) = "VCX"
  lcClass   = lcPath + lcClassFileName 
  lcCommand = SPACE(0)
  CLEAR CLASSLIB (lcClass)
*!*	  *E302692,1 [T20100418.0003] Split the Aria.exe TMI 5/10/2010 [ END ] comment out this part as all classes will be excluded ( note, building aria.exe used the commented version, pls comment when the testing pass successfully )
*!*	  IF INLIST(UPPER(lcClassFileName), "MAIN.VCX", "GLOBALS.VCX", "UTILITY.VCX")
*!*	    lcCommand = "IN ARIA.EXE"
*!*	  ENDIF
*!*	  *E302692,1 [T20100418.0003] Split the Aria.exe TMI 5/10/2010 [ END ] 

  *B605053,1 Hassan [Begin]
  *IF !(lcClassFileName $ SET("CLASSLIB"))
  IF !("\"+lcClassFileName $ SET("CLASSLIB"))
    SET CLASSLIB TO (lcClass) &lcCommand ADDITIVE
  ENDIF
  *B605053,1 Hassan [End]
ENDIF

*!*************************************************************
*! Name      : gfTempName
*! Developer : Wael Ali Mohamed
*! Date      : 2002
*! Purpose   : Creat temp file name
*!*************************************************************
*! Calls     : 
*!      GetTempName
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfTempName
lcRetName = ("X"+SUBSTR(SYS(2015),4))

IF TYPE('oAriaApplication.WorkDir') = 'C'
  DO WHILE FILE(oAriaApplication.WorkDir+lcRetName+'.DBF')
    lcRetName = ("X"+SUBSTR(SYS(2015),4))
  ENDDO
ENDIF 
IF TYPE('laPrgTemps')#'U' 
  IF !EMPTY(laPrgTemps[1])
    DIMENSION laPrgTemps[ALEN(laPrgTemps,1)+1,1]
  ELSE  
    DIMENSION laPrgTemps[1,1]  
  ENDIF
  laPrgTemps[ALEN(laPrgTemps,1),1] = lcRetName
ENDIF
RETURN lcRetName

*!***********************************************************************************************************************
*! Name      	: gfCreatePerfLog
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Create performance log file 
*! Tracking   	: B128052,1 MAH 06/05/2005 Takes a long time to log in.
*!***********************************************************************************************************************
*! Parameters  	: 
*!  lcPath : Path to create file on.
*!***********************************************************************************************************************
PROCEDURE gfCreatePerfLog
LPARAMETERS lcPath

PUBLIC goPrefObject, gcPerfSessionID, gcPerfLogFileName, gcPerfSessionDate, gcPerfSessionTime, gcSessUserID

IF FILE(lcPath + "\PerfData.txt")
  goPrefObject = CREATEOBJECT('vbpPerformance.Measure')
ENDIF

gcPerfLogFileName = lcPath + "\SQLDictionary\SYPREFLG.dbf"
gcPerfSessionID   = GFTempName()
gcPerfSessionDate = DATE()
gcPerfSessionTime = TIME()
gcSessUserID      = ''

LOCAL lnSelected
lnSelected = SELECT()

IF !FILE(gcPerfLogFileName)
*  CREATE TABLE (gcPerfLogFileName) ;
*               (cSessID C(8), cUserID C(10), dSessD D, cSessT C(10), nTime N(10,3), ;   
*                cProcType C(10), cProcID C(30), cProcChkID C(45), ;
*                mResCons M, mMoreInfo M)
*  USE
ENDIF

SELECT(lnSelected)

**********************************************************************************************************************
*! Name      	: gfAddPerfLog
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Add record to performance log file 
*! Tracking   	: B128052,1 MAH 06/05/2005 Takes a long time to log in.
**********************************************************************************************************************
*! Returns 	:
*!  lcProcType     : Porcess Type "LOGIN", "OPEN_SCREEN" ...etc
*!  lcProcID       : Porcess ID
*!  lcProcChkID    : Check Point ID "START", "END" ...etc 
*!  lmMoreInfo     : Additional Information
*!  llGetFullData  : (Optional) Measure all data.
**********************************************************************************************************************
PROCEDURE gfAddPerfLog
LPARAMETERS lcProcType, lcProcID, lcProcChkID, lmMoreInfo, llGetFullData

IF TYPE('goPrefObject') # 'O' .OR. ISNULL(goPrefObject)
  RETURN 
ENDIF

LOCAL lnSelected
lnSelected = SELECT()

IF TYPE('gcSessUserID') = 'C'
  LOCAL lmResCons
  lmResCons = ''
  
  IF llGetFullData .OR. !EMPTY(gcSessUserID)
    LOCAL lnSecFrom 
    lnSecFrom = SECONDS()

    lmResCons = goPrefObject.GetCheckPointResultString(_VFP.ProcessId, llGetFullData)
    
    LOCAL lnSecTo 
    lnSecTo = SECONDS()
    
    lmResCons = lmResCons + CHR(13) + 'CS_GetConsumptionDataTime = ' + STR(lnSecTo - lnSecFrom, 10, 3)
  ENDIF
  
  INSERT INTO (gcPerfLogFileName)(cSessID,         cUserID,      dSessD,            cSessT,           nTime,      cProcType,  cProcID,  cProcChkID,   mResCons, mMoreInfo) ;
                          VALUES (gcPerfSessionID, gcSessUserID, gcPerfSessionDate, gcPerfSessionTime, SECONDS(), lcProcType, lcProcID, lcProcChkID, lmResCons, lmMoreInfo)
ENDIF

IF USED("SyPrefLg")
 USE IN SyPrefLg
ENDIF

SELECT(lnSelected)

*!***********************************************************************************************************************
*! Name      	: gfPerfLogUserInfo
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Assign user info to performance log file 
*! Tracking   	: B128052,1 MAH 06/05/2005 Takes a long time to log in.
*!***********************************************************************************************************************
*! Parameters  	: 
*! lcUserID : User ID
*!***********************************************************************************************************************
PROCEDURE gfPerfLogUserInfo
LPARAMETERS lcUserID

IF TYPE('goPrefObject') # 'O' .OR. ISNULL(goPrefObject)
  RETURN 
ENDIF

LOCAL lnSelected
lnSelected = SELECT()

IF EMPTY(lcUserID)
  gcSessUserID = .F.
  
  DELETE (gcPerfLogFileName) FROM SyPrefLg WHERE SyPrefLg.cSessID = gcPerfSessionID
ELSE
  gcSessUserID = UPPER(lcUserID)
  
  UPDATE (gcPerfLogFileName) SET cUserID = gcSessUserID WHERE SyPrefLg.cSessID = gcPerfSessionID
ENDIF

IF USED("SyPrefLg")
 USE IN SyPrefLg
ENDIF

SELECT(lnSelected)

*!*************************************************************
FUNCTION gfStation

lcStation = ALLTRIM(GETENV('P_STATION'))

IF !EMPTY(lcStation)
  lcStation = ALLTRIM(SYS(2007,lcStation))
ELSE
  lcStation = SUBSTR(SYS(3),4) 
ENDIF

RETURN (lcStation)





















