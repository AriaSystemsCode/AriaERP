*:************************************************************
*: Program file  : icInvLo
*: Program desc. : Style Inventory Location
*: For screen    : icInvLo
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (IC).
*:     Developer : Mohamed Atia Badran (MAB)
*:************************************************************
*: Calls : 
*:         Procedures : lpShow,lpSavscr,lpTab,lpBackTab
*:         Functions  : gfGetMemVar,gfModalGen,gfSetup        ,
*:                    : gfOpenFile,gfTempName,gfItemMask      ,
*:                    : gfStyBrw,gfSeekRec,gfStopBrow,gfMover ,
*:                    : gfGLBrowse,gfGettime,lfWHBrow,lfAct   ,
*:                    : lfwWareBrs,lfvStyle,lfVBrow,lfRefresh ,
*:                    : lfvLoc,lfvWareHs,lfvRem,lfRemCal      ,
*:                    : lfvGLWareHs,lfOldValue,lfDeAct 
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO icInvLo
*:************************************************************
*E300739,1  BADRAN 10/20/1997 Allocation style in warehouses
*:************************************************************
*: Modifications :
*E301077,22 MAB 01/05/1999 Open and close files.
*B602440,1  Walid Abou El-Magd 08/18/1999 Fix the bug "Alias Codes not found"
*           Open the Codes file to be used by the global browes from the main menu
*B602778,1  Walid Abou El-Magd 08/18/1999 Fix the bug "Illegal value"
*           when the style code structure has not been setuped . 
*B606803,1 ASH 12/19/2002 Fix bug of 'Alias not found' in case of keep track of bins = 'N'
*:************************************************************

*-- laData     is array to hold the values of the header file.
*-- laScrMode  is array to hold the values of screen mode.
*-- laKeyField is array to hold the information of the key fields.
*-- laSource   is array to hold the whole available warehouses or loactions to send it to mover.
*-- laTarget   is array to hold the assigned warehouses or loactions to send it to mover.
DIMENSION laData[1],laScrMode[4],laKeyField[1,4],laSource[1],laTarget[1] 

*-- Save the value of arrays to reuse it when return from another session
STORE SPACE(0) TO laData,laSource,laTarget

*-- This is to prevent the global save
laDefProc[9] = .F.                 

*-- If this Company use Extended size scale create laExtend array
*-- to hold extended scale only.   
*-- for example laExtend[1] = "EE1", laExtend[2] = "EE2", and so on.
DIMENSION laExtend[1]
laExtend = ''   && assign spaces to array.

*-- Logical Variables saved when goto another session.
*-- glFromBrow : This is a flag to control a trapping for browse screens
*-- llBrowse   : This is a flag set it when we push a Bit_Map picture, otherwise reset it.
*-- llNoShow   : This is a flag reset it to execute (lpShow) first time on screen entry .
STORE .F. TO glFromBrow,llBrowse,llNoShow

*-- Numeric Variables saved when goto another session.
*-- lnItmWid   : This is to hold the value of mask width to make the key field sizable
*-- lnCurrRec  : This is to hold the value of current record to move (>) shape to this record
STORE 0 TO lnItmWid,lnCurrRec,lnCut

*-- Character Variables saved when goto another session.
*-- lcICInv1   : This is to hold the name of the header screen
*-- lcICInv2   : This is to hold the name of the Browse screen
*-- lcICInv3   : This is to hold the name of the push button screen
*-- lcICInv4   : This is to hold the name of the information screen
*-- lcScFields : This is to hold the name of the filelds used in screen
*-- lcGLWareHs : This is to hold the value of Gl_Link Code to be sent to function gfGLBrowse
*-- lcGLWaDesc : This is to hold the value of Gl_Link Desc to be sent to function gfGLBrowse
*-- lcOldValue : This is to hold the value of the Gl_Link befor change it
*-- lcStyle    : This is to hold the value of the full key structure to use it in browsing to avoid multiple record
*-- lcWareFil  : This is to hold the temp name of the cursor that hold the warehouses(StyDye Table)
*-- lcLocFil   : This is to hold the temp name of the cursor that hold the locations (WhsLoc Table)
*-- lcForExp   : This is to hold the expresion used in browsing for both View and Edit modes.
*-- lcBrowFile : This is to hold the name of file used in browsing (i.e: temp file or master file)
*-- lcItmTtl   : This is to hold the value of the header of style field (Key). 
*-- lcItmPct   : This is to hold the value of the picture used in the GET cluase of style field (Key)
*-- lcBrowTtl  : This is to hold the title of browse window which used to refer to this window 
*-- lcDefWH    : This is to hold the title of browse window which used to refer to this window 

STORE SPACE(0) TO lcICInv1,lcICInv2,lcICInv3,lcICInv4,lcScFields    ,;
                  lcGLWareHs,lcGLWaDesc,lcOldValue,lcStyle,lcWareFil,;
                  lcLocFil,lcForExp,lcBrowFile,lcItmTtl,lcItmPct,lcDefWH
                  
lcBrowTtl = 'Locations'

*-- If you can not setup break excuation 
IF !gfSetup()
  RETURN
ENDIF

*-- This is to hold the restored values from the gfGetMemVar function 
*-- to show if there is link with GL or not, multi warehouses, 
*-- multi locations, and extended sizes.
*E301077,22 Call gfGetMemVar once for all setting. [Begin]
*llLink_GL  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL')) )  = 'Y'
*llMultiWH  = ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS')))  = 'Y'
*llMultiLoc = ALLTRIM(UPPER(gfGetMemVar('M_WARELOC')) )  = 'Y'
*llExtSize  = gfGetMemVar('M_USEEXSSC')

DECLARE laSetups[4,2]
laSetups[1,1]  = 'M_Link_GL'
laSetups[2,1]  = 'M_WAREHOUS'
laSetups[3,1]  = 'M_WARELOC'
laSetups[4,1]  = 'M_USEEXSSC'
=gfGetMemVar(@laSetups,gcAct_Comp)

llLink_GL  = UPPER(ALLTRIM(laSetups[1,2]))  = 'Y'
llMultiWH  = UPPER(ALLTRIM(laSetups[2,2]))  = 'Y'
llMultiLoc = UPPER(ALLTRIM(laSetups[3,2]))  = 'Y'
llExtSize  = laSetups[4,2]
*E301077,22 Call gfGetMemVar once for all setting. [End  ]

*-- If no warehouses and no locations assigend to this Company 
*-- we do not need to continue.   
IF !llMultiLoc AND !llMultiWH    
  *-- System has not been setup to keep track of Warehouses or Locations.
  *-- < OK >
  = gfModalGen("TRM42088B42000","DIALOG")
  llReturn = .F.
  RETURN
ENDIF

*B602440,1 Open the Codes file [start] 
= gfOpenFile(gcDataDir+'Codes','','SH')
*B602440,1 Open the Codes file [end..] 

laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.T.
laKeyField[1,3] = "STYLE"
laKeyField[1,4] = 1

*E301077,22 Open Bins files using global function [Begin]
*-- llBinsFile : .T. if program open WhsLoc File.
*-- llGlopen   : .T. if company use GL_LINK

STORE .F. TO llBinsFile
*-- if system support multi bins.
IF llMultiLoc
  IF USED('WHSLOC')
    lcLocOrd = ORDER('WHSLOC')
    lcLocRec = WHSLOC.cwarecode+WHSLOC.clocation+WHSLOC.style+WHSLOC.color
    SET ORDER TO WHSLOC IN WHSLOC
  ELSE
    llBinsFile = gfOpenFile(gcDataDir+'WHSLOC',gcDataDir+'WHSLOC','SH')
  ENDIF
ENDIF  

*-- If this company use GL open GL_Link File  
IF llLink_GL 
  *-- The llGlopen flag is true when current user is first one open the GL_Link file
  *-- and in clearup I use it to close the file if it is true  
  = gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH')
ENDIF

*-- If this is a new base window this means that it is a new session
*-- thus create new values for predefined variables, and assign new 
*-- variables unique to the screen.
IF !WEXIST(gcBaseWind)
  *-- Windows names.
  lcICInv1   = gfTempName()
  lcICInv2   = gfTempName()
  lcICInv3   = gfTempName()
  lcICInv4   = gfTempName()

  *-- gfItemMask is a global Function that returend header and picture 
  *-- of Key Field according to the passed parameters 
  
  lcItmTtl   = gfItemMask("HI")
  lcItmPct   = gfItemMask("PI")

  *B602778,1 if Item code structure has not been setuped return [start]
  
  IF EMPTY(lcItmTtl) OR EMPTY(lcItmPct)
    *-- Item structure not found , Can't proceed . 
    =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
    glQuitting=.T.
    RETURN
  ENDIF 
  
  *B602778,1 if Item code structure has not been setuped return [end..]  
  
  *-- If it is extended size scale we must cut the picture to accomplish 
  *-- new one If the global cutting finished we must unuse this condition
  IF llExtSize

    *E301077,22 Open Style code structure file
    = gfSysOpen(gcDataDir+'ICISTRU',gcDataDir+'SEGNO','SH')
    GO BOTTOM
    SKIP -1
    lnCut = IIF(EMPTY(cISegSepr),3,4)

    *E301077,22 Close style code structue file
    =gfSysClose('ICISTRU')
      
    lcItmPct   = SUBSTR(lcItmPct,1,LEN(lcItmPct)-lnCut)
    *-- Adjust lcBrfields to affect the global browsing BitMap
    lcBrfields = "ExStyle = SUBSTR(Style,1,LEN(lcItmPct))" + SUBSTR(lcBrFields,6)
  ENDIF  

  lnItmWid   = LEN(lcItmPct)
  lcScFields = "Style"

  *-- Create temp file name to hold wharehouses.
  lcWareFil = gfTempName()
  
  *-- Places the structure of the StyDye table into array laFileStru
  SELECT StyDye
  = AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1) 
  
  *-- Redimension the array to create a new field to hold the status of 
  *-- the record (S, M, A or D)
  DIMENSION laFileStru[lnFileStru+1, 4]
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cStatus'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0

  CREATE TABLE (gcWorkDir+lcWareFil) ;
         FROM ARRAY laFileStru

  *-- Create temp file name to hold the locations.
  lcLocFil = gfTempName()

  *-- Fill screen Fields with blank data
  SELECT Style
  SCATTER FIELDS &lcScFields TO laData BLANK

  *-- To browse in case of Select mode (From temp. File)
  lcBrowFile = lcWareFil
ENDIF 						&& Ending base window not existing IF.

SELECT Style
SET ORDER TO TAG Style

lcOrd    = SPACE(0)                            && To save the order of a tables

PUSH KEY                                       && To save the the current keys functions.
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl) && To activate the browse screen when pressing ALT+B

*-- Run the screen.
DO (gcScrDir+gcWinAppl+'\ICINVLO.SPR')

*-- CleanUp code
RELEASE WINDOW (lcBrowTtl)                     && Release browse window before exit.
POP KEY                                        && Restore old keys functions.

*-- Break the relation if found before exit
SELECT (lcBrowFile)
IF !EMPTY(RELATION(1))
  SET RELATION OFF INTO WAREHOUS
ENDIF 				&& Ending break relation IF.

*-- If you quit the program (i.e: close it not open another session)
IF glQuitting
  *-- This condition is because if we not have multi locations 
  *-- we do not create the temp. lcLocFil File that holding locations. 
  IF llMultiLoc
    *-- To assure that lcLocFil is created, because if we exit 
    *-- before enter in Edit mode this file still not exist,
    *-- thus there is no reson to erase it. 
    IF USED(lcLocFil)
      USE IN (lcLocFil)
      ERASE (gcWorkDir+lcLocFil+".DBF")
      ERASE (gcWorkDir+lcLocFil+".CDX")
      ERASE (gcWorkDir+lcLocFil+".FPT")
    ENDIF 		&& Ending file is used IF. 
  ENDIF 		&& Ending using multi location IF.  

  *-- Erase temp. StyDye File
  IF USED(lcWareFil)
    USE IN (lcWareFil)
    ERASE (gcWorkDir+lcWareFil+".DBF")
    ERASE (gcWorkDir+lcWareFil+".CDX")
    ERASE (gcWorkDir+lcWareFil+".FPT")
  ENDIF
  
  *E301077,22 Comment out this lines and transfer it out glQuitting block to closed every 
  *E301077,22 time user close screen.
  *-- If you use GL Link and you are one who open GL_Link file 
  *-- you must close it.  
  *IF llLink_GL AND llGlopen
  *  USE IN Gl_Link  
  *ENDIF
  
  llReturn = .T.
ENDIF 				&& Ending true quiting IF.

*E301077,22 Close Bins files if program open it. [Begin]
*-- if system support multi bins, and program open bins file.
IF llMultiLoc
  IF !llBinsFile
    SELECT WHSLOC
    SEEK lcLocRec
    SET ORDER TO &lcLocOrd
  ENDIF
ENDIF  
*E301077,22 Close Bins files if program open it. [End  ]

*E301077,22 Close Gl_Link file if program open it. [Begin]
*-- If you use GL Link and you are one who open GL_Link file 
*-- take the system deal with Gl Link file.
*-- you must close it.  
*IF llGlopen
  *USE IN Gl_Link  
*  =gfCloseFile('Gl_Link')
*ENDIF
*-- end of program code.
*E301077,22 Close Gl_Link file if program open it. [End  ]

*------------------------------------
*-- Start of Procedures and Functions
*------------------------------------
*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/97
*! Purpose   : Handling the screen modes.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfWHBrow,lfwWareBrs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
PROCEDURE lpShow
lnAlias = SELECT(0)

*-- The following case is to control the screen modes.
DO CASE
  *-- SELECT MODE
  CASE laScrMode[1]
    *-- Redefine new data
    SELECT Style
	SCATTER FIELDS &lcScFields TO laData BLANK
    lcGLWareHs   = SPACE(0)                 && The gl linke code.

    *-- Clear data if found from temp. location file .
    IF USED(lcLocFil) AND RECCOUNT(lcLocFil) != 0
      *-- Delete any existing data in the same ssession
      SELECT (lcLocFil)
      ZAP
    ENDIF  
  
    *-- Clear data if found from temp. ware file .
    IF USED(lcWareFil) AND RECCOUNT(lcWareFil) != 0
      *-- Delete any existing data in the same ssession
      SELECT (lcWareFil)
      ZAP
    ENDIF  

    *-- Disable buttons we control it by ourself 
    *-- and then put the current object to key field 
	SHOW GET pbWhse DISABLE
	SHOW GET pbLoc  DISABLE
    _CUROBJ = OBJNUM(laData[1])

  *-- VIEW MODE
  CASE laScrMode[2]
    *-- Redefine data to concating it to be suitable with extended 
    *-- size scales
    SELECT Style
	SCATTER FIELDS &lcScFields TO laData 
    lcStyle   = laData[1]
    lcDefWH   = Style.cDefWare
    laData[1] = SUBSTR(laData[1],1,lnItmWid)
    
    *-- ENABLE/DISABLE pbWhs button.
    lcWhsShw = IIF(llMultiWH,"ENABLE","DISABLE")	
    SHOW GET pbWhse &lcWhsShw
	
    *-- ENABLE/DISABLE pbLoc button.
    lcWhsShw = IIF(llMultiLoc,"ENABLE","DISABLE")	
	SHOW GET pbLoc  &lcWhsShw
	
    *-- Load the GL_Link value.
    *MAB Bug 02/26/1999 Dealing with Gl Link must under this condition. [Begin
    IF llLink_GL
    *MAB Bug 02/26/1999 Dealing with Gl Link must under this condition. [End..
      lcGLWareHs = &lcWareFil..GL_Link
    ENDIF  

    *-- Clear data if found from temp. location file .
    IF USED(lcLocFil) AND RECCOUNT(lcLocFil) != 0
      *-- Delete any existing data in the same ssession
      SELECT (lcLocFil)
      ZAP
    ENDIF  
  
    *-- Clear data if found from temp. ware file .
    IF USED(lcWareFil) AND RECCOUNT(lcWareFil) != 0
      *-- Delete any existing data in the same ssession
      SELECT (lcWareFil)
      ZAP
    ENDIF  

  *-- EDIT MODE
  CASE laScrMode[3]
    *-- Assure concating laData[1] to be suitable with extended size scale
    laData[1] = SUBSTR(laData[1],1,lnItmWid)
    
    *-- This confirmation avoid overriding the temp. ware file with another 
    *-- data from StyDye file if it is already have data
    IF RECCOUNT(lcWareFil) = 0
      *-- Load the style's size scales in the size scale array if the 
      *-- system is setup to use the extended size scales.
      IF llExtSize
        SELECT DISTINCT SUBSTR(Style,lnItmWid+1) ;
        FROM STYLE                               ; 
        WHERE Style = laData[1]                  ;
        INTO ARRAY laExtend
      ENDIF

      SELECT *, "S" AS cStatus                   ;
        FROM StyDye                              ; 
        WHERE Style+cWareCode+DyeLot = laData[1] ;
        AND EMPTY(DyeLot)                        ;
        ORDER BY cWareCode                       ;
        INTO TABLE (gcWorkDir+lcWareFil)
      
      INDEX ON cWareCode+Style+DyeLot TAG lcAddTag
      INDEX ON Style+cWareCode+DyeLot TAG (lcWareFil)
    ENDIF 				&& Ending confirmation for data in temp. ware file 	

    *-- If no multilocation there is no reason to create the temp file, 
    *-- the button will be disabled.
    IF llMultiLoc 		

      *-- llFill is a flag when it is true fill temp. location file.
      llFill = !USED(lcLocFil) OR (USED(lcLocFil) AND (RECCOUNT(lcLocFil) = 0))
    
      *-- This confirmation avoid overriding the temp. loc file with another 
      *-- data from WhsLoc file if it is already have data
      IF llFill      
        *-- Create temp file holding the locations.
        SELECT *, "S" AS cStatus            ;
          FROM WhsLoc                       ;
          WHERE Style + Color + cWareCode + ;
                cLocation = laData[1]       ;
          ORDER BY cWareCode, cLocation     ;
          INTO TABLE (gcWorkDir+lcLocFil)
    
        INDEX ON cWareCode+cLocation+Style+Color TAG "WhsWare"
        INDEX ON Style+Color+cWareCode+cLocation TAG "WhsSty"
      ENDIF 			&& Ending of filling the file IF.
    ENDIF 				&& Ending of ask obout multilocation IF.  

    lcWhsShw = IIF(llMultiWH,"ENABLE","DISABLE")
    SHOW GET pbWhse &lcWhsShw
	
    lcWhsShw = IIF(llMultiLoc,"ENABLE","DISABLE")
	SHOW GET pbLoc  &lcWhsShw
ENDCASE 			&& Ending the CASE that control screen modes  

*-- Disable delete button and delete bar, because it is only in style screen.
SHOW GET pbDlt DISABLE
DEFINE BAR 10 OF P03PU03 PROMPT "\<Delete" KEY CTRL+D,"^D" SKIP FOR .T.

*-- Refreshing data in both [screen fields(SAYs and GETs)] and [browse window] 
= lfWHBrow() AND lfwWareBrs()

*-- Restore the ALIAS we enter the function with it.
SELECT(lnAlias)


*!*************************************************************
*! Name      : lfvStyle
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/97
*! Purpose   : Validat the screen key (Style).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfStyBrw,gfSeekRec
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Style

*-- If come from BitMap button or the key is found
IF llBrowse OR (!EMPTY(laData[1]) AND !SEEK(laData[1]))
  llBrowse  = .F. 		                && Disable flag to its intial state   
  laData[1] = gfStyBrw('I','','',.F.) 	&& Calling global browse 	
  IF EMPTY(laData[1])			        && If ESCAPE pressed wait in your position	
    _CUROBJ = _CUROBJ
  ENDIF 						        && Ending ESCAPE pressed
ENDIF							        && Ending seek condition

IF !EMPTY(laData[1])
  lcStyle   = Style.Style		        && Save full key value to use in browse
  lcDefWH   = Style.cDefWare			&& Save Default WareHouse
  = gfSeekRec()         				&& Switch the scrren mode.
ENDIF

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfWHBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/97
*! Purpose   : Browse the warehouses assigned to the Style.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfwWareBrs,lfvBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWHBrow()
*!*************************************************************
FUNCTION lfWHBrow
PRIVATE lnAlias,lcBrfields

*-- Break relation if found
SELECT (lcBrowFile)
IF !EMPTY(RELATION(1))
  SET RELATION OFF INTO WAREHOUS
ENDIF

*-- Release window if found
IF WEXIST(lcBrowTtl)
  RELEASE WINDOW (lcBrowTtl)
ENDIF

lnAlias    = SELECT(0)

*-- Assign values corresponding to the file used
*-- lcBrfields : Hold the fields used in browse (note that it is local in this function)
*-- lcBrowFile : Hold the file used in browse
*-- lcForExp   : Hold the expression used in browse
lcBrfields = "cDummi=IIF(RECNO()=lnCurrRec,'>',' '):1:R:H=' ':W=.F.,cWareCode :H= 'Location'  :R,WareHous.cDesc :H= 'Name'"
lcBrowFile = IIF(laScrMode[2],"StyDye", lcWareFil)
lcForExp   = IIF(laScrMode[2],"Style = lcStyle AND EMPTY(Dyelot)"   , ;
                              "Style = lcStyle AND cStatus <> 'D'")

*-- Make relation between used file and WareHous File
SELECT (lcBrowFile)
SET RELATION TO &lcBrowFile..cWareCode INTO WareHous ADDITIVE
GO TOP
lnCurrRec  = RECNO()
           
BROWSE FIELDS &lcBrfields  ;
      FOR &lcForExp        ;
      WHEN lfwWareBrs()    ;
      NOAPPEND NODELETE    ;
      NOWAIT NOCLEAR  SAVE ;
      TITLE lcBrowTtl      ;
      VALID :F lfVBrow()   ;         
      FREEZE cDummi 	   ; 
      WINDOW (lcICInv2) IN WINDOW (gcBaseWind)

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwWareBrs
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/97
*! Purpose   : Assign cWarecode to lcGLWaDesc and GL_Link to lcGlWareHs
*!           : and the record number to lnCurrRec
*!           : to have ( > shape ) refrence to current record
*!           : it also refresh the screen fields
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwWareBrs()
*!*************************************************************
FUNCTION lfwWareBrs
PRIVATE lnAlias

lnAlias    = SELECT(0)
lcBrowFile = IIF(laScrMode[2],"StyDye", lcWareFil)

SELECT (lcBrowFile)

*-- Load the browse variables...
lnCurrRec  = RECNO()

*MAB Bug 02/26/1999 Dealing with Gl Link must under this condition. [Begin
IF llLink_GL
*MAB Bug 02/26/1999 Dealing with Gl Link must under this condition. [End..
  lcGLWareHs = &lcBrowFile..GL_Link
  lcGLWaDesc = IIF(SEEK('03'+PADR(lcGLWareHs,6), 'GL_LINK'), GL_LINK.linkdesc,"")
ENDIF  

*-- Refresh warehouse data window
= lfRefresh(lcICInv4)
SHOW GET lcGLWareHs 

*-- Refresh the browse window.
SHOW WINDOW (lcBrowTtl) REFRESH

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/19/97
*! Purpose   : To Check if comming from browse call gfStopBrow() 
*!             function to accomplish the activating of browse 
*!             window if coming from another session, and also 
*!             serve when press ESCAPE.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvBrow()
*!*************************************************************
FUNCTION lfVBrow

IF WONTOP() # (lcBrowTtl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : calling the gfmover with the location arrays and 
*!             handling the returned laTarget (i.e: updating in 
*!             temp. Location file [from WhsLoc]) brifely we can 
*!             say that it is prepair to save locations.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen,lfvNotMove
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvLoc()
*!*************************************************************
FUNCTION lfvLoc
PRIVATE lcSDel

*-- Initialize the mover arrays.
STORE SPACE(0) TO laSource, laTarget

*-- lcWareH : Hold cWareCode used in SELECT SQL filling arrays
lcWareH = IIF(laScrMode[2],StyDye.cWareCode,&lcWareFil..cWareCode)

*-- Prepair to use GfMover hold locations
*-- Fill laSource array with all locations for specific WareHouse
SELECT DISTINCT cLocation                      ;
 FROM WhsLoc                                   ;
 WHERE cWareCode+cLocation+Style+Color=lcWareH ;
 ORDER BY cWareCode                            ;
 INTO ARRAY laSource

*-- If no collection of data from last SELECT SQL there is no locations 
*-- thus you must return
IF _TALLY = 0
  *-- No Locations assigined to warehouse XXXXXXXXXX.
  *-- <  OK  >
  = gfModalGen("TRM42058B42000","DIALOG" , lcWareH)
  RETURN
ENDIF

*-- lcSelFile : Hold the file used in SQL statement 
*-- lcSelExp  : Hold the expression used in SQL statement 
*--             according to used file
lcSelFile = IIF(laScrMode[2],"WhsLoc",lcLocFil)
lcSelExp  = IIF(laScrMode[2], ;
                "cWareCode+cLocation+Style+Color     = lcWareH     ;
                 AND Style+Color+cWareCode+cLocation = laData[1]" ,;
                "cWareCode+cLocation+Style+Color     = lcWareH     ;
                 AND cStatus != 'D'")

*-- Fill laTarget array with all locations assigined to specific WareHouse
SELECT DISTINCT cLocation ;
  FROM (lcSelFile)        ;
  WHERE &lcSelExp         ;
  ORDER BY cWareCode      ;
  INTO ARRAY laTarget

*-- If no collection of data from last SELECT SQL redimension array
*-- and store NULL to it.
IF _TALLY = 0
  *-- To avoid have spaces items in laTarget because a bug in GfMover 
  IF ALEN(laTarget) > 1 AND laTarget[1] = SPACE(0)
    DIMENSION laTarget[1]
    laTarget = SPACE(0)
  ENDIF
ENDIF

= gfMover(@laSource,@laTarget,'Assign Bins',!laScrMode[2],'')  

*-- You are in Edit mode
IF laScrMode[3]
  lnAlias = SELECT(0)

  *-- Assure that delete is off to deal with deleted records
  lcSDel = SET("DELETED")
  SET DELETED OFF
  
  *-- Search for locations for specific warehouse 
  SELECT (lcLocFil)
  SCAN FOR cWarecode+cLocation+Style+Color = lcWareH 
    *-- IF you do not find this location in array this means 
    *-- that it deleted we replace its status with "D" or DELETE it,
    *-- corresponding to its current status
    IF ASCAN(laTarget, ALLTRIM(cLocation)) = 0 
      IF cStatus = "S"
        REPLACE cStatus WITH "D"
      ELSE
        IF cStatus = "A" 
          DELETE        
        ENDIF
      ENDIF       

    *-- IF you find this location in array this means 
    *-- it is an (A)dded one or exi(S)ting one
    ELSE
      *-- If you replace the status for location with "D" in the same screen
      IF cStatus = "D"
        REPLACE cStatus WITH "S"
      ENDIF
      *-- If you delete location in the same screen (New in this screen)
      IF DELETED()
        RECALL
      ENDIF   
    ENDIF 		&& Ending array scan IF.
  ENDSCAN 		&&   Ending Search for locations for specific warehouse 

  *-- Appending new record for new location if it is not found in file
  *-- loop until you reach the end of array (For all items in array)
  FOR lnI = 1 TO ALEN(laTarget,1)
    IF !EMPTY(laTarget[lnI])
      LOCATE FOR cWarecode+cLocation+Style+Color = lcWareH+laTarget[lnI]
      IF !FOUND()
        *-- If you use extended size scales loop with data from
        *-- laExtend array filled in edit mode from Style file
        *-- to add multiple records
        IF llExtSize
          FOR lnJ = 1 TO ALEN(laExtend,1)
            INSERT INTO (lcLocFil)                                             ;
                     (cWarecode,cLocation    ,Style                  ,cStatus) ;
              VALUES (lcWareH  ,laTarget[lnI],laData[1]+laExtend[lnJ],"A"    )     
          ENDFOR    
        *-- If you do not use extended size scales add single record
        ELSE
          INSERT INTO (lcLocFil)                               ;
                   (cWarecode,cLocation    ,Style    ,cStatus) ;
            VALUES (lcWareH  ,laTarget[lnI],laData[1],"A"    )     
        ENDIF 			&& Ending extended size IF.  
      ENDIF				&& Ending record not found IF.
    ENDIF				&& Ending not empty data IF.    
  ENDFOR				&& Ending for

  *-- Restore old delete status
  SET DELETED &lcSDel

  SELECT(lnAlias)
ENDIF


*!*************************************************************
*! Name      : lfvWareHs
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : calling the gfmover with warehouse arrays and handling 
*!           : the returned laTarget (i.e: updating in temp. ware file
*!           : [from StyDye], and also updating temp. location file)
*!           : brifely we can say that it is prepair to save warehouses
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover,lfvRem 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvWareHs()
*!*************************************************************
FUNCTION lfvWareHs
PRIVATE lcSDel,lcSaveRec

STORE SPACE(0) TO laSource,laTarget

*-- Prepair to use GfMover hold Warehouses
*-- Fill laSource array with all warehouses in WareHouse file
SELECT DISTINCT cWarecode FROM WareHous ;
  INTO ARRAY laSource


*-- lcSelFile : Hold the file used in SQL statement 
*-- lcSelExp  : Hold the expression used in SQL statement according to used file
*-- lcSaveRec : Hold the current record value to restore it after Mover
lcSelFile = IIF(laScrMode[2],"StyDye",lcWareFil)
lcSelExp  = IIF(laScrMode[2], ;
                "Style+cWareCode+DyeLot = laData[1]", ;
                "cStatus != 'D'")
lcSaveRec = &lcSelFile..cWarecode

*-- Fill laTarget array with all warehouse assigined to this style
SELECT DISTINCT cWarecode ;
  FROM (lcSelFile)        ;
  WHERE &lcSelExp         ;
  ORDER BY cWareCode      ;
  INTO ARRAY laTarget

*-- Call the Mover with function lfvRem that validate for removing warehous
= gfMover(@laSource,@laTarget,'Assign Locations',!laScrMode[2],'lfvRem',.F.,.T.)
*-- You are in Edit mode
IF laScrMode[3]
  lnAlias = SELECT(0)

  *-- Assure that delete is off to deal with deleted records
  lcSDel = SET("DELETED")
  SET DELETED OFF
  
  *-- Search for warehouses for this style in temporary file 
  SELECT (lcWareFil)
  SCAN 
    *-- IF you do not find this warehouse in array this means 
    *-- that it deleted we replace its status with "D" or DELETE it,
    *-- corresponding to its current status
    IF ASCAN(laTarget, ALLTRIM(cWarecode)) = 0 
      *-- If it is already in file or it is modified
      IF cStatus = "S" OR cStatus = "M"
        REPLACE cStatus WITH "D"
        *B606803,1 ASH 12/19/2002 (Begin) lcLocFil not opened in case of keep track of bins = 'N'
        IF llMultiLoc
        *B606803,1 ASH 12/19/2002 (End)
          SELECT (lcLocFil)
          REPLACE ALL cStatus WITH "D" ;
                  FOR cWarecode+cLocation+Style+Color = &lcWareFil..cWareCode
        ENDIF
      ELSE
        *B606803,1 ASH 12/19/2002 (Begin) lcLocFil not opened in case of keep track of bins = 'N'
        *IF cStatus = "A" 
        IF cStatus = "A" AND llMultiLoc
        *B606803,1 ASH 12/19/2002 (End)
          SELECT (lcLocFil)
          REPLACE ALL cStatus WITH "D" ;
                  FOR cWarecode+cLocation+Style+Color = ;
                      &lcWareFil..cWareCode
          SELECT (lcWareFil)
          DELETE
        ENDIF
      ENDIF       
    *-- IF you find this warehouse in array this means 
    *-- it is a (A)new one or (M)modified one
    *-- we make this substituation to avoid complicating code
    ELSE
      *-- If you replace the status for warehouse with "D" in the same screen
      IF cStatus = "D"
        REPLACE cStatus WITH "M"
      ENDIF

      *-- If you delete warehouse in the same screen (New in this screen)
      IF DELETED()
        RECALL
      ENDIF   
    ENDIF 				&& Ending array scan IF. 
  ENDSCAN				&& Ending Search for warehouses

  *-- Prepaire to use seek
  SELECT (lcWareFil)
  SET ORDER TO lcAddTag

  *-- Appending new record for new warehouse if it is not found in file
  *-- loop until you reach the end of array (For all items in array)
  FOR lnI = 1 TO ALEN(laTarget,1)
    IF !SEEK(laTarget[lnI]) AND !EMPTY(laTarget[lnI])
      *-- If you use extended size scales loop with data from
      *-- laExtend array filled in edit mode from Style file
      *-- to add multiple records
      IF llExtSize
        FOR lnJ = 1 TO ALEN(laExtend,1)
          INSERT INTO (lcWareFil)                                          ;
                           (Style                  ,cWarecode    ,cStatus) ;
                    VALUES (ladata[1]+laExtend[lnJ],laTarget[lnI],"A"    )     
        ENDFOR

      *-- If you do not use extended size scales add single record
      ELSE
        INSERT INTO (lcWareFil)                            ;
                         (Style    ,cWarecode    ,cStatus) ;
                  VALUES (ladata[1],laTarget[lnI],"A"    )     
      ENDIF			&& Ending extended size IF.
    ENDIF			&& Ending record not found IF.	
  ENDFOR			&& Ending for

  *-- Restore old delete status
  SET DELETED &lcSDel

  *-- Restore the same cursor (>) location if it is found 
  IF !SEEK(lcSaveRec) OR cStatus = "D"
    LOCATE FOR &lcForExp
  ENDIF  

  *-- Prepaire to use seek
  SELECT (lcWareFil)
  SET ORDER TO (lcWareFil)

  *-- Refresh the browse screen.
  = lfwWareBrs()

  SELECT(lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfvRem
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : Validat the removed warehouses to avoid removing 
*!           : ordered or have a stock ONEs
*!           : case of remove we returned to Mover by .T. to remove
*!           : or .F. to cancel remove
*!           : case of remove all we make the moving and returned to Mover
*!           : with .F. to cancel remove all
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen,lfRemCal
*!*************************************************************
*! Passed Parameters  : lnBttn 
*!*************************************************************
*! Returns            : llRetVal = .T. , Mover can remove
*!                    : llRetVal = .F. , Mover can not remove
*!*************************************************************
*! Example   : =lfvRem(lnButton)
*!*************************************************************
FUNCTION lfvRem
PARAMETERS lnBttn
PRIVATE lnI

IF laScrMode[3] OR laScrMode[4]
  *-- Prepair to remove valid
  lnCurAlias = SELECT(0)
  SELECT StyDye
  lcCurNdx = ORDER()
  SET ORDER TO StyDyeW
  lcOldExact = SET("EXACT")
  SET EXACT OFF
  llRetVal = .F.         && To hold the returned value
   
  *-- To distinct between lnBttn cases
  *-- If <Move> or <MoveAll> do nothing
  IF lnBttn <= 2
    llRetVal = .T.
  ENDIF

  *-- If <Remove> Call to valid function and store its return value in llRetVal
  *-- which returned to Mover.
  *-- Note that this code is executed in <Remove All> case 
  *-- because we pass .T. in end of gfMover call.        
  IF lnBttn = 3
    llRetVal = lfRemCal(lsTarget)
  ENDIF

  *-- Restore previous status   
  SET ORDER TO (lcCurNdx)
  SELECT (lnCurAlias)
  SET EXACT &lcOldExact
ENDIF
RETURN (llRetVal)


*!*************************************************************
*! Name      : lfRemCal
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : This Function calculate the expression for removing  
*!           : warehouse , and I separate it to keep it available to
*!           : both remove and remove all cases just refrence it by 
*!           : the removed item in laTarget array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnArrRef 
*!*************************************************************
*! Returns            : llRemCal 
*!*************************************************************
*! Example   : =lfRemCal(lnRef)
*!*************************************************************
FUNCTION lfRemCal
PARAMETERS lnArrRef

*-- lnTotStk  : Hold the total stock in this warehouse, (initially ZERO) 
*-- lnTotOrd  : Hold the total order quantity in this warehouse, (initially ZERO) 
*-- lnTotWip  : Hold the total wip quantity in this warehouse, (initially ZERO) 
lnTotStk  =  0
lnTotOrd  =  0
lnTotWip  =  0
llProceed = .T.

*-- Are you sure you want to remove warehouse XXX.
*-- <  Yes > <  No  >
lnChoice = gfModalGen("QRM42031B42002","Dialog",laTarget[lnArrRef])                            
*-- If user choice is <  Yes > do the following 
*-- Note that if user choice is <  No >, lnChoice have 2, i.e: return False 
IF lnChoice = 1

  IF UPPER(ALLTRIM(lcDefWH)) == UPPER(ALLTRIM(laTarget[lnArrRef]))
    *-- Can not remove the default warehouse
    *-- <  OK  >
    = gfModalGen("IRM42038B42001","DIALOG")
    lnChoice = 2
    llProceed = .F.
  ENDIF

  IF llProceed
    *-- If you find item in StyDye file do the following
    IF SEEK(PADR(laTarget[lnArrRef],6)+laData[1],"StyDye")

      *-- Using SUM function is to make compatibality with extended size scales
      SUM TotStk, TotOrd, TotWip TO lnTotStk,  lnTotOrd, lnTotWip               ;
        FOR cwarecode+style+dyelot = PADR(laTarget[lnArrRef],6)+laData[1] ;
            AND EMPTY(DYELOT)
    ENDIF
    
    IF lnTotOrd >0
      *-- Style XXXX has on order quantity in this warehouse, can not remove
      *-- <  OK  >
      = gfModalGen("IRM42032B42000","DIALOG",laData[1])
      lnChoice = 2			&& To return False
      llProceed = .F.
    ENDIF  					&& Ending On Order confirmation IF.
  ENDIF						&& Ending Proceed IF.

  IF llProceed
    IF lnTotWip >0
      *-- Style XXXX has WIPPED quantity in this warehouse, can not remove
      *-- <  OK  >
      = gfModalGen("IRM42011B42001","DIALOG")
      lnChoice = 2			&& To return False
      llProceed = .F.
    ENDIF					&& Ending Wipped confirmation IF.
  ENDIF  					&& Ending Proceed IF.

  IF llProceed
    *-- You have stock in this warehouse   
    IF lnTotStk > 0

      *-- You can not remove warehouse,because you are linked with GL  
      IF llLink_Gl
        *-- Style XXXX has an inventory in this warehouse, can not remove
        *-- <  OK  >
        = gfModalGen("IRM42034B42000","DIALOG",laData[1])
        lnChoice = 2		&& To return False 
      ELSE
        *-- Style XXXX has an inventory in this warehouse, are you sure 
        *-- you want to remove.
        *-- <  Yes > <  No  >
        lnChoice = gfModalGen("QRM42036B42002","DIALOG",laData[1])
      ENDIF			&& Ending llLink_GL  IF.
    ENDIF			&& Ending Stock confirmation IF.
  ENDIF				&& ENDING llProceed IF. 				
ENDIF				&& Ending First confirmation IF.
RETURN (lnChoice = 1)

*!*************************************************************
*! Name      : lfvGLWareHs
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : Validation of GL_Link codes assigned to warehous
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfGLBrowse
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvGLWareHs()
*!*************************************************************
FUNCTION lfvGLWareHs

IF MDOWN()
  RETURN
ENDIF
  
PRIVATE lnAlias

lnAlias = SELECT(0)

IF llBrowse .OR. !EMPTY(lcGLWareHs)
  *-- calling the global browse for Gl_Link
  SELECT GL_Link
  = gfGLBrowse('03',@lcGLWareHs,@lcGLWaDesc)
  
  IF EMPTY(lcGLWareHs)
    lcGLWareHs = lcOldValue
  ENDIF
ENDIF  

lcGLWaDesc = IIF(SEEK('03'+PADR(lcGLWareHs,6), 'GL_LINK'), GL_LINK.linkdesc,"")
REPLACE &lcWareFil..GL_Link WITH lcGLWareHs

*-- Refresh warehouse data window
= lfRefresh(lcICInv4)

SELECT(lcWareFil)

*-- If user change GL_Link code
IF !((lcGLWareHs = lcOldValue) OR (&lcWareFil..cStatus = "A"))
  REPLACE &lcWareFil..cStatus WITH 'M'
ENDIF

SELECT (lnAlias)
llBrowse = .F.

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : To save the old value of the GL_link befor change it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : 1- Saving data from temporary files to master files
*!           :    (i.e: from lcWareFil to StyDye
*!           :          from lcLocFil  to WhsLoc)
*!           : 2- Update the stocks of Style file in case of Deleted
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************
PROCEDURE lpSavScr
PRIVATE lnAlias,lcSDel,llWareUp,llLocUp

lnAlias = SELECT(0)

*-- Rising flags detemining whether to continue saving or not
SELECT (lcWareFil)
LOCATE FOR INLIST(cStatus,'A','M','D')
llWareUp = FOUND()

IF llMultiLoc
  SELECT (lcLocFil)
  LOCATE FOR INLIST(cStatus,'A','D')
  llLocUp = FOUND()
ELSE
  llLocUp = .F.
ENDIF  

*-- If you change data in your screen
IF llWareUp OR llLocUp
  *-- Assure that delete is off to deal with deleted records
  lcSDel = SET("DELETED")
  SET DELETED OFF
ENDIF

*-- In case of changes made in Temporary StyDye File
IF llWareUp
  *-- Save in StyDye File loop
  SELECT (lcWareFil)
  SCAN				&& Loop in temporary StyDye file
    DO CASE
      *-- If record status is 'A' (i.e:Added record)
      CASE cStatus = 'A'
        IF SEEK(&lcWareFil..cWareCode,"WareHous")
          
          *-- Add one record to StyDye File
          *E301077,22 MAB 03/09/1999 Remove odded fields update [Begin
          *E301077,22                and call global update for it.
          *Odded INSERT INTO StyDye                          ;
          *Odded          (Style,cWareCode,GL_Link,Add_Date ,;
          *Odded          Add_Time,Add_UserId,Ave_Cost)      ;
          *Odded          VALUES (&lcWareFil..Style         ,;
          *Odded                  &lcWareFil..cWareCode     ,;
          *Odded                  &lcWareFil..GL_Link       ,;
          *Odded                  DATE()                    ,;
          *Odded                  gfGettime()               ,;
          *Odded                  gcUser_Id                 ,;
          *Odded                  Style.Ave_Cost)     

          INSERT INTO StyDye                          ;
                   (Style,cWareCode,GL_Link,Ave_Cost) ;
                   VALUES (&lcWareFil..Style         ,;
                           &lcWareFil..cWareCode     ,;
                           &lcWareFil..GL_Link       ,;
                           Style.Ave_Cost)     
          =gfAdd_Info('STYDYE')
          *E301077,22 MAB 03/09/1999 Remove odded fields update [End
          
        ENDIF
      *-- If record status is 'M' (i.e:Modified record)
      CASE cStatus = 'M'
        SELECT StyDye
        *-- Pointer to the same record in StyDye

       *E301077,22 MAB 03/09/1999 Remove odded fields update [Begin

        *Odded REPLACE GL_Link WITH &lcWareFil..GL_Link ,;
        *Odded         Mod_Action WITH 'MODIFIED'       ,;
        *Odded         Mod_Date   WITH DATE()           ,;
        *Odded         Mod_Time   WITH gfGettime()      ,;
        *Odded         Mod_UserId WITH gcUser_Id         ;
        *Odded    FOR  Style+cWareCode+DyeLot+STR(RECNO(),7) =   ;
        *Odded         &lcWareFil..Style + &lcWareFil..cWareCode

        REPLACE GL_Link WITH &lcWareFil..GL_Link          ;
           FOR  Style+cWareCode+DyeLot =                  ;
                &lcWareFil..Style + &lcWareFil..cWareCode

        =gfAdd_Info('STYDYE')
        *E301077,22 MAB 03/09/1999 Remove odded fields update [End

      *-- If record status is 'D' (i.e:Deleted record)
      CASE cStatus = 'D'
        *-- Pointer to the same record in StyDye
        SELECT StyDye
        DELETE FOR Style+cWareCode+DyeLot ; 
                 = &lcWareFil..Style+&lcWareFil..cWareCode
        *-- Updating Stocks in Style file 
        SELECT Style
        = SEEK(&lcWareFil..Style,"Style") 
        REPLACE Style.Stk1   WITH Style.Stk1   - &lcWareFil..Stk1 ,;
                Style.Stk2   WITH Style.Stk2   - &lcWareFil..Stk2 ,;
                Style.Stk3   WITH Style.Stk3   - &lcWareFil..Stk3 ,;
                Style.Stk4   WITH Style.Stk4   - &lcWareFil..Stk4 ,;
                Style.Stk5   WITH Style.Stk5   - &lcWareFil..Stk5 ,;
                Style.Stk6   WITH Style.Stk6   - &lcWareFil..Stk6 ,;
                Style.Stk7   WITH Style.Stk7   - &lcWareFil..Stk7 ,;
                Style.Stk8   WITH Style.Stk8   - &lcWareFil..Stk8 ,;
                Style.TotStk WITH Style.TotStk - &lcWareFil..TotStk 
    ENDCASE
  ENDSCAN			&& Ending Loop in temporary file

  *-- Return cursor of Style file to its original position
  =SEEK(lcStyle,"Style")
  SHOW GET lcGLWareHs

ENDIF

*-- In case of changes made in Temporary WhsLoc File
IF llLocUp
  *-- Save in WhsLoc File loop
  SELECT (lcLocFil)
  SCAN				&& Loop in temporary WhsLoc file
    *-- If record status is 'A' (i.e:Added record)
    IF cStatus = 'A'
      *-- badran add.
      IF SEEK(&lcLocFil..cWarecode+&lcLocFil..cLocation,"WHSLOC")
        INSERT INTO WhsLoc                                                    ;
                 (Style           ,cWarecode           ,cLocation           ) ;
          VALUES (&lcLocFil..Style,&lcLocFil..cWarecode,&lcLocFil..cLocation)     
      ENDIF  
    ELSE			&& cStatus = "D"
      *-- If record status is 'D' (i.e:Deleted record)
      IF cStatus = 'D'
        SELECT WhsLoc
        DELETE FOR cWareCode+cLocation+Style+Color =  ; 
                   &lcLocFil..cWareCode+&lcLocFil..cLocation ;
                 + &lcLocFil..Style
      ENDIF 
    ENDIF
  ENDSCAN			&& Ending Loop in temporary file
ENDIF				&& Ending case of changes made in Temporary WhsLoc File
SELECT (lnAlias)  

*-- If you change data in your screen.
IF llWareUp OR llLocUp
  SET DELETED &lcSDel
ENDIF  

*!*************************************************************
*! Name      : lfDeAct
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : Assigned KEYs TAB and BACKTAB go to browse
*!           : to have the same action like any normal GETs
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfDeAct()
*!*************************************************************
FUNCTION lfDeAct

IF WONTOP()  = (lcBrowTtl)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL BACKTAB    DO lpBackTab
ENDIF


*!*************************************************************
*! Name      : lfAct
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : Release KEYs TAB and BACKTAB when out from browse
*!           : to continue with the normal sequence of KEYs
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAct()
*!*************************************************************
FUNCTION lfAct

* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lpTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : Trapping TAB
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB
*-- Go to next window
ACTIVATE WINDOW (lcICInv3) 
_CUROBJ = OBJNUM(pbWhse)

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/20/97
*! Purpose   : Trapping BACKTAB
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB
*-- Go to previous window
IF laScrMode[1]
  ACTIVATE WINDOW (lcICInv1)
  _CUROBJ = OBJNUM(laData[1])
ELSE
  ACTIVATE WINDOW gwcContrl1
  _CUROBJ = OBJNUM(pbCls)
ENDIF
