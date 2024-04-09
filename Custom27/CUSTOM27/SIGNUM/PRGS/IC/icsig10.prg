*:************************************************************************
*: Program file  : ICSIG10.PRG
*: Program desc. : DATA CONVERT PROGRAM
*:                 This program is used to convert the data stored in text
*:                 files to Aria system tables.
*: Date          : 16/09/1999
*: For screen    : ICSIG10.SPR
*: System        : ARIA APPAREL SYSTEM 2.7
*: Module        : Inventory Control (IC).
*: Developer     : Sameh Saiid (SSE)
*: Reference     : *C101650
*:************************************************************************
*: Calls         
*:    Procedures : lpCreateTmp
*:               : lpPrintRep
*:               : lpAddStyle
*:               : gfDispRe 
*: 
*:    Functions  : lfvSourDir()
*:               : lfvTargDir()
*:               : lfvLocaton()
*:               : lfShowGets()
*:               : lfFlsToOpn()
*:               : lfvReport()
*:               : lfvConvert()
*:               : lfMajSeg()
*:               : gfModalGen()
*:               : gfItemMask()
*:               : gfGetMemVar()
*:               : gfTempName() 
*:               : gfBrowWare()
*:**************************************************************************
*: Example       : DO ICSIG10.PRG
*:**************************************************************************
*

*-- initializing variables
STORE '' TO lcTextFile,lcMajTitle,lcSepartor,lcOldData,lcNonMajT
STORE 0 TO lnMajorLen,lnClrPo,lnColorLen
STORE .T. TO llCrtChang , llDataFile
lcTargDir = gcDataDir
llStyMark = gfGetMemVar('M_STYMARK') = 'T'

IF !USED('WareHous')
  USE (gcDataDir+'WareHous') IN 0
  lcLocation = WareHous.cWareCode
ENDIF

*-- creating 3 temp. file names
lcTempFile = gfTempName()
lcStyRejct = gfTempName()
lcStyleDif = gfTempName()
=lfMajSeg()        && get major title , length and separator

PUSH KEY
DO (gcScrDir+gcWinAppl+'\ICSIG10.SPR')

*-- Close all temp. files and data files opened [Begin.]
IF USED(lcTempFile)
  USE IN (lcTempFile)
  ERASE (gcWorkDir+lcTempFile+'.DBF')
ENDIF

IF USED(lcStyRejct)	
  USE IN (lcStyRejct)
  ERASE (gcWorkDir+lcStyRejct+'.DBF')
ENDIF

IF USED("Style")	
  USE IN Style
ENDIF

IF USED("StyDye")	
  USE IN StyDye
ENDIF
  
IF USED("Scale")
  USE IN Scale
ENDIF

IF USED("Codes")
  USE IN Codes
ENDIF

IF USED("WAREHOUS")
  USE IN WAREHOUS
ENDIF

IF USED(lcStyleDif)
  USE IN (lcStyleDif)
  ERASE (gcWorkDir+lcStyleDif+'.DBF')
ENDIF

*-- Close all temp. files and data files opened [End.]
*-- Restore all keys from stack
ON KEY
POP KEY
SET DEFAULT TO &gcDef_path

****************************************************************************
*************************** *--End of Program--* ***************************
****************************************************************************

*!************************************************************************** 
*! Name      : lfwOldData
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To save the old data of Source Dir OR Target Dir OR Locations
*!************************************************************************** 
*
FUNCTION lfwOldData
PARAMETERS lcPassData
lcOldData = lcPassData
*-- End of lfwOldData.

*!************************************************************************** 
*! Name      : lfvSourDir
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To Select the text files directory 
*!************************************************************************** 
*! Calls     : lfShowGets()
*!************************************************************************** 
*! Returns   : The name of the text files Derectory
*!************************************************************************** 
*
FUNCTION lfvSourDir
lcTextFile = GETFILE('TXT','Source File')
lcTextFile = IIF(EMPTY(lcTextFile),lcOldData,lcTextFile)
llCrtChang = llCrtChang OR !(lcTextFile == lcOldData)
= lfShowGets()             && call the show gets function
*-- End of lfvSourDir.

*!************************************************************************** 
*! Name      : lfvTargDir
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To select the directory that contain required Tables 
*!************************************************************************** 
*! Calls     : lfShowGets()
*!************************************************************************** 
*! Returns   : The name of the selected directory
*!************************************************************************** 
*
FUNCTION lfvTargDir
lcTargDir  = GETDIR('','Target Directory')
lcTargDir  = IIF(EMPTY(lcTargDir),lcOldData,lcTargDir)
llCrtChang = llCrtChang OR !(lcTargDir == lcOldData)
llDataFile = llDataFile OR !(lcTargDir == lcOldData)

IF FILE(lcTargDir+'WareHous.dbf')
  IF !USED('WareHous') OR !(lcOldData == lcTargDir)
    IF USED('WareHous')
      USE IN WareHous
    ENDIF
    USE (lcTargDir+'WareHous') IN 0
  ENDIF
  lcLocation = WareHous.cWareCode
  = lfShowGets()             && call the show gets function
ELSE
  lcTargDir = lcOldData
  *-- Message : M42207 => 'ð not found in this current directory'
  *-- Button  : B00000 =>                '<Ok> '
  = gfModalGen('TRM42207B00000','DIALOG','WareHous file is')
ENDIF
*-- End of lfvTargDir.

*!************************************************************************** 
*! Name      : lfvLocaton
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : Select default warehouse.
*!************************************************************************** 
 *! Calls     : lfShowGets()
*!************************************************************************** 
*! Returns   : The name of the default Location
*!************************************************************************** 
*
FUNCTION lfvLocaton
SELECT WareHous
SET ORDER TO TAG WareHous
lcLocation = gfBrowWare(.T.)
lcLocation = IIF(EMPTY(lcLocation),lcOldData,lcLocation)
= lfRefresh()
*-- End of lfvLocaton.

*!************************************************************************** 
*! Name      : lfShowGets
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To refresh all objects on screen 
*!************************************************************************** 
*
FUNCTION lfShowGets
= lfRefresh()
IF EMPTY(lcLocation) OR EMPTY(lcTargDir) OR EMPTY(lcTextFile)
  SHOW GET pbConvert DISABLE
  SHOW GET pbReport  DISABLE
ELSE
  lcEnabDisa = IIF(llCrtChang,'ENABLE','DISABLE')
  SHOW GET pbConvert &lcEnabDisa
  SHOW GET pbReport  ENABLE
ENDIF
*-- End of lfShowGets.

*!************************************************************************** 
*! Name      : lfFlsToOpn
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To open the necessary files
*!**************************************************************************
*! Calls     : gfModalGen() , lpCreateTmp
*!**************************************************************************
*! Example   : = lfFlsToOpn()
*!**************************************************************************
*
FUNCTION lfFlsToOpn
PRIVATE llFlOpened
llFlOpened = .F.
*-- If all the 3 data files is found [Begin]
IF FILE(lcTargDir + 'STYLE.DBF') AND  FILE(lcTargDir + 'STYDYE.DBF') AND ;   
   FILE(lcTargDir + 'CODES.DBF') AND FILE(lcTargDir + 'SCALE.DBF')
  llFlOpened = .T.
  
  *-- IF files in this data directory is already opened
  IF llDataFile
    IF USED('Style')
      USE IN Style
    ENDIF
    USE (lcTargDir + 'Style') IN 0 ORDER TAG Style
  
    IF USED('StyDye')
      USE IN StyDye
    ENDIF
    USE (lcTargDir + 'StyDye') IN 0 ORDER TAG StyDye

    IF USED('Codes')
      USE IN Codes
    ENDIF
    USE (lcTargDir + 'Codes') IN 0 ORDER TAG cCode_no
  
    IF USED('Scale')
      USE IN Scale
    ENDIF
    USE (lcTargDir + 'Scale') IN 0 ORDER TAG Scale

    llDataFile = .F.
  ENDIF    
  *-- EndIF of files in this data directory is already opened
    	
  *-- If main temp file and temp reject style file is not found [Begin]
  IF !USED(lcTempFile)
    SELECT Style
    COPY STRUCTURE TO (gcWorkDir + lcTempFile)
    USE (gcWorkDir + lcTempFile) IN 0 EXCLUSIVE
  
    CREATE TABLE (gcWorkDir + lcStyRejct) ;
    (Style C(19), Scale C(25), Reason C(25))    

  ELSE
    *-- If user changed Text file or data directory
    IF llCrtChang
      SELECT (lcTempFile)
      ZAP
      SELECT (lcStyRejct)
      ZAP
    ENDIF
    *-- EndIf of user changed Text file or data directory    
  ENDIF
  *-- If main temp file and temp reject style file is not found [End]
  DO lpCreateTmp
  llCrtChang = .F.
  
ELSE        && Else any of the 3 data is not found 
  *Message : M42207 => 'ð not found in this current directory'
  *Button  : B00000 =>                '<Ok> '
  = gfModalGen('TRM42207B00000','DIALOG','Some files are')
ENDIF
*-- EndIf of all the 3 data files is found [End]
RETURN llFlOpened
*-- End of lfFlsToOpn.

*!************************************************************************** 
*! Name      : lfvConvert
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To convert to the style file.
*!************************************************************************** 
*! Calls     : lfFlsToOpn() , lpCreateTmp , gfModalGen()
*!************************************************************************** 
*! Example   :  =lfvConvert()
*!************************************************************************** 
*
FUNCTION lfvConvert
IF llCrtChang
  IF !lfFlsToOpn()
    RETURN
  ENDIF
ENDIF  

*-- If temp style file is empty
IF RECCOUNT(lcTempFile) = 0
  *Message : M00324 => 'Nothing ð , Cannot continue'
  *Button  : B00000 =>            '<Ok> '
  = gfModalGen('TRM00324B00000','DIALOG','to convert')
  SET DEVICE TO SCREEN	
  RETURN
ELSE   
  IF !USED(lcStyleDif)
    CREATE TABLE (gcWorkDir + lcStyleDif) ;
    (Style C(19),Scale C(1),Season C(2),Reason C(50))
  ELSE
    SELECT (lcStyleDif)
    ZAP
  ENDIF
  DO lpAddStyle  
ENDIF
*-- EndIf of temp style file is empty      
*-- End of lfvConvert.

*!************************************************************************** 
*! Name      : lpAddStyle
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To add the lines from style temp file to the style file.
*!************************************************************************** 
*! Calls     : gfModalGen()
*!************************************************************************** 
*! Example   : DO lpAddStyle
*!************************************************************************** 
*
PROCEDURE lpAddStyle
SELECT (lcTempFile)
*-- Scan all main temp file [Begin]
SCAN
  lnOldPrice = 0
  *-- If current style is found in Style file [Begin]
  IF SEEK(Style,'Style')
  
    *-- If Season is not like Style season [Begin]
    IF Season <> Style.Season
      INSERT INTO (lcStyleDif) (Style, Season, Reason) VALUES ;
      (&lcTempFile..Style, &lcTempFile..Season, ;
      lcMajTitle + ' already exists with different season ' + Style.Season)
    ELSE     && Else Season is like Style season

      *-- If Scale is not like Style scale [Begin]
      IF Scale <> Style.Scale
        INSERT INTO (lcStyleDif) (Style, Scale, Reason) VALUES ;
        (&lcTempFile..Style, &lcTempFile..Scale, ;
        lcMajTitle + ' already exists with different size scale ' + Style.Scale)
      ELSE      && Else Scale is like Style scale
			
        *-- If PriceA is not like Style PriceA [Begin]
        IF PriceA <> Style.PriceA
          lnOldPrice = Style.PriceA
          =RLOCK()
          REPLACE Style.PriceA WITH PriceA
          UNLOCK
          INSERT INTO (lcStyleDif) (Style, Reason) VALUES ;
                      (&lcTempFile..Style, ;
                      'Price was changed from '+ALLTRIM(STR(lnOldPrice,7,2))+' to '+ ;
					            ALLTRIM(STR(&lcTempFile..PriceA,7,2)))
        ELSE      && Else PriceA is like Style PriceA
          INSERT INTO (lcStyleDif) (Style, Reason) VALUES ;
                      (&lcTempFile..Style, lcMajTitle + '/';
                      +lcNonMajT+' already exists')
        ENDIF
        *-- EndIf of PriceA is not like Style PriceA [End]				
			
      ENDIF
      *-- EndIf of Scale is not like Style scale [End]
		
    ENDIF
    *-- EndIf of Season is not like Style season [End]
  
  ELSE     && Else current style is not found in Style file
    SCATTER MEMVAR
    m.cDefWare  = lcLocation
    m.cAdd_User = gcUser_ID
    m.dAdd_Date = DATE()
    m.cAdd_Time = gfGetTime()
    INSERT INTO Style FROM MEMVAR
    
    INSERT INTO StyDye (Style, cWareCode, Desc, Ave_Cost, cAdd_User,;
    dAdd_Date, cAdd_Time) VALUES (m.Style, lcLocation, m.Desc, m.Ave_Cost,;
    m.cAdd_User, m.dAdd_Date, m.cAdd_Time)
  ENDIF
  *-- EndIf of current style is found in Style file [End]
ENDSCAN
*-- EndScan of all main temp file [End]

=lfvReport("ICSIG11")

*-- the convert is done only once
SHOW GET pbConvert DISABLE
*-- End of lpAddStyle.

*!************************************************************************** 
*! Name      : lfvReport
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To print report with rejected styles.
*!************************************************************************** 
*! Calls     : lpPrintRep , lfFlsToOpn()
*!************************************************************************** 
*
FUNCTION lfvReport
PARAMETERS lcRepName
PRIVATE lcRepName,lnSelect,lcMessage,llDoRep,lcRjctFile,lcSetConsl

IF TYPE("lcRepName") $ "UL"
  lcRepName = "ICSIG10"
  lcRjctFile = lcStyRejct
ELSE
  lcRjctFile = lcStyleDif
ENDIF

IF llCrtChang
  IF lcRepName = 'ICSIG10' AND !lfFlsToOpn()
    RETURN
  ENDIF
ENDIF  

lcMessage = IIF(lcRepName = 'ICSIG10',"",'rejected styles')
*Message : M42208 => 'Print rejected styles report'
*Button  : B36010 => '<Preview>  <Print>  <Cancel>'
lnSelect = gfModalGen('QRM42208B36010','DIALOG',lcMessage)

*-- if user selected <none> 
IF lnSelect <> 3
  llDoRep = .T.
  R_WIDTH = 'N'
  lcOGPlatForm = "DOS"
  lcSetConsl = SET('CONSOLE')    && save console setting

  *--If user selected <Preview>
  IF lnSelect=1 
    gcDevice = "SCREEN"
  ELSE  && Else user selected <Print>
    gcDevice = "PRINTER"
    SET CONSOLE OFF                
    IF !pSetup(.T.,.F.)
      llDoRep = .F.
    ENDIF  
  ENDIF
  *--EndIf of user selected <Preview>  
  
  *-- If user selected <Preview>
  IF llDoRep
    DO lpPrintRep WITH lcRepName , lcRjctFile
    gcDevice = "SCREEN"
  ENDIF
  *-- If user selected <Preview>

SET CONSOLE &lcSetConsl
ENDIF
*-- Endif of user selected <none> 
*-- End of lfvReport.

*!************************************************************************** 
*! Name      : lpPrintRep
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To print report
*!************************************************************************** 
*! Calls     : gfModalGen() , gfDispRe
*!************************************************************************** 
*! Example   : DO lpPrintRep  
*!************************************************************************** 
*
PROCEDURE lpPrintRep
PARAMETERS lcRepName , lcTempName
SELECT (lcTempName)
GO TOP
IF RECCOUNT(lcTempName) = 0
  *-- Message : M00052 => 'There are no records to display'
  *-- Button  : B00000 =>                '<Ok> '
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ELSE
  =gfSetDevice()
  DO (gcRepHome + "gfDispRe") WITH EVALUATE('lcRepName')
ENDIF
*-- End of lpPrintRep.
  
*!************************************************************************** 
*! Name      : lpCreateTmp
*! Developer : Sameh Saiid (SSE)
*! Date      : 16/09/1999
*! Purpose   : To create temp file.
*!************************************************************************** 
*! Example   : DO lpCreateTmp  
*!************************************************************************** 
*
PROCEDURE lpCreateTmp
STORE '' TO lcLine , lcScale
lnHandle = FOPEN(lcTextFile, 0)     && open the text file with low level function

*-- Do While loop till end of text file 
DO WHILE !FEOF(lnHandle)
  lcLine     = lcLine+ALLTRIM(FGETS(lnHandle))
  lnAllFelds = OCCURS(";", lcLine)
  IF lnAllFelds < 12
    LOOP
  ENDIF
  FOR lnField = 1 TO lnAllFelds
    lcZ = ALLTRIM(STR(lnField))
    lcField&lcZ = SUBSTR(lcLine,1,ATC(";",lcLine)-1)
    lcLine      = SUBSTR(lcLine,ATC(";",lcLine)+1)
  ENDFOR

  *-- Do While loop is true (to be terminated with EXIT)
  DO WHILE .T.
    lcStyMajor = PADR((SUBSTR(lcField3,2,1) + SUBSTR(lcField3,4,1) + ' ' + ;
                 lcField4 + IIF(lcLine='+','+',' ') + lcField5),lnMajorLen)
    lcStyle    = lcStyMajor + lcSepartor + ALLTRIM(lcField6)               
    WAIT WINDOW lcMajTitle + ' ' + lcStyle NOWAIT
		
    *-- If Color is not found in Codes file [Begin]    
    IF !SEEK('N'+PADR('COLOR',10)+ALLTRIM(lcField6),'Codes')
      INSERT INTO (lcStyRejct) ;
                  (Style, Reason) VALUES ;
                  (lcStyle, 'Color code does not exist')
      EXIT
    ENDIF
    *-- If Color is not found in Codes file [End]    

    *-- make all the sizes in one string
    lcSize = STRTRAN(ALLTRIM(lcField11),',')
    DO CASE
      CASE lcSize = '343638404244'
        lcSize = '34/436/638/840/1042/1244/14'
      CASE lcSize = '34363840424446'
        lcSize = '34/436/638/840/1042/1244/1446/16'
    ENDCASE

    *-- check for the scale string in the scale file [Begin]
    llScFound = .F.
    SELECT SCALE
    =SEEK('S','SCALE')
    *-- Scan the scale file for all scales
    SCAN WHILE TYPE = 'S'
      lcScalSize = ''
      FOR lnI = 1 TO Cnt
        lcX = STR(lnI,1)
        lcScalSize = lcScalSize + ALLTRIM(Sz&lcX)
      ENDFOR
      IF lcSize = lcScalSize
        llScFound = .T.
        lcScale = Scale
        EXIT
      ENDIF
    ENDSCAN
    *-- EndScan the scale file for all scales

    *-- if scale string is not found in scale file add 
    *-- this style in the rejected temp file
    IF !llScFound    
      INSERT INTO (lcStyRejct) ;
                  (Style,Scale, Reason) VALUES ;
                  (lcStyle, ALLTRIM(lcField11), 'Sizes does not exist')
      EXIT
    ENDIF
	*-- check for the scale string in the scale file [End]

    *-- check for the season in codes file [Begin]
    lcField1 = STRTRAN(lcField1,'+')
    lcSeason = SUBSTR(lcField1,4,1) + lcField2
    IF !SEEK('N'+PADR('SEASON',10)+ALLTRIM(lcSeason),'Codes')
      INSERT INTO Codes ;
      (cDefCode, cFld_Name, cCode_No, lRltFields, cRltField) VALUES ;
      ('N', 'SEASON', lcSeason, .F., 'N')
    ENDIF
    *-- check for the season in codes file [End]

    *-- check for the cStyGroup in codes file [Begin]
    lcStyGroup = SUBSTR(lcField3,3,2)
    IF !SEEK('N'+PADR('CSTYGROUP',10)+ALLTRIM(lcStyGroup),'Codes')
      INSERT INTO Codes ;
      (cDefCode, cFld_Name, cCode_No, lRltFields, cRltField) VALUES ;
      ('N', 'CSTYGROUP', lcStyGroup, .F., 'N')
    ENDIF
    *-- check for the cStyGroup in codes file [End]
    
    *-- this commennted lines is not removed for readability
    *lnPriceA  = VAL(lcField9)
    *lnPurch   = VAL(lcField8)
    *lnMarkupA = IIF(VAL(lcField9)=0,0.00,((VAL(lcField9)-VAL(lcField8))/;
                 IIF(llStyMark,VAL(lcField8),VAL(lcField9)))*100)

    INSERT INTO (lcTempFile) ;
    (Style    , cStyMajor, cDye_Flg, Status    , cStyGroup , Scale     ,;
     cDivision, Desc     , Season  , Commission, Make      , NiCost1   ,;
     TotCost  , Ave_Cost , PriceA  , NCurrRate , cConsInfo1, cConsInfo2,; 
     Content1 , Content2 , nicost1   , MarkA, lInvSty) VALUES ;
    (lcStyle , lcStyMajor, 'N', 'A', lcStyGroup, lcScale, 'CE', SUBSTR(lcField7,1,20),;
     lcSeason, .F., .F., VAL(lcField8), VAL(lcField8), VAL(lcField8),  ;
     VAL(lcField9), 1, SUBSTR(lcField7,1,30), SUBSTR(lcField7,30,30),  ;
     SUBSTR(lcField12,1,20), SUBSTR(lcField12,21,20), VAL(lcField8), ;
     IIF(VAL(lcField9) = 0, 0.00, ((VAL(lcField9) - VAL(lcField8));
     /IIF(llStyMark,VAL(lcField8),VAL(lcField9)))*100) ,.T.)  
    EXIT
  ENDDO
	*-- EndDo While loop
ENDDO
*-- EndDo While loop of the text file 

= FCLOSE(lnHandle)      && close the text file with low level function
WAIT CLEAR
*-- End of lpCreateTmp.

*!**************************************************************************
*! Name      : lfMajSeg
*! Developer : Sameh (SSE)
*! Date      : 16/09/1999
*! Purpose   : To get the style major segment structure
*!**************************************************************************
*! Calls     : gfItemMask()
*!**************************************************************************
*! Example   : = lfMajSeg()
*!**************************************************************************
*
FUNCTION lfMajSeg
*-- Get the number of segments in the major partition 
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lcSepartor = laMajSeg[lnMajSeg,6]
lcMajTitle = gfItemMask('HM')
lnMajorLen = LEN(gfItemMask('PM'))

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C'
    lnClrPo    = laMajSeg[lnI,4]
    lnColorLen = LEN(laMajSeg[lnI,3])
    lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
    EXIT  
  ENDIF
ENDFOR    && end Loop Around Non Major elements.
RETURN ''
*-- End of lfMajSeg.
