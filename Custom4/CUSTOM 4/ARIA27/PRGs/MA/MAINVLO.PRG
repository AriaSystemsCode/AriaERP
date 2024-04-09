*:************************************************************
*: Program file  : maInvLo
*: Program desc. : Material Inventory Bin
*: For screen    : maInvLo
*:        System : Aria Advantage Series.
*:        Module : Material (MA).
*:     Developer : Ahmed Amer (AHM)
*:************************************************************
*: Calls : 
*:         Procedures : lpShow,lpSavscr
*:         Functions  : gfSetup,gfOpenFile,Mainvlo,lfvData_1,lfvData_2,
*:                    : gfCodDes,gfSeekRec,lfWHBrow,lfWBrow,lfvpbWhse,lfVrem,  
*:                    : lfvpbLoc,lfvGLWarehs,lfOldvalue,lfTrap,lfClrTrap  
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO maInvLo
*:        
*:************************************************************
*:* B803431,1 ADEL 07/25/2000 Fix the bug of "Alias not found".
*:************************************************************

*-- laData     is array to hold the values of the header screen controls
*-- laScrMode  is array to hold the values of screen mode
*-- laKeyField is array to hold the information of the key fields
*-- laSource   is array to hold the whole available locations or bins to send it to mover
*-- laTarget   is array to hold the assigned locations or bins to send it to mover    
DIMENSION laData[1], laScrMode[4], laKeyField[2,4],laSource[1],laTarget[1]

laDefproc[9] = .F.                 && This is to prevent the global save

*-- lcMainv1   : This is to hold the name of the header screen
*-- lcMainv2   : This is to hold the name of the Browse screen
*-- lcMainv3   : This is to hold the name of the push button screen
*-- lcMainv4   : This is to hold the name of the information screen
*-- laData     : This is to hold the values of the key fields in the sreen
*-- lcScFields : This is to hold the name of the key filelds in the table
*-- lcDesc     : This is to hold the descreption of the color
*-- laSource   : This is to hold the values that are sent to the mover as the source
*-- laTarget   : This is to hold the values that are sent to the mover as the target
*-- lcGLWareHs : This is to hold the value of Gl_Link Code to be sent to function gfGLBrowse
*-- lcGLWaDesc : This is to hold the value of Gl_Link Desc to be sent to function gfGLBrowse
*-- lcOldValue : This is to hold the value of the Gl_Link befor change it
*-- lcWhs      : This is to hold the locatione code for the current record for the browse screen
*-- lcWareFil  : This is to hold the temp name of the cursor that hold the locations
*-- lcLocFil   : This is to hold the temp name of the cursor that hold the bins
*-- lcGL_Link  : This is to hold "GL Link" if the system is linked to Gl 
STORE SPACE(0) TO lcMainv1, lcMainv2, lcMainv3, lcMainv4, laData, ;     
                  lcScFields, lcDesc, laSource, laTarget, lcGLWareHs, ; 
                  lcGLWaDesc ,lcOldValue, lcWhs, lcWareFil, lcLocFil,lcGL_Link

*-- lcWhsMod   : This is hold the mode "ENABLE/DISABLE" for the locations button according to multi locations or not lcLocMod 
*-- lcLocMod   : This is hold the mode "ENABLE/DISABLE" for the loactions button according to multi Bins or not lcLocMod 
STORE 'DISABLE' TO lcWhsMod, lcLocMod 

*-- llBrowse   : This is to show if the browse button has clicked
*-- llFromEdit : This is to show if you come from rdit mode
*-- glFromBrow : True if you comming from Browse, Else it is False
STORE .F. TO llBrowse, llFromEdit, glFromBrow

lcBrTtl    = 'Locations'          && This is to hold the title of the browse screen
lnBrRecNo  = 0                    && This is to hold the no of the current record of the browse screen

laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.F.
laKeyField[1,3] = "FABRIC"
laKeyField[1,4] = 1

laKeyField[2,1] = "laData[2]"
laKeyField[2,2] = .T.
laKeyField[2,3] = "FABRIC"
laKeyField[2,4] = 2

*--  This is to hold the returned value of the function gfopen() to show
*--  if the file has opened by this fuction or it is already opened
llGlopn   = .F.                    

IF !gfSetup()
  RETURN
ENDIF  

*-- This is to hold the restored value from the gfGetMemVar function 
*-- to show if there is link with GL or not, multi locations and multi Bins
llLink_GL  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL')))   = 'Y'
lcGL_Link = IIF(llLink_GL,"GL Link",SPACE(0))

llMultiWH  = ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS')))  = 'Y'        && To hold multiple location facility.
llMultiLoc = ALLTRIM(UPPER(gfGetMemVar('M_WARELOC')))   = 'Y'

IF !llMultiLoc .AND. !llMultiWH          &&Do not use multi Bin.
  *-- THIS MESSAGE IS 'System has not been setup to keep track of Locations or Bins.'
  = gfModalGen("INM36073B36000","Dialog")
  llReturn = .F.
ELSE
  llReturn = .T.
ENDIF

IF llReturn
  IF llLink_GL 
    llGlopn =gfOpenFile(gcDataDir+'Gl_Link',gcDataDir+'Gl_Link1','SH')
  ENDIF

  laCtrStat[8]  = "DISABLE"               && Disable Delete button

  IF !WEXIST(gcBaseWind)
    lcMainv1   = gfTempName()
    lcMainv2   = gfTempName()
    lcMainv3   = gfTempName()
    lcMainv4   = gfTempName()
  
    lcLocFil   = gfTempName()         && Hold the temp file of bins.
    lcWareFil  = gfTempName()         && Hold the temp file of location.
  
    lcScFields = "Fabric, Color"
    SELECT Fabric
    SCATTER FIELDS &lcScFields TO laData BLANK
  ENDIF
  SELECT Warehous
  SET ORDER TO Warehous

  *-- create relation between the FBDYE and location file depending on cWarecode
  SELECT FABDYE
  SET ORDER TO TAG FABDYE
  SET RELATION TO cWareCode INTO WAREHOUS
  SELECT Fabric
  SET ORDER TO Fabric
  PUSH KEY                                     && To save the the current on key label
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl) && To activate the browse screen when pressing ALT+B
  DO (gcScrDir + gcWinAppl + '\MAINVLO.SPR')   && calling the screen Mainvlo
  RELEASE WINDOW (lcBrTtl)
  POP KEY                                      && To Restore the previous assignments for on key label

  SELECT FABDYE
  SET RELATION TO               && Breake the relation between the FABDYE file and the location file

  *-- If we realy quitting the screen 
  IF glQuitting                 
    IF USED(lcWareFil)
      USE IN (lcWareFil)
    ENDIF
    ERASE (gcWorkDir+lcWareFil+".DBF")
    ERASE (gcWorkDir+lcWareFil+".CDX")
    IF USED(lcLocFil)
      USE IN (lcLocFil)
    ENDIF
    ERASE (gcWorkDir+lcLocFil +".DBF")
    ERASE (gcWorkDir+lcLocFil +".CDX")
  ENDIF  

  IF llGlopn
    USE IN GL_LINK
  ENDIF
ENDIF
  
RETURN (llReturn)

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (Fabric).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : FaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_1()
*!*************************************************************
FUNCTION lfvData_1
PRIVATE lnAlias        && This to hold the number of the current alias

laData[1] = PADR(ALLTRIM(laData[1]), FSIZE('Fabric', 'FABRIC'))
IF llBrowse OR (!EMPTY(laData[1]) AND !SEEK(laData[1], "Fabric"))
  SELECT Fabric
  llBrowse = .F.
  lnAlias  = SELECT(0)
  lcFabric = laData[1]
  = FaBrow(@lcFabric, "*")          && Calling the global browse for fabric
  laData[1] = lcFabric
  IF EMPTY(laData[1])
    _CUROBJ = _CUROBJ
  ENDIF  
  SELECT(lnAlias)
ENDIF  

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (Color).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : FaBrow,gfCodDes,gfSeekRec
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_2()
*!*************************************************************

FUNCTION lfvData_2
laData[2] = PADR(ALLTRIM(laData[2]), FSIZE('Color', 'FABRIC'))

IF llBrowse OR !EMPTY(laData[2])
  IF llBrowse OR !SEEK(laData[1]+laData[2], "Fabric"))
    SELECT Fabric
    llBrowse = .F.
    lnAlias  = SELECT(0)
    lcFabric = laData[1]
    lcColor  = IIF(EMPTY(laData[1]),laData[2],CHR(240))
    IF !FaBrow(@lcFabric,@lcColor)        && Calling the global browse for fabric
      laData[2] = SPACE(FSIZE('Color', 'FABRIC'))
      _CUROBJ = OBJNUM(laData[2])
    ELSE
      laData[1]    = lcFabric
      laData[2]    = lcColor
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ENDIF
    SELECT(lnAlias)
  ELSE  
    laScrMode    = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF  
  lcDesc    = IIF(!EMPTY(laData[2]), gfCodDes(laData[2], "COLOR"), SPACE(0)) 
ENDIF  

*!*************************************************************
*! Name      : lfWHBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse the locations assigned to the Fabric/Color.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfwBrow,lfvBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWHBrow()
*!*************************************************************

FUNCTION lfWHBrow
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

IF laScrMode[3]
  SELECT (lcWareFil)
  GOTO TOP
  BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:R:H=' ':W=.F.,;
                cWareCode :H="Location", WAREHOUS.cDesc :H="Location Desc."   ;
         SAVE              ;
         NOWAIT            ;
         NOAPPEND	 	   ;
         NODELETE          ;
         NOEDIT            ;
         NOMENU            ;
         NOCLEAR           ;
         TITLE (lcBrTtl)   ;
         WHEN lfwBrow()    ;
         VALID :F lfVBrow();         
         WINDOW (lcMainv2) ;
         IN WINDOW (gcBaseWind)         
ELSE

  SELECT FabDye
  BROWSE FIELDS cMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:R:H=' ':W=.F.,;
                cWareCode :H="Location", WAREHOUS.cDesc :H="Location Desc."   ;
         FOR    Fabric+Color+cWareCode+Dyelot+STR(RECNO(),7) = ;
                laData[1]+laData[2] AND EMPTY(Dyelot) ;     
         SAVE              ;
         NOWAIT            ;
         NOAPPEND	 	   ;
         NODELETE          ;
         NOEDIT            ;
         NOMENU            ;
         NOCLEAR           ;
         TITLE (lcBrTtl)   ;
         WHEN lfwBrow()    ;
         VALID :F lfVBrow(); 
         WINDOW (lcMainv2) ;
         IN WINDOW (gcBaseWind)         
ENDIF
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfwBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Assign cWarecode to lcWhs and GL_Link to lcGlWareHs
*!           : and the record number to lnMBrRcNo
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow()
*!*************************************************************

FUNCTION lfwBrow
PRIVATE lnAlias

lnAlias = SELECT(0)

lcFilToUse = IIF(laScrMode[3], lcWareFil, "FABDYE")
SELECT (lcFilToUse)
lnBrRecNo  = RECNO(lcFilToUse)
lcWhs      = cWarecode
lcLocMod = IIF(EOF() OR BOF() OR DELETED(), "DISABLE", "ENABLE")
SHOW GET pbLoc  &lcLocMod 
lcGLWareHs = &lcFilToUse..GL_Link
*B803431,1 (Begin) Don't seek in GL_LINK unless the system is linked to GL.
*lcGLWaDesc = IIF(SEEK('04'+PADR(lcGLWareHs,6), 'GL_LINK'), GL_LINK.linkdesc,"")
lcGLWaDesc = IIF(llLink_GL AND SEEK('04'+PADR(lcGLWareHs,6), 'GL_LINK'), GL_LINK.linkdesc,"")
*B803431,1 (End)
llNoThing  = lfRefresh(lcMainv4)
SHOW GET lcGLWareHs
SHOW WINDOW (lcBrTtl) REFRESH SAME

SELECT(lnAlias)

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWHBrow,lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************

PROCEDURE lpShow
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

DO CASE
  *-- In select mode
  CASE laScrMode[1]
    DIMENSION laTarget[1]
    laTarget[1] = SPACE(0)
    SHOW GET pbWhse DISABLE
    SHOW GET pbLoc  DISABLE
    
    SELECT Fabric
    SCATTER FIELDS &lcScFields TO laData BLANK
    lcDesc     = SPACE(0)
    lcGLWareHs = SPACE(0)
    STORE 'DISABLE' TO lcWhsMod, lcLocMod
    _CUROBJ   = OBJNUM(laData[1])
    =lfwBrow()
        
  *-- In view mode
  CASE laScrMode[2]
    
    lcWhsMod  = IIF(llMultiWH , "ENABLE", "DISABLE")
    lcLocMod  = IIF(llMultiLoc, "ENABLE", "DISABLE")    
    SHOW GET pbWhse &lcWhsMod
    SHOW GET pbLoc  &lcLocMod 

    lcDesc    = gfCodDes (laData[2], "COLOR")        && having the desc of the color code in laData[2]
   
    IF llFromEdit
      llFromEdit = .F.
      RELEASE WINDOW (lcBrTtl)
      llNoThing = lfWHBrow() AND lfwBrow()
    ELSE
      SELECT FABDYE
      SEEK laData[1]+laData[2]
      =lfwBrow()
    ENDIF  

  *-- In edit mode
  CASE laScrMode[3]

    *-- Fill the (lcWarefil)  data that suit 
    *-- the fabric and color (laData[1]/laData[2]) FROM FABDYE
      SELECT Fabric,Color,cWarecode,nfAve_Cost,onHand,GL_Link, "S" AS cStatus ;
      FROM FABDYE ;
      WHERE fabric    + color                 + cwarecode + dyelot    + STR(RECNO(),7) like;
            laData[1] + PADR(laData[2],6,' ') + '______'  + SPACE(10) + '_______';
      INTO DBF (gcWorkDir+lcWareFil) 
    INDEX ON cWarecode TAG (lcWareFil)
    SET ORDER TO TAG (lcWareFil)
     
    *-- Fill the (lcLocfil) with the data that suit 
    *-- the fabric and color (laData[1]/laData[2])
    SELECT Style,Color,cWarecode,cLocation, "S" AS cStatus ;
      FROM WHSLOC                                 ;
      WHERE Style                  + Color                 + cWarecode + cLocation LIKE ;
            PADR(laData[1],19,' ') + PADR(laData[2],6,' ') + '%'  ;
      INTO DBF (gcWorkDir+lcLocFil) 
    INDEX ON cWarecode+cLocation TAG (lcLocFil)
    SET ORDER TO TAG (lcLocFil)
    SELECT (lcWareFil)
    SET RELATION TO cWareCode INTO WAREHOUS
    GOTO TOP

    llFromEdit = .T.
    RELEASE WINDOW (lcBrTtl)
    llNoThing = lfWHBrow() AND lfwBrow()

ENDCASE

*E300732,1 Disable Delete buttons always
SHOW GET pbDlt DISABLE

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvpbWhse
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : calling the gfmover and handling the returned laTarget
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvpbWhse()
*!*************************************************************

FUNCTION lfvpbWhse
PRIVATE lnCurAlias,lcCurNdx,lnI,lnJ

SELECT cWarecode FROM WAREHOUS INTO ARRAY laSource
IF _TALLY = 0
  *-- This message shows"there is no locations"
  = gfModalGen("INM36068B36000","Dialog")
ELSE
  IF laScrMode[3]   
    SELECT cWarecode FROM (lcWareFil) ;
      WHERE cStatus != "D"            ;
      INTO ARRAY laTarget
  ELSE
   *-- Fill the (lcWarefil)  data that suit 
   *-- the fabric and color (laData[1]/laData[2]) FROM FABDYE
   SELECT cWarecode FROM FABDYE ;
     WHERE fabric    + color                 + cwarecode + dyelot    + STR(RECNO(),7) like;
           laData[1] + PADR(laData[2],6,' ') + '______'  + SPACE(10) + '_______';
     INTO ARRAY laTarget
  ENDIF 

 *-- To hold true if we are in edit or add mode to be send to gfmover()
  llOpenall = laScrMode[4] OR laScrMode[3]     

  = gfMover(@laSource,@laTarget,"Assign locations",llOpenall,'lfvRem',.F.,.T.)  

  IF laScrMode[3]
    lnCurAlias = SELECT(0)
    SELECT (lcWareFil)
    *-- To save the returned laTarget in the (lcWareFil) with status
    *-- 'A' for new or 'D' for deleted
    SCAN
      IF ASCAN(laTarget,cWarecode) = 0
        REPLACE cStatus WITH "D"
        SELECT(lcLocFil)
        SCAN FOR cWarecode+cLocation = &lcWareFil..cWareCode + SPACE(0)
          REPLACE cStatus WITH "D" 
          DELETE
        ENDSCAN 
        SELECT (lcWareFil)
        DELETE
      ENDIF
    ENDSCAN
  
    FOR lnJ = 1 TO ALEN(laTarget)
      IF !EMPTY(laTarget[lnJ]) AND !SEEK(laTarget[lnJ])
        APPEND BLANK
        REPLACE Fabric    WITH laData[1],     ;
                Color     WITH laData[2],     ;
                cWarecode WITH laTarget[lnJ], ;
                cStatus   WITH "A"
      ENDIF
    ENDFOR
    = lfwBrow()
    SELECT (lnCurAlias)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvRem
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the removing locations
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : lnBttn 
*!*************************************************************
*! Returns            : llRetVal
*!*************************************************************
*! Example   : = lfvRem()
*!*************************************************************

FUNCTION lfvRem
PARAMETERS lnBttn
*-- lnBttn hold the passed parameter that show which button called the 
*-- function (1=move,2=moveall,3=remove,4=removeall)
PRIVATE lnI,lnJ
PRIVATE llRetVal    && To hold the returned value

llRetVal = .F.
lnTotStk = 0

lnCurAlias = SELECT(0)
SELECT FABDYE
lcCurNdx = ORDER()
SET ORDER TO FABDYEW
lcOldExact = SET("EXACT")
SET EXACT OFF
    
DO CASE
  CASE lnBttn <= 2
    llRetVal = .T.
        
  CASE lnBttn = 3
    *-- This message is "Are you sure you want to remove location"
    *-- lcChoice to hold the return of the message function
    *-- 1 = yes , 2 = no
    lcChoice = gfModalGen("QRM36069B36001","Dialog",laTarget[lsTarget])                            
    IF lcChoice = 1
      IF SEEK(&lcWareFil..cWareCode + SPACE(0),lcLocFil)
        *-- This message is "Location bins have been assigned ;
                             to location XXXXX, ;
                             Are you sure you want to remove it?"
        *-- lcChoice to hold the return of the message function
        *-- 1 = yes , 2 = no          
        lcChoice = gfModalGen("QRM36083B36001","Dialog",laTarget[lsTarget])                                      
      ENDIF
    ENDIF
    IF lcChoice = 1
      *-- Check if there is any stock or ordered quantities in 
      *-- this location, for this material\color.
      IF SEEK (laTarget[lsTarget]+laData[1]+laData[2],'FabDye')
        llRetVal = !(FabDye.OnHand > 0)
        lnTotStk = FabDye.OnHand
      ELSE
        llRetVal = .T.
        lnTotStk = 0
      ENDIF
    ELSE
      llRetVal = .F.
    ENDIF

    IF lnTotStk > 0
      *-- Check if there is link with Gl.
      IF llLink_GL
        lcTmpStr = laData[1]+"|"+laData[2]                             
        *-- This message is "Material/Color has inventory. 
        *--                  Please adjust the existing inventory
        *--                  befor removing the material from location."
        = gfModalGen("INM36072B36000","Dialog",lcTmpStr)                            
        llRetVal = .F.
      ELSE
        *-- This message is "Material/color has inventory.
        *--                  Are you sure you want remove the Material
        *--                  from this location"
        lcTmpStr = laData[1]+"|"+laData[2]                             
        lcConfirm = gfModalGen("QRM36071B36001","Dialog",lcTmpStr)                            
        llRetVal = !(lcConfirm = 1)
      ENDIF
    ENDIF          

ENDCASE
SET ORDER TO (lcCurNdx)
SELECT (lnCurAlias)
SET EXACT &lcOldExact

RETURN (llRetVal)

*!*************************************************************
*! Name      : lfvpbLoc
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : calling the gfmover and handling the returned laTarget
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvpbLoc()
*!*************************************************************

FUNCTION lfvpbLoc

PRIVATE llOpenall,lnCurAlias
DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
llOpenall = laScrMode[4] OR laScrMode[3]

*-- create array with the whole bins for this location
SELECT DISTINCT cLocation FROM WHSLOC ;
  WHERE CWARECODE == lcWhs ;
  AND   EMPTY(Style+Color) ;
  INTO  ARRAY laSource
IF _TALLY = 0
  *-- This message is "No bins assigned for location"
  = gfModalGen("QRM36070B36000","Dialog",lcWhs)                            
ELSE
  IF laScrMode[3]
    *-- create array with the bins assigned for this 
    *-- fabric/color in this location
    SELECT cLocation FROM (lcLocFil) ;
      WHERE CWARECODE == lcWhs     ;
      AND   Style     == laData[1] ;
      AND   Color     == laData[2] ;  
      AND   cStatus   != "D"       ;    
      INTO ARRAY laTarget
  ELSE
    SELECT cLocation FROM WHSLOC ;
      WHERE Style + Color + cWarecode + cLocation LIKE ;
            PADR(laData[1],19,' ')+ PADR(laData[2],6,' ') + lcWhs + '%'  ;
      INTO ARRAY laTarget
  ENDIF
 
  = gfMover(@laSource,@laTarget,"Assign Bins",llOpenall,'')  
  IF laScrMode[3]
    lnCurAlias = SELECT(0)
    SELECT (lcLocFil)
    *-- check the returned laTarget to detremine the status
    *-- 'D' for deleted or 'A' for new

    SCAN FOR cWarecode = lcWhs
      IF ASCAN(laTarget,cLocation) = 0 
        REPLACE cStatus WITH "D"
        DELETE
      ELSE
        REPLACE cStatus WITH IIF(cStatus=="S","S","A")
      ENDIF
    ENDSCAN
    
    FOR lnJ = 1 TO ALEN(laTarget)
      IF !EMPTY(laTarget[lnJ]) AND !SEEK(lcWhs+laTarget[lnJ])
        APPEND BLANK
        REPLACE Style     WITH laData[1]    , ;
                Color     WITH laData[2]    , ;
                cWarecode WITH lcWhs        , ;
                cLocation WITH laTarget[lnJ], ;
                cStatus   WITH "A"
      ENDIF
    ENDFOR
    SELECT(lnCurAlias)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local save.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************

PROCEDURE lpSavscr

PRIVATE lnCurAlias,lcCurNdx

lcDelStat = SET("DELETED")
lnCurAlias = SELECT(0)
SET DELETED OFF
SELECT (lcWareFil)

*-- check the status of each record in the (lcWareFil)
*-- if it is 'A' we will add new record to file FABDYD.DBF
*-- if it is 'D' we will delete this record
*-- if it is 'M' we will replace the GL_Link Field with the new value of Gl_Link
SCAN
  DO CASE
    CASE cStatus = 'A'
      *-- In case of assigning a Fabric/Color to a location then
      *-- append a new record to the master FabDye file.
      *-- Initialize the average cost for this Fabric/color in this location
      *-- by the Fabric avarage cost for this Fabric/color from Fabric file.
      SCATTER TO laRecArr
      SELECT FabDye
      APPEND BLANK
      *-- AAMER Audit fields (Start)
      *REPLACE Fabric     WITH laRecArr[1], ;
              Color      WITH laRecArr[2], ;
              cWarecode  WITH laRecArr[3], ;
              GL_Link    WITH laRecArr[6], ;              
              dAdd_Date  WITH Date()     ,;
              cAdd_Time  WITH gfGettime(),;
              cAdd_User  WITH gcUser_Id  ,;
              nfAve_Cost WITH Fabric.nfAve_Cost
      REPLACE Fabric     WITH laRecArr[1], ;
              Color      WITH laRecArr[2], ;
              cWarecode  WITH laRecArr[3], ;
              GL_Link    WITH laRecArr[6]
      = gfAdd_Info('FabDye')
      *-- AAMER Audit fields (End)
    CASE cStatus = 'M'
      SELECT (lcWareFil)
      SCATTER TO laRecArr
      SELECT FabDye
      IF SEEK (laData[1]+laData[2]+&lcWareFil..cWareCode+SPACE(10))
        LOCATE REST WHILE fabric + color+cwarecode+dyelot;
	                = laData[1]+laData[2]+&lcWareFil..cWareCode+SPACE(10);
            FOR !DELETED()
        IF FOUND()
          =RLOCK()
          *-- AAMER Audit fields (Start)
          *REPLACE GL_Link WITH laRecArr[6],    ;
                  Mod_Action WITH 'MODIFIED',  ;
                  Mod_Date   WITH DATE(),      ;
                  Mod_Time   WITH gfGettime(), ;
                  Mod_UserId WITH gcUser_Id
          
          REPLACE GL_Link WITH laRecArr[6]
          = gfAdd_Info('FabDye')
          *-- AAMER Audit fields (Endif)
          UNLOCK
        ENDIF
      ENDIF
    CASE cStatus = 'D'
      *-- If we want to remove a Fabric/Color from a location, then
      *-- we have to do the following :
      SELECT FabDye
      *- Go to the location record in the FabDye file.
      IF SEEK (laData[1]+laData[2]+&lcWareFil..cWareCode+SPACE(10))
        lnWrTotStk = OnHand
        *- Delete the location and the Dyelot(s) record(s).
        DELETE REST WHILE Fabric+Color+cWareCode = laData[1]+laData[2]+&lcWareFil..cWareCode
                   
        *- Remove the deleted location stock from the Fabric stock.
        SELECT Fabric
        =RLOCK ()
        *-- Decreasing the total onhand in fabric with the value of onhand in fabdye for this bin
        REPLACE OnHand WITH OnHand - lnWrTotStk
        UNLOCK
      ENDIF
  ENDCASE
ENDSCAN
SHOW GET lcGLWareHs

*-- check the status of each record in the (lcLocFil)
*-- if it is 'A' we will add new record to file WHSLOC.DBF
*-- if it is 'D' we will delete this record

SELECT (lcLocFil)
GO TOP

SCAN
  DO CASE
    CASE cStatus = 'A'
      SCATTER TO laLocArr
      SELECT WHSLOC
      APPEND BLANK
      REPLACE STYLE     WITH laLocArr[1], ;
              Color     WITH laLocArr[2], ;
              cWarecode WITH laLocArr[3], ;
              cLocation WITH laLocArr[4]

    CASE cStatus = 'D'
      SELECT WHSLOC
      lcCurNdx = ORDER()
      SET ORDER TO WHSLOCST
      IF SEEK(PADR(laData[1],19)+PADR(laData[2],6)+PADR(&lcLocFil..cWareCode,6)+PADR(&lcLocFil..cLocation,10))
        BLANK
        DELETE
      ENDIF
      SET ORDER TO (lcCurNdx)
  ENDCASE
ENDSCAN
SELECT (lnCurAlias)  
SET DELETED &lcDelStat

*!*************************************************************
*! Name      : lfvGLWareHs
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To show the GL_Link codes to assign GL_Link to location
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfGLBrowse
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvGLWareHs()
*!*************************************************************

FUNCTION lfvGLWareHs
PRIVATE lcCurAlias

lcCurAlias = SELECT(0)

IF llBrowse .OR. !EMPTY(lcGLWareHs)
  SELECT GL_Link
  *-- calling the global browse for Gl_Link
  =gfGLBrowse('04',@lcGLWareHs,@lcGLWaDesc)
  llNoThing  = lfRefresh(lcMainv4)
  IF EMPTY(lcGLWareHs)
    lcGLWareHs = lcOldValue
  ENDIF
ENDIF  
SELECT(lcWareFil)

*-- if the gl_link has changed we replace the gl_link in the (lcWarefil)
*-- with the new gl_link and make the status 'M' modified if it is not 'A'
IF lcGLWareHs <> lcOldValue
  REPLACE &lcWareFil..GL_Link WITH lcGLWareHs ,;
          &lcWareFil..cStatus WITH IIF (&lcWareFil..cStatus='A','A','M') 
ENDIF
SELECT (lcCurAlias)
llBrowse = .F.

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To save the old value of the GL_link befor change it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************

FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrap()
*!*************************************************************

FUNCTION lfTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTtl
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lfBrTab
  ON KEY LABEL BACKTAB    DO lfBrBack
ENDIF

*!*************************************************************
*! Name      : lfClrTrap
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClrTrap()
*!*************************************************************

FUNCTION lfClrTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

IF glFromBrow
  =gfStopBrow()
ENDIF

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() function
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrow()
*!*************************************************************

FUNCTION lfVBrow
IF WONTOP() # (lcBrTtl)
  =gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfBrTab
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfBrTab

ON KEY LABEL TAB
ACTIVATE WINDOW (lcMainv3) 
_CUROBJ = OBJNUM(pbWhse)

*!*************************************************************
*! Name      : lfBrBack
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrBack()
*!*************************************************************

FUNCTION lfBrBack
ON KEY LABEL BACKTAB
IF laScrMode[1]
  ACTIVATE WINDOW (lcMainv1)
  _CUROBJ = OBJNUM(laData[2])
ELSE
  ACTIVATE WINDOW gwcContrl1
  _CUROBJ = OBJNUM(pbCls)
ENDIF  

