*!**************************************************************************
*! Name      : DARMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/05/02
*! Purpose   : Dare Bare Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C200388,1
*MODIFICATIONS
*B606449,1 TMI Zeroing lnBarNo so the option can be rerun
*B606735,1 TMI 12/08/2002 Save office path in a separte file c:\oficpath.mem to 
*B606735,1                consider local installations of office
*C200439,1 ABD 12/12/2002 New Fields add to Material PO Detail Screen.
*B607437,1 AMH 07/28/2003 Empty the ctktno field in case of return to different warehouse 
*B037268,1 ABD 12/09/2003 Amendment to PO cost sheet 'issue/return' to allow return 
*B037268,1 ABD            Of trims to a different warehouse.
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.



*!**************************************************************************
*! Name      : lfGETDIRS
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/05/02
*! Purpose   : Do every thing accordiling to the settinges allowed.m
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200388
FUNCTION lfGETDIRS

STORE "" TO lcPath1,lcPath2,lcPath3,lcPath4,lcPath5,lcPOffice
*-- Check the memo file existance.
llFoundP = FILE('DARMAIN.MEM')
IF llFoundP
  RESTORE FROM DARMAIN.MEM ADDITIVE
ENDIF

lnRecUpd = 0
*--Get the 5 paths
lcFile   = ""
*--Open Objects files
IF !USED('OBJLINK')
  =gfOpenFile(gcDataDir+'OBJLINK','Objlnkty','SH')
ENDIF
IF !USED('objects')  
   =gfOpenFile(gcDataDir+'objects','Objectid','SH')
ENDIF
FOR lnFrom = 1 TO 5
  lcFrom = STR(lnFrom,1)
  STORE .F. TO llDoc,llXls,llJpg
  *--Internal loop for the 3 file types
  IF !EMPTY(lcPExt&lcFrom)
    DIMENSION laFiles[1]
    laFiles = ""
    lcFile  = ""
    llDoc =  .T.
    =ADIR(laFiles,lcPath&lcFrom+SUBSTR(ALLTRIM(laData[1]),1,5)+'*.'+lcPExt&lcFrom)
    IF ALEN(laFiles,1) > 1
      DO lpMover
    ELSE
      *IF FILE(lcPath&lcFrom+ALLTRIM(laData[1])+'.'+lcPExt&lcFrom)
      IF FILE(lcPath&lcFrom+ALLTRIM(laFiles[1])+'.'+lcPExt&lcFrom)
        *lcFile = lcPath&lcFrom+ALLTRIM(laData[1])+'.'+lcPExt&lcFrom
        lcFile = lcPath&lcFrom+ALLTRIM(laFiles[1])
      ENDIF  
    ENDIF  
    IF !EMPTY(lcFile)
      DO lpUpdate
    ENDIF  
  ENDIF    
ENDFOR
WAIT WINDOW STR(lnRecUpd,2)+ ' records are updated.' NOWAIT
*B606449,1 TMI [START] Zeroing lnBarNo so the option can be rerun
lnBarNo = 0
*B606449,1 TMI [END  ] Zeroing lnBarNo so the option can be rerun


PROCEDURE lpUpdate

PRIVATE lnAlias
lnAlias = SELECT()
IF !SEEK('S'+laData[1],'OBJLINK')
  SELECT OBJECTS
  lcObjCode = lfGetLastObj()
  SELECT OBJLINK  
  lnRecUpd = lnRecUpd + 1
  APPEND BLANK
  REPLACE cobject_id WITH lcObjCode,;
          cObjLink   WITH PADR(laData[1],20),;
          cObjLnkTyp WITH 'S',;
          cObj_Desc  WITH SUBSTR(lcFile,RAT("\",lcFile)+1)

  IF INLIST(RIGHT(ALLTRIM(lcFile),3),'XLS','DOC')
    REPLACE mobj_notes WITH 'Please hit the OPEN icon to view the attached '+IIF(RIGHT(ALLTRIM(lcFile),3)='XLS','Excel Sheet.','Document.')
  ELSE
    IF INLIST(RIGHT(ALLTRIM(lcFile),3),'JPG','BMP')
      REPLACE mobj_notes WITH 'Please hit the ZOOM icon to view the attached '+RIGHT(ALLTRIM(lcFile),3)
    ENDIF  
  ENDIF

  SELECT objects
  APPEND BLANK
  REPLACE cobject_id WITH lcObjCode,;
          cImgPath   WITH lcFile,;
          llnk       WITH .T.
ELSE
  SELECT OBJLINK
  llNewLine = .T.
  SCAN REST WHILE cobjlnktyp+cobjlink =  'S'+laData[1] 
    IF SEEK(cObject_id,'objects') AND UPPER(ALLTRIM(objects.cImgPath))==UPPER(ALLTRIM(lcFile))
      llNewLine = .F.
      EXIT
    ENDIF
  ENDSCAN
  IF  llNewLine
    lnRecUpd = lnRecUpd + 1  
    SELECT OBJECTS
    lcObjCode = lfGetLastObj()
    SELECT OBJLINK
    APPEND BLANK
    REPLACE cobject_id WITH lcObjCode,;
            cObjLink   WITH PADR(laData[1],20),;
            cObjLnkTyp WITH 'S',;
            cObj_Desc  WITH SUBSTR(lcFile,RAT("\",lcFile)+1)

  IF INLIST(RIGHT(ALLTRIM(lcFile),3),'XLS','DOC')
    REPLACE mobj_notes WITH 'Please hit the OPEN icon to view the attached '+IIF(RIGHT(ALLTRIM(lcFile),3)='XLS','Excel Sheet.','Document.')
  ELSE
    IF INLIST(RIGHT(ALLTRIM(lcFile),3),'JPG','BMP')
      REPLACE mobj_notes WITH 'Please hit the ZOOM icon to view the attached '+RIGHT(ALLTRIM(lcFile),3)
    ENDIF  
  ENDIF

    SELECT objects
    APPEND BLANK
    REPLACE cobject_id WITH lcObjCode,;
            cImgPath   WITH lcFile,;
            llnk       WITH .T.
  ENDIF
ENDIF
SELECT (lnAlias)


*!*************************************************************************
*! Name      : lfGetLastObj
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/05/02
*! Purpose   : function to get sequential object ID
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*
FUNCTION lfGetLastObj

lcFilter = FILTER()
SET FILTER TO
GO BOTT
lcReturn = PADL(INT(VAL(COBJECT_ID))+1,10,'0')
SET FILTER TO &lcFilter

RETURN lcReturn


PROCEDURE lpMover

DECLARE laRpTarget[1]
laRpTarget = ""
DIMENSION laSource[ALEN(laFiles,1)]
FOR lnI = 1 TO ALEN(laFiles,1)
  laSource[lnI] = laFiles[lnI,1]
ENDFOR
*--Fill the target array
DO lpGetFile

=ASORT(laSource)
= gfMover(@laSource,@laRpTarget,'Select objects from: '+lcPath&lcFrom,.T.,'')
FOR lnI = 1 TO ALEN(laRpTarget,1)
  IF !EMPTY(laRpTarget)
    lcFile = lcPath&lcFrom+ALLTRIM(laRpTarget[lnI])
    IF !EMPTY(lcFile)
      DO lpUpdate
      lcFile = ""
    ENDIF  
  ENDIF  
ENDFOR

*!*************************************************************************
*! Name      : lfAdOptDar
*! Developer : AMH (Ahmed Maher)
*! Date      : 08/19/2002
*! Purpose   : add option to call screen item desc.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to C200392
*:**************************************************************************
*
FUNCTION lfAdOptDar

*--check if the option pad is already defined on the sysmenu
IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
*SET SKIP OF PAD _OPTION OF _MSYSMENU .T.
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Item Description' SKIP FOR laScrMode[1] .OR. EOF(lcTmpBom)
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO gfDoTriger WITH "POSCTSH",PADR("ITEMDESC",10)
RETURN
*-- end of lfAdOptDar.

*!*************************************************************
*! Name      : lfFoundPad
*! Developer : AMH (Ahmed Maher)
*! Date      : 08/19/2002
*! Purpose   : check if any pad menu is exit in _sysmenu
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcPadName ---> Pad NAme 
*!*************************************************************
*! Return      : .T. ----> if exist
*!               .F. ----> if not exist
*!*************************************************************
*! Example     : =lfFoundPad(lcPadName)
*!*************************************************************
*! Due to C200392
*:**************************************************************************
*
FUNCTION lfFoundPad
PARAMETER lcPadName

PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
RETURN(llFOund)
*-- end of lfFoundPad.

*!**************************************************************************
*! Name      : lfItemDesc
*! Developer : (AMH) Ahmed Maher
*! Date      : 08/19/2002
*! Purpose   : Edit the cBomDesc field for Dar12
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Due to C200392
*:**************************************************************************
*
FUNCTION lfItemDesc

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpBom)
SCATTER FIELDS CBOMDESC MEMVAR
DO (gcScrDir+gcWinAppl+'\POITMDSC.SPX')
GATHER FIELDS CBOMDESC MEMVAR
SELECT (lnAlias)
*-- end of lfItemDesc.

*!*************************************************************************
*! Name      : lfSkOptDar
*! Developer : AMH (Ahmed Maher)
*! Date      : 08/19/2002
*! Purpose   : Enable the option pad.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to C200392
*:**************************************************************************
*
FUNCTION lfSkOptDar

SET SKIP OF PAD _OPTION OF _MSYSMENU (.F.)
DEFINE BAR 1 OF _OPTIONPOP PROMPT "\<Costing" SKIP FOR .T.
RETURN
*-- end of lfSkOptDar.

*!*************************************************************************
*! Name      : lfEnbCst
*! Developer : AMH (Ahmed Maher)
*! Date      : 08/19/2002
*! Purpose   : Enable the costing bar.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to C200392
*:**************************************************************************
*
FUNCTION lfEnbCst

DEFINE BAR 1 OF _OPTIONPOP PROMPT "\<Costing" SKIP FOR laScrMode[1]
RETURN
*-- end of lfEnbCst.

*!**************************************************************************
*! Name      : lfGETDIRS
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/05/02
*! Purpose   : Do every thing accordiling to the settinges allowed.m
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200388
FUNCTION lfADDBAR

*--Add 'Auto Search' bar to Option menu.
DEFINE BAR 10 OF _lPopOpt PROMPT "Object Auto Sea\<rch" SKIP FOR (laScrMode[1] OR laScrMode[4] )

*!**************************************************************************
*! Name      : lfOPENOBJ
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/05/02
*! Purpose   : Open objects
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200388
FUNCTION lfOPENOBJ

STORE "" TO lcPath1,lcPath2,lcPath3,lcPath4,lcPath5,lcPOffice
*-- Check the memo file existance.
*B606735,1 TMI [Start] Save office path in a separte file c:\oficpath.mem
*llFoundP = FILE('DARMAIN.MEM')
lcPOffice = ' '
llFoundP = FILE('C:\OFICPATH.MEM')
*B606735,1 TMI [End  ] 
IF llFoundP
  *B606735,1 TMI [Start] Save office path in a separte file c:\oficpath.mem
  *RESTORE FROM DARMAIN.MEM ADDITIVE
  RESTORE FROM C:\OFICPATH.MEM ADDITIVE
  *B606735,1 TMI [End  ] 
ENDIF

*--Open the object
IF EMPTY(lcPOffice)
  IF  gfModalGen('QRM00000B00006',.F.,.F.,.F.,"MS Office path is empty. Do you want to select the correct one?") = 2
    RETURN
  ENDIF
  lcPOffice = GETDIR()
  IF EMPTY(lcPOffice)
    RETURN
  ELSE
    *B606735,1 TMI [Start] Save office path in a separte file c:\oficpath.mem
    *SAVE ALL LIKE lcP* TO DARMAIN.MEM
    SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
    *B606735,1 TMI [End  ] 
  ENDIF
ENDIF
  
IF INLIST(RIGHT(ALLTRIM(objects.cImgPath),4),".DOC",".XLS")
  DO CASE 
    CASE RIGHT(ALLTRIM(objects.cImgPath),4)= ".DOC"
      *--Update setup path
      IF !FILE(lcPOffice+'WINWORD.EXE')
        IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"MS Office path is invalid. Do you want to select the correct one?") = 2
          RETURN
        ENDIF
        lcPOffice = GETDIR()
        IF EMPTY(lcPOffice)
          RETURN
        ELSE
          IF !FILE(lcPOffice+'WINWORD.EXE')
            RETURN
          ENDIF
          *B606735,1 TMI [Start] Save office path in a separte file c:\oficpath.mem
          *SAVE ALL LIKE lcP* TO DARMAIN.MEM
          SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
          *B606735,1 TMI [End  ] 
        ENDIF
      ENDIF
      lcOldPath = SYS(5)+SYS(2003)
      SET DEFA TO (lcPOffice)
      lcFileToOpen = ALLTRIM(objects.cImgPath)
      RUN /N WINWORD.EXE &lcFileToOpen
      SET DEFA TO (lcOldPath)
    CASE RIGHT(ALLTRIM(objects.cImgPath),4)= ".XLS"
      *--Update setup path
      IF !FILE(lcPOffice+'Excel.exe')
        IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"MS Office path is invalid. Do you want to select the correct one?") = 2
          RETURN
        ENDIF
        lcPOffice = GETDIR()
        IF EMPTY(lcPOffice)
          RETURN
        ELSE
          *B606735,1 TMI [Start] here we check for excel , not winword
          *IF !FILE(lcPOffice+'WINWORD.EXE')
          IF !FILE(lcPOffice+'Excel.EXE')          
          *B606735,1 TMI [End  ] 
            RETURN
          ENDIF
          *B606735,1 TMI [Start] Save office path in a separte file c:\oficpath.mem
          *SAVE ALL LIKE lcP* TO DARMAIN.MEM
          SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
          *B606735,1 TMI [End  ] 
        ENDIF
      ENDIF
      lcOldPath = SYS(5)+SYS(2003)
      SET DEFA TO (lcPOffice)
      lcFileToOpen = ALLTRIM(objects.cImgPath)
      RUN /N Excel.exe &lcFileToOpen
      SET DEFA TO (lcOldPath)
  ENDCASE
ENDIF

*!**************************************************************************
*! Name      : lpGetFile
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/05/02
*! Purpose   : Get the previously added files to be the on right side of the mover
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200388
PROCEDURE lpGetFile

PRIVATE lnAlias
lnAlias = SELECT()
SELECT OBJLINK
=SEEK('S'+laData[1],'OBJLINK')
lnNoOfElm = 0
SCAN REST WHILE cobjlnktyp+cobjlink = 'S'+laData[1] FOR !EMPTY(OBJLINK.cobj_desc);
          AND ASCAN(laRpTarget,ALLTRIM(OBJLINK.cobj_desc))  = 0;
          AND ASCAN(laSource,ALLTRIM(OBJLINK.cobj_desc))  <> 0
  lnNoOfElm = lnNoOfElm + 1 
  DIMENSION laRpTarget[lnNoOfElm]
  laRpTarget[lnNoOfElm]  = ALLTRIM(OBJLINK.cobj_desc)
ENDSCAN

SELECT (lnAlias)
*:**************************************************************************
*: Name          : lfLINEDATE
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 12/10/2002
*: Purpose       : add dates by line.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfLINEDATE()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200439,1.
*:**************************************************************************
*: Start C#200439,1.
FUNCTION lfLINEDATE

DEFINE BAR 6 OF _OPTIONPOP PROMPT 'Dates by line'  SKIP FOR (laScrMode[1] OR (lnactfolder<>2) .OR. EOF(lcDetTmp))
ON SELECTION BAR 6 OF _OPTIONPOP DO lpDateLine IN DARMAIN

*-- End Of lfLINEDATE
*:*************************************************************
*: Name      : lpDateLine
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/10/2002
*: Purpose   : Call the date line Screen.
*:*************************************************************
*: Called from : Option menu.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : DO  lpDateLine
*:*************************************************************
*: Due to  C#200439,1.
*:*************************************************************
*

PROCEDURE lpDateLine
PRIVATE lnAlias

lnAlias = SELECT (0)

STORE {} TO ldOldDate , ldExmilDt , ldComplDt
ldExmilDt = Eval(lcDetTmp+'.Cexmill')
ldComplDt = Eval(lcDetTmp+'.DarComple')

lcDatStats = IIF(laScrMode[3] .OR. laScrMode[4],"ENABLE" ,"DISABLE")

PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\MALINEDT.SPX")
POP KEY

SELECT (lcDetTmp)
REPLACE  Cexmill   WITH ldExmilDt,;
         DarComple WITH ldComplDt


SELECT(lnAlias)
*-- End OF lfDateLine
*:**************************************************************************
*: Name          : lfUpdLINED
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 12/10/2002
*: Purpose       : Update the new line withe the 2 custom's date fields.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfUpdLINED()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200439,1.
*:**************************************************************************
*:
FUNCTION lfUpdLINED
Private lnalias , lnDatePos , lDExMill

STORE {} TO lDExMill

lnalias = SELECT (0)
lnDatePos  = 0
lnDatePos  =  aSubScript(laUsrFields,Ascan(laUsrFields,PADR('CEXMILL',10)),1)
IF lnDatePos # 0 .AND. laUsrFields[lnDatePos,1] = PADR('CEXMILL',10)
  lDExMill = IIF(TYPE('laUsrFields[lnDatePos,6]')= 'C',CTOD(laUsrFields[lnDatePos,6]),;
                laUsrFields[lnDatePos,6])
ENDIF


SELECT (lcDetTmp)

*B606888,1 KHM 01/21/2003 (Begin) Replacing the DarComple with laData[10] instead of Eval(lcPOHdr+'.Complete')
*REPLACE  Cexmill   WITH lDExMill ,;
         DarComple WITH Eval(lcPOHdr+'.Complete')
REPLACE  Cexmill   WITH lDExMill ,;
         DarComple WITH laData[10]
*B606888,1 KHM 01/21/2003 (End)

SELECT (lnalias)


*-- End Of lfUpdLINED
*:**************************************************************************
*: Name          : lfREPLEXML
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 12/10/2002
*: Purpose       : Update the Custom Cexmill date for all lines.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : gfModalGen().
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfREPLEXML()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200439,1.
*:**************************************************************************
*:
FUNCTION lfREPLEXML
PARAMETER lcReturn
PRIVATE lnalias , lnDatePos , lDExMill , lnRecNo

lcReturn = .T.

lcVar = SYS(18)
IF TYPE('LAUSRFIELD') = "U"
  RETURN
ENDIF
lnPackPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,'CEXMILL'),1)

IF !(lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnPackPos)) + ",6)")
  RETURN
ENDIF

IF lcKeytype = 'P' 
  lnalias = SELECT (0)
  lnDatePos  = 0
  lnDatePos  =  aSubScript(laOgFxFlt,Ascan(laOgFxFlt,PADR('CEXMILL',10)),1)
  IF lnDatePos # 0 .AND. laOgFxFlt[lnDatePos,1] = PADR('CEXMILL',10)
    lDExMill = laOgFxFlt[lnDatePos,6]
  ENDIF
  
  IF lDExMill == IIF(TYPE('laUsrFields[lnDatePos,6]')= 'C',CTOD(laUsrFields[lnDatePos,6]),;
                laUsrFields[lnDatePos,6])
    _CUROBJ = _CUROBJ + 1
    RETURN 
  ENDIF
  *-- Text Message :- Are you sure you want to apply Ex-Mill date to all lines.
  IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"Are you sure you want to apply Ex-Mill Date to all lines.") = 2
    RETURN
  ENDIF

	

  SELECT (lcDetTmp)
  lnRecNo = RECNO()
  REPLACE  ALL Cexmill  WITH lDExMill
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF

  SELECT(lnalias)
ENDIF
RETURN lcReturn

*--END OF  lfREPLEXML
*:**************************************************************************
*: Name          : lfREPLCOMP
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 12/10/2002
*: Purpose       : Update the Custom Complte date for all lines.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : gfModalGen().
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfREPLCOMP()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#200439,1.
*:**************************************************************************
*:
FUNCTION lfREPLCOMP
PRIVATE lnalias , lnDatePos , lDExMill , lnRecNo

lnalias = SELECT (0)
*-- Text Message :- Are you sure you want to apply Ex-Mill date to all lines.
IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"Are you sure you want to apply Complete Date to all lines.") = 2
  RETURN
ENDIF
SELECT (lcDetTmp)
lnRecNo = RECNO()
REPLACE  ALL DarComple WITH laData[10]

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

SELECT(lnalias)

*--END OF  lfREPLCOMP
*: End C#200439,1.
*:*************************************************************************

*!*************************************************************************
*! Name      : lfAdOptRol
*! Developer : AMH (Ahmed Maher)
*! Date      : 01/27/2003
*! Purpose   : add option to call screen Issue/Return Rolls.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to C200477
*:**************************************************************************
*
FUNCTION lfAdOptRol

*--check if the option pad is already defined on the sysmenu
IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Issue/Return Rolls' SKIP FOR !(laScrMode[2] .AND. lnactfolder=1)
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO gfDoTriger WITH "POCSSH",PADR("ISSRETRL",10)

*C200570,1 AMH Add new option to call the cut figures screen [Start]
DEFINE BAR lnBarNo+1 OF _OPTIONPOP PROMPT 'Docket Cut Figures' SKIP FOR !(laScrMode[2] .OR. laScrMode[1])
ON SELECTION BAR lnBarNo+1 OF _OPTIONPOP DO gfDoTriger WITH "POCSSH",PADR("POCUTOPT",10)
*C200570,1 AMH [End]

RETURN
*-- end of lfAdOptRol.

*!**************************************************************************
*! Name      : lfIssRetRl
*! Developer : (AMH) Ahmed Maher
*! Date      : 01/27/2003
*! Purpose   : Issue/Return By Roll
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Due to C200477
*:**************************************************************************
*
FUNCTION lfIssRetRl

IF EVALUATE(lcTktSheet+'.cCatgTyp') # 'F' .AND. (EVALUATE(lcTktSheet+'.cCatgTyp') # 'T' .OR.;
   !EVALUATE(lcTktSheet+'.Trim_Invt'))
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"This item is not a fabric nor a trim, or it is a trim that is not inventory maintained.")
  RETURN
ENDIF

IF !SEEK(PADR(EVALUATE(lcTktSheet+'.Item'),7)+EVALUATE(lcTktSheet+'.IClr'),'FABRIC') .OR.;
   !FABRIC.LTRKROLLS
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Fabric is not tracked by roll.")
  RETURN
ENDIF

PRIVATE lnAlias,lnReq,lnIssue,lnWareIss,lnBalance,lnOnHand,lnQtyIss,lnQtyRet,lnTotQty
PRIVATE lcFabric,lcColor,lcRollID,lcRRollID,llOpenRoll,lcOldOrder,lcWareCode,lcDyelot,lcRetWare

STORE 0  TO lnReq,lnIssue,lnWareIss,lnBalance,lnOnHand,lnTotQty
STORE '' TO lcRollID,lcRRollID,lcFabric,lcColor,lcWareCode,lcDyelot,;
            lcTmpRolls,lcTmpRoll2,lcTmpRoll3,lcFabDesc
lcWareCode = POSHDR.CMATWARE
lcRetWare  = POSHDR.CMATWARE
lnIssWare  = ASUBSCRIPT(laMatWare,ASCAN(laMatWare,lcWarecode),1)
lnRetWare  = lnIssWare
lcFabric   = PADR(EVALUATE(lcTktSheet+'.ITEM'),7)
lcFabDesc  = FABRIC.DESC
lcColor    = EVALUATE(lcTktSheet+'.ICLR')
lcDyelot   = EVALUATE(lcTktSheet+'.DYELOT')
lnReq      = EVALUATE(lcTktSheet+'.REQ_QTY')
lnIssue    = EVALUATE(lcTktSheet+'.USED_QTY')
lnBalance  = lnReq - lnIssue
=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot,'FABDYE')
lnOnHand   = FABDYE.ONHAND
lcTmpRolls = gfTempName()
lcTmpRoll2 = gfTempName()
lcTmpRoll3 = gfTempName()
lnAlias = SELECT(0)
llOpenRoll = .F.
IF !USED('ROLLS')
  llOpenRoll = gfOpenFile(gcDataDir+'ROLLS',gcDataDir+'ROLLITEM','SH')
ENDIF

SELECT ROLLS
=AFIELDS(laRollStru)

CREATE CURSOR (lcTmpRoll2) FROM ARRAY laRollStru
INDEX ON CROLLID TAG (lcTmpRoll2) OF (lcTmpRoll2)
INDEX ON CRSESSION TAG (lcTmpRoll3) OF (lcTmpRoll2)
SET ORDER TO TAG (lcTmpRoll2)

lnRollStru = ALEN(laRollStru,1) + 1
DECLARE laRollStru[lnRollStru,4]
laRollStru[lnRollStru,1] = 'nRetQty'
laRollStru[lnRollStru,2] = 'N'
laRollStru[lnRollStru,3] = 12
laRollStru[lnRollStru,4] = 3

CREATE CURSOR (lcTmpRolls) FROM ARRAY laRollStru
INDEX ON CROLLID TAG (lcTmpRolls) OF (lcTmpRolls)

SELECT ROLLS
lcOldOrder = SET('ORDER')
SET ORDER TO TAG ROLLITEM
=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot)
SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollID+TranCd+cRSession =;
                lcFabric+lcColor+lcWareCode+lcDyelot FOR CROLLID # '***** N/A *****     '
  SCATTER MEMVAR
  IF TRANCD = '1'
    INSERT INTO (lcTmpRolls) FROM MEMVAR
    lnTotQty = lnTotQty + m.nQtyBal
  ELSE
    IF CTKTNO = laData[1]
      IF SEEK(m.cRollId,lcTmpRoll2)
        SELECT (lcTmpRoll2)
        REPLACE NQTYBAL WITH NQTYBAL + m.nQtyBal,;
                NQTY    WITH NQTY    + m.nQty
      ELSE
        INSERT INTO (lcTmpRoll2) FROM MEMVAR
      ENDIF
      lnWareIss = lnWareIss + m.nQty
    ENDIF
  ENDIF
ENDSCAN

lcSav = gcBmpHome + "SAV.BMP"
DO (gcScrDir+gcWinAppl+'\POISRTRL.SPX')

SELECT ROLLS
SET ORDER TO &lcOldOrder.

IF llOpenRoll .AND. USED('ROLLS')
  USE IN ROLLS
ENDIF
USE IN (lcTmpRolls)
USE IN (lcTmpRoll2)
SELECT (lnAlias)

*B607295,1 AMH Activate the PO cost sheet screen [Start]
ACTIVATE WINDOW (lcWinCh0)
*B607295,1 AMH [End]
*-- end of lfIssRetRl.

*!*************************************************************
*! Name      : lfBrowRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/27/2003
*! Purpose   : Browse Issued/Returned rolls
*!*************************************************************
*! Calls     : lfwRollBrs
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowRoll()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfBrowRoll

SELECT (lcTmpRolls)
BROWSE FIELDS cMarker =IIF(RECNO()=lnDMarker,'>',' '):H='':W=.F.,;
              crollid:H='Roll ID' :R,;
              NQTYBAL:H='Balance' :R ;
              WINDOW POISRTR0   ;
              IN WINDOW POISRTRL;
              NOMENU            ;         
              NOAPPEND          ;
              NODELETE          ;
              NOWAIT            ;
              SAVE              ;
              NOCLEAR           ;
              WHEN lfwRollBrs() ;
              TITLE lcRollTit   ;
              FOR nQtyBal>0
SELECT (lcTmpRoll2)
BROWSE FIELDS cMarker =IIF(RECNO()=lnRMarker,'>',' '):H='':W=.F.,;
              crollid:H='Roll ID':R,;
              NQTYBAL:H='Assigned':R ;
              WINDOW POISRTR1   ;
              IN WINDOW POISRTRL;
              NOMENU            ;         
              NOAPPEND          ;
              NODELETE          ;
              NOWAIT            ;
              SAVE              ;
              NOCLEAR           ;
              WHEN lfwAsRoll()  ;
              TITLE lcAsRollTit ;
              FOR NQTYBAL>0

*!*************************************************************
*! Name      : lfwRollBrs
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/27/2003
*! Purpose   : Show Available rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwRollBrs()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfwRollBrs

lndMarker = RECNO(lcTmpRolls)
SHOW WINDOW (lcRollTit) REFRESH SAME
lnQtyIss = EVALUATE(lcTmpRolls+'.nQtyBal')
lcRollID = EVALUATE(lcTmpRolls+'.crollid')
IF lnTotQty <= 0
  SHOW GET lcRollID DISABLE
  SHOW GET lnQtyIss DISABLE
  SHOW GET pbNewRol DISABLE
ELSE
  SHOW GET lcRollID ENABLE
  SHOW GET lnQtyIss ENABLE
  SHOW GET pbNewRol ENABLE
ENDIF

*!*************************************************************
*! Name      : lfwAsRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/27/2003
*! Purpose   : Show Assigned rolls
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwAsRoll()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfwAsRoll

lnRMarker = RECNO(lcTmpRoll2)
SHOW WINDOW (lcAsRollTit) REFRESH SAME
lnQtyRet  = EVALUATE(lcTmpRoll2+'.nQtyBal')
lcRRollID = EVALUATE(lcTmpRoll2+'.crollid')
IF lnWareIss <= 0
  lcRRollID = ''
  SHOW GET lcRRollID DISABLE
  SHOW GET lnQtyRet  DISABLE
  SHOW GET pbRemRol  DISABLE
  SHOW GET lnRetWare DISABLE
ELSE
  SHOW GET lcRRollID ENABLE
  SHOW GET lnQtyRet  ENABLE
  SHOW GET pbRemRol  ENABLE
  SHOW GET lnRetWare ENABLE
ENDIF

*!*************************************************************
*! Name      : lfAssRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/27/2003
*! Purpose   : Assign Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfAssRoll()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfAssRoll

IF EVALUATE(lcTmpRolls+'.NQTYBAL') < lnQtyIss
  *Message : 38079
  *cannot exceed the total roll quantity for this fabric/Color
  *Button : 00000
  *Ok
  =gfModalGen('TRM38079B00000','ALERT','roll')
  RETURN
ENDIF
SELECT (lcTmpRolls)
lnIssue   = lnIssue + lnQtyIss
lnBalance = lnReq - lnIssue
lnOnHand  = lnOnHand - lnQtyIss
lnTotQty  = lnTotQty - lnQtyIss
lnWareIss = lnWareIss + lnQtyIss

REPLACE Nqtybal WITH Nqtybal - lnQtyIss
SELECT (lcTmpRoll2)
IF !SEEK(lcRollId)
  APPEND BLANK
  REPLACE CROLLID    WITH EVALUATE(lcTmpRolls+'.cRollid'),;
          cRollItem  WITH EVALUATE(lcTmpRolls+'.cRollItem'),;
          Color      WITH EVALUATE(lcTmpRolls+'.Color'),;
          CwareCode  WITH EVALUATE(lcTmpRolls+'.cWareCode'),;
          Dyelot     WITH EVALUATE(lcTmpRolls+'.Dyelot'),;
          Trancd     WITH '2',;
          Ctktno     WITH laData[1],;
          Csession   WITH '',;
          Crsession  WITH EVALUATE(lcTmpRolls+'.cRsession'),;
          Cisession  WITH ''
ENDIF
REPLACE NQTY    WITH NQTY    + lnQtyIss ,;
        Nqtybal WITH Nqtybal + lnQtyIss

SELECT (lcTmpRolls)
LOCATE REST FOR NQTYBAL > 0

=lfRefresh('POISRTR3')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfRetRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/27/2003
*! Purpose   : Return Rolls
*!*************************************************************
*! Calls     : gfModalGen,lfRefresh,lfwRollBrs,lfwAsRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfRetRoll()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfRetRoll

IF lnQtyRet > EVALUATE(lcTmpRoll2+'.NQty')
  *Message : 38079
  *You cannot exceed the total issued quantity for this fabric/Color.
  *Button : 00000
  *Ok
  =gfModalGen('TRM38079B00000','ALERT','issued')
  RETURN
ENDIF

SELECT (lcTmpRolls)
=SEEK(lcRRollID)
IF lcWareCode == lcRetWare
  REPLACE Nqtybal WITH Nqtybal+lnQtyRet
ENDIF

SELECT (lcTmpRoll2)
REPLACE nqtybal WITH nqtybal - lnQtyRet,;
        NQTY    WITH NQTY    - lnQtyRet

IF !EMPTY(CISESSION)
  SET ORDER TO TAG Rolapl IN ROLLS
  IF SEEK(CRSESSION+CISESSION+lcFabric+lcColor+lcWareCode+lcDyelot+CROLLID,'ROLLS')
    SELECT ROLLS
    SET ORDER TO TAG Rollitem
    =SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+EVALUATE(lcTmpRoll2+'.cRollid')+;
          '2'+EVALUATE(lcTmpRoll2+'.cRsession'))
    lnRollQty = 0
    SUM REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRsession =;
                   lcFabric+lcColor+lcWareCode+lcDyelot+EVALUATE(lcTmpRoll2+'.cRollid')+;
                   '2'+EVALUATE(lcTmpRoll2+'.cRsession');
        FOR CTKTNO = laData[1] NQTY TO lnRollQty
    IF lnRollQty > EVALUATE(lcTmpRoll2+'.NQTY')
      SELECT (lcTmpRolls)
      REPLACE NRETQTY WITH NRETQTY+lnRollQty-EVALUATE(lcTmpRoll2+'.NQTY')
    ENDIF
    IF !(lcWareCode == lcRetWare)
      SELECT (lcTmpRolls)
      REPLACE Nqtybal WITH Nqtybal+lnQtyRet-lnRollQty+EVALUATE(lcTmpRoll2+'.NQTY')
    ENDIF
  ENDIF
ELSE
  IF !(lcWareCode == lcRetWare)
    SELECT (lcTmpRolls)
    REPLACE Nqtybal WITH Nqtybal + lnQtyRet
  ENDIF
ENDIF

lnIssue   = lnIssue - lnQtyRet
lnBalance = lnReq - lnIssue
lnOnHand  = lnOnHand + lnQtyRet
lnTotQty  = lnTotQty + lnQtyRet
lnWareIss = lnWareIss - lnQtyRet

SELECT (lcTmpRoll2)
LOCATE REST FOR NQTYBAL > 0

=lfRefresh('POISRTR3')
=lfwRollBrs() .AND. lfwAsRoll()

*!*************************************************************
*! Name      : lfDMainLot
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/28/2003
*! Purpose   : Deactivate main screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDMainLot()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfDMainLot

IF WONTOP()=lcRcvTit   .OR. WONTOP()=lcNewTit    .OR. WONTOP()=lcModTit   .OR. ;
   WONTOP()=lcIssLtTit .OR. WONTOP()=lcDistrbTit .OR. WONTOP()=lcLotTitle .OR. ;
   WONTOP()=lcIssLog   .OR. WONTOP()=lcConCnTit  .OR. WONTOP()= lcRollTit .OR. ;
   WONTOP()=lcAsRollTit
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  IF WONTOP()=lcRollTit .OR. WONTOP()=lcAsRollTit
    ON KEY LABEL ENTER DO lfMainRoll
  ENDIF  
  DO CASE
    CASE WONTOP()=lcRcvTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFRCVLT2','pbSelect'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFRCVLT0',IIF(lcAction='2',IIF(laData[28]='M','rbLotType','puToOpr'),'puLots')
    CASE WONTOP()=lcNewTit 
      *E301235,4 WAB - Trap of tab key. with screen mflot3 in case of material
      *ON KEY LABEL TAB     DO lpTab WITH 'MFLOTS2','pbSelect'
      IF lcTranType = 'T'
        ON KEY LABEL TAB     DO lpTab WITH 'MFLOTS3','pbSelect'
      ELSE
        ON KEY LABEL TAB     DO lpTab WITH 'MFLOTS2','pbSelect'
      ENDIF
      *E301235,4 WAB - END
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFLOTS0','lcContCode'
    CASE WONTOP()=lcModTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFMODLT2','pbAddItem'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFMODLT0','lcContCode'
    CASE WONTOP()=lcIssLtTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFISSLT2','pbISelect'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFISSLT0','puCstTypes'
    CASE WONTOP()=lcDistrbTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFDSTRB1','pbOk'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFDSTRB1','pbOk'
    CASE WONTOP()=lcLotTitle  
      ON KEY LABEL TAB     DO lpTab WITH 'MFOPLOT2','pbIssue'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFOPLOT0','lnIssWare'
    CASE WONTOP()=lcIssLog
      ON KEY LABEL TAB     DO lpTab WITH 'MFISLOG2','pbIssue'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFISLOG2','pbClose'
    CASE WONTOP()=lcConCnTit
      ON KEY LABEL TAB     DO lpTab WITH 'MFCONCN2','pbOK'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFCONCN2','pbCancel'
    CASE WONTOP()=lcRollTit
      ON KEY LABEL TAB     DO lpTab WITH 'POISRTR2','pbNewRol'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'POISRTR3','lnIssWare'
    CASE WONTOP()=lcAsRollTit
      ON KEY LABEL TAB     DO lpTab WITH 'POISRTR2','pbRemRol'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'POISRTR3','lnIssWare'
  ENDCASE
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvRolSav
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/27/2003
*! Purpose   : Validate Roll screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvRolSav()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfvRolSav

PRIVATE lnAlias,lnToIssue,lcRsession,lcIsession,llInsert,lnRollQty,lnRemain,lnRecNo
lnAlias = SELECT(0)

*B607110,1 AMH Open MatInvJl to use when get the correct fabric cost [Start]
PRIVATE llOpenMatj
llOpenMatj = .F.
IF !USED('MATINVJL')
  =gfOpenFile(gcDataDir+'MATINVJL','MATINVJL','SH')
  llOpenMatj = .T.
ENDIF
*B607110,1 AMH [End]

SELECT ROLLS
SET ORDER TO TAG Rolapl
SELECT (lcTmpRoll2)
SET ORDER TO TAG (lcTmpRoll3)
STORE 0 TO lnToIssue
LOCATE
lcRsession = CRSESSION
lcIsession = gfsequence('GLSession')
lnOkToRtrn = IIF((lcWareCode == lcRetWare) .OR. SEEK(lcFabric+lcColor+lcRetWare,'FabDye'),0,1)

SCAN
  llInsert = .F.
  WAIT WINDOW 'Update Roll # : '+ALLTRIM(CROLLID)+'...' NOWAIT
  IF CRSESSION # lcRsession
    IF lnToIssue > 0
      
      *B607110,1 AMH Get the correct fabric cost [Start]
      *=lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                   lcColor,lcWareCode,lcDyelot,lnToIssue,Fabric.CostBuy,'','',gdSysDate,;
                   lcRsession,lcIsession)
      lnCost = lfGetCost(lcFabric,lccolor,lcWareCode,lcDyelot,lcRsession)
      =lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                   lcColor,lcWareCode,lcDyelot,lnToIssue,lnCost,'','',gdSysDate,;
                   lcRsession,lcIsession)
      *B607110,1 AMH [End]
      
    ENDIF
    STORE 0 TO lnToIssue
    lcRsession = CRSESSION
    lcIsession = gfsequence('GLSession')
  ENDIF
  
  SCATTER MEMVAR
  IF EMPTY(CISESSION)
    lnToIssue = lnToIssue + NQTY
    llInsert  = (NQTY>0)
  ELSE
    IF SEEK(CRSESSION+CISESSION+lcFabric+lcColor+lcWareCode+lcDyelot+CROLLID,'ROLLS')
      SELECT ROLLS
      SET ORDER TO TAG Rollitem
      =SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+m.cRollid+'2'+m.cRsession)
      lnRollQty = 0
      SUM REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRsession =;
                     lcFabric+lcColor+lcWareCode+lcDyelot+m.cRollid+'2'+m.cRsession;
          FOR CTKTNO = laData[1] NQTY TO lnRollQty
      lnToIssue  = lnToIssue  + MAX(m.nQty-lnRollQty,0)
      llInsert   = (m.nQty>lnRollQty)
      IF lnRollQty > m.nQty
        IF lnOkToRtrn=1
          lnOkToRtrn = IIF(lfFabInWar(lcFabric,lcColor,lcRetWare),0,2)
        ENDIF
        IF lnOkToRtrn=2
          LOOP
        ENDIF
        =SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+m.cRollid+'2'+m.cRsession)
        lnRemain = lnRollQty - m.nQty
        SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRsession =;
                        lcFabric+lcColor+lcWareCode+lcDyelot+m.cRollid+'2'+m.cRsession;
                    FOR CTKTNO = laData[1]
          IF NQTYBAL <= 0
            LOOP
          ENDIF
          SCATTER MEMVAR
          REPLACE NQTYBAL WITH MAX(NQTYBAL-lnRemain,0)
          lnRemain  = lnRemain - M.NQTYBAL
          lnRecNo   = RECNO()
          m.nQty    = NQTYBAL - NQTY
          m.nQtyBal = 0
          INSERT INTO ROLLS FROM MEMVAR
          GO lnRecNo
          IF lcWareCode == lcRetWare
            
            *B607110,1 AMH Get the correct fabric cost [Start]
            *=lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                         lcColor,lcWareCode,lcDyelot,m.nQty,Fabric.CostBuy,'','',gdSysDate,;
                         m.cRsession,m.cIsession)
            lnCost = lfGetCost(lcFabric,lccolor,lcWareCode,lcDyelot,m.cRsession)
            =lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                         lcColor,lcWareCode,lcDyelot,m.nQty,lnCost,'','',gdSysDate,;
                         m.cRsession,m.cIsession)
            *B607110,1 AMH [End]
            
          ENDIF
          IF lnRemain <= 0
            EXIT
          ENDIF
        ENDSCAN
        IF !(lcWareCode == lcRetWare)
          SELECT (lcTmpRoll2)
          SCATTER MEMVAR
          
          *B607110,1 AMH Get the correct fabric cost [Start]
          lnCost = lfGetCost(lcFabric,lcColor,lcWareCode,lcDyelot,m.cRsession)
          *B607110,1 AMH [End]
          
          m.cRollid   = lfGetNewId(SUBSTR(m.cRollid,1,16))
          lcRetIsesn  = gfsequence('GLSession')
          m.cIsession = ''
          m.cSession  = lcRetIsesn
          m.cRsession = lcRetIsesn
          m.cWareCode = lcRetWare
          m.nQty      = lnRollQty - m.nQty
          m.nQtyBal   = m.nQty
          m.TranCd    = '1'
          
          *B607437,1 AMH Empty the ctktno field [Start]
          m.cTktNo    = ''
          *B607437,1 AMH [End]
          
          INSERT INTO ROLLS FROM MEMVAR
          
          *B607110,1 AMH Get the correct fabric cost [Start]
          *=lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                       lcColor,lcRetWare,lcDyelot,-m.nQty,Fabric.CostBuy,'','',gdSysDate,;
                       lcRetIsesn,'')
          =lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                       lcColor,lcRetWare,lcDyelot,-m.nQty,lnCost,'','',gdSysDate,;
                       lcRetIsesn,'')
          *B607110,1 AMH [End]
          
          IF SEEK(lcFabric+lcColor,'FABRIC')
            REPLACE FABRIC.NMATWIP WITH FABRIC.NMATWIP - m.nQty
          ENDIF
          IF SEEK(lcFabric+lcColor+lcRetWare,'FABDYE')
            REPLACE FABDYE.USAGE   WITH FABDYE.USAGE   + m.nQty
          ENDIF
          IF SEEK(lcFabric+lcColor+lcWareCode,'FABDYE')
            REPLACE FABDYE.USAGE   WITH FABDYE.USAGE   - m.nQty,;
                    FABDYE.NMATWIP WITH FABDYE.NMATWIP - m.nQty
          ENDIF
        ENDIF
      ENDIF
      SELECT ROLLS
      SET ORDER TO TAG Rolapl
      SELECT (lcTmpRoll2)
      m.nQty     = MAX(NQTY-lnRollQty,0)
      m.nQtyBal  = MAX(NQTYBAL-lnRollQty,0)
    ELSE
      lnToIssue = lnToIssue + NQTY
      llInsert  = (NQTY>0)
    ENDIF
  ENDIF
  IF llInsert
    m.cIsession = lcIsession
    m.cSession  = lcIsession
    INSERT INTO ROLLS FROM MEMVAR
    SELECT (lcTmpRoll2)
    IF EMPTY(CISESSION)
      REPLACE CISESSION WITH lcIsession,;
              CSESSION  WITH lcIsession
    ENDIF
  ENDIF
ENDSCAN
IF CRSESSION # lcRsession
  IF lnToIssue > 0
    
    *B607110,1 AMH Get the correct fabric cost [Start]
    *=lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                 lcColor,lcWareCode,lcDyelot,lnToIssue,Fabric.CostBuy,'','',gdSysDate,;
                 lcRsession,lcIsession)
    lnCost = lfGetCost(lcFabric,lccolor,lcWareCode,lcDyelot,lcRsession)
    =lfIssRetFab(EVALUATE(lcTktSheet+'.Typ'),EVALUATE(lcTktSheet+'.cCatgTyp'),lcFabric,;
                 lcColor,lcWareCode,lcDyelot,lnToIssue,lnCost,'','',gdSysDate,;
                 lcRsession,lcIsession)
    *B607110,1 AMH [End]
    
  ENDIF
ENDIF

*B607110,1 AMH Close MatInvJl [Start]
IF llOpenMatj .AND. USED('MATINVJL')
  USE IN MATINVJL
ENDIF
*B607110,1 AMH [End]

SELECT ROLLS
SET ORDER TO TAG Rollitem
SELECT (lcTmpRolls)
SCAN
  WAIT WINDOW 'Update Roll # : '+ALLTRIM(CROLLID)+'...' NOWAIT
  IF SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+crollid+trancd+crsession,'ROLLS') .AND.;
     NQTYBAL # ROLLS.NQTYBAL
    REPLACE ROLLS.NQTYBAL WITH NQTYBAL
  ENDIF
ENDSCAN

SELECT (lcTmpRolls)
LOCATE
SELECT (lcTmpRoll2)
SET ORDER TO TAG (lcTmpRoll2)
LOCATE
WAIT CLEAR
=gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Updating Completed.")
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfMainRoll
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/28/2003
*! Purpose   : Assign/Return Rolls
*!*************************************************************
*! Calls     : lfAssRoll,lfRetRoll
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfMainRoll()
*!*************************************************************
*! Due to C200477
*:*************************************************************
PROCEDURE lfMainRoll

IF WONTOP() = lcRollTit
  =lfAssRoll()
ELSE
  =lfRetRoll()
ENDIF

*!*************************************************************
*! Name      : lfIssRetFab
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/28/2003
*! Purpose   : Issue/Return Fabrics and Inventory mantained trims
*!*************************************************************
*! Calls     : gfModalGen,GLDIST
*!*************************************************************
*! Parameters: lcItemType   : Cost Item Type
*!             lcCatgType   : Cost Item Category 
*!             lcFabric     : Fabric
*!             lcColor      : Color
*!             lcWareCode   : Warehouse
*!             lcDyelot     : Dyelot
*!             lnIssued     : Issued/Return Quantity
*!             lnBuyIssCost : Issued/Return unit cost buy
*!             lcOprCode    : Operation
*!             lcLotNo      : Lot#
*!             ldIssDate    : Issue/Return Date
*!             lcRSession   : Receiving Session#
*!             lcISession   : Issue Session#
*!             llFromDel    : Called upon Deleting The Cost Sheet,
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfIssRetFab()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfIssRetFab


PARAMETERS lcItemType,lcCatgType,lcFabric,lcColor,lcWareCode,lcDyelot,;
           lnIssued,lnBuyIssCost,lcOprCode,lcLotNo,ldIssDate,lcRSession,;
           lcISession,llFromDel,llUseActCost

PRIVATE lcFabLinkCode,lnAmount,lnOldStock,lnOldCost,lcRollID,lnAlias

IF SEEK(lcFabric+lcColor,'Fabric') .AND. lnIssued <> 0
*--- DOC.
*--- Check if this fabric/color are assigned to the selected
*--- warehouse
*--- DOC.
  IF laSetups[2,2]='Y' .AND. !EMPTY(lcWareCode) .AND. ;
    !SEEK (lcFabric+lcColor+lcWareCode+SPACE(10),'FABDYE')
    *Message : 38029
    *Item/Color xxxxx/xxxx is not assigned to warehouse xxxx
    *Button : 38001
    *Add Cancel
    IF gfModalGen('QRM38029B38001','ALERT','Fabric/Color: '+ALLTRIM(lcFabric)+'/'+ALLTRIM(lcColor)+'|'+lcWareCode) = 1
      DO gpAdFabWar WITH lcFabric,lcColor,SPACE(10),lcWareCode
    ELSE
      RETURN
    ENDIF
  ENDIF
  IF laSetups[12,2]='Y' .AND. Fabric.cDye_Flg='Y' AND ;
    !EMPTY(lcDyelot) .AND. !SEEK(lcFabric+lcColor+lcWareCode+lcDyelot,'FabDye') AND;
    !gfSelDyelot(lcTranType,'F','','',lcWareCode,lcDyelot,lcFabric,lcColor)
    RETURN
  ENDIF

  lnAlias = SELECT()
*--- DOC.
*--- Get the Gl Link code such that is the fabric link code is empty then
*--- update it with DEFDEF
*--- DOC.
  lcFabLinkCode = IIF(EMPTY(Fabric.Link_Code),'DEFDEF',Fabric.Link_Code)
  
  IF SEEK(lcFabric+lcColor+lcWareCode+SPACE(10),'FabDye')
    lcFabLinkCode = IIF(EMPTY(FabDye.Gl_Link),lcFabLinkCode,FabDye.Gl_Link)
  ENDIF
  IF laSetups[1,2]='Y'
*--- DOC.
*--- Update the array that used by material control functoin
*--- DOC.
    DECLARE laGLDistAr[2,13]
    laGLDistAr[1,1] = lcFabLinkCode
    laGLDistAr[2,1] = laData[29]
    laGLDistAr[1,2] = '015'
    laGLDistAr[2,2] = '013'
    laGLDistAr[1,3] = 1
    laGLDistAr[2,3] = -1
    STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
    STORE laData[1]  TO laGLDistAr[1,5],laGLDistAr[2,5]
    STORE ldIssDate  TO laGLDistAr[1,6],laGLDistAr[2,6]
    STORE lcGlYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
    STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
    STORE lcGlDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
    STORE ''         TO laGLDistAr[1,10],laGLDistAr[2,10]
  ELSE
    DIME laGLDistAr[1,1]
    laGLDistAr = ''
  ENDIF
  PRIVATE laOtherPar
  DIMENSION laOtherPar[3,2]
*--- DOC.
*--- List of all variable to be used as parameter in the material control
*--- DOC.
  laOtherPar[1,1] = 'llUseACst'
  laOtherPar[1,2] = llUseActCost
  laOtherPar[2,1] = 'lcOprCode'
  laOtherPar[2,2] = lcOprCode
  laOtherPar[3,1] = 'lcLotNo'
  laOtherPar[3,2] = lcLotNo
  IF lcTranType $ 'MID' .AND. lnIssued < 0
    =lfUpdWip()
  ENDIF
  
  =gfMatCrl('4',lcFabric,lcColor,lcWareCode,lcDyelot,ldIssDate,ldIssDate,;
            laData[1],-1*lnIssued,lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv),;
            '','',0,'','',@laGLDistAr,'',lcTranType,laData[1],lcRSession,lcISession,;
            '','','',@laOtherPar)

  IF lcTranType $ 'MID' .AND. lnIssued < 0
    =lfRetWip()
  ENDIF
  
  IF !llFromDel 
    SELECT (lcTktSheet)
    IF SEEK(lcItemType+'1'+PADR(lcFabric,19)+lcColor+SPACE(6)+lcDyelot)
      REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
              Issue_Qty WITH Issue_Qty + MAX(lnIssued,0) ,;
              Dyelot    WITH lcDyelot
    ENDIF
    SELECT CTktBom
    IF SEEK(lcTranType+laData[1]+lcItemType+PADR(lcFabric,19)+lcColor+SPACE(6)+lcDyelot)
      REPLACE Used_Qty  WITH Used_Qty  + lnIssued ,;
              Issue_Qty WITH Issue_Qty + MAX(lnIssued,0) ,;
              Dyelot    WITH lcDyelot              
    ENDIF
    SELECT BomCost
    SET ORDER TO TAG Bomcstkt
    SET DELETE OFF
    IF !SEEK(lcItemType+lcTranType+laData[1]+PADR(lcFabric,19)+lcColor+;
             SPACE(6)+lcWareCode+lcDyelot+lcRSession+lcISession)
      APPEND BLANK
    ELSE
      IF DELETED()
        BLANK
        RECALL
      ENDIF
    ENDIF
    SET DELETE ON

    REPLACE cTktNo    WITH laData[1]  ,;
            cWareCode WITH lcWareCode ,;
            cDyelot   WITH lcDyelot   ,;
            Item      WITH lcFabric   ,;
            IClr      WITH lcColor    ,;
            cBomType  WITH lcItemType ,;
            cIMTyp    WITH lcTranType ,;
            MfgCode   WITH SPACE(6)   ,;
            nTotQty   WITH nTotQty+lnIssued,;
            nTotCst   WITH nTotCst +lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv) ,;
            dTranDate WITH ldIssDate  ,;
            cRSession WITH lcRSession ,;
            cISession WITH lcISession ,;
            cCostType WITH lcCatgType ,;
            nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
            nUnitACst WITH nUnitCst  ,;
            nTotACst  WITH nTotCst   ,;
            cOprCode  WITH lcOprCode ,;
            cLotNo    WITH lcLotNo   ,;
            Actualize WITH 'N'
    
    IF nTOtQty = 0
      DELETE
    ENDIF
    laData[22+VAL(lcItemType)-1] = laData[22+VAL(lcItemType)-1] + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
    DO CASE
      CASE lcTranType = 'M'
        SELECT CUTTKTH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
        UNLOCK
      CASE lcTranType $ 'ID'
            
        SELECT POSHDR
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
        
         DO CASE
           CASE lcCatgType = 'P'
              REPLACE nfActCost&lcItemType WITH (nAct_Cost&lcItemType &lcPExSign POSHDR.nPriceRat &lcPUntSin POSHDR.nCurrUnit)
            CASE !INLIST(lcCatgType ,'S','F','T')
              REPLACE nfActCost&lcItemType WITH (nAct_Cost&lcItemType &lcDExSign POSHDR.nDutyRat &lcDUntSin POSHDR.nDCurUnit)
            OTHERWISE
              REPLACE nfActCost&lcItemType WITH nAct_Cost&lcItemType
          ENDCASE                   
        
        UNLOCK
      CASE lcTranType = 'T'
        SELECT MMFGORDH
        =RLOCK()
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType + lnIssued*lnBuyIssCost/IIF(Fabric.Conv=0,1,Fabric.Conv)
        UNLOCK
    ENDCASE
  ENDIF
  IF laSetups[1,2] = 'Y'
    SELECT (lcGlDTemp)
    REPLACE ALL GlSession WITH lcSession
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGlDTemp)
    SELECT (lcGlDTemp)
    ZAP
  ENDIF
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfvIssWare
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/02/2003
*! Purpose   : Validate the warehouse field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvIssWare()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfvIssWare

lcWareCode = laMatWare[lnIssWare,2]
lnRetWare  = lnIssWare
lcRetWare  = lcWareCode

PRIVATE lnAlias,lcOldOrder
STORE 0  TO lnWareIss,lnOnHand,lnTotQty
STORE '' TO lcRollID,lcRRollID
lnIssue    = EVALUATE(lcTktSheet+'.USED_QTY')
lnBalance  = lnReq - lnIssue

=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot,'FABDYE')
lnOnHand   = FABDYE.ONHAND

lnAlias = SELECT(0)
SELECT (lcTmpRolls)
ZAP
SELECT (lcTmpRoll2)
ZAP
SELECT ROLLS
lcOldOrder = SET('ORDER')
SET ORDER TO TAG ROLLITEM
=SEEK(lcFabric+lcColor+lcWareCode+lcDyelot)
SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollID+TranCd+cRSession =;
                lcFabric+lcColor+lcWareCode+lcDyelot FOR CROLLID # '***** N/A *****     '
  SCATTER MEMVAR
  IF TRANCD = '1'
    INSERT INTO (lcTmpRolls) FROM MEMVAR
    lnTotQty = lnTotQty + m.nQtyBal
  ELSE
    IF CTKTNO = laData[1]
      IF SEEK(m.cRollId,lcTmpRoll2)
        SELECT (lcTmpRoll2)
        REPLACE NQTYBAL WITH NQTYBAL + m.nQtyBal,;
                NQTY    WITH NQTY    + m.nQty
      ELSE
        INSERT INTO (lcTmpRoll2) FROM MEMVAR
      ENDIF
      lnWareIss = lnWareIss + m.nQty
    ENDIF
  ENDIF
ENDSCAN

=lfBrowRoll()
=lfRefresh('POISRTR3')
=lfwRollBrs() .AND. lfwAsRoll()
SHOW GET lnRetWare

SELECT ROLLS
SET ORDER TO &lcOldOrder.
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvRetWare
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/02/2003
*! Purpose   : Validate the Return warehouse field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRetWare()
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfvRetWare

PRIVATE llBefore,llAfter,lnAlias
llBefore  = (lcWareCode==lcRetWare)
lcRetWare = laMatWare[lnRetWare,2]
llAfter   = (lcWareCode==lcRetWare)
IF llAfter # llBefore
  lnAlias = SELECT(0)
  SELECT (lcTmpRolls)
  SCAN
    REPLACE NQTYBAL WITH NQTYBAL + (NRETQTY * SIGN(IIF(llAfter,1,-1)))
  ENDSCAN
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfGetNewId
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/02/2003
*! Purpose   : Create new roll id to return into it for another warehouse
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: Roll id
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetNewId(m.cRollid)
*!*************************************************************
*! Due to C200477
*:*************************************************************
FUNCTION lfGetNewId
PARAMETERS lcOldRoll

PRIVATE lnCount,lcNewRoll
SET ORDER TO TAG ROLLS IN ROLLS
lcNewRoll = lcOldroll + SUBSTR(laData[1],3)
IF SEEK(lcNewRoll,'ROLLS')
  lnCount = 0
  lcNewRoll = lcOldRoll + STRTRAN(STR(lnCount,4,0),' ','0')
  DO WHILE SEEK(lcNewRoll,'ROLLS')
    lnCount   = lnCount + 1
    lcNewRoll = lcOldRoll + STRTRAN(STR(lnCount,4,0),' ','0')
  ENDDO
ENDIF
SET ORDER TO TAG ROLLITEM IN ROLLS
RETURN lcNewRoll

*!*************************************************************
*! Name      : lfFabInWar
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/03/2003
*! Purpose   : check if the fabric/color is assigned to the warehouse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfFabInWar
PARAMETER lcfFabric,lcfColor,lcfWare

IF !SEEK(lcfFabric+lcfColor+lcfWare,'FabDye')
  lnChoice=gfModalGen('TRM36049B36001','ALERT',ALLTRIM(lcfFabric)+'/'+ALLTRIM(lcfColor)+'|'+ALLTRIM(lcfWare)) 
  IF lnChoice = 1
    DO gpAdFabWar WITH lcfFabric, lcfColor,SPACE(10), lcfWare
  ELSE
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*!*************************************************************************
*! Name      : lfDlOptRol
*! Developer : AMH (Ahmed Maher)
*! Date      : 02/09/2003
*! Purpose   : Delete option to call screen Issue/Return Rolls.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to C200477
*:**************************************************************************
*
FUNCTION lfDlOptRol

*B607295,1 AMH release the pad only [Start]
*lnBarNo = CNTBAR('_OPTIONPOP')
*RELEASE BAR lnBarNo OF _OPTIONPOP

*C200570,1 AMH Remove the new option [Start]
*RELEASE BAR lnBarNo-1 OF _OPTIONPOP

*IF lnBarNo = 1
*IF lnBarNo = 2
*C200570,1 AMH [End]
*  RELEASE POPUP _OPTIONPOP
  RELEASE PAD _OPTION OF _MSYSMENU
*ENDIF
*B607295,1 AMH [End]
*-- end of lfDlOptRol.

*!*************************************************************************
*! Name      : lfAdMABrOp
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/10/2003
*! Purpose   : Option to call Rolls Entry screen.
*!*************************************************************************
*! Due to C200472,1 KHM
*:**************************************************************************
FUNCTION lfAdMABrOp

lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Rolls Entry' SKIP FOR !(llTrkRolls AND Fabric.lTrkRolls AND lcTrancd = '2')
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO (gcAppHome+gcWinAppl+'\MARLTDB.PRG')

*!*************************************************************************
*! Name      : lfGetCost
*! Developer : AMH (Ahmed Maher)
*! Date      : 05/04/2003
*! Purpose   : Get the correct cost of fabric.
*!*************************************************************************
*! Returns   : None.
*!*************************************************************************
*! Due to B607110
*:**************************************************************************
*
FUNCTION lfGetCost
PARAMETER lcFabric,lcColor,lcWareCode,lcDyelot,lcRsession

PRIVATE lnAlias,lnRet,lcOrder
lnAlias = SELECT(0)
lnRet = 0
SELECT MATINVJL
lcOrder = SET('ORDER')
SET ORDER TO TAG MATINVJL
IF SEEK(lcFabric+lcColor+lcWareCode+lcDyelot+lcRsession)
  lnRet = nUnitCost
ENDIF
SET ORDER TO &lcOrder.
SELECT (lnAlias)
RETURN lnRet
*-- end of lfGetCost.

*!**************************************************************************
*! Name      : lfPoCutOpt
*! Developer : (AMH) Ahmed Maher
*! Date      : 08/11/2003
*! Purpose   : Call the Docket Cut Figures screen.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Due to C200570
*:**************************************************************************
*
FUNCTION lfPoCutOpt

DO gpDoProg WITH "AWRPOCUT", .F., "PO", ""
*-- end of lfPoCutOpt.

*!**************************************************************************
*! Name      : lfGetRet
*! Developer : (AMH) Ahmed Maher
*! Date      : 08/14/2003
*! Purpose   : Get the correct return quantity from the fabric/color when delete PO cost sheet.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Due to B607465
*:**************************************************************************
*
FUNCTION lfGetRet

SUM REST nIssued-nReceived TO lnIssued ;
    WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode+cDyelot+cRSession+cISession=;
          laData[1]+m.cOprCode+m.cLotNo+'4'+m.cFabric+m.cColor
*-- end of lfGetRet.
*:**************************************************************************
*: Name          : lfEnablWar
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 12/09/2003
*: Purpose       : Enable the WareHouse Field popup to issue for new wearehuse.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfEnablWar()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037268,1.
*:**************************************************************************
*: Start C#037268
FUNCTION lfEnablWar

lcWareStat  = 'ENABLE'
SET FILTER TO

*-- End Of lfEnablWar
*:**************************************************************************
*: Name          : lfCalcRetn
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 12/09/2003
*: Purpose       : Calculate the return if he return all the Qty or Not.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfCalcRetn()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037268,1.
*:**************************************************************************
*: 
FUNCTION lfCalcRetn
PRIVATE lnOld_Alis , lnoldRECNO , lcOldOrder , llShowIssue
llShowIssue = .F.

IF !(m.Typ = '3')
  RETURN llShowIssue
ENDIF
lnOld_Alis = SELECT(0)
lnoldRECNO = RECNO()

SELECT BOMCOST
lcOldOrder = ORDER()
SET ORDER TO TAG Bomcstkt
*-- Check first if the recored is exist or not.
IF SEEK(m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode)
  SUM ntotQty while cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession = ;
  m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode FOR crsession+cisession = M.crsession+M.cisession TO lnSumTotIs
  
  IF lnSumTotIs <= 0
    llShowIssue = .T.
  ENDIF
ENDIF

IF !llShowIssue
  SET FILTER TO ntotQty > 0
ENDIF

SELECT(lnOld_Alis)
SET ORDER TO &lcOldOrder
IF BETWEEN(lnoldRECNO,1,RECCOUNT())
  GOTO lnoldRECNO
ENDIF

*-- cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode
=SEEK(m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode+m.cwarecode)
LOCATE REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession = ;
m.Typ+lcTranType+laData[1]+m.Item+m.IClr+m.MfgCode FOR crsession+cisession = M.crsession+M.cisession


RETURN llShowIssue

*-- End Of lfCalcRetn
*:**************************************************************************
*: Name          : lfGetrtQty
*: Developer     : Abdou Elgendy. [ABD]
*: Date          : 12/09/2003
*: Purpose       : Calculate the return for the all warehouse.
*:**************************************************************************
*: Called from   : Triger Program.
*:**************************************************************************
*: Calls         : None
*:**************************************************************************
*: Return        : None
*:**************************************************************************
*: Example       : = lfGetrtQty()
*:**************************************************************************
*: Passed Parameters : None.
*:**************************************************************************
*: Due to  C#037268,1.
*:**************************************************************************
*: 
FUNCTION lfGetrtQty
PRIVATE lnOld_Alias , lnOldRecNo

IF EOF()
  RETURN
ENDIF

lnOld_Alias = SELECT(0)

=SEEK(m.cFabric+m.cColor,'Fabric')
*-- Go to top
lnOldRecNo = RECNO()
LOCATE
LOCATE FOR ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode+cDyelot+cRSession+cISession=;
laData[1]+m.cOprCode+m.cLotNo+'4'+m.cFabric+m.cColor

SUM REST nIssued-nReceived TO lnIssued ;
WHILE ctran+coprcode+clotno+ctrantype+cfabric+ccolor+cwarecode+cDyelot+cRSession+cISession=;
laData[1]+m.cOprCode+m.cLotNo+'4'+m.cFabric+m.cColor; 
FOR cDyelot+cRSession = m.cDyelot+m.cRSession

IF BETWEEN (lnOldRecNo,1,RECCOUNT()) 
  GOTO lnOldRecNo
ENDIF
SELECT(lnOld_Alias)


*-- End OF lfGetrtQty.
*: End C#037268
*:*************************************************************