*:************************************************************************
*: Program file  : SoExpSo.PRG
*: System        : Aria Apparel System
*: Module        : SO2
*: Date          : 11/11/2002
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Customized for: C#200414 
*: Porpuse       : Export sales orders to Access (*.mdb) files
***************************************************************************
DECLARE laSeasons[1,2], laSetups[2,2], laDivision[1,2], laCodes[2,10], laRpSource[1,2] ,;
        laRpSTrgt[1], laRpSOld[1], laRpDTrgt[1], laRpDOld[1] ,laSlctdOrd[1]

STORE ""  TO laRpSTrgt[1] , laRpSOld[1],laRpDOld[1],laRpDTrgt[1]
STORE '' TO lcFileName, lcDivStr, lcSesStr, lcTmpOrd
STORE .T. TO llFrstSes,llFrstDiv

STORE 1   TO lnSeason, lnDivision
*B606863,1 TMI [Start] Define the variable "lnAppOvr" that determine if to append to an existing mdb file or overwrite it
PRIVATE lnAppOvr
lnAppOvr = 2  &&Overwrite case
*B606863,1 TMI [End  ] 

lcSysDir = ' '
lcTmpOrd = gfTempName()
llRturnUsr = .F.

IF !gfSetup()
  RETURN
ENDIF  

laSetups[1,1] = 'M_DIV_LINK'       && Link code at division level
laSetups[2,1] = 'M_SEASNCHK'       && Check for Season Validation on every style entered on Order Line.

laCodes[1,1] = 'SEASON'
laCodes[1,2] = 'laSeasons'
laCodes[1,3] = 'lnSeason'
laCodes[1,4] = ''
laCodes[1,5] = .T.
laCodes[1,6] = .F.
laCodes[1,10] = 'SEASON'

laCodes[2,1] = 'CDIVISION'
laCodes[2,2] = 'laDivision'
laCodes[2,3] = 'lnDivision'
laCodes[2,4] = ''
laCodes[2,5] = .F.
laCodes[2,6] = .F.
laCodes[2,10] = 'cDivision'

=lfOpenFile()

SELECT ORDHDR
IF TYPE('SENDORD')='U' OR TYPE('HVNR')='U'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please update file structure for ORDHDR file')
  RETURN
ENDIF
IF !SEEK('HVNR      ','SEQUENCE')
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Please Open the sequence screen , Edit then Save to Initialize the new sequence field HVNR.")
  RETURN
ENDIF


=gfGetMemVar(@laSetups,gcAct_Comp)

ON KEY LABEL ESC DO lfClsScr
DO (gcScrDir+gcWinAppl+"\SoExpac.SPX")

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/11/2002
*! Purpose   : Open Files.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfOpenFile()
*!*************************************************************
FUNCTION lfOpenFile

*-- Open files
=gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
=gfOpenFile(gcDataDir+'ORDLINE','Ordlinst','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','Customer','SH')
=gfOpenFile(gcDataDir+'CODES','Ccode_no','SH')
=gfOpenFile(gcDataDir+'STYLE','Style','SH')
=gfOpenFile(gcDataDir+'SCALE','scale','SH')
=gfOpenFile(gcDataDir+'SEQUENCE','CSEQ_TYPE','SH')

*-- Set relations
SELECT ORDLINE
SET RELATION TO ORDLINE.style INTO STYLE ADDITIVE
LOCATE
SELECT STYLE
SET RELATION TO 'S'+ STYLE.scale INTO SCALE ADDITIVE
LOCATE

*!*************************************************************
*! Name      : lfvSysDir
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/11/2002
*! Purpose   : Valid function of System directory.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvSysDir()
*!*************************************************************
FUNCTION lfvSysDir

lcSysDir = GETDIR("","Select SysDir")
IF EMPTY(lcSysDir)
  lcSysDir = gcWorkDir
ELSE
  IF SUBSTR(lcSysDir,LEN(lcSysDir)) <> '\'
    lcSysDir = lcSysDir + '\'
  ENDIF
ENDIF

SHOW GET lcFileName ENABLE
SHOW GET lcSysDir DISABLE
_CUROBJ = _CUROBJ
_CUROBJ = OBJNUM(lcFileName)

*!*************************************************************
*! Name      : lfCrtFile
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/11/2002
*! Purpose   : Create text file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfCrtFile()
*!*************************************************************

FUNCTION lfCrtFile
lcFileName = ALLTRIM(lcFileName)
lnFileHNDL = FCREATE(lcSysDir+lcFileName+'.MDB')
IF lnFileHNDL>0
  =FClose(lnFileHNDL)
  ERASE (lcSysDir+lcFileName+'.MDB')
ENDIF
RETURN lnFileHNDL

*!*************************************************************
*! Name      : lfvOk
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/11/2002
*! Purpose   : Valid function to export SO to text file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvOk()
*!*************************************************************
FUNCTION lfvOk

IF EMPTY(lcSysDir)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please select the Sys Directory.')
  _CUROBJ = OBJNUM(pbSysDir)  
  RETURN
ENDIF

IF EMPTY(lcFileName)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please enter file name.')
  _CUROBJ = OBJNUM(lcFileName)  
  RETURN
ENDIF

*B606863,1 TMI [Start] Reset lnAppOvr to 2
lnAppOvr = 2
*B606863,1 TMI [End  ] 
IF FILE(lcSysDir+lcFileName+'.MDB')
  *B606863,1 TMI [Start] Change the message buttons to allow append or overwrite to an existing file
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'The file ' + UPPER(ALLT(lcFileName)) + '.MDB already exists.')
  lnAppOvr = gfModalGen('INM00000B32015',.F.,.F.,.F.,'The file ' + UPPER(ALLT(lcFileName)) + '.MDB already exists.')
  IF lnAppOvr = 3
    *B606863,1 TMI [End  ] 
    _CUROBJ = OBJNUM(lcFileName)    
    RETURN
  *B606863,1 TMI [Start] 
  ENDIF
  *B606863,1 TMI [End  ] 
ENDIF

*B606863,1 TMI [Start] No need for this function
*IF lfCrtFile() < 0
*  *-- Can not open or create output file.
*  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Cannot open or create an output file.')
*  _CUROBJ = OBJNUM(lcFileName)  
*  RETURN
*ELSE
*B606863,1 TMI [End  ] 

=lfColData()

*ENDIF && B606863,1 Commented by tmi

*!*************************************************************
*! Name      : lfColData
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 11/11/2002
*! Purpose   : Collection for the data.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfColData()
*!*************************************************************
FUNCTION lfColData
*-- Refresh the variables hold the division and the season
STORE '' TO lcDivStr  , lcSesStr

FOR lnCount = 1 TO ALEN(laRpDOld,1)
  IF !EMPTY(laRpDOld[lnCount])
    lcDivStr = lcDivStr + SUBSTR(laRpDOld[lnCount],1,6)+','
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(laRpSOld,1)
  IF !EMPTY(laRpSOld[lnCount])
    lcSesStr = lcSesStr + SUBSTR(laRpSOld[lnCount],1,6)+','
  ENDIF
ENDFOR

CREATE CURSOR (lcTmpOrd) (Order C(6))
ZAP
INDEX ON Order TAG (lcTmpOrd) OF (gcWorkDir+lcTmpOrd+'.CDX')

lcBrowFlds = [ORDER     :H = 'ORDER#'       :15 ,]  + ;
      	     [STATUS    :H = 'S  '          :5  ,]  + ;
      	     [ACCOUNT   :H = 'Account'      :15 ,]  + ;
             [SEASON    :H = 'Season'       :25 ,]  + ;
             [CDIVISION :H = 'Division'     :25  ]

SELECT ORDHDR
LOCATE
lcFltOrd = IIF(!EMPTY(lcDivStr) , "ORDHDR.Cdivision $ lcDivStr" ,".T.")     +;
           IIF(!EMPTY(lcSesStr) ,' AND ' + "ORDHDR.Season $ lcSesStr" , "") +;
           ' AND IIF(SUBSTR(lcVendor,1,1)="Y",ORDHDR.SENDORD = "Y",ORDHDR.SENDORD<>"Y")' +;
           ' AND !ORDHDR.STATUS $ "CX"'

LOCATE FOR CORDTYPE+ORDER = 'O' .AND. EVAL(lcFltOrd)
IF EOF('ORDHDR')
  PUSH KEY CLEAR
  *-- There are no records to display.
  =gfModalGen('TRM00052B00000','DIALOG')
  _CUROBJ = _CUROBJ
  POP KEY
  RETURN
ELSE
  PUSH KEY CLEAR  
  *-- Browse the filtered orders when saving.
  WAIT WINDOW NOWAIT 'Collecting Data ,Please wait...'
  llRturnUsr =gfRange(lcBrowFlds,lcTmpOrd,"Order",('"O" FOR '+lcFltOrd),"","","@! XXXXXX")  
  *-- Take a copy of the temp cursor to use it in VFP when opening view files
  SELECT &lcTmpOrd  
  GO TOP
  *--Fill the array laSlctdOrd
  DIME laSlctdOrd[1]
  laSlctdOrd[1]=.F.
  FOR lnI = 1 TO RECCOUNT(lcTmpOrd)
    DIME laSlctdOrd[lnI]
    laSlctdOrd[lnI] = &lcTmpOrd..ORDER
    SKIP
  ENDFOR
  USE IN &lcTmpOrd
  
  POP KEY
  IF !llRturnUsr
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
ENDIF

*--Save all variables to memo file
lcTmpName = gfTempName()
SAVE TO (gcWorkDir+lcTmpName+'.MEM')

lcCommLine = (gcWorkDir+lcTmpName)
lcLib=SYS(2004)+"foxtools.fll"

IF FILE(lcLib)
  *--Now run the VFP exe program that connects to SQL SERVER  and collects data
  WAIT WINDOW NOWAIT 'Collecting Data..'
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",gcAppHome+'SO\'+[SOEXPACV.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
   llPoDisp = .F.
   RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT WINDOW "LIBRARY NOT FOUND" 
ENDIF
WAIT CLEAR
STORE SPACE(0) TO lcSysDir , Lcfilename
SHOW GET lcSysDir   DISABLE
SHOW GET lcFileName DISABLE


*!*************************************************************
*! Name       : lfClsScr
*: Developer  : TMI - TAREK MOHAMED IBRAHIM ELQUSI
*: Date       : 03/26/2002
*! Purpose    : Valid function of CANCEL bottom.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfClsScr()
*!*************************************************************

FUNCTION lfClsScr

IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF  
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF  
IF USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF  
IF USED('CODES')
  USE IN CODES
ENDIF  
IF USED('STYLE')
  USE IN STYLE
ENDIF  
IF USED('SCALE')
  USE IN SCALE
ENDIF  
CLEAR READ

*!*************************************************************
*! Name       : lfvSeason
*: Developer  : TMI - TAREK MOHAMED IBRAHIM ELQUSI
*: Date       : 11/13/2002
*! Purpose    : Valid function of Season mover.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfvSeason()
*!*************************************************************

FUNCTION lfvSeason

IF llFrstSes
  STORE '' TO laRpSTrgt
ELSE
  *--Fill Traget array with the previously saved values.
  FOR lnCout = 1 TO ALEN(laRpSOld,1)
    IF !EMPTY(laRpSOld[lnCout])
      DECLARE laRpSTrgt[lnCout]
      laRpSTrgt[lnCout] = laRpSOld[lnCout]
    ENDIF
  ENDFOR
ENDIF
           
*--Fill arrays.
DECLARE laRpSource[1,2]
STORE '' TO laRpSource

*--Get All Seasons.
=gfwCodePop(@laCodes,'SEASON','L')
IF ALEN(laSeasons,1) > 1
  FOR lnFrom  = 2 TO ALEN(laSeasons,1)
    DECLARE laRpSource[lnFrom-1]
    laRpSource[lnFrom-1] = laSeasons[lnFrom,1]
  ENDFOR
ENDIF
IF llFrstSes
  *--Get Default season.
  =gfwCodePop(@laCodes,'SEASON','D')
  laRpSTrgt[1] = laSeasons[lnSeason,1]
ENDIF  
= gfMover(@laRpSource,@laRpSTrgt,'Select season(S)',.T.,'')
IF EMPTY(laRpSTrgt[1])
  DECLARE laRpSOld[1]
  laRpSOld[1] = ""
ELSE
  FOR lnCout = 1 TO ALEN(laRpSTrgt,1)
    IF !EMPTY(laRpSTrgt[lnCout])
      DECLARE laRpSOld[lnCout]
      laRpSOld[lnCout] = laRpSTrgt[lnCout]
    ENDIF
  ENDFOR
ENDIF
llFrstSes = .F.

*!*************************************************************
*! Name       : lfvDiv
*: Developer  : TMI - TAREK MOHAMED IBRAHIM ELQUSI
*: Date       : 11/13/2002
*! Purpose    : Valid function of Season mover.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfvDiv()
*!*************************************************************

FUNCTION lfvDiv

IF llFrstDiv
  STORE '' TO laRpDTrgt
ELSE
  *--Fill Traget array with the previously saved values.
  FOR lnCout = 1 TO ALEN(laRpDOld,1)
    IF !EMPTY(laRpDOld[lnCout])
      DECLARE laRpDTrgt[lnCout]
      laRpDTrgt[lnCout] = laRpDOld[lnCout]
    ENDIF
  ENDFOR
ENDIF
           
*--Fill arrays.
DECLARE laRpSource[1,2]
STORE '' TO laRpSource

*--Get All Seasons.
=gfwCodePop(@laCodes,'CDIVISION','L')
FOR lnFrom  = 1 TO ALEN(laDivision,1)
  DECLARE laRpSource[lnFrom]
  laRpSource[lnFrom] = laDivision[lnFrom,1]
ENDFOR

IF llFrstDiv
  *--Get Default season.
  =gfwCodePop(@laCodes,'CDIVISION','D')
  laRpDTrgt[1] = laDivision[lnDivision,1]
ENDIF
= gfMover(@laRpSource,@laRpDTrgt,'Select division(s)',.T.,'')
IF EMPTY(laRpDTrgt[1])
  DECLARE laRpDOld[1]
  laRpDOld[1] = ""
ELSE
  FOR lnCout = 1 TO ALEN(laRpDTrgt,1)
    IF !EMPTY(laRpDTrgt[lnCout])
      DECLARE laRpDOld[lnCout]
      laRpDOld[lnCout] = laRpDTrgt[lnCout]
    ENDIF
  ENDFOR
ENDIF

llFrstDiv = .F.

*-- end of lfvDiv.