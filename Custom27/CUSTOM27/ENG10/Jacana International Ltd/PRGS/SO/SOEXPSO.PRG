*:************************************************************************
*: Program file  : SoExpSo.PRG
*: Program desc. : Export Sales Orders to text file
*: System        : Aria Apparel System
*: Date          : 03/10/2002
*: Developer     : RANIA ABDEL RAZIK (RAE)
*: Due to        : C200298
*:************************************************************************
*: Modifications :
*: CP200395,1 BWA 09/19/2002 Browse the filtered orders when saving.
*: B606747,1 BWA 1/2/2003 Fix bug of inrange screen doesn't work.
*: C200528,1 ABD 05/27/2003 Add 2 new fields to the Style File.
***************************************************************************
DECLARE laSeasons[1,2] , laSetups[2,2]   , laDivision[1,2], laCodes[2,10];
        laRpTarget[1,2], laRpSource[1,2] , laUpdate[1,2]
        
STORE '' TO lcFileName, lcChosSDir, lcOrdEntD , lcString1 , lcString2 ,;
			lcDivStr  , lcSesStr  , lcOrdEntD , lcDigit1  , lcDigit2  ,;
			lcTotal1  , lcTotal2  , lcChekDig1, lcChekDig2, lcLinNo   ,;
			lcSDate   , lcCmpDate , lcCount   , lcDiv     , lcSes     ,;
			lcRltFld  , lcDivision, lcPrice   , lcPrice1  , lcPrice2  ,;
			lcFld10

DIMENSION laRpSTrgt[1],laRpSOld[1],laRpDTrgt[1],laRpDOld[1]
STORE ""  TO laRpSTrgt[1] , laRpSOld[1],laRpDOld[1],laRpDTrgt[1]
STORE .F. TO llFound
STORE 1   TO lnSeason, lnDivision, lnVal1, lnVal2, lnTot1, lnTot2
STORE 2   TO lnCount1
STORE 0   TO lnSTot1, lnSTot2, lnSTot3, lnSTot4, lnTotal1, lnTotal2, lnChekDig1, lnChekDig2,;
             lnElement
lcSysDir = SPACE(01)

*CP200395,1 BWA 09/19/2002 Initial Variables.[START]
llRturnUsr = .F.
lnFileHNDL = 0 

IF !gfSetup()
  RETURN
ENDIF  
*CP200395,1 BWA 09/19/2002.[END]

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
=gfGetMemVar(@laSetups,gcAct_Comp)

ON KEY LABEL ESC DO lfClsScr
DO (gcScrDir+gcWinAppl+"\SoExpSo.SPX")

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 03/11/2002
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

*-- Set relations
SELECT ORDLINE
SET RELATION TO ORDLINE.style INTO STYLE ADDITIVE
LOCATE
SELECT STYLE
SET RELATION TO 'S'+ STYLE.scale INTO SCALE ADDITIVE
LOCATE

*!*************************************************************
*! Name      : lfvSysDir
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 03/10/2002
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

lcSysDir = gcWorkDir
lcSysDir = GETDIR("","Select SysDir")
SHOW GET lcFileName ENABLE
lcChosSDir = ALLT(lcSysDir)

*-- Check if the user doesn't select any directory and press Cancel
IF EMPTY(lcSysDir)
  lcSysDir = gcWorkDir
ENDIF

*--- Check if the last character is '\'
IF !EMPTY(lcSysDir) .AND. SUBSTR(lcSysDir,LEN(lcSysDir)) <> '\'
  lcSysDir = lcSysDir + '\'
ENDIF
SHOW GET lcSysDir DISABLE
_CUROBJ = _CUROBJ
_CUROBJ = OBJNUM(lcFileName)

*!*************************************************************
*! Name      : lfCrtFile
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 03/10/2002
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

lnFileHNDL = FCREATE(lcSysDir+lcFileName+'.TXT')
RETURN lnFileHNDL

*!*************************************************************
*! Name      : lfvOk
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 03/10/2002
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

*-- Check if the user enter a name for a text file or no.
IF EMPTY(lcFileName)
  =gfModalGen('INM32100B00000','ALERT')
  RETURN
ENDIF
_CUROBJ = OBJNUM(lcFileName)

IF FILE(lcSysDir+lcFileName+'.txt')					      && Does file exist?
  lnFileOpen = FOPEN(lcSysDir+lcFileName+'.txt',12)	      && If so, open read/write
  IF lnFileOpen > 0
    =FOPEN(lcSysDir+lcFileName+'.txt',10)
    =gfModalGen('INM32103B00000','ALERT',lcFileName)
    RETURN

  *CP200395,1 BWA 09/19/2002 In the 2nd collecting data.[START]
  ELSE
    IF !lfColData()
      =FCLOSE(lnFileOpen)
      STORE SPACE(0) TO lcSysDir , Lcfilename
      SHOW GET lcSysDir
      SHOW GET lcFileName
      RETURN
    ENDIF
  *CP200395,1 BWA 09/19/2002.[END]

  ENDIF
ELSE
  lnFileHNDL = lfCrtFile()
  IF lnFileHNDL < 0
    *-- Can not open or create output file.
    =gfModalGen('INM32101B00000','ALERT')
    RETURN
  ELSE
    IF !lfColData()

      *CP200395,1 BWA 09/19/2002 Close the TEXT file and refresh the fields.[START]
      =FCLOSE(lnFileHNDL)
      STORE SPACE(0) TO lcSysDir , Lcfilename
      SHOW GET lcSysDir
      SHOW GET lcFileName
      *CP200395,1 BWA 09/19/2002.[END]

      RETURN
    ENDIF
  ENDIF
ENDIF
*-- Close file

*CP200395,1 BWA 09/19/2002.[START]
*CLEAR READ
IF llRturnUsr
  CLEAR READ
ENDIF
*CP200395,1 BWA 09/19/2002.[END]

*!*************************************************************
*! Name      : lfColData
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 03/11/2002
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

*CP200395,1 BWA 09/19/2002 Refresh the variables hold the division and the season .[START]
STORE '' TO lcDivStr  , lcSesStr
*CP200395,1 BWA 09/19/2002.[END]

FOR lnCount = 1 TO ALEN(laRpDOld,1)
  IF !EMPTY(laRpDOld[lnCount])
    lcDiv = SUBSTR(laRpDOld[lnCount],1,6)+','
    lcDivStr = lcDivStr + lcDiv
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(laRpSOld,1)
  IF !EMPTY(laRpSOld[lnCount])
    lcSes = SUBSTR(laRpSOld[lnCount],1,6)+','
    lcSesStr = lcSesStr + lcSes
  ENDIF
ENDFOR

*CP200395,1 BWA 09/19/2002 Browse the filtered orders when saving.[START]
PRIVATE lcAliasSty

lcAliasSty = ALIAS()
STORE SPACE(0) TO lcExpsty
lcTempOrder = gfTempName()

CREATE CURSOR (lcTempOrder) (Order C(6))
ZAP
INDEX ON Order TAG (lcTempOrder) OF (gcWorkDir+lcTempOrder+'.CDX')

*C200528 ABD - Add send order field to the browse. [Begin]
*lcBrowFlds = [ORDER     :H = 'ORDER#'       :15 ,]  + ;
      	     [STATUS    :H = 'S  '          :5  ,]  + ;
      	     [ACCOUNT   :H = 'Account'      :15 ,]  + ;
             [SEASON    :H = 'Season'       :25 ,]  + ;
             [CDIVISION :H = 'Division'     :25  ]  
lcBrowFlds = [ORDER     :H = 'ORDER#'       :15 ,]  + ;
      	     [STATUS    :H = 'S  '          :5  ,]  + ;
      	     [ACCOUNT   :H = 'Account'      :15 ,]  + ;
             [SEASON    :H = 'Season'       :25 ,]  + ;
             [CDIVISION :H = 'Division'     :25 ,]  + ;
             [lcSend=IIF(Sendord='Y','Yes','No')   :H = 'Send Order'   :15 ]
*C200528 ABD - [End]

SELECT ORDHDR
LOCATE
lcFltOrd = IIF(!EMPTY(lcDivStr) , "ORDHDR.Cdivision $ lcDivStr" ,".T.")     +;
           IIF(!EMPTY(lcSesStr) ,' AND ' + "ORDHDR.Season $ lcSesStr" , "") +;
           ' AND ' + 'ORDHDR.SENDORD = SUBSTR(lnGelco,1,1)'                 +;
           ' AND ' + 'ORDHDR.STATUS <> "C"'                                 +;
           ' AND ' + 'ORDHDR.STATUS <> "X"'

lcExpsty = 'FOR ' + IIF(!EMPTY(lcDivStr) , "ORDHDR.Cdivision $ lcDivStr" ,".T.")     +;
                    IIF(!EMPTY(lcSesStr) ,' AND ' + "ORDHDR.Season $ lcSesStr" , "") +;
                    ' AND ' + 'ORDHDR.SENDORD = SUBSTR(lnGelco,1,1)'                 +;
                    ' AND ' + 'ORDHDR.STATUS <> "C"'                                 +;
                    ' AND ' + 'ORDHDR.STATUS <> "X"'

SET FILTER TO &lcFltOrd
LOCATE
IF EOF('ORDHDR')
  PUSH KEY CLEAR
  =gfModalGen('TRM00052B00000','DIALOG')
  _CUROBJ = _CUROBJ
  POP KEY
  RETURN
ELSE
  PUSH KEY CLEAR

  *B606747,1 BWA 1/2/2003 Fix bug of inrange screen doesn't work.[START]
  lcExpsty = ["O"] + SPACE(1) + lcExpsty
  *B606747,1 BWA 1/2/2003.[END]

  llRturnUsr =gfrange(lcBrowFlds,lcTempOrder,"Order",(lcExpsty),"","","@! XXXXXX")
  POP KEY
  IF !llRturnUsr
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  SELECT (lcAliasSty)
ENDIF
*CP200395,1 BWA 09/19/2002.[END]


llFound = .F.
SELECT ORDHDR

*CP200395,1 BWA 09/19/2002 Browse the filtered orders when saving and fix the bug of the status.[START]
*SCAN FOR cordtype+order = 'O' ;
        .AND. IIF(!EMPTY(lcDivStr),Cdivision $lcDivStr,.T.);
        .AND. IIF(!EMPTY(lcSesStr),Season    $lcSesStr,.T.);
        .AND. Status    <> "CX" .AND. ;
        IIF(SUBSTR(lnGelco,1,1)="N", (SENDORD<>"Y"),(SENDORD="Y"))

SCAN FOR cordtype+order = 'O' ;
        .AND. IIF(!EMPTY(lcDivStr),Cdivision $lcDivStr,.T.);
        .AND. IIF(!EMPTY(lcSesStr),Season    $lcSesStr,.T.);
        .AND. Status <> "C"       ;
        .AND. Status <> "X" .AND. ;
        IIF(SUBSTR(lnGelco,1,1)="N", (SENDORD<>"Y"),(SENDORD="Y"))

  IF IIF(EOF(lcTempOrder) AND RECCOUNT(lcTempOrder) = 0, .T. , SEEK(ORDHDR.ORDER,lcTempOrder))
  *CP200395,1 BWA 09/19/2002.[END]

  *B606136
  *-- if ORDHDR.STORE is not empty pick up CUSTOMER.USR_DFND1 based on ORDHDR.STORE
  *-- not ORDHDR.account
  =SEEK(IIF(!EMPTY(ORDHDR.STORE),'S','M')+ Ordhdr.account + IIF(!EMPTY(ORDHDR.STORE),ORDHDR.STORE,""),'CUSTOMER')
  
  WAIT WINDOW "Collecting data for Order# " +OrdHdr.Order  NOWAIT
  llFound = .T.
  =lfHdrData()
  IF SEEK(OrdHdr.cordtype+OrdHdr.order,'OrdLine')
    SELECT ORDLINE
    SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = OrdHdr.cordtype+OrdHdr.order
      WAIT WINDOW "Collecting data for Order# "+OrdHdr.Order+" style :" +OrdLine.Style NOWAIT
      =lfLinData()
    ENDSCAN
  ENDIF
  =FFLUSH(lnFileHNDL)

  *CP200395,1 BWA 09/19/2002 Browse the filtered orders when saving.[START]
  ENDIF
  *CP200395,1 BWA 09/19/2002.[END]

ENDSCAN
SELECT ORDHDR
REPLACE ORDHDR.SENDORD WITH "Y"
IF llFound
  *-- the text file was created.
  =gfModalGen('INM32102B00000','ALERT')
  =FCLOSE(lnFileHNDL)
  RETURN .F.
ELSE
  *-- If there is no data matching the selected season(s) and division(s).
  =gfModalGen('INM32105B00000','ALERT')
  =FCLOSE(lnFileHNDL)
  ERASE lcSysDir+lcFileName+'.txt'
  RETURN .F.
ENDIF
WAIT CLEAR

*!*************************************************************
*! Name      : lfHdrData
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 03/10/2002
*! Purpose   : Write the sales order header record data.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfHdrData()
*!*************************************************************

FUNCTION lfHdrData

lcOrdEntD = DTOS(ORDHDR.entered)
*-- Position 5
lcString1 = PADL(ALLT('4029635'+SUBSTR(CUSTOMER.usr_dfnd1,1,6)),13)

*-- Position 7
lcString2 = '4029635098700'
=FPUTS(lnFileHNDL,'100'+lcOrdEntD+PADR(ORDHDR.ORDER,15)+SPACE(16)+lcString1+;
                   SPACE(26)+lcString1+SPACE(13)+lcString2+SPACE(56))

REPLACE ORDHDR.SENDORD WITH "Y"

*!*************************************************************
*! Name       : lfLinData
*: Developer  : Rania Abdel Razik
*: Date       : 03/13/2002
*! Purpose    : Write the sales order line records data.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfLinData()
*!*************************************************************
FUNCTION lfLinData

lcLinNo    = PADL(ALLTRIM(STR(lineno)),3)
lcSDate    = ORDHDR.START
lcSDate    = DTOS(lcSDate)    && (YYYYMMDD)
lcCmpDate  = ORDLINE.COMPLETE
lcCmpDate  = DTOS(lcCmpDate)
lcPrice    = ALLTRIM(STR(STYLE.GROS_PRICE,16,4))
lcPrice1   = SUBSTR(lcPrice,1,AT('.',lcPrice)-1)
lcPrice2   = SUBSTR(lcPrice,AT('.',lcPrice)+1)
lcGrosPric = PADL(lcPrice1+lcPrice2,20,'0')

*B606747,1 BWA 1/2/2003 Chnage the line of data due to the customer.[START]
*IF ORDHDR.CDIVISION = 'WO' .OR. ORDHDR.CDIVISION = 'WI' .OR. ORDHDR.CDIVISION = 'WC'
*  lcDivision = '777 '
*ELSE
*  lcDivision = '    '
*ENDIF

IF ORDHDR.CDIVISION = 'WO' .OR. ORDHDR.CDIVISION = 'WI' .OR. ORDHDR.CDIVISION = 'WC'
  
  *C200528 ABD - Add check for the new field gAw if empty let the 
  *C200528 ABD - Progarm work as it is, else update the filed v10 with style.gAw. [Begin]
  *lcDivision = '7777'
  lcDivision = IIF(Empty(STYLE.gAw),'7777',STYLE.gAw)
  *C200528 ABD - [End]
  
  lcFld10  = '777'
ELSE
  lcFld10 = '   '
  lcDivision = '    '
ENDIF
*B606747,1 BWA 1/2/2003.[END]

SELECT ORDHDR
lcRltFld  = ''
lcNewSes  = ORDHDR.SEASON
DECLARE laTrmRltFd[1,2]
laTrmRltFd[1,1] = 'CALTSES'
laTrmRltFd[1,2] = 'lcRltFld'
=gfRltFld(lcNewSes , @laTrmRltFd , "CNEWSES")
SELECT ORDLINE
FOR lnCount = 1 TO SCALE.Cnt
  lcCount = ALLT(STR(lnCount))
  IF !EMPTY(ORDLINE.QTY&lcCount)
    ll = 0
    DO WHILE (ll = 0)

      *B606747,1 BWA 1/2/2003 Chnage the line of data due to the customer.[START]
      *ll = FPUTS(lnFileHNDL,'200'+lcLinNo+lcSDate+lcCmpDate+SPACE(8)+lcCmpDate+SPACE(13)+;
          PADR(STYLE.CVENSTY,20)+PADL(ORDLINE.GROUP,2)+lcDivision+SPACE(20)+;
	      lCGrosPric+''+'EUR'+STR(ORDLINE.QTY&lcCount,6)+SPACE(3)+;
          PADR(SUBSTR(ORDLINE.STYLE,9,4),10)+;
          PADR(SUBSTR(ORDLINE.STYLE,14,3),4)+;
          PADR(SUBSTR(ORDLINE.STYLE,1,7),10)+;
          SUBSTR(STYLE.PATTERN,1,2)+SUBSTR(lcRltFld,1,3)+SPACE(12)+;
          PADR(SUBSTR(SCALE.CNRFCODE&lcCount,3,2),10))
      *C200528 ABD - replace field 9 with new field call style.gsetup.f [Begin]
      *ll = FPUTS(lnFileHNDL,'200'+lcLinNo+lcSDate+lcCmpDate+SPACE(8)+lcCmpDate+SPACE(13)+;
          PADR(STYLE.CVENSTY,20)+PADL(ORDLINE.GROUP,2)+lcDivision+lcFld10+SPACE(17)+;
	      lCGrosPric+''+'EUR'+STR(ORDLINE.QTY&lcCount,6)+SPACE(3)+;
          PADR(SUBSTR(ORDLINE.STYLE,9,4),10)+;
          PADR(SUBSTR(ORDLINE.STYLE,14,3),4)+;
          PADR(SUBSTR(ORDLINE.STYLE,1,7),10)+;
          SUBSTR(STYLE.PATTERN,1,2)+SUBSTR(lcRltFld,1,3)+SPACE(12)+;
          PADR(SUBSTR(SCALE.CNRFCODE&lcCount,3,2),10))
      ll = FPUTS(lnFileHNDL,'200'+lcLinNo+lcSDate+lcCmpDate+SPACE(8)+lcCmpDate+SPACE(13)+;
          PADR(STYLE.CVENSTY,20)+STR(Style.gSetup,2)+lcDivision+lcFld10+SPACE(17)+;
	      lCGrosPric+''+'EUR'+STR(ORDLINE.QTY&lcCount,6)+SPACE(3)+;
          PADR(SUBSTR(ORDLINE.STYLE,9,4),10)+;
          PADR(SUBSTR(ORDLINE.STYLE,14,3),4)+;
          PADR(SUBSTR(ORDLINE.STYLE,1,7),10)+;
          SUBSTR(STYLE.PATTERN,1,2)+SUBSTR(lcRltFld,1,3)+SPACE(12)+;
          PADR(SUBSTR(SCALE.CNRFCODE&lcCount,3,2),10))
      *C200528 ABD - [End]
      *B606747,1 BWA 1/2/2003.[END]

	  IF ll = 0
         WAIT WINDOW "CANNOT WRITE TO TXT FILE" NOWAIT
      ENDIF
	ENDDO
  ENDIF
ENDFOR

*!*************************************************************
*! Name       : lfClsScr
*: Developer  : Rania Abdel Razik
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

USE IN ORDHDR
USE IN ORDLINE
USE IN CUSTOMER
USE IN CODES
USE IN STYLE
USE IN SCALE
CLEAR READ

*!*************************************************************
*! Name       : lfvSeason
*: Developer  : Rania Abdel Razik
*: Date       : 03/26/2002
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

llFirst = EMPTY(laRpSOld[1])
IF llFirst
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
IF llFirst
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

*!*************************************************************
*! Name       : lfvDiv
*: Developer  : Rania Abdel Razik
*: Date       : 03/26/2002
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

llFirst = EMPTY(laRpDOld[1])
IF llFirst
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

IF llFirst
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