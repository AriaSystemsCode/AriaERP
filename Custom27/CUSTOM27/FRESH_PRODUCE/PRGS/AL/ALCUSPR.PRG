***************************************************************
*: Program file  : ALCUSPR.PRG
*: Program desc. : Customer Priorirty Reassignment
*:                 (For Fresh Produce)
*: Module        : ALLOCATION
*:         System: Aria Apparel System
*:      Developer: IHB
*:*************************************************************
*: Calls 		 : ALCUSPR.SPR
*:         Functions  : 
*:*************************************************************
*: Passed Parameters  : NONE
*:*************************************************************
*: C200082,1 IHB 07/06/1999
*C200137,1 TAK 10/10/2000
*:*************************************************************

STORE '' TO lcTemp, lcBal  && temp file to hold the collected data
lcBrowTtl  = "Customers"

*-- lnMode indicates whether we've prompt either 'close'
*-- or 'cancel' , initially 1 means close. after reassign
*-- it becomes 2 meaning 'cancel'
STORE 1 TO lnMode

*-- Initial values for scope filter
lcOpt1 = 'Yes'   && Include customers with zero netsales
lcOpt2 = 'No'    && Recommended priority less than 998
lcOpt3 = 'Yes'   && Include new customers (less than 90 days)

STORE '' TO lcDelCon, lcRecCon 

lcBrowWind = " "
lcContWind = " "
lcCurExcept= ''
lcColor    = "RGB(,,,192,192,192)"
lnCurrRec  = 0
lnFirstClk = 0
llNoContrl = .T.
llFirstEnt = .T.
llEdit     = .F.
lnOldCurUnt= 0

DIMENSION laScObj[1,5]
IF !gfSetup()
  RETURN
ENDIF

llClick=.F.
STORE 0 TO lnTimelimt,lnCurR
lnLineRange=1.77
laCtrStat[10] = 'DISABLE'
IF !WEXIST(gcBaseWind)
  lcBrowWind = gfTempName()
  lcContWind = gfTempName()
  lcTemp     = gfTempName()
  lcBal      = gfTempName()
ENDIF

PUSH KEY 
ON KEY
ON KEY LABEL TAB        DO lfKeyTrap
ON KEY LABEL BACKTAB    DO lfKeyTrap
ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL Ctrl+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1 
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL ESC        DO lfKeyTrap
ON KEY LABEL INS        DO lfKeyTrap
ON KEY LABEL DEL        DO lfKeyTrap

DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTtl)  


WAIT WINDOW 'Collecting Customers... , please wait.' NOWAIT
*-- Select major needed data
SELECT Account, StName, Priority AS CurPrior,;
       Priority AS NewPrior, Dadd_Date ,;
       0000000000.00 AS YTDSales, 0000000000.00 AS NetSales ;
       FROM CUSTOMER a ;
       INTO TABLE (gcWorkDir+lcTemp) ;
       WHERE a.Type = 'M'
*-- it is left to calculate both net sales and ytd sales
*-- net sales for a customer equal invoices - returns - allowances
*-- for the past 12 monthes (current mont plus past 11 monthes)
*-- ytd sales for a customer equal invoices - returns - allowances
*-- since the begining of the year until now
IF !USED('CREDIT')
  =gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
ENDIF
IF !USED('DEBIT')  
  =gfOpenFile(gcDataDir+'DEBIT',gcDataDir+'DEBIT','SH')
ENDIF
IF !USED('ARHIST')  
  =gfOpenFile(gcDataDir+'ARHIST',gcDataDir+'ARHIST','SH')
ENDIF
**C200137,1
SELECT ARHIST
SET ORDER TO TAG Arhistht


ldPastY  = GOMONTH(gdSysDate,-12)  && past 12 monthes
ldStartY = CTOD('01/01/'+ALLTRIM(STR(YEAR(gdSysDate))))  && start of the year

SELECT (lcTemp)
SCAN
  STORE 0.0 TO lnNetSales,lnYTDSales
 
  SELECT DEBIT
  IF SEEK(&lcTemp..Account)
    lnRec = RECNO()
    SUM REST Amount WHILE Account = &lcTemp..Account ;
             TO lnNetSales FOR BETWEEN(Trandate,ldPastY,gdSysDate)
    GO lnRec
    SUM REST Amount WHILE Account = &lcTemp..Account ;
             TO lnYTDSales FOR BETWEEN(Trandate,ldStartY,gdSysDate)
    SELECT (lcTemp)
    REPLACE YTDSales WITH lnYTDSales ,;
            NetSales WITH lnNetSales
  ENDIF
  SELECT CREDIT
  IF SEEK(&lcTemp..Account)
    lnRec = RECNO()
    SUM REST Amount WHILE Account = &lcTemp..Account ;
             TO lnNetSales FOR BETWEEN(Trandate,ldPastY,gdSysDate)
    GO lnRec
    SUM REST Amount WHILE Account = &lcTemp..Account ;
             TO lnYTDSales FOR BETWEEN(Trandate,ldStartY,gdSysDate)
    SELECT (lcTemp)
    REPLACE YTDSales WITH YTDSales+lnYTDSales ,;
            NetSales WITH NetSales+lnNetSales
  ENDIF
  SELECT ARHIST
  IF SEEK(&lcTemp..Account)
    lnRec = RECNO()
    SUM REST Amount WHILE Account = &lcTemp..Account ;
             TO lnNetSales FOR BETWEEN(Trandate,ldPastY,gdSysDate)
    GO lnRec
    SUM REST Amount WHILE Account = &lcTemp..Account ;
             TO lnYTDSales FOR BETWEEN(Trandate,ldStartY,gdSysDate)
    SELECT (lcTemp)
    REPLACE YTDSales WITH YTDSales+lnYTDSales ,;
            NetSales WITH NetSales+lnNetSales
  ENDIF
ENDSCAN
WAIT CLEAR
SELECT (lcTemp)
INDEX ON NetSales DESCENDING TAG NetSales

SELECT (lcTemp)
GO TOP

*-- Calling the screen
DO (gcScrDir + gcWinAppl + '\ALCUSPR.SPR')

CLEAR TYPEAHEAD

*-- Release the browse pad from the menu.
RELEASE PAD _BROWSE OF _MSYSMENU
RELEASE BAR 100 OF P01PU01 
RELEASE WINDOW (lcBrowTtl)
SET REFRESH TO 0

ON KEY 
POP KEY

IF glQuitting AND WEXIST(gcBaseWind)
  *--In Order to prevent the flicker.
  HIDE WINDOW (gcBaseWind)
  RELEASE WINDOW (lcBrowTtl)
ENDIF

*-- iF The Temp file .DBF exist we are going to erase it
IF FILE(gcWorkdir + lcTemp + '.DBF')
  IF USED(lcTemp)
    USE IN (lcTemp)
  ENDIF
  ERASE &gcWorkdir.&lcTemp..DBF          && Erase the Temp file.
  IF FILE(gcWorkdir + lcTemp + '.CDX')
    ERASE &gcWorkdir.&lcTemp..CDX          && Erase the Temp index.
  ENDIF
ENDIF

RETURN




*!*************************************************************
*! Name      : lfBrowse
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Browse customers along with priorities
*!*************************************************************
*! Called from : ibDummi in the Base Screen (ALCUSPR.SCX)
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfBrowse
SELECT (lcTemp)
GO TOP
lnCurrRec = RECNO()
*-- cDummi is a dummi filed evaluated to display the arrow for the current rec.
BROWSE FIELDS cDummi=IIF(lnCurrRec=RECNO(),">"," ") :V=lfEditRec() :F :W=.F. :H=" ",;
              Account :H="Account" :W=.F. :7, StName :H="Name" :W=.F. :30,;
              YTDSales :H="YTD Sales" :W=.F. :P='99999999.99',;
              NetSales :H="Net Sales" :W=.F. :P='99999999.99',;
              CurPrior :H="Cur. Priority" :W=.F. :8,;
              NewPrior :H="Recommended Priority" :V=lfvPr() :W=.T. :13;
              NOAPPEND  SAVE     ;
              NOWAIT    LOCK 0  NOCLEAR  ;
              TITLE lcBrowTtl ;
              NODELETE ;
              WHEN  lfwBrwWhen() ;
              VALID :F lfvBrwVald();
              WINDOW (lcBrowWind) IN WINDOW (gcBaseWind)



*!*************************************************************
*! Name      : lfwBrwWhen
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : When Browse
*!*************************************************************
*! Called from : lfBrowse()
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfwBrwWhen
glFromBrow = .T.
KEYBOARD "{RIGHTARROW}"
lnCurrRec  = RECNO() 
SHOW WINDOW (lcBrowTtl) REFRESH

*!*************************************************************
*! Name      : lfvBrwVald
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Browse
*!*************************************************************
*! Called from : lfBrowse()
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvBrwVald
*-- In case of DOS only define a pad in the menu for Browse.
IF !WONTOP(lcBrowTtl)
  IF _DOS
    DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' KEY ALT+B
    ON SELECTION PAD _BROWSE OF _msysmenu ACTIVATE WINDOW (lcBrowTtl)
  ELSE
    DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
    ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTtl)      
  ENDIF  
  =gfStopBrow()
ELSE
  IF !WEXIST(lcContWind) 
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO ALCUSPR.SPR
  ENDIF
ENDIF    

*!*************************************************************
*! Name      : lfEntrBrow
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Enter Browse
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*-- Called from the activate at the screen.
FUNCTION lfEntrBrow
RETURN

*!*************************************************************
*! Name      : lfEditRec
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Edit the desc. for the current avilable record.
*!*************************************************************
*! Called from : lfBrowse()
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfEditRec
IF LASTKEY() = 13 .AND. WONTOP() = (lcBrowTtl) .OR. llEdit
  IF gfObj_Lock(.T.)
    PUSH KEY
    ON KEY 
    llNewRec   = .F.
    *-- calling priority filter screen
    DO (gcScrDir + gcWinAppl + '\ALPRFLT.SPR')
    =gfObj_Lock(.F.)
    POP KEY
    SHOW WINDOW (lcBrowTtl) REFRESH
  ENDIF
ENDIF  
llEdit = .F.

*!*************************************************************
*! Name      : lfvOk
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid OK
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvOk
RETURN

*!*************************************************************
*! Name      : lfwNew
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : When New
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfwNew
RETURN

*!*************************************************************
*! Name      : lfvNew
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid New
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvNew
CLEAR TYPEAHEAD
PUSH KEY
ON KEY 
llNewRec   = .T.
llAbort    = .F.
WAIT WINDOW 'Updating Customer Master ...' NOWAIT
SELECT (lcTemp)
SCAN
  IF SEEK('M'+&lcTemp..Account,'CUSTOMER') ;
     AND CUSTOMER.Priority <> &lcTemp..NewPrior
    SELECT CUSTOMER
    REPLACE Priority WITH &lcTemp..NewPrior
  ENDIF
ENDSCAN
SELECT (lcTemp)
REPLACE ALL CurPrior WITH NewPrior
GO TOP

*--
SHOW GET pbClose,1 PROMPT 'Close'     ENABLE
STORE 1 TO lnMode
*

POP KEY
*-- Jump to the browse window.
IF WONTOP() <> lcBrowTtl
  KEYBOARD "{Alt+B}"
ENDIF

*!*************************************************************
*! Name      : lfvCode
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Code
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*-- Valid the code to privent the duplication of records.
*
FUNCTION lfvCode
RETURN

*!*************************************************************
*! Name      : lfvDesc
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Discr.
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*-- Valid the entered desc no empty desc. is allowded.
FUNCTION lfvDesc
RETURN

*!*************************************************************
*! Name      : lfvPrint
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Print
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*-- Print the screen
FUNCTION lfvPrint
lcConsSet = SET('CONSOLE')
SET CONSOLE OFF
lcDevice = gcDevice 
gcDevice = 'PRINTER'
lcOGPlatForm = 'DOS'
lcOutFile = gcOutFile
_PEJECT   = "NONE"
DEFINE WINDOW lcTmpWin FROM 1,1 TO 2,2
ACTIVATE WINDOW lcTmpWin NOSHOW
SET PRINTER TO
SET DEVICE TO SCREEN
lcRpName = 'ALCUSPR'
DO gfDispRe WITH EVAL('lcRpName')
RELEASE WINDOW lcTmpWin
gcDevice = lcDevice 
gcOutFile = lcOutFile
SET CONSOLE &lcConsSet
SET PRINT TO
RETURN

*!*************************************************************
*! Name      : lfKeyTrap
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Trap Keys
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*-- Trap all required key board keys to control the browse.
FUNCTION lfKeyTrap
PARAMETERS lcWhichBut
IF _DOS
  HIDE MENU _MSYSMENU 
  SHOW MENU _MSYSMENU          
ENDIF  
IF PARAM()<>0
  DO CASE
    CASE lcWhichBut='N'
      =lfvNew()
    CASE lcWhichBut='R'
      =lfvPrint()
    CASE lcWhichBut='E'
      =lfEditRec()
    CASE lcWhichBut='C'
       ACTIVATE WINDOW (lcContWind) 
       _CUROBJ= OBJNUM(pbClose) 
       KEYBOARD "{SPACEBAR}"
  ENDCASE
  RETURN
ENDIF
DO CASE 
  CASE LASTKEY() = 9                   && TAB
    DO CASE
      CASE WONTOP() = lcBrowTtl
        _CUROBJ = OBJNUM(pbNew)
        ACTIVATE WINDOW (lcContWind) 
      CASE _CUROBJ = OBJNUM(pbClose)
        ACTIVATE WINDOW (lcBrowTtl)
      OTHERWISE 
        _CUROBJ = _CUROBJ + 1        
    ENDCASE      
  CASE LASTKEY() = 15                 && BAKTAB
    DO CASE
      CASE WONTOP() = lcBrowTtl
        _CUROBJ = OBJNUM(pbClose)
        ACTIVATE WINDOW (lcContWind) 
      CASE _CUROBJ = OBJNUM(pbNew)
        ACTIVATE WINDOW (lcBrowTtl)
      OTHERWISE 
        _CUROBJ = _CUROBJ - 1        
    ENDCASE      
  CASE LASTKEY() = 22                && INS
    =lfvNew()
  CASE LASTKEY() = 7                 && DEL 
    =lfvPrint()
  CASE LASTKEY() = 27                && ESC
    ACTIVATE WINDOW (lcContWind) 
    _CUROBJ= OBJNUM(pbClose) 
    KEYBOARD "{SPACEBAR}"  
ENDCASE
   
*!*************************************************************
*! Name      : lfDclick
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Double Click
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfDclick
PARAMETERS lcKeyLable
RETURN

*!*************************************************************
*! Name      : lfGetObj
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Get Object
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfGetObj
PARAMETER lnX,lnY
lcObj = ''
FOR lnCount=1 TO ALEN(laScObj,1)
  IF BETWEEN(lnX,laScObj[lnCount,1],laScObj[lnCount,3]) AND BETWEEN(lnY,laScObj[lnCount,2],laScObj[lnCount,4])
    lcObj=laScObj[lnCount,5]
    EXIT
  ENDIF
ENDFOR
RETURN lcObj

*!*************************************************************
*! Name      : lfShow
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Show
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfShow
RETURN

*!*************************************************************
*! Name      : lfvExcept
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Except
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvExcept
RETURN

*!*************************************************************
*! Name      : lfvEdit
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Edit
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvEdit
*-- calling scope on filter screen
DO (gcScrDir + gcWinAppl + '\ALPRFLT.SPR')
llEdit = .T.

*!*************************************************************
*! Name      : lfwUnit
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : When Unit
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfwUnit
RETURN

*!*************************************************************
*! Name      : lfvUnit
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Unit
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvUnit
RETURN

*!*************************************************************
*! Name      : lfvPr
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Priority
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvPr
IF EMPTY(NewPrior)
  WAIT WINDOW 'Invalid Entry ...' NOWAIT
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvReass
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Reassign Priority
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvReass
SELECT (lcTemp)
GO TOP
WAIT WINDOW 'Start Reassigning Priorities ... ' NOWAIT
lnCount = 1
DO WHILE !EOF()
**C200137,1
* IF lnCount <> 999
  IF lnCount < 998
    REPLACE NewPrior WITH PADR(STR(lnCount,3),3)
  ELSE
    REPLACE NewPrior WITH '998'
  ENDIF
  lnCount = lnCount + 1
  SELECT (lcTemp)
  SKIP
ENDDO
SELECT (lcTemp)
GO TOP
*--
SHOW GET pbClose,1 PROMPT 'Cancel'     ENABLE
STORE 2 TO lnMode

*!*************************************************************
*! Name      : lfvPrScp
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : Valid Priority Scope
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
FUNCTION lfvPrScp
STORE '' TO lcDelCon, lcRecCon 
IF lcOpt1 = 'No'
  lcDelCon = lcDelCon + ' NetSales = 0'
ENDIF
IF lcOpt1 = 'Yes'
  lcRecCon = lcRecCon + ' NetSales = 0'
ENDIF
**C200137,1
IF lcOpt2 = 'Yes'
  IF EMPTY(lcDelCon)
    lcDelCon = lcDelCon + ' NewPrior = "998"'
  ELSE
    lcDelCon = lcDelCon + ' .OR. NewPrior = "998"'
  ENDIF
ENDIF  
IF lcOpt2 = 'No'
  IF EMPTY(lcRecCon)
    lcRecCon = lcRecCon + ' NewPrior = "998"'
  ELSE
    lcRecCon = lcRecCon + ' .OR. NewPrior = "998"'
  ENDIF
ENDIF  
IF lcOpt3 = 'Yes'
  IF EMPTY(lcRecCon)
    lcRecCon = lcRecCon + ' gdSysDate-Dadd_Date < 90'
  ELSE
    lcRecCon = lcRecCon + ' .OR. gdSysDate-Dadd_Date < 90'
  ENDIF  
ENDIF  
IF lcOpt3 = 'No'
  IF EMPTY(lcDelCon)
    lcDelCon = lcDelCon + ' gdSysDate-Dadd_Date < 90 '
  ELSE
    lcDelCon = lcDelCon + ' .OR. gdSysDate-Dadd_Date < 90 '
  ENDIF  
ENDIF  
SELECT (lcTemp)
IF !EMPTY(lcRecCon)
  RECALL ALL FOR &lcRecCon
ENDIF
IF !EMPTY(lcDelCon)
  DELETE ALL FOR &lcDelCon
ENDIF
GO TOP
_CUROBJ = OBJNUM(pbNew)
CLEAR READ
SHOW WINDOW (lcBrowTtl) REFRESH
RETURN

*!*************************************************************
*! Name      : gfDispRe
*! Developer : IHB
*! Date      : 07/06/1999
*! Purpose   : GF Display Report
*!*************************************************************
*! Called from : ..
*!*************************************************************
*! Calls       : ..
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*-- Note that it is dragged here because it is a custom program
*-- we might use do gfdispre in .. with ..
FUNCTION gfDispRe
PARAMETERS  lcOGRprtNam,lcCriteria,llEndJob,lcRprtLbl,llPrntToFile
*E300341,1 M.H Begin.
lcRprtLbl = IIF(TYPE('lcRprtLbl') $ 'UL','R',lcRprtLbl)
*E300341,1 M.H End.
*B602772,1 Hesham (Start)
*B602772,1 clear setting of device and printer
SET PRINTER TO 
SET DEVICE TO SCREEN
*B602772,1 Hesham (End)

*B601091,1 Hesham El-Sheltawi (Start)
llPrntToFile = IIF(TYPE('llPrntToFile')='L',llPrntToFile,.F.)
IF llPrntToFile AND _DOS AND gcDevice <> "PRINTER"
  llPrntToFile = .F.
ENDIF
*B601091,1 Hesham El-Sheltawi (End)

lcCriteria=IIF(TYPE('lcCriteria')='C' AND !EMPTY(lcCriteria),lcCriteria,'')
PRIVATE lcTempPr,lnOGCount,lnOGTotal,llQuiting
llQuiting=.F.
lcOGRprtNam=IIF(TYPE('lcOGTmpForm')<>'C' OR EMPTY(lcOGTmpForm),lcOGRprtNam,gcWorkDir+lcOGTmpForm)

IF RAT('\',lcOGRprtNam)=0
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  *Renee
  *lcOGRprtNam= IIF(FILE(gcRepHome+lcOGRprtNam+'.FRX'),gcRepHome+lcOGRprtNam,gcRepHome+gcAct_Appl+'\'+lcOGRprtNam)
  lcOGRprtNam= IIF(FILE(gcRepHome+lcOGRprtNam+'.FRX') ;
  				   .OR. FILE(gcRepHome+lcOGRprtNam+'.LBX'),;
  				   gcRepHome+lcOGRprtNam,gcRepHome+gcAct_Appl+'\'+lcOGRprtNam)
  SET FULL &lcFullSet
ENDIF  
STORE '' TO lcOGWhile,lcOGFor
**&gcRepHome.
lnDsRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())

lcOGScope=lfGETSCOPE('lcCriteria')
llWhileFrst=(ATC('WHILE ',lcCriteria)<ATC('FOR ',lcCriteria)) && AND ATC('WHILE ',lcCriteria)>0

IF ATC('WHILE ',UPPER(lcCriteria))>0
  lcOGWhile=IIF(llWhileFrst,SUBSTR(lcCriteria,1,ATC('FOR ',lcCriteria)-1),SUBSTR(lcCriteria,ATC('WHILE ',lcCriteria)))
ENDIF  

IF ATC('FOR ',UPPER(lcCriteria))>0
  lcOGFor  =IIF(llWhileFrst,SUBSTR(lcCriteria,ATC('FOR ',lcCriteria)-1),SUBSTR(lcCriteria,ATC('FOR ',lcCriteria)-1,ATC(lcOGWhile,lcCriteria)-ATC('FOR ',lcCriteria)))
ENDIF  

llCanDisp = RECCOUNT()>0

IF !EMPTY(lcCriteria) OR !EMPTY(lcOGScope)
   IF EMPTY(lcOGScope)
     GO TOP
   ENDIF 
  lcLoctCond = lcOGScope+' '+lcOGFor 
  LOCATE &lcLoctCond
  llCanDisp = FOUND()
ENDIF

IF lnDsRecNo<>0
  GO lnDsRecNo
ENDIF

IF !llCanDisp
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF
*B601090,1 Hesham El-Sheltawi (Start)
*B601090,1 clear the error handler traping to the fox default
*B601090,1 so if any error occures while printing the user can
*B601090,1 select retry and continue printing from the same point
*B601090,1 the error occures
lcOldErrHnd = ON('ERROR')
ON ERROR
lcPrntError = IIF(_DOS AND gcDevice="PRINTER" ,[lfChckPrtStat() AND ],'')
*B601090,1 Hesham El-Sheltawi (End)
DO CASE
  CASE gcDevice="PRINTER" 
    IF glPrnt_Lan                && if it is a lan option

      lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(gnPrnt_No))+'\S='+ALLTRIM(gcServ_Nam);
                      +'\Q='+ALLTRIM(gcQueu_nam);
                      +IIF(_pCopies>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                      +IIF(glBaner,'\B='+gcBaner_H,'\N')

    ELSE
      lcPrintSet = gcPrnt_Port               
    ENDIF       
    IF _DOS
      SET PRINTER TO &lcPrintSet
    ELSE
      SET PRINTER TO
    ENDIF  
    
  CASE gcDevice="SCREEN" OR gcDevice="FILE"
    SET PRINTER TO &gcOutFile
ENDCASE

lcSetCur=SET('CURSOR')
SET CURSOR OFF
*B601091,1 Hesham El-Sheltawi (Start)
*B601091,1 Change the if condition to if platform is dos and we dont
*B601091,1 have to print to file
* IF _DOS 
IF _DOS AND !llPrntToFile
*B601091,1 Hesham El-Sheltawi (End)
  IF gcDevice="PRINTER" AND SYS(13) = "READY"
    IF _PEJECT $ "BOTH,BEFORE"
       lcAddv=_PADVANCE
       _PADVANCE="FORMFEED"
       EJECT
       _PADVANCE=lcAddv  
     ENDIF
  ENDIF   

  IF gcDevice='SCREEN'
    lcTempPr=_PDSETUP
    _PDRIVER=''    
    _PDSETUP=''
  ENDIF

  PRIVATE lnOGTotal,lnOGCount,lcOGSetEsc

  lnOGTotal=RECCOUNT()    
  lnOGCount=1
*IIF(inkey()<>27,.T.,gfModalGen("QRM00114B00023","ALERT") = 1)  
  lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1)])+;
             ' '+lcOGFOR
*             IIF(EMPTY(lcOGFOR),[ FOR  lfRpThermo(lnOGTotal,"lnOGCount","Generating Report")],lcOGFor)+;
             [ AND lfRpThermo(lnOGTotal,"lnOGCount","Generating Report")])
  _PAGENO=1             
  lcTempWin=SYS(2015)             
  lnPPage=0
  DEFINE WINDOW (lcTempWin) FROM 1,1 TO 5,30 DOUBLE COLOR SCHEME 5 SHADOW
  ACTIVATE WINDOW (lcTempWin) NOSHOW
  MOVE WINDOW (lcTempWin) CENTER
  @ 0,2 SAY 'Printing Page 1' SIZE 1,20
  @ 2,2 SAY "Press [ESC] to abort" SIZE 1,20
  ACTIVATE WINDOW (lcTempWin) TOP
  IF glHeader           
*E300341,1 M.H Begin.
    IF lcRprtLbl = 'L'
      LABEL FORM (lcOGRprtNam) TO PRINTER NOCONSOLE &lcCriteria    
    ELSE
      REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
    ENDIF
*E300341,1 M.H End.
  ELSE
*E300341,1 M.H Begin.
    IF lcRprtLbl = 'L'
      LABEL FORM (lcOGRprtNam) TO PRINTER &lcCriteria    
    ELSE
      REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE PLAIN NOEJECT  &lcCriteria    
    ENDIF
*E300341,1 M.H End.
  ENDIF  

  RELEASE WINDOW (lcTempWin)


  IF gcDevice='SCREEN'
    _PDSETUP=lcTempPr
  ENDIF
  
  &&lcReport      
  IF gcDevice="PRINTER" AND SYS(13) = "READY"
    IF _PEJECT $ "BOTH,AFTER"
       lcAddv=_PADVANCE
       _PADVANCE="FORMFEED"
       EJECT
       _PADVANCE=lcAddv  
     ENDIF
  ENDIF 
  IF !llEndJob
    =gfEndPrn()
  ENDIF
*** Windows
ELSE
  *IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) 
  * AND IIF(inkey()<>27,.T.,gfModalGen("QRM00114B00023","ALERT") = 1)
  *B602903,1 If empty(lcOGWhile), do not include a WHILE condition in lcCriteria
  *lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE .T.],lcOGWhile+[])+;
             ' '+lcOGFor
  lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),"",lcOGWhile+[])+;
             ' '+lcOGFor
  *B602903,1 end         

  DO CASE
    CASE gcDevice = "SCREEN"
      IF lcOGPlatForm='WINDOW' OR lcOGPlatForm='MAC'
        IF glHeader           
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam PREVIEW &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam PREVIEW &lcCriteria
          ENDIF
*E300341,1 M.H End.
        ELSE
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL  FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
          ELSE
            REPORT FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
          ENDIF
*E300341,1 M.H End.        
        ENDIF  
      ELSE
        lcTempPr=_PDSETUP
        _PDRIVER=''    
        _PDSETUP=''
        
        *B801058,4 Change this line to add the AND to the WHILE condition 
        *          if the report exprition got a WHILE clauses
        *lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ])+;
        *     ' '+lcOGFor      
        lcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ])+;
             ' '+lcOGFor      
        *B801058,4 Change this line [End]
        
       _PAGENO=1             
       lcTempWin=SYS(2015)             
       lnPPage=0
       DEFINE WINDOW (lcTempWin) FROM 1,1 TO 13,30  COLOR RGB(,,,192,192,192);
       FONT 'MS SANS SERIF',8 STYLE "B" FLOAT ;
			NOCLOSE ;
			NOMINIMIZE ;
			SYSTEM 

       ACTIVATE WINDOW (lcTempWin) NOSHOW
       MOVE WINDOW (lcTempWin) CENTER
       @ 3,8 SAY 'Printing Page 1' SIZE 1,20
       @ 6,8 SAY "Press [ESC] to abort" SIZE 1,20
       ACTIVATE WINDOW (lcTempWin) TOP             
       
        IF glHeader           
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
          ENDIF
*E300341,1 M.H End.
        ELSE
*E300341,1 M.H Begin.
          IF lcRprtLbl = 'L'
            LABEL FORM &lcOGRprtNam TO PRINTER &lcCriteria
          ELSE
            REPORT FORM &lcOGRprtNam TO PRINTER PLAIN NOEJECT  &lcCriteria
          ENDIF
*E300341,1 M.H End.
        ENDIF            
        RELEASE WINDOW (lcTempWin)
        IF !llEndJob
          =gfEndPrn()        
        ENDIF  
        _PDSETUP =lcTempPr               
      ENDIF  

    CASE gcDevice = "PRINTER"
      *B601091,1 Hesham El-Sheltawi (Start)
      *B601091,1 Get the Enviroment variable called ARIAPRN contents
      *B601091,1 and if we were in DOS then we are in case of forcing
      *B601091,1 printing the report to file to ignore any errors while
      *B601091,1 updating some of the data files
*      lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),"??")
      lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),IIF(GETENV('ARIAPRN')='LIB',"",GETENV('ARIAPRN')))
      *B601091,1 Hesham El-Sheltawi (End)  
      *E300254,1 redirect printing under windows to temprory file
      *E300254,1 and then copy this temprory file to the local port selected
      *E300254,1 port from the control pannel you can link the port to any
      *E300254,1 network queue you want
      IF lcOGPlatForm='DOS' OR lcOGPlatForm='UNIX'
        gcOutFile=gcWorkDir+gfTempName()+".TXT"
        SET PRINTER TO (gcOutFile)

       *B603034,1 Hesham (Start)
       IF "NT" $ GETENV('OS') AND TYPE('_pdparms[3]')='C'
         SET DEVICE TO PRINTER
         @0,0 SAY _pdparms[3]
         SET PRINTER TO
         SET PRINTER TO (gcOutFile) ADDI
         SET DEVICE TO SCREEN
       ENDIF
       *B603034,1 Hesham (End)
        
        *B602787,1 Hesham (Start)
        IF _PEJECT $ "BOTH,BEFORE"
          lcAddv=_PADVANCE
          _PADVANCE="FORMFEED"
          EJECT
         _PADVANCE=lcAddv  
        ENDIF
       *B602787,1 Hesham (End)
        PRINTJOB
          IF glHeader           
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
            ELSE
              REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
            ENDIF
*E300341,1 M.H End.
          ELSE
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
            ELSE
              REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
            ENDIF
*E300341,1 M.H End.  
          ENDIF  
        ENDPRINTJOB

        *B602787,1 Hesham (Start)
        IF _PEJECT $ "BOTH,BEFORE"
          lcAddv=_PADVANCE
          _PADVANCE="FORMFEED"
          EJECT
         _PADVANCE=lcAddv  
        ENDIF
       *B602787,1 Hesham (End)
        
        SET PRINTER TO
        *-- set printer to local port
        SET PRINTER TO (gcPrnt_Port)
        llError=.F.
        lcOnErr=ON('ERROR')
        ON ERROR llError=.T.
        lcSetSafe=SET('SAFE')
        SET SAFE OFF
        *-- copy temprory file to local port to print it
       *B601091,1 Hesham El-Sheltawi (Start)
       *B601091,1 save the printer setup variable
       *B601091,1 and then clear it to Ignore the error
       *B601091,1 handler of the Genpd from working
       *B601091,1 and give the handler to the windows
       lcPDSetup = _PDSETUP
       _PDSETUP = ''
       *B601091,1 Hesham El-Sheltawi (End)    
        
        DO WHILE SYS(13)='OFFLINE'
          IF gfModalGen("QRM00054B00015",'ALERT') = 2
            llError=.T.
            EXIT
          ENDIF
        ENDDO 
        IF FILE(gcOutFile) AND llError=.F.
          *B600951,1 Get the value of the output port in another variable
          gcLoclPrt = gcPrnt_Port
          *B600951,1 ORABY 02/28/96
          *B600951,1 ADDING NEW WAY OF PRINTING
          *-- IF ENVIROMENT VARIABLE FOR PRINTING IS DEFINED
          *-- THEN PRINT BY USING THE TECH. ??? ELSE USE THE COPY COMMAND
          *-- Take care that we are setting the printer to the port in which
          *-- we want to print to before starting printing
          DO CASE
            *B601091,1 Hesham El-Sheltawi (Start)
            *B601091,1 change the contition by using the variable
            *B601091,1 lcPrntTool instead of GETENV() so in DOS
            *B601091,1 we force the program to go through this
            *B601091,1 situation
            CASE lcPrntTool = '???' OR lcPrntTool = '??' OR lcPrntTool = '?'
*            CASE GETENV('ARIAPRN')='???'
            *B601091,1 Hesham El-Sheltawi (End)
              lcTmpCur = gfTempName() && temprory cursor
              lcWorkA = SELECT()      && curr. work area
              CREATE CURS &lcTmpCur (mprnfile M) && create temp. cursor
              APPEND BLANK 
              *-- append memo with the output file
              APPEND MEMO mprnfile FROM (gcOutFile) 
              *-- clear the printer port then set the device to printer with the 
              *-- port needed to print.
              SET PRINTER TO 
*              SET DEVICE TO PRINTER
              *B601091,1 Hesham El-Sheltawi (Start)
              *B601091,1 Take care of the lan printing so in case
              *B601091,1 DOS printing we will print to the spool if
              *B601091,1 needed but in windows there is no Lan option
              *B601091,1 at all
              IF glPrnt_Lan                && if it is a lan option

                 lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(gnPrnt_No))+'\S='+ALLTRIM(gcServ_Nam);
                        +'\Q='+ALLTRIM(gcQueu_nam);
                        +IIF(_pCopies>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                        +IIF(glBaner,'\B='+gcBaner_H,'\N')
                 SET PRINTER TO &lcPrintSet        
               ELSE
                 SET PRINTER TO (gcLoclPrt)
               ENDIF       
              *B601091,1 Hesham El-Sheltawi (End)
              SET PRINTER ON
              lnMemoWid = SET('MEMOW')
              lcConsSet = SET('CONS')
              llWrapStat = _WRAP
              lnIndent = _INDENT
              lnRMargin = _RMARGIN
              lnLMargin = _LMARGIN
               SET CONS OFF
               SET MEMOW TO 65              
              _WRAP = .T.
              _RMARGIN = 255
              _LMARGIN = 0
              _INDENT = 2
              *-- reset the printing cursor then start printing the memo field
*              @ 0,0 SAY ""
*              ??? MPRNFILE
              *ZiZo
*              PRINTJOB
*              _PLENGTH = 42
              &lcPrntTool MPRNFILE FUNCTION 'V253'
*              ENDPRINTJOB
              *-- reset the device to screen and clear the printer buffer to make
              *-- the printer start printing.
*              SET DEVICE TO SCREEN
              SET PRINTER OFF
              SET PRINTER TO
              SET CONS &lcConsSet
              SET MEMOW TO lnMemoWid
              _WRAP = llWrapStat
              _INDENT = lnIndent 
              _RMARGIN = lnRMargin 
              _LMARGIN = lnLMargin 
              
              *-- close the temp. cursor and select the curr. work area
              USE IN &lcTmpCur
              SELECT (lcWorkA)          

            *B601091,1 Hesham El-Sheltawi (Start)
            *B601091,1 change the contition by using the variable
            *B601091,1 lcPrntTool instead of GETENV() so in DOS
            *B601091,1 we force the program to go through this
            *B601091,1 situation
            CASE lcPrntTool="COPY"
*            CASE UPPER(GETENV('ARIAPRN'))="COPY"
              SET PRINTER TO (gcLoclPRT)  
             !COPY &gcOutFile &gcLoclPRT
             
            *B601091,1 Hesham El-Sheltawi (Start)
            *B601091,1 change the contition by using the variable
            *B601091,1 lcPrntTool instead of GETENV() so in DOS
            *B601091,1 we force the program to go through this
            *B601091,1 situation
            CASE lcPrntTool="LIB"
*            CASE UPPER(GETENV('ARIAPRN'))="LIB"
              IF FILE("TXTSPOOL.FLL") AND FILE("PRTWIN.EXE") 
                SET LIBRARY TO TXTSPOOL.FLL ADDITIVE
                lcWinPrt  = "IBM Proprinter on " + ALLTRIM(gcLoclPRT)
                lcOrgPrnt = PRTWIN(lcWinPrt)
                =TXTSPOOL(gcOutFile,"ARIA SYSTEMS")
                = PRTWIN(lcOrgPrnt)
                RELEASE LIBRARY TXTSPOOL.FLL
              ENDIF
            CASE lcPrntTool = 'TYPE'
              lcHeadSet = SET('HEADING')
              lnMemoWid = SET('MEMOW')
              lcConsSet = SET('CONS')
              llWrapStat = _WRAP
              lnRMargin = _RMARGIN
              lnLMargin = _LMARGIN
              SET MEMOW TO 255              
              _WRAP = .F.
              _RMARGIN = 255
              _LMARGIN = 0
              SET HEADING OFF
              SET CONS OFF
              *E300399,1 Hesham El-Sheltawi (Start)
              *E300399,1 Clear the Printer Driver that installed
              *E300399,1 to be used by Genpd
              lcTempPr=_PDSETUP
              _PDRIVER=''    
              _PDSETUP=''              
              *E300399,1 Hesham El-Sheltawi (End)              
              SET PRINTER TO (gcPrnt_Port)
              TYPE (gcOutFile) TO PRINTER
              SET PRINTER TO 
              SET HEADING &lcHeadSet
              SET CONS &lcConsSet
              SET MEMOW TO lnMemoWid
              *E300399,1 Hesham El-Sheltawi (Start)
              *E300399,1 Restore the Printer Driver that was in use
              *E300399,1 by Genpd
              _PDSETUP = lcTempPr
              *E300399,1 Hesham El-Sheltawi (End)              
              _WRAP = llWrapStat
              _RMARGIN = lnRMargin 
              _LMARGIN = lnLMargin 
            OTHERWISE
              COPY FILE (gcOutFile) TO (gcPrnt_Port)          
          ENDCASE
    
        ENDIF
        ON ERROR &lcOnErr
        SET SAFE &lcSetSafe    
        *--delete temprory file
       *B601091,1 Hesham El-Sheltawi (Start)
       *B601091,1 Restore the printer driver that was installed
       _PDSETUP = lcPDSetup 
       *B601091,1 Hesham El-Sheltawi (End)    
        
        ERASE (gcOutFile)
      ELSE 
        *B602787,1 Hesham (Start)
        IF _PEJECT = 'NONE'
        ENDIF
       *B602787,1 Hesham (End)
      
        PRINTJOB
          IF glHeader           
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
            ELSE
              *B602787,1 Hesham (Start)            
              *REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
              IF _PEJECT = 'NONE'
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
              ELSE
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE &lcCriteria 
              ENDIF
             *B602787,1 Hesham (End)
              
            ENDIF
*E300341,1 M.H End.  
          ELSE
*E300341,1 M.H Begin.
            IF lcRprtLbl = 'L'
              LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
            ELSE
              
              *B602787,1 Hesham (Start)            
              *REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
              IF _PEJECT = 'NONE'
                REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
              ELSE
                REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE PLAIN &lcCriteria     
              ENDIF
             *B602787,1 Hesham (End)
              
            ENDIF
*E300341,1 M.H End.  
          ENDIF  
        ENDPRINTJOB  
      ENDIF 
    CASE gcDevice = "FILE"

      IF glHeader           
*E300341,1 M.H Begin.
        IF lcRprtLbl = 'L'
          LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
        ELSE
          REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
        ENDIF
*E300341,1 M.H End.  
      ELSE
*E300341,1 M.H Begin.
        IF lcRprtLbl = 'L'
          LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
        ELSE
          REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
        ENDIF
*E300341,1 M.H End.  
      ENDIF  
  ENDCASE  
ENDIF
SET PRINTER TO
SET CURSOR &lcSetCur
*B601090,1 Hesham El-Sheltawi (Start)
*B601090,1 return the error handler setting to the 
*B601090,1 previous routine
ON ERROR &lcOldErrHnd
*B601090,1 Hesham El-Sheltawi (End)

*!**************************************************************************
*!
*!      Function: lfCondUpper
*!         
*!**************************************************************************
*
FUNCTION lfCondUpper
PARAMETERS lcOGCriteria
llToUp=.T.
lcOGUpCrit=''
FOR lnOGCount = 1 TO LEN(lcOGCriteria)
 IF SUBSTR(lcOGCriteria,lnOGCount,1) $ ['"]
   llToUp=!llToUp 
 ENDIF
   lcOGUpCrit=lcOGUpCrit+IIF(llToUp,UPPER(SUBSTR(lcOGCriteria,lnOGCount,1)),;
                            SUBSTR(lcOGCriteria,lnOGCount,1))
ENDFOR
RETURN lcOGupCrit

*!**************************************************************************
*!
*!      Function: lfGetScope
*!         
*!**************************************************************************
*
FUNCTION lfGetScope
PARAMETERS lcOGCriteria
PRIVATE lcOGExact
lcOGExact=SET('EXACT')
SET EXACT ON
lcOGScope = ''
&lcOGCriteria=lfCondUpper(&lcOGCriteria)
&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
lcCriteria = &lcOGCriteria
lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' ALL '),'ALL ','')
&lcOGCriteria = STRTRAN(&lcOGCriteria,'ALL ')
&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' REST '),'REST ','')
&lcOGCriteria = STRTRAN(&lcOGCriteria,'REST ')
&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
IF ATC(' RECORD ',&lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria)+8))
  lcOGRec = lcOGRec+' '
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + 'RECORD '+lcOGRcCont+' '
  lcOGRec = SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria),(ATC(' RECORD ',&lcOGCriteria)+LEN(lcOGrcCont)+8)-ATC(' RECORD ',&lcOGCriteria))
  &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
ENDIF  
&lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
IF ATC(' NEXT ',&lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria)+6))
  lcOGRec = lcOGRec+' '
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + IIF(!EMPTY(lcOGRec),'NEXT '+lcOGRcCont,'')
  lcOGRec = SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria),(ATC(' NEXT ',&lcOGCriteria)+LEN(lcOGrcCont)+6)-ATC(' NEXT ',&lcOGCriteria))
  &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
ENDIF  
SET EXACT &lcOGExact
RETURN lcOGScope

*!**************************************************************************
*!
*!      Function: lfGetcope1
*!       
*!**************************************************************************
*
FUNCTION lfGetcope1
PARAMETERS lcOGCriteria
PRIVATE lcOGExact
lcOGExact=SET('EXACT')
SET EXACT ON
lcOGScope = ''
lcCriteria=lfCondUpper(lcOGCriteria)
lcOGScope = lcOGScope + IIF(lcOGCriteria<>STRTRAN(lcOGCriteria,'ALL'),'ALL ','')
lcOGScope = lcOGScope + IIF(lcOGCriteria<>STRTRAN(lcOGCriteria,'REST'),'REST ','')

IF ATC('RECORD ',lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(lcOGCriteria,ATC('RECORD ',lcOGCriteria)+7))
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + 'RECORD '+lcOGRcCont+' '
ENDIF  

IF ATC('NEXT ',lcOGCriteria)>0
  lcOGRec = ALLTRIM(SUBSTR(lcOGCriteria,ATC('NEXT ',lcOGCriteria)+5))
  lcOGRcCont = SUBSTR(lcOGRec,1,ATC(' ',lcOGRec)-1)
  lcOGScope = lcOGScope + IIF(!EMPTY(lcOGRec),'NEXT '+lcOGRcCont,'')
ENDIF  

SET EXACT &lcOGExact
RETURN lcOGScope

*!**************************************************************************
*!
*!      Function: lfRpThermo
*!       
*!**************************************************************************
*
FUNCTION lfRpThermo
PARAMETERS lnRpTotal,lnRpCount,lcMessage 

WAIT SUBSTR(SPACE(LEN(lcMessage))+lcMessage+SPACE(LEN(lcMessage)),MOD(&lnRpCount,LEN(lcMessage)*2),LEN(lcMessage));
     WINDOW NOWAIT 
&lnRpCount=&lnRpCount+1      
RETURN .T.

*!**************************************************************************
*!
*!      Function: lfQuitReport
*!       
*!**************************************************************************
*
FUNCTION lfQuitReport
PARAMETERS lnMRow,lnMcol

RETURN IIF(LASTKEY()=27,.F.,!(BETWEEN(lnMRow,2,2) AND BETWEEN(lnMCol,2,12)))

*!**************************************************************************
*!
*!      Function: lfDispPage
*!       
*!**************************************************************************
*
FUNCTION lfDispPage
PRIVATE lcOutWin

lcOutWin=WONTOP()
ACTIVATE WINDOW (lcTempWin) SAME

@ IIF(_DOS,0,3),IIF(_DOS,2,8) SAY 'Printing Page '+ALLTRIM(STR(_PAGENO)) SIZE 1,20

lnPPage=_PAGENO

IF !EMPTY(lcOutWin)
  ACTIVATE WINDOW (lcOutWin)
ENDIF



FUNCTION lfChckPrtStat
RETURN (SET('PRINTER')='ON')

*--
*!*************************************************************
*! Name        : lfvClose
*! Developer   : IHB
*! Date        : 14/07/1999
*!*************************************************************
*! Purpose     : Validate close push button
*!*************************************************************
*! Calls       : 
*!*************************************************************
FUNCTION lfvClose
IF lnMode = 1
  glQuitting = .T.
  CLEAR READ
  RETURN
ELSE
  *-- cancel without saving
  SELECT (lcTemp)
  REPLACE ALL NewPrior WITH CurPrior
  GO TOP
  SHOW GET pbClose,1 PROMPT 'Close'     ENABLE
  STORE 1 TO lnMode
ENDIF  




