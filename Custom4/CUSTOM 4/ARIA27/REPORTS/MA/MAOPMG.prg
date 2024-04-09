*:****************************************************************
*: Program file  : MAOPMG.PRG
*: Program desc. : Material Operations Management Report.
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#000400,1
*: Date          : 06/26/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfCreatemp , lfwOGWhen  , lfGetTit
*:               :            : lfSetmmoRp , lfSetVenRp , lfRpHeader
*:               :            : lfAddFltr  , lfCollect  , lfItmPos
*:               :            : lfvOMsg    , lfEndGroup , lfGetFrx
*:               :            : lfAddIssue , 
*:               -----------------------------------------------
*:               : PROCEDURE  : lfShiftArr 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#000400,1.
*:****************************************************************
*:Modifications  :
*:E302018,1 ABD - 09/24/2002 Add Account Code & Cutomer Po# to 
*:E302018,1 ABD -            Material Manfu. Header file , to
*:E302018,1 ABD -            Print it at the Dye Order form.
*:B606711,1 KHM - 02/25/2003 - Get the pattern from the manufacturing order detail file
*:B606711,1 KHM - 02/25/2003 - Add a new field nLotTQty that has decimal places.
*:****************************************************************
*: ---------------------------    N O T E  -------------------------
*: llCollDetl :-
*: This Variable is used to indicate whether to colloect the data from the rolls & Lots 
*: if the operation is considered as a parent opertion so we will get the issue 
*: fabric & rolls for this Operation. This flag is initiated by .T. for any customizations that
*: does not need to get this information then the optional program has to set it to .F.
*: ---------------------------    N O T E  -------------------------
*:
*-- Begin Declaration variables.

*-- llLogo     ==> Variable hold true in case we found a loge for the company.
*-- lcTime     ==> Variable Hold the time.
*-- laFrom     ==> Array hold from address.
*-- laTo       ==> Array hold to   address.
*-- laCompAdd  ==> Array hold company address.
*-- lcTime     ==> Variable to hold the Time.
*-- lcCompName ==> Variable hold the company name.
*-- lcCompPhon ==> Variable hold the company Phone.
*-- lcPhonPict ==> Variable hold the company Phone format.
*-- llIsAparel ==> Variable o know if this program At say or not.
*-- lcPrgName  ==> Variable Hold the form name
*-- llCollDetl ==> Variable to collect detail recored.


DIMENSION laFrom[5], laTo[5], laCompAdd[6] , lashpadr[6] 
STORE '' TO lcCompName , lcCompPhon , lcPhonPict , laFrom , laTo ,laCompAdd , lcBOMTit,;
            lcShpname  , lashpadr , lcObj_Id
lcTime   = TIME()                       && Variable to hold the Time


STORE .F. TO llLogo , llEndGroup
STORE .T. TO llCollDetl
lcTime     = TIME()

lcPrgName  = lcFormName
*-- This Function to check if the fxp For the at say fom on the root o 
*-- Under the module directory.
llIsAparel = lfIsApparl(@lcPrgName)
*-- End Declaration variables.

*-- Function to get the header variables and fields.

= lfRpHeader ()


*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
 
  *-- Create Temp Files.
  = lfCreatemp ()

  *--  Add Some filter to filter Exp. 
  = lfAddFltr ()

  *-- Collecting Code...
  = lfCollect ()
  
ENDIF
*-- End if for user change filter .

*E302018,1 ABD - Create a relation. [Begin]
SELECT mmfgordh
SET RELATION TO 'M'+Account INTO CUSTOMER ADDITIVE
*E302018,1 ABD - [End]

SELECT(lcMainF)
SET RELATION TO LEFT(ITEM,7)+color INTO Fabric ADDITIVE

*E302018,1 ABD - Create a relation. [Begin]
SET RELATION TO ctktno INTO mmfgordh ADDITIVE
*E302018,1 ABD - [End]

Locate

*-- Call the Optional Program
=lfOptProg()

*-- Added this part for Running @SAY.
IF llIsAparel
  *-- IF the file exist do the program.
  IF FILE(lcPrgName +'.FXP')
    lnOldpLen = _pLength 
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT    
    ENDIF
    _pLength  = lnOldpLen 
  ELSE
    *-- Message "Form 'XX' does not exist.Please check your company information settings."   
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" + ALLTRIM(SUBSTR(lcFormName,7,2)) + "'" )            
  ENDIF  
ELSE
  *-- Function to Check If the FRX file exist or not.
  IF lfGetFrx()
    DO gfDispRe WITH EVAL('lcFormName')
  ELSE  
    *-- Message "Form 'XX' does not exist.Please check your company information settings." 
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" + ALLTRIM(SUBSTR(lcFormName,7,2)) + "'" )          
  ENDIF
ENDIF  

SELECT(lcMainF)
SET RELATION TO 
RETURN

*-- End of Code.
*:****************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Create the temp files.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCreatemp()
*:****************************************************************
*
FUNCTION lfCreatemp
PRIVATE lnAlias

lnAlias = SELECT(0)
*-- Create the temporary file
SELECT MFGOPRDT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*B606711,1 KHM 02/25/2003 (Begin) Increasing the number of fields by 2.
*DIMENSION laFileStru[lnFileStru+9,4]
DIMENSION laFileStru[lnFileStru+11,4]
*B606711,1 KHM 02/25/2003 (End)

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cTarget'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0


lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'NoteFlag'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Notes'
laFileStru[lnFileStru,2] = 'M'
laFileStru[lnFileStru,3] = 0
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cOrder'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cType'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Reference'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 25
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'coperSeq'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 2
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Vendor'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cVencomp'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

*B606711,1 KHM 02/25/2003 (Begin) Adding the two fields.
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cPattern'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'nLotTQty'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 12
laFileStru[lnFileStru,4] = 3
*B606711,1 KHM 02/25/2003 (End)

CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru
INDEX ON cTktNo+coperSeq+cType+Item+Color+cDyelot+cOrder+NoteFlag TAG (lcMainF)


SELECT(lnAlias)

*-- End Of lfCreatemp.
*:*************************************************************
*: Name      : lfwOGWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : When function of the option grid
*:*************************************************************
*: Called from : The Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwOGWhen()
*:*************************************************************
*:
FUNCTION lfwOGWhen


*-- End OF lfwOGWhen.
*:*************************************************************
*: Name      : lfGetTit
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : function to get the option titel in Setup file.
*:*************************************************************
*: Called from : The Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfGetTit()
*:*************************************************************
*:
FUNCTION lfGetTit

lcTitle = ALLTRIM(gfGetMemvar('M_MMOLNLBL',gcAct_Comp))
lcTitle = IIF(RIGHT(lcTitle,1) ='#', lcTitle,lcTitle+'#')

RETURN lcTitle
*-- End OF lfGetTit
*:*************************************************************
*: Name      : lfSetmmoRp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Go top in Mfg. Order file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetmmoRp()
*:*************************************************************
*:
FUNCTION lfSetmmoRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT MMFGORDH
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN

*-- End of lfSetmmoRp.
*:*************************************************************
*: Name      : lfSetVenRp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Go top in Vendor file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetVenRp()
*:*************************************************************
*:
FUNCTION lfSetVenRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT APVENDOR
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN

*-- End of lfSetVenRp.
*:*************************************************************
*: Name      : lfShiftArr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : arrange the elements of the array
*:*************************************************************
*: Called from : MFOPMG.PRG
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfShiftArr()
*:*************************************************************
*:
PROCEDURE lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
*-- Get length of the array.
lnALen = ALEN(laArray,1)
*-- Check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR

*-- End OF lfShiftArr.
*:*************************************************************
*: Name      : lfRpHeader
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : get the header data that print in the report
*:*************************************************************
*: Called from : the Program.
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfRpHeader()
*:*************************************************************
*:
FUNCTION lfRpHeader
PRIVATE lnAlias

lnAlias = SELECT (0)

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcCompPhon = cCom_Phon
lcPhonPict = gfPhoneTem()
*--  Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
DO lfShiftArr WITH laCompAdd

SELECT OBJLINK
SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE

IF SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  SELECT gobject;
   FROM Objects         ;
   WHERE Objects.cobject_id = lcObj_Id ;
   INTO CURSOR (lcLogoPic)
ENDIF

SELECT(lnAlias)
RETURN

*-- End OF lfRpHeader.
*:*************************************************************
*: Name      : lfAddFltr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Add new Filter To Filter Exp.
*:*************************************************************
*: Called from : MAMCTCS.prg
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None..
*:*************************************************************
*: Example     : = lfAddFltr()
*:*************************************************************
*
FUNCTION lfAddFltr

*--  Enter the status in the lcRpExp
IF lcStatus <> "L"
  IF EMPTY(lcRpExp)
    lcRpExp= " Mmfgordh.Status = lcStatus "
  ELSE
    lcRpExp = '(' + lcRpExp+ ") .AND. (Mmfgordh.Status = '" + lcStatus +  "')"
  ENDIF
ENDIF

*-- End OF lfAddFltr.
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : function Collect data into temp file.
*:*************************************************************
*: Called from : The Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect()
*:*************************************************************
*:
FUNCTION lfCollect

llWorkFile   = .F.
STORE '' TO lcOpMgFile 

*-- Make a relation between the mfg Operation detail file and vendor file, 
*-- that because we can change the vendor at line level.
SELECT MFGOPRDT
SET RELATION TO ccontcode              INTO Apvendor ADDITIVE

SELECT MFGOPRHD
SET RELATION TO cTktNo                 INTO MMFGORDH ,;
                cImTyp+cTktNo+cOprCode INTO MFGOPRDT
                
                

*-- Get the Position.
lnOpMgPos    = lfItmPos("MFGOPRHD.CTKTNO")
IF lnOpMgPos > 0 
  lcOpMgFile = laOGFxFlt[lnOpMgPos,6]
  llWorkFile = !EMPTY(lcOpMgFile) .AND. USED(lcOpMgFile) .AND. RECCOUNT(lcOpMgFile) > 0
ENDIF

*-- In case use Cursor , Make Relation With MMFGORDH.
IF llWorkFile
  SELECT(lcOpMgFile)
  SET RELATION TO 'T'+cmFgordno INTO MFGOPRHD ADDITIVE
  SET SKIP TO MFGOPRHD
  LOCATE 
  lcScnExp  = [For ]
ELSE
  SELECT MFGOPRHD
  =SEEK('T')
  lcScnExp   = [REST WHILE cImtyp = 'T' For ]
  lcOpMgFile = [MFGOPRHD]
ENDIF

*-- Scan For the selected Mfg Order.
SCAN &lcScnExp &lcRpExp
  lcFabric   = SPACE(7)
  lcColor    = SPACE(6)
  
  *-- Add the recored into the temp line file.
  IF SEEK(MFGOPRHD.cImTyp+MFGOPRHD.cTktNo+MFGOPRHD.cOprCode,'MFGOPRDT')
    SELECT MFGOPRDT
    SCAN WHILE cImTyp+cTktNo+cOprCode= MFGOPRHD.cImTyp+MFGOPRHD.cTktNo+MFGOPRHD.cOprCode ;
         FOR TranCd='1' .and. IIF(EMPTY(lcRpLot),.T.,cLotNo=lcRpLot)
      m.NoteFlag = 'N'
      m.Notes    = ''
      m.cOrder   =  'A'
      m.cType    =  "1"
      m.coperSeq = MfgOprhd.coperSeq
      SCATTER MEMVAR MEMO
      *-- If Exist Acumulate the Qty.
      *- cTktNo+coperSeq+cType+Item+Color+cDyelot+cOrder+NoteFlag

      *B606711,1 KHM 02/25/2003 (Begin) Getting the patter from the manufacturing order detail file
      *cmfgordno+cfabric+color+dyelot+trancd
      IF SEEK(m.cTktNo+LEFT(m.Item,7)+m.Color+m.cDyelot+"1","MmfgOrdD")
        m.cPattern = MmfgOrdD.Pattern
        m.nLotTQty = MmfgOrdD.nMfgTotQty
      ENDIF
      *B606711,1 KHM 02/25/2003 (End)
      
      IF SEEK(m.cTktNo+m.coperSeq+m.cType+m.Item+m.Color+m.cDyelot+m.cOrder,lcMainF)
        SELECT (lcMainF)
        
        *B606711,1 KHM 02/25/2003 (Begin) Adding the replacement of nLotTQty
        *REPLACE nLottotQty WIth nLottotQty + m.nLottotQty
        REPLACE nLottotQty WIth nLottotQty + m.nLottotQty,;
                nLotTQty   WITH nLotTQty + m.nLotTQty
        *B606711,1 KHM 02/25/2003 (End)
        
        SELECT MFGOPRDT  
      ELSE
        INSERT INTO (lcMainF) FROM MEMVAR
        
        *B606711,1 KHM 02/25/2003 (Begin) Adding the replacement of nLotTQty
        SELECT (lcMainF)
        REPLACE nLotTQty   WITH m.nLotTQty
        *B606711,1 KHM 02/25/2003 (End)
        
      ENDIF
      
      IF lcfabric + lcColor < Left(Item,7) + Color
        lcFabric    = LEFT(Item,7)
        lcColor     = Color
      ENDIF
    ENDSCAN
    SELECT (lcOpMgFile)
  ELSE
    LOOP
  ENDIF
  
  *-- Save the Old Fabric And Old Color.
  m.Item   = lcFabric
  m.Color  = lcColor 

  *-- Print notes for Fabric.
  IF llRPrtSn .AND. SEEK('G'+lcFabric,'NotePad')
    m.cType    =  "1"
    m.cOrder   = 'B'
    m.NoteFlag = 'S'
    m.Notes    = Notepad.MNotes
    INSERT INTO (lcMainF) FROM MEMVAR
  ENDIF
  *-- Print notes for Mfg order. 
  IF llrPrtMmon  .AND. SEEK('O'+ctktno,'NotePad')
    m.cType    =  "1"
    m.cOrder   = 'B'
    m.NoteFlag = 'T'
    m.Notes    = Notepad.MNotes
    INSERT INTO (lcMainF) FROM MEMVAR
  ENDIF

    *-- Collect the detail recored for the Paerent Operation. 
    IF llCollDetl
      *-- assigne Empaty to this value in case we will get more lines.
      m.Notes    = ''
      = lfAddIssue()
    ENDIF

  SELECT (lcOpMgFile)
ENDSCAN

SELECT MFGOPRDT
SET RELATION TO 

SELECT MFGOPRHD
SET RELATION TO 

IF llWorkFile
  SELECT (lcOpMgFile)
  SET RELATION TO 
  SET SKIP TO
ENDIF
*-- End OF lfCollect.
*:*************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Called from : This Program.
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfItmPos()
*:*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfItmPos.
*:*************************************************************
*: Name      : lfvOMsg
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : To Open the Optional message screen
*:*************************************************************
*: Called from : Option Grid.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvOMsg()
*:*************************************************************
*:
FUNCTION lfvOMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 1st. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 1st. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.

*-- End OF lfvOMsg.
*:*************************************************************
*: Name      : lfEndGroup
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : To state that if we would print the word "Continued" 
*:             and to initialize some variables.
*:*************************************************************
*: Called from : MAOPMG.FRX
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfEndGroup()
*:*************************************************************
*:
FUNCTION lfEndGroup

*-- Set this variable .T. to don't print the word "CONTINUED"
llEndGroup = .T.

RETURN ''

*-- End OF lfEndGroup.
*!*************************************************************
*! Name      : lfGetFrx
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*! Purpose   : Check If the file exist or not.
*:*************************************************************
*: Called from : MAOPMG.PRG
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfGetFrx()
*:*************************************************************
*:
FUNCTION lfGetFrx
IF RAT('\',lcFormName)=0
  lcFrxPath= IIF(FILE(gcRepHome+lcFormName+'.FRX') ;
  				     .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
  				     gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
ENDIF  				   
RETURN FILE(lcFrxPath +'.FRX') .OR. FILE(lcFrxPath+'.LBX')  				   

*-- End Of lfGetFrx.
*:****************************************************************
*: Name      : lfAddIssue
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : get the issue Qty.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCodeSeq()
*:****************************************************************
*
FUNCTION lfAddIssue
PRIVATE lnAlias , lcScanExp


lnAlias = SELECT(0)
lcScanExp = ''
*-- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
IF SEEK('T' +&lcMainf..cTktNo,'CTKTBOM')
  SELECT Ctktbom
  SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = 'T' + &lcMainf..cTktNo ;
    FOR cOprCode = &lcMainf..cOprcode .AND. issue_qty # 0
    =SEEK(LEFT(Ctktbom.Item,7),'FABRIC')
    =SEEK(Fabric.Vendor,'APVENDOR') 
    SELECT (lcMainf)
    APPEND BLANK
    GATHER MEMVAR MEMO
    
    REPLACE  Item       WITH Ctktbom.ITEM        ,;
             COLOR      WITH Ctktbom.iclr        ,;
             Vendor     WITH Fabric.Vendor       ,;
             cvencomp   WITH APVENDOR.cvencomp   ,;
             NoteFlag   WITH 'Z'                 ,;
             cOrder     WITH 'C'                 ,;
             nLottotQty WITH Ctktbom.Issue_qty   ,;
             Reference  WITH '~!'                ,;
             cType      WITH "2"
    
    *B606711,1 KHM 02/25/2003 (Begin) Adding the replacement of nLotTQty
    REPLACE nLotTQty WITH Ctktbom.Issue_qty
    *B606711,1 KHM 02/25/2003 (End)
    
    *-- Update With Rolls
    
    IF FABRIC.lTrkRolls 
      SCATTER MEMVAR MEMO
      lcScanExp = LEFT(m.Item,7)+m.Color+cTkTBom.cwarecode+m.cDyelot
      *-- crollitem+color+cwarecode+cDyelot+crollid+trancd+crsession
      IF SEEK(lcScanExp,'Rolls')
        SELECT Rolls
        LOCATE REST WHILE crollitem+color+cwarecode+Dyelot+crollid+trancd+crsession = lcScanExp;
        FOR Trancd = "2" .AND. Nqty > 0 .AND. ctktno = &lcMainF..ctktno
      
        IF FOUND()
          lnRolls = 1
          SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession = lcScanExp;
            FOR Trancd = "2" .AND. Nqty > 0 .AND. ctktno = &lcMainF..ctktno
            SELECT (lcMainF)
            APPEND BLANK
            GATHER MEMVAR MEMO
            REPLACE Reference   WITH Rolls.cRollid,;
                    nLottotQty  WITH Rolls.Nqty   ,;
                    cContName   WITH IIF(lnRolls = 1,'ROLL' ,''),;
                    cOrder      WITH 'D',;
                    cType       WITH "2"
           
           *B606711,1 KHM 02/25/2003 (Begin) Adding the replacement of nLotTQty
            REPLACE nLotTQty WITH Rolls.Nqty
            *B606711,1 KHM 02/25/2003 (End)
            
            SELECT ROLLS
            lnRolls = lnRolls + 1
          ENDSCAN
        
        ENDIF
      ENDIF
    ENDIF
    SELECT Ctktbom
  ENDSCAN

ENDIF
SELECT (lnAlias)
RETURN 

*-- End OF lfAddIssue
*:****************************************************************
*: Name      : lfGetCodes
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : get the header data.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : FRX.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfGetCodes()
*:****************************************************************
*
FUNCTION lfGetCodes
PRIVATE lnAlias

lnAlias = SELECT (0)

STORE '' TO lcShpname , lashpadr
*- In case In House Yes.
IF linhouse
  lcShpname = cContName
ELSE
  *- In case In House No.
  IF SEEK(cContCode,'Apvendor')
    lcShpname   = Apvendor.cVencomp
    laShpAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
    laShpAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
    laShpAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
    laShpAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
    laShpAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
    DO lfShiftArr WITH laShpAdr
  ENDIF  
ENDIF

SELECT (lnAlias)
RETURN ''

*-- End OF lfGetCodes
*:****************************************************************