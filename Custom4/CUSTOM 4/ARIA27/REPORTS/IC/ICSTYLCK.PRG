*****************************************************************************
*: Program file  : ICINVLCK.PRG (C# 101333)
*: Program desc. : Style Invintory Locking Report.
*: Module        : IC (Inventory Control)
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls : FUNCTIONS  : lfColData,lfClearRep,lfvBatch,lfWoldVal,lfwRep
*:                      lfClrRed,lfvWareCode,lfPrnType
*****************************************************************************
*: Passed Parameters  : None
***********************************************************************************************
*:B603610,1 ADEL 4/4/2000 Make the Totals fields work to show report totals not just last line.
*:B603610,1               All above modifications are done in ICSTYLCK.FRX.
*:B603610,1               Add 'DYELOT' column to the frx and fix the bug of duplicating records when using dyelots and bins.
*:B603596,1 KHM 06/18/2000 Changing the field cBatch to be cLkBatch due to the changes
*:B603596,1                that were done in the files.
***********************************************************************************************
R_TITLE   = 'Style Inventory Locking Report'

*--- Set necessary relation
lcStyPic = gfItemMask('HI')
llMulti  = (gfGetMemVar('M_WareHouse') = 'Y')
lcBinStr = ''
*--- Start Main Program
IF ldLockDate = {}
  WAIT WINDOW 'Lock date can not be empty...'
  RETURN
ENDIF
*B603610,1 (Begin) Open style file and get the dyelot flag.
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
ENDIF
*--Dyelot header and data in the FRX will be printed under the flag being .T.
llDyelot   = gfGetMemVar('M_Dyelot')   ='Y'
*B603610,1 (End)

IF llOgFltCh
  =lfColData()
ENDIF
SELECT (lcRpTmp)
IF lcRPZero = 'N'
  SET FILTER TO TotStk <> 0
ENDIF

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
*B603596,1                we removed the cBatch from the file and add cLkBatch instead.
*SET RELATION TO 'S'+CBATCH INTO MDINVNTH
SET RELATION TO 'S'+CLKBATCH INTO MDINVNTH
*B603596,1 KHM 06/18/2000 (End)

GOTO TOP
DO gfDispRe WITH EVAL('lcRpForm')
SELECT (lcRpTmp)
SET RELATION TO
SET DEVICE TO SCREE
*--- End Main

*!*************************************************************
*! Name      : lfColData
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : Function to Collect data.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfColData()
*!*************************************************************
*!
FUNCTION lfColData

USE IN IIF(USED(lcRpTmp),(lcRpTmp),0)
SELECT MDINVNTL
COPY STRUC TO &gcWorkDir.&lcRpTmp
=gfOpenFile('&gcWorkDir.&lcRpTmp',' ','EX')

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
*B603596,1                we removed the cBatch.
*INDEX ON cWareCode+cBatch+Style+Color+cLocation TAG (lcRpTmp)
INDEX ON cWareCode+cLkBatch+Style+Color+cLocation TAG (lcRpTmp)
*B603596,1 KHM 06/18/2000 (End)

SELECT MDINVNTL

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
*B603596,1                we removed the cBatch.
*SET RELATION TO 'S'+CBATCH INTO MDINVNTH
SET RELATION TO 'S'+CLKBATCH INTO MDINVNTH
*B603596,1 KHM 06/18/2000 (End)

SELECT MDINVNTL
=SEEK('S'+IIF(EMPTY(lcRpBatch),'',lcRpBatch))
*B603610,1 (Begin) For any style useing dyelots, don't calculated the header record (holding dyelots total) and its lines.
*SCAN REST WHILE cbattype+cbatch+style+color = ;
                IIF(!EMPTY(lcRpBatch),'S' + lcRpBatch ,'S');
          FOR   MDINVNTH.DATE = ldLockDate .AND. ;
                IIF(!EMPTY(lcRpWareH),cWareCode=lcRpWareH,.T.)

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
*B603596,1                we removed the cBatch.
*SCAN REST WHILE cbattype+cbatch+style+color = ;
                IIF(!EMPTY(lcRpBatch),'S' + lcRpBatch ,'S');
          FOR   MDINVNTH.DATE = ldLockDate .AND. ;
                IIF(!EMPTY(lcRpWareH),cWareCode=lcRpWareH,.T.) AND ;
                IIF(SEEK (Style,'Style') AND Style.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
SCAN REST WHILE cbattype+cLkbatch+style+color = ;
                IIF(!EMPTY(lcRpBatch),'S' + lcRpBatch ,'S');
          FOR   MDINVNTH.DATE = ldLockDate .AND. ;
                IIF(!EMPTY(lcRpWareH),cWareCode=lcRpWareH,.T.) AND ;
                IIF(SEEK (Style,'Style') AND Style.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
*B603596,1 KHM 06/18/2000 (End)

*B603610,1 (End)
  SCAT MEMVAR MEMO
  
  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
  *B603596,1                we removed the cBatch.
  *IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cBatch+m.Style,lcRpTmp)
  IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style,lcRpTmp)
  *B603596,1 KHM 06/18/2000 (End)
    SELECT (lcRpTmp)
    REPLACE Stk1     WITH Stk1+MDINVNTL.Stk1;
            Stk2     WITH Stk2+MDINVNTL.Stk2;
            Stk3     WITH Stk3+MDINVNTL.Stk3;
            Stk4     WITH Stk4+MDINVNTL.Stk4;
            Stk5     WITH Stk5+MDINVNTL.Stk5;
            Stk6     WITH Stk6+MDINVNTL.Stk6;
            Stk7     WITH Stk7+MDINVNTL.Stk7;
            TotStk   WITH TotStk+MDINVNTL.TotStk;
            OldStk1  WITH OldStk1+MDINVNTL.OldStk1;
            OldStk2     WITH OldStk2+MDINVNTL.OldStk2;
            OldStk3     WITH OldStk3+MDINVNTL.OldStk3;
            OldStk4     WITH OldStk4+MDINVNTL.OldStk4;
            OldStk5     WITH OldStk5+MDINVNTL.OldStk5;
            OldStk6     WITH OldStk6+MDINVNTL.OldStk6;
            OldStk7     WITH OldStk7+MDINVNTL.OldStk7;
            OldTotStk   WITH OldTotStk+MDINVNTL.OldTotStk;
            cLocation   WITH SPACE(01)
  ELSE
    INSERT INTO (lcRpTmp) FROM MEMVAR 
  ENDIF
ENDSCAN
SELECT MDINVNTL
SET RELATION TO
*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 14/07/1999
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
ERASE &gcWorkDir.&lcRpTmp+'.DBF'
ERASE &gcWorkDir.&lcRpTmp+'.CDX'


*!*************************************************************
*! Name      : lfvBatch
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 14/07/1999
*! Purpose   : Valid function for batch no.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvBatch()
*!*************************************************************
FUNCTION lfvBatch

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK("S"+lcObjVal , 'MDINVNTH'))
  SELECT MDINVNTH
  SET ORDER TO TAG MDINVNTH
  lcFile_Ttl  = 'Select Batch'
  lcPbrowtitl = 'Batch No.'
  
  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
  *B603596,1                we removed the cBatch.
  *lcBrFields  = "cBatch    :12   :H = 'Batch No.',"+;
                "lctmp = SUBSTR(Content,1,60) :60 :H = 'Desc.',"+;
                "Date      :12   :H = 'Lock Date',"+;
                "DPOSTDATE :12   :H = 'Post Date',"+;
                "CWareCode :15   :H = 'Location'"
                &&,"+;
                "lcdesc =IIF(SEEK(CWareCode,'WareHous'),WareHous.cDesc,'')  :35   :H = 'Location Desc'"
  lcBrFields  = "cLkBatch    :12   :H = 'Batch No.',"+;
                "lctmp = SUBSTR(Content,1,60) :60 :H = 'Desc.',"+;
                "Date      :12   :H = 'Lock Date',"+;
                "DPOSTDATE :12   :H = 'Post Date',"+;
                "CWareCode :15   :H = 'Location'"
                &&,"+;
                "lcdesc =IIF(SEEK(CWareCode,'WareHous'),WareHous.cDesc,'')  :35   :H = 'Location Desc'"
  *B603596,1 KHM 06/18/2000 (End)
  
  DIMENSION latemp[1]
  latemp = ''

  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
  *B603596,1                we removed the cBatch.
  *llObjRet = ARIABROW(["S"],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
                    gnbrfsrow2, gnbrfscol2,'','',;
                    'MDINVNTH.cBatch','laTemp')
  llObjRet = ARIABROW(["S"],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
                    gnbrfsrow2, gnbrfscol2,'','',;
                    'MDINVNTH.cLkBatch','laTemp')
  *B603596,1 KHM 06/18/2000 (End)
  
  lcObjVal = latemp[1]
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF

*!*************************************************************
*! Name      : lfWoldVal
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 14/07/1999
*! Purpose   : To return the old value.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWoldVal()
*!*************************************************************
FUNCTION lfWoldVal

laOldVal = EVALUATE(SYS(18))
*!*************************************************************
*! Name      : lfwRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 14/07/1999
*! Purpose   : Option grid when function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRep()
*!*************************************************************
FUNCTION lfwRep

 ldLockDate = gdSysDate

*!*************************************************************
*! Name      : lfClrRed
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 14/07/1999
*! Purpose   : Function to clear read.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrRed()
*!*************************************************************
FUNCTION lfClrRed
CLEAR READ

*!*************************************************************
*! Name      : lfvWareCode
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 14/07/1999
*! Purpose   : Function to WareHouse Code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvWareCode()
*!*************************************************************
FUNCTION lfvWareCode

lcWareHo  = VARREAD()
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  &lcObjName = gfBroWWare(.T.)
  &lcObjName = IIF(EMPTY(&lcObjName),laOldVal,&lcObjName)
ENDIF

*!*************************************************************
*! Name      : lfPrnType
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 14/07/1999
*! Purpose   : Function to print type.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfPrnType()
*!*************************************************************
FUNCTION lfPrnType

DO CASE
  CASE MDINVNTH.TYPE = 'L'
    RETURN('Locked')
  CASE MDINVNTH.TYPE = 'M'
    RETURN('Mark Down')
  CASE MDINVNTH.TYPE = 'P'
    RETURN('Posted')
ENDCASE