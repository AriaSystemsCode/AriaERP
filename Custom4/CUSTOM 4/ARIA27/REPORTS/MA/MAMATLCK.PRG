**************************************************************************
*: Program file  : MAMATLCK.PRG (E301271,1)
*: Program desc. : Material Invintory Locking Report.
*: Module        : MA (Material)
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls : FUNCTIONS  : lfColData,lfClearRep,lfvBatch,lfWoldVal,lfwRep
*:                      lfClrRed,lfvWareCode,lfPrnType
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:B803244,1 ADEL 4/4/2000 Make the Totals fields work to show report totals not just last line.
*:B803244,1               All above modifications are done in MAMATLCK.FRX.
*:B803244,1               Add 'DYELOT' column to the frx and fix the bug of duplicating records
*:B803244,1               when using dyelots and bins.
*:B603596,1 KHM 06/18/2000 Changing the field cBatch to be cLkBatch due to the changes
*:B603596,1                that were done in the files.
***********************************************************************************************
R_TITLE   = 'Material Inventory Locking Report'

*--- Set necessary relation

llMulti  = (gfGetMemVar('M_WareHouse') = 'Y')
*--- Start Main Program
IF ldLockDate = {}
  WAIT WINDOW 'Lock date can not be empty...'
  RETURN
ENDIF
*B803244,1 (Begin) Set the dyelot flag.
*--Dyelot header and data in the FRX will be printed under the flag being .T.
llDyelot   = gfGetMemVar('M_MATDYE')   ='Y'
*B803244,1 (End)
IF llOgFltCh
  =lfColData()
ENDIF
SELECT (lcRpTmp)
IF lcRPZero = 'N'
  SET FILTER TO TotStk <> 0
ENDIF

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
*B603596,1                we removed the cBatch from the file and add cLkBatch instead.
*SET RELATION TO 'M'+CBATCH INTO MDINVNTH
SET RELATION TO 'M'+CLKBATCH INTO MDINVNTH
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
*SET RELATION TO 'M'+CBATCH INTO MDINVNTH
SET RELATION TO 'M'+CLKBATCH INTO MDINVNTH
*B603596,1 KHM 06/18/2000 (End)

SELECT MDINVNTL
=SEEK('M'+IIF(EMPTY(lcRpBatch),'',lcRpBatch))
*B803244,1 (Begin) If the fabric has delots don't get the header record of delots too.
*SCAN REST WHILE cbattype+cbatch+style+color = ;
                IIF(!EMPTY(lcRpBatch),'M' + lcRpBatch ,'M');
          FOR   MDINVNTH.DATE = ldLockDate .AND. ;
                IIF(!EMPTY(lcRpWareH),cWareCode=lcRpWareH,.T.)

*B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
*B603596,1                we removed the cBatch.
*SCAN REST WHILE cbattype+cbatch+style+color = ;
                IIF(!EMPTY(lcRpBatch),'M' + lcRpBatch ,'M');
          FOR   MDINVNTH.DATE = ldLockDate .AND. ;
                IIF(!EMPTY(lcRpWareH),cWareCode=lcRpWareH,.T.) AND ;
                IIF(SEEK(SUBSTR(STYLE,1,7)+Color,'Fabric') AND Fabric.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
SCAN REST WHILE cbattype+cLkbatch+style+color = ;
                IIF(!EMPTY(lcRpBatch),'M' + lcRpBatch ,'M');
          FOR   MDINVNTH.DATE = ldLockDate .AND. ;
                IIF(!EMPTY(lcRpWareH),cWareCode=lcRpWareH,.T.) AND ;
                IIF(SEEK(SUBSTR(STYLE,1,7)+Color,'Fabric') AND Fabric.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
*B603596,1 KHM 06/18/2000 (End)

*B803244,1 (End)
  SCAT MEMVAR MEMO

  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
  *B603596,1                we removed the cBatch.
  *IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cBatch+m.Style+m.Color,lcRpTmp)
  IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style+m.Color,lcRpTmp)
  *B603596,1 KHM 06/18/2000 (End)
  
    SELECT (lcRpTmp)
    REPLACE TotStk   WITH TotStk+MDINVNTL.TotStk;
            OldTotStk   WITH OldTotStk+MDINVNTL.OldTotStk;
            cLocation   WITH SPACE(01)
  ELSE
    *B803244,1 (Begin) Remark the following line and if no bins print just on totla line for each fabric/color.
    *INSERT INTO (lcRpTmp) FROM MEMVAR 
    
    *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
    *B603596,1                we removed the cBatch.    
    *IF SEEK(m.cWareCode+m.cBatch+m.Style+m.Color,lcRpTmp) AND EMPTY(cLocation)
    IF SEEK(m.cWareCode+m.cLkBatch+m.Style+m.Color,lcRpTmp) AND EMPTY(cLocation)
    *B603596,1 KHM 06/18/2000 (End)
    
      REPLACE &lcRpTmp..TotStk    WITH &lcRpTmp..TotStk+MDINVNTL.TotStk;
              &lcRpTmp..OldTotStk WITH &lcRpTmp..OldTotStk+MDINVNTL.OldTotStk;
              &lcRpTmp..cLocation WITH SPACE(01)
    ELSE
      INSERT INTO (lcRpTmp) FROM MEMVAR    
    ENDIF
    *B803244,1 (End)
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
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK("M"+lcObjVal , 'MDINVNTH'))
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
  
  lcBrFields  = "cLkBatch    :12   :H = 'Batch No.',"+;
                "lctmp = SUBSTR(Content,1,60) :60 :H = 'Desc.',"+;
                "Date      :12   :H = 'Lock Date',"+;
                "DPOSTDATE :12   :H = 'Post Date',"+;
                "CWareCode :15   :H = 'Location'"
  *B603596,1 KHM 06/18/2000 (End)
  
  DIMENSION latemp[1]
  latemp = ''

  *B603596,1 KHM 06/18/2000 (Begin) Changing the cBatch to be cLkBatch because
  *B603596,1                we removed the cBatch.  
  *llObjRet = ARIABROW(["M"],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
                    gnbrfsrow2, gnbrfscol2,'','',;
                    'MDINVNTH.cBatch','laTemp')
  llObjRet = ARIABROW(["M"],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
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



