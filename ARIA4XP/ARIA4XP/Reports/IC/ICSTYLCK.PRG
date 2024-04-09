*****************************************************************************
*: Program file  : ICINVLCK.PRG (N037780)
*: Program desc. : Style Invintory Locking Report.
*: Module        : IC (Inventory Control)
*: 				   System: Aria Apparel System (A4XP)
*: Developer	 : Mariam Mazhar - (MMT) 25/07/2006
*****************************************************************************
*: Calls : FUNCTIONS  : lfColData,lfClearRep,lfvBatch,lfWoldVal,lfwRep
*:                      lfClrRed,lfvWareCode,lfPrnType
*****************************************************************************
*: Passed Parameters  : None
*B609374,1 MMT 08/08/2010 Modify Report Style Inventory Locking TO use the modified indecies of File MDINVNTL[T20100623.0015]
***********************************************************************************************
R_TITLE   = 'Style Inventory Locking Report'
loOgScroll.cCRorientation = 'P'
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

IF loOgScroll.llOGFltCh && OG Filters changed
  =lfCrtTemp()
  =lfColData()
ENDIF
SELECT (lcRpTmp)
IF lcRPZero = 'N'
  SET FILTER TO TotStk <> 0
ENDIF

LOCATE 
DO gfDispRe WITH EVAL('lcRpForm')
SELECT (lcRpTmp)
SET RELATION TO
SET DEVICE TO SCREEN 
*--- End Main

*!*************************************************************
*! Name      : lfColData
*! Developer : Mariam Mazhar- (MMT)
*! Date      : 25/07/2006
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

llUseBatch = .F.
llUseLoc   = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'MDINVNTH.CLKBATCH'),1)
IF lnPosition > 0
  lcBatFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseBatch= IIF(!EMPTY(lcBatFile) .AND. USED(lcBatFile) .AND. RECCOUNT(lcBatFile)>0,.T.,.F.)
ENDIF
IF llUseBatch 
  SELECT(lcBatFile)
  LOCATE 
  IF EOF()
    llUseBatch = .F.
  ENDIF 
ENDIF 


lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'MDINVNTL.CWARECODE'),1)
IF lnPosition > 0
  lcLocFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseLoc  = IIF(!EMPTY(lcLocFile) .AND. USED(lcLocFile) .AND. RECCOUNT(lcLocFile)>0,.T.,.F.)
ENDIF
IF llUseLoc 
  SELECT(lcLocFile)
  LOCATE 
  IF EOF()
	llUseLoc = .F.
  ENDIF 
ENDIF 

IF llUseBatch
  SELECT(lcBatFile)
  SCAN 
    IF loDBFMDINVNTL.Seek('S'+&lcBatFile..CLKBATCH)
      SELECT MDINVNTL
*B609374,1 MMT 08/08/2010 Modify Report Style Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
*!*        SCAN REST WHILE CBATTYPE+CLKBATCH+STYLE+COLOR+DYELOT+CLOCATION = 'S'+&lcBatFile..CLKBATCH  FOR  loDBFMDINVNTH.Seek('S' + MDINVNTL.CLKBATCH) .AND. ;
*!*             		MDINVNTH.DATE = ldLockDate .AND. ;
*!*                  IIF(llUseLoc ,SEEK(MDINVNTL.cWareCode,lcLocFile),.T.) AND ;
*!*                  IIF(loDBFStyle.SEEK (MDINVNTL.Style,'Style') AND Style.cDye_Flg = 'Y',!EMPTY(MDINVNTL.DyeLot),.T.)
      SCAN REST WHILE CBATTYPE+CLKBATCH+STYLE+DYELOT+CLOCATION = 'S'+&lcBatFile..CLKBATCH  FOR  loDBFMDINVNTH.Seek('S' + MDINVNTL.CLKBATCH) .AND. ;
               MDINVNTH.DATE = ldLockDate .AND. ;
                IIF(llUseLoc ,SEEK(MDINVNTL.cWareCode,lcLocFile),.T.) AND ;
                IIF(loDBFStyle.SEEK (MDINVNTL.Style,'Style') AND Style.cDye_Flg = 'Y',!EMPTY(MDINVNTL.DyeLot),.T.)
*B609374,1 MMT 08/08/2010 Modify Report Style Inventory Locking TO use the modified indecies of File MDINVNTL[End]
                
        SCATTER MEMVAR MEMO
		IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style,lcRpTmp)
		  SELECT (lcRpTmp)
		  REPLACE Stk1      WITH Stk1+MDINVNTL.Stk1;
        		  Stk2      WITH Stk2+MDINVNTL.Stk2;
		          Stk3      WITH Stk3+MDINVNTL.Stk3;
        		  Stk4      WITH Stk4+MDINVNTL.Stk4;
		          Stk5      WITH Stk5+MDINVNTL.Stk5;
        	      Stk6      WITH Stk6+MDINVNTL.Stk6;
                  Stk7      WITH Stk7+MDINVNTL.Stk7;
                  TotStk    WITH TotStk+MDINVNTL.TotStk;
                  OldStk1   WITH OldStk1+MDINVNTL.OldStk1;
                  OldStk2   WITH OldStk2+MDINVNTL.OldStk2;
                  OldStk3   WITH OldStk3+MDINVNTL.OldStk3;
                  OldStk4   WITH OldStk4+MDINVNTL.OldStk4;
                  OldStk5   WITH OldStk5+MDINVNTL.OldStk5;
	              OldStk6   WITH OldStk6+MDINVNTL.OldStk6;
	              OldStk7   WITH OldStk7+MDINVNTL.OldStk7;
	              OldTotStk WITH OldTotStk+MDINVNTL.OldTotStk;
	              cLocation WITH SPACE(01)
		ELSE
		  INSERT INTO (lcRpTmp) FROM MEMVAR 
		ENDIF
      ENDSCAN 
    ENDIF 
  ENDSCAN 
ELSE
  SELECT MDINVNTL
   SCAN FOR IIF(llUseLoc,SEEK(MDINVNTL.cWareCode,lcLocFile),.T.) AND ;
		    loDBFMDINVNTH.Seek('S' + MDINVNTL.CLKBATCH) .AND. MDINVNTH.DATE = ldLockDate .AND. ;
            IIF(loDBFStyle.SEEK(MDINVNTL.Style,'Style') AND (Style.cDye_Flg = 'Y'),!EMPTY(MDINVNTL.DyeLot),.T.)
                
        SCATTER MEMVAR MEMO
		IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style,lcRpTmp)
		  SELECT (lcRpTmp)
		  REPLACE Stk1        WITH Stk1+MDINVNTL.Stk1;
        		  Stk2        WITH Stk2+MDINVNTL.Stk2;
		          Stk3        WITH Stk3+MDINVNTL.Stk3;
        		  Stk4        WITH Stk4+MDINVNTL.Stk4;
		          Stk5        WITH Stk5+MDINVNTL.Stk5;
        	      Stk6        WITH Stk6+MDINVNTL.Stk6;
                  Stk7        WITH Stk7+MDINVNTL.Stk7;
                  TotStk  	  WITH TotStk+MDINVNTL.TotStk;
                  OldStk1     WITH OldStk1+MDINVNTL.OldStk1;
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
ENDIF 


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mariam Mazhar- (MMT)
*! Date      : 25/07/2006
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
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
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
*! Developer : MAriam Mazhar - (MMT)
*! Date      : 25/07/2006
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

 ldLockDate    = oAriaApplication.SystemDate
 loDBFStyle    = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
 loDBFScale    = CreateObject("RemoteTable","Scale","Scale",'Scale',SET("DATASESSION"))&&,"",.T.)
 loDBFWareHous = CreateObject("RemoteTable","WAREHOUS","WAREHOUS",'WAREHOUS',SET("DATASESSION"))&&,"",.T.)
 loDBFMDINVNTH = CreateObject("RemoteTable","MDINVNTH","MDINVNTH",'MDINVNTH',SET("DATASESSION"))&&,"",.T.)
 loDBFMDINVNTL = CreateObject("RemoteTable","MDINVNTL","MDINVNTL",'MDINVNTL',SET("DATASESSION"))&&,"",.T.)
*!*************************************************************
*! Name      : lfClrRed
*! Developer : Mariam Mazhar- (MMT)
*! Date      : 25/07/2006
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
CLEARREAD()

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

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar- (MMT)
*! Date      : 25/07/2006
*! Purpose   : Function to create temp. file
*!*************************************************************
FUNCTION lfCrtTemp
IF USED(lcRpTmp)
  USE IN (lcRpTmp)
ENDIF 
SELECT MDINVNTL
DIMENSION laTempStru[1,18]
laTempStru = ''
lnAllFlds = AFIELDS(laTempStru)
*B609374,1 MMT 08/08/2010 Modify Report Style Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
* = gfCrtTmp(lcRpTmp,@laTempStru,"cWareCode+cLkBatch+Style+Color+cLocation" ,lcRpTmp,.T.)
 = gfCrtTmp(lcRpTmp,@laTempStru,"cWareCode+cLkBatch+Style+cLocation" ,lcRpTmp,.T.)
*B609374,1 MMT 08/08/2010 Modify Report Style Inventory Locking TO use the modified indecies of File MDINVNTL[End]
