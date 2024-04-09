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
*:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[T20100623.0015]
***********************************************************************************************
R_TITLE   = 'Material Inventory Locking Report'
*--- Set necessary relation
loOgScroll.cCRorientation = 'P'
llMulti  = (gfGetMemVar('M_WareHouse') = 'Y')
*--- Start Main Program
IF ldLockDate = {}
  WAIT WINDOW 'Lock date can not be empty...'
  RETURN
ENDIF

*--Dyelot header and data in the FRX will be printed under the flag being .T.
llDyelot   = gfGetMemVar('M_MATDYE')   ='Y'

IF loOGScroll.llOgFltCh
  =lfColData()
ENDIF
SELECT (lcRpTmp)
IF lcRPZero = 'N'
  SET FILTER TO TotStk <> 0
ENDIF

LOCATE 
DO gfDispRe WITH EVAL('lcRpForm')
SELECT (lcRpTmp)

SET DEVICE TO SCREEN 
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
DIMENSION laFileStru[1,18]
AFIELDS(laFileStru)
*:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
*=gfCrtTmp(lcRpTmp,@laFileStru,'cWareCode+cLkBatch+Style+Color+cLocation',lcRpTmp,.T.)
=gfCrtTmp(lcRpTmp,@laFileStru,'cWareCode+cLkBatch+Style+cLocation',lcRpTmp,.T.)
*:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[END]
lcBatchFile = ''
llBatSelect = .F.

lnPosBatch = ASCAN(loOgScroll.laOgFxFlt,"MDINVNTH.CLKBATCH")
IF lnPosBatch > 0 
  lnPosBatch = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosBatch,1)
  lcBatchFile = loOgScroll.laOgFxFlt[lnPosBatch,6]
  IF !EMPTY(lcBatchFile) AND USED(lcBatchFile)
    SELECT(lcBatchFile)
    LOCATE 
    IF !EOF()
      llBatSelect = .T.
    ENDIF 
  ENDIF 
ENDIF       

IF llMultWare
  lcLocFile = ''
  llLocSelect = .F.

  lnPosloc = ASCAN(loOgScroll.laOgFxFlt,"MDINVNTL.CWARECODE")
  IF lnPosloc > 0 
    lnPosloc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosloc,1)
    lcLocFile = loOgScroll.laOgFxFlt[lnPosloc,6]
    IF !EMPTY(lcLocFile) AND USED(lcLocFile)
      SELECT(lcLocFile)
      LOCATE 
      IF !EOF()
        llLocSelect = .T.
      ENDIF 
    ENDIF 
  ENDIF       
ENDI 

IF llBatSelect 
  SELECT(lcBatchFile)
  SCAN 
   SELECT MDINVNTL
   IF gfSeek('M'+&lcBatchFile..CLKBATCH) 
*:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
*!*  	 SCAN REST WHILE cbattype+cLkbatch+style+color = 'M'+&lcBatchFile..CLKBATCH ;
*!*  		       FOR gfseek('M'+&lcBatchFile..CLKBATCH,'MDINVNTH','MDINVNTH') AND  MDINVNTH.DATE = ldLockDate .AND. ;
*!*                 IIF(llMultWare and llLocSelect ,SEEK(MDINVNTL.cWareCode,lcLocFile),.T.) AND ;
*!*                 IIF(gfSEEK('0001'+SUBSTR(STYLE,1,7)+Color,'ITEM','Style') AND Item.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
   SCAN REST WHILE cbattype+cLkbatch+style= 'M'+&lcBatchFile..CLKBATCH ;
           FOR gfseek('M'+&lcBatchFile..CLKBATCH,'MDINVNTH','MDINVNTH') AND  MDINVNTH.DATE = ldLockDate .AND. ;
               IIF(llMultWare and llLocSelect ,SEEK(MDINVNTL.cWareCode,lcLocFile),.T.) AND ;
               IIF(gfSEEK('0001'+STYLE,'ITEM','Style') AND Item.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
*:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[End]
	   SCATTER MEMVAR MEMO
     *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
     *IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style+m.Color,lcRpTmp)
	   IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style,lcRpTmp)
     *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[End]
	     SELECT (lcRpTmp)
         REPLACE TotStk   WITH TotStk+MDINVNTL.TotStk;
     	         OldTotStk   WITH OldTotStk+MDINVNTL.OldTotStk;
                 cLocation   WITH SPACE(01)
	   ELSE
       *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
       * IF SEEK(m.cWareCode+m.cLkBatch+m.Style+m.Color,lcRpTmp) AND EMPTY(cLocation)       
 	     IF SEEK(m.cWareCode+m.cLkBatch+m.Style,lcRpTmp) AND EMPTY(cLocation)
       *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[End]
           REPLACE &lcRpTmp..TotStk    WITH &lcRpTmp..TotStk+MDINVNTL.TotStk;
     	  	       &lcRpTmp..OldTotStk WITH &lcRpTmp..OldTotStk+MDINVNTL.OldTotStk;
             	   &lcRpTmp..cLocation WITH SPACE(01)
  	     ELSE
    	   INSERT INTO (lcRpTmp) FROM MEMVAR    
 	     ENDIF
       ENDIF
     ENDSCAN 	     
   ENDIF 
  ENDSCAN
ELSE
  SELECT MDINVNTL
  IF gfSeek('M')
*:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
*!*       SCAN REST WHILE cbattype+cLkbatch+style+color = 'M' ;
*!*  		       FOR gfseek('M'+MDINVNTL.CLKBATCH,'MDINVNTH','MDINVNTH') AND  MDINVNTH.DATE = ldLockDate .AND. ;
*!*                 IIF(llMultWare and llLocSelect ,SEEK(MDINVNTL.cWareCode,lcLocFile),.T.) AND ;
*!*                 IIF(gfSEEK('0001'+SUBSTR(STYLE,1,7)+Color,'ITEM','Style') AND Item.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
     SCAN REST WHILE cbattype+cLkbatch+style= 'M' ;
           FOR gfseek('M'+MDINVNTL.CLKBATCH,'MDINVNTH','MDINVNTH') AND  MDINVNTH.DATE = ldLockDate .AND. ;
               IIF(llMultWare and llLocSelect ,SEEK(MDINVNTL.cWareCode,lcLocFile),.T.) AND ;
               IIF(gfSEEK('0001'+STYLE,'ITEM','Style') AND Item.cDye_Flg = 'Y',!EMPTY(DyeLot),.T.)
*:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[End]
	   SCATTER MEMVAR MEMO
     *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
     *IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style+m.Color,lcRpTmp)     
	   IF lcRPBin = 'N' AND SEEK(m.cWareCode+m.cLkBatch+m.Style,lcRpTmp)
     *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[End]
	     SELECT (lcRpTmp)
         REPLACE TotStk   WITH TotStk+MDINVNTL.TotStk;
     	         OldTotStk   WITH OldTotStk+MDINVNTL.OldTotStk;
                 cLocation   WITH SPACE(01)
	   ELSE
       *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[Start]
 	     *IF SEEK(m.cWareCode+m.cLkBatch+m.Style+m.Color,lcRpTmp) AND EMPTY(cLocation)
       IF SEEK(m.cWareCode+m.cLkBatch+m.Style,lcRpTmp) AND EMPTY(cLocation)
       *:B609374,1 MMT 08/08/2010 Modify Report Material Inventory Locking TO use the modified indecies of File MDINVNTL[End]
           REPLACE &lcRpTmp..TotStk    WITH &lcRpTmp..TotStk+MDINVNTL.TotStk;
     	  	       &lcRpTmp..OldTotStk WITH &lcRpTmp..OldTotStk+MDINVNTL.OldTotStk;
             	   &lcRpTmp..cLocation WITH SPACE(01)
  	     ELSE
    	   INSERT INTO (lcRpTmp) FROM MEMVAR    
 	     ENDIF
       ENDIF
     ENDSCAN
     
  ENDIF 
ENDIF 
RETURN 


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
*---Erase the temp file
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)


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
ldLockDate = oariaapplication.systemdate
=gfOpenTable(oAriaApplication.DataDir+'MDINVNTL',oAriaApplication.DataDir+'MDINVNTL','SH')
=gfOpenTable(oAriaApplication.DataDir+'MDINVNTH',oAriaApplication.DataDir+'MDINVNTH','SH')
=gfOpenTable('ITEM','style','SH')
=gfOpenTable(oAriaApplication.DataDir+'Warehous',oAriaApplication.DataDir+'Warehous','SH')
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
CLEARREAD()

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



