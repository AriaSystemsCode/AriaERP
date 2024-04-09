*:************************************************************************
*:           File: ICTRNIN.prg                                          :*
*:         System: ARIA APPAREL SYSTEM 2.7                              :*
*:         Module: Inventory Control                                    :*
*:        Program: Physical inventory Transfer.                         :*
*:         Author: Timour A. Khalil                                     :*
*:           Date: 10/16/00                                             :* 
*:************************************************************************

IF !gfSetup()
  RETURN
ENDIF

lcPhyPath = ''
IF ! FILE(gcDataDir+'PHY_COMP.MEM')
  WAIT WINDOW 'Physical Inventory Data Directory should be selected, Connot Proceed!'
  RETURN
ELSE
  RESTORE FROM gcDataDir+'PHY_COMP.MEM' ADDI
ENDIF

lcCOFrom   = SUBSTR(lcPhyPath,LEN(lcPhyPath)-2,2)
lcCOTo     = gcAct_Comp
ldDate   = {}


SELECT ALL cWarecode FROM WAREHOUS INTO ARRAY laWare
lnWare = IIF(ALEN(laWare)>1,2,1)

DO (gcScrDir+gcWinAppl+"\ICTRNIN.SPX")


RETURN


**************************************************************************
FUNCTION lfvTrans


*--Variable needed to save.
lcType = 'P'
lcCostMth= gfGetMemVar('M_Cost_Meth')
llGlLink = (gfGetMemVar('M_Link_GL') ='Y')
llDyelot = (gfGetMemVar('M_Dyelot')  ='Y')
llMultiWH= (gfGetMemVar('M_WareHouse')='Y')
llWareLoc= .F.

DIMENSION laOpFile[3]
laOpFile = .F.
ldPstDate = gdSysDate


*--Prepere the temp file.
SELECT INVTADJ
*-- Create tmp adjustment file.
lcTmpAdj = gfTempName()
=AFIELDS(laFStru)
lnNo1=ASCAN(laFStru,'UNT_COST')
lnNo2=ASCAN(laFStru,'OLD_COST')
*--Make the lenth of this two fields as ave_cost field.
STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+2,4]
laFStru[lnFStru+1,1] = 'cAdjReason'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 6
laFStru[lnFStru+1,4] = 0
laFStru[lnFStru+2,1] = 'cRefer'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 6
laFStru[lnFStru+2,4] = 0
CREATE DBF (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru
lcExpr=IIF(llDyelot,'Style+Dyelot+STR(RECNO(),6)','Style+STR(RECNO(),6)')
INDEX ON &lcExpr TAG &lcTmpAdj
SET RELATION TO Style INTO STYLE


WAIT WINDOW 'Start collecting styles...' NOWAIT
SELECT (lcTmpAdj)
APPEND FROM (lcPhyPath+'INVTADJ.DBF') FOR Date >= ldDate AND Type='P'
GO TOP
IF EOF()
  WAIT WINDOW 'No Pysical inventory was done after this date, in company '+ lcCOFrom
  RETURN
ENDIF

*--Call saving of inventory.
llAskPrint = 'N'
DO lpSavScr IN (gcapphome+'IC\ICINVTA.PRG')


*--Update location bin.
SELECT 0
USE (lcPhyPath+'WHSLOC.DBF') ALIAS 'AWHSLOC'
SELECT AWHSLOC
SCAN FOR cWareCode = PADR(laWare[lnWare],6) AND !EMPTY(Style) AND !EMPTY(cLocation)
  IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
     WAIT WINDOW Style NOWAIT
     SCATTER MEMVAR
     INSERT INTO WhsLoc FROM MEMVAR
  ENDIF
ENDSCAN
USE IN AWHSLOC


WAIT WINDOW 'THE TRANSFER COMPLETED!' TIMEOUT 3
CLEAR READ
RETURN
