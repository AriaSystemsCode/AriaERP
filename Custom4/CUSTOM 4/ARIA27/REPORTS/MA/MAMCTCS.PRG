*:****************************************************************
*: Program file  : MAMCTCS.PRG
*: Program desc. : Print Material Manufacturing Order Cost Sheet.
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : ABDOU ELGENDI - (ABD) Due to N#000396,1
*: Date          : 05/24/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfCreatemp , lfwOGWhen , lfSeTSRep
*:               :            : lfGetTit   , lfCollect , lfItmPos
*:               :            : lfAddFltr  , lfvStatus , lfGtCstSht
*:               :            : lfGtCstQty , lfAvgPsYld
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#000396,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcTime     == > Variable to hold the Time.
*-- lcCompName == > Variable to hold the Company Name.
*-- lcRepTitel == > Variable to Hold Report Titel 1.
*-- lcRpTitel  == > Variable to Hold Report Titel 2.
*-- lcCtcsFile == > Cursor Hold the Selected MMFO
*-- llUsDyelot == > Variable to Hold true in case use dyelot.
*-- lcLong1    == > Fabric   long title type 1.
*-- lcLong2    == > Trim     long title type 2.
*-- lcLong3    == > Mfg.Oper long title type 3.
*-- lcLong4    == > Misc.    long title type 4.
*-- lnPieces   == > Variable Hold the total Pieces.
*-- lnAvg_Yld  == > Variable hold the Average Yield
*-- lnAvg_Pcs  == > Variable hold the Average Pcs.
*-- lcPrv_Typ  == > Prv Type in the ctbom file.
*-- lcmfgOrdno == > Variable hold the Mfg Order Number.


STORE SPACE(30) TO lcLong1 , lcLong2 , lcLong3, lcLong4
STORE 0         TO lnPieces,lnExt_Cst,lnUntCst,lnQty,lnAvg_Yld,lnAvg_Pcs
STORE '' TO lcPrv_Typ
lcTime     = TIME() 
lcCompName = gcCom_Name
lcRepTitel = lcMMOTitl + ' Cost Sheet Report'

*-- End Declaration variables.

llUsDyelot = gfGetMemvar('M_MATDYE',gcAct_Comp)  ='Y'

*-- Getting the cost labels from the memory variables.
DIMENSION laSetup[4,2]
laSetUp[1,1] = 'M_CTSLBL1'
laSetUp[2,1] = 'M_CTSLBL2'
laSetUp[3,1] = 'M_CTSLBL3'
laSetUp[4,1] = 'M_CTSLBL4'

=gfGetMemVar(@laSetUp,gcAct_Comp)

lcLong1 = laSetUp[1,2]
lcLong2 = laSetUp[2,2]
lcLong3 = laSetUp[3,2]
lcLong4 = laSetUp[4,2]

IF llOGFltCh
 
 *-- Create Temp Files.
 = lfCreatemp ()

 *--  Add Some filter to filter Exp. 
 = lfAddFltr ()

 *-- Collecting Code...
 = lfCollect ()
  
ENDIF

SELECT (lcTmpCtcs)
SET RELATION TO CFabric+Color INTO Fabric ADDITIVE


DO gfDispRe WITH EVAL('lcRpName')  && Call report layout..

SELECT (lcTmpCtcs)
SET RELATION TO
SET DEVICE TO SCREEN

RETURN

*-- End Of Code.
*:****************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
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


SELECT mmfgordh
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+5,4]

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cMFType'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Typ'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0


lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Color'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Dyelot'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'MFgCode'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

Create Table (gcWorkDir+lcTmpCtcs) FROM ARRAY laFileStru
INDEX ON cmfgordno+cMFType+Typ+CFabric+Color+Dyelot TAG (lcTmpCtcs)


*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfwOGWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
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
*: Name      : lfSeTSRep 
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : Go top in mfo Order file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetSRep()
*:*************************************************************
*:
FUNCTION lfSetSRep
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT MMFGORDH
   GO TOP
ENDCASE

*-- End of lfSetSRep.
*:*************************************************************
*: Name      : lfGetTit
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
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

lcMMOTitl = ALLTRIM(gfGetMemvar('M_MMOLNLBL',gcAct_Comp))

RETURN lcMMOTitl
*-- End OF lfGetTit
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/16/2002
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
lcCtcsFile   = ''

*-- Get the Position.
lnctcsPos    = lfItmPos("MMFGORDH.CMFGORDNO")
IF lnctcsPos > 0 
  lcCtcsFile = laOGFxFlt[lnctcsPos,6]
  llWorkFile = !EMPTY(lcCtcsFile) .AND. USED(lcCtcsFile) .AND. RECCOUNT(lcCtcsFile) > 0
ENDIF

*-- In case use Cursor , Make Relation With MMFGORDH.
IF llWorkFile
  SELECT(lcCtcsFile)
  SET RELATION TO CmfgOrdno INTO MMFGORDH ADDITIVE
  LOCATE 
ELSE
 SELECT MMFGORDH
ENDIF

*-- Scan For the selected Mfg Order.
SCAN FOR &lcRpExp
  SELECT MMFGORDH
  SCATTER MEMVAR MEMO
  SELECT (lcTmpCtcs)
  APPEND BLANK
  GATHER MEMVAR MEMO

  *-- Update Field Color With First Color Found In the Fabric File.
  = SEEK(cfabric,'FABRIC')
  
  REPLACE cMFType WITH '1'         ,;
          Color   WITH Fabric.Color
  
  *-- Scan For the Cost Item.
  IF SEEK('T'+CmfgOrdno,'CTKTBOM')
    lcmfgOrdno = CmfgOrdno
    *-- Function to get the Cost Sheet Item.
    = lfGtCstSht ()
  ENDIF
  
ENDSCAN


IF llWorkFile
  SELECT(lcCtcsFile)
  SET RELATION TO 
ENDIF

*-- End OF lfCollect
*:*************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Called from : MAMCTCS.prg
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
*: Name      : lfAddFltr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
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


IF EMPTY(lcRpExp)
  lcRpExp=' Mmfgordh.NmmgBudget <> 0'
ELSE
  lcRpExp = '('+lcRpExp +')' + ' .AND. Mmfgordh.NmmgBudget <> 0'
ENDIF

 *--  Enter the status in the lcRpExp
 
IF lcStatus <> "L"
   lcRpExp = '(' +lcRpExp+ ") .AND. (Mmfgordh.Status = '" + lcStatus +  "')"
ENDIF

*-- End OF lfAddFltr.
*:*************************************************************
*: Name      : lfvStatus
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : Return the Status to the FRX
*:*************************************************************
*: Called from : MAMCTCS.FRX
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Status Titel .
*:*************************************************************
*: Example     : = lfvStatus()
*:*************************************************************
*
FUNCTION lfvStatus
PRIVATE lcParam
lnPieces = 0

Do CASE
  CASE Status = 'O'
    lcParam   = 'Open'
    lnPieces  = NmmgBudget - ( Received + Canceled + Damaged)
  CASE Status = 'H'
    lcParam   = 'Hold'
    lnPieces  = Nmmfgopen
  CASE Status = 'C'
    lcParam   = 'Complete'
    lnPieces  = Received    
  CASE Status = 'X'
    lcParam   = 'Canceled'
    
  CASE Status = 'A'
    lcParam   = 'Actualized'
    lnPieces  = Pcs_Act
  CASE Status = 'S'
    lcParam   = 'Closed'
    lnPieces  = Received 
ENDCASE

RETURN lcParam

*-- End OF lfvStatus.
*:*************************************************************
*: Name      : lfGtCstSht
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : Get the Cost Sheet line.
*:*************************************************************
*: Called from : MAMCTCS.FRX
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Status Titel .
*:*************************************************************
*: Example     : = lfvStatus()
*:*************************************************************
*
FUNCTION lfGtCstSht
PRIVATE lnAlias , lcOldCtg , OldTyp 
Store '' To lcOldCtg , OldTyp , lcPrv_Typ
lnAlias = SELECT (0)

SELECT CTKTBOM
SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = ;
  'T'+lcmfgOrdno
  

  IF IIF(lcOldCtg='M',OldTyp <> TYP,lcPrv_Typ <> cCatgTyp)
    lcPrv_Typ  = cCatgTyp
    lcOldCtg   = cCatgTyp
    OldTyp     = TYP
  ENDIF
  
  
  *-- Function to get the unit Cost and Qty.
  = lfGtCstQty ()
  *-- Function to get the Avg Yield Avg Pcs.
  = lfAvgPsYld ()

  SELECT (lcTmpCtcs)
  APPEND BLANK
  GATHER MEMVAR MEMO

  REPLACE cFabric    WITH LEFT(CTKTBOM.ITEM,7),;
          cMFType    WITH '2'                 ,;
          Typ        WITH CTKTBOM.Typ         ,;
          Color      WITH CTKTBOM.ICLR        ,;
          Dyelot     WITH CTKTBOM.Dyelot      ,;
          mfgCode    WITH CTKTBOM.MFGCODE     ,;
          nest_cost1 WITH lnUntCst            ,;
          nest_cost2 WITH lnQty               ,;
          nest_cost3 WITH lnExt_Cst           ,;
          nest_cost4 WITH lnAvg_Yld           ,;
          nlan_cost1 WITH lnAvg_Pcs
ENDSCAN

SELECT (lnAlias)
*-- End OF lfGtCstSht
*:*************************************************************
*: Name      : lfGtCstQty
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : get the unit cost and qty.
*:*************************************************************
*: Called from : MAMCTCS.PRG
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfGtCstQty()
*:*************************************************************
*
FUNCTION lfGtCstQty


Alias  = SELECT (0)

IF lcRpCostMt  = 'A'
  STORE 0 TO lnQty,lnUntCst,lnFound
  IF SEEK('T'+lcmfgOrdno,'BomCost')
    SELECT BomCost
    SCAN WHILE cImtyp+cTkTno+Actualize+cBomtype+Item+iclr+mFgCode+cOprCode+cLotNo+cIsession+cRsession  = 'T'+lcmfgOrdno ;
      FOR CBOMTYPE+ITEM+ICLR+cDyelot+MFGCODE = CTKTBOM.TYP+CTKTBOM.ITEM+CTKTBOM.ICLR+CTKTBOM.Dyelot+CTKTBOM.MFGCODE
        lnUntCst = lnUntCst + BomCost.nUnitACst * BomCost.nTotQty
        lnQty    = lnQty    + BomCost.nTotQty
        lnFound  = lnFound + 1
    ENDSCAN
  ENDIF            
  IF lnFound = 0
    lnUntCst  = CTKTBOM.UntCost
  ELSE
    lnUntCst = lnUntCst / lnQty
  ENDIF
ELSE
  lnUntCst  = CTKTBOM.UntCost
  lnQty     = CTKTBOM.Req_Qty
ENDIF

lnExt_Cst = ROUND(lnUntCst*lnQty,2)

SELECT (Alias) 

*-- End OF lfGtCstQty.
*:*************************************************************
*: Name      : lfAvgPsYld
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : get the Avg Yield Avg Pcs.
*:*************************************************************
*: Called from : MAMCTCS.PRG
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfAvgPsYld()
*:*************************************************************
*
FUNCTION lfAvgPsYld
PRIVATE lnAlias , lnBudjet ,lnReciev,lnDamje,lnCancel,lnPieces
STORE 0 TO lnBudjet,lnReciev,lnDamje,lnCancel,lnPieces

lnAlias  = SELECT (0)

*--intialise variables :-
*--lnBudjet-----> budjet qty  when TranCd = '1'
*--lnReciev-----> Recived qty when TranCd = '2' 
*--lnDamje -----> Damaje qty  when TranCd = '3'
*--lnCancel-----> Cancel Qty  when TranCd = '4'

SELECT BOMLINE
IF SEEK('T'+'1'+CTKTBOM.CutTkt)
  IF lcPrv_Typ = 'M'
    lcExpr = [REST FOR CtktNo = CTKTBOM.CutTkt AND mfgCode = CTKTBOM.mfgCode .AND. ]+;
             [cType = '1' .AND. cBomTyp = cTktBom.Typ]
  ELSE
    lcExpr = [REST FOR CtktNo = CTKTBOM.CutTkt .AND. ]+;
             [ITEM = CTKTBOM.ITEM AND IClr = CTKTBOM.IClr .AND. ]+;
             [cType = '1' .AND. cCatgTyp = CTKTBOM.cCatgTyp .AND. ]+;
             [cBomTyp = cTktBom.Typ]
  ENDIF
  SCAN &lcExpr
    SELECT mmFgOrdd
    =SEEK(CtktBom.CutTkt+LEFT(BomLine.STYLE,7)+BomLine.sClr)
    SCAN WHILE cmFgordno+cFabric+Color+Dyelot+Trancd = CTKTBOM.CutTkt + LEFT(BomLine.STYLE,7) + BomLine.sClr
      lnBudjet = lnBudjet + IIF(TranCd = '1',nmfgtotqty,0)
      lnReciev = lnReciev + IIF(TranCd = '2',nmfgtotqty,0)
      lnDamje  = lnDamje  + IIF(TranCd = '3',nmfgtotqty,0)
      lnCancel = lnCancel + IIF(TranCd = '4',nmfgtotqty,0)
    ENDSCAN
  ENDSCAN
ENDIF

*--determin the devided qty depend on the cuttkt status
lnPieces = 0
DO CASE
  CASE &lcTmpCtcs..Status='O'			&& the mfg Order status is 'OPEN'
    lnPieces = lnBudjet - ( lnCancel + lnReciev + lnDamje)
  CASE &lcTmpCtcs..Status $ 'CS'		&& the mfg Order status is 'Complete','CLOSE','ACTUALISE'
    lnPieces = lnReciev
  CASE &lcTmpCtcs..Status = 'A'			&& the mfg Order status is 'ON HOLD'
    lnPieces = lnBudjet 
ENDCASE          

*-- Avg Pcs.
lnAvg_Pcs = IIF(lnPieces>0,ROUND(lnExt_Cst/lnPieces,2),0)

*-- Avg Yield.
lnAvg_Yld = IIF(lnPieces<>0,lnQty/lnPieces,0)

SELECT (lnAlias)

*-- End OF lfAvgPsYld.
*:*************************************************************
