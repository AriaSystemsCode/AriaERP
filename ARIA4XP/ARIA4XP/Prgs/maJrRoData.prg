*:**************************************************************************
*: Program file  : MAJrRoData.prg
*: Program desc. : Material Roll Control Function
*:        System : ARIA APPAREL SYSTEM 4.0
*:        Module : Material (MA).
*:     Developer : Mariam Mazhar(MMT)
*:          Date : 03/18/2007
*:     Reference : N000601 - T20070206.0007 
*:**************************************************************************
*: Passed Parameters  :
*: lcTrType : Tarnsacation type
*:                     '1' for Adjustment
*:                     '2' for Physical inventory
*:                     '3' for Invoice
*:                     '4' for Void invoice
*:                     '5' for Receive Cutting ticket,PO,Dye order,Inter PO
*:                     '6' for Issue Inter PO,Adornment order,return PO
*:                     '7' for Void credit memo
*:                     '8' for Inventory markdown
*:                     '9' for Issue style/fabric component
*:        lcInvType => Inventory type ('0002' for fabric).
*:          lcStyle => Style/fabric.
*:       lcWareCode => Warehouse Code.
*:         lcDyelot => Dyelot ,pass empty if not a dyelot fabric or system
*:                     dyelot No, (if you want to add this dyelot you have to 
*:                     add it before calling this function).
*:         ldTrDate => Transaction Date.
*:       ldPostDate => Posting Date.
*:      laAdjQty[9] => Issued or received Stock as an array per sizes and total.
*:         lcToWare => Translate to Warehouse Code.
*:      lcTmpJour,lcFullRoll,lcTmpRoll:Roll cursors 
*:**************************************************************************
*Modifications:
*: N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[T20060908.0003]
*: B608417,1 WAM 02/03/2008 Fix adding issue records in ITEMJRNL table when costing method is FIFO
*: B608782,1 WAM 01/05/2009 Fix bug when issue the same material/color more than once in the same session table 
*: B608782,1 WAM 01/05/2009 when costing method is FIFO [T20081118.0007]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[T20090303.0017]
*! B608847,1 MMT 04/14/2009 Fix bug of repeating records of rolls [T20090304.0016]
*: B608782,2 MMT 07/13/2009 Fix bug of not Updating ItemJrnl while adjusting More than one Fabric in same session[T20090528.0037]
*: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[T20110823.0017]
*! C201512,1 SAB 09/20/2012 Add flag to show or not to show the rolls screen [T20120625.0018]
*:**************************************************************************
*! C201512,1 SAB 09/20/2012 Add flag to show or not to show the rolls screen [Start]
*PARAMETERS lcTrType,lcInvType,lcStyle,lcDyelot,lcWareCode,lcToWare,laAdjQty,lcTmpJour,lcFullRoll,lcTmpRoll,ldTrDate,ldPostDate
PARAMETERS lcTrType,lcInvType,lcStyle,lcDyelot,lcWareCode,lcToWare,laAdjQty,lcTmpJour,lcFullRoll,lcTmpRoll,ldTrDate,ldPostDate, llDntShwRlsScr
*! C201512,1 SAB 09/20/2012 Add flag to show or not to show the rolls screen [End]


*! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
lcStyle = PADR(lcStyle,19)
*! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]

lcOldWArAls   = ALIAS()

llTrkRolls = IIF(lcInvType='0001',.F.,(ALLTRIM(gfGetMemVar('M_TrkRolls')) = 'Y'))
DIMENSION laIndex[1,3]
laIndex = ''
laIndex[1,1] = 'CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)'
laIndex[1,2] = 'StyInvJl'

IF TYPE('lcCalProg') # 'C'
  lcCalProg = SPACE(0)
ENDIF

IF TYPE('lcTrCode') # 'C'
  lcTrCode = SPACE(6)
ENDIF

IF TYPE('lnLineNo') # 'N'
  lnLineNo = 0
ENDIF  

PRIVATE lcRelCode
IF TYPE('lcRelCode') # 'C'
  lcRelCode = SPACE(6)
ENDIF  


lcDyelot   = IIF(EMPTY(lcDyelot),SPACE(10),lcDyelot)
llDyeLvl   = !EMPTY(lcDyelot)
llWareHous = gfGetMemVar('M_WareHouse')='Y'
llExtCall = .T.

lcitmpic  = gfItemMask("HI",'', lcInvType)
lenclrlen  = LEN(gfitemmask("PN", "", lcInvType))
lnmajlength = LEN(gfItemMask('PM','', lcInvType))
lcSepar   = SUBSTR(lcitmpic,lnmajlength +1,1)
lcFabric  =  PADR(SUBSTR(lcStyle,1,lnmajlength),7) 
lcColor   =  RIGHT(lcStyle,lenclrlen)
lcFDyelot = lcDyelot
lnAdjStk  = laAdjQty[9]

PRIVATE lcItemKeyV, lcWareCodV
lcItemKeyV = lcStyle
lcWareCodV = lcWareCode


**
IF lcTrType <> '8'
**
  IF !gfOpnSqlFl('itemjrnl',"itemjrnl","cinvtype='"+lcInvType+"' and style = ?m.lcItemKeyV " +;
               " AND cwarecode = ?m.lcWareCodV ",@laIndex,"styinvjl")
    llRetvalue = .F.           
    RETURN .F.
  ENDIF

  STORE .T. TO llItmWarDy,llItmWar,llItm

  *-- Open the ITEM and ITEMLOC files.
  DIMENSION laIndex[1,3]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
  laIndex[1,2] = 'StyDye'

  IF !gfOpnSqlFl('itemloc',"itemloc","cinvtype='"+lcInvType+"' and style = ?m.lcItemKeyV",@laIndex,"stydye")
    STORE .F. TO llItmWarDy,llItmWar
  ENDIF


  laIndex[1,1] = 'cInvType+Style'
  laIndex[1,2] = 'Style'

  IF !gfOpnSqlFl('item',"item","cinvtype='"+lcInvType+"' and style= ?m.lcItemKeyV ",@laIndex,"Style")
    llItm = .F.
  ENDIF
**
ELSE
  lcTmpJour = gfTempName() 
  lcFullRoll= gfTempName()
  lcTmpRoll = gfTempName()
ENDIF
**


IF EMPTY(lcRelCode)
  IF lcTrType $ '56'
    lcRelCode = item.cConvBuy
  ENDIF
  
  IF lcTrType $ '34'
    lcRelCode = item.cConvSell
  ENDIF
ENDIF

PRIVATE lnConv
lnConv = 1

IF !EMPTY(lcRelCode)
  =gfGetUOMData(lcRelCode, '', '', @lnConv)
ENDIF



SELECT itemloc
=SEEK(lcInvType+lcStyle+lcWareCode+SPACE(10))


STORE "" TO lcCstMthVar,lcCostMeth
lcCstMthVar = IIF(lcInvType='0001','M_Cost_Meth','M_MatCstMth')
lcCostMeth  = gfGetMemVar(lcCstMthVar)

DIMENSION laOldstk[9]
IF lcTrType = '8'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
  
  IF lcTrType $ '12'
    SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
    IF lcTrType = '2'
      FOR lnI = 1 TO 9
        laOldStk[lnI] = -laOldStk[lnI]
      ENDFOR
    ENDIF
  ENDIF
ENDIF


IF lcCostMeth $ 'FIL' AND lcTrType <> '9'&& AND !(lcTrType $ '3456' AND laAdjQty[9] < 0 )
  IF !lfJrRoData()
    SELECT (lcOldWArAls)
    llRetvalue = .F.           
    RETURN (0)
  ELSE
    IF !EMPTY(lcToWare)
      PRIVATE lcOrgWare
      lcOrgWare = lcWareCode
      lcWareCode = lcToWare
      FOR lnI = 1 TO 9
        laAdjQty[lnI] = ABS(laAdjQty[lnI])
      ENDFOR
      lnAdjStk = laAdjQty[9]
      lcIRType = 'R'
      IF !lfJrRoData()
        SELECT (lcOldWArAls)
        llRetvalue = .F.           
        RETURN (0)
      ENDIF
      lcWareCode = lcOrgWare
      FOR lnI = 1 TO 9
        laAdjQty[lnI] = - laAdjQty[lnI]
      ENDFOR
      lnAdjStk = laAdjQty[9]
      lcIRType = 'I'
    ENDIF
  ENDIF
ENDIF  

SELECT (lcOldWArAls)

*!*************************************************************
*! Name      : lfJrRoData
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/17/2007
*! Purpose   : This function will get the journal records or rolls
*!             records.
*!  Note     : This function is called in case it is issuing 
*!             and costing method is "FIL"
*!                           OR 
*!             it is issuing or receiving rolls
*!*************************************************************
FUNCTION lfJrRoData


IF !USED('ROLLS')
  =gfOpenTable(oAriaApplication.DataDir+'ROLLS',oAriaApplication.DataDir+'ROLLS','SH')
ENDIF 
llGenRolId   = ('MA' $ oAriaApplication.CompanyInstalledModules AND ALLTRIM(gfGetMemVar('M_GENROLID')) = 'N')
IF laAdjQty[9] < 0 OR lcTrType $ '28'
  PRIVATE lcItemJour,lcUseFile,llCollcData
  

  *B608782,1 WAM 01/05/2009 load the data from ITEMJRNL for each record in the issue session
*!*	  IF USED(lcTmpJour)
*!*	    llCollcData = !SEEK(lcInvType+lcStyle+lcWareCode+lcDyelot,lcTmpJour)
*!*	    lcItemJour = gfTempName()
*!*	    lcUseFile = lcItemJour
*!*	  ELSE
*!*	    llCollcData = .T.
*!*	    lcItemJour = gfTempName()
*!*	    lcUseFile = lcTmpJour
*!*	  ENDIF

*: B608782,2 MMT 07/13/2009 Fix bug of not Updating ItemJrnl while adjusting More than one Fabric in same session[Start]
*!*	    llCollcData = .T.
*!*	    lcUseFile = lcTmpJour
  IF USED(lcTmpJour)
    llCollcData = !SEEK(lcInvType+lcStyle+lcWareCode+lcDyelot,lcTmpJour)
    lcItemJour = gfTempName()
    lcUseFile = lcItemJour
  ELSE
    llCollcData = .T.
    lcItemJour = gfTempName()
    lcUseFile = lcTmpJour
  ENDIF
*: B608782,2 MMT 07/13/2009 Fix bug of not Updating ItemJrnl while adjusting More than one Fabric in same session[End]    

  *B608782,1 WAM 01/05/2009 (End)
  IF llCollcData 
    LOCAL lnEngineBehavior
    lnEngineBehavior = SET("EngineBehavior")
    SET ENGINEBEHAVIOR 70
    IF lcTrType $ '34'
      SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
             cTrCode,cTrType,dTrDate,dPostDate,nCost*lnConv As 'nCost',nUntCstBuy,;
             SUM(nStk1) / lnConv AS 'nBal1',SUM(nStk2) / lnConv AS 'nBal2',;
             SUM(nStk3) / lnConv AS 'nBal3',SUM(nStk4) / lnConv AS 'nBal4',;
             SUM(nStk5) / lnConv AS 'nBal5',SUM(nStk6) / lnConv AS 'nBal6',;
             SUM(nStk7) / lnConv AS 'nBal7',SUM(nStk8) / lnConv AS 'nBal8',;
             SUM(nTotStk) / lnConv AS 'nBalance',;
             SUM(IIF(CIRTYPE='R',nTotStk,0))/lnConv AS 'nReceived',;
             SUM(IIF(CIRTYPE='I',ABS(nTotStk),0))/lnConv AS 'nIssued',;
             SUM(nTotStk) / lnConv AS 'nBalance',;
             00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
             00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
             00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
             00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
             00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
             "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
      FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
      ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
	ELSE
      IF lcTrType $ '56'
        SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
               cTrCode,cTrType,dTrDate,dPostDate,nCost*lnConv As 'nCost',nUntCstBuy,;
               SUM(nStk1) / lnConv AS 'nBal1',SUM(nStk2) / lnConv AS 'nBal2',;
               SUM(nStk3) / lnConv AS 'nBal3',SUM(nStk4) / lnConv AS 'nBal4',;
               SUM(nStk5) / lnConv AS 'nBal5',SUM(nStk6) / lnConv AS 'nBal6',;
               SUM(nStk7) / lnConv AS 'nBal7',SUM(nStk8) / lnConv AS 'nBal8',;
               SUM(nTotStk) / lnConv AS 'nBalance',;
               SUM(IIF(CIRTYPE='R',nTotStk,0))/lnConv AS 'nReceived',;
               SUM(IIF(CIRTYPE='I',ABS(nTotStk),0))/lnConv AS 'nIssued',;
               00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
               00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
               00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
               00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
               00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
        FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
        ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
  	  ELSE
        SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
               cTrCode,cTrType,dTrDate,dPostDate,nCost As 'nCost',nUntCstBuy,;
               SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',;
               SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',;
               SUM(nStk5) AS 'nBal5',SUM(nStk6) AS 'nBal6',;
               SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8',;
               SUM(nTotStk) AS 'nBalance',;
               SUM(IIF(CIRTYPE='R',nTotStk,0))/lnConv AS 'nReceived',;
               SUM(IIF(CIRTYPE='I',ABS(nTotStk),0))/lnConv AS 'nIssued',;
               00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
               00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
               00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
               00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
               00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
               "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
        FROM   itemjrnl WHERE  cDyelot =lcDyelot GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
        ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcUseFile)
      ENDIF
    ENDIF
    SET ENGINEBEHAVIOR lnEngineBehavior

    *B608782,1 WAM 01/05/2009 load the data from ITEMJRNL for each record in the issue session
*!*	    IF (lcUseFile = lcItemJour)
*!*	      SELECT (lcTmpJour)
*!*	      APPEND FROM (oAriaApplication.workdir+lcItemJour+'.DBF')
*!*	    ENDIF 
    *B608782,1 WAM 01/05/2009 (End)
    
*: B608782,2 MMT 07/13/2009 Fix bug of not Updating ItemJrnl while adjusting More than one Fabric in same session[Start]
    IF (lcUseFile = lcItemJour)
      SELECT (lcTmpJour)
      APPEND FROM (oAriaApplication.workdir+lcItemJour+'.DBF')
    ENDIF 
*: B608782,2 MMT 07/13/2009 Fix bug of not Updating ItemJrnl while adjusting More than one Fabric in same session[End]    

  ENDIF     
  SELECT (lcTmpJour)
  IF !(TAG() == lcTmpJour)
    IF lcCostMeth $ 'FL'  &&FIFO OR LOT
      INDEX ON cInvType+Style+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
    ELSE
      INDEX ON cInvType+Style+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
    ENDIF
  ENDIF
  GO TOP
  IF EOF() AND lcTrType  <> '2' AND laAdjQty[9] < 0
    *--No open receiving exist for material/color XXXX/yyyyy ,
    *--This transaction line will be ignored.
    lcMsgExp = ALLTRIM(gfItemMask("HI","",lcInvType)) +" "+ ALLTRIM(lcStyle)
    =gfModalGen('TRM42261B00036','DIALOG',lcMsgExp)
    USE
    llRetvalue = .F.           
    RETURN .F.
  ENDIF
  IF lcTrType # '2' .AND. lcCostMeth $ "FI"  &&FIFO OR LIFO
    DECLARE laRcvdQty[9]
    STORE 0 TO laRcvdQty
    *B608417,1 WAM 02/03/2008 Proccess only records for selected item location
    *SCAN WHILE laRcvdQty[9] <> ABS(laAdjQty[9]) OR EOF()
    =SEEK(lcInvType+lcStyle+lcWareCode+lcDyelot)
    SCAN REST WHILE cInvType+Style+cWareCode+cDyelot+cRSession+cISession =;
                    lcInvType+lcStyle+lcWareCode+lcDyelot AND laRcvdQty[9] <> ABS(laAdjQty[9]) OR EOF()
    *B608417,1 WAM 02/03/2008 (End)
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        IF laRcvdQty[lnI] < ABS(laAdjQty[lnI])
          lnUpdate = MIN(EVALUATE('nBal'+lcI) , ABS(laAdjQty[lnI]+laRcvdQty[lnI]))
          *: B608782,2 MMT 07/13/2009 Fix bug of not Updating ItemJrnl while adjusting More than one Fabric in same session[Start]
*!*	          REPLACE ('nApply'+lcI) WITH EVALUATE('nApply'+lcI) + lnUpdate,;
*!*	                  nApply         WITH nApply + lnUpdate ,;
*!*	                  lNeeded        WITH .T.
          REPLACE ('nApply'+lcI) WITH lnUpdate,;
                  nApply         WITH lnUpdate ,;
                  lNeeded        WITH .T.
          *: B608782,2 MMT 07/13/2009 Fix bug of not Updating ItemJrnl while adjusting More than one Fabric in same session[End]
                  
          laRcvdQty[lnI] = laRcvdQty[lnI] + lnUpdate
          *B608417,1 WAM 02/03/2008 Fix adding issue records in ITEMJRNL table when costing method is FIFO
          *laRcvdQty[9]   = laRcvdQty + lnUpdate
          laRcvdQty[9]   = laRcvdQty[9] + lnUpdate
          *B608417,1 WAM 02/03/2008 (End)

        ENDIF
      ENDFOR
    ENDSCAN
    IF laRcvdQty[9] < ABS(laAdjQty[9])
      *--The receiving quantity are not covered the issued quantity
      *--for material/color XXXX , This transaction line will be ignored.
      lcMsgExp = ALLTRIM(gfItemMask("HI","",lcInvType)) +" "+ ALLTRIM(lcStyle)
      =gfModalGen('TRM42262B00036','DIALOG',lcMsgExp)
      USE
      llRetvalue = .F.           
      RETURN .F. 
    ENDIF
  ELSE
    *-- it is lot
    *call lot rol SCREEN only if trans. type <> 2
    IF (lcTrType # '2' OR (llTrkRolls .AND. item.ltrkrolls)) AND !lfLotRolScr()
      llRetvalue = .F.           
      RETURN .F.
    ENDIF
	ENDIF 
ELSE
  IF llTrkRolls .AND. item.ltrkrolls
    IF !lfLotRolScr()
      llRetvalue = .F.           
      RETURN .F.
    ENDIF
  ENDIF
ENDIF   

IF laAdjQty[9] < 0
  SELECT(lcTmpJour)
  *B608417,1 WAM 02/03/2008 Proccess only records for selected item location
  =SEEK(lcInvType+lcStyle+lcWareCode+lcDyelot)
  *B608417,1 WAM 02/03/2008 Proccess only records for selected item location
  SCAN WHILE cInvType+Style+cWareCode+cDyelot+cRSession+cISession =;
             lcInvType+lcStyle+lcWareCode+lcDyelot
    IF lNeeded
      REPLACE dTrDate   WITH ldTrDate  ,;
              dPostDate WITH ldPostDate,;
              cTrType   WITH lcTrType  ,;
              cTrCode   WITH lcTrCode  ,;
              nApply1   WITH nApply1,;
              nApply2   WITH nApply2,;
              nApply3   WITH nApply3,;
              nApply4   WITH nApply4,;
              nApply5   WITH nApply5,;
              nApply6   WITH nApply6,;
              nApply7   WITH nApply7,;
              nApply8   WITH nApply8,;
              nApply    WITH nApply
    ELSE
      DELETE
    ENDIF
  ENDSCAN
ENDIF
IF llTrkRolls .AND. item.ltrkrolls
  SELECT(lcTmpRoll)
  PRIVATE lcTag
  lcTag = ORDER()
  SET ORDER TO lcTmpRoll2
  *-- Retuern the Pointer to first record match the correct Expr.

*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	  = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6))
*!*	  SCAN WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
*!*	             lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)
  = SEEK(lcStyle+lcWareCode+lcFDyelot+STR(lnLineNo,6))
  SCAN WHILE Style+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcSTyle+lcWareCode+lcFDyelot+STR(lnLineNo,6)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  
    IF lNeeded
      *--Change it to Issue transactions,to use it in updating master Journal file.

      REPLACE dTranDate WITH ldTrDate  ,;
              dPostDate WITH ldPostDate,;
              cTranType WITH lcTrType  ,;
              cTran     WITH lcTrCode  ,;
              nApply    WITH nApply
    ELSE
      DELETE
    ENDIF
  ENDSCAN
  SET ORDER TO (lcTag) IN (lcTmpRoll)
  
  IF laAdjQty[9] < 0
    SELECT(lcFullRoll)
    PRIVATE lcTag
    lcTag = ORDER()
    SET ORDER TO 'lcFullRol2'
    *-- Retuern the Pointer to first record match the correct Expr.    
    
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	    = SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6))
*!*	    SCAN WHILE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
*!*	               lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)
    = SEEK(lcStyle+lcWareCode+lcFDyelot+STR(lnLineNo,6))
    SCAN WHILE Style+cWareCode+cDyelot+STR(LineNo,6) = ;
               lcStyle+lcWareCode+lcFDyelot+STR(lnLineNo,6)
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

      IF lNeeded
        *--Change it to Issue transactions,to use it in updating master Journal file.
        REPLACE dTranDate WITH ldTrDate  ,;
                dPostDate WITH ldPostDate,;
                cTranType WITH lcTrType  ,;
                cTran     WITH lcTrCode  ,;
                nApply    WITH nApply
      ELSE
        DELETE
      ENDIF
    ENDSCAN
    *-- Return Order To the Old Order.
    SET ORDER TO &lcTag
   
  ENDIF  
ENDIF

RETURN .T.
*!*************************************************************
*! Name      : lfLotRolScr()
*! Developer : Mariam Mazhar[MMT]
*! Date      : 03/17/2007
*! Purpose   : Call lot or roll screen
*!*************************************************************
*! Example   : =lfLotRolScr()
*!*************************************************************
FUNCTION lfLotRolScr




PRIVATE lcChck,lcUnChck,lnTotApply,lnUsrApply,lcRNewSta,lcRRemSta,lcRModSta,llRetVal,;
        lnOldVal,lnNewRec,lcFileToUse,llBrowse,lcSerchExp,lcFab,lcClr,lcWare,lcDye

lcFab     = lcFabric
lcClr     = lcColor
lcWare    = lcWareCode
lcDye     = lcFDyelot


STORE SPACE(0) To lcSerchExp
llBrowse = .F.

IF llTrkRolls .AND. ITEM.ltrkrolls
  *-- "!llExtCall" If it's called from the function (gfMatCrl) OR 
  *-- "!USED(lcTmpRoll" it's called from receiving material manufacturing order OR
  *-- "lcTrType = '1' AND lnAdjStk < 0" it's called from issue Material PO return

  *IF (llExtCall AND (!USED(lcTmpRoll) OR (lcTrType $ '15' AND lnAdjStk < 0)) )  OR !llExtCall
  
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *IF ((!USED(lcTmpRoll) OR (USED(lcTmpRoll) AND !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)) OR(lcTrType $ '3456' AND laAdjQty[9] < 0)) ) &&6

  *! B608847,1 MMT 04/14/2009 Fix bug of repeating records of rolls [Start]
  *IF ((!USED(lcTmpRoll) OR (USED(lcTmpRoll) AND !SEEK(lcStyle+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)) OR(lcTrType $ '3456' AND laAdjQty[9] < 0)) ) &&6
  IF ((!USED(lcTmpRoll) OR (USED(lcTmpRoll) AND !SEEK(lcStyle+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll, 'lcTmpRoll2')) OR (lcTrType $ '3456' AND laAdjQty[9] < 0)) ) 
  *! B608847,1 MMT 04/14/2009 Fix bug of repeating records of rolls [End]
  
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  
    IF EMPTY(lcToWare) OR lcToWare # lcWareCode
      = lfTmpRoll()
    ENDIF
  ENDIF
ENDIF

lcFileToUse = IIF(llTrkRolls .AND. ITEM.ltrkrolls,lcTmpRoll,lcTmpJour)

lnNewRec = RECNO(lcFileToUse)
llRetVal = .T.
STORE 0 TO lnOldVal

STORE IIF(lcTrType='2',laOldStk[9],0) TO lnUsrApply

STORE laAdjQty[9] TO lnTotApply


*! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
IF llTrkRolls .AND. ITEM.ltrkrolls
  SELECT (lcFileToUse)
  RECALL FOR !EMPTY(Style) AND !lNeeded AND Style+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcStyle+lcWareCode+lcFDyelot
  LOCATE 
  SELECT(lcFullRoll)
  RECALL FOR Style+cWareCode+cDyelot+STR(LineNo,6) = ;
               lcStyle+lcWareCode+lcFDyelot AND !EMPTY(Style) AND !lNeeded 
  LOCATE              
  SELECT (lcFileToUse)               
ENDIF   
*! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]



*!*	IF TYPE('llExtCall') = 'L' AND llExtCall
  DIMENSION laUsrApply[1]
  laUsrApply = 0
  *Check if the case is roll then add (lineno to where condition[Start]
  IF llTrkRolls .AND. ITEM.ltrkrolls
    SELECT (lcFileToUse)
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
   * LOCATE FOR cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) =   lcFab+lcClr+lcWare+lcDye+STR(lnLineNo,6)    
    LOCATE FOR Style+cWareCode+cDyelot+STR(LineNo,6) =   lcStyle+lcWare+lcDye+STR(lnLineNo,6)
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
    IF FOUND()
      
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
            lcFab+lcClr+lcWare+lcDye+STR(lnLineNo,6) ;
      INTO ARRAY laUsrApply

      SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE Style+cWareCode+cDyelot+STR(LineNo,6) = ;
            lcStyle+lcWare+lcDye+STR(lnLineNo,6) ;
      INTO ARRAY laUsrApply
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
      
    ENDIF   
  ELSE
    SELECT (lcFileToUse)
    LOCATE FOR cInvType+Style+cWareCode+cDyelot = lcInvType+lcStyle+lcWareCode+lcDyelot 
    IF FOUND()
      SELECT SUM(nApply) FROM (lcFileToUse) ;
      WHERE cInvType+Style+cWareCode+cDyelot = ;
            lcInvType+lcStyle+lcWareCode+lcDyelot ;
      INTO ARRAY laUsrApply
    ENDIF  
  ENDIF
  STORE laUsrApply * IIF(lnAdjStk>=0,1,-1) TO lnUsrApply
*!*	ENDIF

*! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[T20090303.0017]
  IF lcTrType ='1'  AND lnAdjStk>=0 AND llTrkRolls .AND. ITEM.ltrkrolls
    DIMENSION laAppLied[1]
    laAppLied = 0
    SELECT SUM(nApply - nbalance)FROM (lcFileToUse) ;
	                WHERE Style+cWareCode+cDyelot+STR(LineNo,6) = ;
            lcStyle+lcWare+lcDye+STR(lnLineNo,6) INTO ARRAY laAppLied
  
    IF laAppLied = laAdjQty[9] 
      STORE laUsrApply  TO lnTotApply
    ELSE 
      STORE laUsrApply + laAdjQty[9] TO lnTotApply
    ENDIF 
    STORE laUsrApply  TO lnUsrApply
  ENDIF 
*! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]



lcLotRo  = gfTempName()
lcLotRo1 = gfTempName()
lcLotRo2 = gfTempName()
lcLotRo3 = gfTempName()

*MMT
llShowRol = .F.
*MMT
DO CASE
  CASE laAdjQty[9] < 0
    IF llTrkRolls .AND. ITEM.ltrkrolls
      llShowRol = .T.
      STORE 0 TO lnCurRolBa,lnAppRolQt
    ELSE
      STORE 0 TO lnCurLotBa,lnAppLotQt,lnNewLotBa
      llShowRol = .F.
    ENDIF
  CASE laAdjQty[9] >= 0 AND llTrkRolls .AND. ITEM.ltrkrolls
    llShowRol = .T.
    STORE 0 TO lnCurRolBa,lnAppRolQt
ENDCASE

IF (llTrkRolls .AND. ITEM.ltrkrolls) OR  lcCostMeth = 'L' 
  *! C201512,1 SAB 09/20/2012 Add flag to show or not to show the rolls screen [Start]
  *DO FORM (oAriaApplication.ScreenHome +'MALotRo.scx')
  IF !llDntShwRlsScr
    DO FORM (oAriaApplication.ScreenHome +'MALotRo.scx')
  ENDIF
  *! C201512,1 SAB 09/20/2012 Add flag to show or not to show the rolls screen [End]
ENDIF 


laAdjQty[1] = lnAdjStk
laAdjQty[9] = lnAdjStk

SELECT(lcFileToUse)
SET FILTER TO
LOCAL lnTotRoll
IF (laAdjQty[9] < 0 ) AND llTrkRolls .AND. ITEM.ltrkrolls
  SELECT (lcTmpRoll)
  PRIVATE lcTag
  lcTag = ORDER()
  SET ORDER TO lcTmpRoll2
  
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Srart]
*!*	  SCAN FOR cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = ;
*!*	             lcFabric+lcColor+lcWareCode+lcFDyelot
  SCAN FOR Style+cWareCode+cDyelot+STR(LineNo,6) = ;
             lcStyle+lcWareCode+lcFDyelot
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  
    lnTotRoll = nApply
    SELECT (lcFullRoll)
    
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	    IF SEEK(&lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
*!*	            &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot,lcFullRoll,lcFullRoll)
*!*	      SCAN REST WHILE lnTotRoll <> 0 AND ;
*!*	                      cRollID+cFabric+cColor+cWareCode+cDyelot = ;
*!*	                      &lcTmpRoll..cRollID+&lcTmpRoll..cFabric+&lcTmpRoll..cColor+;
*!*	                      &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot
    IF SEEK(&lcTmpRoll..cRollID+&lcTmpRoll..Style+;
            &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot,lcFullRoll,lcFullRoll)
      SCAN REST WHILE lnTotRoll <> 0 AND ;
                      cRollID+Style+cWareCode+cDyelot = ;
                      &lcTmpRoll..cRollID+&lcTmpRoll..Style+;
                      &lcTmpRoll..cWareCode+&lcTmpRoll..cDyelot
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
     
        REPLACE nApply    WITH IIF(nBalance<lnTotRoll,nBalance,lnTotRoll),;
                RolTranCd WITH IIF(lnAdjStk<0,'2',&lcTmpRoll..RolTranCd) ,;
                lNeeded   WITH !EMPTY(nApply)
        lnTotRoll = lnTotRoll - &lcFullRoll..nApply
      ENDSCAN
    ENDIF
  ENDSCAN
  
  
  
  SET ORDER TO (lcTag) IN (lcTmpRoll)
  PRIVATE lnOldAlais , lcOldRlOdr , lnOldRecNR , lnOldRecNJ , lcScanExpr
  *-- Save Old Alias
  lnOldAlais = SELECT (0)
  *-- Save Old Pointer For the FullRoll File.
  SELECT (lcFullRoll)
  lcOldRlOdr = ORDER()
  SET ORDER TO lcFullRoll
  lnOldRecNR = RECNO()
  *-- Save Old Pointer For the TmpJour File.
  SELECT (lcTmpJour)
  lnOldRecNJ = RECNO()
  LOCATE
  SCAN
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    * lcScanExpr =padr(substr(Style,1,lnmajlength),7)+RIGHT(Style,lenclrlen)+cWareCode+cDyelot
    lcScanExpr =Style+cWareCode+cDyelot
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
    *-- I will update the nApply Qty With 0 to Void duplicated 
    *-- value if we issue the same fabric color in the same session.
    REPLACE nApply WITH 0
    *: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[T20110823.0017][Start]
    REPLACE nApply1 WITH 0
    *: B609791,1 MMT 01/03/2012 Fix bug of nstk1 <> ntotstk in itemjrnl table after -ve adj.[T20110823.0017][END]
    *-- Scan On the Full Roll File and get the total Qty fro this session.
    IF SEEK(lcScanExpr,lcFullRoll,'lcFullRol2')
    
      SELECT (lcFullRoll)
       *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
      lcOldOrder = ORDER()
      SET ORDER TO 'lcFullRol2'
       *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]
       
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [STart]
      *SCAN REST WHILE cfabric+ccolor+cwarecode+cdyelot+STR(lineno,6) = ;
          lcScanExpr  FOR cRSession = &lcTmpJour..cRSession  ;
          .AND. nApply > 0
      SCAN REST WHILE Style+cwarecode+cdyelot+STR(lineno,6) = ;
          lcScanExpr  FOR cRSession = &lcTmpJour..cRSession  ;
          .AND. nApply > 0
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
        SELECT (lcTmpJour)
         REPLACE nApply  WITH nApply + &lcFullRoll..nApply,;
                 lNeeded WITH !EMPTY(nApply)
                 
		 *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
         REPLACE nApply1 WITH nApply1+ &lcFullRoll..nApply
         *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]
         
        SELECT (lcFullRoll)
      ENDSCAN
       *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
      SELECT (lcFullRoll)
      SET ORDER TO (lcOldOrder)
       *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]
    ENDIF
    SELECT (lcTmpJour)  


  ENDSCAN
  SELECT (lcTmpJour)
 
  
  
  IF BETWEEN(lnOldRecNJ,1,RECCOUNT()) 
    GOTO lnOldRecNJ
  ENDIF
  
  SELECT (lcFullRoll)
  SET ORDER TO &lcOldRlOdr
  IF BETWEEN(lnOldRecNR,1,RECCOUNT()) 
    GOTO lnOldRecNR
  ENDIF

  SELECT (lnOldAlais)
ENDIF

RETURN llRetVal
*!*************************************************************
*! Name      : lfTmpRoll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 03/17/2007
*! Purpose   : Create rolls temp. files
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTmpRoll()
*!*************************************************************
FUNCTION lfTmpRoll

IF !USED(lcTmpRoll)
  DIMENSION laTags[3,3]
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *laTags[1,1]='cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession'  
  laTags[1,1]='cRollID+Style+cWareCode+cDyelot+cRsession'
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  laTags[1,2]=lcTmpRoll
  IF lcCalProg = 'ARMINV'
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *laTags[2,1]='cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)+cMorder'
    laTags[2,1]='Style+cWareCode+cDyelot+STR(LineNo,6)+cMorder'
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
    laTags[2,2]='lcTmpRoll2'
  ELSE
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *laTags[2,1]='cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)'
    laTags[2,1]='Style+cWareCode+cDyelot+STR(LineNo,6)'
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
    laTags[2,2]='lcTmpRoll2'
  ENDIF
  
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *  laTags[3,1]='cRsession+cFabric+cColor+cWareCode+cDyelot'
  laTags[3,1]='cRsession+Style+cWareCode+cDyelot'
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  
  laTags[3,2]='lcTmpRoll3'

  IF lcCalProg = 'ARMINV'
  
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *DIMENSION laFileStru[25,4]
    DIMENSION laFileStru[26,4]
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
  ELSE
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *DIMENSION laFileStru[23,4]    
    DIMENSION laFileStru[24,4]
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
  ENDIF
  
  
  laFileStru[1 ,1] = 'cRollId'
  laFileStru[1 ,2] = 'C'
  laFileStru[1 ,3] = 20
  laFileStru[1 ,4] = 0

  laFileStru[2 ,1] = 'cTrn_Seq'
  laFileStru[2 ,2] = 'C'
  laFileStru[2 ,3] = 6
  laFileStru[2 ,4] = 0

  laFileStru[3 ,1] = 'cFabric'
  laFileStru[3 ,2] = 'C'
  laFileStru[3 ,3] = 7
  laFileStru[3 ,4] = 0

  laFileStru[4 ,1] = 'cColor'
  laFileStru[4 ,2] = 'C'
  laFileStru[4 ,3] = 6
  laFileStru[4 ,4] = 0

  laFileStru[5 ,1] = 'cWareCode'
  laFileStru[5 ,2] = 'C'
  laFileStru[5 ,3] = 6
  laFileStru[5 ,4] = 0

  laFileStru[6 ,1] = 'cDyelot'
  laFileStru[6 ,2] = 'C'
  laFileStru[6 ,3] = 10
  laFileStru[6 ,4] = 0

  laFileStru[7 ,1] = 'cRSession'
  laFileStru[7 ,2] = 'C'
  laFileStru[7 ,3] = 6
  laFileStru[7 ,4] = 0

  laFileStru[8 ,1] = 'cISession'
  laFileStru[8 ,2] = 'C'
  laFileStru[8 ,3] = 6
  laFileStru[8 ,4] = 0

  laFileStru[9 ,1] = 'cTran'
  laFileStru[9 ,2] = 'C'
  laFileStru[9 ,3] = 6
  laFileStru[9 ,4] = 0

  laFileStru[10,1] = 'cTranType'
  laFileStru[10,2] = 'C'
  laFileStru[10,3] = 1
  laFileStru[10,4] = 0

  laFileStru[11,1] = 'dTranDate'
  laFileStru[11,2] = 'D'
  laFileStru[11,3] = 8
  laFileStru[11,4] = 0

  laFileStru[12,1] = 'dPostDate'
  laFileStru[12,2] = 'D'
  laFileStru[12,3] = 8
  laFileStru[12,4] = 0

  laFileStru[13,1] = 'nCost'
  laFileStru[13,2] = 'N'
  laFileStru[13,3] = 9
  laFileStru[13,4] = 3

  laFileStru[14,1] = 'nUntCstBuy'
  laFileStru[14,2] = 'N'
  laFileStru[14,3] = 9
  laFileStru[14,4] = 3

  laFileStru[15,1] = 'nBalance'
  laFileStru[15,2] = 'N'
  laFileStru[15,3] = 12
  laFileStru[15,4] = 3

  laFileStru[16,1] = 'nReceived'
  laFileStru[16,2] = 'N'
  laFileStru[16,3] = 12
  laFileStru[16,4] = 3

  laFileStru[17,1] = 'nIssued'
  laFileStru[17,2] = 'N'
  laFileStru[17,3] = 12
  laFileStru[17,4] = 3

  laFileStru[18,1] = 'nApply'
  laFileStru[18,2] = 'N'
  laFileStru[18,3] = 12
  laFileStru[18,4] = 3

  laFileStru[19,1] = 'cMarker'
  laFileStru[19,2] = 'C'
  laFileStru[19,3] = 1
  laFileStru[19,4] = 0

  laFileStru[20,1] = 'lStatus'
  laFileStru[20,2] = 'C'
  laFileStru[20,3] = 1
  laFileStru[20,4] = 0

  laFileStru[21,1] = 'lNeeded'
  laFileStru[21,2] = 'L'
  laFileStru[21,3] = 0
  laFileStru[21,4] = 0

  laFileStru[22,1] = 'RolTranCd'
  laFileStru[22,2] = 'C'
  laFileStru[22,3] = 1
  laFileStru[22,4] = 0

  laFileStru[23,1] = 'LineNo'
  laFileStru[23,2] = 'N'
  laFileStru[23,3] = 6
  laFileStru[23,4] = 0

  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  laFileStru[24,1] = 'Style'
  laFileStru[24,2] = 'C'
  laFileStru[24,3] = 19
  laFileStru[24,4] = 0
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

  IF lcCalProg = 'ARMINV'
  
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
*!*	    laFileStru[24,1] = 'nIssue'
*!*	    laFileStru[24,2] = 'N'
*!*	    laFileStru[24,3] = 6
*!*	    laFileStru[24,4] = 3

*!*	    laFileStru[25,1] = 'cMorder'
*!*	    laFileStru[25,2] = 'C'
*!*	    laFileStru[25,3] = 6
*!*	    laFileStru[25,4] = 0
    laFileStru[25,1] = 'nIssue'
    laFileStru[25,2] = 'N'
    laFileStru[25,3] = 6
    laFileStru[25,4] = 3

    laFileStru[26,1] = 'cMorder'
    laFileStru[26,2] = 'C'
    laFileStru[26,3] = 6
    laFileStru[26,4] = 0
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

  ENDIF

  =gfCrtTmp(lcTmpRoll ,@laFileStru,@laTags)

ENDIF

*: N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[Start]
*IF lcTrType$'123456'&&6
IF lcTrType$'1234568'
*: N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[End]

  IF !USED(lcFullRoll)
    DIMENSION laTagArr[2,2]
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *laTagArr[1,1] = 'cRollID+cFabric+cColor+cWareCode+cDyelot+cRsession'
    laTagArr[1,1] = 'cRollID+Style+cWareCode+cDyelot+cRsession'
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
    laTagArr[1,2] = lcFullRoll
    
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *laTagArr[2,1] = 'cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6)'
    laTagArr[2,1] = 'Style+cWareCode+cDyelot+STR(LineNo,6)'
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    
    laTagArr[2,2] = 'lcFullRol2'
    =gfCrtTmp(lcFullRoll ,@laFileStru,@laTagArr)
  ENDIF
  
  SELECT Rolls
  lcRolTag = ORDER()
  
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *SET ORDER TO RollItem
  =gfSetOrder("RollItem")
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
  
  *--crollitem+color+cwarecode+dyelot+crollid+trancd+crsession

  
  SELECT Rolls
  *: N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[Start]
  *IF (lcTrType $ '3456' AND lnAdjStk < 0) OR (lcTrType $ '12')&&6
  IF (lcTrType $ '3456' AND lnAdjStk < 0) OR (lcTrType $ '128')
  *: N000548,1 MMT 08/30/2007 fix bug of wrong rolls in case of locking[End]
    PRIVATE lcTag,llColldata
    lcTag = ORDER(lcTmpRoll)
    SET ORDER TO lcTmpRoll2 IN (lcTmpRoll)
  

    IF lcCalProg = 'ARMINV'
      
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      * llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6)+lcTrCode,lcTmpRoll)
      llColldata = !SEEK(lcStyle+lcWareCode+lcFDyelot+STR(lnLineNo,6)+lcTrCode,lcTmpRoll)
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [END]
      
    ELSE
    
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *llColldata = !SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)      
      llColldata = !SEEK(lcStyle+lcWareCode+lcFDyelot+STR(lnLineNo,6),lcTmpRoll)
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
      
    ENDIF

    SET ORDER TO (lcTag) IN (lcTmpRoll)
    IF llColldata
   
      SELECT Rolls
      
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
      *IF SEEK(lcFabric+lcColor+lcWareCode+lcFDyelot)
      IF gfSEEK(lcStyle+lcWareCode+lcFDyelot)
      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
      
        lcSkipRoll = SPACE(20)
        
        *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
        *SCAN WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession =;
                   lcFabric +lcColor+lcWareCode+lcFDyelot 
                   
        SCAN WHILE Style+cwarecode+dyelot+crollid+trancd+crsession =;
                   lcStyle+lcWareCode+lcFDyelot 
		*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
		
          IF TRANCD = '1' .AND. NQTYBAL = 0
            lcSkipRoll = CROLLID
          ENDIF
          
          *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
          IF TRANCD = '1' .AND. NQTYBAL <> 0 AND CROLLID = lcSkipRoll
            lcSkipRoll = SPACE(20)
          ENDIF 
          *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]
          
          IF CROLLID = lcSkipRoll
            LOOP
          ENDIF
          
          SELECT ITEMJRNL
          
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
          *LOCATE FOR Style+CWARECODE+CDYELOT+CRSESSION+CISESSION = PADR(Rolls.cRollItem,lnmajlength)+lcSepar+PADR(Rolls.Color,lenclrlen)+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession+Rolls.cISession
          *IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot,lcTmpRoll)
          LOCATE FOR Style+CWARECODE+CDYELOT+CRSESSION+CISESSION = Rolls.Style+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession+Rolls.cISession
          IF !SEEK(Rolls.cRollID+Rolls.Style+Rolls.cWareCode+Rolls.Dyelot,lcTmpRoll)
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          
            IF Rolls.TranCd = '1'
              SELECT (lcTmpRoll)
              APPEND BLANK
              *MMT              
              *REPLACE cTrn_Seq   WITH ''                           ,;
                      cRollId    WITH Rolls.cRollID                ,;
                      cFabric    WITH Rolls.cRollItem              ,;
                      cColor     WITH Rolls.Color                  ,;
                      cWareCode  WITH Rolls.cWareCode              ,;
                      cDyelot    WITH Rolls.Dyelot                 ,;
                      cRSession  WITH Rolls.cRSession              ,;
                      cISession  WITH Rolls.cISession              ,;
                      cTran      WITH ''                           ,;
                      cTranType  WITH ''                           ,;
                      dTranDate  WITH MatInvJl.dTranDate           ,;
                      dPostDate  WITH MatInvJl.dPostDate           ,;
                      nUnitCost  WITH IIF(lcTrType = '5',(MatInvJl.nUnitCost*lnConv),;
                                      MatInvJl.nUnitCost),;
                      nUntCstBuy WITH MatInvJl.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH IIF(lcTrType$'15',Rolls.nQtyBal/IIF(lcTrType='1',;
                                          lnConv,lnConv),Rolls.nQtyBal) ,;
                      nApply     WITH IIF(lcTrType='3' OR (lcTrType='2' AND lnAdjStk > 0),nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType$'15' AND lnAdjStk<0,'2','3'),;
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='3'                 ,;
                      LineNo     WITH lnLineNo
                      
              REPLACE cTrn_Seq   WITH ''                           ,;
                      cRollId    WITH Rolls.cRollID                ,;
                      cFabric    WITH Rolls.cRollItem              ,;
                      cColor     WITH Rolls.Color                  ,;
                      cWareCode  WITH Rolls.cWareCode              ,;
                      cDyelot    WITH Rolls.Dyelot                 ,;
                      cRSession  WITH Rolls.cRSession              ,;
                      cISession  WITH Rolls.cISession              ,;
                      cTran      WITH ''                           ,;
                      cTranType  WITH ''                           ,;
                      dTranDate  WITH ITEMJRNL.DTRDATE           ,;
                      dPostDate  WITH ITEMJRNL.dPostDate           ,;
                      nCost      WITH IIF(lcTrType = '34',(ITEMJRNL.NCOST*lnConv),;
                                      ITEMJRNL.NCOST),;
                      nUntCstBuy WITH ITEMJRNL.nUntCstBuy          ,;
                      nReceived  WITH Rolls.nQty                   ,;
                      nIssued    WITH Rolls.nQty-Rolls.nQtyBal     ,;
                      nBalance   WITH IIF(lcTrType$'3456',Rolls.nQtyBal/IIF(lcTrType='56',;
                                          lnConv,lnConv),Rolls.nQtyBal) ,;&&6
                      nApply     WITH IIF(lcTrType $ '28' OR (lcTrType='1' AND lnAdjStk > 0),nBalance,0) ,;
                      RolTranCd  WITH IIF(lcTrType$'3456' AND lnAdjStk<0,'2','3'),;&&6
                      lStatus    WITH "S"                          ,;
                      lNeeded    WITH lcTrType='2'                 ,;
                      LineNo     WITH lnLineNo
                      
                      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
                      REPLACE Style WITH Rolls.Style
                      *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
                      
                      *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
                      REPLACE lNeeded    WITH lcTrType $ '28'                
                      *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]
                     
              IF lcCalProg = 'ARMINV'
                REPLACE nIssue WITH 0        ,;
                        cMorder WITH lcTrCode
              ENDIF 
            ENDIF
          ENDIF
          SELECT (lcFullRoll)
          
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
          *IF !SEEK(Rolls.cRollID+Rolls.cRollItem+Rolls.Color+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession,lcFullRoll,lcFullRoll)          
          IF !SEEK(Rolls.cRollID+Rolls.Style+Rolls.cWareCode+Rolls.Dyelot+Rolls.cRSession,lcFullRoll,lcFullRoll)
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          
            APPEND BLANK
            REPLACE cTrn_Seq   WITH ''                  ,;
                    cRollId    WITH Rolls.cRollID       ,;
                    cFabric    WITH Rolls.cRollItem     ,;
                    cColor     WITH Rolls.Color         ,;
                    cWareCode  WITH Rolls.cWareCode     ,;
                    cDyelot    WITH Rolls.Dyelot        ,;
                    cRSession  WITH Rolls.cRSession     ,;
                    cISession  WITH Rolls.cISession     ,;
                    cTran      WITH ''                  ,;
                    cTranType  WITH ''                  ,;
                    dTranDate  WITH ITEMJRNL.DTRDATE  ,;
                    dPostDate  WITH ITEMJRNL.dPostDate  ,;
                    nCost      WITH ITEMJRNL.NCOST  ,;
                    nUntCstBuy WITH ITEMJRNL.nUntCstBuy ,;
                    nReceived  WITH 0                   ,;
                    nIssued    WITH 0                   ,;
                    RolTranCd  WITH IIF(lcTrType$'3456' AND lnAdjStk<0,'2','3'),;&&6
                    lStatus    WITH "S"                 ,;
                    lNeeded    WITH lcTrType $ '28'        ,;
                    LineNo     WITH lnLineNo
                    
                    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
                    REPLACE Style WITH Rolls.Style
                    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

                    *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
            		IF Rolls.TranCd = '1'	
			           REPLACE nBalance WITH nBalance+(Rolls.nQtyBal/IIF(lcTrType$'3456',;&&6
   		                          IIF(lcTrType ='56',lnConv,lnConv),1)),;&&6
            					nApply   WITH IIF(lcTrType $ '28',nBalance,0)
			        ENDIF 
                    *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]
                    
          ENDIF
	      *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[Start]
          *REPLACE nBalance WITH nBalance+(IIF(Rolls.TranCd='2',-Rolls.nQty,Rolls.nQty)/IIF(lcTrType$'3456',;&&6
          *                      IIF(lcTrType ='56',lnConv,lnConv),1)),;&&6
          *        nApply   WITH IIF(lcTrType $ '28',nBalance,0)
          *! B608824,1 MMT 03/24/2009 Fix bug of Wrong Values while adjustment Material[End]
          IF lcCalProg = 'ARMINV'
            REPLACE cMorder WITH lcTrCode
          ENDIF

        ENDSCAN
      ENDIF
    ENDIF
  ENDIF

  SELECT Rolls
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
  *SET ORDER TO lcRolTag
  =gfSetOrder(lcRolTag)
  *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
ENDIF

