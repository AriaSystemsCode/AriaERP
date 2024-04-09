*:***************************************************************************
*:           File: COSTING.PRG                                             :*
*:         System: ARIA APPAREL SYSTEM 2.7                                 :*
*:         Module: Inventory Control                                       :*
*:        Program: Display Costing (Cost Sheet).                           :*
*:         Author: Timour Abdalla Khalil                                   :*
*:           Date: 08/28/97                                                :* 
*:***************************************************************************
*: Parameters  : lcItem  => Major+NonMajor (NonMajor can be (*) )
*:             : BOMFILE CONTAINS THE TEMPORARY FILE OR BOM FILE
*:             : lcImpMnu 'I','M' or Material cost will be passed with 'T'
*:***************************************************************************
*: Example :
*:    DO (gcAppHome+"Costing.FXP") WITH lcStyle,'BOM',IIF(llMake,'M','I')
*:    DO (gcAppHome+"Costing.FXP") WITH 'COTTON ******','BOM','T'
*:***************************************************************************
*: Modifications :
*B602293,1 Reham On 11/30/98
*B602293,1 1- gfChkRate return with the wrong exchange rate, get the exchange 
*B602293,1    rate manuall instead of calling the global function in case of
*B602293,1    imported style cost sheet.
*B603348,1  Abdou on 02/02/2000
*B603348,1  Increase The purchase price per item to 9999999.999 up On purchasing
*B603348,1  In Company Multi Currency you need at least  7 characters.
*B603348,1  Increase The Browse of UntCost and TotCost Fileds.
*B803435,1  (MHM) 25/07/2000 separate cost of fabric in case of more than one color.
*B603702,1  AMH 31/07/2000 Fixing Subtotal and Total of each costing item
*B603857,1  AMH 29/08/2000 Fixing Subtotal and Total to be in base currency
*B604813,1 AMH 08/21/2001 Fix the bug of not replacing the values of the 
*B604813,1                cost elements in the style file.
*C200327,1 AMH 04/08/2002 Add trigger to get the unit cost of fabrics from bom file.
*B606792,1 ASH 12/18/2002 Fix the bug of 'Alias not found' when trying to open the fabric file while the MA not installed.
*:*******************************************************************************
PARAMETER lcItem,BomFile,lcImpMnu
PRIVATE lcItem,lcMajor,lcNonMjr,lcNMKey,lcMjSep,llMScale,llAllColors,lnAlias

lnAlias=SELECT()
WAIT 'Compiling the costing information...' WINDOW NOWAIT

*--Restore the long cost element description.
DO CASE
  CASE lcImpMnu='M'
    lcCLgLb1 = gfGetMemVar('M_CMCOST1')
    lcCLgLb2 = gfGetMemVar('M_CMCOST2')
    lcCLgLb3 = gfGetMemVar('M_CMCOST3')
    lcCLgLb4 = gfGetMemVar('M_CMCOST4')
    lcCLgLb5 = gfGetMemVar('M_CMCOST5')      
  CASE lcImpMnu='I'
    lcCLgLb1 = gfGetMemVar('M_CICOST1')
    lcCLgLb2 = gfGetMemVar('M_CICOST2')
    lcCLgLb3 = gfGetMemVar('M_CICOST3')
    lcCLgLb4 = gfGetMemVar('M_CICOST4')
    lcCLgLb5 = gfGetMemVar('M_CICOST5')      
  CASE lcImpMnu='T'
    lcCLgLb1 = gfGetMemVar('M_CTCOST1')
    lcCLgLb2 = gfGetMemVar('M_CTCOST2')
    lcCLgLb3 = gfGetMemVar('M_CTCOST3')
    lcCLgLb4 = gfGetMemVar('M_CTCOST4')
    lcCLgLb5 = gfGetMemVar('M_CTCOST5')      
ENDCASE
*--Use extended size scale.  
llMScale = gfGetMemVar('M_USEEXSSC')
*--Major and Non Major title
lcMjrTtl = gfItemMask('HM')
lcNMjrTl = gfItemMask('HN')
*B606792,1 ASH 12/18/2002 (Begin) Check if the fabric file opened to prevent 'Alias not found' if MA not installed.
IF USED('FABRIC')
*B606792,1 ASH 12/18/2002 (End)
  lcFabTag=ORDER('FABRIC')
  SELECT FABRIC
  SET ORDER TO TAG FABRIC
ENDIF
lcStyTag=ORDER('STYLE')
lcStyFlt=FILTER('STYLE')
SELECT STYLE
SET FILTER TO
SET ORDER TO TAG STYLE


*--Costing information cursor name.
lcCostTmp = gfTempName()
lcTotCurs = gfTempName()    

SELECT BOM               && must be opened before hand
=AFIELDS(laBOMStru)
lnElem   = ALEN(laBOMStru,1) + 3
DIMENSION laBOMStru[lnElem, 4]
laBOMStru[lnElem-2,1] = 'cDispTyp'
laBOMStru[lnElem-2,2] = 'C'
laBOMStru[lnElem-2,3] = '1'
laBOMStru[lnElem-2,4] = '0'
laBOMStru[lnElem-1,1] = 'nTotEqu'
laBOMStru[lnElem-1,2] = 'N'
laBOMStru[lnElem-1,3] = '9'
laBOMStru[lnElem-1,4] = '2'
laBOMStru[lnElem,1]   = 'nIUtCost'
laBOMStru[lnElem,2]   = 'N'
laBOMStru[lnElem,3]   = '7'
laBOMStru[lnElem,4]   = '3'


CREATE CURSOR (lcTotCurs) FROM ARRAY laBOMStru
CREATE CURSOR (lcCostTmp) FROM ARRAY laBOMStru
INDEX ON CITMMAJOR+TYP+IIF(!(cCatGTyp$'MPD'),ITEM,MFGCODE)+ICLR;
                + STR(RECNO(),7) TAG (lcCostTmp) OF (gcWorkDir+lcCostTmp)


STORE 0 TO lnClrSeg,lnClrWid,lnClrSta,lnSzSta
IF lcImpMnu='T'
  llClrFnd=.T.
  lnClrSta=8
  lnClrWid=6
ELSE
  DIME laMajSeg[1,1]
  llClrFnd=.F.
  =gfItemMask(@laMajSeg)
  FOR lnClrSeg=1 TO ALEN(laMajSeg,1)
    IF llMScale AND laMajSeg[lnClrSeg,1] = 'S'
      lnSzSta = laMajSeg[lnClrSeg,4]
    ENDIF
    IF laMajSeg[lnClrSeg,1] = 'C'
      lnClrWid = LEN(laMajSeg[lnClrSeg,3])
      lnClrSta = laMajSeg[lnClrSeg,4]
      llClrFnd = .T.
    ENDIF
  ENDFOR
ENDIF

IF lcImpMnu='T'
  lcMjrPct = 'XXXXXXX'
  lcNMjrPt = 'XXXXXX'
ELSE
  lcMjrPct = gfItemMask('PM')
  lcNMjrPt = gfItemMask('PN')
ENDIF

lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lcMajor  = SUBSTR(lcItem,1,lnstylewid)
lcMjSep  = IIF(lcImpMnu='T','',SUBSTR(lcItem,lnstylewid+1,1))
lcNonMjr = SUBSTR(lcItem,lnstylewid+IIF(lcImpMnu='T',1,2),lncolorwid)
lcNMKey  = IIF(AT('*',lcNonMjr)<>0,SUBSTR(lcNonMjr,1,AT('*',lcNonMjr)-1),lcNonMjr)
llAllColors = (SUBSTR(lcNonMjr,1,1)='*')
lcBrTtl  = ''
lcSDesc  = ''
STORE 1 TO puSelClr,lnSCnt
STORE ' ' TO lcSSz1,lcSSz2,lcSSz3,lcSSz4,lcSSz5,lcSSz6,lcSSz7,lcSSz8
STORE ' ' TO lcSzMrk1,lcSzMrk2,lcSzMrk3,lcSzMrk4,lcSzMrk5,lcSzMrk6,lcSzMrk7,lcSzMrk8
llShwFrn = .F.

IF lcImpMnu = 'I'
  *--Check if the system is setup to use multiple currencies.
  llMulCurr=gfGetMemVar('llMulCurr',gcact_comp)
  IF llMulCurr
    =SEEK(lcMajor+lcMjSep+lcNMkey,'STYLE') 
    *-Get purchase price currency, duty currency or base currency.
    lcPPrCurr = IIF(!EMPTY(Style.cPriceCur),Style.cPriceCur,gcBaseCurr)
    lcDutyCur = IIF(!EMPTY(Style.cDutyCur),Style.cDutyCur,gcBaseCurr)
    llShwFrn=IIF(lcPPrCurr=gcBaseCurr AND lcDutyCur=gcBaseCurr,.F.,.T.)
    STORE 1 TO lnCurrUnit,lnExRate
    STORE '*' TO lcRateSign 
    STORE '/' TO lcUnitSign
  ENDIF
ENDIF

DECLARE laStyClrs[1]
lnArNo=0

IF lcImpMnu='T'
  SELECT FABRIC
  lcWCond = 'Fabric+Color=lcMajor+lcMjSep+lcNMKey'
  lcFCond = "lmaterial AND IIF(cItmMask='******',.T.,cItmMask=lcNMjr)"
ELSE
  SELECT STYLE
  lcWCond = 'Style=lcMajor+lcMjSep+lcNMKey'
  lcFCond = "!lmaterial AND LIKE(STRTRAN(cItmMask,'*','?'),PADR(lcMajor+lcMjSep+lcNMjr,19))"
ENDIF


SEEK lcMajor+lcMjSep+lcNMKey
SCAN WHILE &lcWCond
  IF lcImpMnu='T'
    lcNMjr=Color
  ELSE
    lcNMjr=SUBSTR(Style,lnStyleWid+2,lnColorWid)  
  ENDIF
  SELECT (BomFile)
  SEEK lcMajor
  SCAN WHILE cItmMajor=PADR(lcMajor,19) FOR &lcFCond
    SCATTER MEMVAR MEMO

    *--Get style component code.
    IF cCatGTyp='S'
      *--If style size not used in cost sheet.
      IF !(STYLE.Scale $ MSIZES)
        LOOP
      ENDIF
       
      *--Non major length without scale segment.
      lnNmjPart = lnColorWid - IIF(llMScale,3,0)

      *--Get an equevelent item non major part.
      lcCompNmj=''
      FOR lnI = 1 TO lnNmjPart
        lcCutChr  = SUBSTR(Item,lnStyleWid+1+lnI,1)
        lcCompNmj = lcCompNmj + ;
           IIF(lcCutChr='*',SUBSTR(STYLE.Style,lnStyleWid+1+lnI,1),lcCutChr)
      ENDFOR
      lcCompStyle = SUBSTR(Item,1,lnStyleWid)+lcMjSep+lcCompNmj

      *--Get an equevelent item scale part.       
      IF llMScale
        lcEqSCSz = STYLE.Scale
        FOR lnMI = 1 TO MEMLINES(MSZCROSREF)
          lcMemSLine = MLINE(MSZCROSREF,lnMI)
          IF STYLE.Scale $ lcMemSLine
            lcEqSCSz = SUBSTR(lcMemSLine,AT('~',lcMemSLine)+1,3)
            EXIT
          ENDIF 
        ENDFOR
        lcCompStyle = lcCompStyle + lcEqSCSz
      ENDIF

      *--Check existance of style component.
      lnRcSv=RECNO('STYLE')
      IF !SEEK(lcCompStyle,'STYLE')
        LOOP
	  ENDIF   
      GOTO lnRcSv IN STYLE

    ENDIF

    SELECT (lcCostTmp) 
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cItmMask WITH lcMajor+lcMjSep+lcNMjr
    IF ASCAN(laStyClrs,lcNMjr)=0
      lnArNo=lnArNo+1
      DIME laStyClrs[lnArNo]
      laStyClrs[lnArNo] = lcNMjr
    ENDIF
 
    *--If fabric or trim or style comp.
    IF cCatGTyp $ 'FTS'
      IF cCatGTyp = 'S'
        REPLACE ITEM WITH lcCompStyle,;
                ICLR WITH ''
      ELSE
        REPLACE ICLR WITH IIF(llClrFnd AND ICLR='*',;
             SUBSTR(&lcCostTmp..cItmMask,lnClrSta,lnClrWid),ICLR)
      ENDIF

      SELECT (BomFile)
      =lfReplCst()
    ENDIF

  ENDSCAN
ENDSCAN

SELECT (lcCostTmp) 
GO TOP
IF EOF()
  IF USED(lcTotCurs)
    USE IN (lcTotCurs)
  ENDIF
  IF USED (lcCostTmp)
    USE IN (lcCostTmp)
  ENDIF
  *B606792,1 ASH 12/18/2002 (Begin) Check if the fabric file opened to prevent 'Alias not found' if MA not installed.  
  IF USED('FABRIC')
  *B606792,1 ASH 12/18/2002 (End)
    SELECT FABRIC
    SET ORDER TO TAG &lcFabTag
  ENDIF
  SELECT STYLE
  SET ORDER TO TAG &lcStyTag
  SET FILTER TO &lcStyFlt
  SELECT(lnAlias)
  *--No cost sheet information available to compute costing.'
  =gfModalGen('TRM42052B42001','DIALOG')
  RETURN
ENDIF


******** NOW GO THROUGH THE lcCostTmp FILE AND CALCULATE THE RUNNING TOTAL FOR
******** EVERY TYP FOR EVERY STYLE COLOR AND UPDATE TO THE TYPCOST FIELD
******** THIS WAY THE LAST RECORD OF THE TYPE WILL HOLD THE TOTAL FOR THAT
******** TYPE
SELECT (lcCostTmp)
REPLACE ALL cDispTyp with '1'
GO TOP
INDEX ON cItmMask+typ tag lcCostTmp OF (gcWorkDir+lcCostTmp)

SELECT (lcTotCurs)
=lfAppdDln(&lcCostTmp..cItmMask,&lcCostTmp..Typ,'H')
SELECT (lcCostTmp)
GO TOP
lcOldTyp=typ
lcOldMsk=cItmMask
lcMSizs=''
STORE 0 TO XTOT,XCLRTOT,XTOTE
SCAN
  IF lcOldMsk=cItmMask 
    IF typ<> lcOldTyp
      SELECT (lcTotCurs)
      =lfAppdDln(&lcCostTmp..cItmMask,lcOldTyp,'S')
      =lfAppdDln(&lcCostTmp..cItmMask,lcOldTyp,'E')
      =lfAppdDln(&lcCostTmp..cItmMask,&lcCostTmp..Typ,'H')
      SELECT (lcCostTmp)
      lcOldTyp=typ
      STORE 0 TO XCLRTOT,XTOTE
    ENDIF  
  ELSE
    SELECT (lcTotCurs)
    =lfAppdDln(lcOldMsk,lcOldTyp,'S')
    =lfAppdDln(lcOldMsk,lcOldTyp,'T')
    =lfAppdDln(&lcCostTmp..cItmMask,&lcCostTmp..Typ,'H')
    SELECT (lcCostTmp)
    lcOldTyp=typ
    lcOldMsk=cItmMask
    STORE 0 TO XTOT,XCLRTOT,XTOTE
  ENDIF

  *--Update the costs per size.
  IF lcImpMnu<>'T'
    lnSCnt  =8
    IF SEEK(cItmMask,'STYLE') AND SEEK('S'+STYLE.Scale,'SCALE')
      lnSCnt=SCALE.Cnt
    ENDIF
    
    *--Update size cost.
    IF !EMPTY(MSizes)
      IF llMScale AND cCatgTyp='S' AND !EMPTY(MSzCrosref)
        lnBomSzs=0
        FOR lnMln=1 TO MEMLINES(MSzCrosref)  
          IF STYLE.Scale=SUBSTR( MLINE(MSzCrosref,lnMln) ,1 , 3)
            lnBomSzs=lnBomSzs+1
          ENDIF
        ENDFOR
      ELSE
        lnBomSzs=lnSCnt
        FOR lnMln=1 TO MEMLINES(MSizes)  
          IF OCCURS(STYLE.Scale,MLINE(MSizes,lnMln)) <> 0  
            lnBomSzs=OCCURS(',',MLINE(MSizes,lnMln))+1
            EXIT 
          ENDIF
        ENDFOR
      ENDIF

      REPLACE UntCost WITH (lnBomSzs/lnSCnt)*UntCost,;
              TotCost WITH (lnBomSzs/lnSCnt)*TotCost
    ENDIF 

    IF llShwFrn AND !EMPTY(cCatgTyp)
      lnExRate=1
      REPLACE nTotEqu WITH TotCost  
      REPLACE TotCost WITH lfGetEquv(nTotEqu,cCatgTyp)
    ENDIF

  ENDIF 
  *B603702,1  AMH 31/07/2000 Fixing Subtotal and Total of each costing item (Start)
  *XTOT=XTOT+TotCost
  *XCLRTOT=XCLRTOT+TotCost
  *B603857,1  AMH 29/08/2000 Fixing Subtotal and Total to be in base currency [Start]
  *XTOT=XTOT+(UntCost*nBomTotQty)
  *XCLRTOT=XCLRTOT+(UntCost*nBomTotQty)
  IF llShwFrn AND !EMPTY(cCatgTyp)
    lnExRate=1
    
    *B604813,1 AMH Round before add [Start]
    *XTOT=XTOT+lfGetEquv((UntCost*nBomTotQty),cCatgTyp)
    *XCLRTOT=XCLRTOT+lfGetEquv((UntCost*nBomTotQty),cCatgTyp)

    XTOT=XTOT+ROUND(lfGetEquv((UntCost*nBomTotQty),cCatgTyp),2)
    XCLRTOT=XCLRTOT+ROUND(lfGetEquv((UntCost*nBomTotQty),cCatgTyp),2)
    *B604813,1 AMH [End]
    
  ELSE
    
    *B604813,1 AMH Round before add [Start]
    *XTOT=XTOT+(UntCost*nBomTotQty)
    *XCLRTOT=XCLRTOT+(UntCost*nBomTotQty)
    XTOT=XTOT+ROUND((UntCost*nBomTotQty),2)
    XCLRTOT=XCLRTOT+ROUND((UntCost*nBomTotQty),2)
    *B604813,1 AMH [End]
    
  ENDIF
  *B603857,1  AMH 29/08/2000 Fixing Subtotal and Total to be in base currency [End  ]
  *B603702,1  AMH 31/07/2000 Fixing Subtotal and Total of each costing item (End)
ENDSCAN
SELECT (lcTotCurs)
=lfAppdDln(lcOldMsk,lcOldTyp,'S')
=lfAppdDln(lcOldMsk,lcOldTyp,'T')

*--Insert the heder , sub total and total records.
SELECT (lcTotCurs)
SCAN
  SCATTER MEMVAR MEMO
  SELECT (lcCostTmp)
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

*B602293,1 Reham On 11/30/98   *** Begin ***
*B602293,1 Check if the exchange rate file & the currency file if imported style cost sheet.
IF llMulCurr .AND. lcImpMnu = 'I'
  IF !USED("SycExch")
    llOpnExRt = .T.
    USE (gcSysHome + "SycExch") IN 0
  ELSE
    llOpnExRt = .F.
  ENDIF
  SELECT SycExch
  *-- Set order descending in the exchange rate file to get the latest exchange.
  SET ORDER TO TAG Currency DESCENDING
  
  IF !USED("SycCurr")
    llOpnCurr = .T.
    USE (gcSysHome + "SycCurr") IN 0
  ELSE
    llOpnCurr = .F.
  ENDIF
  SELECT SycCurr
  SET ORDER TO TAG CCURRCODE
ENDIF
*B602293,1 Reham On 11/30/98   *** End   ***

WAIT CLEAR
puOSelClr=0
SELECT (lcCostTmp)
INDEX ON citmmask+typ+cDispTyp tag MainTag  OF (gcWorkDir+lcCostTmp)
GO TOP
=lfvSelClr()

PUSH KEY
=lfTrapKeys()
DO (gcScrDir+"COSTNG.SPX")
POP KEY

*B602293,1 Reham On 11/30/98   *** Begin ***
*B602293,1 Close the exchange rate file & the currency file if imported style cost sheet.
IF llMulCurr .AND. lcImpMnu = 'I'
  IF llOpnExRt
    USE IN SycExch
  ENDIF
  IF llOpnCurr
    USE IN SycCurr
  ENDIF
ENDIF
*B602293,1 Reham On 11/30/98   *** End   ***

IF USED(lcTotCurs)
  USE IN (lcTotCurs)
ENDIF
IF USED (lcCostTmp)
  USE IN (lcCostTmp)
ENDIF  
*B606792,1 ASH 12/18/2002 (Begin) Check if the fabric file opened to prevent 'Alias not found' if MA not installed.
IF USED('FABRIC')
*B606792,1 ASH 12/18/2002 (End)
  SELECT FABRIC
  SET ORDER TO TAG &lcFabTag
ENDIF
SELECT STYLE
SET ORDER TO TAG &lcStyTag
SET FILTER TO &lcStyFlt
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : lfReplCst
*! Developer : Timour A. K.
*! Date      : 08/28/97                                         
*! Purpose   : Function to get and replace costs.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfReplCst()
*!*************************************************************
FUNCTION lfReplCst
IF cCatGTyp='F' .OR. (cCatGTyp='T' .AND. Trim_Invt)
  SELECT FABRIC
  lnRcSv=IIF(!EOF(),RECNO(),0)
  *B803435,1  (MHM)separate cost of fabric in case of more than one color.[Begin]
  *XUNTCOST=IIF(SEEK(SUBSTR(&lcCostTmp..ITEM,1,7)+ICLR),CostBuy/Conv,&lcCostTmp..UntCost)
  
  *C200327,1 AMH Add trigger to get the unit cost of fabrics from bom file [Start]
  *XUNTCOST=IIF(SEEK(SUBSTR(&lcCostTmp..ITEM,1,7)+&lcCostTmp..ICLR),CostBuy/Conv,&lcCostTmp..UntCost)
  IF ASCAN(laEvntTrig , PADR('ADDOPTN',10)) = 0
    XUNTCOST=IIF(SEEK(SUBSTR(&lcCostTmp..ITEM,1,7)+&lcCostTmp..ICLR),CostBuy/Conv,&lcCostTmp..UntCost)
  ELSE
    XUNTCOST= &lcCostTmp..UntCost
  ENDIF  
  *C200327,1 AMH [End]
  
  *B803435,1  (MHM) .[End]

  IF lnRcSv<>0
    GOTO lnRcSv
  ENDIF
ENDIF

IF cCatGTyp='S'
  SELECT STYLE
  lnRcSv=IIF(!EOF(),RECNO(),0)
  XUNTCOST=IIF(SEEK(&lcCostTmp..ITEM,'STYLE'),STYLE.TOTCOST,&lcCostTmp..UNTCOST)
  IF lnRcSv<>0
    GOTO lnRcSv
  ENDIF
ENDIF

IF cCatGTyp='F' OR (cCatGTyp='T' AND Trim_Invt) OR cCatGTyp='S'
  SELECT (lcCostTmp)
  REPLACE UNTCOST  WITH XUNTCOST,;
          nIUtCost WITH UNTCOST ,;
          TOTCOST  WITH ROUND(UNTCOST*nBomTOTQTY,2)
ENDIF
RETURN



*!*************************************************************
*! Name      : lfAppdDln
*! Developer : Timour A. K.
*! Date      : 08/28/97                                         
*! Purpose   : Append detail line.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfAppdDln()
*!*************************************************************
FUNCTION lfAppdDln
PARA LcIMask,LcITyp,lcRecTyp

IF lcRecTyp='H'
  lcTpDesc=lcCLgLb&LcITyp
ENDIF
APPEND BLANK
REPLACE cItmMask WITH LcIMask,;
        Typ      WITH LcITyp

DO CASE
  CASE lcRecTyp='H'
    REPLACE Item       WITH '*******************',;
            IClr       WITH '******',;
            MfgCode    WITH '******',;
            Uom        WITH '***',; 
            Desc       WITH lcTpDesc,;
            cDispTyp   WITH '0'
  CASE lcRecTyp='S'
    REPLACE Desc       WITH 'Sub Total:',;
            TotCost    WITH XCLRTOT,;
            cDispTyp   WITH '2'
  CASE lcRecTyp='T'
    REPLACE Desc       WITH 'Total:',;
            TotCost    WITH XTOT,;
            cDispTyp   WITH '2'
  CASE lcRecTyp='E'
    REPLACE cDispTyp   WITH '2'
ENDCASE
RETURN


*!************************************************************
*! Name      : lfwBrows
*! Developer : Timour A. K.
*! Date      : 08/28/97                                         
*! Purpose   : When function of COSTNG.SCX Read. Activates the 
*!             browse that displays costing details
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfwBrows()
*!*************************************************************
FUNCTION lfwBrows
PRIVATE lcCurAlias

*-- The upper browse displays the original lines of the PO(style+color)
*-- (those having a transaction code of 1 (TranCd = '1')
SELECT (lcCostTmp)
SET FILTER TO cItmMask = PADR(lcMajor,lnStyleWid,' ')+lcMjSep+laStyClrs[puSelClr]
*B603348,1  ABD Increase the UntCost Filed to 9999999.999  & TotCost 9999999999.99. [ Begin ]
*lcBrFld="Item :H='Item' :"+IIF(llShwFrn,"24","27")+","+;
        "IClr :H='I. Clr':8,"+;
        "MFGCode :H='MFG':8,"+;
        "Desc :H='Description' :30,"+;
        "UOM :H='UOM',"+;
        "nBomTotQty:H='Unt Qty':7 :P=IIF(EMPTY(cCatgTyp),'@Z','999.999'),"+;
        "UntCost :H='Unt Cst' :7 :P=IIF(EMPTY(cCatgTyp), '@Z','999.999'),"+;
        IIF(llShwFrn,"nTotEqu:H='Total Cst':P=IIF(EMPTY(cCatgTyp),'@Z','999999.99'),","")+;
        "TotCost :H=IIF(llShwFrn,'Equ.Tot.Cost.','Total Cst') :9 :P=IIF(EMPTY(cCatgTyp),'@Z','999999.99')"

lcBrFld="Item :H='Item' :"+IIF(llShwFrn,"24","27")+","+;
        "IClr :H='I. Clr':8,"+;
        "MFGCode :H='MFG':8,"+;
        "Desc :H='Description' :30,"+;
        "UOM :H='UOM',"+;
        "nBomTotQty:H='Unt Qty':7 :P=IIF(EMPTY(cCatgTyp),'@Z','999.999'),"+;
        "UntCost :H='Unt Cst' :11 :P=IIF(EMPTY(cCatgTyp), '@Z','9999999.999'),"+;
        IIF(llShwFrn,"nTotEqu:H='Total Cst':P=IIF(EMPTY(cCatgTyp),'@Z','9999999999.99'),","")+;
        "TotCost :H=IIF(llShwFrn,'Equ.Tot.Cost.','Total Cst') :13 :P=IIF(EMPTY(cCatgTyp),'9999999999.99','9999999999.99')"
*B603348,1  ABD [ END ]

BROWSE FIELDS &lcBrFld ;
       WHEN lfWhnBrw() ;
       WINDOW COSTNG_1 ;
       IN WINDOW COSTNG ;
       LOCK 0;
       NOMENU;         
       NOAPPEND;
       NOEDIT;
       NODELETE;         
       NOCLEAR;
       SAVE;  
       NOWAIT;
       TITLE lcBrTtl 
RETURN


*!*************************************************************
*! Name      : lfTrapKeys
*! Developer : Timour A. K.
*! Date      : 08/28/97                                         
*! Purpose   : Key traps necessary for browse.
*!*************************************************************
FUNCTION lfTrapKeys

ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL TAB        DO lpTab
ON KEY LABEL BACKTAB    DO lpShiftTab
ON KEY LABEL ESC        DO lpEsc
ON KEY LABEL CTRL+ENTER DO lpEsc

*!*************************************************************
*! Name : lpEsc
*! Synopsis : Esc key trapping procedure
*!*************************************************************
PROCEDURE lpEsc
ACTIVATE WINDOW COSTNG_2
_CUROBJ=OBJNUM(pbClose)
KEYBOARD CHR(13)


*!*************************************************************
*! Name : lpTab
*! Synopsis : Tab key trapping procedure
*!*************************************************************
PROCEDURE lpTab
ON KEY LABEL TAB lnDummy = 1
DO CASE
  CASE WONTOP('COSTNG_0') .AND. _CUROBJ = OBJNUM(puSelClr)
     ACTIVATE WINDOW (lcBrTtl)
  CASE WONTOP(lcBrTtl) 
    ACTIVATE WINDOW COSTNG_2
   _CUROBJ = OBJNUM(pbClose)
  CASE WONTOP('COSTNG_2') .AND. _CUROBJ = OBJNUM(pbClose)
    ACTIVATE WINDOW COSTNG_0
    _CUROBJ = OBJNUM(puSelClr)
ENDCASE
ON KEY LABEL TAB DO lpTab      

*!*************************************************************
*! Name : lpShiftTab
*! Synopsis : Shift Tab key trapping procedure
*!*************************************************************
PROCEDURE lpShiftTab
ON KEY LABEL BACKTAB lnDummy = 1
DO CASE
  CASE WONTOP('COSTNG_0') .AND. _CUROBJ = OBJNUM(puSelClr)
    ACTIVATE WINDOW COSTNG_2
   _CUROBJ = OBJNUM(pbClose)
  CASE WONTOP(lcBrTtl) 
    ACTIVATE WINDOW COSTNG_0
    _CUROBJ = OBJNUM(puSelClr)
  CASE WONTOP('COSTNG_2') .AND. _CUROBJ = OBJNUM(pbClose)
    ACTIVATE WINDOW (lcBrTtl)
ENDCASE
ON KEY LABEL BACKTAB DO lpShiftTab      


*!*************************************************************
*! Name      : lfvSelClr
*! Developer : Timour A. K.
*! Date      : 08/28/97                                         
*! Purpose   : Valid function for color selection popup
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSelClr()
*!*************************************************************
FUNCTION lfvSelClr

IF puSelClr=puOSelClr
  RETURN
ENDIF
SELECT (lcCostTmp)
SET FILTER TO cItmMask = PADR(lcMajor,lnStyleWid,' ')+lcMjSep+laStyClrs[puSelClr]
GO TOP
IF lcImpMnu='T'
  lcSDesc=gfCodDes(ALLTRIM(laStyClrs[puSelClr]),'COLOR')
ELSE
  lcSDesc=IIF(SEEK(&lcCostTmp..cItmMask,'STYLE'),STYLE.Desc1,'')
  lnSCnt=8
  STORE ' ' TO lcSSz1,lcSSz2,lcSSz3,lcSSz4,lcSSz5,lcSSz6,lcSSz7,lcSSz8
  IF SEEK('S'+STYLE.Scale,'SCALE')
    lnSCnt=SCALE.Cnt
    lcSSz1=SCALE.Sz1
    lcSSz2=SCALE.Sz2
    lcSSz3=SCALE.Sz3
    lcSSz4=SCALE.Sz4
    lcSSz5=SCALE.Sz5
    lcSSz6=SCALE.Sz6
    lcSSz7=SCALE.Sz7
    lcSSz8=SCALE.Sz8
  ENDIF
ENDIF
STORE ' ' TO lcSzMrk1,lcSzMrk2,lcSzMrk3,lcSzMrk4,lcSzMrk5,lcSzMrk6,lcSzMrk7,lcSzMrk8
=lfRefresh('COSTNG_0')
=lfRefresh('COSTNG_2')
IF puOSelClr<>0
  SHOW WINDOW (lcBrTtl) REFRESH TOP
ENDIF
puOSelClr=puSelClr
RETURN


*!*************************************************************
*! Name      : lfGetEquv
*! Developer : Timour A. K.
*! Date      : 08/28/97                                         
*! Purpose   : Function to return the equevelent cost.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetEquv()
*!*************************************************************
FUNCTION lfGetEquv
PARA lnTotFrnCst,lcCatgTyp

lnTotEquCst = lnTotFrnCst
IF lcCatgTyp='P' AND lcPPrCurr <> gcBaseCurr 
  *B602293,1 Reham On 11/30/98   *** Begin ***
  *B602293,1 Get the exchange rate manually instead of using gfChkRate()
  *lnExRate   = gfChkRate("lnCurrUnit",lcPPrCurr,gdSysDate,.F.,gcAct_comp)
  *lnExRate   = IIF(lnExRate=0,1,lnExRate)
  *lnCurrUnit = IIF(lnCurrUnit=0,1,lnCurrUnit) 
  
  *-- Get exchange rate. 
  llFound   = SEEK(gcBaseCurr + lcPPrCurr , 'SycExch')
  lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  *-- Get currency unit.
  llFound    = SEEK(lcPPrCurr , 'SycCurr')
  lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
  *B602293,1 Reham On 11/30/98   *** End   ***
  
  lcRateSign = gfGetExSin(@lcUnitSign,lcPPrCurr)  
  lnTotEquCst = lnTotFrnCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit
ENDIF
IF lcCatgTyp $ 'DM' AND lcDutyCur <> gcBaseCurr 
  *B602293,1 Reham On 11/30/98   *** Begin ***
  *B602293,1 Get the exchange rate & its unit manually instead of using gfChkRate()
  *lnExRate   = gfChkRate("lnCurrUnit",lcDutyCur,gdSysDate,.F.,gcAct_comp)
  *lnExRate   = IIF(lnExRate=0,1,lnExRate)
  *lnCurrUnit = IIF(lnCurrUnit=0,1,lnCurrUnit) 
  
  *-- Get exchange rate. 
  llFound   = SEEK(gcBaseCurr + lcDutyCur , 'SycExch')
  lnExRate  = IIF(llFound , sycexch.nExRate , 1)
  *-- Get currency unit.
  llFound    = SEEK(lcDutyCur , 'SycCurr')
  lnCurrUnit = IIF(llFound , SycCurr.nCurrUnit , 1)
  *B602293,1 Reham On 11/30/98   *** End   ***
  
  lcRateSign = gfGetExSin(@lcUnitSign,lcDutyCur)  
  lnTotEquCst = lnTotFrnCst &lcRateSign lnExRate &lcUnitSign lnCurrUnit
ENDIF

RETURN lnTotEquCst

*!*************************************************************
*! Name      : lfWhnBrw
*! Developer : Timour A. K.
*! Date      : 08/28/97                                         
*! Purpose   : Brow When.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfWhnBrw()
*!*************************************************************
FUNCTION lfWhnBrw

IF lcImpMnu='T'
  RETURN
ENDIF

lnSCnt=IIF(SEEK(cItmMask,'STYLE') AND SEEK('S'+STYLE.Scale,'SCALE'),SCALE.Cnt,8)

IF !EMPTY(cCatgTyp)
  IF !EMPTY(MSizes)
    FOR lnMln=1 TO MEMLINES(MSizes)
      IF OCCURS(STYLE.Scale,MLINE(MSizes,lnMln)) <> 0  
        lcMln=SUBSTR(MLINE(MSizes,lnMln),ATC('~',MLINE(MSizes,lnMln))+1,LEN(MLINE(MSizes,lnMln)))
        FOR I=1 TO 8
          lcZ=STR(I,1)
          lcSzMrk&lcZ = IIF(OCCURS(lcZ,lcMln)<>0,CHR(251),' ')  
        ENDFOR        
        EXIT 
      ENDIF
    ENDFOR
  ELSE
  *--All Sizes.
    FOR I=1 TO 8
      lcZ=STR(I,1)
      lcSzMrk&lcZ = IIF(!EMPTY(lcSSz&lcZ),CHR(251),' ')  
    ENDFOR        
  ENDIF 
ELSE
  STORE ' ' TO lcSzMrk1,lcSzMrk2,lcSzMrk3,lcSzMrk4,lcSzMrk5,lcSzMrk6,lcSzMrk7,lcSzMrk8   
ENDIF
=lfRefresh('COSTNG_2')
RETURN 


