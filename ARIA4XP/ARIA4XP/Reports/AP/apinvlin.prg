*:************************************************************************************
*: Program file       : APINVLIN.PRG
*: Program desc.      : AP Detial Report,To show Transactions detail from AP invoice lines.
*: System             : Aria Advantage Series.
*: Module             : Accounts Payable (AP)
*: Developer          : Mariam Mazhar(MMT)
*: Tracking Job Number: N037488
*:************************************************************************************
*: Calls :
*:    Functions : lfGetTyps() , lfvCrTemp() , lfCollData() , lfUpdFltVar()
*:                lfTmpFltr() , lfDocType() , lfPrntCTyp()     
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Notes   : ....
*:************************************************************************************
*: Example : DO APINVLIN.PRG
*:************************************************************************************
*: Modifications :
**************************************************************************************
*: B608496,1 WAM 03/26/2008 fix display the price and amount for materail PO
**************************************************************************************
lcStTime = TIME()

loogScroll.cCROrientation = 'L'
*--- Start Valriable declaration.

*B608496,1 WAM 03/26/2008 fix display the price and amount for materail PO
*IF llOGFltCh
IF loogScroll.llOGFltCh
*B608496,1 WAM 03/26/2008 (End)

  IF lcAPINVHDR <> lcFnlAPINVHDR AND lcFnlAPINVHDR <> lcTempAPINVHDR
	IF USED(lcFnlAPINVHDR)
	  USE IN (lcFnlAPINVHDR)
	ENDIF 
  ENDIF   
  IF lcFnlAPINVHDR == lcTempAPINVHDR
    lcFnlAPINVHDR = loOGScroll.gfTempName()
  ENDIF 
  IF lcAPINVHDR == lcFnlAPINVHDR
    lcFnlAPINVHDR = loOGScroll.gfTempName()
  ENDIF 
  IF lcAPVINVDTFnl <> lcAPVINVDT
    IF USED(lcAPVINVDTFnl)
      USE IN (lcAPVINVDTFnl)
    ENDIF 
  ENDIF 
  IF lcAPVINVDTFnl == lcAPVINVDT
    lcAPVINVDTFnl = loOGScroll.gfTempName()
  ENDIF 
  IF USED(lcAPINVHDR)
    USE IN (lcAPINVHDR)
    SELECT * FROM &lcTempAPINVHDR  WHERE .F. INTO CURSOR &lcAPINVHDR READWRITE 
    =lfMakeIndex(lcAPINVHDR)    
  ENDIF 
  IF USED(lcAPVINVDT)
	USE IN (lcAPVINVDT)
	SELECT * FROM &lcTempAPVINVDT where .F. into CURSOR &lcAPVINVDT READWRITE 
	=lfMakeIndex(lcAPVINVDT)
  ENDIF  
  STORE '' TO lcDocName , lcCstTypFlt , lcCostName , lcDocTyp
  STORE SPACE(08) TO lcVendCode , lcInvAlias , lcOperList
  STORE SPACE(03) TO lcCurrCode
  llSelByInv = .F.
  STORE {} TO ldStrDat , ldEndDat
  =lfGetTyps()
  =lfvCrTemp()  
  =lfUpdFltVar()
  =lfCollData()
  =lfTmpFltr()
ENDIF 	
	
SELECT (lcTempFile)
SET ORDER TO TAG &lcTempFile
LOCATE 
IF EOF()
  *-- 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
ELSE
  DO gfDispRe WITH EVALUATE('lcRpForm')  
ENDIF

*!*************************************************************
*! Name      : lfGetTyps
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Get the Cost types codes.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfGetTyps()
*!*************************************************************

FUNCTION lfGetTyps

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*-- Get the Document Type.
DO CASE
  CASE lcRpDocTyp = 'I'
    lcDocTyp = 'I'
  CASE lcRpDocTyp = 'R'
    lcDocTyp = 'R'
  CASE lcRpDocTyp = 'S'
    lcDocTyp = 'S'
  CASE lcRpDocTyp = 'M'
    lcDocTyp = 'M'
  CASE lcRpDocTyp = 'L'
    lcDocTyp = 'L'
  CASE lcRpDocTyp = 'F'
    lcDocTyp = 'F'   
  CASE lcRpDocTyp = 'T'
    lcDocTyp = 'T'
  CASE lcRpDocTyp = 'G'
    lcDocTyp = 'G'
ENDCASE
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvCrTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Create Temp File.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvCrTemp()
*!*************************************************************

FUNCTION lfvCrTemp

*-- Create temporary file to collect the data.
DIMENSION laTempacStru[31,4]
  laTempacstru[1,1]='Vendor'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 8
  laTempacstru[1,4]= 0
   
  laTempacstru[2,1]='CostName'
  laTempacstru[2,2]='C'
  laTempacstru[2,3]= 20
  laTempacstru[2,4]= 0

  laTempacstru[3,1]='DocTyp'
  laTempacstru[3,2]='C'
  laTempacstru[3,3]= 1
  laTempacstru[3,4]= 0

  laTempacstru[4,1]='Color'
  laTempacstru[4,2]='C'
  laTempacstru[4,3]= 6
  laTempacstru[4,4]= 0
  
  laTempacstru[5,1]=' VilNo'
  laTempacstru[5,2]='C'
  laTempacstru[5,3]= 4
  laTempacstru[5,4]= 0

  laTempacstru[6,1]='LotNo'
  laTempacstru[6,2]='C'
  laTempacstru[6,3]= 2
  laTempacstru[6,4]= 0
    
  laTempacstru[7,1]='Bom_Type'
  laTempacstru[7,2]='C'
  laTempacstru[7,3]= 1
  laTempacstru[7,4]= 0
    
  laTempacstru[8,1]='Codes'
  laTempacstru[8,2]='C'
  laTempacstru[8,3]= 6
  laTempacstru[8,4]= 0
    
  laTempacstru[9,1]='Session'
  laTempacstru[9,2]='C'
  laTempacstru[9,3]= 6
  laTempacstru[9,4]= 0
    
  laTempacstru[10,1]='Amount'
  laTempacstru[10,2]='N'
  laTempacstru[10,3]= 13
  laTempacstru[10,4]= 3
                                  
  laTempacstru[11,1]='Price'
  laTempacstru[11,2]='N'
  laTempacstru[11,3]= 13
  laTempacstru[11,4]= 3
    
  laTempacstru[12,1]='TotQty'
  laTempacstru[12,2]='N'
  laTempacstru[12,3]= 11
  laTempacstru[12,4]= 3
    
  laTempacstru[13,1]='Size8'
  laTempacstru[13,2]='N'
  laTempacstru[13,3]= 6
  laTempacstru[13,4]= 0
    
  laTempacstru[14,1]='Size7'
  laTempacstru[14,2]='N'
  laTempacstru[14,3]= 6
  laTempacstru[14,4]= 0
    
  laTempacstru[15,1]='Size6'
  laTempacstru[15,2]='N'
  laTempacstru[15,3]= 6
  laTempacstru[15,4]= 0
    
  laTempacstru[16,1]='Size5'
  laTempacstru[16,2]='N'
  laTempacstru[16,3]= 6
  laTempacstru[16,4]= 0
    
  laTempacstru[17,1]='Size4'
  laTempacstru[17,2]='N'
  laTempacstru[17,3]= 6
  laTempacstru[17,4]= 0
    
  laTempacstru[18,1]='Size3'
  laTempacstru[18,2]='N'
  laTempacstru[18,3]= 6
  laTempacstru[18,4]= 0
    
  laTempacstru[19,1]='Size2'
  laTempacstru[19,2]='N'
  laTempacstru[19,3]= 6
  laTempacstru[19,4]= 0
    
  laTempacstru[20,1]='Size1'
  laTempacstru[20,2]='N'
  laTempacstru[20,3]= 6
  laTempacstru[20,4]= 0
    
  laTempacstru[21,1]='STYLE'
  laTempacstru[21,2]='C'
  laTempacstru[21,3]= 19
  laTempacstru[21,4]= 0
    
  laTempacstru[22,1]='Operation'
  laTempacstru[22,2]='C'
  laTempacstru[22,3]= 30
  laTempacstru[22,4]= 0
    
  laTempacstru[23,1]='Cost_Type'
  laTempacstru[23,2]='C'
  laTempacstru[23,3]= 1
  laTempacstru[23,4]= 0
    
  laTempacstru[24,1]='DocNo'
  laTempacstru[24,2]='C'
  laTempacstru[24,3]= 6
  laTempacstru[24,4]= 0
    
  laTempacstru[25,1]='Doc_Type'
  laTempacstru[25,2]='C'
  laTempacstru[25,3]= 40
  laTempacstru[25,4]= 0
    
  laTempacstru[26,1]='Inv_Date'
  laTempacstru[26,2]='D'
  laTempacstru[26,3]= 8
  laTempacstru[26,4]= 0

  laTempacstru[27,1]='Invoice'
  laTempacstru[27,2]='C'
  laTempacstru[27,3]= 12
  laTempacstru[27,4]= 0

  laTempacstru[28,1]='Currency '
  laTempacstru[28,2]='C'
  laTempacstru[28,3]= 3
  laTempacstru[28,4]= 0

  laTempacstru[29,1]='Ven_name'
  laTempacstru[29,2]='C'
  laTempacstru[29,3]= 24
  laTempacstru[29,4]= 0
  
  laTempacstru[30,1]='Dyelot'
  laTempacstru[30,2]='C'
  laTempacstru[30,3]= 10
  laTempacstru[30,4]= 0

  laTempacstru[31,1]='PrtHead'
  laTempacstru[31,2]='L'
  laTempacstru[31,3]= 1
  laTempacstru[31,4]= 0

IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF 
=gfCrtTmp(lcTempFile,@laTempacStru,"Vendor + Currency + Invoice + Doc_Type+Bom_Type",lcTempFile,.T.)
SELECT(lcTempFile)
INDEX ON Codes     TAG (lcTempF2)
INDEX ON Session   TAG (lcTempF3)
INDEX ON LotNo     TAG (lcTempF4)
INDEX ON Cost_Type TAG (lcTempF5)
*!*************************************************************
*! Name      : lfCollData
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Data Collection.
*!*************************************************************
*! Calls     : lfDocType() , lfPrntCTyp() , gfAmntDisp()
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
*--function to get filter files
=lfGetInvoices()
SELECT (lcTempFile)
LOCATE 

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Get the values of filter Exp. variables.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfUpdFltVar()
*!*************************************************************

FUNCTION lfUpdFltVar



*--APINVHDR.CINVNO 
lnPosInvNo = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CINVNO")
IF lnPosInvNo > 0 
  lnPosInvNo = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosInvNo,1)
  lcInvAlias = loOgScroll.laOgFxFlt[lnPosInvNo,6]
  IF !EMPTY(lcInvAlias)
    llSelByInv = (USED(lcInvAlias) .AND. RECCOUNT(lcInvAlias) <> 0)
  ENDIF 
ENDIF 

*--Currency
lnPosCurr = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CCURRCODE")
IF lnPosCurr > 0 
  lnPosCurr = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCurr,1)
  lcCurrCode= loOgScroll.laOgFxFlt[lnPosCurr,6]
ENDIF 

*--Vendor code
lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"APVENDOR.CVENDCODE")
IF lnPosVend > 0 
  lnPosVend = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
  lcVendCode = loOgScroll.laOgFxFlt[lnPosVend,6]
ENDIF 

*--DATE
lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.DINVDATE")
IF lnPosDate > 0 
   lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
   ldStrDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosDate,6],1,10)))
   ldEndDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosDate,6],12,20)))
ENDIF 

*--Operation
lnPosOper = ASCAN(loOgScroll.laOgFXFlt,"APVINVDT.COPRCODE")
IF  lnPosOper > 0
   lnPosOper = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOper,1)
   lcOperList = loOgScroll.laOgFxFlt[lnPosOper,6]
ENDIF 



*!*************************************************************
*! Name      : lfTmpFltr
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Filter the temp file.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfTmpFltr()
*!*************************************************************

FUNCTION lfTmpFltr

lcFinalFlt = ".T."
IF !EMPTY(lcOperList)
  lcFinalFlt = lcFinalFlt + ".AND. Codes $ lcOperList"
ENDIF

IF lcRpIncCont ='Y'
  IF !EMPTY(lcRpSess)
    lcRpSess = ALLTRIM(lcRpSess)
    lcFinalFlt = lcFinalFlt + " .AND. Session = lcRpSess"
  ENDIF
ENDIF
IF !EMPTY(lcRpLot)
  lcRpLot = ALLTRIM(lcRpLot)
  lcFinalFlt = lcFinalFlt + " .AND. LotNo = lcRpLot"
ENDIF
IF !EMPTY(laRpCTgt[1])
  FOR lnCout = 1 TO ALEN(laRpCTgt,1)
    lcCstTypFlt = lcCstTypFlt + SUBSTR(laRpCTgt[lnCout],1,1) + '|'
  ENDFOR
  lcFinalFlt = lcFinalFlt + " .AND. Cost_Type $ lcCstTypFlt"
ENDIF
SELECT (lcTempFile)
DELETE ALL FOR !(&lcFinalFlt)
LOCATE 

*!*************************************************************
*! Name      : lfDocType
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Get the name of Document Type to be printed.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : lcDocName
*!*************************************************************
*! Example            : =lfDocType()
*!*************************************************************

FUNCTION lfDocType
PARAMETERS Doctype
*--lcTempAPVINVDT,lcTempAPINVTKT
*!*	IF lcRpIncCont ='Y'
*!*	  DO CASE
*!*	    CASE Doctype = 'I'
*!*	      lcDocName = 'Style Purchase Order'
*!*	    CASE EVALUATE(lcTempAPINVTKT+'.CIMTYP') = 'R'
*!*	      lcDocName = 'Style Purchase Order Return'
*!*	    CASE EVALUATE(lcTempAPINVTKT+'.CIMTYP') = 'S'
*!*	      lcDocName = 'Shipment'
*!*	    CASE EVALUATE(lcTempAPINVTKT+'.CIMTYP') = 'M'
*!*	      lcDocName = 'Cutting Ticket'
*!*	    CASE EVALUATE(lcTempAPINVTKT+'.CIMTYP') = 'L'
*!*	      lcDocName = 'Material PO'
*!*	    CASE EVALUATE(lcTempAPINVTKT+'.CIMTYP') = 'F'
*!*	      lcDocName = 'Material PO Return'
*!*	    CASE EVALUATE(lcTempAPINVTKT+'.CIMTYP') = 'T'
*!*	      lcDocName = 'Material MFG Order' 
*!*	  ENDCASE
*!*	ELSE
  DO CASE
    CASE Doctype = 'I'
      lcDocName = 'Style Purchase Order'
    CASE Doctype = 'R'
      lcDocName = 'Style Purchase Order Return'
    CASE Doctype = 'S'
      lcDocName = 'Shipment'
    CASE Doctype= 'M'
      lcDocName = 'Cutting Ticket'
    CASE Doctype = 'L'
      lcDocName = 'Material PO'
    CASE Doctype = 'F'
      lcDocName = 'Material PO Return'
    CASE Doctype = 'T'
      lcDocName = 'Material MFG Order' 
    CASE Doctype = 'G'
      lcDocName = 'General Expense' 
  ENDCASE
*!*	ENDIF
RETURN lcDocName

*!*************************************************************
*! Name      : lfPrntCTyp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Get the name of Cost Type to be printed.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : lcCostName
*!*************************************************************
*! Example            : =lfPrntCTyp()
*!*************************************************************

FUNCTION lfPrntCTyp

*!*	IF m.DocTyp $'IRS'
*!*	  STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7;
*!*	            M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7
*!*	  =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7',gcAct_Comp)

*!*	  FOR lnCout = 1 TO 5
*!*	    IF m.Cost_Type = EVAL('M_CITYPE'+STR(lnCout,1))
*!*	       lcCostName = EVAL('M_CISLBL'+ m.Bom_Type)
*!*	    ENDIF
*!*	  ENDFOR
*!*	ENDIF

*!*	IF m.DocTyp $'MT'
*!*	*!*	  STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,;
*!*	              M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5
*!*	  =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5',gcAct_Comp)         

*!*	  FOR lnCout = 1 TO 5
*!*	    IF m.Cost_Type = EVAL('M_CMTYPE'+STR(lnCout,1))
*!*	      lcCostName = EVAL('M_CMSLBL'+ m.Bom_Type)
*!*	    ENDIF
*!*	  ENDFOR
*!*	ENDIF

IF m.DocTyp $'LF'
  DO CASE
    CASE m.Cost_Type = 'P' .AND. m.Bom_Type = '1'
      lcCostName = 'Purchase Price'
    CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '2'
      lcCostName = 'Freight'
    CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '3'
      lcCostName = 'Tax'
    CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '4'
      lcCostName = 'Quota'
  ENDCASE
ENDIF
RETURN lcCostName

*!*************************************************************
*! Name      : lfvCostType
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Valid function of Cost Type.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfvCostType()
*!*************************************************************

FUNCTION lfvCostType
=lfOGMover(@laRpCSr,@laRpCTgt,'Cost Type',.T.,'')

*!*************************************************************
*! Name      : lfFillCstA
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Fill the Cost type array.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfFillCstA()
*!*************************************************************

FUNCTION lfFillCstA


  IF lcRpDocTyp $ 'IRS'
    STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,;
                M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7
*/    =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5',gcAct_Comp)
    =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7',oAriaApplication.ActiveCompanyID)
    DIMENSION laICstType[3,1]
    DIMENSION laRpCSr[3,1]
      
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    lnElements=0
    lnElements=0
    FOR lnCount = 1 TO 7
      IF INLIST(EVAL('M_CITYPE'+STR(lnCount,1)),'P','D','M')
        lnElements = lnElements+1
        DIMENSION laICstType[lnElements,1]
        laICstType[lnElements,1] = EVAL('M_CITYPE'+STR(lnCount,1))+' - '+EVAL('M_CISLBL'+STR(lnCount,1))
      ENDIF
    ENDFOR
    DIMENSION laRpCSr[lnElements,1]
    =ACOPY(laICstType,laRpCSr)
  ENDIF

  IF lcRpDocTyp $'MT'
    STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,;
                M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7
*/    =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5',gcAct_Comp)         
    =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7',oAriaApplication.ActiveCompanyID)         
    DIMENSION laICstType[2,1]
    DIMENSION laRpCSr[2,1]  
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    lnElements=0
    FOR lnCount = 1 TO 7
      IF INLIST(EVAL('M_CMTYPE'+STR(lnCount,1)),'M')
        lnElements = lnElements+1
        DIMENSION laICstType[lnElements,1]
        laICstType[lnElements,1] = EVAL('M_CMTYPE'+STR(lnCount,1))+' - '+EVAL('M_CMSLBL'+STR(lnCount,1))
      ENDIF
    ENDFOR
    DIMENSION laRpCSr[lnElements,1]
    =ACOPY(laICstType,laRpCSr)
  ENDIF

  IF lcRpDocTyp $'LF'
    DIMENSION laICstType[4,1]
    DIMENSION laRpCSr[4,1]  
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    laICstType[1,1] = 'P - Purchase Price'
    laICstType[2,1] = 'D - Freight'
    laICstType[3,1] = 'D - Tax'
    laICstType[4,1] = 'D - Quota'  
    =ACOPY(laICstType,laRpCSr)
  ENDIF 

*!*************************************************************
*! Name      : lfclearrd
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Clear the Option Grid files.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfclearrd()
*!*************************************************************

FUNCTION lfclearrd
*--CLEARREAD()
IF lcRpIncCont = 'Y'
  lnSession = ASCAN(laOGObjType,'LCRPSESS')
  IF lnSession > 0
    lnSession = ASUBSCRIPT(laOGObjType,lnSession,1)
    laOGObjCnt[lnSession] = .T.
  ENDIF
ELSE
  lnSession = ASCAN(laOGObjType,'LCRPSESS')
  IF lnSession > 0
    lnSession = ASUBSCRIPT(laOGObjType,lnSession,1)
    laOGObjCnt[lnSession] = .F.
    LCRPSESS = ""
  ENDIF
ENDIF   
= lfOGShowGet('LCRPSESS')
llCostTyp = .F.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Valid function to display the Currency.
*!*************************************************************
*! Called from        : SYDREPRT
*!*************************************************************
*! Example            : =lfvCurDisp()
*!*************************************************************

FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)


*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Valid function of Currency Code.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfvCurCode()
*!*************************************************************

FUNCTION _lfvCurCode
PRIVATE lcSavBrFld,lcSavTitle,lcBrFields,lcFile_Ttl,lnAlias,lcCurrSele
STORE ' ' TO lcSavBrFld,lcSavTitle,lcBrFields,lcFile_Ttl,lcCurrSele
STORE 0 TO lnAlias
lnAlias = SELECT(0)
lnPosCurr = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CCURRCODE")
IF lnPosCurr > 0 
  lnPosCurr = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCurr,1)
  lcCurrSele= loOgScroll.laOgFxFlt[lnPosCurr,6]
ENDIF 
*--APINVHDR.CCURRCODE
IF EMPTY(lcCurrSele) .AND. lcRpCurr <> 'F'
  RETURN
ENDIF
IF !SEEK(lcCurrSele,'SYCCURR') .OR. ATC("?",lcCurrSele) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    loOgScroll.laOgFxFlt[lnPosCurr,6] = lcOldCurr
  ELSE
    loOgScroll.laOgFxFlt[lnPosCurr,6] = laTemp[1]
  ENDIF
ENDIF
*--SHOW GET loOgScroll.laOgFxFlt[lnPosCurr,6]
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwCurCode()
*!*************************************************************

FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,27]

*!*************************************************************
*! Name      : lfvDoc
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Valid function of Document types popup. Change
*!             the cost type mover according to Document type.
*!*************************************************************
*! Called from : SYREPUVR
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvDoc()
*!*************************************************************

FUNCTION lfvDoc

CLEARREAD()
IF lcRpDocTyp $ 'IRS'
    STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,;
                M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7
*/    =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5',gcAct_Comp)
    =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7',oAriaApplication.ActiveCompanyID)
    DIMENSION laICstType[3,1]
    DIMENSION laRpCSr[3,1]
      
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    lnElements=0
    lnElements=0
    FOR lnCount = 1 TO 7
      IF INLIST(EVAL('M_CITYPE'+STR(lnCount,1)),'P','D','M')
        lnElements = lnElements+1
        DIMENSION laICstType[lnElements,1]
        laICstType[lnElements,1] = EVAL('M_CITYPE'+STR(lnCount,1))+' - '+EVAL('M_CISLBL'+STR(lnCount,1))
      ENDIF
    ENDFOR
    DIMENSION laRpCSr[lnElements,1]
    =ACOPY(laICstType,laRpCSr)
  ENDIF

  IF lcRpDocTyp $'MT'
    STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,;
                M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7
*/    =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5',gcAct_Comp)         
    =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7',oAriaApplication.ActiveCompanyID)         
    DIMENSION laICstType[2,1]
    DIMENSION laRpCSr[2,1]  
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    lnElements=0
    FOR lnCount = 1 TO 7
      IF INLIST(EVAL('M_CMTYPE'+STR(lnCount,1)),'M')
        lnElements = lnElements+1
        DIMENSION laICstType[lnElements,1]
        laICstType[lnElements,1] = EVAL('M_CMTYPE'+STR(lnCount,1))+' - '+EVAL('M_CMSLBL'+STR(lnCount,1))
      ENDIF
    ENDFOR
    DIMENSION laRpCSr[lnElements,1]
    =ACOPY(laICstType,laRpCSr)
  ENDIF

  IF lcRpDocTyp $'LF'
    DIMENSION laICstType[4,1]
    DIMENSION laRpCSr[4,1]  
    DECLARE laRpCTgt[1]
    laRpCTgt = ''
    laICstType[1,1] = 'P - Purchase Price'
    laICstType[2,1] = 'D - Freight'
    laICstType[3,1] = 'D - Tax'
    laICstType[4,1] = 'D - Quota'  
    =ACOPY(laICstType,laRpCSr)
  ENDIF 
IF lcRpDocTyp= 'I' .OR. lcRpDocTyp= 'R'
  lcBrowFlds = "PO :R :H="+IIF(lcRpDocTyp='R',"'Return P/O#'","'P/O #'")+":12,"+;
  "Status    :R :H='S':4,Vendor    :R :H='Vendor' :15,"+;
  "ApVendor.cVenComp :R :H='Name':22,Entered   :R :H='Entered':15,"+;
  "Complete  :R :H='Complete':15,nStyOrder :R :H='Tot.Qty.':10,"+;
  "POTotal   :R :H='Amount':15,Receive   :R :H='Receive':10,Open      :R :H='Open':10"
ENDIF 
IF lcRpDocTyp= 'M'
  lcBrowFlds = "PO  :H='Cutkt'   ,Style   :H='Style'   ,Status  :H='S'       ,"+;
               "Entered :H='Issue'   ,Complete:H='Complete',Season  :H='Se'      ,"+;
               "cDivision:H='Di'     ,        NSTYORDER :H='Budget':P='999999',"+;
               "RECEIVE :H='Recvd' :P='999999',DAMAGE :H='Damged':P='999999',"+;
               "OPEN :H='Open'  :P='999999' "
ENDIF
IF lcRpDocTyp $ 'LF'
  lcBrowFlds ="PO  :R :H='P/O #':12,Status  :R :H='S':4,"+;
            "Vendor  :R :H='Vendor':15,ApVendor.cVenComp :R :H='Name':22,"+;
            "Complete:R :H='Complete':10,NSTYORDER  :R :H='Tot.Qty.':10,"+;
            "POTotal :R :H='Amount':15,RECEIVE :R :H='Receive':10,OPEN  :R :H='Open':10"
ENDIF 

IF lcRpDocTyp= 'T'
  lcBrowFlds ="PO:H='Order#',STYLE  :H='Fabric',"+IIF((gfGetMemVar('M_WareHouse')= 'Y'),"cWareCode:H='Warehouse',","")+;
              "Status   :H='S',Entered  :H='Entered',Complete :H='Complete'"
ENDIF 
DO CASE 
  CASE lcRpDocTyp = 'I'
    lcBusDocu = 'P'
    lcStyType = 'P'  
    lcdocument = "PO #"
  CASE lcRpDocTyp= 'R'
    lcBusDocu = 'R'
    lcStyType = 'P'  
    lcdocument = "Return PO #"
  CASE lcRpDocTyp= 'M'
    lcBusDocu = 'P'
    lcStyType = 'U'  
    lcdocument = "Cutting Ticket"
  CASE lcRpDocTyp= 'F'
    lcBusDocu = 'R'
    lcStyType = 'M'  
    lcdocument = "Return Material PO #"
  CASE lcRpDocTyp= 'L'
    lcBusDocu = 'P'
    lcStyType = 'M'  
    lcdocument = "Material PO #"
  CASE lcRpDocTyp= 'T'
    lcBusDocu = 'P'
    lcStyType = 'F'  
    lcdocument = "Material MFG Order"
ENDCASE   

IF lcRpDocTyp $'G'
  CLEARREAD()
ENDIF
  
*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/31/2005
*! Purpose   : Set function for the invoice number option in 
*!             case of In Range.
*!*************************************************************
*! Called from : SYREPUVR
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!*************************************************************
*! Return      : NONE
*!*************************************************************
*! Example     : =lfvInvNo()
*!*************************************************************

FUNCTION lfvInvNo

PARAMETERS lcParm
IF lcParm = 'S'
  SELECT APINVHDR
  GO TOP
ENDIF

*!*************************************************************
*! Name      : lfSRVPosHD
*! Developer : Mariam Mazhar (MMT)
*! Date      : 29/03/2005
*! Purpose   : set - reset - valid funtion  of poshdr file
*!*************************************************************
*! Passed Parameters  :  lcParm
*!*************************************************************
*! Returns            :  NONE
*!*************************************************************
*! Example            :  =lfSRVPosHD()
*!*************************************************************
FUNCTION lfSRVPosHD
PARAMETERS lcParm

IF lcParm = 'S'
  SELECT POSHDR
  SET RELATION TO Vendor INTO APVendor
ENDIF

IF lcParm = 'R'
  SELECT POSHDR
  SET RELATION TO
ENDIF
*!*************************************************************
*! Name      : lfWRunGrid
*! Developer : Mariam Mazhar(MMT)
*! Date      : 10/19/98
*! Purpose   : When run OG function.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWRunGrid()
*!*************************************************************
FUNCTION lfWRunGrid

STORE '' TO lcRpDocNo
IF llCostTyp
  DIMENSION laRpCSr[1] , laRpCTgt[1] , laOldVal[1]
  =lfFillCstA()
  llCostTyp = .F.
ELSE
  RETURN  
ENDIF

  lcBrowFlds = "PO :R :H="+"'P/O #'"+":12,"+;
  "Status    :R :H='S':4,Vendor    :R :H='Vendor' :15,"+;
  "ApVendor.cVenComp :R :H='Name':22,Entered   :R :H='Entered':15,"+;
  "Complete  :R :H='Complete':15,nStyOrder :R :H='Tot.Qty.':10,"+;
  "POTotal   :R :H='Amount':15,Receive   :R :H='Receive':10,Open      :R :H='Open':10"
  lcBusDocu = 'P'
  lcStyType = 'P'

IF TYPE('loAPINVHDR') <> 'O'
  loAPINVHDR  = CreateObject("RemoteTable","APINVHDR","INVVEND",lcTempAPINVHDR,SET("DATASESSION"))
  SELECT * FROM &lcTempAPINVHDR  WHERE .F. INTO CURSOR &lcAPINVHDR READWRITE 
  =lfMakeIndex(lcAPINVHDR)    && CINVNO+CVENDCODE 
ENDIF 

IF TYPE('loAPVINVDT') <> 'O'
  loAPVINVDT = CreateObject("RemoteTable","APVINVDT","ORGVINV",lcTempAPVINVDT,SET("DATASESSION"))
  SELECT * FROM &lcTempAPVINVDT where .F. into CURSOR &lcAPVINVDT READWRITE 
  =lfMakeIndex(lcAPVINVDT)
ENDIF 
*--ORGVINV   && CVENDCODE+CAPINVNO+CAPVILNO

IF TYPE('loAPINVTKT') <> 'O'
  loAPINVTKT  = CreateObject("RemoteTable","APINVTKT","LNCONT",lcTempAPINVTKT,SET("DATASESSION"))
ENDIF 
=lfclearrd()
*--LNCONT   && CVENDCODE+CAPINVNO+CAPVILNO+CRSESSION
*!*************************************************************
*! Name      : lfGetInvoices
*! Developer : Mariam Mazhar(MMT)
*! Date      : 10/19/98
*! Purpose   : When run OG function.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWRunGrid()
*!*************************************************************
FUNCTION lfGetInvoices
 
PRIVATE llCallGfAm
llCallGfam = EMPTY(lcCurrCode) .OR. (!EMPTY(lcCurrCode) .AND. lcRpCurr <> 'F')
*---------------------
IF lcRpDocTyp $ 'IRS'
  STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,;
              M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7
    =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7',oAriaApplication.ActiveCompanyID)
ENDIF 
IF lcRpDocTyp $'MT'
   STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,;
               M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7
    =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7',oAriaApplication.ActiveCompanyID)         
ENDIF 

*----------------------
IF llSelByInv 
  SELECT(lcInvAlias)
  LOCATE 
  SCAN
    *B608496,1 WAM 03/26/2008 fix display the price and amount for materail PO
    *loAPINVHDR.seek(&lcInvAlias..cInvNo)
    loAPINVHDR.seek(&lcInvAlias..cInvNo,'INVVEND')
    *B608496,1 WAM 03/26/2008 (End)
    
    SELECT(lcTempAPINVHDR)
    SCAN REST WHILE CINVNO+CVENDCODE  = &lcInvAlias..cInvNo AND BETWEEN(DTOS(&lcTempAPINVHDR..dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
      SCATTER MEMO MEMVAR  
      *--- temp file hold the selected invoices but not filtered with currency and vendor 
      SELECT(lcAPINVHDR)
      APPEND BLANK 
      GATHER MEMO MEMVAR 
    ENDSCAN   
  ENDSCAN 
  SELECT(lcAPVINVDT)
  LOCATE 
  lcSelCond = ""
  lcSleFils = lcAPINVHDR
  lcSeleFlds =  lcAPINVHDR+".*"
  IF !EMPTY(lcVendCode) AND USED(lcVendCode)
    lcSelCond = lcAPINVHDR+".cVendCode = " +lcVendCode+".cVendCode"
    lcSleFils = lcSleFils + ","+lcVendCode
  ENDIF 
  IF !EMPTY(lcCurrCode) AND USED(lcCurrCode)
    lcSleFils = lcSleFils + ","+lcCurrCode
    lcSelCond = lcSelCond + IIF(!EMPTY(lcSelCond)," AND ","")+lcAPINVHDR+".ccurrcode = "+lcCurrCode+".ccurrcode"
  ENDIF 
  SELECT(lcAPINVHDR)
  LOCATE 
  IF !EMPTY(lcSelCond)   
     SELECT &lcSeleFlds FROM &lcSleFils WHERE &lcSelCond INTO CURSOR &lcFnlAPINVHDR
  ELSE    
     lcFnlAPINVHDR = lcAPINVHDR
  ENDIF 
ELSE
  IF loAPINVHDR.llNative
    lcSelCond = ""
    lcSleFils = lcTempAPINVHDR
    lcSeleFlds =  lcTempAPINVHDR+".*"
    IF !EMPTY(lcVendCode) AND USED(lcVendCode)
      lcSelCond = lcTempAPINVHDR+".CVENDCODE+CINVNO= " +lcVendCode+".cVendCode"
      lcSleFils = lcSleFils + ","+lcVendCode
    ENDIF 
    *--               
    IF !EMPTY(lcCurrCode)
      lcSleFils = lcSleFils + ","+lcCurrCode
      lcSelCond = lcSelCond + IIF(!EMPTY(lcSelCond)," AND ","")+lcTempAPINVHDR+".CCURRCODE+CVENDCODE+CINVNO  = "+lcCurrCode +".ccurrcode"
    ENDIF
    IF !EMPTY(ldStrDat) AND !EMPTY(ldEndDat)
      lcSelCond = lcSelCond + IIF(!EMPTY(lcSelCond)," AND ","")+"BETWEEN(DTOS(&lcTempAPINVHDR..dinvdudat),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))"
    ENDIF 
    IF !EMPTY(lcSelCond)   
       SELECT &lcSeleFlds FROM &lcSleFils WHERE &lcSelCond INTO CURSOR &lcFnlAPINVHDR
    ELSE    
       lcFnlAPINVHDR = lcTempAPINVHDR
    ENDIF 
  ELSE
    lcSelCond = ""
    lcSleFils = "APINVHDR"
    lcSeleFlds=  "APINVHDR.*"
    IF !EMPTY(lcVendCode) AND USED(lcVendCode)
      SELECT(lcVendCode)
      LOCATE 
      IF !EOF()&&IF user select styles from the style file
        lcCurName = lcVendCode
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLVendCode = loOgScroll.gfSQLTempName('','cvendcode C(8)',lcCurName,'cvendcode')
            IF EMPTY(lcSQLVendCode)
              *-- SQL connection error. can't open the report
              =gfModalGen('TRM00416B40011','ALERT')
              RETURN .F.
            ELSE
              lcSelCond = "APINVHDR.cVendCode = " +lcSQLVendCode+".cVendCode"
              lcSleFils = lcSleFils + ","+lcSQLVendCode
            ENDIF
          ENDIF
        ENDIF
      ENDIF  
    ENDIF 
    IF !EMPTY(lcCurrCode) AND USED(lcCurrCode)
      SELECT(lcCurrCode)
      LOCATE 
      IF !EOF()&&IF user select styles from the style file
        lcCurName = lcCurrCode
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLCurrCode = loOgScroll.gfSQLTempName('','ccurrcode C(3)',lcCurName,'ccurrcode')
            IF EMPTY(lcSQLCurrCode)
              *-- SQL connection error. can't open the report
              =gfModalGen('TRM00416B40011','ALERT')
              RETURN .F.
            ELSE
              lcSelCond = lcSelCond + IIF(!EMPTY(lcSelCond)," AND ","")+"APINVHDR.ccurrcode ="+lcSQLCurrCode+".ccurrcode"
              lcSleFils = lcSleFils + ","+lcSQLCurrCode
            ENDIF
          ENDIF
        ENDIF
      ENDIF  
    ENDIF 
    IF !EMPTY(ldStrDat) AND !EMPTY(ldEndDat)
      lcSelCond = lcSelCond + IIF(!EMPTY(lcSelCond)," AND ","")+" APINVHDR.dinvdudat BETWEEN ALLTRIM(DTOS(ldStrDat))and ALLTRIM(DTOS(ldEndDat))"
    ENDIF 
    = lfOpenSql(lcSeleFlds,lcSleFils ,lcAPINVHDR,lcSelCond)
    lcFnlAPINVHDR = lcTempAPINVHDR
  ENDIF
ENDIF 
lcShipNo = ''
lcPosNo  = ''
IF lcDocTyp = 'S'
  lnPosShpNo = ASCAN(loOgScroll.laOgFXFlt,"SHPMTHDR.SHIPNO")
  IF lnPosShpNo > 0 
    lnPosShpNo = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosShpNo,1)
    lcShipNo = loOgScroll.laOgFxFlt[lnPosShpNo,6]
    IF !EMPTY(lcShipNo) AND USED(lcShipNo) AND RECCOUNT(lcShipNo)> 0
      SELECT(lcShipNo)
      INDEX on ShipNo TAG &lcShipNo
    ENDIF   
  ENDIF  
ELSE
  lnPosNo = ASCAN(loOgScroll.laOgFXFlt,"POSHDR.PO")
  IF lnPosNo > 0 
     lnPosNo = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosNo,1)
     lcPosNo = loOgScroll.laOgFxFlt[lnPosNo,6]
  ENDIF  
  IF !EMPTY(lcPosNo) AND USED(lcPosNo) AND RECCOUNT(lcPosNo)>0
    SELECT (lcPosNo)
    INDEX on po TAG &lcPosNo
  ENDIF   
ENDIF   
IF lcDocTyp = 'S'
  lcForCond = 'cimtyp+SHIPNO = lcDocTyp '
ELSE
  lcForCond = 'cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp'
ENDIF
llHasShip = (lcDocTyp = 'S' AND !EMPTY(lcShipNo) AND USED(lcShipNo) AND  RECCOUNT(lcShipNo)>0)
llHasPO   = (!EMPTY(lcPosNo) AND USED(lcPosNo) AND RECCOUNT(lcPosNo)>0)
*--
loAPVINVDT.SETORDER("ITEM")
IF !loAPVINVDT.SEEK(lcDocTyp)
  loAPVINVDT.SETORDER("ORGVINV")
  RETURN 
ENDIF 
loAPVINVDT.SETORDER("ORGVINV")
SELECT(lcFnlAPINVHDR)
SET ORDER TO 
SCAN 
  m.Invoice  = CINVNO
  m.Vendor   = CVENDCODE
  m.Ven_Name = COUTCOMP
  m.Currency = CCURRCODE
  m.Inv_Date = DINVDATE

  WAIT WINDOW "Validating vendor "+m.Vendor+" Transactions"   NOWAIT
  IF loAPVINVDT.Seek(m.Vendor+ m.Invoice)
 
    SELECT(lcTempAPVINVDT)
    SCAN REST WHILE CVENDCODE+CAPINVNO+CAPVILNO = m.Vendor+ m.Invoice;
                FOR cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp AND ;
                  IIF(llHasShip,SEEK(SHIPNO,lcShipNo),IIF(llHasPO,SEEK(CTKTNO,lcPosNo),.T.))
      WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
      m.VilNo     = CAPVILNO
      m.Cost_Type = CCOSTTYPE
      m.Bom_Type  = CBOMTYPE
      m.Operation = gfCodDes(COPRCODE,PADR('MFGCODE',10))
      IF m.Operation = 'All'
        m.Operation = ''
      ENDIF
      m.Codes     =COPRCODE
      m.LotNo     =CLOTNO
      m.DocNo     = IIF(lcDocTyp = 'S',SHIPNO,CTKTNO)
      *-----------
      m.DocTyp    = CIMTYP
      IF !EMPTY(m.Cost_Type)
        m.CostName  = IIF(m.DocTyp $'IRS',EVAL('M_CISLBL'+m.Bom_Type),IIF(m.DocTyp $'MT',EVAL('M_CMSLBL'+ m.Bom_Type),lfprntctyp()))
      ELSE
        m.CostName = ""
      ENDIF 
      m.Doc_Type  = lfDocType(m.DocTyp)
      m.Style     = ITEM
      m.Color     = COLOR
      m.Session   = ""
      m.Size1     = napaprqty1
      m.Size2     = napaprqty2
      m.Size3     = napaprqty3
      m.Size4     = napaprqty4
      m.Size5     = napaprqty5
      m.Size6     = napaprqty6
      m.Size7     = napaprqty7
      m.Size8     = napaprqty8
      *B608496,1 WAM 03/26/2008 fix display the price and amount for materail PO
*!*	      *--B127399,1 MMT 05/11/2005 fix bug of wrong total qty [Start]
*!*	      m.TotQty    = napaprqty1+napaprqty2+napaprqty3+napaprqty4+napaprqty5+napaprqty6+napaprqty7+napaprqty8
*!*	*      m.TotQty    = naptaprQTY
*!*	      *--B127399,1 MMT 05/11/2005 fix bug of wrong total qty [End]
      IF INLIST(lcDocTyp,'L','F','T','G')
        m.TotQty = naptaprQTY
      ELSE
        m.TotQty = napaprqty1+napaprqty2+napaprqty3+napaprqty4+napaprqty5+napaprqty6+napaprqty7+napaprqty8
      ENDIF   
      *B608496,1 WAM 03/26/2008 (End)

      m.Price     = napaprPRIC
      m.dyelot 	  = cdyelot            
      m.PrtHead   = .F.
      lnCount     = 0
      SELECT(lcFnlAPINVHDR)
      llInserted = .F.
     */ IIF(m.DocTyp $ 'LF',m.TotQty,VAL(m.TotQty))
      m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
      m.Price     = m.Amount / m.TotQty
		
      IF lcRpIncCont ='Y'
        *B608496,1 WAM 03/26/2008 fix display the price and amount for materail PO
        *IF loAPINVTKT.seek(m.Vendor+m.Invoice+m.VilNo)
        IF loAPINVTKT.seek(m.Vendor+m.Invoice+m.VilNo,'LNCONT')
        *B608496,1 WAM 03/26/2008 (End)
          SELECT(lcTempAPINVTKT)
          SCAN REST WHILE cvendcode+capinvno+capvilno+crsession = m.Vendor+m.Invoice+m.VilNo ;
                      FOR cimtyp+ctktno+STR(lineno,6)+crsession = lcDocTyp &&AND !EMPTY(crsession)
            IF !llInserted
		      INSERT INTO (lcTempFile) FROM MEMVAR  
		      llInserted = .T.          
            ENDIF 
            IF EMPTY(crsession)
              LOOP
            ENDIF 
           *-- WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
            m.VilNo     = EVALUATE(lcTempAPVINVDT+'.CAPVILNO')
            m.Cost_Type = EVALUATE(lcTempAPVINVDT+'.CCOSTTYPE')
            m.Bom_Type  = EVALUATE(lcTempAPVINVDT+'.CBOMTYPE')
            m.Operation = gfCodDes(EVALUATE(lcTempAPVINVDT+'.COPRCODE'),PADR('MFGCODE',10))
            IF m.Operation = 'All'
              m.Operation = ''
            ENDIF
            lnCount = lnCount + 1
            IF lnCount = 1
              m.PrtHead = .T.
            ELSE
              m.PrtHead = .F.
            ENDIF 
            m.Codes     =EVALUATE(lcTempAPVINVDT+'.COPRCODE')
            m.LotNo     =EVALUATE(lcTempAPVINVDT+'.CLOTNO')
            m.DocNo     = IIF(lcDocTyp = 'S',EVALUATE(lcTempAPVINVDT+'.SHIPNO'),EVALUATE(lcTempAPVINVDT+'.CTKTNO'))
            m.DocTyp    = CIMTYP
            IF !EMPTY(m.Cost_Type)
              m.CostName  = IIF(m.DocTyp $'IRS',EVAL('M_CISLBL'+m.Bom_Type),IIF(m.DocTyp $'MT',EVAL('M_CMSLBL'+ m.Bom_Type),lfprntctyp()))
            ELSE
              m.CostName = ""
            ENDIF 
  *         m.CostName  = IIF(m.DocTyp $'IRS',EVAL('M_CISLBL'+m.Bom_Type),IIF(m.DocTyp $'MT',EVAL('M_CMSLBL'+ m.Bom_Type),lfprntctyp()))
            m.Doc_Type  = lfDocType(m.DocTyp)
            m.Style     = ITEM
            m.Color     = COLOR
            m.Session   = CRSESSION
            m.Size1     = NAPAPLQTY1
            m.Size2     = NAPAPLQTY2
            m.Size3     = NAPAPLQTY3
            m.Size4     = NAPAPLQTY4
            m.Size5     = NAPAPLQTY5
            m.Size6     = NAPAPLQTY6
            m.Size7     = NAPAPLQTY7
            m.Size8     = NAPAPLQTY8
            *B608496,1 WAM 03/26/2008 fix display the price and amount for materail PO
*!*	            *--B127399,1 MMT 05/11/2005 FIX BUG OF WRONG TOTAL QTY[START]
*!*	            *   m.TotQty    = NAPTAPLQTY &&IIF(m.DocTyp $ 'LF',naptaprQTY,STR(naptaprQTY))
*!*	            m.TotQty    = NAPAPLQTY1+NAPAPLQTY2+NAPAPLQTY3+NAPAPLQTY4+NAPAPLQTY5+NAPAPLQTY6+NAPAPLQTY7+NAPAPLQTY8
*!*	            *--B127399,1 MMT 05/11/2005 FIX BUG OF WRONG TOTAL QTY[END]

            IF INLIST(lcDocTyp,'L','F','T','G')
              m.TotQty = NAPTAPLQTY
            ELSE
              m.TotQty = NAPAPLQTY1+NAPAPLQTY2+NAPAPLQTY3+NAPAPLQTY4+NAPAPLQTY5+NAPAPLQTY6+NAPAPLQTY7+NAPAPLQTY8
            ENDIF   
            *B608496,1 WAM 03/26/2008 (End)

            m.Price     = NAPAPLPRIC       
            m.dyelot	  = cdyelot     
            SELECT(lcFnlAPINVHDR)
            m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
            m.Price     = m.Amount / m.TotQty
            INSERT INTO (lcTempFile) FROM MEMVAR
          ENDSCAN
        ENDIF   
      ELSE
        *B608496,1 WAM 03/26/2008 fix display the price and amount for materail PO
        *IF loAPINVTKT.seek(m.Vendor+m.Invoice+m.VilNo) 
        IF loAPINVTKT.seek(m.Vendor+m.Invoice+m.VilNo,'LNCONT') 
        *B608496,1 WAM 03/26/2008 (End)
          SELECT(lcTempAPINVTKT)
          LOCATE REST WHILE cvendcode+capinvno+capvilno+crsession = m.Vendor+m.Invoice+m.VilNo ;
   			          FOR cimtyp+ctktno+STR(lineno,6)+crsession = lcDocTyp
 		     IF FOUND()
      			INSERT INTO (lcTempFile) FROM MEMVAR
     		  ENDIF 	
        ENDIF   
      ENDIF 
		  *-----------
*!*	      IF loAPINVTKT.seek(m.Vendor+m.Invoice+m.VilNo)
*!*	        SELECT(lcTempAPINVTKT)
*!*	        SCAN REST WHILE cvendcode+capinvno+capvilno+crsession = m.Vendor+m.Invoice+m.VilNo ;
*!*	                    FOR cimtyp+ctktno+STR(lineno,6)+crsession = lcDocTyp AND;
*!*	                      IIF(lcRpIncCont ='Y',.T.,EMPTY(crsession))
*!*	      
*!*	          WAIT WINDOW "Validating data for Invoice# : " + m.Invoice NOWAIT
*!*	          m.DocTyp    = CIMTYP
*!*	          m.CostName  = IIF(m.DocTyp $'IRS',EVAL('M_CISLBL'+m.Bom_Type),IIF(m.DocTyp $'MT',EVAL('M_CMSLBL'+ m.Bom_Type),lfprntctyp()))
*!*	          *--lfPrntCTyp()
*!*	          m.Doc_Type  = lfDocType()
*!*	          m.Style     = ITEM
*!*	          m.Color     = COLOR
*!*	          m.Session   = CRSESSION
*!*	          m.Size1     = NAPAPLQTY1
*!*	          m.Size2     = NAPAPLQTY2
*!*	          m.Size3     = NAPAPLQTY3
*!*	          m.Size4     = NAPAPLQTY4
*!*	          m.Size5     = NAPAPLQTY5
*!*	          m.Size6     = NAPAPLQTY6
*!*	          m.Size7     = NAPAPLQTY7
*!*	          m.Size8     = NAPAPLQTY8
*!*	          m.TotQty    = NAPTAPLQTY
*!*	          m.Price     = NAPAPLPRIC            
*!*	          SELECT(lcFnlAPINVHDR)
*!*	          m.Amount    = m.TotQty * IIF(llCallGfam,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
*!*	          m.Price     = m.Amount / m.TotQty
*!*	          INSERT INTO (lcTempFile) FROM MEMVAR
*!*	        ENDSCAN
*!*	      ENDIF   
    eNDSCAN  
  ENDIF 
ENDSCAN 
*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/31/05
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMakeIndex
PARAMETERS lcTempName
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcCursor = lcTempName
lnBuffering = CURSORGETPROP("Buffering",lcCursor)
 =CURSORSETPROP("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
=lfCrtindex(lcCursor)
SELECT (lcCursor)
FOR lnI = 1 TO ALEN(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  INDEX ON &lcIndex. TAG (lcTag)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/31/05
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE

  CASE UPPER(lcTable) =  lcAPINVHDR
    DIMENSION laIndex[2,2]
    laIndex[1,1] = 'ccurrcode'
    laIndex[1,2] = lcAPINVHDR
    
    laIndex[2,1] = 'cvendcode'
    laIndex[2,2] = 'cvendcode'
 
  CASE UPPER(lcTable) = lcAPVINVDT    
    DIMENSION laIndex[2,2]
    laIndex[1,1] = 'cimtyp+SHIPNO'
    laIndex[1,2] = lcAPVINVDT  
    
    laIndex[2,1] = 'cimtyp+ctktno'
    laIndex[2,2] = 'ctktno'
 
  CASE UPPER(lcTable) =lcAPVINVDTFnl
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CVENDCODE+CAPINVNO'
    laIndex[1,2] = lcAPVINVDTFnl

ENDCASE  
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 03/31/05
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.




