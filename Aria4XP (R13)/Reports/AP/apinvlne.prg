*:************************************************************************************
*: Program file       : APINVLIN.PRG
*: Program desc.      : AP Detial Report,To show Transactions detail from AP invoice lines.
*: System             : Aria Advantage Series.
*: Module             : Accounts Payable (AP)
*: Developer          : Mariam Mazhar(MMT)
*: Tracking Job Number: E302669
*:************************************************************************************
*: Calls :
*:    Functions : lfGetTyps() , lfvCrTemp() , lfCollData() , lfUpdFltVar()
*:                lfTmpFltr() , lfDocType() , lfPrntCTyp()     
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Notes   : ....
*:************************************************************************************
*: Example : DO APINVLNE.PRG
*:************************************************************************************
*: Modifications :
**************************************************************************************
IF oAriaApplication.MULTIINST 
  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\AP\APINVLIN.FXP ADDITIVE
  DO X:\ARIA4XP\SRVRPTS\AP\APINVLIN.FXP    
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"AP\APINVLIN.FXP" WITH .F.,.F.
ENDIF 
RETURN 
*!*************************************************************
*! Name      : lfvCostType
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Valid function of Cost Type.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfvCostType()
*!*************************************************************

FUNCTION lfvCostType
=lfOGMover(@laRpCSr,@laRpCTgt,'Cost Type',.T.,'')
lcRpTrgArr= ''
FOR lnCout = 1 TO ALEN(laRpCTgt,1)
  lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
ENDFOR
lcRpSrcArr = ''
FOR lnCout = 1 TO ALEN(laRpCSr,1)
  lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
ENDFOR

*!*************************************************************
*! Name      : lfFillCstA
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
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
    

  lcRpTrgArr= ''
  FOR lnCout = 1 TO ALEN(laRpCTgt,1)
    lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
  ENDFOR
  lcRpSrcArr = ''
  FOR lnCout = 1 TO ALEN(laRpCSr,1)
    lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
  ENDFOR
    
  ENDIF

  IF lcRpDocTyp $'MT'
    STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,;
                M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7
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

    lcRpTrgArr= ''
    FOR lnCout = 1 TO ALEN(laRpCTgt,1)
      lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
    ENDFOR
    lcRpSrcArr = ''
    FOR lnCout = 1 TO ALEN(laRpCSr,1)
      lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
    ENDFOR
     
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

    lcRpTrgArr= ''
    FOR lnCout = 1 TO ALEN(laRpCTgt,1)
      lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
    ENDFOR
    lcRpSrcArr = ''
    FOR lnCout = 1 TO ALEN(laRpCSr,1)
      lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
    ENDFOR
    
  ENDIF 

*!*************************************************************
*! Name      : lfclearrd
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
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
*! Date      : 02/17/2010
*! Purpose   : Valid function to display the Currency.
*!*************************************************************
*! Called from        : SYDREPRT
*!*************************************************************
*! Example            : =lfvCurDisp()
*!*************************************************************

FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)


*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
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
*! Date      : 02/17/2010
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
 
    lcRpTrgArr= ''
    FOR lnCout = 1 TO ALEN(laRpCTgt,1)
      lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
    ENDFOR
    lcRpSrcArr = ''
    FOR lnCout = 1 TO ALEN(laRpCSr,1)
      lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
    ENDFOR
   
  ENDIF

  IF lcRpDocTyp $'MT'
    STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,;
                M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7
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

    lcRpTrgArr= ''
    FOR lnCout = 1 TO ALEN(laRpCTgt,1)
      lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
    ENDFOR
    lcRpSrcArr = ''
    FOR lnCout = 1 TO ALEN(laRpCSr,1)
      lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
    ENDFOR
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

    lcRpTrgArr= ''
    FOR lnCout = 1 TO ALEN(laRpCTgt,1)
      lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
    ENDFOR
    lcRpSrcArr = ''
    FOR lnCout = 1 TO ALEN(laRpCSr,1)
      lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
    ENDFOR
  ENDIF 
*"ApVendor.cVenComp :R :H='Name':22,"  
IF lcRpDocTyp= 'I' .OR. lcRpDocTyp= 'R'
  lcBrowFlds = "PO :R :H="+IIF(lcRpDocTyp='R',"'Return P/O#'","'P/O #'")+":12,"+;
  "Status    :R :H='S':4,Vendor    :R :H='Vendor' :15,"+;
  "Entered   :R :H='Entered':15,"+;
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
*ApVendor.cVenComp :R :H='Name':22,
  lcBrowFlds ="PO  :R :H='P/O #':12,Status  :R :H='S':4,"+;
            "Vendor  :R :H='Vendor':15,"+;
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
*! Date      : 02/17/2010
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
*! Date      : 02/17/2010
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
*  SET RELATION TO Vendor INTO APVendor
ENDIF

IF lcParm = 'R'
  SELECT POSHDR
  SET RELATION TO
ENDIF
*!*************************************************************
*! Name      : lfWRunGrid
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
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

  lcRpTrgArr= ''
  FOR lnCout = 1 TO ALEN(laRpCTgt,1)
    lcRpTrgArr= lcRpTrgArr + SUBSTR(laRpCTgt[lnCout],1,1) + ','
  ENDFOR
  lcRpSrcArr = ''
  FOR lnCout = 1 TO ALEN(laRpCSr,1)
    lcRpSrcArr= lcRpSrcArr+ SUBSTR(laRpCSr[lnCout],1,1) + ','
  ENDFOR

  llCostTyp = .F.
ELSE
  RETURN  
ENDIF
*"ApVendor.cVenComp :R :H='Name':22,
  lcBrowFlds = "PO :R :H="+"'P/O #'"+":12,"+;
  "Status    :R :H='S':4,Vendor    :R :H='Vendor' :15,"+;
  "Entered   :R :H='Entered':15,"+;
  "Complete  :R :H='Complete':15,nStyOrder :R :H='Tot.Qty.':10,"+;
  "POTotal   :R :H='Amount':15,Receive   :R :H='Receive':10,Open      :R :H='Open':10"
  lcBusDocu = 'P'
  lcStyType = 'P'

  =gfOpenTable("APINVHDR","INVVEND",'SH',lcTempAPINVHDR)
  SELECT * FROM &lcTempAPINVHDR  WHERE .F. INTO CURSOR &lcAPINVHDR READWRITE 
  =lfMakeIndex(lcAPINVHDR)    && CINVNO+CVENDCODE 

  =gfOpenTable("APVINVDT","ORGVINV",'SH',lcTempAPVINVDT)
  SELECT * FROM &lcTempAPVINVDT where .F. into CURSOR &lcAPVINVDT READWRITE 
  =lfMakeIndex(lcAPVINVDT)
  *--ORGVINV   && CVENDCODE+CAPINVNO+CAPVILNO

  =gfOpenTable("APINVTKT","LNCONT",'SH',lcTempAPINVTKT)
  =lfclearrd()
*--LNCONT   && CVENDCODE+CAPINVNO+CAPVILNO+CRSESSION
*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/17/2010
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
*: Date      : 02/17/2010
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
