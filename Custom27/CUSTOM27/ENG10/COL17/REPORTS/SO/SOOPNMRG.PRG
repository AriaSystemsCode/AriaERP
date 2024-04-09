*:************************************************************************************
*: Program file       : SOOPNMRG.PRG
*: Program desc.      : Open Order Margin Report.
*: System             : Aria Advantage Series.
*: Module             : SALES ORDERS (SO)
*: Developer          : RANIA ABDEL RAZIK (RAE)
*: Date 			  : 06/26/2002
*: Tracking Job Number: C200268
*:************************************************************************************
*: Calls :
*:    Functions : 
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Example : DO APINVLIN.PRG
*:************************************************************************************
*: Modifications ..
*:************************************************************************************
*: B606740,1 RAE 12/08/2002 Fix the bug of variavle 'nRltFld' not found
*:************************************************************************************

STORE ''  TO lcStyCode , lcDivision , lcStyGrp  , lcCustomer , lcCustClas , lcStyle ,;
             lcGrpExp  , lcDivExp   , lcSGrpExp , lcStatus
STORE 0   TO lnMajLen  , nTotQty    , lnPrice   , lnTotQty   , lcRltFld
STORE 1   TO lnRate
STORE {}  TO ldStrDat  , ldEndDat   , ldStart   , ldEnd
STORE .F. TO llSelSty  , llSelCust

DECLARE laCustDisc[1,2]
laCustDisc[1,1] = 'NTERDISCR'
laCustDisc[1,2] = 'lcRltFld'

*-- Style title
lcStyTitle = gfItemMask('HI')

*-- Reporting only the open SO
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ]) + [ORDHDR.STATUS =='O']
*-- If Division is selected
IF ATC('STYLE.CDIVISION',lcRpExp) > 0
  lcRpExp = STRTRAN(lcRpExp,'STYLE.CDIVISION','ORDHDR.CDIVISION')
ENDIF

*-- lcStyMaj = the major of the style
*-- Get the length of style major
lnMajLen = LEN(lcStyMaj)

*-- Creation of Temp File.
=lfvCrTemp()
*-- Update the filter Exp.
=lfUpdFltVar()
*-- Collection of Data.
=lfDataScan()

*!*************************************************************
*! Name      : lfvCrTemp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 06/26/2002
*! Purpose   : Create temporary file to collect the data.
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

*B606740,1 RAE Add new field (nRldFld) [start]
*CREATE TABLE (lcTempFile) (cCustClas C(30) , cCustCode C(5) , cOrder C(6)   , cCustPO C(15) ,;
                           dCompDat D(8)   , cStyMaj C(19)  , nPrice N(12,2), nTotQty N(7)  ,;                                    
                           nAve_cost N(12,2))                                  

CREATE TABLE (lcTempFile) (cCustClas C(30) , cCustCode C(5) , cOrder C(6)   , cCustPO C(15) ,;
                           dCompDat D(8)   , cStyMaj C(19)  , nPrice N(12,2), nTotQty N(7)  ,;                                    
                           nRldFld N(8,2)  , nAve_cost N(12,2))                                  
*B606740,1 RAE [end]

INDEX ON cCustClas + cCustCode + cOrder    TAG (lcTempFile) OF (gcWorkDir + lcTempFile + ".CDX")
INDEX ON cOrder + cStyMaj + DTOS(dCompDat) TAG DATE         OF (gcWorkDir + lcTempFile + ".CDX")

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 06/26/2002
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

FOR lnInd = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYMAJOR' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Style Code..
      lcStyCode = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,8))
      llSelSty  = (USED(lcStyCode) .AND. RECCOUNT(lcStyCode) <> 0)
      
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Division..
      lcDivision = laOgFxFlt[lnInd,6]
      lcDivExp   = "&laOGFxFlt[lnInd,1]." + ' $ lcDivision'
      
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Style Group..
      lcStyGrp = laOgFxFlt[lnInd,6]
      lcGrpExp = "&laOGFxFlt[lnInd,1]." + ' $ lcStyGrp'

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Customer..
      lcCustomer = laOgFxFlt[lnInd,6]
      llSelCust  = (USED(lcCustomer) .AND. RECCOUNT(lcCustomer) <> 0)
      
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.CLASS' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Customer Class..
      lcCustClas = laOgFxFlt[lnInd,6]
      lcSGrpExp  = "&laOGFxFlt[lnInd,1]." + ' $ lcCustClas'
      
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Completion date..
      ldStrDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldEndDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfDataScan
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 06/26/2002
*! Purpose   : Data Collection
*!*************************************************************
*! Called from : SOOPNMRG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDataScan()
*!*************************************************************
FUNCTION lfDataScan

DO CASE
  *-- If Style and Customer are selected
  CASE llSelSty .AND. llSelCust
    SELECT (lcCustomer)
    SCAN
      m.cCustCode = ACCOUNT
      =SEEK('M'+m.cCustCode,'CUSTOMER')
      m.cCustClas = gfCodDes(CUSTOMER.CLASS,PADR('CLASS',10))
      SELECT ORDHDR
      SCAN FOR account+status+bulk+cordtype+order = m.cCustCode + 'O' .AND. ;
               IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) .AND. ;
               IIF(EMPTY(lcDivExp),.T.,EVALUATE(lcDivExp)) .AND. ;
               IIF(EMPTY(lcSGrpExp),.T.,EVALUATE(lcSGrpExp))
        m.cOrder  = ORDHDR.ORDER
        m.cCustPO = ORDHDR.CUSTPO
        lnRate    = ORDHDR.NEXRATE
        =gfRltFld(ORDHDR.CTERMCODE,@laCustDisc,'CTERMCODE')

        *B606740,1 RAE Store CTERMCODE to the new field.[start]
        m.nRldFld = lcRltFld
        *B606740,1 RAE [end]

        SELECT (lcStyCode)
        SCAN
          m.cStyMaj = ALLTRIM(CSTYMAJOR)      
          SELECT ORDLINE
          SCAN FOR cordtype+order+STR(lineno,6) = 'O' + m.cOrder .AND. ;
                   style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = m.cStyMaj .AND. ;
                   BETWEEN(DTOS(ORDLINE.COMPLETE),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))        
            m.dCompDat  = ORDLINE.COMPLETE
            ldStart = IIF(EMPTY(ldStart),m.dCompDat,ldStart)
            ldEnd   = IIF(EMPTY(ldEnd),m.dCompDat,ldEnd)
            ldStart = IIF(ldStart < m.dCompDat,ldStart,m.dCompDat)
            ldEnd   = IIF(ldEnd   > m.dCompDat,ldEnd,m.dCompDat)
            m.nPrice    = ORDLINE.PRICE
            m.nPrice    = ORDLINE.PRICE / lnRate
            m.nTotQty   = ORDLINE.TOTQTY
            lcStyle     = ORDLINE.STYLE
            SELECT STYLE
            =SEEK(lcStyle,'STYLE')
            m.nAve_cost = STYLE.AVE_COST
            SELECT (lcTempFile)
            SET ORDER TO DATE
            IF SEEK(m.cOrder+PADR(m.cStyMaj,19)+DTOS(m.dCompDat),lcTempFile)
              REPLACE nTotQty WITH ORDLINE.TOTQTY + lnTotQty
            ELSE
              INSERT INTO (lcTempFile) FROM MEMVAR
              lnTotQty = m.nTotQty              
            ENDIF
            SET ORDER TO (lcTempFile)
          ENDSCAN
        ENDSCAN
      ENDSCAN
    ENDSCAN
    
  *-- If style is selected
  CASE llSelSty
    SELECT (lcStyCode)
    SCAN
      m.cStyMaj = ALLTRIM(CSTYMAJOR)
      SELECT ORDLINE
      SCAN FOR style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = m.cStyMaj .AND. ;
               IIF(EMPTY(lcDivExp),.T.,EVALUATE(lcDivExp)) .AND. ;
               IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) .AND. ;
               BETWEEN(DTOS(ORDLINE.COMPLETE),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))        
        m.dCompDat  = ORDLINE.COMPLETE
        ldStart     = IIF(EMPTY(ldStart),m.dCompDat,ldStart)
        ldEnd       = IIF(EMPTY(ldEnd),m.dCompDat,ldEnd)
        ldStart     = IIF(ldStart < m.dCompDat,ldStart,m.dCompDat)
        ldEnd       = IIF(ldEnd   > m.dCompDat,ldEnd,m.dCompDat)
        m.cCustCode = ORDLINE.ACCOUNT
        m.nPrice    = ORDLINE.PRICE        
        =SEEK('M'+m.cCustCode,'CUSTOMER')
        m.cCustClas = gfCodDes(CUSTOMER.CLASS,PADR('CLASS',10))        
        m.nTotQty   = ORDLINE.TOTQTY
        m.cOrder    = ORDLINE.ORDER

        SELECT ORDHDR
        SCAN FOR cordtype+order = 'O' + m.cOrder .AND. ;
                 IIF(EMPTY(lcSGrpExp),.T.,EVALUATE(lcSGrpExp))
          lcStatus = ORDHDR.Status
          lnRate   = ORDHDR.NEXRATE
          m.nPrice  = ORDLINE.PRICE / lnRate
          m.cCustPO = ORDHDR.CUSTPO
          =gfRltFld(ORDHDR.CTERMCODE,@laCustDisc,'CTERMCODE')
      
          *B606740,1 RAE Store CTERMCODE to the new field.[start]
          m.nRldFld = lcRltFld
          *B606740,1 RAE [end]
      
          m.cCustClas = gfCodDes(CUSTOMER.CLASS,PADR('CLASS',10))

        SELECT STYLE
        SET ORDER TO CSTYLE
        =SEEK(m.cStyMaj)
        m.nAve_cost = STYLE.AVE_COST
        SELECT (lcTempFile)
        SET ORDER TO DATE
        IF lcStatus = 'O'
          IF SEEK(m.cOrder+PADR(m.cStyMaj,19)+DTOS(m.dCompDat),lcTempFile)
            REPLACE nTotQty WITH ORDLINE.TOTQTY + lnTotQty
          ELSE
            INSERT INTO (lcTempFile) FROM MEMVAR
            lnTotQty   = m.nTotQty
          ENDIF
        ENDIF
        SET ORDER TO (lcTempFile)
        ENDSCAN
      ENDSCAN
    ENDSCAN

  *-- If Customer is selected    
  CASE llSelCust
    SELECT ORDLINE
    SET ORDER TO TAG Ordline
  
    SELECT ORDHDR
    SET RELATION TO "O" + ORDER   INTO ORDLINE  ADDITIVE
    SELECT ORDLINE
    SET RELATION TO STYLE INTO STYLE ADDITIVE

    SELECT (lcCustomer)
    SCAN
      m.cCustCode = ACCOUNT
      =SEEK('M'+m.cCustCode,'CUSTOMER')
      m.cCustClas = gfCodDes(CUSTOMER.CLASS,PADR('CLASS',10))
      SELECT ORDHDR
      SCAN FOR account+status+bulk+cordtype+order = m.cCustCode + 'O' .AND. ;
               IIF(EMPTY(lcDivExp),.T.,EVALUATE(lcDivExp)) .AND. ;               
               IIF(EMPTY(lcSGrpExp),.T.,EVALUATE(lcSGrpExp))
        m.cOrder  = ORDHDR.ORDER
        m.cCustPO = ORDHDR.CUSTPO
        lnRate    = ORDHDR.NEXRATE
        =gfRltFld(ORDHDR.CTERMCODE,@laCustDisc,'CTERMCODE')

        *B606740,1 RAE Store CTERMCODE to the new field.[start]
        m.nRldFld = lcRltFld
        *B606740,1 RAE [end]

        SELECT ORDLINE
        SCAN FOR cordtype+order+STR(lineno,6) = 'O' + m.cOrder .AND. ;
                 IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) .AND. ;
                 BETWEEN(DTOS(ORDLINE.COMPLETE),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
          m.dCompDat  = ORDLINE.COMPLETE
          ldStart = IIF(EMPTY(ldStart),m.dCompDat,ldStart)
          ldEnd   = IIF(EMPTY(ldEnd),m.dCompDat,ldEnd)
          ldStart = IIF(ldStart < m.dCompDat,ldStart,m.dCompDat)
          ldEnd   = IIF(ldEnd   > m.dCompDat,ldEnd,m.dCompDat)          
          m.cStyMaj   = SUBSTR(ORDLINE.STYLE,1,lnMajLen)
          m.nPrice    = ORDLINE.PRICE        
          m.nPrice    = ORDLINE.PRICE / lnRate
          m.nTotQty   = ORDLINE.TOTQTY
          lcStyle     = ORDLINE.STYLE
          m.nAve_cost = STYLE.AVE_COST
          SELECT (lcTempFile)
          SET ORDER TO DATE
          IF SEEK(m.cOrder+PADR(m.cStyMaj,19)+DTOS(m.dCompDat),lcTempFile)
            REPLACE nTotQty WITH ORDLINE.TOTQTY + lnTotQty
          ELSE
            INSERT INTO (lcTempFile) FROM MEMVAR
            lnTotQty = m.nTotQty
          ENDIF
          SET ORDER TO (lcTempFile)
        ENDSCAN
      ENDSCAN
    ENDSCAN

  *-- If Style and Customer are not selected
  CASE !llSelCust .AND. !llSelSty
  *-- Set the Relations
  *	ORDHDR
  *     |
  *     |-- CUSTOMER
  *     |-- ORDLINE
  *           |
  *			  |-- STYLE    

    SELECT ORDLINE
    SET ORDER TO TAG Ordline
  
    SELECT ORDHDR
    SET RELATION TO "M" + ACCOUNT INTO CUSTOMER ADDITIVE
    SET RELATION TO "O" + ORDER   INTO ORDLINE  ADDITIVE
    SELECT ORDLINE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
   
    SELECT (lcTempFile)
    SET ORDER TO (lcTempFile) 

    SELECT ORDHDR
    LOCATE
    SCAN REST WHILE cordtype+order = 'O'
                    
      m.cCustCode = ACCOUNT
      m.cCustClas = gfCodDes(CUSTOMER.CLASS,PADR('CLASS',10))
      m.cOrder    = ORDHDR.ORDER
      m.cCustPO   = ORDHDR.CUSTPO
      lnRate      = ORDHDR.NEXRATE
      lcStatus    = ORDHDR.Status
      =gfRltFld(ORDHDR.CTERMCODE,@laCustDisc,'CTERMCODE')

      *B606740,1 RAE Store CTERMCODE to the new field.[start]
      m.nRldFld = lcRltFld
      *B606740,1 RAE [end]

      SELECT ORDLINE
      SCAN FOR &lCRpExp .AND. ;
               cordtype+order+STR(lineno,6) = 'O' + m.cOrder .AND.;
               IIF(EMPTY(lcGrpExp),.T.,EVALUATE(lcGrpExp)) .AND. ;
               BETWEEN(DTOS(ORDLINE.COMPLETE),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
       
        m.cStyMaj   = SUBSTR(ORDLINE.STYLE,1,lnMajLen)
        m.nAve_cost = STYLE.AVE_COST
        m.nTotQty   = ORDLINE.TOTQTY
        m.dCompDat  = ORDLINE.COMPLETE
        ldStart = IIF(EMPTY(ldStart),m.dCompDat,ldStart)
        ldEnd   = IIF(EMPTY(ldEnd),m.dCompDat,ldEnd)
        ldStart = IIF(ldStart < m.dCompDat,ldStart,m.dCompDat)
        ldEnd   = IIF(ldEnd   > m.dCompDat,ldEnd,m.dCompDat)
        m.nPrice    = ORDLINE.PRICE / lnRate
        SELECT (lcTempFile)
        SET ORDER TO DATE
        IF lcStatus = 'O'
          IF SEEK(m.cOrder+PADR(m.cStyMaj,19)+DTOS(m.dCompDat),lcTempFile)
            REPLACE nTotQty WITH ORDLINE.TOTQTY + nTotQty
          ELSE
            INSERT INTO (lcTempFile) FROM MEMVAR
            lnTotQty = m.nTotQty
          ENDIF
        ENDIF
        SET ORDER TO (lcTempFile)
      ENDSCAN
    ENDSCAN
ENDCASE

SELECT (lcTempFile)
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
  LOCATE
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 06/26/2002
*! Purpose   : To set relation on or off when running the in 
*!             range function in the option grid.
*!*************************************************************
*! Called from : SOOPNMRG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************

FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
  SELECT STYLE
  LOCATE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfCustBrw
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 06/26/2002
*! Purpose   : To set relation on or off when running the in 
*!             range function in the option grid.
*!*************************************************************
*! Called from : SOOPNMRG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCustBrw()
*!*************************************************************

FUNCTION lfCustBrw

PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SELECT CUSTOMER
  LOCATE
ENDIF