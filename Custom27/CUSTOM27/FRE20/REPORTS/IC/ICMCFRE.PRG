*:**************************************************************************
*: Name        : ICMCFRE.PRG  
*: TITLE       : Merchandise Control Report                                 
*: ENTRY       : C102335
*: System      : Aria Apparel System (A27).
*: Module      : Inventory Control (IC)
*: Developer   : TMI - TAREK MOHAMED IBRAHIM
*: Date        : 05/27/2001
*:***************************************************************************
*: Calls : FUNCTIONS  : lfwRepWhen()
*:                    : lfFillCode()
*:                    : lfFillOne()
*:                    : lfvGroups()
*:                    : lfvSeasons()
*:                    : lfvColors()
*:                    : lfUpFltExp()
*:                    : lfUpdOneVr()
*:                    : lfClrDtl()
*:                    : lfvDate()
*:                    : lfvpbOk()
*:                    : lfvSetRst()
*:                    : lfGrpTitle()
*:***************************************************************************
*MODIFICATIONS
*B804285,1 TMI 07/17/2001 Some problems in the custom merchandise report.
*C102417,1 TMI 08/14/2001 Some modification in collecting data as follows
*C102417,1                1.Deduct In Transit from OpenWIP
*C102417,1                2.Add shipped qty with no discount to SOLD@PRM
*C102417,1                3.Add shipped qty with discount to SOLD@OP
*C102417,1                4.Add Pattern , Division and Output to comma delimited file to the OG
*C102417,1                The comma delimited file will be used by Excel.
*B804405,1 TMI 09/10/2001 Add the Recap data to the generated excel csv file.
*B804433,1 ABD 10/01/2001 Increase the width of the total recpts by 2 digits.
*B804517,1 BWA 11/06/2001 Fix the bug of omit the decimal from all the fields.[Fix in FRX]
*:***************************************************************************
PRIVATE lcIndexExp,lnCnt
*SET TALK OFF

*--Time start collecting data
lcTimeStrt = LEFT(TIME(),5)

*--lnClrPos ----- > variable hold Color position
*--lnClrLen ----- > variable hold Color Lenght
STOR 0 TO lnClrPos,lnClrLen

*--Get Color position & color lenght
=lfClrDtl()

*--Update the variable lcRpExp
=lfUpFltExp()

*--Create temp table to collect data
*lcColData = gfTempName()  && A record is inserted in syrepuvr for this variable
*--TMI C102335,4 [Start] Add a new fiels to store the first pricea of the style
*CREATE TABLE (gcWorkDir+lcColData) (STYLE     C(19) ,;
                                    CSTYMAJOR C(19) ,;
                                    DESC      C(20) ,;
                                    COLOR     C(36) ,;
                                    COLORCD   C(lnClrLen),;
                                    SEASON    C(6)  ,;
                                    CSTYGROUP C(6)  ,;
                                    OWN       N(6,0),;
                                    PRM       N(6,0),;
                                    OP        N(6,0),;
                                    BalToSell N(6,0),;
                                    RECEPTS   N(6,0),;
                                    ALLOC     N(6,0),;
                                    SHIP      N(6,0),;
                                    InStock   N(6,0),;
                                    OpenWIP   N(6,0),;
                                    InTrans   N(6,0),;
                                    WHSL      N(12,2) )
*B804285,1 TMI [Start] Add a field for type to Calculate the RECAP,
*B804285,1 TMI [Start] Also ADD the description of the season,stygroup,color
*CREATE TABLE (gcWorkDir+lcColData) (STYLE     C(19) ,;
                                    CSTYMAJOR C(19) ,;
                                    DESC      C(20) ,;
                                    COLOR     C(36) ,;
                                    COLORCD   C(lnClrLen),;
                                    SEASON    C(6)  ,;
                                    CSTYGROUP C(6)  ,;
                                    OWN       N(6,0),;
                                    PRM       N(6,0),;
                                    OP        N(6,0),;
                                    BalToSell N(6,0),;
                                    RECEPTS   N(6,0),;
                                    ALLOC     N(6,0),;
                                    SHIP      N(6,0),;
                                    InStock   N(6,0),;
                                    OpenWIP   N(6,0),;
                                    InTrans   N(6,0),;
                                    WHSL      N(12,2),;
                                    WHSL1st   N(12,2) )
*C102417,1 TMI [Start] Comment this line to add a new field 
*CREATE TABLE (gcWorkDir+lcColData) (STYLE     C(19) ,;
                                    CSTYMAJOR C(19) ,;
                                    DESC      C(20) ,;
                                    COLOR     C(36) ,;
                                    COLORCD   C(lnClrLen),;
                                    SEASON    C(6)  ,;
                                    CSTYGROUP C(6)  ,;
                                    OWN       N(6,0),;
                                    PRM       N(6,0),;
                                    OP        N(6,0),;
                                    BalToSell N(6,0),;
                                    RECEPTS   N(6,0),;
                                    ALLOC     N(6,0),;
                                    SHIP      N(6,0),;
                                    InStock   N(6,0),;
                                    OpenWIP   N(6,0),;
                                    InTrans   N(6,0),;
                                    WHSL      N(12,2),;
                                    WHSL1st   N(12,2),;
                                    CTYPE     C(2),;
                                    SEADESC   C(30),;
                                    GRPDESC   C(30),;
                                    CLRDESC   C(30))
*C102417,1 TMI [End  ]
*B804285,1 TMI [End  ]                                    
*--TMI C102335,4 [END  ] Add a new fiels to store the first pricea of the style                                    

*C102417,1 TMI [Start] Add a field LNEWSTYLE to know that a new style comes 
*B804433,1 ABD - Increase the width of the total recpts by 2 digits, Also we will 
*B804433,1 ABD - Increase the OWN fiels because the field hold the total of 3 fields. [Begin]
*CREATE TABLE (gcWorkDir+lcColData) (STYLE     C(19) ,;
                                    CSTYMAJOR C(19) ,;
                                    DESC      C(20) ,;
                                    COLOR     C(36) ,;
                                    COLORCD   C(lnClrLen),;
                                    SEASON    C(6)  ,;
                                    CSTYGROUP C(6)  ,;
                                    OWN       N(6,0),;
                                    PRM       N(6,0),;
                                    OP        N(6,0),;
                                    BalToSell N(6,0),;
                                    RECEPTS   N(6,0),;
                                    ALLOC     N(6,0),;
                                    SHIP      N(6,0),;
                                    InStock   N(6,0),;
                                    OpenWIP   N(6,0),;
                                    InTrans   N(6,0),;
                                    WHSL      N(12,2),;
                                    WHSL1st   N(12,2),;
                                    CTYPE     C(2),;
                                    SEADESC   C(30),;
                                    GRPDESC   C(30),;
                                    CLRDESC   C(30),;
                                    LNEWSTYLE L)

CREATE TABLE (gcWorkDir+lcColData) (STYLE     C(19) ,;
                                    CSTYMAJOR C(19) ,;
                                    DESC      C(20) ,;
                                    COLOR     C(36) ,;
                                    COLORCD   C(lnClrLen),;
                                    SEASON    C(6)  ,;
                                    CSTYGROUP C(6)  ,;
                                    OWN       N(8,0),;
                                    PRM       N(8,0),;
                                    OP        N(8,0),;
                                    BalToSell N(8,0),;
                                    RECEPTS   N(8,0),;
                                    ALLOC     N(8,0),;
                                    SHIP      N(8,0),;
                                    InStock   N(8,0),;
                                    OpenWIP   N(8,0),;
                                    InTrans   N(8,0),;
                                    WHSL      N(12,2),;
                                    WHSL1st   N(12,2),;
                                    CTYPE     C(2),;
                                    SEADESC   C(30),;
                                    GRPDESC   C(30),;
                                    CLRDESC   C(30),;
                                    LNEWSTYLE L)
                                    
*B804433,1 ABD - [End]

*C102417,1 TMI [End  ]

*B804285,1 TMI [Start] Add a field for type to the index
*lcIndexExp = lcRpSortBy+IIF(lcRpSortBy='COLORCD','+SUBSTR(STYLE,1,lnClrPos-2)','+STYLE')
lcIndexExp = 'CTYPE+'+lcRpSortBy+IIF(lcRpSortBy='COLORCD','+SUBSTR(STYLE,1,lnClrPos-2)','+STYLE')  
*B804285,1 TMI [End  ]
INDEX ON &lcIndexExp TAG (lcColData)
SET ORDER TO TAG (lcColData)

*B804285,1 TMI [Start] No default transaction date needed 
*--Reset date range
*ldRpFDt = IIF(!EMPTY(ldRpFDt),ldRpFDt,gdSysDate)
*ldRpTDt = IIF(!EMPTY(ldRpTDt),ldRpTDt,gdSysDate)
*B804285,1 TMI [End  ] 

*--Set orders
SET ORDER TO ORDHDR   IN ORDHDR   && KEY: cordtype+order
SET ORDER TO ORDLINES IN ORDLINE  && KEY: style+DTOS(complete)+cordtype+order+store+STR(lineno,6)
SET ORDER TO INVLINES IN INVLINE  && KEY: style+invoice+STR(lineno,6)
SET ORDER TO POSHDR   IN POSHDR   && KEY: cstytype+po
SET ORDER TO POSLNS   IN POSLN    && KEY: style+cstytype+po+STR(lineno,6)+trancd
*C102417,1 TMI [Start] INVHDR order
SET ORDER TO INVHDR IN INVHDR     && KEY: invoice
*C102417,1 TMI [End  ]
  
*--Collecting data
*--TMI C102335,4 [Start] Saves last style major id
M.lcLastSty = SPACE(19)
M.lc1stPriA = 0.00
*--TMI C102335,4 [End  ]
SELECT STYLE
SET ORDER TO STYLE
*C102417,1 TMI [Start] Define a variable to know that we get a new style
lcNewSty = SPACE(19)
*C102417,1 TMI [End  ]
SCAN FOR &lcRpExp
  *--Initialize values
  STORE 0 TO lnOwn,lnPrm,lnOp,lnRecepts,lnAlloc,lnShip,lnOpenWip,lnIntrans
  STORE '' TO lcColor
  *C102417,1 TMI [Start] Define Two variables to hold Shipped qty's with and with no discount
  STORE 0 TO lnShipPRM,lnShipOP
  *C102417,1 TMI [End  ]
  
  
  *--GET lcColor
  lcColor = ALLTRIM(gfCodDes(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),'COLOR'))+'-'+ALLTRIM(SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen))

  SELECT ORDLINE
  SEEK STYLE.STYLE
  SCAN REST WHILE STYLE=STYLE.STYLE FOR CORDTYPE='O'
    *--GET lnPrm   :: Units sold with no line discount
    *--GET lnOp    :: Units sold with a discount
    =SEEK('O'+ORDLINE.ORDER,'ORDHDR')    
    *B804285,1 TMI [Start] To allow to print data when no date range is specified     
    *IF INLIST(ORDHDR.STATUS,'H','O') AND BETWEEN(ORDHDR.ENTERED,ldRpFDt,ldRpTDt)
    lcDtRang = IIF(EMPTY(ldRpTDt),'.T.','BETWEEN(ORDHDR.ENTERED,ldRpFDt,ldRpTDt)')
    IF INLIST(ORDHDR.STATUS,'H','O') AND EVAL(lcDtRang)
    *B804285,1 TMI [End] 
      IF DISC_PCNT = 0
        lnPrm = lnPrm + ORDLINE.TOTQTY
      ELSE
        lnOP  = lnOP  + ORDLINE.TOTQTY        
      ENDIF
    ENDIF
        
    *--GET lnAlloc :: Number of units allocated
    *B804285,1 TMI [Start] To allow to print data when no date range is specified     
    *IF BETWEEN(ORDLINE.PIKDATE,ldRpFDt,ldRpTDt)
    lcDtRang = IIF(EMPTY(ldRpTDt),'.T.','BETWEEN(ORDLINE.PIKDATE,ldRpFDt,ldRpTDt)')
    IF EVAL(lcDtRang)
    *B804285,1 TMI [End] 
      lnAlloc = lnAlloc + ORDLINE.TOTPIK
    ENDIF
  ENDSCAN

  *--TMI C102335,4 [Start] Consider the case of over recive PO 
  lcPO = SPACE(6)                    && to save PO id's
  DIME laOpenWip[8]            && to save open WIP for each qty for current PO
  STORE 0 TO laOpenWip
  *--TMI C102335,4 [End  ]   
  SELECT POSLN
  SEEK STYLE.STYLE+'P'
  SCAN REST WHILE STYLE+CSTYTYPE=STYLE.STYLE+'P'
    =SEEK('P'+PO,'POSHDR')
    *B804285,1 TMI [Start] To allow to print data when no date range is specified     
    *IF INLIST(POSHDR.STATUS,'H','O') AND BETWEEN(POSHDR.ENTERED,ldRpFDt,ldRpTDt)        
    lcDtRang = IIF(EMPTY(ldRpTDt),'.T.','BETWEEN(POSHDR.ENTERED,ldRpFDt,ldRpTDt)')
    IF INLIST(POSHDR.STATUS,'H','O') AND EVAL(lcDtRang)
    *B804285,1 TMI [End  ]     
      *--TMI C102335,4 [Start] Consider the case of over recive PO 
      IF lcPO<>POSHDR.PO
        lnOpenWip = lnOpenWip + lfGtOpnWip()
        STORE 0 TO laOpenWip         
        lcPO=POSHDR.PO
      ENDIF
      *--TMI C102335,4 [End  ]   
      
      *B804285,1 TMI [Start] To allow to print data when no date range is specified     
      lcDtRang = IIF(EMPTY(ldRpTDt),'.T.','BETWEEN(POSLN.DATE,ldRpFDt,ldRpTDt)')
      *B804285,1 TMI [End  ] 
      
      
      DO CASE
        *--GET lnOpenWip :: Number of work in process units ( on outstanding style purchase orders )
        CASE TRANCD='1'
          *--TMI C102335,4 [Start] Consider the case of over recive PO 
          *lnOpenWip = lnOpenWip + POSLN.TOTQTY
          laOpenWip[1]=laOpenWip[1]+POSLN.QTY1
          laOpenWip[2]=laOpenWip[2]+POSLN.QTY2
          laOpenWip[3]=laOpenWip[3]+POSLN.QTY3
          laOpenWip[4]=laOpenWip[4]+POSLN.QTY4
          laOpenWip[5]=laOpenWip[5]+POSLN.QTY5
          laOpenWip[6]=laOpenWip[6]+POSLN.QTY6
          laOpenWip[7]=laOpenWip[7]+POSLN.QTY7
          laOpenWip[8]=laOpenWip[8]+POSLN.QTY8
          *--TMI C102335,4 [End  ]             

        *B804285,1 TMI [Start] To allow to print data when no date range is specified     
        *CASE TRANCD$'245' AND BETWEEN(POSLN.DATE,ldRpFDt,ldRpTDt)
        CASE TRANCD$'245' AND EVAL(lcDtRang)
        *B804285,1 TMI [End  ] 
          
          *--TMI C102335,4 [Start] Consider the case of over recive PO 
          *lnOpenWip = lnOpenWip - POSLN.TOTQTY
          laOpenWip[1]=laOpenWip[1]-POSLN.QTY1
          laOpenWip[2]=laOpenWip[2]-POSLN.QTY2
          laOpenWip[3]=laOpenWip[3]-POSLN.QTY3
          laOpenWip[4]=laOpenWip[4]-POSLN.QTY4
          laOpenWip[5]=laOpenWip[5]-POSLN.QTY5
          laOpenWip[6]=laOpenWip[6]-POSLN.QTY6
          laOpenWip[7]=laOpenWip[7]-POSLN.QTY7
          laOpenWip[8]=laOpenWip[8]-POSLN.QTY8
          *--TMI C102335,4 [End  ]   
        *--GET lnIntrans :: Number of units In transit
        CASE TRANCD='3'
          lnIntrans = lnIntrans + POSLN.TOTQTY
      ENDCASE   
    ENDIF
    *--GET lnRecepts :: Number of units received ( from Style Purchase Orders )
    *B804285,1 TMI [Start] To allow to print data when no date range is specified     
    *IF TRANCD='2' AND BETWEEN(POSLN.DATE,ldRpFDt,ldRpTDt)
    lcDtRang = IIF(EMPTY(ldRpTDt),'.T.','BETWEEN(POSLN.DATE,ldRpFDt,ldRpTDt)')
    IF TRANCD='2' AND EVAL(lcDtRang)
    *B804285,1 TMI [End  ] 
      lnRecepts = lnRecepts + POSLN.TOTQTY
    ENDIF
  ENDSCAN
  *--TMI C102335,4 [Start] Consider the case of over recive PO 
  *--This line since last PO can not be summed within the loop
  lnOpenWip = lnOpenWip + lfGtOpnWip()
  *--TMI C102335,4 [End  ]   
  
  *C102417,1 TMI [Start] ==> 1.Deduct In Transit from OpenWIP
  lnOpenWip = lnOpenWip - lnIntrans
  *C102417,1 TMI [End  ]  
  
  *--GET lnShip :: Number of units shipped
  SELECT INVLINE
  SEEK STYLE.STYLE  
  *B804285,1 TMI [Start] To allow to print data when no date range is specified     
  *SCAN REST WHILE STYLE=STYLE.STYLE FOR BETWEEN(INVLINE.INVDATE,ldRpFDt,ldRpTDt)
  lcDtRang = IIF(EMPTY(ldRpTDt),'.T.','BETWEEN(INVLINE.INVDATE,ldRpFDt,ldRpTDt)')
  SCAN REST WHILE STYLE=STYLE.STYLE FOR EVAL(lcDtRang)
  *B804285,1 TMI [End  ] 
    *C102417,1 TMI [Start] Exclude voided invoices    
    IF SEEK(INVLINE.INVOICE,'INVHDR')      
      IF INVHDR.STATUS='V'
        LOOP
      ENDIF    
    *C102417,1 TMI [End  ]
        
      lnShip = lnShip + INVLINE.TOTQTY
    
      *C102417,1 TMI [Start] 2.Include shipped qty with no discount to SOLD@PRM
      *C102417,1             3.Include shipped qty with discount to SOLD@OP    
      *C102417,4 TMI [Start] Provided that the order still Open or Hold
      *B804405,1 TMI [Start] Include all Orders as before
      *IF SEEK('O'+INVLINE.ORDER,'ORDHDR') AND ORDHDR.STATUS$'OH'
      *B804405,1 TMI [End  ] 
      *C102417,4 TMI [End  ]
      
      lnShipPRM = lnShipPRM + IIF(INVLINE.DISC_PCNT = 0,INVLINE.TOTQTY,0)
      lnShipOP  = lnShipOP  + IIF(INVLINE.DISC_PCNT # 0,INVLINE.TOTQTY,0)
      
      *C102417,4 TMI [Start]
      *B804405,1 TMI [Start] Remove the ENDIF of the above removed IF statemnt
      *ENDIF
      *B804405,1 TMI [End  ] 
      *C102417,4 TMI [End  ]
    ENDIF
    *C102417,1 TMI [End  ]    
  ENDSCAN
  
  *C102417,1 TMI [Start]
  lnPrm = lnPrm + lnShipPRM     && == 2.Include shipped qty with no discount to SOLD@PRM
  lnOP  = lnOP  + lnShipOP      && == 3.Include shipped qty with discount to SOLD@OP
  *C102417,1 TMI [End  ]
    
  SELECT (lcColData)
  *--TMI C102335,4 [Start] ADD LINE IF AT LEAST ONE ENTRY <> 0 
  IF lnPrm+lnOp+lnRecepts+lnAlloc+lnShip+lnOpenWip+lnIntrans > 0
  *--TMI C102335,4 [END  ] 
    APPEND BLANK
    *--TMI C102335,4 [Start] Fill the field WHSL1st
    *REPLACE STYLE     WITH  STYLE.STYLE,;
            CSTYMAJOR WITH  STYLE.CSTYMAJOR,;
            DESC      WITH  STYLE.DESC,;
            COLOR     WITH  lcColor,;
            COLORCD   WITH  SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),;
            SEASON    WITH  STYLE.SEASON,;
            CSTYGROUP WITH  STYLE.CSTYGROUP,;
            OWN       WITH  lnRecepts+lnOpenWip+lnIntrans,;
            PRM       WITH  lnPrm,;
            OP        WITH  lnOp,;
            BalToSell WITH  OWN-PRM-OP,;
            RECEPTS   WITH  lnRecepts,;
            ALLOC     WITH  lnAlloc,;
            SHIP      WITH  lnShip,;
            InStock   WITH  lnRecepts-lnShip,;
            OpenWIP   WITH  lnOpenWip,;
            INTRANS   WITH  lnIntrans,;
            WHSL      WITH  STYLE.PRICEA               
    *--if this is a new style get 1st pricea
    IF M.lcLastSty<>STYLE.CSTYMAJOR
      M.lcLastSty=STYLE.CSTYMAJOR
      M.lc1stPriA=STYLE.PRICEA
    ENDIF
    REPLACE STYLE     WITH  STYLE.STYLE,;
            CSTYMAJOR WITH  STYLE.CSTYMAJOR,;
            DESC      WITH  STYLE.DESC,;
            COLOR     WITH  lcColor,;
            COLORCD   WITH  SUBSTR(STYLE.STYLE,lnClrPos,lnClrLen),;
            SEASON    WITH  STYLE.SEASON,;
            CSTYGROUP WITH  STYLE.CSTYGROUP,;
            OWN       WITH  lnRecepts+lnOpenWip+lnIntrans,;
            PRM       WITH  lnPrm,;
            OP        WITH  lnOp,;
            BalToSell WITH  OWN-lnPrm-lnOP,;
            RECEPTS   WITH  lnRecepts,;
            ALLOC     WITH  lnAlloc,;
            SHIP      WITH  lnShip,;
            InStock   WITH  lnRecepts-lnShip,;
            OpenWIP   WITH  lnOpenWip,;
            INTRANS   WITH  lnIntrans,;
            WHSL      WITH  STYLE.PRICEA,;               
            WHSL1st   WITH  M.lc1stPriA            
    *--TMI C102335,4 [End  ] Fill the field WHSL1st
    
    *B804285,1 TMI [Start] Fill the field CTYPE with '1 ' for usual line AND DESC fields
    REPLACE CTYPE   WITH '1 ',;
            SEADESC WITH gfCodDes(SEASON,'SEASON'),;
            GRPDESC WITH gfCodDes(CSTYGROUP,'CSTYGROUP'),;
            CLRDESC WITH gfCodDes(COLORCD,'COLOR')
    
    *C102417,1 TMI [Start] Update LNEWSTYLE field
    IF STYLE.CSTYMAJOR # lcNewSty 
      REPLACE LNEWSTYLE WITH .T.
    ENDIF
    lcNewSty = STYLE.CSTYMAJOR
    *C102417,1 TMI [End  ]
    
    *--Fill in lines of type '2 '
    IF llRpRECAP
      SCATTER MEMV MEMO
      M.CTYPE='2 '
      INSERT INTO (gcWorkDir+lcColData) FROM MEMVAR
    ENDIF    
    *B804285,1 TMI [End  ] 

  *--TMI C102335,4 [Start] ADD LINE IF AT LEAST ONE ENTRY <> 0 
  ENDIF   lnPrm+lnOp+lnRecepts+lnAlloc+lnShip+lnOpenWip+lnIntrans>0
  *--TMI C102335,4 [END  ]   

ENDSCAN

*-- Copy generated data to Excel file
IF !lfCopy2Xls()
  RETURN
ENDIF

SELEC (lcColData)
LOCATE

IF RECCOUNT()=0
  =gfDialog("I","No records to display")
ELSE
  *-- Run the report
  DO gfDispRe WITH EVAL('lcRpForm')  
ENDIF

*--Erase created files       
USE IN (lcColData)
ERASE (gcWorkDir+lcColData+'.DBF')
ERASE (gcWorkDir+lcColData+'.CDX')

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/24/2001
*:* Purpose     : When report function
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRepWhen()
*:***************************************************************************
FUNCTION lfwRepWhen
=lfClrDtl()
*-- end of lfwRep.

*:**************************************************************************
*:* Name        : lfFillCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Fill mover arrays in OG
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFillCode()
*:***************************************************************************
FUNCTION lfFillCode
DIME laRpSSea[1,1],laRpTSea[1,1],laRpSGrp[1,1],laRpTGrp[1,1],laRpSClr[1,1],laRpTClr[1,1]
STORE '' TO laRpSSea,laRpTSea,laRpSGrp,laRpTGrp,laRpSClr,laRpTClr
=lfFillOne('laRpSSea','NNSEASON    ')
=lfFillOne('laRpSGrp','NNCSTYGROUP ')
=lfFillOne('laRpSClr','NNCOLOR     ')
*C102417,1 TMI [Start] Fil Division and Pattern arrays
DIMENSION laRpSDiv[1,1],laRpTDiv[1,1],laRpSPat[1,1],laRpTPat[1,1]
STORE '' TO laRpSDiv,laRpTDiv,laRpSPat,laRpTPat
=lfFillOne('laRpSDiv','NNCDIVISION ')
SELECT DISTINCT PATTERN FROM STYLE WHERE !EMPTY(PATTERN) INTO ARRAY laRpSPat
=ASORT(laRpSPat)
*C102417,1 TMI [End  ]
*-- end of lfFillCode.

*:**************************************************************************
*:* Name        : lfFillOne
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Fill passed array names used in OG
*:***************************************************************************
*:* Called from : lfFillCode
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFillOne()
*:***************************************************************************
FUNCTION lfFillOne
PARAMETERS lcSArr,lcCode
PRIVATE lnCnt

SELECT CCODE_NO+' - '+CDISCREP ;
FROM CODES ;
WHERE cdefcode+crltfield+cfld_name=lcCode ;
INTO ARRAY &lcSArr
*-- end of lfFillOne.

*:**************************************************************************
*:* Name        : lfvGroups
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Valid Func. for groups mover
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvGroups()
*:***************************************************************************
FUNCTION lfvGroups
= gfMover(@laRpSGrp,@laRpTGrp,'Select Groups',.T.,'')
*-- end of lfvGroups.

*:**************************************************************************
*:* Name        : lfvSeasons
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Valid Func. for seasons mover
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSeasons()
*:***************************************************************************
FUNCTION lfvSeasons
= gfMover(@laRpSSea,@laRpTSea,'Select Seasons',.T.,'')
*-- end of lfvSeasons.

*:**************************************************************************
*:* Name        : lfvColors
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Valid Func. for colors mover
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvColors()
*:***************************************************************************
FUNCTION lfvColors
= gfMover(@laRpSClr,@laRpTClr,'Select Colors',.T.,'')
*-- end of lfvColors.

*C102417,1 TMI [Start] Add Divisions to OG
*:**************************************************************************
*:* Name        : lfvDivsn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2001
*:* Purpose     : Valid Func. for Divison mover
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvDivsn()
*:***************************************************************************
FUNCTION lfvDivsn
= gfMover(@laRpSDiv,@laRpTDiv,'Select Divisions',.T.,'')
*-- end of lfvDivsn.
*C102417,1 TMI [End  ]

*C102417,1 TMI [Start] Add Pattern to the OG
*:**************************************************************************
*:* Name        : lfvPattern
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2001
*:* Purpose     : Valid Func. for Pattern mover
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvPattern()
*:***************************************************************************
FUNCTION lfvPattern
= gfMover(@laRpSPat,@laRpTPat,'Select Patterns',.T.,'')
*-- end of lfvPattern.
*C102417,1 TMI [End  ]

*:**************************************************************************
*:* Name        : lfUpFltExp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Update the filter expression lcRpExp by mover variables
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpFltExp()
*:***************************************************************************
FUNCTION lfUpFltExp
PRIVATE     lcSeasons,lcGroups,lcColors
STORE '' TO lcSeasons,lcGroups,lcColors

lcRpExp = IIF(!EMPTY(lcRpExp),lcRpExp,'.T.')
=lfUpdOneVr('laRpTSea','STYLE.SEASON',6)
=lfUpdOneVr('laRpTGrp','STYLE.CSTYGROUP',6)
=lfUpdOneVr('laRpTClr','SUBSTR(STYLE.STYLE,'+ALLT(STR(lnClrPos))+','+ALLT(STR(lnClrLen))+')',6)
*C102417,1 TMI [Start] Update lcRpExp by Divisions and Patterns
=lfUpdOneVr('laRpTDiv','STYLE.CDIVISION',6)
=lfUpdOneVr('laRpTPat','STYLE.PATTERN',10)
*C102417,1 TMI [End  ]

*-- end of lfUpFltExp.

*:**************************************************************************
*:* Name        : lfUpdOneVr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Update lcRpExp for one variable
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdOneVr()
*:***************************************************************************
FUNCTION lfUpdOneVr
PARAMETERS lcTArrName,lcField,lnFldWidth
PRIVATE lnCnt , lcAddStr
lcAddStr = ''
FOR lnCnt = 1 TO ALEN(&lcTArrName)
  *lcAddStr = lcAddStr + LEFT(&lcTArrName.[lnCnt],6) + ' | '
  lcAddStr = lcAddStr + PADR(&lcTArrName.[lnCnt],lnFldWidth) + ' | '
NEXT
lcAddStr = IIF(ALLTRIM(lcAddStr)='|','',lcAddStr)
IF !EMPTY(lcAddStr)
  lcRpExp = lcRpExp+' AND '+lcField+' $ "'+lcAddStr + '"'
ENDIF
*-- end of lfUpdOneVr.

*:**************************************************************************
*:* Name        : lfClrDtl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Get color details
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfClrDtl()
*:***************************************************************************
FUNCTION lfClrDtl
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--end function lfGetClrD


*:**************************************************************************
*:* Name        : lfvDate
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Date range.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvDate()
*:***************************************************************************
FUNCTION lfvDate
PRIVATE ldFrom,ldTo,lcCent
STORE {} TO ldFrom,ldTo
IF !EMPTY(ldRpTDt) 
  ldFrom = ldRpFDt
  ldTo   = ldRpTDt
ENDIF
lcCent = SET('CENTU')
SET CENTU ON
lcTitle = 'Dates'
DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
SET CENTU &lcCent
*!*************************************************************
*! Name      : lfvpbOk
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The validation the date range
*!*************************************************************
*! Example   : = lfvpbOk()
*!*************************************************************
FUNCTION lfvpbOk
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
   ldRpFDt = ldFrom
   ldRpTDt = ldTo
   CLEAR READ
ENDIF

*:**************************************************************************
*:* Name        : lfvSetRst 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2001
*:* Purpose     : Set-Reset-valid function for browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSetRst ()
*:***************************************************************************
FUNCTION lfvSetRst 
PARAMETERS lcParm
SELECT STYLE
IF lcParm = "S"
  lcStyOrder = ORDER("STYLE")  
  SET ORDER TO Cstyle  
ELSE  && ELSE Rest case
  SET ORDER TO &lcStyOrder.  
ENDIF
LOCATE
*-- end of lfvSetRst .

*:**************************************************************************
*:* Name        : lfGrpTitle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/08/2001
*:* Purpose     : Get group title
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGrpTitle()
*:***************************************************************************
FUNCTION lfGrpTitle
PRIVATE lcRet
DO CASE 
  CASE lcRpSortBy = 'SEASON'
    lcRet = 'Season:'
  CASE lcRpSortBy = 'CSTYGROUP'
    lcRet = 'Style Group:'
  CASE lcRpSortBy = 'CSTYMAJOR'
    lcRet = 'Style:'
  CASE lcRpSortBy = 'COLORCD'
    lcRet = 'Color:'
ENDCASE
RETURN lcRet 
*-- end of lfGrpTitle.

*:**************************************************************************
*:* Name        : lfGrpDesc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/17/2001
*:* Purpose     : Get group description
*:* Entry       : B804285,1
*:***************************************************************************
*:* Called from : the report
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGrpDesc()
*:***************************************************************************
FUNCTION lfGrpDesc
PRIVATE lcRet
lcRet = ''
DO CASE 
  CASE lcRpSortBy = 'SEASON'
    lcRet = gfCodDes(ALLT(EVAL(lcRpSortBy)),'SEASON')
    
  CASE lcRpSortBy = 'CSTYGROUP'
    lcRet = gfCodDes(ALLT(EVAL(lcRpSortBy)),'CSTYGROUP')
  
  CASE lcRpSortBy = 'COLORCD'
    lcRet = gfCodDes(ALLT(EVAL(lcRpSortBy)),'COLOR')
    
ENDCASE
RETURN lcRet 
*-- end of lfGrpDesc.
*:**************************************************************************
*:* Name        : lfGtOpnWip
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/10/2001
*:* Purpose     : Get Open wip considering over recive case
*:* entry       : C#102335,4
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGtOpnWip()
*:***************************************************************************
FUNCTION lfGtOpnWip
PRIVATE lnCnt,lnWipOpn
lnWipOpn = 0
FOR lnCnt = 1 TO 8
  lnWipOpn = lnWipOpn + IIF(laOpenWip[lnCnt]>0,laOpenWip[lnCnt],0)
NEXT
RETURN lnWipOpn
*-- end of lfGtOpnWip.

*:**************************************************************************
*:* Name        : lfRecapBy
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/26/2001
*:* Purpose     : Prints RECAP BY title 
*:***************************************************************************
*:* Called from : the report
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRecapBy()
*:***************************************************************************
*B804285,1 TMI 07/26/2001 
FUNCTION lfRecapBy
PRIVATE lcRet
DO CASE 
  CASE lcRpSortBy = 'SEASON'
    lcRet = 'SEASON'
  CASE lcRpSortBy = 'CSTYGROUP'
    lcRet = 'STYLE GROUP'
  CASE lcRpSortBy = 'CSTYMAJOR'
    lcRet = 'STYLE'
  CASE lcRpSortBy = 'COLORCD'
    lcRet = 'COLOR'
ENDCASE
RETURN lcRet 
*-- end of lfRecapBy.

*C102417,1 TMI [Start] Select a comma delimited file to output Data
*:**************************************************************************
*:* Name        : lfvExcelFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2001
*:* Purpose     : Select a comma delimited file to output Data
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvExcelFl()
*:***************************************************************************
FUNCTION lfvExcelFl
PRIVATE lnHandle,lnResp,llContinue,lcSaf
*lcRpExcel = ALLTRIM(SUBSTR(lcRpExcel,AT(":",lcRpExcel)-1))
*IF EMPTY(lcRpExcel)
*  RETURN
*ENDIF

*C102417,4 TMI [Start] SET SAFETY ON 
lcSaf = SET('SAFETY')
SET SAFETY ON

*IF !(ALLTRIM(lcOldExcl) == ALLTRIM(lcRpExcel))
*  IF !FILE(lcRpExcel)
*    lcRpExcel = IIF('?'$lcRpExcel,'',lcRpExcel)
    lcRpExcel = PUTFILE('Output Excel file',lcRpExcel,'CSV')
    *C102417,4 TMI [Start] If the user selects a file and press Esc , retain the old value of lcRpExcel
    IF EMPTY(lcRpExcel)
      lcRpExcel = lcOldExcl
    ELSE
      DELETE FILE (lcRpExcel)
    ENDIF
    *C102417,4 TMI [End  ]    
*  ENDIF
*ENDIF
*C102417,4 TMI [Start] Restore old safty setting
SET SAFETY &lcSaf
*C102417,4 TMI [End  ]
*-- end of lfvExcelFl.
*C102417,1 TMI [End  ]

*C102417,1 TMI [Start] When function for Output excel file
*:**************************************************************************
*:* Name        : lfwExcelFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/15/2001
*:* Purpose     : When function for Output excel file
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwExcelFl()
*:***************************************************************************
FUNCTION lfwExcelFl
lcOldExcl = lcRpExcel
*-- end of lfwExcelFl.
*C102417,1 TMI [End  ]


*C102417,4 TMI [Start] Copy generated data to Excel file
*:**************************************************************************
*:* Name        : lfCopy2Xls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/22/2001
*:* Purpose     : Copy generated data to Excel file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCopy2Xls()
*:***************************************************************************
FUNCTION lfCopy2Xls
PRIVATE llXlsCopy,lnResp,lcHeader,lcCSVFile,laFl,lcSafe

*--Copy Generated data to an Excel file
IF EMPTY(lcRpExcel)
  RETURN .T.

ELSE

  *--Save safety setting
  lcSafe = SET('SAFETY')
  SET SAFETY OFF

  SELEC (lcColData)
  LOCATE
  *--Check if the file exists and not empty , if yes display a confirmation message
  llXlsCopy = .F.
  
  DIME laFl[1,5]
  laFl = 0
  =ADIR(laFl,lcRpExcel)
  IF laFl[1,2]=0   && if file size is 0
    llXlsCopy = .T.
  ELSE
    lnResp = gfModalGen('QRM00000B42002','DIALOG',.F.,.F.,;
              'File '+lcRpExcel+' Contains data. Are you sure you want to overwrite?')
    IF lnResp=1
      llXlsCopy = .T.
    ENDIF
  ENDIF
  
  IF llXlsCopy = .T.

    *--Define the header that appear in the excel file
    lcHeader = ["Style","Style Major","Desc.","Color Code","Color-Desc.",]+;
               ["Season","Desc.","Style Group","Desc.",]+;
               ["Units Own" ,"Units Sold @ PRM","Units Sold @ OP","Bal. To Sell"]+;
               [,"Total Recpts","Total Alloc","Total Ship","In Stock","Open Wip","In Trans","WHSL"]

    *--Copy the data,This does not include the header 
    SELEC (lcColData)

    *C102417,4 TMI [Start] Save the old error handler, and set a new local error handler
    PRIVATE lcErrHand,llError
    lnError = 0
    lcErrHand = ON('ERROR')
    ON ERROR lnError = ERROR()
    *C102417,4 TMI [End  ]

    COPY TO (lcRpExcel) DELIMITED FOR CTYPE='1 ' ;
         FIELDS STYLE,CSTYMAJOR,DESC,COLORCD,COLOR,;
                SEASON,SEADESC,CSTYGROUP,GRPDESC,;
                OWN,PRM,OP,BalToSell,;
                RECEPTS,ALLOC,SHIP,InStock,OpenWIP,InTrans,WHSL

    *C102417,4 TMI [Start] Restore the old error handler
    ON ERROR &lcErrHand
    *--Show a message if the file (lcRpExcel) is opened by Excel.
    IF !EMPTY(lnError)
      DO lfFreErrHd
      RETURN .F.
    ENDIF
    *C102417,4 TMI [End  ]

    lcCSVFile = gfTempName()
    
    *--Create a tmp dbf that holds the data with headers
    CREATE CURSOR (lcCSVFile) (CCONTENTS C(250))

    *--Add a line that holds the headers of the Excel columns
    APPEND BLANK
    REPLACE CCONTENTS WITH lcHeader
    
    *--Get the data to the temp dbf from the generated csv file
    APPEND FROM (lcRpExcel) SDF
    
    *B804405,1 TMI [Start] Add the Recap data to the generated Excel csv file.
    IF llRpRECAP      
      *-- Add empty lines
      INSERT INTO (lcCSVFile) VALUES ('')
      INSERT INTO (lcCSVFile) VALUES ('')
      *--Add recap title
      INSERT INTO (lcCSVFile) VALUES ('RECAP BY '+lfRecapBy())
      *--Add empty line
      INSERT INTO (lcCSVFile) VALUES ('')
      *--Create RECAP data Returning the Name of the Temp file contains this data
      lcRecap = lfCrtRECAP()
      *--Add recap data itself
      SELECT (lcCSVFile)
      APPEND FROM (lcRecap) SDF    
      *--Remove (lcRecap) file
      ERASE (lcRecap)
    ENDIF     
    *B804405,1 TMI [End  ]     
    *--Copy data with headers to the csv file (overwrite it)
    COPY TO (lcRpExcel) SDF
    
    *--Release the cursor from the memory
    USE IN (lcCSVFile)
    
  ENDIF
  
  *--Restore safety setting
  SET SAFE &lcSafe   
ENDIF

RETURN llXlsCopy
*--End of lfCopy2Xls.
*C102417,4 TMI [End  ]


*C102417,4 TMI [Start] Define Local error handler
*:**************************************************************************
*:* Name        : FUNCTION lfFreErrHd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/28/2001
*:* Purpose     : Local Error handler
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = FUNCTION lfFreErrHd()
*:***************************************************************************
FUNCTION lfFreErrHd
PRIVATE lcMsg
lcMsg = ''
DO CASE
  CASE lnError=1705   && File access is denied
    lcMsg  = 'The file '+lcRpExcel+' is opened by other application. Please close it and rerun the report.'
  OTHERWISE
    lcMsg = MESSAGE()
ENDCASE
=gfModalGen('INM00000B42000','DIALOG',.F.,.F.,lcMsg)
*-- end of lfFreErrHd.

*B804405,1 TMI [Start] Create RECAP data to be added to the excel csv file.
*:**************************************************************************
*:* Name        : lfCrtRECAP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/10/2001
*:* Purpose     : Create RECAP data to be addedd to the excel csv file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrtRECAP()
*:***************************************************************************
FUNCTION lfCrtRECAP
PRIVATE lnSlct,lcFile,laFlds,lnCnt
lnSlct = SELECT(0)
*--Create tmp file for recap data
lcFile = gfTempName()
SELECT (lcColData)
=AFIELDS(laFlds)
*--Add fields for WHSL for all collected quantities
lcFlds = ''  && Holds names of collected data fields
FOR lnCnt = 8 TO 17
  =lfAddFld('laFlds',laFlds[lnCnt,1]+'W','N',12,2)
  lcFlds = lcFlds + laFlds[lnCnt,1]+','+laFlds[lnCnt,1]+'W'+','
ENDFOR
lcFlds = LEFT(lcFlds,LEN(lcFlds)-1)

CREATE CURSOR (lcFile) FROM ARRAY laFlds
INDEX ON &lcRpSortBy TAG (lcFile)

*--Loop and Fill (lcFile) with RECAP data
SELECT (lcColData)
SEEK '2 '
SCAN WHILE cType = '2 '
  SCATTER MEMVAR
  SELECT (lcFile)
  IF !SEEK(&lcColData..&lcRpSortBy),(lcFile))
    APPEND BLANK
    GATHER MEMVAR
    BLANK FIELDS &lcFlds
  ENDIF
  REPLACE OWN        WITH OWN        + M.OWN              ,;
          OWNW       WITH OWNW       + M.OWN*M.WHSL       ,;
          PRM        WITH PRM        + M.PRM              ,;
          PRMW       WITH PRMW       + M.PRM*M.WHSL       ,;
          OP         WITH OP         + M.OP               ,;
          OPW        WITH OPW        + M.OP*M.WHSL        ,;
          BALTOSELL  WITH BALTOSELL  + M.BALTOSELL        ,;
          BALTOSELLW WITH BALTOSELLW + M.BALTOSELL*M.WHSL ,;
          RECEPTS    WITH RECEPTS    + M.RECEPTS          ,;
          RECEPTSW   WITH RECEPTSW   + M.RECEPTS*M.WHSL   ,;
          ALLOC      WITH ALLOC      + M.ALLOC            ,;
          ALLOCW     WITH ALLOCW     + M.ALLOC*M.WHSL     ,;
          SHIP       WITH SHIP       + M.SHIP             ,;
          SHIPW      WITH SHIPW      + M.SHIP*M.WHSL      ,;
          INSTOCK    WITH INSTOCK    + M.INSTOCK          ,;
          INSTOCKW   WITH INSTOCKW   + M.INSTOCK*M.WHSL   ,;
          OPENWIP    WITH OPENWIP    + M.OPENWIP          ,;
          OPENWIPW   WITH OPENWIPW   + M.OPENWIP*M.WHSL   ,;
          INTRANS    WITH INTRANS    + M.INTRANS          ,;
          INTRANSW   WITH INTRANSW   + M.INTRANS*M.WHSL         
ENDSCAN

*--Calculate grand totals and save it in the last line
SELECT (lcFile)
LOCATE
SCATTER MEMVAR BLANK
SCAN
  FOR lnCnt = 1 TO ALEN(laFlds,1)
    IF laFlds[lnCnt,2]='N'
      &laFlds[lnCnt,1] = EVAL('M.'+laFlds[lnCnt,1]) + EVAL(lcFile+'.'+laFlds[lnCnt,1])
    ENDIF
  ENDFOR  
ENDSCAN
APPEND BLANK
GATHER MEMVAR
*-- Add chr(255) so that the grand total go in the last of the file.
REPLACE &lcRpSortBy WITH CHR(255)

*--Create the final cursor to hold the Needed layout for the excel csv file
*  i.e. a line for total units  and a line for total wasl $

PRIVATE laCSV
DIMENSION laCSV[1,4]
*--Add fields only to reserve columns in the excel file
laCSV[1,1] = 'FLD1'  && this array element since the funciton lfAddFld asumes that the array already contains fields
laCSV[1,2] = 'C'
laCSV[1,3] = 1
laCSV[1,4] = 0
=lfAddFld('laCSV','FLD2','C',20,0)
=lfAddFld('laCSV','FLD3','C',1 ,0)
=lfAddFld('laCSV','FLD4','C',1 ,0)
=lfAddFld('laCSV','FLD5','C',1 ,0)
=lfAddFld('laCSV','FLD6','C',1 ,0)
=lfAddFld('laCSV','FLD7','C',20,0)
=lfAddFld('laCSV','FLD8','C',1 ,0)
=lfAddFld('laCSV','FLD9','C',1 ,0)

*--Add fields to hold data
=lfAddFld('laCSV','FLD10','N',12,2)
=lfAddFld('laCSV','FLD11','N',12,2)
=lfAddFld('laCSV','FLD12','N',12,2)
=lfAddFld('laCSV','FLD13','N',12,2)
=lfAddFld('laCSV','FLD14','N',12,2)
=lfAddFld('laCSV','FLD15','N',12,2)
=lfAddFld('laCSV','FLD16','N',12,2)
=lfAddFld('laCSV','FLD17','N',12,2)
=lfAddFld('laCSV','FLD18','N',12,2)
=lfAddFld('laCSV','FLD19','N',12,2)

lcFile2 = gfTempName()
CREATE CURSOR (lcFile2) FROM ARRAY laCSV

SELECT (lcFile)
LOCATE
SCAN
  *--Fill memory variables to use them in insert into statement
  M.FLD2 = &lcRpSortBy
  M.FLD7 = 'TOTAL UNITS:'
  
  *--Fields contain total units
  FOR lnCnt = 8 TO 17
    lcCnt=LTRIM(STR(lnCnt+2))
    M.FLD&lcCnt = EVAL(lcFile+'.'+laFlds[lnCnt,1])
  NEXT  
  INSERT INTO (lcFile2) FROM MEMVAR

  M.FLD2 = ''
  M.FLD7 = 'TOTAL WHSL$:'
  
  *--fields contain whsl$
  FOR lnCnt = 8 TO 17
    lcCnt=LTRIM(STR(lnCnt+2))
    M.FLD&lcCnt = EVAL(lcFile+'.'+laFlds[lnCnt+17,1])
  NEXT  
  INSERT INTO (lcFile2) FROM MEMVAR

  SELECT (lcFile2) 
  APPEND BLANK
 
ENDSCAN

*--Put the work 'GRAND TOTALS' in the last collected 2 lines
SELECT (lcFile2) 
GO BOTTOM
SKIP -2
REPLACE FLD2 WITH 'GRAND TOTALS'

*--Copy this data to a text file to append it to the lcCSVFile
LOCATE
COPY TO (gcWorkDir+lcFile2) DELIMITED

*--No need dbf files now
USE IN (lcFile)
USE IN (lcFile2)

SELECT (lnSlct)
RETURN gcWorkDir+lcFile2
*--End of lfCrtRECAP.
*B804405,1 TMI [Start]


*B804405 TMI [Start] Add a field to an array
*:**************************************************************************
*:* Name        : lfAddFld
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/10/2001
*:* Purpose     : Add a field to an array
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAddFld()
*:***************************************************************************
FUNCTION lfAddFld
PARAMETERS lcArrName,lcFldName,lcType,lnFdlLen,lnDec
PRIVATE lnLen
lnLen = ALEN(&lcArrName,1)
DIMENSION &lcArrName[lnLen+1,4]
&lcArrName[lnLen+1,1] = lcFldName
&lcArrName[lnLen+1,2] = lcType
&lcArrName[lnLen+1,3] = lnFdlLen
&lcArrName[lnLen+1,4] = lnDec
*--End of lfAddFld.
*B804405 TMI [End  ] 
