*:************************************************************************************
*: Program file       : SOCNVPO.PRG
*: Program desc.      : Convert Style PO to SO.
*: System             : Aria Advantage Series.
*: Module             : Sales Order (SO)
*: Date               : 07/15/2002
*: Developer          : RANIA ABDEL RAZIK (RAE)
*: Tracking Job Number: C200375
*:************************************************************************************
*: Calls :
*:    Functions       : =lfClsScr()    , =lfvDbfDir() , =lfvAccount() , =lfOpenFile()
*:                      =lfCrtTmpFil() , =lfvOK()
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Example            : DO SOCNVPO.PRG
*:************************************************************************************
*: Note               : This programs calls SOUPDATE.PRG to save a new SO.
*:************************************************************************************
*: Modifications ..
*C037344,1 ABD 01/12/2004 Prior to creating a sales order, the system should check that 
*C037344,1 ABD            the style/colours on the PO's selected for conversion exist on 
*C037344,1 ABD            the system.  If any style/colours do not exist on the system, 
*C037344,1 ABD            a list should be given for Preview and Print it.
**************************************************************************************

EXTERNAL ARRAY laData , laKeyField
STORE 'O'  TO lcOrdType
DECLARE laWareHouses[1,2]  , laSetups[6,2] , laKeyField[2,4] , laSeasons[1,2] ,;
        laCodes[1,10]      , laSource[1,2] , laDisRltFld[1,2]     
STORE ''   TO laWareHouses , lcODefWare    , lcWareHouse     , lcScFields ,;
              laSeasons    , laCodes       , lcODefSes       , lcSeason   ,;
              lcCustCode   , lcExact       , lcPricLvl       , lcstyle    ,;
              lcDbfDir
STORE .F.  TO laSetups     , llFound       , llBrowse   ,;
              llFirst      , llMulCurr     , llEditExRt
STORE 1    TO lnWareHouse  , lnOrdStatus   , lnSeason
STORE 0    TO lnOpen       , lnDisc_Pcnt   , lnGros_Prc
STORE 0.00 TO lnOpenAmt    , lnTotAmt

lcOrdHdr  = gfTempName()
lcOrdLine = gfTempName()


*C037344,1 ABD - Define New variables. [Begin]
lcWinTitl  = 'Rejected Style(s)'
lcFilHandl = ''
lcExprtSty = gfTempName()
llOpenRep  = .F.
*C037344,1 ABD - [End]

*-- variables of SOUPDATE.PRG [Start]
DECLARE laVariables[6] , laScrMode[4] , laOrdStatus[2]
STORE 2   TO lnOrdStatus
laOrdStatus[1] = 'Bid      '
laOrdStatus[2] = 'Open     '

STORE .F. TO llContinue , llBomVarnt  , llCDPerL
STORE ''  TO lcFlToUpd  , lcSession   , lcFiles   , laVariables , lcGlYear , lcGlPeriod ,;
             lcExRsin   , lcUntSin    , lcODefDiv , lcScrMode   , lcCurrOrd
STORE {}  TO ldDefOrdDate
STORE 2   TO lnactfolder
lcFlToUpd = gfTempName()
*-- variables of SOUPDATE.PRG [End]

llFirst = .T.
=lfOpenFile()

laSetups[1,1]  = 'M_PACK'           && System has been steup to use packs
laSetups[2,1]  = 'M_STY_COM'        && Edit sales reps commissions at style level
laSetups[3,1]  = 'M_OR_NOTE'        && Edit order lines notepad
laSetups[4,1]  = 'M_LINK_GL'        && System has been linked to GL
laSetups[5,1]  = 'M_WareHouse'      && System has been steup to use multiple warehouses
laSetups[6,1]  = 'M_GenOrNum'       && Generate order number manually
=gfGetMemVar(@laSetups,gcAct_Comp)

laCodes[1,1] = 'SEASON'
laCodes[1,2] = 'laSeasons'
laCodes[1,3] = 'lnSeason'
laCodes[1,4] = ''
laCodes[1,5] = .T.
laCodes[1,6] = .F.
laCodes[1,10] = 'SEASON'
=gfwCodePop(@laCodes,'SEASON','L')

lnSeason   = ASCAN('laSeasons',lcODefSes)
lnSeason   = IIF(lnSeason=0,1,ASUBSCRIPT(laSeasons,lnSeason,1))

llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)
*--Get Default season.
IF llFirst
  =gfwCodePop(@laCodes,'SEASON','D')
ENDIF
*-- variables of SOUPDATE.PRG [Start]
laVariables[1] = 'ldDefOrdDate'
laVariables[2] = 'lcODefSes'  
laVariables[3] = 'lcODefDiv'
laVariables[4] = 'lcODefWare'
laVariables[5] = 'lcScrMode'
laVariables[6] = 'lcCurrOrd'
*-- variables of SOUPDATE.PRG [End]

laDisRltFld[1,1] = 'DISCPCNT'
laDisRltFld[1,2] = 'lnDisc_Pcnt'

llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)

lcScFields = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
             'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
             'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
             'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
             'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
             'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
             'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
             'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS,LFROMWEB'

laKeyField[1,1] = 'lcOrdType'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'ORDHDR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'ORDHDR'
laKeyField[2,4] = 2

=lfCrtTmpFil()

SELECT WAREHOUS
SELECT cDesc,cWareCode FROM WAREHOUS INTO ARRAY laWareHouses
=gfCloseFile('WAREHOUS')

lnWareHouse = ASCAN(laWareHouses,lcODefWare)
lnWareHouse = IIF(lnWareHouse=0,1,ASUBSCRIPT(laWareHouses,lnWareHouse,1))
lcKeyBmp    = gcBmpHome + "ExtKey.BMP"

ON KEY LABEL ESC DO lfClsScr
SELECT (lcOrdHdr)
SCATTER FIELDS &lcScFields TO laData

DO (gcScrDir+gcWinAppl+"\SOCNVPO.SPX")

*!*************************************************************
*! Name       : lfClsScr
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 07/15/2002
*! Purpose    : Valid function of CANCEL bottom.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfClsScr()
*!*************************************************************

FUNCTION lfClsScr

IF USED('CUSTOMER')
  USE IN CUSTOMER
ENDIF
IF USED('POSHDR')
  USE IN POSHDR
ENDIF
IF USED('POSLN')
  USE IN POSLN
ENDIF
IF USED('ORDHDR')
  USE IN ORDHDR
ENDIF
IF USED('ORDLINE')
  USE IN ORDLINE
ENDIF
IF USED('STYLE')
  USE IN STYLE
ENDIF
IF USED('CODES')
  USE IN CODES
ENDIF
IF USED('STYDYE')
  USE IN STYDYE
ENDIF
IF USED('UnCmSess')
  USE IN UnCmSess
ENDIF
IF USED(lcOrdHdr)
  USE IN (lcOrdHdr)
ENDIF
ERASE (gcDef_Path+lcOrdHdr+".DBF")
ERASE (gcDef_Path+lcOrdHdr+".CDX")
ERASE (gcDef_Path+lcOrdHdr+".FPT")
IF USED(lcOrdLine)
  USE IN (lcOrdLine)
ENDIF
ERASE (gcDef_Path+lcOrdLine+".DBF")
ERASE (gcDef_Path+lcOrdLine+".CDX")
ERASE (gcDef_Path+lcOrdLine+".FPT")

*- Abdou
CLEAR READ

*!*************************************************************
*! Name      : lfvDbfDir
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 07/15/2002
*! Purpose   : Valid function of PO directory.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvDbfDir()
*!*************************************************************

FUNCTION lfvDbfDir

lcDbfDir = GETDIR("","Select DBF Directory")
lcDbfDir = ALLT(lcDbfDir)

*--- Check if the last character is '\'
IF !EMPTY(lcDbfDir) .AND. SUBSTR(lcDbfDir,LEN(lcDbfDir)) <> '\'
  lcDbfDir  = lcDbfDir + '\'
ENDIF
SHOW GET lcDbfDir
_CUROBJ = _CUROBJ
_CUROBJ = OBJNUM(lcDbfDir)

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 07/15/2002
*! Purpose   : Validate Order Account
*!*************************************************************
*! Calls     : CUSBROWM
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .F.
*!*************************************************************
*! Example   :  =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount
PRIVATE xAccount

IF (!EMPTY(laData[2]) .AND. !SEEK('M'+laData[2],'CUSTOMER')) OR llBrowse
  xAccount = laData[2]
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  laData[2] = xAccount
ENDIF
llBrowse = .F.

*!*************************************************************
*! Name       : lfOpenFile
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 07/15/2002
*! Purpose    : Open files.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfOpenFile()
*!*************************************************************

FUNCTION lfOpenFile

IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
ENDIF
IF !USED('CUSTOMER')
  =gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
ENDIF

IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
ENDIF
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
ENDIF
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
ENDIF
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
ENDIF
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF
IF !USED('UnCmSess')
  =gfOpenFile(gcDataDir+'UnCmSess',gcDataDir+'TRANS','SH')
ENDIF

*!*************************************************************
*! Name       : lfCrtTmpFil
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 07/15/2002
*! Purpose    : Create temp. file.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfCrtTmpFil()
*!*************************************************************

FUNCTION lfCrtTmpFil

SELECT ORDHDR
lnFileStru = AFIELDS(laFileStru)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

CREATE TABLE (lcOrdHdr) FROM ARRAY laFileStru
INDEX ON cordtype+order TAG (lcOrdHdr)

SELECT ORDLINE
lnFileStru = AFIELDS(laFileStru)
DIMENSION laFileStru[lnFileStru+3,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'nTotAmt'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 10
laFileStru[lnFileStru+2,4] = 2

laFileStru[lnFileStru+3,1] = 'lcSes'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 0

CREATE TABLE (lcOrdLine) FROM ARRAY laFileStru
INDEX ON cordtype+order+STR(lineno,6) TAG 'ORDLINE'

*!*************************************************************
*! Name       : lfvOK
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 07/15/2002
*! Purpose    : Valid function of OK bottom.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfvOK()
*!*************************************************************
FUNCTION lfvOK

PRIVATE lnContinue , lnSave
lnContinue = 1
lnSave = 1
lcWareHouse = laWareHouses[lnWareHouse,2]
lcSeason    = laseasons[lnseason,2]

lcDbfDir = ALLT(lcDbfDir)

DO WHILE EMPTY(lcDbfDir)  OR ;
         EMPTY(laData[2])
  lcMsg1 = ''
  DO CASE
    CASE EMPTY(lcDbfDir)
      lcMsg1 = 'You have to choose the PO Directory.'
    CASE EMPTY(laData[2])
      lcMsg1 = 'You have to choose a Customer Code.'
  ENDCASE
  
  IF !EMPTY(lcMsg1)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg1)
    IF EMPTY(lcDbfDir)
      _CUROBJ = _CUROBJ
      _CUROBJ = OBJNUM(lcDbfDir)
    ELSE
      IF EMPTY(laData[2])
        _CUROBJ = _CUROBJ
        _CUROBJ = OBJNUM(laData[2])
      ENDIF
    ENDIF
    RETURN
  ENDIF
ENDDO
*C200375,4
lcDbfDir = ALLT(lcDbfDir)
*C200375,4

lcMsg2 ='The selected PO directory is incorrect, please select the right path.'
llFound = (FILE(lcDbfDir+'POSHDR.DBF') .AND. FILE(lcDbfDir+'POSLN.DBF'))
IF !llFound
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  _CUROBJ = _CUROBJ
  _CUROBJ = OBJNUM(lcDbfDir)
  RETURN  
ELSE
  lcCustCode = laData[2]
  *-- Are you sure you want to generate a new SO based on the following criteria:
  *   Customer Code:'---', Season:'---' and Warehouses:'---'?
  lnContinue = gfModalGen("QRM32117B00006","DIALOG",lcCustCode+'|'+IIF(lcSeason='*','All',lcSeason)+'|'+lcWareHouse+'|')
  IF lnContinue = 1
    =lfOpnPoFl()
    * add new browse form POSHDR before generating SO
    lcBrowFld = "PO :R :H='PO #', VENDOR :R :H='Vendor', STATUS :R :H='Status',;
                 CPURCODE :R :H='CpurCode', ENTERED :R :H='Entered Date' , ;
                 COMPLETE :R :H='Complete Date'"
    SELECT POSHDR
    *-- CREATE TEMPORARY INDEX.
    lcTmpIndx = gfTempName()
    INDEX ON PO TAG PO OF (gcWorkDir + lcTmpIndx+ '.CDX')
    SET ORDER TO TAG PO OF (gcWorkDir +lcTmpIndx + '.CDX')
    llRetVal = gfrange(lcBrowFld,"POSHDR","PO","FOR !EMPTY(PO)","","","@! XXXXXXXXXXXXXXXXXXX")
    IF llRetVal
      IF SEEK('M'+laData[2],'CUSTOMER')
        SELECT (lcOrdHdr)
        APPEND BLANK
        REPLACE cOrdType  WITH 'O'                ,;
                StName    WITH CUSTOMER.StName    ,;
                Buyer     WITH CUSTOMER.Buyer     ,;
                Phone     WITH CUSTOMER.Phone1    ,;
                Rep1      WITH CUSTOMER.SalesRep  ,;
                Comm1     WITH CUSTOMER.Comm      ,;
                Rep2      WITH CUSTOMER.Rep2      ,;
                Comm2     WITH CUSTOMER.Comm2     ,;
                Disc      WITH CUSTOMER.Disc      ,;
                cdivision WITH POSHDR.cdivision   ,;
                cTermCode WITH CUSTOMER.cTermCode ,;
                ShipVia   WITH CUSTOMER.ShipVia   ,;
                Spcinst   WITH CUSTOMER.Spcinst   ,;
                cCurrcode WITH CUSTOMER.cCurrCode ,;
                Priority  WITH CUSTOMER.Priority  ,;
                Link_Code WITH IIF(EMPTY(CUSTOMER.Link_Code),'DEFDEF',CUSTOMER.Link_Code) ,;
                GL_Sales  WITH IIF(EMPTY(CUSTOMER.cSlsGlLink),'DEF',CUSTOMER.cSlsGlLink)                           
      ENDIF
      *C200375,4
      laData[33] = &lcOrdHdr..cCurrcode
      IF laData[33] = gcBaseCurr
        STORE 1 TO laData[34], laData[50]
      ELSE
        laData[34]=gfChkRate('laData[50]',laData[33],gdSysDate,.T.,.F.,.F.,llEditExRt)
        IF laData[34] = 0
          IF llEditExRt
            * Message : 00262
            * A valid xxx to xxx exchange rate could not be found on xxx.
            * Button : 00000
            * Ok
            =gfModalGen('INM00262B00000','ALERT',ALLTRIM(laData[33])+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(gdSysDate))
          ELSE
            laData[33] = gcBaseCurr
            STORE 1 TO laData[34], laData[50]
          ENDIF
        ENDIF
      ENDIF
    
      lcPricLvl = CUSTOMER.Pricelvl
      *C200375,4
      laData[5]  = &lcOrdHdr..cOrdType
      laData[11] = &lcOrdHdr..cTermCode
      laData[12] = &lcOrdHdr..ShipVia
      laData[13] = &lcOrdHdr..Spcinst
      laData[15] = &lcOrdHdr..cdivision
      laData[16] = &lcOrdHdr..Disc
      laData[20] = &lcOrdHdr..Buyer
      laData[21] = &lcOrdHdr..Phone
      laData[25] = &lcOrdHdr..Priority
      laData[27] = &lcOrdHdr..Rep1
      laData[28] = &lcOrdHdr..Comm1
      laData[29] = &lcOrdHdr..Rep2
      laData[30] = &lcOrdHdr..Comm2
      laData[32] = &lcOrdHdr..Link_Code
      laData[53] = &lcOrdHdr..GL_Sales
     
      SELECT (lcOrdHdr)
      REPLACE cAdd_User WITH gcUser_ID    ,;
              cAdd_Time WITH TIME()       ,;
              dAdd_Date WITH gdSysDate    ,;
              Account   WITH laData[2]    ,;
              cWareCode WITH lcWareHouse  ,;
              Season    WITH lcSeason     ,;
              Entered   WITH gdSysDate    ,;
              Start     WITH gdSysDate    ,;
              Complete  WITH gdSysDate    ,;
              Multi     WITH 'N'          ,;
              Cinsur    WITH 'Y'          ,;
              llok_stat WITH .F.          ,;
              lFromweb  WITH .F.          ,;
              Status    WITH 'O'          ,;
              MultiPO   WITH .F.          ,;
              Nexrate   WITH laData[34]
     
      STORE gdSysDate TO laData[8] , laData[9] , laData[10]  
      STORE 'N' TO laData[6] , laData[23] , laData[24]  
      STORE .F. TO laData[7]
      STORE 'Y' TO laData[22]
      laData[31] = lcWareHouse
      laData[14] = &lcOrdHdr..Season
     
      SELECT STYLE
      SET ORDER TO TAG Style
      SELECT (lcOrdLine)
      SET ORDER TO TAG 'ORDLINE' IN (lcOrdLine)
      SELECT (lcOrdHdr)
      SET RELATION TO 'O'+ &lcOrdhdr..order INTO (lcOrdline) ADDITIVE
      LOCATE
      SELECT POSLN
      SCAN FOR cstytype+po+style+STR(lineno,6)+trancd = 'P' .AND. SEEK(PO,'POSHDR')
        *C037344,1 ABD - Check for the cnverted style if exist into style file or not. [Begin]
        IF !SEEK(POSLN.Style,'STYLE')
          = lfRjctStyl ()
          LOOP
        ENDIF
        *C037344,1 ABD - [End]
        
        SELECT (lcOrdLine)
        APPEND BLANK
        REPLACE cOrdType   WITH 'O'              ,;
                Account    WITH laData[2]        ,;
                cWareCode  WITH lcWareHouse      ,;
                Style      WITH POSLN.Style      ,;
                Scale      WITH POSLN.Scale      ,;
                Prepak     WITH POSLN.Prepak     ,;
                PpQty      WITH POSLN.PpQty      ,;
                Qty1       WITH POSLN.Qty1       ,;
                Qty2       WITH POSLN.Qty2       ,;
                Qty3       WITH POSLN.Qty3       ,;
                Qty4       WITH POSLN.Qty4       ,;
                Qty5       WITH POSLN.Qty5       ,;
                Qty6       WITH POSLN.Qty6       ,;
                Qty7       WITH POSLN.Qty7       ,;
                Qty8       WITH POSLN.Qty8       ,;
                TotQty     WITH POSLN.TotQty        
      
        lcstyle = &lcOrdLine..Style
        IF SEEK(&lcOrdLine..Style,'STYLE')
          SELECT (lcOrdLine)
          REPLACE Desc1      WITH STYLE.Desc1   ,;
                  lcSes      WITH STYLE.Season
          *C200375,4
          DO CASE
            CASE lcPricLvl = 'A'
              lnGros_Prc = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Pricea,gfStyPrice(lcStyle,'A',laData[33]))
              SELECT (lcOrdLine)
              REPLACE Gros_Price WITH lnGros_Prc
            CASE lcPricLvl = 'B'
              lnGros_Prc = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Priceb,gfStyPrice(lcStyle,'B',laData[33]))
              SELECT (lcOrdLine)
              REPLACE Gros_Price WITH lnGros_Prc
            CASE lcPricLvl = 'C'
              lnGros_Prc = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Pricec,gfStyPrice(lcStyle,'C',laData[33]))
              SELECT (lcOrdLine)
              REPLACE Gros_Price WITH lnGros_Prc
            CASE lcPricLvl ='Q'
              DO CASE
                CASE Style.nAtQtyC > 0
                  lnGros_Prc = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Pricec,gfStyPrice(lcStyle,'C',laData[33]))
                  SELECT (lcOrdLine)
                  REPLACE Gros_Price WITH lnGros_Prc
                CASE Style.nAtQtyB > 0
                  lnGros_Prc = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Priceb,gfStyPrice(lcStyle,'B',laData[33]))
                  SELECT (lcOrdLine)
                  REPLACE Gros_Price WITH lnGros_Prc
                OTHERWISE
                  SELECT (lcOrdLine)
                  REPLACE &lcOrdLine..Gros_Price WITH IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Pricea,gfStyPrice(lcStyle'A',laData[33]))
              ENDCASE
          ENDCASE
        ENDIF
        lcDiscCode  = IIF(SEEK(&lcOrdLine..Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')      
        IF !EMPTY(ALLTRIM(lcDiscCode))
          *-- Get the disecound related filed to now which 
          *-- type whole Sale Or Retail sale Or Both.
          DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
          STORE '' To lcDisType , ldstartDte ,ldEndDate
      
          *-- Array to get the Discount affect for DecCode.
          laDisType[1,1]  = 'CCOSTAFECT'
          laDisType[1,2]  = 'lcDisType'
      
          *-- Array to get the start date For DescCode.
          lastartDte[1,1] = 'START'
          lastartDte[1,2] = 'ldstartDte'
               
          *-- Array to get the end date For DescCode.
          laEndDate[1,1]  = 'DENDATE'
          laEndDate[1,2]  = 'ldEndDate'
          = gfRltFld(lcDiscCode , @laDisType , 'CDISCCODE')
          = gfRltFld(lcDiscCode , @lastartDte, 'CDISCCODE')
          = gfRltFld(lcDiscCode , @laEndDate , 'CDISCCODE')        
          IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
            lnDisc_Pcnt = 0
            =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
            SELECT (lcOrdLine)
            REPLACE Disc_pcnt WITH lnDisc_Pcnt
            lnPrice = &lcOrdLine..Gros_Price*(100-lnDisc_Pcnt)/100
            REPLACE Price WITH lnPrice
          ELSE
            SELECT (lcOrdLine)
            lnPrice = &lcOrdLine..Gros_Price*(100-lnDisc_Pcnt)/100
            REPLACE Price WITH lnPrice                  
          ENDIF        
        ELSE
          lnDisc_Pcnt = 0
          SELECT (lcOrdLine)
          lnPrice = &lcOrdLine..Gros_Price*(100-lnDisc_Pcnt)/100
          REPLACE Price WITH lnPrice        
        ENDIF
        *C200375,4
      
        lnTotAmt = &lcOrdLine..TotQty * &lcOrdLine..Gros_Price
        REPLACE nTotAmt WITH lnTotAmt
      ENDSCAN
   
      SELECT (lcOrdLine)
      SUM ALL TotQty , nTotAmt TO lnOpen , lnOpenAmt
   
      SELECT (lcOrdHdr)
      REPLACE Book    WITH lnOpen    ,;
              BookAmt WITH lnOpenAmt ,;
              Open    WITH lnOpen    ,;
              OpenAmt WITH lnOpenAmt
   
      laData[35] = &lcOrdHdr..Book
      laData[36] = &lcOrdHdr..BookAmt
      laData[41] = &lcOrdHdr..Open
      laData[42] = &lcOrdHdr..OpenAmt
  
      laScrMode[4] = .T.
      *C200375,4
      
      *C037344,1 ABD - Function to Print the rejected styles while converting. [Begin]
      IF llOpenRep
        = lfPrtRjSty ()
      ELSE
        *C037344,1 ABD - [End]  
        IF !SEEK(lcOrdType,lcOrdLine)
          *-- Message : 32044
          *-- No lines entered! No updates performed.
          *-- Button : 00000 
          *-- Ok
          =gfModalGen('TRM32044B00000','DIALOG')
        ELSE
          *C037344,1 ABD - [End]
          IF lcSeason <> '*'
            SELECT (lcOrdline)
            SET RELATION TO style INTO Style ADDITIVE
            SELECT (lcOrdLine)
            LOCATE FOR lcSes <> lcSeason    
            IF FOUND()
              lcMsg3 = ' Some styles belong to seasons different from the one you selected. Do you want to continue?'
              lnSave = gfModalGen("QRM00000B00006","DIALOG",.F.,.F.,lcMsg3)
              IF lnSave = 1
                DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
              ENDIF
            ELSE
              DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
            ENDIF
            SELECT (lcOrdLine)
            SET RELATION TO
          ELSE
            DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
          ENDIF
        
          *C037344,1 ABD - End if for if statment. [Begin]
        ENDIF
      ENDIF
      *C037344,1 ABD - [End]
      *C200375,4
      SELECT (lcOrdHdr)
      SET RELATION TO
      llFirst = .F.
      =lfClsScr()
      CLEAR READ
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name       : lfOpnPoFl
*: Developer  : Rania Abdel Razik (RAE)
*: Date       : 09/09/2002
*! Purpose    : Open POSHDR and POSLN files from the selected 
*!              path.
*!*************************************************************
*! Calls      :
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfOpnPoFl()
*!*************************************************************

FUNCTION lfOpnPoFl
PRIVATE lnOldAls
lnOldAls = SELECT(0)

IF USED('POSHDR')
  USE IN POSHDR
ENDIF
USE (lcDbfDir+"POSHDR") SHARED IN 0 ORDER Poshdr

IF USED('POSLN')
  USE IN POSLN
ENDIF
USE (lcDbfDir+"POSLN") SHARED IN 0 ORDER POSLN
SELECT(lnOldAls)

*C037344,1 ABD - Open Scale and style file from the export path. [Begin]
*-- Open the Scale File From the Export Directory.
IF !USED('SCALE')
  =gfOpenFile(lcDbfDir+'SCALE',lcDbfDir+'SCALE','SH')
ENDIF

*-- Open the Style File From the Export Directory.
=gfOpenFile(lcDbfDir+'STYLE',lcDbfDir+'STYLE','SH',@lcExprtSty,.T.)
*C037344,1 ABD - [End]


*C037344,1 ABD - [Begin]
*!*************************************************************
*! Name       : lfRjctStyl
*: Developer  : Abdou Elgendy (ABD)
*: Date       : 01/12/2004
*! Purpose    : Save the current rejected style.
*!*************************************************************
*! Calls      : Nonw.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfRjctStyl()
*!*************************************************************
*C037344,1 ABD
*!
FUNCTION lfRjctStyl
PRIVATE lnPrv_Als

lnPrv_Als = SELECT(0)

IF !llOpenRep
  llOpenRep = .T.
  lcFilHandl = FCREAT('&gcWorkDir.ReBalLog.txt')
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,"*Styles/Colours missingfrom database                               *")
  =FPUTS(lcFilHandl,"*PO Number STYLE - Color          Sizes                       Total*")
  =FPUTS(lcFilHandl,REPLICATE('*',68))
ENDIF
= SEEK(Posln.Style,lcExprtSty)
= SEEK('S'+&lcExprtSty..Scale,'SCALE')

lcMessage1 = ' '+Posln.PO + SPACE(4)+ Posln.Style + PADR(Scale.Sz1,4) + PADR(Scale.Sz2,4) + PADR(Scale.Sz3,4) + PADR(Scale.Sz4,4) + ;
            PADR(Scale.Sz5,4)+ PADR(Scale.Sz6,4)+ PADR(Scale.Sz7,4)+ PADR(Scale.Sz8,4)
            
lcMessage2 = SPACE (27)        + STR(Posln.Qty1,4) +' '+ STR(Posln.Qty2,4) + STR(Posln.Qty3,4) + STR(Posln.Qty4,4) +;
             STR(Posln.Qty5,4) + STR(Posln.Qty6,4) + STR(Posln.Qty7,4) +STR(Posln.Qty8,4) + '  '+STR(Posln.TotQty,5)

=FPUTS(lcFilHandl,lcMessage1)
=FPUTS(lcFilHandl,lcMessage2)
SELECT(lnPrv_Als)
*-- End OF lfRjctStyl
*!*************************************************************
*! Name       : lfPrtRjSty
*: Developer  : Abdou Elgendy (ABD)
*: Date       : 01/12/2004
*! Purpose    : Save the current rejected style.
*!*************************************************************
*! Calls      : Nonw.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfPrtRjSty()
*!*************************************************************
*C037344,1 ABD.
*!
FUNCTION lfPrtRjSty
PRIVATE lnPrv_Als


lnPrv_Als = SELECT(0)
=FPUTS(lcFilHandl,REPLICATE('*',68))
=FFLUSH(lcFilHandl)
* Close the text file
DO WHILE !FCLOSE(lcFilHandl)
ENDDO

CREATE CURSOR TMPSTR (mRjStyle M(10))
APPEND BLANK
APPEND MEMO mRjStyle FROM (gcWorkDir+"ReBalLog.txt")

DO (gcScrDir + 'SO\SORJSTY.SPX')

SELECT(lnPrv_Als)
*-- End Of lfPrtRjSty
*!*************************************************************
*! Name       : lfVPrnt
*: Developer  : Abdou Elgendy (ABD)
*: Date       : 01/12/2004
*! Purpose    : Save the current rejected style.
*!*************************************************************
*! Calls      : Nonw.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  =lfVPrnt()
*!*************************************************************
*C037344,1 ABD.
*!
FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mRjStyle TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*-- End of lfVPrnt
*C037344,1 ABD - [End]
*!*************************************************************