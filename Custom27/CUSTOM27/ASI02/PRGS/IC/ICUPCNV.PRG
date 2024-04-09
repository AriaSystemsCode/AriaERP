*****************************************************************************
*: Program file  : ICUPCNV.PRG
*: Program desc. : Program To convert excel tabel to Data Base tabel.
*: System        : Aria Apparel System (A27).
*: Module        : Inventory Contol  (IC)
*: Developer     : WAB - Walid A. Wahab Due to C#102103,1
*: Date          : 01/24/2001
*:**************************************************************************
*: Calls 
*:               : FUNCTIONS  : lfOpenFile()
*:								lfCreatFle()
*:								lfOldValue()
*:								lfPstDate()
*:								lfvGetFile()
*:								lfvProceed()
*:								lfCrtPhscal()
*:								lfInvUpdt()
*:								lfCloseTmp()
*:								lfCrtRjct()
*:
*:               -----------------------------------------------------------
*:               : PROCEDURE  : 
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*-- the main object from this report that convret an excel sheet contain upcno and 
*-- ware house code and qty to create phisical inventorey with this qty
*: Passed Parameters  : None
*:**************************************************************************
*: Modifications :
*: *E301558,1 02/12/2001 AHM Send product Activity
*:**************************************************************************

PRIVATE lcTempUpc,lcRejReas,lcFileName,lcReason,ldPstDate,lcOldValue,lcTmpAdj,llGlLink,;
        llMultiWH,lcCostMth,lnRejCont,lcSysType,lcTmpGlDt,lcAReason,laAdjCode,laCodInfo,;
        lnAdjCode          


*--check if the setup use UPC is YES
IF ALLTRIM(gfGetMemVar('M_UPC_USE')) <> 'Y'
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"System doesn't use UPC")
  RETURN
ENDIF


*--lcSysType  ---> hold the system type ( vack office , point of sale
*--lcTempUpc  ---> hold the upc temp file name after convert from excel file
*--lcTmpGlDt  ---> hold the gldist temp file
*--lcReason   ---> hold the phiscal inventory reason
*--ldPstDate  ---> hold the posted date
*--lcFileName ---> hold the path for the excel file contain the upc no
*--lcOldValue ---> hold the old value before the validation 
*--lcTmpAdj   ---> hold the file name contain the style and qty to create phiscal inventory
*--llGlLink   ---> hold .T. if linked to GL module
*--llMultiWH  ---> hold .T. if multy warehouses
*--lcCostMth  ---> hold cost method type
*--llChkDate  ---> hold true to validate the posted date
*--lnRejCont  ---> hold no of record created in the rejected file
*--llCmInstld ---> hold .T. if Cominication module installed
*--laAdjCode  ---> hold the code adjustment codes 
*--lnAdjCode  ---> hold the number of code element select from the popup
*--laCodInfo  ---> hold codes infomation nedd it for gfwCodePop()
*--lcAReason  ---> hold the select adjustment code

DECLARE laAdjCode[1,1],laCodInfo[1,10]
STORE "" TO lcFileName ,lcTempUpc,lcRejReas,lcOldValue,lcReason,lcAReason,laAdjCode,laCodInfo 
STORE gdSysDate TO ldPstDate

lnAdjCode = 1


*--get system type ( back office , point of sale
lcSysType = gfGetMemVar('M_SYSTYPE')
llChkDate = .F.
lnRejCont = 0 
lcTempUpc  = gfTempName()
lcRejReas  = gfTempName()
lcTmpAdj   = gfTempName()
lcTmpGlDt  = gfTempName()
lcCostMth  = gfGetMemVar('M_Cost_Meth')
llMultiWH  = gfGetMemVar('M_WareHouse')='Y'
llGlLink   = gfGetMemVar('M_Link_GL')  ='Y'
llCmInstld = (OCCURS('NC',gcCmpModules)<>0)


*-- open needed files
=lfOpenFile()

IF llGlLink
  laCodInfo[1,01] = "CADJREASON"
  laCodInfo[1,02] = "laAdjCode"
  laCodInfo[1,03] = "lnAdjCode"
  laCodInfo[1,04] = ""
  laCodInfo[1,05] = .F.
  laCodInfo[1,06] = .F.
  laCodInfo[1,07] = ""
  laCodInfo[1,08] = ""
  laCodInfo[1,09] = ""
  laCodInfo[1,10] = "cAdjReason"
  =gfwCodePop(@laCodInfo, "CADJREASON", "D") 
  lcAReason = laAdjCode[1,2]
ENDIF 

*-- Creat temp files
=lfCreatFle()

*--call the convert screens
DO (gcScrDir+gcWinAppl+"\ICUPCCNV.SPX")

*--call function to erase temp file
=lfCloseTmp()

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Open files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpenFile()
*!*************************************************************
*
FUNCTION lfOpenFile

=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'Styupcn','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'UPCSEQ',gcDataDir+'UPCSEQ','SH')
=gfOpenFile(gcDataDir+'CODESET',gcDataDir+'FILDNAME','SH')
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
=gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
=gfOpenFile(gcDataDir+'INVTADJ',gcDataDir+'INVTADJ','SH')


*!*************************************************************
*! Name      : lfCreatFle
*! Developer : Hend Ghanem (HBG)
*! Date      : 05/09/2000
*! Purpose   : Creat temp files
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCreatFle()
*!*************************************************************
*
FUNCTION lfCreatFle

*-- create temp file  to convert from excel  sheet
CREATE TABLE (gcWorkDir+lcTempUpc) (cWareCode C(6), nQty N(7), cUpcNo C(13))
INDEX ON UPPER(cWareCode)+cUpcNo TAG (lcTempUpc)

*-- create temp file for holding the style and qty for phisical inventory
SELECT INVTADJ
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
INDEX ON STYLE+cFromWare TAG (lcTmpAdj)

*--create temp file for reject records
CREATE TABLE (gcDataDir+lcRejReas) (Type C(1),cUpcNo C(13), cWareCode C(6),Reason C(30))
INDEX ON Type+cUpcNo TAG (lcRejReas)

IF llGlLink
  =gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')
  *-- Open a temp file to be used in calling gl distributer proc.
  COPY STRU TO &gcWorkDir.&lcTmpGlDt
  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF
*--End of lfCreatFle



*!*************************************************************
*! Name      : lfOldValue
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfoldvalue()
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN


*:*************************************************************
*! Name    : lfPstDate
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose : Valid Posting date.
*:*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfPstDate()
*!*************************************************************
FUNCTION lfPstDate

*-- If the user pressed "Enter" and did not use the mouse..
*-- We usually use this condition to validate all the key fields..
IF !MDOWN()
  IF ldPstDate<>lcOldValue
    llChkDate=.T.
  ENDIF
  IF llChkDate AND !CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod','IP')
    _CUROBJ = OBJNUM(ldPstDate)
    RETURN
  ENDIF
  llChkDate = .F.
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvGetFile
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : Get the file to be convert
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvGetFile()
*!*************************************************************
*
FUNCTION lfvGetFile

lcFileName = GETFILE('XLS','Select File to be convert','Convert')
=lfRefresh('ICUPCCNV')
*--End of lfvGetFile


*!*************************************************************
*! Name      : lfvProceed
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : Get the file to be convert
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed
PRIVATE llError ,llValid , lcStyle ,lcSize ,lcOnError

IF llChkDate AND !CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod','IP')
    _CUROBJ = OBJNUM(ldPstDate)
    RETURN
ENDIF
  
llError = .F.
lcOnError = ON('ERROR')
ON ERROR llError = .T.
SELECT (lcTempUpc)

*--convert from the excel file
WAIT 'Prepare data. Please wait...' WINDOW NOWAIT
APPEND FROM &lcFileName FOR !EMPTY(&lcTempUpc..cwarecode)  TYPE XLS
WAIT CLEAR
IF llError
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"Invalid Excel file or invalid format. Make sure that you select Excel file format 4.0,3.0, or 2.1")
  lcFileName = ""
  =lfRefresh('ICUPCCNV')
  RETURN
ENDIF
ON ERROR &lcOnError

IF SEEK("LOCATI") 
  BLANK
  DELETE
ENDIF

*-- scan the temp file after the converting and 
*-- check if we have the warecode  in the warehouse file 
*-- and in case of the system type is point of sale we must check if the the warehouse
*-- have the same siteId 
*-- after that we check if we have the upc no in the StyleUpc file
*-- if all above checking is true so we create on record for the style in the temp file
*-- contain the style and warecode and all sizes and the total qty cause the converison file
*-- have a one record for every style/color/size
*-- if any of all above checking is False we create record in the reject file with the reason
SELECT (lcTempUpc)

lnRejCont = 0 
SCAN
  IF SEEK(cWareCode,"WAREHOUS") 
    IF lcSysType<>"P" .OR. !llCmInstld .OR. (lcSysType = "P" .AND. WAREHOUS.cSiteId = gcCurSite)
      IF SEEK(PADR(cUpcNo,12),"STYLEUPC")
        lcStyle = STYLEUPC.STYLE
        lcSize  = SUBSTR(ALLTRIM(STYLEUPC.SIZE),1)
        = SEEK(lcStyle,'STYLE')
        SELECT (lcTmpAdj)
        IF !SEEK(lcStyle+&lcTempUpc..cWAreCode)
          APPEND BLANK
          REPLACE STYLE      WITH lcStyle					            ,;
                  cFromWare  WITH UPPER(&lcTempUpc..cWAreCode),;
                  cReason    WITH lcReason 					          ,;
                  DATE 		   WITH ldPstDate                   ,;
                  DPOSTDATE  WITH ldPstDate                   ,;
                  TYPE       WITH 'P' 						            ,;
                  cAdjReason WITH lcAReason								,;                            
                  UNT_COST   WITH IIF(lcCostMth<>'S',STYLE.Ave_Cost,STYLE.TotCost) ,;
                  OLD_COST   WITH UNT_COST
        ENDIF
        REPLACE ADJ&lcSize WITH ADJ&lcSize + &lcTempUpc..nQty
        REPLACE TOTADJ     WITH TOTADJ + &lcTempUpc..nQty
      ELSE
        =lfCrtRjct('A')
      ENDIF
    ELSE
      =lfCrtRjct('B')
    ENDIF
  ELSE
    =lfCrtRjct('C')
  ENDIF
ENDSCAN  

*-- call function to create  phisical transaction 
SELECT (lcTmpAdj)
LOCATE
IF !EOF()
  =lfCrtPhscal()
ENDIF
IF lnRejCont > 0  
  SELECT (lcRejReas)
  LOCATE
  lcFIle = LEFT(lcFileName,RAT('\',lcFileName))+'RjReason.XLS'
  IF FILE(lcFIle)
    ERASE (lcFile)
  ENDIF
  EXPORT TO (LEFT(lcFileName,RAT('\',lcFileName))+'RjReason.XLS') FIELDS cUpcNo , cWareCode ,Reason  TYPE XLS
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,"One or more records have been rejected. Rejection reasons have been reported and stored in "+lcFile)
ELSE
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'Physical Inventory has been created successfully.')
ENDIF
WAIT CLEAR
CLEAR READ
RETURN
  




*!*************************************************************
*! Name      : lfCrtPhscal
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : create phisical transaction
*!*************************************************************
*! Calls     : lfInvUpdt()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCrtPhscal()
*!*************************************************************
FUNCTION lfCrtPhscal
PRIVATE laToSave,laTemp,lnOldStk,lnOldCost,lcLinkCode,lnTotStk,lcGlSess

*--laToSave   ---> hold the current stock qty per size
*--lnOldStk   ---> hold current total stock before create the phisical transaction
*--lnOldCost  ---> hold current cost before create the phisical transaction
*--lcLinkCode ---> hold Gl link code 
*--lnTotStk   ---> hold the current total stock qty
*--lcGlSess   ---> hold gl session 

*--Save to master files.
DIMENSION laToSave[8],laTemp[8]

SELECT (lcTmpAdj)
SCAN
  = SEEK(Style,'STYLE')
  IF !SEEK( Style + cFromWare + SPACE(10),'STYDYE')
    DO gpAdStyWar WITH &lcTmpAdj..Style,SPACE(10),&lcTmpAdj..cFromWare
  ENDIF
  SELECT (lcTmpAdj)
  *-- Store Style old cost, Old stock, And link code.
  SELECT Style
  lnOldStk   = TotStk
  lnOldCost  = IIF(lcCostMth<>'S',Ave_Cost,TotCost)  
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  SELECT (lcTmpAdj)

  *--Saving the old quantity in the master file
  *--and the checking of the FROM stock.
   
  SELECT IIF(!llMultiWH , 'Style', 'StyDye')
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk
  SELECT (lcTmpAdj)
  GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
                              OldQty5,OldQty6,OldQty7,OldQty8
  REPLACE TotOld    WITH lnTotStk
  REPLACE dPostDate WITH ldPstDate
  
  WAIT WINDOW 'Start updating => '+ALLTRIM(Style)  NOWAIT

  *-- Store WAREHOUSE link code. If warehouse link code is empty default
  *-- warehouse link code to style link code. If the last one is empty
  *-- default to 'DEF' link code.
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")
  *--Start Updating.
  IF ! lfInvUpdt()
    SELECT (lcTmpAdj)
    LOOP
  ENDIF
  SELECT (lcTmpAdj)
  REPLACE cSession WITH STYINVJL.cSession
  SELECT (lcTmpAdj)
ENDSCAN

*--Update master adjustment file.
WAIT WINDOW ' Updating the master adjustment file.' NOWAIT
USE IN (lcTmpAdj)
SELECT InvtAdj
APPEND FROM &gcWorkDir.&lcTmpAdj FOR !DELETED()

*-- Update distripution master file
IF llGlLink 
  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
  SELECT (lcTmpGlDt)
  *-- Generate a unique session number.
  lcGlSess = gfsequence('GLSESSION')
  REPLACE ALL GLSESSION WITH lcGlSess
  SELECT GLDIST  
  APPEND FROM &gcWorkDir.&lcTmpGlDt     
ENDIF  
WAIT CLEAR


*!*************************************************************
*! Name      : lfInvUpdt
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : update the inventory
*!*************************************************************
*! Calls     : gfStyCrl()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfInvUpdt()
*!*************************************************************
FUNCTION lfInvUpdt
PRIVATE lcAdjAcct,lcAdjReason,laGLDistAr,lcAdjWareH,laAdjust,lnACost,lcAdjTyp,lcRefer,lnRet

*--lcAdjAcct   ---> hold the adjusment account
*--lcAdjReason ---> hold the adjustment reason
*--laGLDistAr  ---> hold the G/L Array difinition
*--lcAdjWareH  ---> hold the adjusment warecode
*--laAdjust    ---> hold the phisical qty 
*--lnACost     ---> hold the cost for the phiscal qty
*--lcAdjTyp    ---> hold type '2' phicical type ( in global function style control)
*--lcRefer     ---> hold the reference 
*--lnRet       ---> the return value from the gfStyctr function

SELECT (lcTmpAdj)
*--Gl adjustment account.
lcAdjAcct = ' '

IF llGlLink AND !EMPTY(cAdjReason)
  lcAdjReason = cAdjReason
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF


*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IP'      TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPstDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF


SELECT (lcTmpAdj)
*--Adjustment quantity array TRANSFER Case.

*--Warehouse Code.
lcAdjWareH = &lcTmpAdj..cFromWare

*--Adjustment quantity array.
DECLARE laAdjust[9]
SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj TO laAdjust
*--Adjustment cost.
lnACost = IIF(TotAdj>0,Unt_Cost,Old_Cost)
*--Type of the adjustment.
lcAdjTyp = '2'

*--Call the global function for update style inventory control.
*-- Fixing the bug of wrong updating of ctrcode field in styinvjl (Start)
PRIVATE lcRefer 
lcRefer = IIF(EMPTY(cReason),gfCodDes(cAdjReason,'CADJREASON'),cReason)


lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                 lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"","")

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.

*E301558,1 AHM Send product Activity (Start)
ELSE
  = lfAddPodAct()
*E301558,1 (End)  

ENDIF  
RETURN

*!*************************************************************
*! Name      : lfAddPodAct
*! Developer : AHM - Ahmed Amer
*! Date      : 02/12/2001
*! Purpose   : Add record in EDITrans table for Product Activity
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAddPodAct()
*!*************************************************************

FUNCTION lfAddPodAct

*E301558,1 AHM Send product Activity (Start)

IF 'NC' $ gcCmpModules
  lcFromWare = &lcTmpAdj..cFromWare
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  SELECT CODES
  SET ORDER TO TAG Idrltfname
  =SEEK('NYCSITEID')
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  lcBackSite = ALLTRIM(Codes.cCode_No)
  =SEEK(lcFromWare,'WareHous')
  lcFromSite = ALLTRIM(WareHous.cSiteId)

  STORE '' TO lcSendTo1
  IF gcCurSite = lcBackSite
    IF lcFromSite <> lcBackSite
      lcSendTo1 = lcFromSite
    ENDIF
  ELSE
    lcSendTo1 = lcBackSite
  ENDIF
  IF !EMPTY(lcSendTo1)
    =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
    =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
    =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

    SELECT Date FROM (lcTmpAdj) GROUP BY Date INTO CURSOR Dates
    SELECT Dates
    SCAN
      lcDateKey = DTOS(Date)+'-'+DTOS(Date)
      IF !EMPTY(lcSendTo1)
        SELECT EdiAcPrt
        LOCATE FOR cSiteId = lcSendTo1
        IF FOUND() AND SEEK(cpartcode+'852','EDIPD')
          SELECT EdiTrans
          IF !SEEK('852'+PADR(lcDateKey,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
            INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) VALUES ;
                                     ('852',lcDateKey,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
          ENDIF
          REPLACE cStatus WITH 'N'
          =gfAdd_Info('EDITRANS')
        ENDIF
      ENDIF
    ENDSCAN
    USE IN Dates
  ENDIF
ENDIF
*E301558,1 AHM (End)

*!*************************************************************
*! Name      : lfCloseTmp
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : close & erase temp file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCloseTmp()
*!*************************************************************
FUNCTION lfCloseTmp
IF USED(lcTmpGlDt)
  USE IN (lcTmpGlDt)
  IF FILE(gcWorkDir+lcTmpGlDt+'.DBF') 
    ERASE(gcWorkDir+lcTmpGlDt+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcTmpGlDt+'.FPT') 
    ERASE(gcWorkDir+lcTmpGlDt+'.FPT')
  ENDIF
  IF FILE(gcWorkDir+lcTmpGlDt+'.CDX') 
    ERASE(gcWorkDir+lcTmpGlDt+'.CDX')
  ENDIF
ENDIF

IF USED(lcTempUpc)
  USE IN (lcTempUpc)
  IF FILE(gcWorkDir+lcTempUpc+'.DBF') 
    ERASE(gcWorkDir+lcTempUpc+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcTempUpc+'.FPT') 
    ERASE(gcWorkDir+lcTempUpc+'.FPT')
  ENDIF
  IF FILE(gcWorkDir+lcTempUpc+'.CDX') 
    ERASE(gcWorkDir+lcTempUpc+'.CDX')
  ENDIF
ENDIF
IF USED(lcTmpAdj)
  USE IN (lcTmpAdj)
  IF FILE(gcWorkDir+lcTmpAdj+'.DBF') 
    ERASE(gcWorkDir+lcTmpAdj+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcTmpAdj+'.FPT') 
    ERASE(gcWorkDir+lcTmpAdj+'.FPT')
  ENDIF
  IF FILE(gcWorkDir+lcTmpAdj+'.CDX') 
    ERASE(gcWorkDir+lcTmpAdj+'.CDX')
  ENDIF
ENDIF
IF USED(lcRejReas)
  USE IN (lcRejReas)
  IF FILE(gcWorkDir+lcRejReas+'.DBF') 
    ERASE(gcWorkDir+lcRejReas+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcRejReas+'.FPT') 
    ERASE(gcWorkDir+lcRejReas+'.FPT')
  ENDIF
  IF FILE(gcWorkDir+lcRejReas+'.CDX') 
    ERASE(gcWorkDir+lcRejReas+'.CDX')
  ENDIF
ENDIF
RETURN


*!*************************************************************
*! Name      : lfCrtRjct
*! Developer : WAB - Walid A. Wahab 
*! Date      : 01/25/2001
*! Purpose   : create reject record
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCrtRjct()
*!*************************************************************
FUNCTION lfCrtRjct
PARAMETER lcType
DO CASE 
  CASE lcType = 'A'
    lcRjctRsn = 'UPC No not found'
  CASE lcType = 'B'
    lcRjctRsn = 'Invalide Site No'
  CASE lcType = 'C'
    lcRjctRsn = 'Invalid WareHouse Code'  
  OTHERWISE
    RETURN
ENDCASE
IF !SEEK(PADR(lcType,1)+PADR(&lcTempUpc..cUpcNo,13),lcRejReas)
   INSERT INTO (lcRejReas) (Type,cUpcNo,cWareCode,Reason ) VALUES ;
          (PADR(lcType,1),&lcTempUpc..cUpcNo,&lcTempUpc..cWARECODE,lcRjctRsn)
   lnRejCont = lnRejCont + 1
ENDIF
RETURN