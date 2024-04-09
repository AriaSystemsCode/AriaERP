*****************************************************************************
*: Program file  : ARSTUIN.PRG  (C# 037179) For Stuncroft
*: Program desc. : Importing Invoices from Excel files.
*: Module        : AR 
*: Screens       : ARSTUIN.SPX
*:        System : Aria Apparel System (A27)
*:     Developer : MOHAMED SHOKRY- (MHM)
*****************************************************************************
*: Calls : FUNCTIONS  : gfSetup,gfOpenFile,lfvProceed,lfvPath,
*:                      lfvFile,lfUpdInv,lfUpdTGl,lfvStyles,lfPrnRep,
*:                      lfUnLock,lfUpdRes,lfFiilDiv,lfvDiv,lfFiilGrp,lfvGrp.
*****************************************************************************
*: Passed Parameters  : None.
*****************************************************************************
*: Modifications :
*:B122368,1 NNA 04/20/2004 Fix bug of that the account tax is being charged even the account is exempted
*:B122368,1 NNA            specially if there is a taxable style
*****************************************************************************

*-- Initializing the necessary variables.
lnanswer = 3

DIMENSION laAddress[6,3], laSetups[21,2],laTFile[1],laTRltFld[6,2]
STORE '' TO laAddress, laSetups

STORE 0 TO lnCountLn , lnRest , lnTotRec, lnTaxRate

STORE ''  TO lcTEOM,lcTCod
STORE 20 TO lnEomDay   && End of month day default value
STORE 0   TO lnTDaysDue,lnTaxDue
llOpenRep  = .F.
lcFilHandl = ''
lcWinTitl  = "Import Log Report"
lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.

lcPathName  = SPACE(40)
STORE '' TO lcPriceLvl 

STORE '' TO  lcDShpName,lcDShpAdd1,lcDShpAdd2,lcDShpAdd3,lcDShpAdd4,lcDShpAdd5,;
             lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcDBllName,lcDBllAdd1,lcDBllAdd2,lcDBllAdd3,lcDBllAdd4,lcDBllAdd5,;
             lcIDefWare,lcIDefSes,lcIDefDiv
IF !gfSetup()
  RETURN
ENDIF

DO gcScrDir+'AR\'+("ARSTUIN.SPR")
*****************************************************************************
*: Program file  : lfvProceed.PRG  
*: Program desc. : Valid Function For Update Button.
*:        System : Aria Apparel System (A2.7)
*:     Developer : Mohamed Shokry Mohamed - (MHM)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  lfvProceed()
*****************************************************************************
*:
FUNCTION lfvProceed

IF gfModalGen("QRM00000B42002","DIALOG",.F.,.F.,'Are you sure you want to proceed ?')=2
  _CUROBJ = OBJNUM(pbCancel)
  RETURN
ENDIF
*--mhm2000
=gfOpenFile(gcdatadir+'INVHDR','INVHDR','SH')
=gfOpenFile(gcdatadir+'ORDLINE','ORDLINE','SH')
=gfOpenFile(gcdatadir+'INVLINE','INVLINE','SH')
=gfOpenFile(gcdatadir+'STYLE','STYLE','SH')
=gfOpenFile(gcdatadir+'STYDYE','STYDYE','SH')
=gfOpenFile(gcdatadir+'STYINVJL','STYINVJL','SH')
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
********** AMENDED ADDED BY TONY ***********
= GFOPENFILE(GCDATADIR+'DIVSLSRP','DIVSLSRP','SH')
*********  AMENDED ADDED BY TONY ***********

STORE '' TO lcGlSession 
STORE ""  TO lcInvLine,lcInvHdr,lcScFields
lcInvLine = gfTempName()
lcInvHdr  = gfTempName()

=lfCratTemp()
*--open files needed

lcScFields = 'INVOICE,ACCOUNT,ORDER,CUSTPO,STORE,SHIPDATE,DUEDATE,APPROVAL,'+;
               'cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DEPT,NOTE1,NOTE2,REP1,'+;
               'COMM1,REP2,COMM2,CWARECODE,CCURRCODE,NEXRATE,CFACCODE,CARTONS,'+;
               'WEIGHT,CODTAG,COD_AMT,TRDE_DISC,PIKTKT,SHIPAMT,DISCPCNT,'+;
               'DISCOUNT,FREIGHT,INSUR,COD,TAX_AMT,TOTALCHG,NCHARGES,'+;
               'NCURRUNIT,INVDATE,SHIP,nPstAmt,cTaxRule,nPstRate,TAX_RATE,'+;
               'BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO'

SELECT InvHdr
SCATTER FIELDS &lcScFields TO laData BLANK
lcSession   = gfsequence('CSESSION')
lcGlSession = gfsequence('GLSESSION')
llAlowNew = .T.
laSetups[1,1]  = 'M_PACK'         &&  Use style packs / SKU    
laSetups[2,1]  = 'M_STY_COM'      &&  Commision at Style level   
laSetups[3,1]  = 'M_LN_NOTE'      &&  Add Note to the invoice line
laSetups[4,1]  = 'M_LINK_GL'      &&  Check for Gl link
laSetups[5,1]  = 'M_WareHouse'    &&  use maltiple locations Y Or N  Ic Setup 
laSetups[6,1]  = 'M_GenOrNum'     &&  Enter order # Manually Y Or N
laSetups[7,1]  = 'M_COST_METH'    &&  Cost Method
laSetups[8,1]  = 'M_DYELOT'       &&  Use Dylot Y Or N      
laSetups[9,1]  = 'M_TAX'          &&  use Taxes Y or N
laSetups[10,1] = 'M_TAX_RATE'     &&  Tax Rate
laSetups[11,1] = 'M_TAX_METH'     &&  Tax Method
laSetups[12,1] = 'M_UPC_USE'      &&  Use U.P.C. Numbers        
laSetups[13,1] = 'M_DIV_LINK'     &&  GL link codes at Division level
laSetups[14,1] = 'M_REP_COMM'     &&  Salesrep commission base 
laSetups[15,1] = 'M_UPSBOX'       &&  Track Boxes for Ups Manifacuring   
laSetups[16,1] = 'XAGINGTYPE'     &&  Aging AR by Date\Terms 
laSetups[17,1] = 'XPOSTFINV'      &&  Post Factored invoice to customer
laSetups[18,1] = 'M_CRDT_LMT'     &&  Warn above Credit limit 
laSetups[19,1] = 'M_EDTPRICE'     &&  Edit style price Y/N 
laSetups[20,1] = 'M_TAX_DESC'     &&  Tax Name      
laSetups[21,1] = 'M_HST_RATE' && HST Tax Rate

=gfGetMemVar(@laSetups,gcAct_Comp)

*-- Payment Terms Related Fields
laTRltFld[1,1] = 'NTERDISCR'   
laTRltFld[1,2] = 'laData[29]'  && Trade Discount
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'      && End of Month Y/N
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnTDaysDue'  && Net Due Days
laTRltFld[4,1] = 'CODYN'       
laTRltFld[4,2] = 'lcTCod'      && Cash on delivery Y/N
laTRltFld[5,1] = 'LINSTALLM'
laTRltFld[5,2] = 'llInstTerm'  && Use Instalment 
laTRltFld[6,1] = 'EOMDAY'
laTRltFld[6,2] = 'lnEomDay'    && End Of Month Day


lcTaxName  = IIF(EMPTY(laSetups[20,2]),'G.S.T. Tax',laSetups[20,2])
llIsCanada = IIF(UPPER(ALLTRIM(gcContCode))='CANADA',.T.,.F.)

IF UPPER(ALLTRIM(gcContCode))='ENG'   && in case of England
  *-- Arrat to hold Tax Rate at Style level 
  DECLARE laEngStyTax[1,2]    
  laEngStyTax[1,1] = 'NTAXRATE'
  laEngStyTax[1,2] = 'lnTaxRate'  && Tax Rate at Style level  

  STORE "" TO lcTaxTitle,lcTaxBreak
  llIsEngland = .T.
  *-- variable to hold the temp Name for the english charges file
  lcEngChrg = gfTempName() 
  *-- in case of england Track Boxes for Ups Manifacuring must be No
  laSetups[15,2]='N'
ELSE
  llIsEngland = .F.
ENDIF
llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)   && Use Multi Currency       
llEditExRt  = gfGetMemVar('LLEDITEXRA',gcAct_Comp)   && Edit exchange rate
*--
*-- Create Temp File to get all needed data from Excel sheet
CREATE CURSOR TmpInv (ACCOUNT C(5),STORE C(8),CBMCINVNO C(8),INVDATE  D(8),;
					  CUSTPO C(15),STYLE C(19), AMOUNT N(13,2),CCustName C(35))

FOR lncnt = 1 TO ALEN(laTFile)
  SELECT TmpInv
  ZAP
  APPEND FROM (ALLT(lcpathname)+IIF(RIGHT(lcPathName,1) <> "\","\","")+;
               laTFile(lnCnt)) DELIMITED
ENDFOR
COUNT TO lnTotRec

=lfGetData()
=lfViewRLog()
*--Initialize Error Handeler
llOpenRep  = .F.
lcFilHandl = ''

IF USED(lcInvLine)
  USE IN (lcInvLine)
ENDIF
ERASE (gcWorkDir+lcInvLine+".DBF")
ERASE (gcWorkDir+lcInvLine+".CDX")
ERASE (gcWorkDir+lcInvLine+".FPT")
IF USED(lcInvHdr)
  USE IN (lcInvHdr)
ENDIF
ERASE (gcWorkDir+lcInvHdr+".DBF")
ERASE (gcWorkDir+lcInvHdr+".CDX")
*****************************************************************************
*: Program file  : lfvPath.PRG  
*: Program desc. : Valid Function For Path Button.
*:         System: Aria Apparel System (A2.7)
*:     Developer : Mohamed Shokry Mohamed - (MHM)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfvPath()
*****************************************************************************
*:
FUNCTION lfvPath

DIMENSION laSFile[1,1]
STORE ' ' TO laSFile

lcOldPath = FULLPATH("")
lcPathName=GETDIR('','Select Source Directory')
IF EMPTY(lcPathName)
  SHOW GET pbProceed DISABLE
  STORE SPACE(40) TO lcPathName
  =lfRefresh()
  SET DEFAUL TO (lcOldPath)
  RETURN
ELSE
  =lfRefresh()
ENDIF
SET DEFA TO (lcPathName)
IF ADIR(laFiles,"*.CSV") = 0 
  SET DEFAULT TO (lcOldPath)
  *--- "No files to be uploaded in the specified path."
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No files to be uploaded in the specified path.')
  
  lcPathName = SPACE(40)
  SHOW GET pbProceed DISABLE
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
SET DEFAUL TO (lcOldPath)
DIMENSION laSFile[ALEN(laFiles,1),1]
FOR lnCount = 1 TO ALEN(laFiles,1)
  laSFile[lnCount,1] = laFiles[lnCount,1]
ENDFOR
=gfMover(@laSFile,@laTFile,"Select files to upload",.T.)
IF EMPTY(laTFile)
  SHOW GET pbProceed DISABLE
  RETURN
ENDIF
SHOW GET pbProceed ENABLE
_CUROBJ = OBJNUM(pbProceed)

*****************************************************************************
*: Program file  : lfvFile.PRG  
*: Program desc. : Valid Function To check the selected *.CSV File.
*:        System : Aria Apparel System (A2.7)
*:     Developer : Mohamed Shokry Mohamed - (MHM)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfvFile()
*****************************************************************************
*:
FUNCTION lfvFile

IF EMPTY(ldDate) 
  *---"Cannot upload file " +laTFile(lnCnt)+"No Posting Date is specified in cell A2." )
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ No Posting Date is specified in cell A2.])
  RETURN .F.
ENDIF
IF ldDate > gdSysdate
  *---('I',"Cannot upload file " + laTFile(lnCnt)+ ", " +;
                "the posting date is greater than the system date." )  
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ the posting date is greater than the system date.])
  RETURN .F.
ENDIF

IF !SEEK(lcWare,"WAREHOUS")		
  *---('I',"Cannot upload file " + laTFile(lnCnt)+;
                " Warehouse code " +lcWare +" is not found in the warehouse file.")
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ Warehouse code ]+lcWare+" is not found in the warehouse file.") 
  RETURN .F.
ENDIF
IF !SEEK(lcWare,"MULLOCK")
  *---('I',"Cannot upload file " + laTFile(lnCnt)+ ". " +;
                "No locking is done for warehouse " + lcWare)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ No locking is done for warehouse ]+ lcWare) 
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfCratTemp
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : Create temp files
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCratTemp()
*!*************************************************************
FUNCTION lfCratTemp
PRIVATE laFileStru,lnFileStru,laIndex

SELECT INVHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+12,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'lUpsIns'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'lCompUps'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 1
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'LastLine'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 6
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'STNAME'
laFileStru[lnFileStru+5,2] = 'C'
laFileStru[lnFileStru+5,3] = 30
laFileStru[lnFileStru+5,4] = 0
laFileStru[lnFileStru+6,1] = 'CADDRESS1'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 30
laFileStru[lnFileStru+6,4] = 0
laFileStru[lnFileStru+7,1] = 'CADDRESS2'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 30
laFileStru[lnFileStru+7,4] = 0
laFileStru[lnFileStru+8,1] = 'CADDRESS3'
laFileStru[lnFileStru+8,2] = 'C'
laFileStru[lnFileStru+8,3] = 30
laFileStru[lnFileStru+8,4] = 0
laFileStru[lnFileStru+9,1] = 'CADDRESS4'
laFileStru[lnFileStru+9,2] = 'C'
laFileStru[lnFileStru+9,3] = 30
laFileStru[lnFileStru+9,4] = 0
laFileStru[lnFileStru+10,1] = 'CADDRESS5'
laFileStru[lnFileStru+10,2] = 'C'
laFileStru[lnFileStru+10,3] = 30
laFileStru[lnFileStru+10,4] = 0
laFileStru[lnFileStru+11,1] = 'LKEYOFF'
laFileStru[lnFileStru+11,2] = 'L'
laFileStru[lnFileStru+11,3] = 0
laFileStru[lnFileStru+11,4] = 0
laFileStru[lnFileStru+12,1] = 'NTAXDUE'
laFileStru[lnFileStru+12,2] = 'N'
laFileStru[lnFileStru+12,3] = 13
laFileStru[lnFileStru+12,4] = 2
=gfCrtTmp(lcInvHdr,@laFileStru,[ACCOUNT+ORDER+STORE+PIKTKT],lcInvHdr)

SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+5,4]
laFileStru[lnFileStru+1,1] = 'nTaxRate'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 2
laFileStru[lnFileStru+2,1] = 'nSteps'
laFileStru[lnFileStru+2,2] = 'N'

laFileStru[lnFileStru+2,3] = 6

laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'LBACKORD'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 0
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'CONSOL'
laFileStru[lnFileStru+4,2] = 'C'
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'LTAXABLE'
laFileStru[lnFileStru+5,2] = 'L'
laFileStru[lnFileStru+5,3] = 0
laFileStru[lnFileStru+5,4] = 0

DECLARE laIndex[2,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt+STYLE'
laIndex[1,2] = 'InvLines'
laIndex[2,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
laIndex[2,2] = 'InvLine'

=gfCrtTmp(lcInvLine,@laFileStru,@laIndex)


*!*************************************************************
*! Name      : lfGetAcc
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : Validate Invoice Account
*!*************************************************************
*! Calls     : gfGetAdr,gfChkRate,gfModalGen,gfActWind,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfGetAcc()
*!*************************************************************
FUNCTION lfGetAcc
PARAMETER  xAccount 
PRIVATE llChkAcc
STORE .T. TO llChkAcc

IF !SEEK('M'+xAccount,'Customer')
	*--write to log file 

    DECLARE laRebMsg[1]
    laRebMsg[1] = "Account " + XAccount + " is not vaild "
    =lfVryRport()
	llChkAcc=.F.	
    RETURN llChkAcc
    EXIT 	
ENDIF
STORE Customer.cCurrCode TO laData[22]
**** AMENDED ADDED BY TONY *******************
STORE CUSTOMER.CDIVISION TO LADATA[13], LCDIVISION
*************************************************

IF !EMPTY(TmpInv.Store) AND !SEEK('S'+xAccount+ALLTRIM(TmpInv.Store),'Customer')
	*--write to log file 
    DECLARE laRebMsg[1]
    laRebMsg[1] = "Account Store " + XAccount + " " + TmpInv.Store + " is not vaild "
    =lfVryRport()
	llChkAcc=.F.	
    RETURN llChkAcc
    EXIT 	
ENDIF

IF Customer.Status = 'X'
	*-- Write to Log File 
	llChkAcc=.F.	
    RETURN llChkAcc
    EXIT 	
ENDIF
laData[2] = xAccount
laData[5] = ALLTRIM(TmpInv.Store)
lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
*-- Customer Bill To name
STORE Customer.BtName TO lcBillName,lcDBllName
=gfGetAdr('Customer','','','',1,'2')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcDBllAdd&lcCount = lcDBllAdd&lcCount + IIF(EMPTY(lcDBllAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  lcBillAdd&lcCount = lcDBllAdd&lcCount
ENDFOR  
*-- Dba =Do business as 
*-- get the shipname  
STORE IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba) TO lcDShpName,lcShipName
=gfGetAdr('CUSTOMER','','','',1,'')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcDShpAdd&lcCount = lcDShpAdd&lcCount + IIF(EMPTY(lcDShpAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  lcShipAdd&lcCount = lcDShpAdd&lcCount
ENDFOR 
llupsinsur = (Customer.cInsur='Y')
*-- get the tax rate from the customer in case of canada
STORE IIF(laSetups[9,2] <> 'Y',0,;
      IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) TO laData[46]
*-- tax Rule
STORE IIF(EMPTY(laData[44]),' 1',laData[44]) TO laData[44]
STORE 1 TO lnPstRule
*--factor code
STORE Customer.cFacCode TO laData[24]  
*-- the default codes
STORE IIF(EMPTY(Customer.ShipVia),lfGetDefCode('SHIPVIA'),Customer.ShipVia) TO laData[10]
STORE IIF(EMPTY(Customer.cTermCode),lfGetDefCode('CTERMCODE'),Customer.cTermCode)   TO laData[09]
STORE IIF(EMPTY(Customer.SpcInst),lfGetDefCode('SPCINST'),Customer.SpcInst) TO laData[11]
STORE Customer.Disc      TO laData[32]
STORE Customer.SalesRep  TO laData[17]
STORE Customer.Rep2      TO laData[19]
STORE Customer.Note      TO laData[15]
STORE TmpInv.INVDATE     TO laData[6],laData[41]
STORE TmpInv.INVDATE     TO laData[49]

**** AMENDED ADDED BY TONY *********
*STORE CUSTOMER.CDIVISION TO LADATA[ 13]
SELECT DIVSLSRP
SET ORDER TO 1
LCKEY = (CUSTOMER.ACCOUNT+CUSTOMER.STORE+LCDIVISION)
IF SEEK(LCKEY)
 STORE REPCODE TO LADATA[ 17]
 STORE COMM_RATE TO LADATA[ 18], LNSOUCOMM1
ELSE
 STORE CUSTOMER.SALESREP TO LADATA[ 17]
 STORE 0.00 TO LADATA[ 18], LNSOUCOMM1
ENDIF
***************************************

GOTO TOP IN WAREHOUS
STORE WAREHOUS.cWareCode TO laData[21]
**** AMENDED ADDED BY TONY *********
*STORE IIF(EMPTY(laData[17]),0,IIF(Customer.salesrep # laData[17],SalesRep.Comm,CUSTOMER.Comm)) TO laData[18],lnSouComm1
*STORE IIF(EMPTY(laData[19]),0,IIF(Customer.Rep2 # laData[19],SalesRep.Comm,CUSTOMER.Comm2)) TO laData[20],lnSouComm2
***************************************

=gfRltFld(laData[9],@laTRltFld,'CTERMCODE')
lcTEOM = ALLTRIM(lcTEOM)
lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
lcTCod = ALLTRIM(lcTCod)
llCod  = (lcTCod='Y')
**-- due date 
laData[7] = IIF(lcTEOM <> 'Y',laData[41]+lnTDaysDue,;
                CTOD('01'+SUBSTR(DTOC(GOMONTH(laData[41],1)),3))-1+lnTDaysDue)
*--  currency 
laData[22] = IIF(EMPTY(laData[22]),gcBaseCurr,laData[22])
STORE 1 TO laData[23], laData[40]
IF laData[22] <> gcBaseCurr
  *-- Exchange Rate
  laData[23] = gfChkRate('laData[40]', laData[22], laData[41] ,;
                          .T., .F., .F., llEditExRt)
  IF laData[23] = 0
    laData[22] = gcBaseCurr
    STORE 1 TO laData[23], laData[40]
  ENDIF
ENDIF
DO gfAcOpCrdt IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
                 laData[2],laData[22],'llActCrdt'
    
SELECT (lcInvHdr)
APPEND BLANK
=RLOCK() 
IF SEEK(SUBSTR(TmpInv.Style,1,lnMajorLen),'STYLE')
  m.Season   = Style.Season
ELSE
  m.Season   = ''
ENDIF
  
GATHER FROM laData FIELDS &lcScFields
REPLACE Status     WITH '' ,;
        DIRECT_INV WITH .T. ,;
        Cod_Flag   WITH 'N',;
        Phone      WITH Customer.Phone1,;
        SHIPVIA    WITH Customer.SHIPVIA,; 
        SEASON     WITH m.Season,;
        STORE      WITH ALLTRIM(TMPINV.STORE),;
        trde_disc  WITH laData[29],;
        cOwner     WITH '037179'
=SEEK('M'+xAccount,'Customer')
REPLACE CTERMCODE  WITH Customer.CTERMCODE,;
        SPCINST    WITH Customer.SPCINST,;
        CDIVISION  WITH Customer.CDIVISION

UNLOCK
llCUpDate = .T.
  
RETURN llChkAcc
*!*************************************************************
*! Function  : lfVryRport
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : - Create a low level text file (ImprtLog.txt). 
*!*************************************************************
FUNCTION lfVryRport

IF !llOpenRep  
  llOpenRep = .T.
  lcFilHandl = FCREAT('&gcWorkDir.ImprtLog.txt')
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,"*                  IMPORT VERIFICATION  REPORT                  *")
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,' ')
  =FPUTS(lcFilHandl,' ')
ENDIF
lnRebMsg  =  ALEN(laRebMsg,1) 
FOR lnCount = 1 TO lnRebMsg  
  =FPUTS(lcFilHandl,laRebMsg[lnCount])
ENDFOR  

*-- End of lfVryRport()

*!*************************************************************
*! Function  : lfViewRLog
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : - Preview Import log file (ImprtLog.txt). 
*!*************************************************************
FUNCTION lfViewRLog

*** If there is a report created dispaly it to the user
IF llOpenRep
  =FFLUSH(lcFilHandl)
  * Close the text file
  *DO WHILE !FCLOSE(lcFilHandl)
  *ENDDO
  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (gcWorkDir+"ImprtLog.txt") OVERWRITE
  DO (gcScrDir + gcWinAppl + '\ARPRTLOG.SPR')
  USE IN TMPSTR
  =FCLOSE(lcFilHandl)
  DELETE FILE gcWorkDir+"ImprtLog.txt"
  
ELSE
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Importing Process has been completed.')
ENDIF

*!*************************************************************
*! Name      : lfPrnt
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : Print the Import log report
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************

FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*!*************************************************************
*! Name      : lfGetData
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : Update Data in temp files then in master files
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfGetData()
*!*************************************************************

FUNCTION lfGetData

lnCountLn = 0
SELECT(lcInvLine)
SET RELATION TO STYLE INTO STYLE ADDITIVE
SELECT TMPINV
SCAN
  IF YEAR(INVDATE) < 1990
	REPLACE INVDATE WITH INVDATE+36525
  ENDIF  
  lnCountLn = lnCountLn + 1
  =gfThermo(lnTotRec,lnCountLn,'Importing')
  SELECT(lcInvHdr)
  DELETE ALL
  SELECT(lcInvLine)
  DELETE ALL
  IF !lfGetAcc(TmpInv.Account)
    LOOP
  ENDIF
  IF !lfvStyle(TmpInv.STyle)
	LOOP
  ENDIF
  SELECT(lcInvHdr)
  REPLACE custpo     WITH TmpInv.custpo,;
          cbmbinvno  WITH TmpInv.CBMCINVNO,;
          CCustName  WITH TmpInv.CCustName,;
          SHIP       WITH 1,;
          SHIPAMT    WITH TmpInv.Amount,;
          APPRAMT    WITH TmpInv.Amount,;
          Tax_Amt    WITH (lnTaxRate*TmpInv.Amount/100)-((lnTaxRate*TmpInv.Amount/100)*laData[29]/100),;
          TotalChg   WITH TmpInv.Amount+Tax_Amt,;
          trde_disc  WITH laData[29],;
          cOwner     WITH '037179'
          		
  *B122368,1 NNA 04/20/2004 (Begin) If the customer is exempted so we don't calculate any taxes even the style is taxable
  IF CUSTOMER.LVATEXEM = .T.
    REPLACE TOTALCHG WITH TOTALCHG-TAX_AMT
    REPLACE TAX_AMT  WITH 0
    REPLACE TAX_RATE WITH 0
  ENDIF 
  *B122368,1 NNA (End)
						
	SELECT(lcInvLine)
	REPLACE Price      WITH TmpInv.Amount,;
			gros_price WITH TmpInv.Amount

	lcStStyl = STYLE
	DECLARE laInv[1]
	STORE '' TO laInv
	DO gpSaveInv IN (gcapphome+gcwinappl+'\ARINV.PRG') WITH ;
	lcInvHdr,lcInvLine,'','','','','',lcGlSession,'laInv'
	SELECT InvLine
	REPLACE nEqvAmnt WITH Gros_Price*TotQty
	
    *--delete record from STYINVJL
    *ashraf Don't delete the StyinVjl records as required from Tony on 01/12/2004.
    *SELECT (lcInvHdr)
    *LOCATE
    *SELECT STYINVJL
	*=SEEK(SUBSTR(TmpInv.Style,1,lnMajorLen))
	*SCAN REST WHILE SUBSTR(Style,1,lnMajorLen) =SUBSTR(TmpInv.Style,1,lnMajorLen)
	*  IF ctrcode = &lcInvHdr..invoice
	*    lcStStyl = STYLE
	*  	DELE
	*  ENDIF
	*ENDSCAN 
	=SEEK(SUBSTR(TmpInv.Style,1,lnMajorLen),'STYLE')
	REPLACE STYLE.STK1    WITH STYLE.STK1+1,;
			STYLE.TotSTK  WITH STYLE.TotSTK+1
	=SEEK(lcStStyl+&lcInvHdr..cWareCode,'STYDYE')
	REPLACE STYDYE.STK1    WITH STYDYE.STK1+1,;
			STYDYE.TotSTK  WITH STYDYE.TotSTK+1
			
ENDSCAN

*!*************************************************************
*! Name      : FUNCTION lfvStyle
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : Validate entered style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PARAMETER xStyle
PRIVATE llCheckSty
STORE .T. TO llCheckSty

SELECT (lcInvLine) 

IF !SEEK(ALLTRIM(xStyle),'Style')
	*--write in Log File 
    DECLARE laRebMsg[1]
    laRebMsg[1] = "Style " + XStyle + " is not vaild "
    =lfVryRport()

	llCheckSty = .F.
	RETURN llCheckSty
ELSE

  m.Style =STYLE.Style
  m.TotQty = 1
  m.Qty1=1
  m.Gros_Price = TmpInv.Amount
  m.Price      = TmpInv.Amount
  m.Season     = Style.Season
  m.Scale      = Style.Scale
  m.Desc1      = Style.Desc
  m.Cowner     = '037179'
  SELECT (lcInvLine)
  IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[21]+SPACE(10),'StyDye')
    *-- Add a new record to the StyDye file.
    DO gpAdStyWar WITH m.Style,SPACE(10),laData[21]
  ENDIF
  lnLines   =  1
  m.LineNo  = lnLines
  m.Account = laData[2]
  m.store   = ALLTRIM(TmpInv.store)
  m.InvDate = laData[41]
  m.cWareCode = WAREHOUS.cWareCode
  INSERT INTO (lcInvLine) FROM MEMVAR

  *SELECT STYLE
  *SKIP
  *lcLastStyle = Style 
  *=SEEK(m.Style) 
  SELECT (lcInvLine)
  SET ORDER TO TAG InvLine
  *=SEEK(laData[2]+laData[3]+laData[5]+laData[30]+STR(m.LineNo,6))

  *-- get the cDiscCode From stydye in every case
  lcDiscCode = IIF(SEEK(Style.Style+&lcInvHDr..cWareCode+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
  IF !EMPTY(ALLTRIM(lcDiscCode))
    *-- Get the discount related field to now which 
    *-- type whole Sale Or Retail sale Or Both.
    DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
    STORE '' To lcDisType , ldStartDte ,ldEndDate
    *-- Array to get the Discount affect for DecCode.
    laDisType[1,1]  = 'CCOSTAFECT'
    laDisType[1,2]  = 'lcDisType'
    *-- Array to get the start date For DescCode.
    lastartDte[1,1] = 'START'
    lastartDte[1,2] = 'ldStartDte'
    *-- Array to get the end date For DescCode.
    laEndDate[1,1]  = 'DENDATE'
    laEndDate[1,2]  = 'ldEndDate'
    = gfRltFld(lcDiscCode, @laDisType, 'CDISCCODE')
    = gfRltFld(lcDiscCode, @laStartDte,'CDISCCODE')
    = gfRltFld(lcDiscCode, @laEndDate, 'CDISCCODE')
    STORE 0 To m.Disc_Pcnt , lnDisc_Pcnt
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[41],ldStartDte,ldEndDate)
      lnDisc_Pcnt = m.Disc_Pcnt
      =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
      *m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100
      m.Disc_Pcnt = lnDisc_Pcnt
    ENDIF
  ENDIF  
  STORE 0 To m.Disc_Pcnt , lnDisc_Pcnt
  *m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  IF llIsEngland .AND. !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
  *-- get the england style tax related field if it is not smaller the the tax Break
    =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')  
  ENDIF
  llCUpdate  = .T.
  m.Desc1    = Style.Desc1
  m.Scale    = Style.Scale
  m.PrePak   = Style.PrePak
  m.Comm1    = IIF(Style.commission,laData[18],0)
  m.Comm2    = IIF(Style.commission,laData[20],0)
  m.lTaxAble = STYLE.lTaxAble
  m.Season   = Style.Season
  m.nTaxRate = lnTaxRate
  laData[36] = TmpInv.Amount * lnTaxRate/100
  =RLOCK()
  IF !EOF()
    GATHER MEMVAR MEMO
  ENDIF
  UNLOCK
  *SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF  
RETURN llCheckSty  
*-- end of lfvStyle.

*!*************************************************************
*! Name      : lfGetprice
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : Get Style Price
*!*************************************************************
*! Calls     : lfCheckPri()
*!*************************************************************
*! Passed Parameters  :  lcStyle : Style
*!                       lcLevel : Price level
*!                       lnQuantity : invoice Quantity
*!*************************************************************
*! Returns            :  Alternative price
*!*************************************************************
*! Example            :  =lfGetprice(m.Style,'A',100)
*!*************************************************************
FUNCTION lfGetprice
PARAMETERS lcStyle,lcLevel,lnQuantity
PRIVATE lnPrice

=SEEK(lcStyle,'Style')
IF lcLevel = 'Q'
  DO CASE
    CASE Style.nAtQtyC > 0 AND lnQuantity > Style.nAtQtyC
      lcLevel = 'C'
    CASE Style.nAtQtyB > 0 AND lnQuantity >Style.nAtQtyB
      lcLevel = 'B'
    OTHERWISE
      lcLevel = 'A'
  ENDCASE
ELSE
  lcLevel=IIF(INLIST(lcLevel,'A','B','C'),lcLevel,'A')
ENDIF
lnPrice = IIF(!llMulCurr OR laData[22]=gcBaseCurr,Style.Price&lcLevel,;
                      gfStyPrice(lcStyle,lcLevel,laData[22]))
*-- if price= 0 then get the price from the user                      
RETURN(lnPrice)
*!*************************************************************
*! Name      : lfGetDefCode
*! Developer : Mohamed Shokry Mohamed - (MHM)
*! Date      : 05/11/2003
*! Purpose   : Get defaut code
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcCodeType : Code Type
*!*************************************************************
*! Return      : Default code
*!*************************************************************
FUNCTION lfGetDefCode
PARAMETERS lcCodeType
PRIVATE lcCurrTag

lcCurrTag = ORDER('CODES')
SET ORDER TO TAG 'CCODE_NO' IN 'CODES'
=SEEK('D'+PADR(UPPER(lcCodeType),10),'CODES')
SET ORDER TO TAG (lcCurrTag) IN 'CODES'
RETURN(CODES.CCODE_NO)
