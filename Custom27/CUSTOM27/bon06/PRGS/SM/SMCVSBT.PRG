*:****************************************************************
*: Program file  : SMCVSBT.PRG
*: Program desc. : Data Conversion From Sbt  (Import SBT data)
*: System        : Aria Apparel System - Version 2.7.
*: Module        : System Manager (SM)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 08/20/2003
*: Tracking Job Number : E#302208,1
*:****************************************************************
*: Calls         : G.FUNCTIONS: gfSetup , CHECKPRD
*:               : FUNCTIONS  : lfProceed , lfConvCust , lfConStyls
*:               :            : lfConDbCrt, lfvFpath   , lfOpenFls
*:               :            : lfClosFls , lfvCancel  , lpDetEsc
*:               :            : lfChecks  , lfcnvVf2Fx , lfGetdCods
*:               :            : lfErrLog  , lfUpd_Note , lfTempExcl
*:               :            : lfgetSeasn, lfvPrint   ,lfGetDCCod
*:               :            : lfarglacnt.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:E#302208,1.
*:****************************************************************
*:Modifications  :
*:B119941,1 ABD 09/25/2003 Some New Modification.
*:B120526,1 ASH 11/13/2003 1- Fix bug of not saving season.
*:B120526,1                2- Leave the pack ID extension in the style records 
*:B120526,1                and create a new record for the style without the pack ID extension.
*:****************************************************************
*:
*-- laOpenFile :- Array Hold the Opened Files.
*-- lcSbtPath  :- Variable hold the SBT directory.
*-- lcSbtCompy :- Variable hold the SBT Company code.
*-- lcSbtWork  :- Variable Hold The Work Directory.
*-- llOpenFils :- Variable To know If we open any file ort not.
*-- lcTmpMemo  :- Temp File To hold the memo Variables.
*-- lcSelCompy :- Variable Hold the selected company.
*-- lcTmpSeson :- Season Excel File and Convert to Data Base File.
*-- lcTmpScale :- Scale Excel File and Convert to Data Base File.
*-- lcTmpAcct  :- Table hold the old and new account
*-- lnLineNo   :- Variable to hold the printed line number at error file.

DIMENSION laOpenFile[12,3]
STORE ''  TO lcSbtPath  , lcSbtCompy , lcArcust , lcSelCompy ,;
             lcTmpScale , lcTmpSeson , lcGLTemp

STORE .F. TO llOpenRep , llOpenFils , llSycfact , llLink_AP  , llLink_Gl,;
             llOpenGLD
STORE 0 TO lnFilHandl , lnLineNo

lcTmpScale =  gfTempName()
lcTmpSeson =  gfTempName()
lcTmpMemo  =  gfTempName()
lcTmpAcct  =  gfTempName()
lcGLTemp   =  gfTempName()

*B119941,1 ABD - Define New File. [Begin]
lcTmpGroup =  gfTempName()
*B119941,1 ABD - [End]


lcGLFYear  = SPACE(4)
lcGLPeriod = SPACE(2)
ldBatchDt  = gdSysDate
lcBatchNo  = ""
lcCurrCode = gcBaseCurr      && Variable to hold currency code.
lnExRate   = 1               && Variable to hold exchange rate.
lnCurrUnit = 1               && Variable to hold exchange rate.
lcUntSin   = '/'             && Variable to hold unit sign.
lcExRSin   = '/'             && Variable to hold exchange rate sign.

llLink_AP  = (OCCURS('AP',gcCmpModules) <> 0) OR (OCCURS('PS',gcCmpModules) <> 0)
llLink_Gl  = ALLTRIM(gfGetMemVar("M_LINK_GL")) = "Y"

IF !gfSetup()
  RETURN
ENDIF
lcSbtWork  = gcWorkDir
lnCompany  = 1
SELECT ccomp_id+ '  -  ' + cCom_Name From syccomp INTO ARRAY laCompanys

IF FILE(gcDataDir+"SBTPath.FXP")
  RESTORE FROM (gcDataDir+"SBTPath.FXP") ADDI
ENDIF

PUSH KEY
lcWindTitle = 'Import SBT data'
*--Call screen.
lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpDetEsc

DO (gcScrDir+gcWinAppl+"\SMCVSBT.SPX")
ON KEY
POP KEY
glQuitting = .T.  && This is modal screen, this rise quitting flag.

RETURN
*-- END of Code.
*:*************************************************************
*: Name      : lfProceed
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Funtion to collect the data into text file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfProceed ()
*:*************************************************************
*:
FUNCTION lfProceed


*-- lfChecks   :- Function to check if the files is exist or not.
IF lfChecks()
  *-- lfcnvVf2Fx :- Function to run EXE Form the Visual Fox Pro and convert the selected files into Temp 
  *--            :- Files, this tem files will create at Fox 2.6 and use to convert this files.
  IF !lfcnvVf2Fx()
    RETURN
  ENDIF  
ELSE
  RETURN
ENDIF

*-- Open needed files
= lfOpenFls ()

*-- Function to Convert Customer file.

= lfConvCust ()

*-- Function to Convert Item Files.
= lfConStyls ()

*-- Function to Convert the debit and Credit files.
= lfConDbCrt ()

*-- Close opened files 
= lfClosFls ()

IF llOpenRep

  CREAT CURS XXXX (MEMO M(10))
  APPEN BLANK
  APPEND MEMO MEMO FROM gcWorkdir+'ErrLog.txt' OVERWRITE
  PUSH KEY
  *--Call Error screen.
  DO (gcScrDir+gcWinAppl+"\smdispr.SPX")
  ON KEY
  POP KEY
  
ELSE
  *- Message Text   :- Import SBT files complete successfully.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  lcMessage = 'Data was imported successfully from the SBT System to Aria 2.7 system.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
ENDIF
RETURN

*-- End of lfProceed
*:*************************************************************
*: Name      : lfConvCust
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Funtion to Convert the Customer File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfConvCust ()
*:*************************************************************
*:
FUNCTION lfConvCust
PRIVATE lnActSeqNo , lcActSeqNo
lnActSeqNo = 0
lcActSeqNo = RIGHT(ALLTRIM(STR(lnActSeqNo)),2)
lcActSeqNo = IIF(LEN(ALLTRIM(lcActSeqNo))=1,'0'+lcActSeqNo,lcActSeqNo)

SELECT Sycfact
LOCATE
lcFrstFact = Sycfact.cFaccode

SELECT lcARCUST
LOCATE
lnCurRec = 0                && Varible to use for the thermometer
lnTotRec = RECCOUNT()       && Varible to use for the thermometer

lcAccount = LEFT(custno,3)+lcActSeqNo
SCAN
  lnCurRec = lnCurRec + 1
  SCATTER MEMVAR MEMO
  
  SELECT CUSTOMER
  *-- Assign Zero Again.
  lnActSeqNo = 0
  lcActSeqNo = RIGHT(ALLTRIM(STR(lnActSeqNo)),2)
  lcActSeqNo = IIF(LEN(ALLTRIM(lcActSeqNo))=1,'0'+lcActSeqNo,lcActSeqNo)

  *B120526,1 ASH 11/13/2003 (Begin) Add variables to add chr to the account name if it exceeds 99.
  llAddChr   = .F.
  lcAddChr   = 'A'
  *B120526,1 ASH 11/13/2003 (End) 
  DO WHILE .T.
    lcAccount = LEFT(M.custno,3)+lcActSeqNo
    *-- Check if this record was exist before.
    IF SEEK('M'+lcAccount,'CUSTOMER')
      lnActSeqNo = lnActSeqNo + 1
      lcActSeqNo = RIGHT(ALLTRIM(STR(lnActSeqNo)),2)
      *B120526,1 ASH 11/13/2003 (Begin) Add char. to the account name to prevent duplicate.
      IF llAddChr
        IF lnActSeqNo > 9
          lnActSeqNo = 0
          lcActSeqNo = RIGHT(ALLTRIM(STR(lnActSeqNo)),2)
          lcAddChr   = CHR(ASC(lcAddChr)+1)
        ENDIF
        lcActSeqNo = lcAddChr +  lcActSeqNo 
      ENDIF
      *B120526,1 ASH 11/13/2003 (End)
      lcActSeqNo = IIF(LEN(ALLTRIM(lcActSeqNo))=1,'0'+lcActSeqNo,lcActSeqNo)

      *-- If the Sequ. number still under 10, to be 00, or 01 and etc.
      lcActSeqNo = IIF(LEN(ALLTRIM(lcActSeqNo))=1,'0'+lcActSeqNo,lcActSeqNo)
      lcAccount  = LEFT(M.custno,3)+lcActSeqNo
    ELSE
      EXIT
    ENDIF
    *-- Exit From the Sequn no to void Infinte loop.
    IF lnActSeqNo >=100
      *B120526,1 ASH 11/13/2003 (Begin) Don't exit from the loop if the account name exceeds 99.
      *EXIT
      *B120526,1 ASH 11/13/2003 (End)
      lnActSeqNo = -1
      llAddChr   = .T.
    ENDIF
  ENDDO
  
  *-- Update the temp file that hold the old and new account.
  IF !SEEK(M.custno,lcTmpAcct)
    SELECT (lcTmpAcct)
    APPEN BLANK
    REPLACE ACCOUNT  WITH lcAccount,;
            SBTAcunt WITH M.custno
  ENDIF
  
  SELECT CUSTOMER
  *-- Get the Default Commision from the sales rep file.
  lnRepComm = 0
  IF !EMPTY(M.Salesmn) .AND. SEEK(LEFT(M.Salesmn,3),'SalesRep')
    lnRepComm = SalesRep.Comm
  ENDIF
  
  *-- Remvoe the telephone picture from the fiedls.
  M.Phone = STRTRAN(M.Phone,'/')
  M.Phone = STRTRAN(M.Phone,'-')
  
  M.FaxNo = STRTRAN(M.FaxNo,'/')
  M.FaxNo = STRTRAN(M.FaxNo,'-')
    
  APPEND BLANK
  *-- Update the Customer file.the Customer File.
  REPLACE Type        WITH 'M'               ,;
          Billto      WITH 'M'               ,;
          Account     WITH lcAccount         ,;
          cOldAcct    WITH LEFT(M.custno,5)  ,;
          Status      WITH IIF(M.Limit = -1 ,'H','A'),;
          Link_Code   WITH 'DEFDEF'          ,;
          Btname      WITH LEFT(M.Company,30),;
          Stname      WITH LEFT(M.Company,30),;
          Phone1      WITH M.Phone           ,;
          Buyer       WITH M.contact         ,;
          cCont_Code  WITH 'USA'             ,;
          cCont_Code2 WITH 'USA'             ,;
          cAddress1   WITH M.Address1        ,;
          cAddress12  WITH M.Address1        ,;
          cAddress2   WITH M.Address2        ,;
          cAddress22  WITH M.Address2        ,;
          cAddress3   WITH M.City            ,;
          cAddress32  WITH M.City            ,;
          cAddress4   WITH M.State           ,;
          cAddress42  WITH M.State           ,;
          cAddress5   WITH M.Zip             ,;
          cAddress52  WITH M.Zip             ,;
          cAddress6   WITH 'USA'             ,;
          cAddress62  WITH 'USA'             ,;
          Factacct    WITH M.Country         ,;
          SalesRep    WITH M.Salesmn         ,;
          Comm        WITH lnRepComm         ,;
          cFacCode    WITH IIF(M.Code = 'FA',lcFrstFact,''),;
          cTermCode   WITH lfGetdCods('N','CTERMCODE ',M.pterms,.T.),;
          cRLimit     WITH IIF(M.Limit = -1 ,0,M.Limit),;
          cravail     WITH IIF(M.Limit = -1 ,0,M.Limit),;
          FAX         WITH M.FaxNo,;
          Note        WITH M.Email
          
          
  *-- Update Default Fields.
  REPLACE Consol      WITH 'N'  ,;
          Priority    WITH '5'  ,;
          Region      WITH lfGetdCods('D','REGION    ','',.F.),;
          CLASS       WITH lfGetdCods('D','CLASS     ','',.F.),;
          SHIPVIA     WITH lfGetdCods('D','SHIPVIA   ','',.F.),;
          SPCINST     WITH lfGetdCods('D','SPCINST   ','',.F.),;
          CInsur      WITH 'Y'  ,;
          PriceLvl    WITH 'A'  ,;
          SKUTmpl     WITH 'DEF',;
          Prnt_statm  WITH 'Y'  ,;
          cTaxRule    WITH '1'  ,;
          cCurrCode   WITH lcCurrCode,;
          cSlsgllink  WITH 'DEF'

  REPLACE cAdd_User  WITH gcUser_ID  ,;
          dAdd_Date  WITH DATE()     ,;
          cAdd_Time  WITH gfGetTime(),;
          CADD_VER   WITH "A27"

  *-- Update the Customer history file with the next and current and last year.
  IF !SEEK(lcAccount,'ARCUSHST') 
    INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (lcAccount,STR(VAL(gcCurrYear)-1,4))
    INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (lcAccount,gcCurrYear)
    INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (lcAccount,STR(VAL(gcCurrYear)+1,4))
  ENDIF
  
  *- Update Customer Note           
  IF !EMPTY(M.Comment) .OR. !EMPTY(M.cstmemo)
    REPLACE lHasNotes WITH .T.
    IF !EMPTY(M.Comment)
      lcSend_Note = M.Comment 
    ENDIF
    
    IF !EMPTY(M.cstmemo)
      lcSend_Note = IIF(EMPTY(lcSend_Note),'',lcSend_Note+CHR(13)) + M.cstmemo
    ENDIF
    = lfUpd_Note (lcSend_Note)
  ENDIF

  *-- UpDate the Ship 2 data.
  SELECT lcARCADR
  IF SEEK(M.custno)

    SCAN REST WHILE Custno = M.custno
    *-- Get the Default Commision from the sales rep file.
    lnRepComm = 0
    IF !EMPTY(lcARCADR.Salesmn) .AND. SEEK(LEFT(lcARCADR.Salesmn,3),'SalesRep')
      lnRepComm = SalesRep.Comm
    ENDIF

      SELECT CUSTOMER
      APPEND BLANK
      *-- Update the Customer file.the Customer File.
      REPLACE Type        WITH 'S'               ,;
              Billto      WITH 'M'               ,;
              Account     WITH lcAccount         ,;
              STORE       WITH lcARCADR.cShipNo  ,;
              cOldAcct    WITH LEFT(lcARCADR.custno,5)  ,;
              Status      WITH IIF(M.Limit = -1 ,'H','A'),;
              Link_Code   WITH 'DEFDEF'                 ,;
              Btname      WITH LEFT(lcARCADR.Company,30),;
              Stname      WITH LEFT(lcARCADR.Company,30),;
              Phone1      WITH lcARCADR.Phone           ,;
              Buyer       WITH lcARCADR.contact         ,;
              cCont_Code  WITH 'USA'                    ,;
              cCont_Code2 WITH 'USA'                    ,;
              cAddress1   WITH lcARCADR.Address1        ,;
              cAddress12  WITH M.Address1               ,;
              cAddress2   WITH lcARCADR.Address2        ,;
              cAddress22  WITH M.Address2               ,;
              cAddress3   WITH lcARCADR.City            ,;
              cAddress32  WITH M.City                   ,;
              cAddress4   WITH lcARCADR.State           ,;
              cAddress42  WITH M.State                  ,;
              cAddress5   WITH lcARCADR.Zip             ,;
              cAddress52  WITH M.Zip                    ,;
              cAddress6   WITH 'USA'                    ,;
              cAddress62  WITH 'USA'                    ,;
              Factacct    WITH lcARCADR.Country         ,;
              SalesRep    WITH lcARCADR.Salesmn         ,;
              Comm        WITH lnRepComm         ,;
              FAX         WITH lcARCADR.FaxNo


      REPLACE cAdd_User  WITH gcUser_ID  ,;
              dAdd_Date  WITH DATE()     ,;
              cAdd_Time  WITH gfGetTime(),;
              CADD_VER   WITH "A27"
      SELECT lcARCADR
    ENDSCAN
  ENDIF

  =gfThermo(lnTotRec , lnCurRec , 'Importing the Customer file.')
  
  SELECT lcARCUST
ENDSCAN

*-- End OF lfConvCust.
*:*************************************************************
*: Name      : lfConStyls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Funtion to Convert Style SBt File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfConStyls ()
*:*************************************************************
*:
FUNCTION lfConStyls

SELECT lcICITEM
LOCATE
lnCurRec = 0                && Varible to use for the thermometer
lnTotRec = RECCOUNT()       && Varible to use for the thermometer

SCAN
  lnCurRec = lnCurRec + 1
  IF Item = '_MANUAL_INVOICE' .OR. Item = 'SHIP'
    LOOP
  ENDIF
  
  SCATTER MEMVAR MEMO
  lcColor   = ALLTRIM(STRTRAN(M.Item,ALLTRIM(M.Itmclss)))
  lcPrePack = LEFT(lcColor,1)
  lcColor   = SUBSTR(lcColor,2,LEN(lcColor))
  
  *B119941,1 ABD - Add Color if not Found. [Begin]
  =lfAddCodes('COLOR     ',lcColor)
  *B119941,1 ABD - [End]
  *B120526,1 ASH 11/13/2003 (Begin) Add record for the style + it's pack ID extension.
  *lcStyle = PADR(M.Itmclss,12)  +'-'+PADR(lcColor,6)  
  lnStyLen = LEN(ALLTRIM(Itmclss))+1
  lcStyle  = PADR(SUBSTR(M.Item,1,lnStyLen),12)  +'-'+PADR(lcColor,6)
  *B120526,1 ASH 11/13/2003 (End)
  SELECT STYLE
  IF SEEK(lcStyle)
    lcError = 'Style # '+ lcStyle + 'was found within another prepack :' +lcPrePack +'.'
    = lfErrLog (lcError)
    LOOP
  ELSE
    APPEN BLANK
  ENDIF
  
  *-- Update the style file.
  *B119941,1 ABD - Update the wearhouse with '3D' only. [Begin]
  *REPLACE STYLE      WITH lcStyle   ,;
          cStyMajor  WITH M.Itmclss ,;
          STATUS     WITH 'A'       ,;
          Desc       WITH M.ItmDesc ,;
          Desc1      WITH M.ItmDesc ,;
          Pricea     WITH M.Price   ,;
          nICost1    WITH M.stdcost ,;
          TotCost    WITH M.stdcost ,;
          Ave_Cost   WITH M.stdcost ,;
          Gros_Price WITH M.stdcost ,;
          PrePak     WITH lcPrePack ,;
          SCALE      WITH IIF(SEEK(Itmclss,lcTmpScale),'P','T'),;
          cDefWare   WITH 'WH3D'    ,;
          cStyGrade  WITH '1'       ,;
          Make       WITH .F.       ,;
          lInvsty    WITH .T.       ,;
          lDetCost   WITH .T.       ,;
          Commission WITH .T.       ,;
          CStyGroup  WITH lfGetdCods('D','CSTYGROUP ','',.F.),;
          cDivision  WITH lfGetdCods('D','CDIVISION ','',.F.),;
          Season     WITH lfgetSeasn(m.ComCode) && Update the old Season with the new season from the Excel file.
  REPLACE STYLE      WITH lcStyle   ,;
          cStyMajor  WITH SUBSTR(lcStyle,1,12),;
          STATUS     WITH 'A'       ,;
          Desc       WITH M.ItmDesc ,;
          Desc1      WITH M.ItmDesc ,;
          Pricea     WITH M.Price   ,;
          nICost1    WITH M.stdcost ,;
          TotCost    WITH M.stdcost ,;
          Ave_Cost   WITH M.stdcost ,;
          Gros_Price WITH M.stdcost ,;
          PrePak     WITH lcPrePack ,;
          SCALE      WITH IIF(SEEK(ALLTRIM(Itmclss),lcTmpScale),&lcTmpScale..Scale,'1'),;
          cDefWare   WITH '3D'      ,;
          cStyGrade  WITH '1'       ,;
          Make       WITH .F.       ,;
          lInvsty    WITH .T.       ,;
          lDetCost   WITH .F.       ,;
          Commission WITH .T.       ,;
          CStyGroup  WITH lfGetGroup(&lcTmpScale..StyleGroup)  ,;
          cDivision  WITH lfGetdCods('D','CDIVISION ','',.F.),;
          Season     WITH lfgetSeasn(m.ComCode) && Update the old Season with the new season from the Excel file.
  *B119941,1 ABD - [End]
  *-- rerplace the Default Value that didn't mention in the specs.
  REPLACE cDye_Flg   WITH 'N'     ,;
          Link_Code  WITH 'DEFDEF'  ,;
          cPricecur  WITH lcCurrCode,;
          cDutyCur   WITH lcCurrCode,;
          nCurrRate  WITH lnExRate  ,;
          nTaxBreak  WITH lnCurrUnit,;
          cSlsglLink WITH 'DEF'

  
  REPLACE cAdd_User  WITH gcUser_ID  ,;
          dAdd_Date  WITH DATE()     ,;
          cAdd_Time  WITH gfGetTime(),;
          CADD_VER   WITH "A27"


  *B120526,1 ASH 11/13/2003 (Begin) Don't save empty scale.
  IF EMPTY(SCALE)
    REPLACE SCALE WITH '1'
  ENDIF
  IF !SEEK('P'+Scale+lcPrePack,'Scale')
    REPLACE PrePak WITH ''
  ENDIF
  SELECT STYDYE
  APPEN BLANK
  *B119941,1 ABD - Update the wearhouse with '3D' only. [Begin]
  *REPLACE STYLE      WITH lcStyle   ,;
          cWareCode  WITH 'WH3D'    ,;
          Gl_Link    WITH 'DEFDEF' 
  REPLACE STYLE      WITH lcStyle   ,;
          cWareCode  WITH '3D'    ,;
          Gl_Link    WITH 'DEFDEF' 

  *B119941,1 ABD - [End]
  
  FOR I = 1 TO 8           
    lnSize = STR(I,1)
    REPLACE STK&lnSize      WITH 0,;
            ALO&lnSize      WITH 0,;
            Shp&lnSize      WITH 0,;
            ORD&lnSize      WITH 0,;
            Ret&lnSize      WITH 0,;
            Wip&lnSize      WITH 0,;
            RA&lnSize       WITH 0,;
            InTrans&lnSize  WITH 0,;
            nwo&lnSize      WITH 0
    
  ENDFOR
  REPLACE TotStk     WITH 0,;
          TotAlo     WITH 0,;
          TotShp     WITH 0,;
          TotOrd     WITH 0,;
          TOTRET     WITH 0,;
          TOTWIP     WITH 0,;
          dmgQty     WITH 0,;
          TOTRA      WITH 0,;
          TotInTrn   WITH 0,;
          ntotwo     WITH 0,;
          Ave_Cost   WITH M.stdcost
  
  REPLACE cAdd_User  WITH gcUser_ID  ,;
          dAdd_Date  WITH DATE()     ,;
          cAdd_Time  WITH gfGetTime(),;
          CADD_VER   WITH "A27"


  *B120526,1 ASH 11/13/2003 (Begin) Add record for the main style without it's pack ID extension.
  lcStyle = PADR(M.Itmclss,12)  +'-'+PADR(lcColor,6)  
  *lnStyLen = LEN(ALLTRIM(Itmclss))+1
  *lcStyle  = PADR(SUBSTR(M.Item,1,lnStyLen),12)  +'-'+PADR(lcColor,6)
  SELECT STYLE
  IF SEEK(lcStyle)
    LOOP
  ELSE
    APPEN BLANK
  ENDIF
  
  *-- Update the style file.
  REPLACE STYLE      WITH lcStyle   ,;
          cStyMajor  WITH SUBSTR(lcStyle,1,12),;
          STATUS     WITH 'A'       ,;
          Desc       WITH M.ItmDesc ,;
          Desc1      WITH M.ItmDesc ,;
          Pricea     WITH M.Price   ,;
          nICost1    WITH M.stdcost ,;
          TotCost    WITH M.stdcost ,;
          Ave_Cost   WITH M.stdcost ,;
          Gros_Price WITH M.stdcost ,;
          PrePak     WITH lcPrePack ,;
          SCALE      WITH IIF(SEEK(ALLTRIM(Itmclss),lcTmpScale),&lcTmpScale..Scale,'1'),;
          cDefWare   WITH '3D'      ,;
          cStyGrade  WITH '1'       ,;
          Make       WITH .F.       ,;
          lInvsty    WITH .T.       ,;
          lDetCost   WITH .F.       ,;
          Commission WITH .T.       ,;
          CStyGroup  WITH lfGetGroup(&lcTmpScale..StyleGroup)  ,;
          cDivision  WITH lfGetdCods('D','CDIVISION ','',.F.),;
          Season     WITH lfgetSeasn(m.ComCode) && Update the old Season with the new season from the Excel file.
  *-- replace the Default Value that didn't mention in the specs.
  REPLACE cDye_Flg   WITH 'N'     ,;
          Link_Code  WITH 'DEFDEF'  ,;
          cPricecur  WITH lcCurrCode,;
          cDutyCur   WITH lcCurrCode,;
          nCurrRate  WITH lnExRate  ,;
          nTaxBreak  WITH lnCurrUnit,;
          cSlsglLink WITH 'DEF'

  
  REPLACE cAdd_User  WITH gcUser_ID  ,;
          dAdd_Date  WITH DATE()     ,;
          cAdd_Time  WITH gfGetTime(),;
          CADD_VER   WITH "A27"

  *B120526,1 ASH 11/13/2003 (Begin) Don't save empty scale.
  IF EMPTY(SCALE)
    REPLACE SCALE WITH '1'
  ENDIF
  IF !SEEK('P'+Scale+lcPrePack,'Scale')
    REPLACE PrePak WITH ''
  ENDIF
  *B120526,1 ASH 11/13/2003 (End)

  SELECT STYDYE
  APPEN BLANK
  REPLACE STYLE      WITH lcStyle   ,;
          cWareCode  WITH '3D'    ,;
          Gl_Link    WITH 'DEFDEF' 
  FOR I = 1 TO 8           
    lnSize = STR(I,1)
    REPLACE STK&lnSize      WITH 0,;
            ALO&lnSize      WITH 0,;
            Shp&lnSize      WITH 0,;
            ORD&lnSize      WITH 0,;
            Ret&lnSize      WITH 0,;
            Wip&lnSize      WITH 0,;
            RA&lnSize       WITH 0,;
            InTrans&lnSize  WITH 0,;
            nwo&lnSize      WITH 0
    
  ENDFOR
  REPLACE TotStk     WITH 0,;
          TotAlo     WITH 0,;
          TotShp     WITH 0,;
          TotOrd     WITH 0,;
          TOTRET     WITH 0,;
          TOTWIP     WITH 0,;
          dmgQty     WITH 0,;
          TOTRA      WITH 0,;
          TotInTrn   WITH 0,;
          ntotwo     WITH 0,;
          Ave_Cost   WITH M.stdcost
  
  REPLACE cAdd_User  WITH gcUser_ID  ,;
          dAdd_Date  WITH DATE()     ,;
          cAdd_Time  WITH gfGetTime(),;
          CADD_VER   WITH "A27"
  
  *B120526,1 ASH 11/13/2003 (End)
  =gfThermo(lnTotRec , lnCurRec , 'Importing the Item file.')
  SELECT lcICITEM
ENDSCAN


*-- End OF lfConStyls
*:*************************************************************
*: Name      : lfConDbCrt
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Funtion to get the bottom part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfConDbCrt ()
*:*************************************************************
*:
FUNCTION lfConDbCrt
PRIVATE lcNewAcct , lnPrvalias ,lcCrdCode,lcDbtCode
STORE '' TO lcNewAcct, lcCrdCode , lcDbtCode , lcGLAcc
DECLARE laAdjRltFd[3,2]
lcLinkCode = "DEFDEF"
laAdjRltFd[1,1] = 'CADJACCT'
laAdjRltFd[1,2] = 'lcAdjAcct'
laAdjRltFd[2,1] = 'CBNKCODE'
laAdjRltFd[2,2] = 'lcBnkCode'
laAdjRltFd[3,1] = 'CCHKACCT'
laAdjRltFd[3,2] = 'lcChkAcct'

lnPrvalias  = SELECT (0)
SELECT FSPRD
LOCATE
IF FOUND()
  LOCATE REST FOR BETWEEN(ldBatchDt,Dfsppbgdt,Dfsppendt) ;
              WHILE (ldBatchDt >= Dfsppbgdt)                 
ENDIF

IF !FOUND()                  && No period match checked date
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
    =gfDialog( 'I',lcErrorM1+lcErrorM2)
    RETURN
ELSE
  lcGLFYear  = Cfisfyear      && Transaction date year
  lcGLPeriod = Cfspprdid      && Transaction date period     
ENDIF  

SELECT lcARMAST
LOCATE
lnCurRec = 0                && Varible to use for the thermometer
lnTotRec = RECCOUNT()       && Varible to use for the thermometer

*-- Get the Code Number for the beginning balance.
lcCrdCode = lfGetDCCod ('CCREDITCOD','Beginning balance') 
lcDbtCode = lfGetDCCod ('TRANCODE','Beginning balance')

SCAN 
  lnCurRec = lnCurRec + 1
  IF Balance = 0
    LOOP
  ENDIF
  
  SCATTER MEMVAR MEMO
  
  *-- get the new account.
  IF SEEK(M.custno,lcTmpAcct)
    lcAccount = &lcTmpAcct..Account
  ELSE
    *-- Send Error.
    lcError = 'Account # :' + ALLTRIM(M.custno) + ' was not found into the SBT Customer file.' 
    = lfErrLog (lcError)
    Loop
  ENDIF

  lcToSnd   = IIF(M.ARTYPE = "C" , 'CREDIT', 'DEBIT' )
  lcTranNo  = gfsequence(lcToSnd, lcSelCompy , "", "", "TRAN")
  lcBatchNo = gfsequence('BATCH')
  
  STORE '' TO   lcAdjAcct , lcBnkCode , lcChkAcct
   
  =gfRltFld(IIF(M.ARTYPE = 'C',lcCrdCode,lcDbtCode) , @laAdjRltFd , IIF(M.ARTYPE = "C","CCREDITCOD","TRANCODE"))
  lcGLAcc   = lcAdjAcct
  
  *-- Check for Invoice type = C Credit or Debit if emty.
  IF M.ARTYPE = 'C'
    SELECT CREDIT
    APPEN BLANK

    REPLACE DESC      WITH 'SBT TR#: '+ ALLTRIM(M.INVNO),;
            TranDate  WITH M.InvDte  ,;
            dPostDate WITH M.InvDte  ,;
            Account   WITH lcAccount ,;
            Reference WITH M.Ponum   ,;
            Amount    WITH M.Balance

    *-- Update the commen field didn't mention into the specs.
    REPLACE Trantype   WITH '5'       ,;
            BATCH      WITH lcBatchNo ,;
            TRAN       WITH lcTranNo  ,;
            lnonar     WITH .F.       ,;
            cCurrcode  WITH lcCurrCode,;
            nexrate    WITH lnExRate  ,;
            nCurrUnit  WITH lnCurrUnit,;
            CCREDITCOD WITH lcCrdCode ,;
            cAdjAcct   WITH lcAdjAcct ,;
            cBnkCode   WITH lcBnkCode ,;
            cChkAcct   WITH lcChkAcct

    IF llLink_AP
    ENDIF
                
  ELSE
    SELECT DEBIT
    APPEN BLANK
    REPLACE DESC      WITH 'SBT TR#: '+ ALLTRIM(M.INVNO),;
            TranDate  WITH M.InvDte  ,;
            DueDate   WITH M.InvDte  ,;
            dPostDate WITH M.InvDte  ,;
            Account   WITH lcAccount ,;
            Reference WITH M.Ponum   ,;
            Amount    WITH M.Balance
   
    *-- Update the commen field didn't mention into the specs.
    REPLACE Trantype   WITH '2'       ,;
            BATCH      WITH lcBatchNo ,;    
            TRAN       WITH lcTranNo  ,;
            cCurrcode  WITH lcCurrCode,;
            nexrate    WITH lnExRate  ,;
            nCurrUnit  WITH lnCurrUnit,;
            TRANCODE   WITH lcDbtCode ,;
            cAdjAcct   WITH lcAdjAcct ,;
            cBnkCode   WITH lcBnkCode ,;
            cChkAcct   WITH lcChkAcct
            
  ENDIF

  REPLACE cAdd_User  WITH gcUser_ID  ,;
          dAdd_Date  WITH DATE()     ,;
          cAdd_Time  WITH gfGetTime(),;
          CADD_VER   WITH "A27"
  
  
  IF llLink_AP
    lcGLAcc = IIF(SEEK(lcBnkCode + lcChkAcct, "APChecks") , APChecks.cChkGLAcc , lcGLAcc)
    REPLACE cAdjAcct WITH lcGLAcc    
  ENDIF

  *-- Get the glar account.
  lccarglacc = lfarglacnt(lcLinkCode,'001') 
  REPLACE carglacc WITH lccarglacc
  

  *-- Update the Customer history file.
  IF !EMPTY(lcGLFYear) .AND. !EMPTY(lcGLPeriod) .AND. BETWEEN(VAL(lcGLPeriod) , 1 , 13)
    lcGLPeriod = PADL(ALLTRIM(lcGLPeriod) , 2 , "0" )
    *-- Update the customer history file.
    SELECT ARCUSHST
    *-- Lock the file to grantee the phiscal update.
    IF SEEK(lcAccount + lcGLFYear , "ARCUSHST")
      IF M.ARTYPE = "C"
        REPLACE nCrAdj            WITH nCrAdj + ROUND(ABS(M.Balance) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
          nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ROUND(ABS(M.Balance) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
      ELSE      
        REPLACE nDrAdj            WITH nDrAdj + ROUND(ABS(M.Balance) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
          nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ABS(M.Balance) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
      ENDIF
    ENDIF
  ENDIF
    

  =gfThermo(lnTotRec , lnCurRec , 'Importing the Debit / Credit files.')
  SELECT lcARMAST
ENDSCAN


SELECT(lnPrvAlias)

*-- End OF lfConDbCrt
*:*************************************************************
*: Name      : lfvFpath
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Funtion to get the fil path.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvFpath()
*:*************************************************************
*:
FUNCTION lfvFpath
PRIVATE lcoldPath

lcoldPath = FULLPATH('')

lcSbtPath = GETDIR()
SHOW GET lcSbtPath 

=lfRefresh('')
SET DEFA TO &lcoldPath
*-- End of lfvFpath
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls
PRIVATE lnPrvAlias 

lnPrvAlias = SELECT(0)

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .
*-- Aria 27 Tables.
llOpenFils = .T.
laOpenFile[1,1] = 'CUSTOMER'
laOpenFile[1,2] = 'CUSTOMER'

laOpenFile[2,1] = 'STYLE'
laOpenFile[2,2] = 'STYLE'

laOpenFile[3,1] = 'DEBIT'
laOpenFile[3,2] = 'DEBIT'

laOpenFile[4,1] = 'CREDIT'
laOpenFile[4,2] = 'CREDIT'

laOpenFile[5,1] = 'CODES'
laOpenFile[5,2] = 'Ccode_no'

laOpenFile[6,1] = 'NOTEPAD'
laOpenFile[6,2] = 'NOTEPAD'

laOpenFile[7,1] = 'SALESREP'
laOpenFile[7,2] = 'SALESREP'

laOpenFile[8,1] = 'ARCUSHST'
laOpenFile[8,2] = 'Acthst'


laOpenFile[9,1] = 'FSPRD'
laOpenFile[9,2] = 'COMFYRPRDI'

laOpenFile[10,1] = 'GL_LINK'
laOpenFile[10,2] = 'GL_LINK'

laOpenFile[11,1] = 'STYDYE'
laOpenFile[11,2] = 'STYDYE'

*B120526,1 ASH 11/13/2003 (Begin) Open the scale file.
laOpenFile[12,1] = 'SCALE'
laOpenFile[12,2] = 'SCALE'
*B120526,1 ASH 11/13/2003 (End) 

IF llLink_AP
  DIMENSION laOpenFile[13,3]
  laOpenFile[13,1] = 'APCHECKS'
  laOpenFile[13,2] = 'BANKCHECK'
ENDIF

lcDataPath = LEFT(gcDataDir,LEN(gcDataDir)-3)
lcDataPath = lcDataPath +lcSelCompy+'\'

FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(lcDataPath+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR

IF llLink_Gl
  IF !USED("GLDIST")
    llOpenGLD = .T.
    =gfOpenFile(lcDataPath+"GLDIST",lcDataPath+"GLDISTAC","SH")
  ELSE
    llOpenGLD = .F.
    SELECT GLDIST
    SET ORDER TO TAG GLDISTAC
  ENDIF
  
  IF !FILE(gcWorkDir+lcGLTemp)
    *-- Create temp. file hold the gl entries.
    =AFIELDS(laFileStru)
    CREATE TABLE (gcWorkDir+lcGLTemp) FROM ARRAY laFileStru
  ENDIF
ENDIF



*-- Open Factor File.
llSycfact = gfOpenFile(gcSysHome+'Sycfact','Cfaccode')


*-- Open Needed files from SBT application.
*-- Open Main Customer File.
IF USED('lcARCUST')
  USE IN lcARCUST
ENDIF
USE (lcSbtWork+'lcARCUST'+'.DBF') EXCL IN 0
SELECT lcARCUST
INDEX ON CUSTNO TO 'lcARCUST'

*-- Open Store  Customer File.
IF USED('lcARCADR') 
  USE IN lcARCADR
ENDIF  
USE  (lcSbtWork+'lcARCADR'+'.DBF') EXCL IN 0
SELECT lcARCADR
INDEX ON custno TO 'lcARCADR'


*-- Open  Style File.
IF USED('lcICITEM')
  USE IN lcICITEM
ENDIF
USE (lcSbtWork+'lcICITEM'+'.DBF') EXCL IN 0
SELECT lcICITEM
INDEX ON  Itmclss TO 'lcICITEM'


*-- Open Debit and credit file.
IF USED('lcARMAST')
  USE IN lcARMAST
ENDIF
USE (lcSbtWork+'lcARMAST'+'.DBF') SHARED IN 0

*-- Open Season Excel File and Convert to Data Base File.
CREATE TABLE (gcWorkDir+lcTmpSeson) (Comcode C(2) , SEASON C(6))

lcInprtfil = lcSbtPath+'Comcode'
IF FILE(lcSbtPath+'Comcode'+'.XLS')
  = lfTempExcl(lcInprtfil,lcTmpSeson)
  LOCATE
  DELETE
  PACK
ELSE
  lcError = 'Excel File Comcode was not found in path '+ lcSbtPath +'.'
  = lfErrLog (lcError)
ENDIF  
INDEX ON Comcode TO  (lcTmpSeson)


*B119941,1 ABD - Craete a new file. [Begin]
*-- Open Season Excel File and Convert to Data Base File.
CREATE TABLE (gcWorkDir+lcTmpGroup) (cSequn C(6) , cGroup C(6) , cGroupdec C(30))

lcInprtfil = lcSbtPath+'StyGroup'
IF FILE(lcSbtPath+'StyGroup'+'.XLS')
  = lfTempExcl(lcInprtfil,lcTmpGroup)
  LOCATE
ELSE
  lcError = 'Excel File Style Group was not found in path '+ lcSbtPath +'.'
  = lfErrLog (lcError)
ENDIF  
INDEX ON ALLTRIM(cSequn) TO  (lcTmpGroup)
*B119941,1 ABD - [End]

*-- Open Scale Excel File and Convert to Data Base File.
*B119941,1 ABD - Craete a new file structure . [Begin]
*CREATE TABLE (gcWorkDir+lcTmpScale) (Itmclss C(10))
CREATE TABLE (gcWorkDir+lcTmpScale) (Itmclss C(10),Scale C(1), Prepack C(1), StyleGroup C(4))
*B119941,1 ABD - [End]
INDEX ON ALLTRIM(Itmclss) TO  (lcTmpScale)
IF FILE(lcSbtPath+'ItemNum'+'.XLS')
  lcInprtfil = lcSbtPath+'ItemNum'
  = lfTempExcl(lcInprtfil,lcTmpScale)
ELSE
  lcError = 'Excel File ItemNum was not found in path '+ lcSbtPath +'.'
  = lfErrLog (lcError)
ENDIF

*-- Open Scale Excel File and Convert to Data Base File.
CREATE TABLE (gcWorkDir+lcTmpAcct) (SBTAcunt C(6), Account C(5))
INDEX ON SBTAcunt TO  (lcTmpAcct)

SELECT(lnPrvAlias)

*-- End of lfOpenFls.
*:*************************************************************
*: Name      : lfClosFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfClosFls ()
*:*************************************************************
*:
FUNCTION lfClosFls

IF llOpenFils
  FOR I = 1 To ALEN(laOpenFile,1)
    IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
      = gfCloseFile(laOpenFile[I,1])
    ENDIF
  ENDFOR
ENDIF

IF llSycfact .AND. USED('SYCFACT')
  USE IN Sycfact
ENDIF

IF llOpenGLD .AND. USED("GLDIST")
  USE IN GLDIST 
ENDIF

IF USED('lcARCUST')
  SELECT lcARCUST
  USE
  ERASE (gcWorkDir+'lcARCUST'+".DBF")
  ERASE (gcWorkDir+'lcARCUST'+".FPT")
ENDIF

IF USED('lcARCADR')
  SELECT lcARCADR
  USE
  ERASE (gcWorkDir+'lcARCADR'+".DBF")
  ERASE (gcWorkDir+'lcARCADR'+".FPT")
ENDIF

IF USED('lcICITEM')
  SELECT lcICITEM
  USE
  ERASE (gcWorkDir+'lcICITEM'+".DBF")
  ERASE (gcWorkDir+'lcICITEM'+".FPT")
ENDIF

IF USED('lcARMAST')
  SELECT lcARMAST
  USE
  ERASE (gcWorkDir+'lcARMAST'+".DBF")
  ERASE (gcWorkDir+'lcARMAST'+".FPT")
ENDIF

IF USED('lcTmpScale')
  SELECT lcTmpScale
  USE
  ERASE (gcWorkDir+'lcTmpScale'+".DBF")
  ERASE (gcWorkDir+'lcTmpScale'+".FPT")
ENDIF

IF USED('lcTmpSeson')
  SELECT lcTmpSeson
  USE
  ERASE (gcWorkDir+'lcTmpSeson'+".DBF")
  ERASE (gcWorkDir+'lcTmpSeson'+".FPT")
ENDIF

IF USED('lcTmpAcct')
  SELECT lcTmpAcct
  USE
  ERASE (gcWorkDir+'lcTmpAcct'+".DBF")
  ERASE (gcWorkDir+'lcTmpAcct'+".FPT")
ENDIF

IF USED('lcGLTemp')
  SELECT lcGLTemp
  USE
  ERASE (gcWorkDir+'lcGLTemp'+".DBF")
  ERASE (gcWorkDir+'lcGLTemp'+".FPT")
ENDIF

*B119941,1 ABD - Close and release the file. [Begin]
IF USED(lcTmpGroup)
  SELECT (lcTmpGroup)
  USE
  ERASE (gcWorkDir+lcTmpGroup+".DBF")
  ERASE (gcWorkDir+lcTmpGroup+".FPT")
  
ENDIF
*B119941,1 ABD - [End]

= FCLOSE(lnFilHandl)

*-- End of lfClosFls
*:*************************************************************
*: Name      : lfvCancel
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function valid the screen before close the screen.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfvCancel ()
*:*************************************************************
*:
FUNCTION lfvCancel

SAVE TO (gcDataDir+"SBTPath.FXP") ALL LIKE lcSbtPath

CLEAR READ

*-- End OF lfvCancel.
*:*************************************************************
*: Name      : lpDetEsc
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpDetEsc
*:*************************************************************
*
PROCEDURE lpDetEsc

_CUROBJ = OBJNUM(pbcancel)
KEYBOARD '{ENTER}'

RETURN

*-- End Of lpDetEsc
*:*************************************************************
*: Name      : lfChecks
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to check on the pathes and converted files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfChecks ()
*:*************************************************************
*:
FUNCTION lfChecks

lcSbtCompy = ALLTRIM(lcSbtCompy)
*-- Check For Selected directory.
DO CASE
  CASE EMPTY(lcSbtPath)
    = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select an SBT data files folder, cannot proceed.')
    RETURN .F.
  CASE EMPTY(lcSbtCompy)
    = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select an SBT Company code, cannot proceed.')
    RETURN .F.
ENDCASE

*-- Check on the Converted files.

DO CASE
   CASE !FILE(lcSbtPath+'ARCUST'+lcSbtCompy+'.DBF')
     lcMessage = 'File ARCUST'+lcSbtCompy+'.DBF was not found in path '+ lcSbtPath +' , cannot proceed.' 
     = gfModalGen('INM00000B00000','F','ALERT', ' ',lcMessage)
     RETURN .F.
  
  CASE !FILE(lcSbtPath+'ARCADR'+lcSbtCompy+'.DBF')
     lcMessage = 'File ARCADR'+lcSbtCompy+'.DBF was not found in path '+ lcSbtPath+' , cannot proceed.' 
     = gfModalGen('INM00000B00000','F','ALERT', ' ',lcMessage)
     RETURN .F.
 
  CASE !FILE(lcSbtPath+'ICITEM'+lcSbtCompy+'.DBF')
     lcMessage = 'File ICITEM'+lcSbtCompy+'.DBF was not found in path '+ lcSbtPath+' , cannot proceed.'   
     = gfModalGen('INM00000B00000','F','ALERT', ' ',lcMessage)
     RETURN .F.
 
  CASE !FILE(lcSbtPath+'ARMAST'+lcSbtCompy+'.DBF')
     lcMessage = 'File ARMAST'+lcSbtCompy+'.DBF was not found in path '+ lcSbtPath+' , cannot proceed.' 
     = gfModalGen('INM00000B00000','F','ALERT', ' ',lcMessage)
     RETURN .F.
ENDCASE

lcSelCompy = LEFT(laCompanys[lnCompany],2)

IF gfModalGen('INM00000B00006','F','ALERT', ' ','Are you sure you want to import SBT data from company '+lcSbtCompy+' to company ' +lcSelCompy +' .') = 2
  RETURN .F.
ENDIF

RETURN .T.
*-- End OF lfChecks
*:*************************************************************
*: Name      : lfcnvVf2Fx
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to Convert Visual Tables to fox 2x.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfcnvVf2Fx ()
*:*************************************************************
*:
FUNCTION lfcnvVf2Fx

*-- lcSbtCompy :- Variable hold the SBT Company code.
*-- lcSbtPath  :- Variable hold the SBT directory.
*-- lcSbtWork  :- Variable Hold The Work Directory.

SAVE TO (gcWorkDir+lcTmpMemo+'.MEM') ALL LIKE lcSbt* 

lcCommLine = (gcWorkDir+lcTmpMemo+'.MEM')

lcLib=SYS(2004)+"foxtools.fll"

lcProgDir = FULLPATH("") +'PRGS\SM\'
IF FILE(lcLib)
  WAIT WINDOW 'Copying V.Fox pro files, Please Wait.' NOWAIT
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",lcProgDir+[SMVF2F26.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
WAIT CLEAR

*-- End Of lfcnvVf2Fx
*:*************************************************************
*: Name      : lfGetdCods
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to get the equivlant code for code description.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Passed Parameters  : lcDefCode = 'N' , 'D'
*:                    : lcCode_No = CTERMCODE  , SEASON ..ETC
*:                    : lcDesc    = Code Description.
*:                    : llSendEror= Send Error to error file.
*:*************************************************************
*: Example   : = lfGetdCods ()
*:*************************************************************
*:
FUNCTION lfGetdCods
PARAMETERS lcDefCode ,lcCode_No , lcDesc , llSendEror

PRIVATE lcDesc , lnAlias , lcCode, lcDefCode ,lcCode_No
lnAlias = SELECT(0)

SELECT CODES
=SEEK (lcDefCode+lcCode_No)
IF !EMPTY(lcDesc)
  LOCATE REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = lcDefCode +lcCode_No ;
  FOR ALLTRIM(UPPER(cDiscrep)) == ALLTRIM(UPPER(lcDesc))
ENDIF

IF FOUND()
  lcCode = Codes.cCODE_No
ELSE
  =SEEK ('D'+lcCode_No)
  lcCode = Codes.cCODE_No
  IF llSendEror
    lcError = 'Payment Terms Description ' + ALLTRIM(lcDesc) + ' was not found into the Codes file, and so the Customer ';
              +M.custNo+ ' was updated with the default Payment Terms.'
    = lfErrLog (lcError)
  ENDIF
ENDIF

SELECT(lnAlias)
RETURN lcCode

*-- End OF lfGetdCods
*:*************************************************************
*: Name      : lfErrLog
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to write the error into log file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfErrLog ()
*:*************************************************************
*:
FUNCTION lfErrLog
PARAMETER lcErr_Mesage

PRIVATE lnAlias
lnAlias = SELECT(0)
lnLineNo = lnLineNo + 1
IF !llOpenRep
  llOpenRep = .T.
  lnFilHandl = FCREAT('&gcWorkdir.ErrLog.txt')
  =FPUTS(lnFilHandl,REPLICATE('*',65))
  =FPUTS(lnFilHandl,"*         Errors occurred while importing the SBT to Aria27         *")
  =FPUTS(lnFilHandl,REPLICATE('*',65))
  =FPUTS(lnFilHandl,' ')
  =FPUTS(lnFilHandl,' ')
ENDIF

=FPUTS(lnFilHandl, ALLTRIM(STR(lnLineNo)) + ' - '+ lcErr_Mesage)

SELECT(lnAlias)
RETURN

*-- END OF lfErrLog
*:*************************************************************
*: Name      : lfUpd_Note
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to update the Customer note.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfUpd_Note ()
*:*************************************************************
*:
FUNCTION lfUpd_Note
PARAMETERS lcNote_Par
PRIVATE lnAlias

lnAlias = SELECT(0)
*-- Key = type+key
SELECT NOTEPAD
IF !SEEK('A'+lcAccount,'NOTEPAD')
  APPEND BLANK
ENDIF

REPLACE Type   WITH 'A',;
        KEY    WITH lcAccount,;
        cDesc  WITH 'Notes For Account Number : '+lcAccount,;
        mNotes WITH mNotes + lcNote_Par
        
        
REPLACE cAdd_User  WITH gcUser_ID  ,;
        dAdd_Date  WITH DATE()     ,;
        cAdd_Time  WITH gfGetTime(),;
        CADD_VER   WITH "A27"

SELECT (lnAlias)
*-- End OF lfUpd_Note
*:*************************************************************
*: Name      : lfTempExcl
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to open Excel File and Convert to Data Base File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfTempExcl ()
*:*************************************************************
*:
FUNCTION lfTempExcl
PARAMETERS lcFileName , lcTempName

SELECT (lcTempName)
ZAP
APPEN FROM (ALLTRIM(lcFileName)) TYPE XLS

*-- End Of lfTempExcl
*:*************************************************************
*: Name      : lfgetSeasn
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to get the season Code from the excel file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfgetSeasn ()
*:*************************************************************
*:
FUNCTION lfgetSeasn
PARAMETERS lcSeason

PRIVATE lnPrvAlias , lcRetrnVlu , llRetrnDef
lcRetrnVlu = ''
llRetrnDef = .T.

lnPrvAlias = SELECT(0)
IF !EMPTY(lcSeason) .AND. SEEK(lcSeason,lcTmpSeson)
  *-- If Found at excel file.
  *-- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam

  *- Abdou
  *-- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
  IF SEEK('N'+'SEASON    '+&lcTmpSeson..SEASON,'CODES')
  *- Abdou SEASON

    lcRetrnVlu = Codes.cCode_no
    llRetrnDef = .F.
  ENDIF
ENDIF

IF llRetrnDef
  =SEEK ('D'+'SEASON    ','CODES')
  lcRetrnVlu = Codes.cCode_no
  IF EMPTY(lcSeason)
    lcError = 'The Season code at the SBT System for style '+ lcStyle +;
              ' was found empty,'+;
              'and so it was updated with the default season code  '+ Alltrim(Codes.cDiscrep) +'.'
  ELSE
    lcError = 'The Season code ' + lcSeason +' from the SBT System for style '+ lcStyle +;
              ' was not found into the Excel or the Codes file,'+;
              'and so it was updated with the default season code  '+ Alltrim(Codes.cDiscrep) + '.'
  ENDIF
    = lfErrLog (lcError)
ENDIF

SELECT(lnPrvAlias)

RETURN lcRetrnVlu

*-- End OF lfgetSeasn
*:*************************************************************
*: Name      : lfvPrint
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to print error file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfvPrint()
*:*************************************************************
*:
FUNCTION lfvPrint

IF PSETUP(.T.) 
  SET DEVICE TO PRINT 
  SET CONSOLE OFF
  TYPE  gcWorkdir+'ErrLog.txt' TO PRINTER
  SET CONSOLE ON
  lcOGPlatForm='DOS'
  DO GFENDPRN
  SET DEVICE TO SCREEN
ENDIF        
RETURN

*-- End OF lfvPrint
*:*************************************************************
*: Name      : lfGetDCCod
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to get the code For debit Credit Beginning Balance.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfGetDCCod()
*:*************************************************************
*:
FUNCTION lfGetDCCod
PARAMETERS lcSndCode , lcSendDesc
PRIVATE lnPrvAlias , lcCode
lcCode = ''

lnPrvAlias = SELECT (0)

SELECT CODES
=SEEK ('N'+lcSndCode)
LOCATE REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N' + lcSndCode ;
FOR ALLTRIM(UPPER(cDiscrep)) == ALLTRIM(UPPER(lcSendDesc))

IF FOUND()
  lcCode = Codes.cCODE_No
ENDIF

SELECT(lnPrvAlias)
RETURN lcCode
*-- End OF lfGetDCCod
*:*************************************************************
*: Name      : lfarglacnt
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to get the argl account code.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfGetDCCod()
*:*************************************************************
*:
FUNCTION lfarglacnt
PARAMETERS _GLLINK , _CATGKEY

IF !SEEK(PADR(_GLLINK,6)+_CATGKEY,'GL_LINK')
  =SEEK('DEFDEF'+_CATGKEY,'GL_LINK')
ENDIF

RETURN GL_LINK.GLACNT
*-- End OF lfarglacnt
*:*************************************************************
*B119941,1 ABD - [Begin]
*:*************************************************************
*: Name      : lfAddCodes
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/25/2003
*: Purpose   : Function to Add the color if not found into Codes File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfAddCodes()
*:*************************************************************
*:
FUNCTION lfAddCodes
PARAMETERS lcFldCodes,lcSndCodNo
PRIVATE lnPrvAlias , lcOldOrder

lnPrvAlias = SELECT(0)

IF TYPE('lcFldCodes') = 'U' .OR. TYPE('lcSndCodNo') = 'U'
  RETURN
ENDIF
lcSndCodNo = PADR(lcSndCodNo,6)

SELECT CODES
lcOldOrder = ORDER()
*-- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
SET ORDER TO Ccode_no

IF !SEEK('N'+lcFldCodes+lcSndCodNo)
  *- Record No. 1
  INSERT INTO CODES (cDefCode,cFld_Name,cCode_No,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
  VALUES ('N',lcFldCodes,lcSndCodNo,.F.,'N',gcUser_ID,DATE(),gfGetTime(),"A27")
      
  *- Record No. 2
  INSERT INTO CODES (cDefCode,cFld_Name,cCode_No,lRltFields,cRltField,cRltd_nam,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
  VALUES ('N',lcFldCodes,lcSndCodNo,.F.,'Y','CLRLNAME  ','C',gcUser_ID,DATE(),gfGetTime(),"A27")

  *- Record No. 3
  INSERT INTO CODES (cDefCode,cFld_Name,cCode_No,lRltFields,cRltField,cRltd_nam,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
  VALUES ('N',lcFldCodes,lcSndCodNo,.F.,'Y','CNRFCODE  ','C',gcUser_ID,DATE(),gfGetTime(),"A27")

  *- Record No. 4
  INSERT INTO CODES (cDefCode,cFld_Name,cCode_No,lRltFields,cRltField,cRltd_nam,crltd_typ,cRltd_Vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
  VALUES ('N',lcFldCodes,lcSndCodNo,.F.,'Y','LLOBSOLETE','L','F',gcUser_ID,DATE(),gfGetTime(),"A27")

  *- Record No. 5
  INSERT INTO CODES (cDefCode,cFld_Name,cCode_No,lRltFields,cRltField,cRltd_nam,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
  VALUES ('N',lcFldCodes,lcSndCodNo,.F.,'Y','CPAINT    ','C',gcUser_ID,DATE(),gfGetTime(),"A27")
ENDIF
SELECT CODES
SET ORDER TO &lcOldOrder
SELECT(lnPrvAlias)
*-- End OF lfAddCodes
*:*************************************************************
*: Name      : lfGetGroup
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/25/2003
*: Purpose   : Function to Add the color if not found into Codes File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfGetGroup()
*:*************************************************************
*:
FUNCTION lfGetGroup
PARAMETERS lcSendGrop
PRIVATE lnPrvAlias , lcOldOrder
lcReturn =''

lnPrvAlias = SELECT(0)

IF TYPE('lcSendGrop ') = 'U'
  RETURN
ENDIF
lcSendGrop = ALLTRIM(lcSendGrop)

IF EMPTY(lcSendGrop) .OR. !SEEK(lcSendGrop,lcTmpGroup)
   lcReturn = lfGetdCods('D','CSTYGROUP ','',.F.)
ELSE
  lcStyGroup = &lcTmpGroup..cGroup
  SELECT CODES
  lcOldOrder = ORDER()
  *-- cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam
  SET ORDER TO Ccode_no

  IF !SEEK('N'+'CSTYGROUP '+lcStyGroup)
    *- Record No. 1
    INSERT INTO CODES (cDefCode,cFld_Name,cCode_No,cdiscrep,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
    VALUES ('N','CSTYGROUP ',lcStyGroup,&lcTmpGroup..cGroupdec,.F.,'N',gcUser_ID,DATE(),gfGetTime(),"A27")

    *- Record No. 2
    INSERT INTO CODES (cDefCode,cFld_Name,cCode_No,lRltFields,cRltField,cRltd_nam,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
    VALUES ('N','CSTYGROUP ',lcStyGroup,.F.,'Y','CGRPDESC2 ','C',gcUser_ID,DATE(),gfGetTime(),"A27")

  ENDIF
  lcReturn = lcStyGroup
ENDIF

SELECT(lnPrvAlias)
RETURN lcReturn
*- End of lfGetGroup
*B119941,1 ABD - [End]
*:*************************************************************

