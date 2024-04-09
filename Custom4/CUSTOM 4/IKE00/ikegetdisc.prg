*! C201850,1 MMT 08/15/2016 Change 820 program to Checked if payment record has no corresponding smart invoice[T20151014.0017]
Lparameters lcAccount,lcFileCode
SET STEP ON

lcErroHandler = On('Error')
On Error N=10

Set Resource Off
Set Exclusive Off
Set Reprocess To 2 Seconds
Set Cpdialog Off
Set Deleted On
Set Exact Off
Set Safety Off
Set Classlib To (oAriaApplication.lcAria4Class+"MAIN.VCX"),(oAriaApplication.lcAria4Class+"UTILITY.VCX")
aria27path = Strtran(Upper(oAriaApplication.lcAria4Class),'CLASSES\','')
aria4path=oAriaApplication.a4sharedpath
companyid = oAriaApplication.activecompanyid
aria27sysfilespath = (aria27path+'sysfiles\')
dbfspath = oAriaApplication.datadir
classespath = (aria4path+'Classes\')
aria4exe = aria4path+'Aria.exe'
MainClass = Upper(classespath+"Main.VCX")
globalclass = Upper(classespath+'globals.VCX')
utilityclass  = Upper(classespath+'UTILITY.VCX')
globalizeclass  = Upper(classespath+'globalize.vcx')

Set Default To (aria4path)
Set Procedure To (aria4exe) Additive
If  .Not. (MainClass$Set("CLASSLIB"))
  Set Classlib To (MainClass) Additive
Endif
If  .Not. (globalclass$Set("CLASSLIB"))
  Set Classlib To (globalclass) Additive
Endif

If  .Not. (utilityclass  $ Set("CLASSLIB"))
  Set Classlib To (utilityclass) Additive
Endif
If  .Not. (globalizeclass  $ Set("CLASSLIB"))
  Set Classlib To (globalizeclass) Additive
Endif

Set Procedure To (aria4path+"PRGS\SY\Ariaglb.fxp")AddIt
lcCurProc = SET("Procedure")
lcCurProc = '"'+STRTRAN(UPPER(oAriaApplication.clientapplicationhome),'PRGS','')+'IKECREATEPAYMENT.FXP'+'",'+lcCurProc 
SET PROCEDURE TO &lcCurProc.
lcEDIARCHIVEFolder =  ADDBS(STRTRAN(UPPER(oAriaApplication.clientapplicationhome),"\PRGS",""))+"EDI\INBOX\ARCHIVE\"
lcFileName = ALLTRIM(oAriaApplication.ActiveCompanyID)+ALLTRIM(lcFileCode)+ALLTRIM(DTOS(DATE()) + STRTRAN(TIME(),":",""))+".XLS"
oAriaApplication.applicationhome = aria4path+"PRGS\"
oAriaApplication.Screenhome = aria4path+"SCREENS\"
oAriaApplication.clientapplicationhome =  Strtran(oAriaApplication.clientapplicationhome,'ARIA3EDI','ARIA4XP')
oAriaApplication.datadir = dbfspath
oAriaApplication.AddProperty ('remotesystemdata', Newobject("RemoteDataAccess",aria4path+"\CLASSES\MAIN.VCX" ))
oAriaApplication.remotesystemdata.Usetechnique = 2
oAriaApplication.AddProperty ("SYSTEMCONNECTIONSTRING",'')
oAriaApplication.SYSTEMCONNECTIONSTRING=[Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB=]+oAriaApplication.Syspath+[;SourceType=DBF;Exclusive=No;BackgroundFetch=No;Collate=Machine;Null=No;Deleted=Yes;]
oAriaApplication.AddProperty ("oActiveLang", Createobject("Custom"))
oAriaApplication.AddProperty ("oGlobalize",Createobject("Globalize"))
oAriaApplication.oActiveLang.AddProperty("cLang_ID","EN")
oAriaApplication.AddProperty ("ProgramTitle",'')
oAriaApplication.oActiveLang.AddProperty("lIs_RTL",.F.)
oAriaApplication.AddProperty ('ActiveProgramHelpCNTX',0)
oAriaApplication.AddProperty ("LangPath",'')
oAriaApplication.LangPath = Addbs(Alltrim(aria4path)) + "Lang\" + Alltrim(oAriaApplication.oActiveLang.cLang_ID) + "\"
oAriaApplication.AddProperty ("lcBaseFile",'')
oAriaApplication.AddProperty ('Aria5ObjectName','')
oAriaApplication.AddProperty ('ActiveModuleHelpFile','')
oAriaApplication.AddProperty ('lnAria5ID',0)
oAriaApplication.AddProperty ("FileW",30)
oAriaApplication.AddProperty ("FieldW",30)
oAriaApplication.AddProperty ("IndexW",30)
oAriaApplication.AddProperty ("laremotetable[1]",'')
oAriaApplication.AddProperty ('ActiveProgramHelpFile','')
oAriaApplication.AddProperty ('ToolBarButtonUDFPos',0)
oAriaApplication.AddProperty ("caria4sysfiles",'')
oAriaApplication.AddProperty ("UserAddCode",.F.)
oAriaApplication.AddProperty ("oDummyToolbar",CREATEOBJECT('Custom'))&&ariacontroltoolbar')
oAriaApplication.AddProperty ("aSepSessionPad[1, 5]","")
oAriaApplication.AddProperty ("aSepSessionBar[1, 8]","")
oAriaApplication.aSepSessionBar[1, 2]=.f.
oAriaApplication.aSepSessionBar[1, 1]='lcProcess'
oAriaApplication.aSepSessionBar[1, 3]="P06PU0602"
oAriaApplication.aSepSessionBar[1, 4]='PP'
oAriaApplication.aSepSessionBar[1, 5]=1
oAriaApplication.aSepSessionBar[1, 6]='KK'
oAriaApplication.aSepSessionBar[1, 7]=''
oAriaApplication.aSepSessionBar[1, 8]=''
oAriaApplication.caria4sysfiles=[Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB=]+oAriaApplication.Syspath+[;SourceType=DBF;Exclusive=No;BackgroundFetch=No;Collate=Machine;Null=No;Deleted=Yes;]

oAriaApplication.AddProperty ("CARIANATIVEDATAFILESCONSTR",'')
oAriaApplication.CARIANATIVEDATAFILESCONSTR=[Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB=]+oAriaApplication.datadir+[;SourceType=DBF;Exclusive=No;BackgroundFetch=No;Collate=Machine;Null=No;Deleted=Yes;]
lcFirstInv = ''
lcLastInv = ''
Set Path To oAriaApplication.applicationhome +"SY" Additive
CREATE CURSOR 'TmpExcel' (Issue C(100),SMART_INVOICE_NO C(18), Invoice_Amount N(10,2),Paid_Amount N(10,2),Variance N(10,2))
lcReturnInvoice = lfCreateConsInv(lcAccount,lcFileCode)
wait clear
IF USED('TmpExcel') AND RECCOUNT('TmpExcel') > 0
  SELECT 'TmpExcel'
  EXPORT TO (lcEDIARCHIVEFolder+lcFileName) TYPE XLS
  DECLARE INTEGER ShellExecute IN shell32.DLL ;
    INTEGER hndWin, STRING cAction,  STRING cFileName, STRING cParams, STRING cDir, INTEGER nShowWin
  ShellExecute(0 ,'open' ,lcEDIARCHIVEFolder+lcFileName,"","",1)
  if used('SMRTSHPLIN')  
    use in  SMRTSHPLIN
  ENDIF  
  if used('RemitsInv')
    use in 'RemitsInv'
  ENDIF
  On Error &lcErroHandler.
Else
  On Error &lcErroHandler.
  Return .F.
Endif
********************************************************************************************
Function lfCreateConsInv
  Parameters lcAccount,lcFileCode
  Use (dbfspath +'EBREMITT.DBF') In 0 Shared  Order EBREMITT   && ACCOUNT+REFERENCE+STR(LINENO,6)
  Use (dbfspath +'ORDHDR.DBF')In 0 Shared  Order ORDHDR
  Use (dbfspath +'STYLE.DBF')In 0 Shared  Order Style
  Use (dbfspath +'StyDye.DBF')In 0 Shared  Order STYDYE
  Use (dbfspath +'INVHDR.DBF')In 0 Shared  Order InvHdr
  Use (dbfspath +'invline.DBF') In 0 Shared Order invline
  Use (dbfspath +'Ordline.DBF')In 0 Shared  Order OrdLine
  Select EBREMITT
  =Seek(lcAccount)
  Locate Rest While ACCOUNT+Reference = lcAccount For cfileCode =lcFileCode
  If !Found()
    Return .F.
  Endif
  SELECT 'TmpExcel' 
  INDEX ON Issue + SMART_INVOICE_NO TAG 'TmpExcel' 
  lnRemItAmt = 0
  lnEbPayAmt = 0
  ldPaymntDate = {}
  Select EBREMITT
  =Seek(lcAccount)
  Select SUM(APPRAMT) as RemitAmt From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode Into Cursor 'RemitTot'
  SELECT 'RemitTot'
  LOCATE 
  lnRemItAmt = RemitTot.RemitAmt 
  if !USED('EbPayMent')
    Use (dbfspath +'EBPAYMT.dbf') In 0 Shared Order EBPAYMT again Alias 'EbPayMent'
  endif 
  SELECT EbPayMent
  =Seek(lcAccount)
  LOCATE REST WHILE ACCOUNT+REFERENCE = lcAccount FOR cfileCode =lcFileCode 
  IF Found()
    lnEbPayAmt =  EbPayMent.nAmount
    ldPaymntDate = EbPayMent.dapdtrdat
  ENDIF
  IF lnRemItAmt  <> lnEbPayAmt 
    MESSAGEBOX("There is mismatch between Paid amount ("+Allt(STR(lnEbPayAmt ,12,2))+") and remittance amount ("+allt(STR(lnRemItAmt  ,12,2))+"). Cannot Proceed")
    use in 'EbPayMent'
    RETURN .F.
  ENDIF
  use in 'EbPayMent'
  
  Select EBREMITT
  =Seek(lcAccount)
  Select * From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode AND TranType $ 'AP' Order By crdrno Into Cursor 'RemitCHB'
  IF RECCOUNT('RemitCHB') > 0
    SELECT 'RemitCHB'
    LOCATE
    SCAN
      Wait window "Checking Smart Invoice # "+alltrim(RemitCHB.crdrno) nowait
      INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Paid_Amount,variance) VALUES (IIF(RemitCHB.APPRAMT > 0,'Credit on Account# ','Chargeback# ')+RemitCHB.crdrno,RemitCHB.crdrno,RemitCHB.APPRAMT,-1*RemitCHB.APPRAMT)
    ENDSCAN   
  ENDIF    
  
  Select EBREMITT
  =Seek(lcAccount)
  Select Distinct crdrno, .T. as 'Include' From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode AND TranType ='I' and '18318' $ crdrno Order By crdrno Into Cursor 'RemitsInv' READWRITE 
  Select 'RemitsInv'
  Locate
  SCAN
    Wait window "Checking Smart Invoice # "+alltrim(crdrno) nowait
    lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_HEADER Where SMART_INVOICE_NO ='"+allt(RemitsInv.crdrno)+"'",'SMRTSHPHDR' , ;
    "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
    *! C201850,1 MMT 08/15/2016 Change 820 program to Checked if payment record has no corresponding smart invoice[T20151014.0017][Start]
    *If lnResult<>1     
    If lnResult=1 
    *! C201850,1 MMT 08/15/2016 Change 820 program to Checked if payment record has no corresponding smart invoice[T20151014.0017][End]
      if RECCOUNT('SMART_INVOICE_HEADER') = 0
        Select EBREMITT
        =Seek(lcAccount)
        Locate Rest While ACCOUNT+Reference = lcAccount FOR cfileCode =lcFileCode AND Allt(crdrno)  = RemitsInv.crdrno
        INSERT INTO 'TmpExcel' (Issue,Paid_Amount,variance) VALUES ('Smart invoice# '+RemitsInv.crdrno + " not found for payment",EBREMITT.appramt,-1 * EBREMITT.appramt)
        REPLACE Include WITH .F. IN 'RemitsInv'
      else
        select SMART_INVOICE_HEADER
        locate
        if !Empty(SMART_INVOICE_HEADER.invoice) and SMART_INVOICE_HEADER.Status ='C'
          REPLACE Include WITH .F. IN 'RemitsInv'
        endif
      ENDIF  
      Select 'RemitsInv'
    ENDIF    
  ENDSCAN
  lcFirstInv = ''
  lcLastInv = ''
  Select 'RemitsInv'
  Locate
  lcFirstInv = allt(RemitsInv.crdrno)
  Go Bottom
  lcLastInv = Allt(RemitsInv.crdrno)
  lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_HEADER Where SMART_INVOICE_NO < '"+lcFirstInv+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
    "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
  If lnResult<>1
    oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
    Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
    Return .F.
  Else
    Select SMRTSHPHDR
    Locate
    If !Eof()
      SCAN FOR SHIPAMT <> 0
        Wait window "Checking Smart Invoice # "+alltrim(SMRTSHPHDR.SMART_INVOICE_NO) nowait
        INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount) VALUES ('Previous smart invoice# '+SMRTSHPHDR.SMART_INVOICE_NO + " is not paid",SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT)
      ENDSCAN 
    Endif
  Endif
  Select 'RemitsInv'
  Index On crdrno Tag 'RemitsInv'
  lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_HEADER Where SMART_INVOICE_NO >= '"+lcFirstInv+;
    "' AND  SMART_INVOICE_NO <= '"+lcLastInv +"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
    "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
  =Strtofile("33","X:\aria4xp\LogEDI.txt",1)
  SET STEP ON 
  If lnResult<>1
    oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
    Wait Window 'SMART_INVOICE_HEADER '+'.SqlRun()'
    Return .F.
  Else
    Select SMRTSHPHDR
    SCAN
      Wait window "Checking Smart Invoice # "+alltrim(SMRTSHPHDR.SMART_INVOICE_NO) nowait
      If !Seek(SMRTSHPHDR.SMART_INVOICE_NO ,'RemitsInv') AND SMRTSHPHDR.SHIPAMT <> 0
        INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,variance) VALUES ('Invoice is missing from Remittance Advices',SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT,SMRTSHPHDR.SHIPAMT)
      ELSE
        Select EBREMITT
        =Seek(lcAccount)
        Locate Rest While ACCOUNT+Reference = lcAccount FOR cfileCode =lcFileCode AND Allt(crdrno)  = SMRTSHPHDR.SMART_INVOICE_NO
        If (SMRTSHPHDR.SHIPAMT <>  EBREMITT.appramt)
          INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
          ('Invoice total amount is not matching the Remittance Advice amount',SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT,EBREMITT.appramt,SMRTSHPHDR.SHIPAMT-EBREMITT.appramt)
          REPLACE Include WITH .F. IN 'RemitsInv'
        Endif
      Endif
    Endscan
  ENDIF
  llIssueFound = .F.
  SELECT RemitsInv
  LOCATE FOR !Include 
  IF FOUND()
    llIssueFound = .T.
    SELECT 'TmpExcel'
    LOCATE 
    SUM Invoice_Amount,Paid_Amount,variance TO lnTotAmt,lnTotPay,lnvarTotal
    INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES ("Total",'',lnTotAmt,lnTotPay,lnvarTotal)
  ENDIF

*************************************************************************************
FUNCTION  gfModalGen 
  LPARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg
  IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
    oAriaApplication.oToolBar.Enabled = .F.
  ENDIF

  LOCAL oMessageBox, llActiveFormLocked, llFormExist
  oMessageBox = NEWOBJECT("AriaMessageBox",ADDBS(oAriaApplication.ClassDir)+"Utility.vcx")
  IF VARTYPE(oMessageBox) != "O"

    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF

    RETURN 0
  ENDIF 
  
  *-- Get the dialog and buttons from the dictionary.
  IF !oMessageBox.GetMessage(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)
    IF TYPE("oAriaApplication.oToolBar") = 'O' .AND. !ISNULL(oAriaApplication.oToolBar)
      oAriaApplication.oToolBar.Enabled = .T.
    ENDIF

    RETURN 0
  ENDIF 

  IF (TYPE("_SCREEN.ActiveForm.LockScreen") = "L") AND !EMPTY(TYPE("_SCREEN.ActiveForm.LockScreen")) AND TYPE('_SCREEN.ActiveForm') = 'O' AND !ISNULL(_SCREEN.ActiveForm)
    llFormExist = .T.
    llActiveFormLocked = _SCREEN.ActiveForm.LockScreen
    _SCREEN.ActiveForm.LockScreen = .F.
  ENDIF 

  PRIVATE lnMessageChoice
  lnMessageChoice = lnChoice
  RETURN lnMessageChoice  && Return the response.
ENDFUNC 