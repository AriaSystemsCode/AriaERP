*! C201814,1 AEG 05/11/2016 modify creating consolidated invoice to make invoice date to be payment date [T20151014.0017]
*! B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8]
*! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8]
*! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017]
*! C201850,1 MMT 08/15/2016 Change 820 program to Checked if payment record has no corresponding smart invoice[T20151014.0017]
*!C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL
*------------------------------------------------------------------------------------------------------------
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
*SYS(2335, 0)
*SET STEP ON 
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
*MT
lcCurProc = SET("Procedure")
lcCurProc = '"'+STRTRAN(UPPER(oAriaApplication.clientapplicationhome),'PRGS','')+'IKECREATEPAYMENT.FXP'+'",'+lcCurProc 
SET PROCEDURE TO &lcCurProc.
*XX1
lcEDIARCHIVEFolder =  ADDBS(STRTRAN(UPPER(oAriaApplication.clientapplicationhome),"\PRGS",""))+"EDI\INBOX\ARCHIVE\"
lcFileName = ALLTRIM(oAriaApplication.ActiveCompanyID)+ALLTRIM(lcFileCode)+".XLS"
*XX1
*MT
*oariaapplication = CREATEOBJECT("AriaApplication", aria27sysfilespath)
oAriaApplication.applicationhome = aria4path+"PRGS\"
oAriaApplication.Screenhome = aria4path+"SCREENS\"
oAriaApplication.clientapplicationhome =  Strtran(oAriaApplication.clientapplicationhome,'ARIA3EDI','ARIA4XP')
*oariaapplication.activecompanyid = companyid
*oariaapplication.getcompanyinformation (companyid)
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
*oariaapplication.SetUserLang('EN')
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

lcAppCrdt = gfTempName()
Use (dbfspath+'ARHIST.DBF') Shar  In 0
Select ARHIST
=Afields(laFileStru)
Use In ARHIST
lnFileStru = Alen(laFileStru,1)
Dimension laFileStru[lnFileStru+7,18]
laFileStru[lnFileStru+1,1] = 'Order'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'PikTkt'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 6
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'cStore'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 8
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'nTrnNewAmn'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 11
laFileStru[lnFileStru+4,4] = 2
laFileStru[lnFileStru+5,1] = 'cShToOpn'
laFileStru[lnFileStru+5,2] = 'C'
laFileStru[lnFileStru+5,3] = 1
laFileStru[lnFileStru+5,4] = 0

laFileStru[lnFileStru+6,1] = 'cReason'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 6
laFileStru[lnFileStru+6,4] = 0
  
laFileStru[lnFileStru+7,1] = 'cTranDesc'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 20
laFileStru[lnFileStru+7,4] = 0

For lnCount = 1 To 7
  Store '' To laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
    laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
    laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
    laFileStru[lnFileStru+lnCount,16]
  Store 0  To laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
Endfor
=gfCrtTmp(lcAppCrdt,@laFileStru,[Account+Order+cStore+PikTkt+Tran],lcAppCrdt)
*mt2
CREATE CURSOR 'CRDIRECT' (CtrCode C(6),Amount N(14,2),Type C(1),Order C(6),Store C(8))
Select 'CRDIRECT'
INDEX ON Type + CtrCode TAG 'CRDIRECT' 
*mt2
*XX1
CREATE CURSOR 'TmpExcel' (Issue C(100),SMART_INVOICE_NO C(18), Invoice_Amount N(10,2),Paid_Amount N(10,2),Variance N(10,2))
*XX1
*SET STEP ON 
*Mariam
oKeyOff = Newobject("ARKEYOFF",oAriaApplication.lcAria4Class+"AR.VCX")
*MAriam
Wait wind "Creating Payment..." nowait 
lcBatchNo =lfCreatePyamnet (lcAccount,lcFileCode)
wait clear
=Strtofile("16","X:\aria4xp\LogEDI.txt",1)
Wait wind "Creating Invoice..." nowait 
lcReturnInvoice = lfCreateConsInv(lcAccount,lcFileCode)
wait clear
=Strtofile("17","X:\aria4xp\LogEDI.txt",1)

*MT
Wait window "Preparing data for key off..." nowait 
if !USED('INVHDR_X')
  Use (dbfspath +'INVHDR.DBF') In 0 Shared  again  Order  INVHDRA ALIAS 'INVHDR_X'
ENDIF
Select 'INVHDR_X'
=Seek(lcAccount)
Select Invoice from INVHDR_X Where Account+Invoice =lcAccount And Consol ='Y' and allt(Note2) = allt(lcFileCode) and Status <>'V' into Array laInvCrt
if _tally > 0
  lcReturnInvoice = laInvCrt[1]
endif 
if alen(laInvCrt,1) > 1
  *MT
  for lnXX = 2 to alen(laInvCrt,1)
  *MT
    if !Seek('I'+laInvCrt[lnXX],'CRDIRECT' ,'CRDIRECT')
      INSERT into 'CRDIRECT' (Type , CtrCode ) values ('I',laInvCrt[lnXX])
    ENDIF
  ENDFOR
ENDIF
Select 'RemitsInv'
loca 
*XX1
*Scan for Include
SCAN
*XX1
  lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_LINES Where SMART_INVOICE_NO ='"+allt(RemitsInv.crdrno)+"' and CTRCODE <> '' and ISNULL(CTRCODE,'S') <> 'S'",'SMRTSHPLIN' , ;
     "SMART_INVOICE_LINES",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
   If lnResult=1 
     Select SMRTSHPLIN
     Loca 
     SCAN
       if !Seek(IIF(SMRTSHPLIN.TOTQTY > 0,'I','C')+SMRTSHPLIN.CTRCODE,'CRDIRECT' ,'CRDIRECT')
         if !(SMRTSHPLIN.TOTQTY > 0 AND SMRTSHPLIN.CTRCODE = lcReturnInvoice)
          INSERT into 'CRDIRECT' (Type , CtrCode) values (IIF(SMRTSHPLIN.TOTQTY > 0,'I','C'),SMRTSHPLIN.CTRCODE)
         ENDIF
       ENDIF
     ENDSCAN
   ENDIF
ENDSCan 
Wait clear
*MT


If !Empty(lcReturnInvoice)
  Wait wind "Applying Debit/Credit..." nowait 
  =Strtofile("18","X:\aria4xp\LogEDI.txt",1)
  llRetKey = lfKeyOff(lcAccount,lcReturnInvoice ,lcFileCode)
  =Strtofile("19","X:\aria4xp\LogEDI.txt",1)
  =Messagebox("Invoice# "+lcReturnInvoice+ "has been created"+Iif(Type('lcBatchNo') = 'C'," and batch# "+lcBatchNo +".",'.'))
  =Strtofile("20","X:\aria4xp\LogEDI.txt",1)
  *XX1
  **XX2
*!*	  if !Empty(lcFirstInv) and !Empt(lcLastInv)
*!*	    lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * "+;
*!*	      "from SMART_INVOICE_LINES Where SMART_INVOICE_NO >= '"+lcFirstInv+;
*!*	      "' AND  SMART_INVOICE_NO <= '"+lcLastInv +"'",'SMRTSHPLIN' ,;
*!*	      "SMART_INVOICE_LINES",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
*!*	    If lnResult<>1
*!*	      oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*	      Wait Window 'SMART_INVOICE_LINES '+'.SqlRun()'
*!*	      Return .F.
*!*	    ELSE
*!*	      Select SMRTSHPLIN
*!*	      LOCATE
*!*	      Wait wind "Calculating consolidated invoice total amount" nowait 
*!*	      SUM TotQty*Price TO lnConsAmt FOR !EMPTY(Order)  AND (!EMPTY(cTrCode)  And  !isnull(cTrCode))
*!*	    
*!*	      LOCATE
*!*	      Wait wind "Calculating direct invoices total amount" nowait 
*!*	      SUM TotQty*Price TO lnDirectAmt FOR (EMPTY(ALLTRIM(Order))  or isnull(Order)) AND (!EMPTY(cTrCode)  And  !isnull(cTrCode)) AND TotQty > 0
*!*	    
*!*	      LOCATE
*!*	      Wait wind "Calculating returns total amount" nowait 
*!*	      SUM TotQty*Price TO lnReturnAmt FOR (EMPTY(ALLTRIM(Order)) or isnull(Order))  AND (!EMPTY(cTrCode)  And  !isnull(cTrCode)) AND TotQty < 0    
*!*	    
*!*	      INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
*!*	      ("Total - Consolidated Invoice","",lnConsAmt,0,lnConsAmt)
*!*	      INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
*!*	      ("Total - Direct Invoices","",lnDirectAmt,0,lnDirectAmt)
*!*	      INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
*!*	      ("Total - Returns","",lnReturnAmt,0,lnReturnAmt)
*!*	    ENDIF 
*!*	  ENDIF 
  **XX2
  
  
  
  
  SELECT 'TmpExcel'
  EXPORT TO (lcEDIARCHIVEFolder+lcFileName) TYPE XLS
  DECLARE INTEGER ShellExecute IN shell32.DLL ;
    INTEGER hndWin, STRING cAction,  STRING cFileName, STRING cParams, STRING cDir, INTEGER nShowWin
  ShellExecute(0 ,'open' ,lcEDIARCHIVEFolder+lcFileName,"","",1)
  *XX1
  iF USed('CRDIRECT')
    use in 'CRDIRECT'
  ENDIF
  if used('SMRTSHPLIN')  
    use in  SMRTSHPLIN
  ENDIF  
  if used('RemitsInv')
    use in 'RemitsInv'
  ENDIF
  if used('INVHDR_X')  
    use IN 'INVHDR_X'
  ENDIF  
  if used(lcAppCrdt)
    USE IN (lcAppCrdt)
  ENDIF
  release oKeyOff 
  On Error &lcErroHandler.
  *
  =Strtofile("21","X:\aria4xp\LogEDI.txt",1)
Else
  =Strtofile("22","X:\aria4xp\LogEDI.txt",1)
  On Error &lcErroHandler.
  =Strtofile("23","X:\aria4xp\LogEDI.txt",1)
  Return .F.
Endif
*************************************************************
Function  lfCreatePyamnet
  Parameters lcAccount,lcFileCode
  Dimension laRetunArr[1]
  laRetunArr = ''
  Use (dbfspath +'ARCUSHST.DBF') In 0 Shared  Order  Acthst && ACCOUNT+CFISFYEAR
  Use (dbfspath +'Credit.dbf') In 0 Shared
  *IF !USED('EBPAYMT')
    Use (dbfspath +'EBPAYMT.dbf') In 0 Shared Order EBPAYMT&& ACCOUNT+REFERENCE
  *ENDIF  
  *USE (dbfspath +'Customer.dbf') IN 0 SHARED ORDER Customer
  Use (dbfspath +'ApPayMnt.dbf') In 0 Shared
  Use (dbfspath +'ApChecks.dbf') In 0 Shared Order BANKCHECK   && CBNKCODE+CCHKACCT
  *USE (dbfspath +'EDIACPRT.dbf') IN 0 SHARED  ORDER ACCFACT
  *USE (dbfspath +'EDIPD.dbf') IN 0 SHARED order  PARTTRANS
  *USE (dbfspath +'EDITRANS.dbf') IN 0 SHARED Order TYPEKEY
  
  *C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[Begin]
*  Use (dbfspath +'GLDIST.dbf') In 0 Shared Order GLDISTAC
    lnConnH = Sqlstringconnect(OAriaApplication.ActiveCompanyConStr)
    lnRes = SQLEXEC(lnConnH ,"Select * from GLDIST WHERE 1=2",'GLDIST')
  *C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
   
  Use (dbfspath +'CODES.dbf') In 0 Shared again 

  ****
  *SET ORDER TO EBPAYMT   && ACCOUNT+REFERENCE
  Select EBPAYMT
  *!*	    SET ORDER TO EBPAYMT   && ACCOUNT+REFERENCE
  *!*	    =SEEK(lcAccount)
  *!*	    SET FILTER TO ACCOUNT =
  ****

  lcTmpGLD   = gfTempName()
  *TmpCrdtGL  = gfTempName()

  *C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[Begin]
 * llSuccess = CursorSetProp("Buffering", 5, "GLDIST")
 *C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
   
  Select GLDIST
  Copy Structure To (oAriaApplication.WorkDir+lcTmpGLD)
  Use (oAriaApplication.WorkDir+lcTmpGLD) Exclusive In 0

  llEdiAccount = .F.
  Select CODES
  Set Order To Tag Idrltfname
  =Seek('NYCSITEID')
  Locate Rest While cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
    FOR   cRltd_Nam = 'CCMSITETYP' And cRltd_Vlu= 'B'
  If Found()
    lcSiteId = CODES.cCode_No
    Select EDiAcPrt
    Locate For cSiteId = lcSiteId
    If Found('EDIACPRT') And Seek(EDiAcPrt.cPartCode+'812','EDIPD')
      llEdiAccount = .T.
      *change the Table Buffring mode to be optimistic
      llSuccess = CursorSetProp("Buffering", 5, "EDITRANS")
    Endif
  Endif

  =Seek('M'+lcAccount,'Customer','Customer')
  lcFacCode = CUSTOMER.CFACCODE
  Select EBPAYMT
  *=CursorSetProp("Buffering" ,5)
  =Seek(lcAccount)
  Locate While ACCOUNT+Reference = lcAccount For cfileCode = lcFileCode And  !Invoiced And EBPAYMT.namount > 0
  If !Found()


    Use In ARCusHst
    Use In Credit
    Use In EBPAYMT
    *USE IN Customer
    Use In ApPayMnt
    Use In ApChecks
    *USE IN EDIACPRT
    *USE IN EDIPD
    *USE IN EDITRANS
  *

   Use In GLDIST

   Use In CODES
    Return .T.
  Else
    *      lcOldhand = ON("Error")
    *      ON ERROR x=10
    lcBatSeqNo = gfSequence("BATCH", companyid)

    *      ON ERROR &lcOldhand.
    =Seek(lcAccount)
    Scan Rest While ACCOUNT+Reference = lcAccount For cfileCode = lcFileCode And !Invoiced And EBPAYMT.namount > 0
      Replace Invoiced With .T.
      lcBankCode = Substr(EBPAYMT.crecbank,1,8)
      lcChkAcnt  = EBPAYMT.cchkacct
      If !Empty(lcChkAcnt) And Seek(lcBankCode+lcChkAcnt,"ApChecks")
        lcGLAcct2  = ApChecks.cChkGLAcc
      Else
        lcGLAcct2  = Space(1)
      Endif

      lcGlFYear = ''
      lcGlPeriod= ''
      ldBatDate = EBPAYMT.dapdtrdat
      lcPayComp = ''
      *--Check this date  period
      *        lcOldhand = ON("Error")
      *        ON ERROR x=10
      If checkprd(ldBatDate,"lcGLFYear","lcGLPeriod",'CR',.T.)
        lcTrnSeqNo = gfSequence("TRAN", companyid)
        If !Empty(laRetunArr[1])
          Dimension laRetunArr[ALEN(laRetunArr,1)+1]
        Endif
        laRetunArr[ALEN(laRetunArr,1)]= Alltrim(lcBatSeqNo) + "-"+lcTrnSeqNo
        *          ON ERROR &lcOldhand.
        llNoThing  =  lfUpdCust(lcAccount,-1 * Abs(EBPAYMT.namount))
        llNoThing  =  lfUpdCusHs(lcAccount,-1 * Abs(EBPAYMT.namount))
        llNoThing  =  lfUpdApPay(lcAccount,-1 * Abs(EBPAYMT.namount))
        llNoThing  = lfUpdTmp(lcAccount,-1 * Abs(EBPAYMT.namount))
        If llEdiAccount
          Insert Into 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,cStatus,lInterComp) Values ;
            ('812',"4"+lcTrnSeqNo ,'A',EDiAcPrt.cPartner,'N',EDiAcPrt.lInterComp)
        Endif
        lfUpdGL()
      Endif
      *        ON ERROR &lcOldhand.
    Endscan
  Endif
  lcGLSeqNo = gfSequence("GLSESSION", companyid)
  Select (lcTmpGLD)
  Replace All GLSESSION With lcGLSeqNo
  

*C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[Begin]
*Use In (lcTmpGLD)
*  Select GLDIST
 * Append From (oAriaApplication.WorkDir+lcTmpGLD)
  
  SELECT (lcTmpGLD)
  SCAN 
  SCATTER MEMVAR MEMO
  *m.Oid =gfTempName()
  lcstat =  "INSERT INTO GLDIST (glsession,glaccount,nglamount,tran_date,tran_no,tran_type,tran_desc,catg_key,"+;
                 "posted,printed,glbatch,ctrnsledn,nentryno,glcomp,glperiod,glfyear,glacnttype,ccurrcode,ncurrunit,"+;
                 "nexrate,neqvamnt,cglref,cadd_user,cadd_time,dadd_date,llok_stat,clok_user,dlok_date,clok_time,cowner,"+;
                 "cedit_user,dedit_date,cedit_time,cadd_ver,cedt_ver) VALUES (m.glsession,m.glaccount,m.nglamount,m.tran_date,"+;
                 "m.tran_no,m.tran_type,m.tran_desc,m.catg_key,m.posted,m.printed,m.glbatch,m.ctrnsledn,m.nentryno,m.glcomp,"+;
                 "m.glperiod,m.glfyear,m.glacnttype,m.ccurrcode,m.ncurrunit,m.nexrate,m.neqvamnt,m.cglref,m.cadd_user,m.cadd_time,"+;
                 "m.dadd_date,m.llok_stat,m.clok_user,m.dlok_date,m.clok_time,m.cowner,m.cedit_user,m.dedit_date,m.cedit_time,m.cadd_ver,m.cedt_ver)"
       lnRemResult = SQLExec(lnConnH , lcstat)
       If lnRemResult < 0
         RETURN 
       ENDIF 
  ENDSCAN
    SELECT (lcTmpGLD)
    USE 
      *Tableupdate(.T.)
*C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
  


  If llEdiAccount
    Select EDITRANS
    =Tableupdate(.T.)
  Endif

*  Select EBPAYMT
 * =Tableupdate(.T.)

  Select ApPayMnt
  =Tableupdate(.T.)
  Use In ARCusHst
  Use In Credit
  Use In EBPAYMT
  *MMT
*  Use In CUSTOMER
  *MMT
  Use In ApPayMnt
  Use In ApChecks
  Use In EDiAcPrt
  Use In EDIPD
  Use In EDITRANS
  
  
  Use In GLDIST
  
    
  
  Use In CODES

  Return (lcBatSeqNo)
  ******************************************************************************************
Function lfUpdCust
  Parameters lcAccountNO,lnAmount
  If Seek("M" + lcAccountNO, "Customer")
    lcPayComp = CUSTOMER.BTName
    lnEqvAmnt = lnAmount
    Select CUSTOMER
    llNoThing = Rlock()
    Replace OpenCr With OpenCr + lnEqvAmnt ,;
      NetBal With NetBal + lnEqvAmnt ,;
      nHgWtrMark With Iif(NetBal>nHgWtrMark,NetBal,nHgWtrMark)
    Unlock
  Endif
  ******************************************************************************************
Function lfUpdCusHs
  Parameters lcAccountNO,lnAmount
  If Between(Val(lcGlPeriod), 1, 13)
    lcPeriod  = Padl(Alltrim(lcGlPeriod), 2, "0" )
    lnEqvAmnt = Round(lnAmount, 2)
    lnEqvAmnt = Abs(lnEqvAmnt)
    If Seek(lcAccountNO+lcGlFYear, "ARCusHst")
      Select ARCusHst
      llNoThing = Rlock()
      Replace nPayment&lcPeriod With nPayment&lcPeriod + lnEqvAmnt ,;
        nPayment          With nPayment          + lnEqvAmnt
      Unlock
    Endif
  Endif
  ********************************************************************************************
Function lfUpdApPay
  Parameters lcAccountNO,lnAmount
  llSuccess = CursorSetProp("Buffering", 5, "ApPayMnt")
  Select ApPayMnt
  Append Blank
  llNoThing = Rlock()
  Replace cPayType  With "A"                              ,;
    cPayMeth  With 'A' ,;
    cBnkCode  With lcBankCode                       ,;
    cchkacct  With lcChkAcnt                        ,;
    dPayDate  With ldBatDate                        ,;
    cFisFYear With lcGlFYear                        ,;
    cFspprdid With lcGlPeriod                       ,;
    cPayDocNo With ''               ,;
    cPayClNo  With lcAccountNO           ,;
    cPayComp  With lcPayComp                        ,;
    nPayAmnt  With lnAmount,;
    cCurrCode With 'USD'  ,;
    nExRate   With 1                         ,;
    nCurrUnit With 1,;
    cPayRecSt With "O"                              ,;
    BATCH     With lcBatSeqNo

  Unlock
  *******************************************************************************************
Function lfUpdTmp
  Parameters lcAccountNO,lnAmount

  Dimension laTermAry[1,2]
  laTermAry[1,1] = "CARGLACC"
  laTermAry[1,2] = "lcGLAcct"
  lcGLAcct =''
  llNoThing = gfRltFld('000043', @laTermAry, "CARPTYPE")

  Select Credit
  llSuccess = CursorSetProp("Buffering", 5, "Credit")
  Append Blank
  Replace ACCOUNT With lcAccountNO,;
    Amount With lnAmount ,;
    DPOSTDATE With ldBatDate ,;
    Store With EBPAYMT.Reference  ,;
    cArpType With '000043',;
    carglacc With lcGLAcct,;
    CFACCODE With lcFacCode

  Replace cAdjAcct   With lcGLAcct2                       ,;
    cBnkCode   With lcBankCode                      ,;
    cchkacct   With lcChkAcnt                       ,;
    TranDate   With ldBatDate                       ,;
    TranType   With "4"                             ,;
    TRAN       With lcTrnSeqNo                      ,;
    DESC       With "CHECK"  ,;
    BATCH      With lcBatSeqNo                      ,;
    cCurrCode  With 'USD'                    ,;
    nCurrUnit  With 1,;
    nExRate    With 1
  Select Credit 
  TableUpdate(.T.)  
  *!*  SCATTER MEMO MEMVAR
  *!*  m.nTrnNewAmn = m.Amount
  *!*  m.cShToOpn   = IIF(m.nTrnNewAmn<0,'N','Y')
  *!*  INSERT INTO (lcAppCrdt) FROM MEMVAR

  *******************************************************************************************
Function lfUpdGL

  Goto Bottom In Credit
  Select Credit
  Go Bottom
  Do GLDIST With Credit.Link_Code,'002',Abs(Credit.Amount)      ,;
    'CR',Credit.Tran,ldBatDate,lcGlFYear,lcGlPeriod,'&lcTmpGLD',;
    Credit.cAdjAcct,'USD',1,1
  Do GLDIST With Credit.Link_Code,'001',-Abs(Credit.Amount)     ,;
    'CR',Credit.Tran,ldBatDate,lcGlFYear,lcGlPeriod,'&lcTmpGLD',;
    Credit.carglacc,'USD',1,1
  *******************************************************************************************
Function lfCreateInvTemp
  Select InvHdr
  =Afields(laFileStru)
  lnFileStru = Alen(laFileStru,1)
  Dimension laFileStru[lnFileStru+14,18]
  laFileStru[lnFileStru+1,1] = 'cSelect'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'Picked'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 7
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = 'lUpsIns'
  laFileStru[lnFileStru+3,2] = 'L'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0
  laFileStru[lnFileStru+4,1] = 'nChrgTax'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 13
  laFileStru[lnFileStru+4,4] = 2
  laFileStru[lnFileStru+5,1] = 'nMerchTax'
  laFileStru[lnFileStru+5,2] = 'N'
  laFileStru[lnFileStru+5,3] = 17
  laFileStru[lnFileStru+5,4] = 8
  laFileStru[lnFileStru+6,1] = 'lCompUps'
  laFileStru[lnFileStru+6,2] = 'L'
  laFileStru[lnFileStru+6,3] = 1
  laFileStru[lnFileStru+6,4] = 0
  laFileStru[lnFileStru+7,1] = 'LastLine'
  laFileStru[lnFileStru+7,2] = 'N'
  laFileStru[lnFileStru+7,3] = 6
  laFileStru[lnFileStru+7,4] = 0
  laFileStru[lnFileStru+8,1] = 'LKEYOFF'
  laFileStru[lnFileStru+8,2] = 'L'
  laFileStru[lnFileStru+8,3] = 0
  laFileStru[lnFileStru+8,4] = 0
  laFileStru[lnFileStru+9,1] = 'NTAXDUE'
  laFileStru[lnFileStru+9,2] = 'N'
  laFileStru[lnFileStru+9,3] = 17
  laFileStru[lnFileStru+9,4] = 6
  laFileStru[lnFileStru+10,1] = 'NCARTONS'
  laFileStru[lnFileStru+10,2] = 'N'
  laFileStru[lnFileStru+10,3] = 11
  laFileStru[lnFileStru+10,4] = 5
  laFileStru[lnFileStru+11,1] = 'Ordered'
  laFileStru[lnFileStru+11,2] = 'N'
  laFileStru[lnFileStru+11,3] = 7
  laFileStru[lnFileStru+11,4] = 0
  laFileStru[lnFileStru+12,1] = 'cConStore'
  laFileStru[lnFileStru+12,2] = 'C'
  laFileStru[lnFileStru+12,3] = 8
  laFileStru[lnFileStru+12,4] = 0
  laFileStru[lnFileStru+13,1] = 'nOldCarton'
  laFileStru[lnFileStru+13,2] = 'N'
  laFileStru[lnFileStru+13,3] = 5
  laFileStru[lnFileStru+13,4] = 0
  laFileStru[lnFileStru+14,1] = 'nOldWight'
  laFileStru[lnFileStru+14,2] = 'N'
  laFileStru[lnFileStru+14,3] = 8
  laFileStru[lnFileStru+14,4] = 2
  lnNewArrLen = Alen(laFileStru,1)+1
  Dimension laFileStru[lnNewArrLen,ALEN(laFileStru,2)]
  laFileStru[lnNewArrLen,1] = 'cEdtCrtns'
  laFileStru[lnNewArrLen,2] = 'C'
  laFileStru[lnNewArrLen,3] = 1
  laFileStru[lnNewArrLen,4] = 0
  For lnCount = 1 To Alen(laFileStru,1)-lnFileStru
    Store '' To laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
      laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
      laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
      laFileStru[lnFileStru+lnCount,16]
    Store 0  To laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
  Endfor


  Declare laIndex[3,2]
  laIndex[1,1] = 'Account+Order+Store+PikTkt+cDivision'
  laIndex[1,2] = lcInvHdr
  laIndex[2,1] = 'Consol+Account+cDivision+cCurrCode+Dist_Ctr'
  laIndex[2,2] = 'Consol'
  laIndex[3,1] = 'Account+Dist_Ctr+Order+Store+PikTkt+cDivision'
  laIndex[3,2] = 'ConsDist'

  =gfCrtTmp(lcInvHdr,@laFileStru,@laIndex)
  =gfCrtTmp(lcConsInvH,@laFileStru,[Consol+Account+cDivision+cCurrCode+Dist_Ctr],lcConsInvH)

  Select OrdLine
  =Afields(laFileStru)
  lnFileStru = Alen(laFileStru,1)
  If llUseTradeDisc
    Dimension laFileStru[lnFileStru+20,18]
  Else
    Dimension laFileStru[lnFileStru+17,18]
  Endif
  laFileStru[lnFileStru+1,1] = 'LNEWLINE'
  laFileStru[lnFileStru+1,2] = 'L'
  laFileStru[lnFileStru+1,3] = 0
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'LPACKED'
  laFileStru[lnFileStru+2,2] = 'L'
  laFileStru[lnFileStru+2,3] = 0
  laFileStru[lnFileStru+2,4] = 0
  laFileStru[lnFileStru+3,1] = 'LBACKORD'
  laFileStru[lnFileStru+3,2] = 'L'
  laFileStru[lnFileStru+3,3] = 0
  laFileStru[lnFileStru+3,4] = 0
  laFileStru[lnFileStru+4,1] = 'nTaxRate'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 10
  laFileStru[lnFileStru+4,4] = 2
  laFileStru[lnFileStru+5,1] = 'cCurrCode'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 3
  laFileStru[lnFileStru+5,4] = 0
  laFileStru[lnFileStru+6,1] = 'cDivision'
  laFileStru[lnFileStru+6,2] = 'C'
  laFileStru[lnFileStru+6,3] = 6
  laFileStru[lnFileStru+6,4] = 0
  laFileStru[lnFileStru+7,1] = 'Consol'
  laFileStru[lnFileStru+7,2] = 'C'
  laFileStru[lnFileStru+7,3] = 1
  laFileStru[lnFileStru+7,4] = 0
  laFileStru[lnFileStru+8,1] = 'nNetAmnt'
  laFileStru[lnFileStru+8,2] = 'N'
  laFileStru[lnFileStru+8,3] = 18
  laFileStru[lnFileStru+8,4] = 10
  laFileStru[lnFileStru+9,1] = 'nGrosAmnt'
  laFileStru[lnFileStru+9,2] = 'N'
  laFileStru[lnFileStru+9,3] = 18
  laFileStru[lnFileStru+9,4] = 10
  laFileStru[lnFileStru+10,1] = 'LTAXABLE'
  laFileStru[lnFileStru+10,2] = 'L'
  laFileStru[lnFileStru+10,3] = 0
  laFileStru[lnFileStru+10,4] = 0
  laFileStru[lnFileStru+11,1] = 'cDyeFlag'
  laFileStru[lnFileStru+11,2] = 'C'
  laFileStru[lnFileStru+11,3] = 1
  laFileStru[lnFileStru+11,4] = 0
  laFileStru[lnFileStru+12,1] = 'cConStore'
  laFileStru[lnFileStru+12,2] = 'C'
  laFileStru[lnFileStru+12,3] = 8
  laFileStru[lnFileStru+12,4] = 0
  laFileStru[lnFileStru+13,1] = 'lContract'
  laFileStru[lnFileStru+13,2] = 'L'
  laFileStru[lnFileStru+13,3] = 0
  laFileStru[lnFileStru+13,4] = 0
  laFileStru[lnFileStru+14,1] = 'Dist_Ctr'
  laFileStru[lnFileStru+14,2] = 'C'
  laFileStru[lnFileStru+14,3] = 8
  laFileStru[lnFileStru+14,4] = 0
  laFileStru[lnFileStru+15,1] = 'llUpdHdr'
  laFileStru[lnFileStru+15,2] = 'L'
  laFileStru[lnFileStru+15,3] = 1
  laFileStru[lnFileStru+15,4] = 0
  laFileStru[lnFileStru+16,1] = 'OldDyelot'
  laFileStru[lnFileStru+16,2] = 'C'
  laFileStru[lnFileStru+16,3] = 10
  laFileStru[lnFileStru+16,4] = 0
  laFileStru[lnFileStru+17,1] = 'Nordline'
  laFileStru[lnFileStru+17,2] = 'N'
  laFileStru[lnFileStru+17,3] = 6
  laFileStru[lnFileStru+17,4] = 0
  If llUseTradeDisc
    laFileStru[lnFileStru+18,1] = 'TRD_price'
    laFileStru[lnFileStru+18,2] = 'N'
    laFileStru[lnFileStru+18,3] =  12
    laFileStru[lnFileStru+18,4] = 2
    laFileStru[lnFileStru+19,1] = 'nTrdNetAmt'
    laFileStru[lnFileStru+19,2] = 'N'
    laFileStru[lnFileStru+19,3] =  18
    laFileStru[lnFileStru+19,4] = 10
    laFileStru[lnFileStru+20,1] = 'OTRDDISC'
    laFileStru[lnFileStru+20,2] = 'N'
    laFileStru[lnFileStru+20,3] =  5
    laFileStru[lnFileStru+20,4] = 2
  Endif
  For lnCount = 1 To Iif(llUseTradeDisc,20,17)
    Store '' To laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
      laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
      laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
      laFileStru[lnFileStru+lnCount,16]
    Store 0  To laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
  Endfor

  *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][Start]
  *Declare laIndex[8,2]
  Declare laIndex[9,2]
  *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][End]
  laIndex[1,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
  laIndex[1,2] = lcInvLine
  laIndex[2,1] = 'Account+Order+Store+PikTkt+Style+Dyelot'
  laIndex[2,2] = 'InvLines'
  laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Style+Dyelot+Dist_Ctr'
  laIndex[3,2] = 'Consol'
  laIndex[4,1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
  laIndex[4,2] = 'Dyelot'
  laIndex[5,1] = 'Account+Order+Store+PikTkt+Consol+cDivision+Dist_Ctr'
  laIndex[5,2] = 'ConsDiv'
  laIndex[6,1] = 'Account+Dist_Ctr+Order+Store+PikTkt+STR(LineNo,6)'
  laIndex[6,2] = 'ConsDist'
  laIndex[7,1] = 'Order+Store+STYLE+Dyelot+STR(LineNo,6)+Dist_Ctr+Consol'
  laIndex[7,2] = 'CONFIGLIN'
  laIndex[8,1] = 'Account+Order+Store+Consol+cDivision+Dist_Ctr'
  laIndex[8,2] = 'ConsDivFlt'
  *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][Start]
  laIndex[9,1] = 'Account+Order+Store+STYLE+STR(NORDLINE,6)'
  laIndex[9,2] = 'ConsORDLN'
  *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][End]
  =gfCrtTmp(lcInvLine,@laFileStru,@laIndex)
  =gfCrtTmp(lcConsInvD,@laFileStru,[Consol+Account+cDivision+cCurrCode+cWareCode+Style+Dyelot+Dist_Ctr],lcConsInvD)
  ********************************************************************************************
Function lfCreateConsInv
  Parameters lcAccount,lcFileCode
  =Strtofile("30","X:\aria4xp\LogEDI.txt",1)
  *IF !USED('EBREMITT')
    Use (dbfspath +'EBREMITT.DBF') In 0 Shared  Order EBREMITT   && ACCOUNT+REFERENCE+STR(LINENO,6)
  *ENDIF  
  *USE (dbfspath +'Customer.DBF')IN 0 SHARED  Order CUSTOMER
  Use (dbfspath +'ORDHDR.DBF')In 0 Shared  Order ORDHDR
  Use (dbfspath +'STYLE.DBF')In 0 Shared  Order Style
  Use (dbfspath +'StyDye.DBF')In 0 Shared  Order STYDYE
  Use (dbfspath +'INVHDR.DBF')In 0 Shared  Order InvHdr
  Use (dbfspath +'invline.DBF') In 0 Shared Order invline
  Use (dbfspath +'Ordline.DBF')In 0 Shared  Order OrdLine
  =Strtofile("31","X:\aria4xp\LogEDI.txt",1)
  Select EBREMITT
  =Strtofile("MMT1","X:\aria4xp\LogEDI.txt",1)
  =Seek(lcAccount)
  =Strtofile("MMT2","X:\aria4xp\LogEDI.txt",1)
  Locate Rest While ACCOUNT+Reference = lcAccount For cfileCode =lcFileCode And !Invoiced
  =Strtofile("MMT3","X:\aria4xp\LogEDI.txt",1)
  If !Found()
    =Strtofile("MMT4","X:\aria4xp\LogEDI.txt",1)
    *=MESSAGEBOX("The related Remittance Advices are invoiced, cannot proceed.")&& ,0,_SCREEN.CAPTION
    =Strtofile("MMT5","X:\aria4xp\LogEDI.txt",1)
    Return .F.
  Endif
  =Strtofile("MMT6","X:\aria4xp\LogEDI.txt",1)
  *MMT
  *XX1
  *CREATE CURSOR 'TmpExcel' (Issue C(100),SMART_INVOICE_NO C(18), Invoice_Amount N(10,2),Paid_Amount N(10,2))
  *XX1
  SELECT 'TmpExcel' 
  INDEX ON Issue + SMART_INVOICE_NO TAG 'TmpExcel' 
  *MMT
  ***Create Cursor 'TmpStr' (mStrRep M(10))
  
  *Mariam
*  oKeyOff = Newobject("ARKEYOFF",oAriaApplication.lcAria4Class+"AR.VCX")
  lnRemItAmt = 0
  lnEbPayAmt = 0
*!*C201814,1 05/11/2016 AEG modify creating consolidated invoice to make invoice date to be payment date [BEGIN]
  ldPaymntDate = {}
  *!*C201814,1 05/11/2016 AEG modify creating consolidated invoice to make invoice date to be payment date [END]
  Select EBREMITT
  =Seek(lcAccount)
  Select SUM(APPRAMT) as RemitAmt From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode Into Cursor 'RemitTot'
  SELECT 'RemitTot'
  LOCATE 
  lnRemItAmt = RemitTot.RemitAmt 
*!*	  _Screen.Visible = .T.
*!*	  set step on
  if !USED('EbPayMent')
   Use (dbfspath +'EBPAYMT.dbf') In 0 Shared Order EBPAYMT again Alias 'EbPayMent'
  endif 
  SELECT EbPayMent
  =Seek(lcAccount)
  LOCATE REST WHILE ACCOUNT+REFERENCE = lcAccount FOR cfileCode =lcFileCode 
  IF Found()
    lnEbPayAmt =  EbPayMent.nAmount
    *!*C201814,1 05/11/2016 AEG modify creating consolidated invoice to make invoice date to be payment date [BEGIN]
    ldPaymntDate = EbPayMent.dapdtrdat
    *!*C201814,1 05/11/2016 AEG modify creating consolidated invoice to make invoice date to be payment date [END]
  ENDIF
  IF lnRemItAmt  <> lnEbPayAmt 
    MESSAGEBOX("There is mismatch between Paid amount ("+Allt(STR(lnEbPayAmt ,12,2))+") and remittance amount ("+allt(STR(lnRemItAmt  ,12,2))+"). Cannot Proceed")
    use in 'EbPayMent'
    RETURN .F.
  ENDIF
  use in 'EbPayMent'
  
  ****Adjustment********[Start]
*!*	  Select EBREMITT
*!*	  =Seek(lcAccount)
*!*	  Select * From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode AND TranType ='A' Order By crdrno Into Cursor 'RemitAdj'
*!*	  IF RECCOUNT('RemitAdj') > 0
*!*	    IF !USED('Codes_DEF')
*!*	      Use (dbfspath +'CODES.dbf') In 0 Shared AGAIN Alias Codes_DEF  Order CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
*!*	    ENDIF
*!*	    *ras 2016/05/21 fix bug it didn't open COdes in gfRltFld using again [begin]
*!*	    IF !USED("CODES")
*!*	      USE (dbfspath +'CODES.dbf') IN 0  Shared AGAIN Alias CODES 
*!*	    ENDIF
*!*	    *ras 2016/05/21 fix bug it didn't open COdes in gfRltFld using again [end]
*!*	    =SEEK('D'+"CCREDITCOD",'Codes_DEF','CCODE_NO')
*!*	    lcCreditReason = Codes_DEF.CCODE_NO
*!*	    lcCreditRDesc = Codes_DEF.cdiscrep
*!*	    =SEEK('D'+"TRANCODE",'Codes_DEF','CCODE_NO')
*!*	    lcDebitReason =  Codes_DEF.CCODE_NO
*!*	    lcDebitRDesc = Codes_DEF.cdiscrep
*!*	    lcDcGLAccnt = ''
*!*	    lcCcGLAccnt = ''
*!*	    DIMENSION laTermAry[1,2]
*!*	    laTermAry[1,1] = "CADJACCT  "
*!*	    laTermAry[1,2] = "lcGLAccnt"
*!*	    lcGLAccnt = SPACE(0)
*!*	    llNoThing      = gfRltFld(lcCreditReason , @laTermAry, "CCREDITCOD")
*!*	    lcCcGLAccnt = lcGLAccnt 
*!*	    lcGLAccnt = SPACE(0)
*!*	    llNoThing      = gfRltFld(lcDebitReason , @laTermAry, "TRANCODE")
*!*	    lcDcGLAccnt = lcGLAccnt 
*!*	    *-- Fill the related GL information from the codes file.
*!*	    

*!*	    IF !USED('Customer_Def')
*!*	      Use (dbfspath +'Customer.dbf') In 0 Shared AGAIN ALIAS Customer_Def  Order Customer
*!*	    ENDIF
*!*	    =SEEK('M'+lcAccount,'Customer_Def','Customer')
*!*	    select  'RemitAdj'
*!*	    loca 
*!*	    SCAN 
*!*	      lcGlFYear = ''
*!*	      lcGlPeriod=''
*!*	      =CheckPrd(ldBatDate,"lcGLFYear","lcGLPeriod",'CR',.T.)
*!*	      SELECT (oKeyOff.cDCOnATmp)
*!*	      lcDCOnATmp = oKeyOff.cDCOnATmp
*!*	      APPEND BLANK 
*!*	      REPLACE Account   WITH lcAccount   ,;
*!*	              Tran      WITH 'T'+SUBSTR(SYS(3),4,5) ,;
*!*	              TranDate  WITH RemitAdj.trandate ,;
*!*	              Amount    WITH 0                      ,;
*!*	              cFacCode  WITH Customer_Def.cFacCode  ,;
*!*	              cCurrCode WITH 'USD',;
*!*	              nExRate   WITH 1,;
*!*	              nCurrUnit WITH 1,;
*!*	              cTranDesc WITH 'N/A'                  ,;
*!*	              cYear     WITH lcGlFYear ,;
*!*	              cPrd      WITH lcGlPeriod,;
*!*	              TranType  WITH SUBSTR("27",IIF(RemitAdj.APPRAMT > 0,2,1),1) ,;
*!*	              cReason   WITH IIF(RemitAdj.APPRAMT > 0,lcCreditReason ,lcDebitReason ),;
*!*	              Desc      WITH IIF(RemitAdj.APPRAMT > 0,lcCreditRDesc ,lcDebitRDesc ),;
*!*	              dPostDate WITH RemitAdj.trandate ,;
*!*	              cBnkCode  WITH '',;
*!*	              cChkAcct  WITH '',;
*!*	              cAdjAcct  WITH IIF(TranType="7",lcCcGLAccnt,lcDcGLAccnt),;
*!*	              Deb_Adj   WITH IIF(TranType = "2", "Y", Deb_Adj) ,;
*!*	              Amount    with IIF(TranType="7", -1 * RemitAdj.APPRAMT ,ABS(RemitAdj.APPRAMT)),;
*!*	              Reference  WITH RemitAdj.crdrno
*!*	              
*!*	      INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Paid_Amount) VALUES (IIF(RemitAdj.APPRAMT > 0,'Credit Adjustment# ','Debit Adjustment# ')+RemitAdj.crdrno,RemitAdj.crdrno,RemitAdj.APPRAMT)
*!*	    ENDSCAN      
*!*	    use in 'Codes_DEF'
*!*	    use in 'Customer_Def'
*!*	  ENDIF        
  ****Adjustment********[End]
  
  Select EBREMITT
  =Seek(lcAccount)
  Select * From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode AND TranType $ 'AP' Order By crdrno Into Cursor 'RemitCHB'
  IF RECCOUNT('RemitCHB') > 0
    IF !USED('Codes_DEF')
      Use (dbfspath +'CODES.dbf') In 0 Shared AGAIN Alias Codes_DEF  Order CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
    ENDIF
    IF !USED('Customer_Def')
      Use (dbfspath +'Customer.dbf') In 0 Shared AGAIN ALIAS Customer_Def  Order Customer
    ENDIF
    IF !USED('FACTOR_Def')
      Use (dbfspath +'FACTOR.dbf') In 0 Shared AGAIN ALIAS FACTOR_Def  Order FACTOR
    ENDIF
    IF !USED('GL_LINK_DEF')
      USE (dbfspath +'GL_LINK.dbf') In 0 Shared AGAIN ALIAS GL_LINK_DEF Order GL_LINK   && LINK_CODE+CATGKEY
    ENDIF
*!*	    
*!*	    IF !USED('CREDIT_DEF')
*!*	      USE (dbfspath +'CREDIT.dbf') In 0 Shared AGAIN ALIAS CREDIT_DEF 
*!*	    ENDIF
*!*	    
*!*	    IF !USED('DEBIT_DEF')
*!*	      USE (dbfspath +'DEBIT.dbf') In 0 Shared AGAIN ALIAS DEBIT_DEF 
*!*	    ENDIF
    
    lcOnAcGLAc = ''
    =SEEK('M'+lcAccount,'Customer_Def','Customer')
    lcCreditReason = ''
    lcDebitReason = ''
    lcLnkCod =''
    lcFactor = Customer_Def.cFacCode
    IF !EMPTY(lcFactor)
      =SEEK(lcFactor ,'FACTOR_Def','FACTOR')
      lcLnkCod = FACTOR_Def.Link_code 
    ELSE
      lcLnkCod = Customer_Def.Link_Code
    ENDIF
    lcCusLnk =  lcLnkCod 
    lcOnAcGLAc = ''
    IF SEEK("M" + lcAccount, "Customer")
      IF SEEK(lcCusLnk+"001","GL_LINK_DEF")
        lcOnAcGLAc = GL_LINK_DEF.GLAcnt
      ENDIF
    ENDIF  
    
    =SEEK('D'+"CCREDITCOD",'Codes_DEF','CCODE_NO')
    lcCreditReason = Codes_DEF.CCODE_NO
    =SEEK('D'+"TRANCODE",'Codes_DEF','CCODE_NO')
    lcDebitReason =  Codes_DEF.CCODE_NO
    
    use in 'Codes_DEF'
    use in 'Customer_Def'
    use in 'GL_LINK_DEF'
    use in 'FACTOR_Def'
    
    SELECT 'RemitCHB'
    LOCATE
    SCAN
      *lcTranNo =''
*!*        IF RemitCHB.APPRAMT > 0 && Credit on Account
*!*          lcTranNo = gfsequence("CREDIT", companyid, "", "", "TRAN")
*!*          INSERT INTO CREDIT_DEF (Account,Store,tranType,ccreditcod ,tran,TranDate,DPOSTDATE,credt_date,Desc,Reference,Amount,batch,;
*!*          History,cfaccode,ccurrcode,ncurrunit,nexrate,cadjacct,carglacc,cadd_user) values;
*!*          (lcAccount ,'','6',lcCreditReason ,lcTranNo ,RemitCHB.trandate,CTOD(""),RemitCHB.trandate,'',RemitCHB.crdrno,-1* RemitCHB.APPRAMT,;
*!*          '','',lcFactor ,'USD',1,1,'',lcOnAcGLAc ,'Aria820')
*!*        ELSE
*!*          lcTranNo = gfsequence("DEBIT", companyid, "", "", "TRAN")
*!*          INSERT INTO DEBIT_DEF (Account,Store,tranType,trancode,tran,TranDate,DPOSTDATE,chgbk_date,Desc,Reference,Amount,batch,;
*!*          History,cfaccode,ccurrcode,ncurrunit,nexrate,cadjacct,carglacc,cadd_user) values;
*!*          (lcAccount ,'','3',lcDebitReason ,lcTranNo ,RemitCHB.trandate,CTOD(""),RemitCHB.trandate,'',RemitCHB.crdrno, abs(RemitCHB.APPRAMT),;
*!*          '','',lcFactor ,'USD',1,1,'',lcOnAcGLAc ,'Aria820')
*!*        ENDIF
      SELECT (oKeyOff.cDCOnATmp)
      lcDCOnATmp = oKeyOff.cDCOnATmp
      APPEND BLANK 
      REPLACE Account    WITH lcAccount ,;
          Tran       WITH 'T'+SUBSTR(SYS(3),4,5) ,;
          TranDate   WITH ldPaymntDate ,;
          Amount     WITH 0                      ,;
          cCurrCode  WITH 'USD'              ,;
          cFacCode   WITH lcFactor 				 ,;
          nExRate    WITH 1               ,;
          nCurrUnit  WITH 1             ,;
          cTranDesc  WITH 'N/A' 
      REPLACE &lcDCOnATmp..TranType   WITH IIF(RemitCHB.APPRAMT > 0,'6','3'),;
              &lcDCOnATmp..cReason    WITH IIF(RemitCHB.APPRAMT > 0,lcCreditReason ,lcDebitReason ),;
              &lcDCOnATmp..Desc       WITH ''               ,;
              &lcDCOnATmp..ChgBk_Date WITH IIF(TranType="3", RemitCHB.trandate, CTOD('')) ,;
              &lcDCOnATmp..Credt_Date WITH IIF(TranType="6",  RemitCHB.trandate, CTOD('')) ,;
              &lcDCOnATmp..cFacCode   WITH lcFactor ,;
              &lcDCOnATmp..Reference  WITH RemitCHB.crdrno,;
              &lcDCOnATmp..Amount WITH IIF(TranType="6", -1 * RemitCHB.APPRAMT ,ABS(RemitCHB.APPRAMT))
              
*!*	      Select TmpStr
*!*	      If Eof()
*!*	        Append Blank
*!*	      Endif
*!*	      Replace mStrRep With mStrRep + IIF(RemitCHB.APPRAMT > 0,'Credit on Account# ','Chargeback# ')+RemitCHB.crdrno + " "+Chr(13)+Chr(10)
      INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Paid_Amount) VALUES (IIF(RemitCHB.APPRAMT > 0,'Credit on Account# ','Chargeback# ')+RemitCHB.crdrno,RemitCHB.crdrno,RemitCHB.APPRAMT)
    ENDSCAN   
  ENDIF    
  *Mariam
  
  Select EBREMITT
  =Seek(lcAccount)
  *Mariam
  *Select Distinct crdrno From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode Order By crdrno Into Cursor 'RemitsInv'
  *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
  *Select Distinct crdrno From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode AND TranType ='I' Order By crdrno Into Cursor 'RemitsInv'
  Select Distinct crdrno, .T. as 'Include' From EBREMITT Where ACCOUNT+Reference = lcAccount And cfileCode =lcFileCode AND TranType ='I' and '18318' $ crdrno Order By crdrno Into Cursor 'RemitsInv' READWRITE 
  *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
  *Mariam
  =Strtofile("32","X:\aria4xp\LogEDI.txt",1)
  Select 'RemitsInv'
  Locate
  SCAN
    lnResult=oAriaApplication.remotecompanydata.SqlRun("Select * from SMART_INVOICE_HEADER Where SMART_INVOICE_NO ='"+allt(RemitsInv.crdrno)+"'",'SMRTSHPHDR' , ;
    "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
    *! C201850,1 MMT 08/15/2016 Change 820 program to Checked if payment record has no corresponding smart invoice[T20151014.0017][Start]
    *If lnResult<>1     
    If lnResult=1 
    *! C201850,1 MMT 08/15/2016 Change 820 program to Checked if payment record has no corresponding smart invoice[T20151014.0017][End]    
      if recc('SMART_INVOICE_HEADER') = 0
*!*	        Select TmpStr
*!*	        If Eof()
*!*	          Append Blank
*!*	        Endif
*!*	        Replace mStrRep With mStrRep + 'Smart invoice# '+RemitsInv.crdrno + "  not found for payment"+Chr(13)+Chr(10)
        *XX1
        *INSERT INTO 'TmpExcel' (Issue) VALUES ('Smart invoice# '+RemitsInv.crdrno + " not found for payment")
        Select EBREMITT
        =Seek(lcAccount)
        Locate Rest While ACCOUNT+Reference = lcAccount FOR cfileCode =lcFileCode AND Allt(crdrno)  = RemitsInv.crdrno
        INSERT INTO 'TmpExcel' (Issue,Paid_Amount,variance) VALUES ('Smart invoice# '+RemitsInv.crdrno + " not found for payment",EBREMITT.appramt,-1 * EBREMITT.appramt)
        *XX1
        *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
        REPLACE Include WITH .F. IN 'RemitsInv'
        *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
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
      *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][Start]
      *Scan 
      SCAN FOR SHIPAMT <> 0
      *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][End]
*!*	        Select TmpStr
*!*	        If Eof()
*!*	          Append Blank
*!*	        Endif
*!*	        Replace mStrRep With mStrRep + 'Smart invoice# '+SMRTSHPHDR.SMART_INVOICE_NO + " not paid"+Chr(13)+Chr(10)
        *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][Start]
        *XX1
        *INSERT INTO 'TmpExcel' (Issue,Invoice_Amount) VALUES ('Old smart invoice# '+SMRTSHPHDR.SMART_INVOICE_NO + " has no Consolidated invoice.",SMRTSHPHDR.SHIPAMT)
        INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount) VALUES ('Previous smart invoice# '+SMRTSHPHDR.SMART_INVOICE_NO + " is not paid",SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT)
        *XX1
        *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][End]
      ENDSCAN 
      *MMT
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
      *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][Start]
      *If !Seek(SMRTSHPHDR.SMART_INVOICE_NO ,'RemitsInv')
      If !Seek(SMRTSHPHDR.SMART_INVOICE_NO ,'RemitsInv') AND SMRTSHPHDR.SHIPAMT <> 0
      *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][End]
*!*	        Select TmpStr
*!*	        If Eof()
*!*	          Append Blank
*!*	        Endif
*!*	        Replace mStrRep With mStrRep +'Smart Invoices# '+SMRTSHPHDR.SMART_INVOICE_NO+" is missing from Remittance Advices."+Chr(13)+Chr(10)
        *MMT
        *XX1
        *INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,) VALUES ('Invoice is missing from Remittance Advices',SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT)
        INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,variance) VALUES ('Invoice is missing from Remittance Advices',SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT,SMRTSHPHDR.SHIPAMT)
        *XX1
        *MMT
      ELSE
*        SET STEP ON 
        Select EBREMITT
        =Seek(lcAccount)
        Locate Rest While ACCOUNT+Reference = lcAccount FOR cfileCode =lcFileCode AND Allt(crdrno)  = SMRTSHPHDR.SMART_INVOICE_NO
        *MT
        *If SMRTSHPHDR.SHIPAMT <>  EBREMITT.appramt
        *xx1
        *If (SMRTSHPHDR.SHIPAMT <>  EBREMITT.appramt) AND ABS(SMRTSHPHDR.SHIPAMT - EBREMITT.appramt) > 0.05
        If (SMRTSHPHDR.SHIPAMT <>  EBREMITT.appramt)
        *xx1
        *MT
*!*	          Select TmpStr
*!*	          If Eof()
*!*	            Append Blank
*!*	          Endif
*!*	          Replace mStrRep With mStrRep +'Smart Invoices# '+SMRTSHPHDR.SMART_INVOICE_NO+" total amount is not matching the Remittance Advice amount."+Chr(13)+Chr(10)
          *MMT
          *XX1
*!*            INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount) VALUES;
*!*            ('Invoice total amount is not matching the Remittance Advice amount',SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT,EBREMITT.appramt)
          INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
          ('Invoice total amount is not matching the Remittance Advice amount',SMRTSHPHDR.SMART_INVOICE_NO,SMRTSHPHDR.SHIPAMT,EBREMITT.appramt,SMRTSHPHDR.SHIPAMT-EBREMITT.appramt)
          *XX1
          *MMT
          *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
          REPLACE Include WITH .F. IN 'RemitsInv'
          *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]

        Endif
      Endif
    Endscan
  ENDIF
  *XX1
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
  *XX1
  
  
  =Strtofile("34","X:\aria4xp\LogEDI.txt",1)
  *MAriam
*!*	  Select TmpStr
*!*	  Locate
*!*	  If !Eof()
*!*	    =Messagebox("There are some descrepencies between the Remittance Advice and Smart invoices. Please check the descrepencies log." ,0,_Screen.Caption)
*!*	    Select TmpStr
*!*	    Do Form (oAriaApplication.Screenhome + 'SM\SMSTRREP')
*!*	    *MT
*!*	    lcMsg = "Do you want to Export descrepencies report to Excel"
*!*	    lnMsgRet = Messagebox(lcMsg ,1,_Screen.Caption)
*!*	    If lnMsgRet = 1
*!*	      lcExcelFileP = GETFILE("XLS")
*!*	      IF !EMPTY(lcExcelFileP)
*!*	        SELECT 'TmpExcel'
*!*	        EXPORT TO (lcExcelFileP) TYPE XLS
*!*	      ENDIF
*!*	    ENDIF
*!*	    *MT
*!*	    *MAriam
*!*	    *Return .F.
*!*	    *MAriam
*!*	  Endif
  *Mariam
*!*	  lcInvHdr = gfTempName()
*!*	  lcConsInvH = gfTempName()
*!*	  lcConsInvD = gfTempName()
*!*	  lcInvLine = gfTempName()
*!*	  llUseTradeDisc = gfGetMemVar('M_TRDDISCL',oAriaApplication.activecompanyid)
*!*	  lcCostingMethod = gfGetMemVar('M_COST_METH',oAriaApplication.activecompanyid)
*!*	  lfCreateInvTemp()

  lnResult=oAriaApplication.remotecompanydata.SqlRun("Select *,'      ' as CDIVISION,'      ' as SEASON "+;
    "from SMART_INVOICE_LINES Where SMART_INVOICE_NO >= '"+lcFirstInv+;
    "' AND  SMART_INVOICE_NO <= '"+lcLastInv +"' AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLIN' ,;
    "SMART_INVOICE_LINES",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
  =Strtofile("35","X:\aria4xp\LogEDI.txt",1)
  If lnResult<>1
    oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
    Wait Window 'SMART_INVOICE_LINES '+'.SqlRun()'
    =Strtofile("36","X:\aria4xp\LogEDI.txt",1)
    Return .F.
  ELSE
    *XX1
    *Check if there is discrapancies and save file under EDI ARCHIVE Folder
    *X:\ARIA3EDI\EDI\INBOX\ARCHIVE
    *lcEDIARCHIVEFolder 
*!*	    Select SMRTSHPLIN
*!*	    LOCATE
*!*	    Wait wind "Calculating consolidated invoice total amount" nowait 
*!*	    SUM TotQty*Price TO lnConsAmt FOR !EMPTY(Order) AND (EMPTY(cTrCode) or isnull(cTrCode))
*!*	    
*!*	    LOCATE
*!*	    Wait wind "Calculating direct invoices total amount" nowait 
*!*	    SUM TotQty*Price TO lnDirectAmt FOR (EMPTY(ALLTRIM(Order))  or isnull(Order)) AND (EMPTY(cTrCode)  or isnull(cTrCode)) AND TotQty > 0
*!*	    
*!*	    LOCATE
*!*	    Wait wind "Calculating returns total amount" nowait 
*!*	    SUM TotQty*Price TO lnReturnAmt FOR (EMPTY(ALLTRIM(Order)) or isnull(Order)) AND (EMPTY(cTrCode)  or isnull(cTrCode)) AND TotQty < 0    
*!*	    
*!*	    INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
*!*	    ("Total - Consolidated Invoice","",lnConsAmt,0,lnConsAmt)
*!*	    INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
*!*	    ("Total - Direct Invoices","",lnDirectAmt,0,lnDirectAmt)
*!*	    INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Invoice_Amount,Paid_Amount,variance) VALUES;
*!*	    ("Total - Returns","",lnReturnAmt,0,lnReturnAmt)
    
    *EXPORT TO (lcEDIARCHIVEFolder+lcFileName) TYPE XLS
        *lcEDIARCHIVEFolder 
    *XX1
    
    Select SMRTSHPLIN
    Locate
    If !Eof()
      lcInvNo = lfCreateConsLines()
      =Strtofile("37","X:\aria4xp\LogEDI.txt",1)
      
      
      
      *MAriam
*!*	  Select TmpStr
*!*	  Locate
*!*	  If !Eof()
    if RECCOUNT('TmpExcel') > 0
    *MT
*!*	    IF !USED('CREDIT')
*!*	      USE (dbfspath +'CREDIT.dbf') In 0 Shared AGAIN Order Credit 
*!*	    ENDIF
*!*	    
*!*	    IF !USED('DEBIT')
*!*	      USE (dbfspath +'DEBIT.dbf') In 0 Shared AGAIN Order Debit
*!*	    ENDIF
*!*	    Sele TmpExcel
*!*	    LocaTE 
*!*	    SCAN for !Empty(SMART_INVOICE_NO) and ('Credit on Account#' $ ISSUE OR 'Chargeback#' $ ISSUE)
*!*	      Select Debit 
*!*	      =Seek(lcAccount)
*!*	      LOcate rest while ACCOUNT+TRAN+CINSTALNO+DTOS(TRANDATE) = lcAccount for ALLT(Reference) = ALLT(TmpExcel.SMART_INVOICE_NO) and tranType ='3'
*!*	      if Found()
*!*	        repl ISSUE with STRTran(ISSUE,ALLT(SMART_INVOICE_NO),Debit.Tran) in TmpExcel
*!*	      else
*!*	        Select Credit 
*!*	        =Seek(lcAccount)
*!*	        LOcate rest while ACCOUNT+TRAN+DTOS(TRANDATE) = lcAccount for ALLT(Reference) = ALLT(TmpExcel.SMART_INVOICE_NO)  and tranType ='6'
*!*	        if Found()
*!*	          repl ISSUE with STRTran(ISSUE,ALLT(SMART_INVOICE_NO),Credit.Tran) in TmpExcel
*!*	        ENDIF
*!*	      ENDIF
*!*	    ENDSCAN
    *MT
    *XX1
    *=Messagebox("There are some descrepencies between the Remittance Advice and Smart invoices. Please check the descrepencies log "+ALLTRIM((lcEDIARCHIVEFolder+lcFileName)) ,0,_Screen.Caption)
*!*	    IF llIssueFound 
*!*	      =Messagebox("There are some descrepencies between the Remittance Advice and Smart invoices. Please check the descrepencies log "+ALLTRIM((lcEDIARCHIVEFolder+lcFileName)) ,0,_Screen.Caption)
*!*	    ELSE
*!*	      =Messagebox("Please check the update log "+ALLTRIM((lcEDIARCHIVEFolder+lcFileName)) ,0,_Screen.Caption)
*!*	    ENDIF  
    *XX1
*!*	    Select TmpStr
*!*	    Do Form (oAriaApplication.Screenhome + 'SM\SMSTRREP')
    *MT
    *XX1
*!*      lcMsg = "Do you want to Export descrepencies report to Excel"
*!*      lnMsgRet = Messagebox(lcMsg ,1,_Screen.Caption)
*!*      If lnMsgRet = 1
*!*        lcExcelFileP = GETFILE("XLS")
*!*        IF !EMPTY(lcExcelFileP)
*!*          SELECT 'TmpExcel'
*!*          EXPORT TO (lcExcelFileP) TYPE XLS
*!*        ENDIF
*!*      ENDIF
   ENDIF
 * XX1
  *Mariam
      Return (lcInvNo)
    Endif
  Endif
  
  
  
  
  *********************************************************************************
Function lfCreateConsLines
  Private lcKey
  Store 1 To LNEXRATE, LNCURRUNIT
  
  set step ON
  *MT2
  lnDataSessionCurrent =SET("Datasession")
  lcDefWareHouse = ''
  Use (dbfspath+'WareHous.dbf') SHARED IN 0 ALIAS 'WareH' ORDER  WAREHOUS   && CWARECODE 
  SELECT 'WareH' 
  LOCATE FOR lDefWare
  IF FOUND()
    lcDefWareHouse  =  WareH.CWARECODE 
  ENDIF
  Select SMRTSHPLIN
  LOCATE
  SCAN
    Select SMRTSHPLIN
    *MT
    *IF !SEEK(SUBSTR(SMRTSHPLIN.STORE,6),'WareH','WAREHOUS')
    IF !SEEK(SUBSTR(SMRTSHPLIN.STORE,1,6),'WareH','WAREHOUS')
    *mt
      SELECT 'WareH' 
      APPEND BLANK 
      REPLACE CWARECODE  WITH SMRTSHPLIN.STORE,;
              cDesc WITH SMRTSHPLIN.STORE
    ENDIF
  ENDSCAN
  Use (dbfspath+'Style.DBF') Shar In 0 ALIAS 'Style_Div'  again 
  SELECT SMRTSHPLIN
  CURSORSETPROP("Buffering" ,3)
  SCAN 
     =SEEK(SMRTSHPLIN.Style,'Style_Div','STYLE')
     REPLACE CDIVISION WITH Style_Div.CDIVISION,;
             SEASON    WITH Style_Div.SEASON
  ENDSCAN
  SELECT SMRTSHPLIN
  CURSORSETPROP("Buffering" ,1) 
  * Checking Missing stores 
  Use (dbfspath +'Customer.DBF') In 0 Shared Order Customer ALIAS 'CustStore'  again 
  Select SMRTSHPLIN
  LOCATE
  SCAN
    IF !SEEK('S'+lcAccount+SMRTSHPLIN.STORE,'CustStore' ,'Customer')
      lfAddStore(lcAccount,SMRTSHPLIN.STORE)
    ENDIF
  ENDSCAN 

  * Check Credit memo Lines 
  ON Error MESSAGEBOX(MESSAGE())
  Select SMRTSHPLIN
  LOCATE FOR totqty < 0
  IF FOUND()
*    SET STEP ON 
    lfCreateCreditMemo(lcAccount)
  ENDIF
  SET DATASESSION TO (lnDataSessionCurrent)
  * Check Lines with Qty > 0 and has no linked orders
  Select SMRTSHPLIN
  LOCATE FOR totqty > 0 AND EMPTY(Order)
  IF FOUND()
    lfCreateDirectInvoice(lcAccount)
  ENDIF
  
  SET DATASESSION TO (lnDataSessionCurrent)
    *MMMT
  if !USED('INVHDR_X')
    Use (dbfspath +'INVHDR.DBF') In 0 Shared  again  Order  INVHDRA ALIAS 'INVHDR_X'
  ENDIF
  Select 'INVHDR_X'
  =Seek(lcAccount)
  Select Invoice from INVHDR_X Where Account+Invoice =lcAccount And Consol ='Y' and allt(Note2) = lcFileCode and Status <>'V' into Array laInvCrt
  if _TALLY > 0
  
    ****sss 
    If !Empty(Alltrim(laInvCrt[1]))
      SEleCT  'RemitsInv'
      LOCATE
      *XX1
      *SCAN FOR RemitsInv.Include 
      *SCAN
      *XX1
        *lcINVSMARTINV = allt(RemitsInv.crdrno)
        *XX1
*!*	        lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInvCrt[1]+;
*!*	        "', Status ='C' Where SMART_INVOICE_NO = '"+lcINVSMARTINV+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
*!*	        "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
        lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInvCrt[1]+;
        "', Status ='C' Where SMART_INVOICE_NO >= '"+lcFirstInv+;
        "' AND  SMART_INVOICE_NO <= '"+lcLastInv +"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
        "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
        *XX1
        If lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
          loop
        ELSE
         *XX1
*!*	         lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInvCrt[1]+;
*!*	                  "' Where SMART_INVOICE_NO = '"+lcINVSMARTINV+"' and [Order]<>''  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
*!*	                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
         lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInvCrt[1]+;
                  "' Where SMART_INVOICE_NO >= '"+lcFirstInv+;
        "' AND  SMART_INVOICE_NO <= '"+lcLastInv +"' and [Order]<>''  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
         *XX1        
          If lnResult<>1
            oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
            Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
            loop
          Endif
        Endif
      *ENDSCAN
    ENDIF
    Return laInvCrt[1]
  ENDIF
  *MMMT

  
  
  
  lcInvHdr = gfTempName()
  lcConsInvH = gfTempName()
  lcConsInvD = gfTempName()
  lcInvLine = gfTempName()
  llUseTradeDisc = gfGetMemVar('M_TRDDISCL',oAriaApplication.activecompanyid)
  lcCostingMethod = gfGetMemVar('M_COST_METH',oAriaApplication.activecompanyid)
  lfCreateInvTemp()
  *MMT 2016/06/07 fix bug it didn't open COdes in gfRltFld using again [begin]
    IF !USED("CODES")
      USE (dbfspath +'CODES.dbf') IN 0  Shared AGAIN Alias CODES 
    ENDIF
  *MMT 2016/06/07 fix bug it didn't open COdes in gfRltFld using again [end]

  Wait window 'Create Consolidated Invoice...' nowait 
  *MT2
  ****Counter
  Select SMRTSHPLIN
  *XX1
  *Count to lnTotCons FOR  totqty > 0 AND !EMPTY(Order) and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include 
  Count to lnTotCons FOR  totqty > 0 AND !EMPTY(Order) &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
  *XX1
  lnCountLine = 0
  *****
  Select SMRTSHPLIN
  Locate
  =Strtofile("38","X:\aria4xp\LogEDI.txt",1)
  *MT
*  SCAN FOR  totqty > 0 AND !EMPTY(Order)
  *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
  *SCAN FOR  totqty > 0 AND !EMPTY(Order) and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') 
  *MMT
  *SCAN FOR  totqty > 0 AND !EMPTY(Order) and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include 
  *  *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][Start]
  lnNoIncVar = 0
    *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][End]
  *XX1
  *SCAN FOR  totqty > 0 AND !EMPTY(Order) and (IIF(Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv','RemitsInv'),RemitsInv.Include,.F.)) 
  SCAN FOR  totqty > 0 AND !EMPTY(Order) &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv','RemitsInv')
  *XX1
  *MMT 
  *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
    *MT
    lnCountLine = lnCountLine + 1
    lnPerCent = (lnCountLine / lnTotCons)* 100
    *MT
    wait window 'Adding Smart invoice# '+SMRTSHPLIN.SMART_INVOICE_NO +" to the consolidated invoice...."+ ALLT(Str(lnPerCent,3))+"%" nowait
  *MT
    Scatter Memo Memvar
    m.Dyelot =''
    =Seek(m.Style, 'STYLE', 'STYLE')
    =Seek('O'+m.Order,'Ordhdr','Ordhdr')
    =Seek('S'+m.ACCOUNT+m.Store,'Customer','Customer')
    =Seek('O'+m.Order+Str(m.nOrdlineNo,6),'Ordline','Ordline')
    m.CWARECODE = OrdLine.CWARECODE
    m.ALTSTYLE =''
    m.Comm1 = OrdLine.Comm1
    m.Comm2 = OrdLine.Comm2
    m.DESC1 = OrdLine.DESC1
    LCDIST_CTR = ''
    LCSTORE = m.Store
    LCSALEREP1 = ORDHDR.REP1
    LNCOMM1    = ORDHDR.Comm1
    LCSALEREP2 = ORDHDR.REP2
    LNCOMM2    = ORDHDR.Comm2
    If ORDHDR.Multi='Y' And Empty(ORDHDR.REP1) And Empty(ORDHDR.REP2)
      LCSALEREP1 = CUSTOMER.SALESREP
      LNCOMM1    = CUSTOMER.Comm
      LCSALEREP2 = CUSTOMER.REP2
      LNCOMM2    = CUSTOMER.Comm2
    Endif
    =Strtofile("38","X:\aria4xp\LogEDI.txt",1)
    If !Empty(OrdLine.ALTSTYLE)
      m.Style = OrdLine.ALTSTYLE
      m.ALTSTYLE = OrdLine.Style
      =Seek(OrdLine.ALTSTYLE,'STYLE')
      m.DESC1  = Style.DESC1
    Endif
    =Iif(Empty(m.Store),Seek('M'+m.ACCOUNT,'Customer'),Seek('S'+m.ACCOUNT+m.Store,'Customer'))
    LCDIST_CTR = CUSTOMER.DIST_CTR
    =Seek(m.Style+m.CWARECODE+m.Dyelot,'StyDye')
    m.COST = STYDYE.AVE_COST
    =Seek(m.Style,'Style')
    m.GROS_PRICE = m.PRICE
    If llUseTradeDisc
      m.TRD_PRICE = Round((m.GROS_PRICE*(100-m.TRDE_DISC)/100),2)
      m.OTRDDISC = m.TRDE_DISC
    Endif
    =Strtofile("39","X:\aria4xp\LogEDI.txt",1)
    m.LTAXABLE = Style.LTAXABLE
    *-- Compute merchandise tax amount for england
    Store 0 To LNTAXQTY,LNTAXRATE
    
    *  *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][Start]
    *Insert Into (lcInvLine);
      (Order,ACCOUNT,Lineno,Store,PIKTKT,Style,ALTSTYLE,Dyelot,NOTE_MEM,Comm1,Comm2,BOOK1,BOOK2,;
      BOOK3,BOOK4,BOOK5,BOOK6,BOOK7,BOOK8,TOTBOOK,Flag,PIK1,PIK2,PIK3,PIK4,PIK5,;
      PIK6,PIK7,PIK8,TOTPIK,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,;
      PRICE,PACK_ID,GROS_PRICE,DISC_PCNT,LPACKED,LBACKORD,NTAXRATE,Group,PREPAK,;
      DESC1,SEASON,PPQTY,Scale,CWARECODE,Consol,CDIVISION,cCurrCode,LTAXABLE,CDYEFLAG,CWARECODE,DIST_CTR,NORDLINE);
      VALUES (m.Order,m.ACCOUNT,m.nOrdlineNo,m.Store,'',m.Style,m.ALTSTYLE,m.Dyelot,;
      OrdLine.NOTE_MEM,m.Comm1,m.Comm2,m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,;
      M.QTY7,m.QTY8,m.TOTQTY,' ',0,0,0,0,;
      0,0,0,0,0,m.QTY1,m.QTY2,m.QTY3,m.QTY4,;
      M.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY,m.PRICE,OrdLine.PACK_ID,;
      M.GROS_PRICE,0,.F.,.T.,LNTAXRATE,OrdLine.Group,OrdLine.PREPAK,;
      M.DESC1,OrdLine.SEASON,OrdLine.PPQTY,OrdLine.Scale,OrdLine.CWARECODE,'N',ORDHDR.CDIVISION,ORDHDR.cCurrCode,m.LTAXABLE,Style.CDYE_FLG,OrdLine.CWARECODE,LCDIST_CTR,OrdLine.Lineno)


    IF !SEEK(m.Account+m.Order+m.Store+m.STYLE+STR(m.nOrdLineNo,6),lcInvLine,'ConsORDLN')   
      lnNoIncVar = lnNoIncVar + 1
      Insert Into (lcInvLine);
      (Order,ACCOUNT,Lineno,Store,PIKTKT,Style,ALTSTYLE,Dyelot,NOTE_MEM,Comm1,Comm2,BOOK1,BOOK2,;
      BOOK3,BOOK4,BOOK5,BOOK6,BOOK7,BOOK8,TOTBOOK,Flag,PIK1,PIK2,PIK3,PIK4,PIK5,;
      PIK6,PIK7,PIK8,TOTPIK,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,;
      PRICE,PACK_ID,GROS_PRICE,DISC_PCNT,LPACKED,LBACKORD,NTAXRATE,Group,PREPAK,;
      DESC1,SEASON,PPQTY,Scale,CWARECODE,Consol,CDIVISION,cCurrCode,LTAXABLE,CDYEFLAG,CWARECODE,DIST_CTR,NORDLINE);
      VALUES (m.Order,m.ACCOUNT,lnNoIncVar,m.Store,'',m.Style,m.ALTSTYLE,m.Dyelot,;
      OrdLine.NOTE_MEM,m.Comm1,m.Comm2,m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,;
      M.QTY7,m.QTY8,m.TOTQTY,' ',0,0,0,0,;
      0,0,0,0,0,m.QTY1,m.QTY2,m.QTY3,m.QTY4,;
      M.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TOTQTY,m.PRICE,OrdLine.PACK_ID,;
      M.GROS_PRICE,0,.F.,.T.,LNTAXRATE,OrdLine.Group,OrdLine.PREPAK,;
      M.DESC1,OrdLine.SEASON,OrdLine.PPQTY,OrdLine.Scale,OrdLine.CWARECODE,'N',ORDHDR.CDIVISION,ORDHDR.cCurrCode,m.LTAXABLE,Style.CDYE_FLG,OrdLine.CWARECODE,LCDIST_CTR,OrdLine.Lineno)
    ELSE
      FOR lnCnr = 1 TO 8
        lcCnr = STR(lnCnr ,1)
        REPLACE Qty&lcCnr. WITH Qty&lcCnr. + m.Qty&lcCnr. ,;
                BOOK&lcCnr. WITH BOOK&lcCnr. + m.Qty&lcCnr. IN (lcInvLine)
      ENDFOR
      REPLACE GROS_PRICE WITH ((TotQty * GROS_PRICE + m.TotQty * m.GROS_PRICE)/(TotQty + m.TotQty)),;
              PRICE    WITH   ((TotQty * PRICE + m.TotQty * m.PRICE)/(TotQty + m.TotQty)) IN (lcInvLine)
      REPLACE TotQty WITH TotQty + m.TotQty,;
              TOTBOOK WITH TOTBOOK + m.TotQty IN (lcInvLine)
    ENDIF  
    *  *! B611179,1 MMT 08/10/2016 Change 820 program to stop repeating CONSINVL lines for same Account - Store - Order - Order line#[T20151014.0017][End]

    Replace OLDDYELOT With m.Dyelot In (lcInvLine)
    =Strtofile("40","X:\aria4xp\LogEDI.txt",1)
    If llUseTradeDisc
      Replace TRDE_DISC With OrdLine.TRDE_DISC,;
        OTRDDISC With OrdLine.TRDE_DISC,;
        TRD_PRICE With OrdLine.TRD_PRICE  In (lcInvLine)
    Endif
    *-- Do not ship more than on hand quantity if LIFO of FIFO costing methods is used.
    If Inlist(lcCostingMethod ,'F','L') And Seek(m.Style+m.CWARECODE+m.Dyelot,'StyDye')
      Replace QTY1 With Max(Min(QTY1,STYDYE.STK1),0) ,;
        QTY2 With Max(Min(QTY2,STYDYE.STK2),0) ,;
        QTY3 With Max(Min(QTY3,STYDYE.STK3),0) ,;
        QTY4 With Max(Min(QTY4,STYDYE.STK4),0) ,;
        QTY5 With Max(Min(QTY5,STYDYE.STK5),0) ,;
        QTY6 With Max(Min(QTY6,STYDYE.STK6),0) ,;
        QTY7 With Max(Min(QTY7,STYDYE.STK7),0) ,;
        QTY8 With Max(Min(QTY8,STYDYE.STK8),0) ,;
        TOTQTY With QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 In (lcInvLine)
    Endif
    =Strtofile("41","X:\aria4xp\LogEDI.txt",1)
    Declare LATRLTFLD[6,2]
    LATRLTFLD[1,1] = 'NTERDISCR'
    LATRLTFLD[1,2] = 'lnTerDiscR'
    LATRLTFLD[2,1] = 'EOM '
    LATRLTFLD[2,2] = 'lcTEOM'
    LATRLTFLD[3,1] = 'NTERDUED'
    LATRLTFLD[3,2] = 'lnTDaysDue'
    LATRLTFLD[4,1] = 'CODYN'
    LATRLTFLD[4,2] = 'lcTCod'
    LATRLTFLD[5,1] = 'EOMDAY'
    LATRLTFLD[5,2] = 'lnEomDay'
    LATRLTFLD[6,1] = 'LINSTALLM'
    LATRLTFLD[6,2] = 'llInstTerm'  && Use Instalment
    Store 0  To LNTERDISCR, LNTDAYSDUE, LNEOMDAY
    Store '' To LCTEOM, LCTCOD
    Store .F. To LLINSTTERM
    =gfRltFld(ORDHDR.CTERMCODE,@LATRLTFLD,'CTERMCODE')
    LCCURRCODE = Iif(Empty(ORDHDR.cCurrCode), oAriaApplication.BASECURRENCY, ORDHDR.cCurrCode)
    =Strtofile("42","X:\aria4xp\LogEDI.txt",1)
    Select (lcInvHdr)
    If !Seek(m.ACCOUNT+m.Order+m.Store)
      Append Blank
      Replace Order     With m.Order ,;
        STORE     With m.Store ,;
        PIKTKT    With '' ,;
        CUSTPO    With ORDHDR.CUSTPO,;
        DIST_CTR  With LCDIST_CTR  ,;
        ACCOUNT   With ORDHDR.ACCOUNT ,;
        CTERMCODE With ORDHDR.CTERMCODE  ,;
        SPCINST   With ORDHDR.SPCINST ,;
        LUPSINS   With (ORDHDR.CINSUR='Y') ,;
        REP1      With LCSALEREP1 ,;
        Comm1     With LNCOMM1    ,;
        REP2      With LCSALEREP2 ,;
        Comm2     With LNCOMM2    ,;
        NOTE1     With ORDHDR.NOTE1 ,;
        NOTE2     With ORDHDR.NOTE2 ,;
        LCOMPUPS  With .T. ,;
        CONSOL    With 'N' ,;
        LASTLINE  With ORDHDR.LASTLINE,;
        CCARTRCKNO With  ORDHDR.CCARTRCKNO
      *MT
*!*	      LDDUEDATE = Iif(LCTEOM <> 'Y',Date()+LNTDAYSDUE,;
*!*	        GOMONTH(Ctod(Substr(Dtoc(Date()),1,3)+'10'+;
*!*	        SUBSTR(Dtoc(Date()),6,5)),Iif(Day(Date()) > LNEOMDAY,2,1))+LNTDAYSDUE)
  LDDUEDATE = Iif(LCTEOM <> 'Y',ldPaymntDate+LNTDAYSDUE,;
        GOMONTH(Ctod(Substr(Dtoc(ldPaymntDate),1,3)+'10'+;
        SUBSTR(Dtoc(ldPaymntDate),6,5)),Iif(Day(ldPaymntDate) > LNEOMDAY,2,1))+LNTDAYSDUE)
        *MT
*!*C201814,1 05/11/2016 AEG modify creating consolidated invoice to make invoice date to be payment date [BEGIN]
*!*	      Replace  DISCPCNT   With  ORDHDR.DISC                                      ,;
*!*	        INVDATE    With Date() ,;
*!*	        SHIPDATE   With Date() , ;
*!*	        DPOSTDATE  With Date() ,;
*!*	        DUEDATE    With LDDUEDATE                                         ,;
*!*	        DEPT       With ORDHDR.DEPT                                       ,;
*!*	        CFACCODE   With ORDHDR.CFACCODE                                   ,;
*!*	        APPROVAL   With ORDHDR.APPROVAL                                   ,;
*!*	        appramt    With ORDHDR.appramt                                    ,;
*!*	        SEASON     With ORDHDR.SEASON                                     ,;
*!*	        CDIVISION  With ORDHDR.CDIVISION                                  ,;
*!*	        UPSZONE    With CUSTOMER.UPSZONE                                  ,;
*!*	        PHONE      With CUSTOMER.PHONE1                                   ,;
*!*	        CWARECODE  With ORDHDR.CWARECODE ,;
*!*	        TRDE_DISC  With LNTERDISCR                                        ,;
*!*	        TAX_RATE   With 0 ,;
*!*	        NPSTRATE   With 0 ,;
*!*	        CTAXRULE   With '' ,;
*!*	        NHSTRATE   With 0 ,;
*!*	        COD_FLAG   With Iif(LCTCOD='Y','Y','N'),;
*!*	        STATUS     With ORDHDR.Status ,;
*!*	        cCurrCode  With LCCURRCODE ,;
*!*	        nExRate    With LNEXRATE   ,;
*!*	        nCurrUnit  With LNCURRUNIT ,;
*!*	        DADD_DATE  With Date()   ,;
*!*	        CADD_TIME  With Time()     ,;
*!*	        CADD_USER  With oAriaApplication.USER_ID

      Replace  DISCPCNT   With  ORDHDR.DISC                                      ,;
        INVDATE    With ldPaymntDate ,;
        SHIPDATE   With ldPaymntDate , ;
        DPOSTDATE  With ldPaymntDate  ,;
        DUEDATE    With LDDUEDATE                                         ,;
        DEPT       With ORDHDR.DEPT                                       ,;
        CFACCODE   With ORDHDR.CFACCODE                                   ,;
        APPROVAL   With ORDHDR.APPROVAL                                   ,;
        appramt    With ORDHDR.appramt                                    ,;
        SEASON     With ORDHDR.SEASON                                     ,;
        CDIVISION  With ORDHDR.CDIVISION                                  ,;
        UPSZONE    With CUSTOMER.UPSZONE                                  ,;
        PHONE      With CUSTOMER.PHONE1                                   ,;
        CWARECODE  With ORDHDR.CWARECODE ,;
        TRDE_DISC  With LNTERDISCR                                        ,;
        TAX_RATE   With 0 ,;
        NPSTRATE   With 0 ,;
        CTAXRULE   With '' ,;
        NHSTRATE   With 0 ,;
        COD_FLAG   With Iif(LCTCOD='Y','Y','N'),;
        STATUS     With ORDHDR.Status ,;
        cCurrCode  With LCCURRCODE ,;
        nExRate    With LNEXRATE   ,;
        nCurrUnit  With LNCURRUNIT ,;
        DADD_DATE  With Date()   ,;
        CADD_TIME  With Time()     ,;
        CADD_USER  With oAriaApplication.USER_ID
        *!*C201814,1 05/11/2016 AEG modify creating consolidated invoice to make invoice date to be payment date [END]

    ENDIF

    =Strtofile("43","X:\aria4xp\LogEDI.txt",1)
    LNCARTONS  = NCARTONS + Iif(Style.QTY_CTN>0,m.TOTQTY/Style.QTY_CTN,0)
    LNALLCARTN = Iif(Ceiling(LNCARTONS)=0,1,Ceiling(LNCARTONS))
    Replace ORDERED   With ORDERED  + OrdLine.TOTBOOK ,;
      SHIP      With SHIP     + m.TOTQTY ,;
      SHIPAMT   With SHIPAMT  + m.TOTQTY*m.PRICE ,;
      DISCOUNT  With -SHIPAMT * DISCPCNT/100,;
      WEIGHT    With WEIGHT   + m.TOTQTY*Style.NSTYWEIGHT ,;
      NCARTONS  With LNCARTONS ,;
      CARTONS   With LNALLCARTN ,;
      PICKED    With 0 ,;
      SHIPVIA   With Iif(CUSTOMER.NBRKWEIGHT <> 0 And WEIGHT > CUSTOMER.NBRKWEIGHT,CUSTOMER.CALTSHPVIA,;
      IIF(Alltrim(ORDHDR.SHIPVIA)='*',CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA)),;
      NMERCHTAX With NMERCHTAX + LNTAXQTY * m.PRICE * LNTAXRATE/100 ,;
      TAX_AMT   With NMERCHTAX*(100-DISCPCNT)/100*(100-TRDE_DISC)/100  ,;
      COD_AMT   With 0 ,;
      TOTALCHG  With 0 ,;
      NTAXDUE   With NTAXDUE + Iif(m.LTAXABLE,m.TOTQTY*m.PRICE,0)

    Select (lcInvHdr)
    Replace NOLDCARTON With CARTONS,;
      NOLDWIGHT  With WEIGHT

    If llUseTradeDisc
      Replace TRDDSCAMNT With TRDDSCAMNT+Round(((m.TOTQTY*m.PRICE*m.TRDE_DISC)/100),2),;
        TRDE_DISC  With  Iif((SHIPAMT+DISCOUNT)<>0, (TRDDSCAMNT/(SHIPAMT+DISCOUNT))*100,TRDE_DISC)
    Endif
  Endscan
  =Strtofile("44","X:\aria4xp\LogEDI.txt",1)
  llconsbydc = .F.
  Select (lcInvLine)
  Set Order To CONSDIV
  Select (lcInvHdr)
  Set Order To Tag Consol
  Go Top
  SET STEP ON
  Do While !Eof()
    =Strtofile("45","X:\aria4xp\LogEDI.txt",1)
    lcAccount = ACCOUNT
    llConsDist = Seek('M'+lcAccount,'Customer') And CUSTOMER.ConByDc $ 'SY'
    llConsStore = Seek('M'+lcAccount,'Customer') And CUSTOMER.ConByDc = 'S'

    If llConsDist
      llconsbydc = .T.
    Endif
    lcKey = Consol+ACCOUNT+CDIVISION+cCurrCode+Iif(llConsDist,DIST_CTR,"")

    lnInvoices = 0
    lcOrderNo  = Order
    lcTermCode = CTERMCODE
    lcShipvia  = SHIPVIA
    If Seek('M'+lcAccount,'Customer') And CUSTOMER.Consol = 'Y'
      Scan Rest While Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR = lcKey
        lnInvoices = lnInvoices + 1
        lcOrderNo  = Iif(Order    =lcOrderNo ,lcOrderNo ,Space(6))
        lcTermCode = Iif(CTERMCODE=lcTermCode,lcTermCode,Space(6))
        lcShipvia  = Iif(SHIPVIA = lcShipvia ,lcShipvia ,'*')
      Endscan
    Endif
    =Strtofile("46","X:\aria4xp\LogEDI.txt",1)
    llConsInv = Seek('M'+lcAccount,'Customer') And CUSTOMER.Consol='Y' And lnInvoices > 1
    =Seek(lcKey)
    Local lnTax_Amt,lnTOTALCHG,lnOrdTax
    lnTax_Amt  = 0
    lnTOTALCHG = 0
    =Strtofile("47","X:\aria4xp\LogEDI.txt",1)
    SET STEP ON
    Scan Rest While Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR = lcKey For lnInvoices > 1
      Scatter Memvar
      m.SHIPVIA = lcShipvia
      Select (lcConsInvH)
      If !Seek('Y'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,""))
        Append Blank
        Replace cSelect   With '»'         ,;
          Consol    With 'Y'         ,;
          ACCOUNT   With m.ACCOUNT   ,;
          CDIVISION With m.CDIVISION ,;
          cCurrCode With m.cCurrCode ,;
          nExRate   With m.nExRate   ,;
          nCurrUnit With m.nCurrUnit ,;
          Order     With lcOrderNo   ,;
          CTERMCODE With m.CTERMCODE ,;
          SPCINST   With m.SPCINST   ,;
          SHIPVIA   With m.SHIPVIA   ,;
          LUPSINS   With m.LUPSINS   ,;
          INVDATE   With m.INVDATE   ,;
          SHIPDATE  With m.SHIPDATE  ,;
          DPOSTDATE With m.DPOSTDATE ,;
          UPSZONE   With m.UPSZONE   ,;
          PHONE     With m.PHONE     ,;
          TAX_RATE  With m.TAX_RATE  ,;
          NPSTRATE  With m.NPSTRATE  ,;
          NHSTRATE  With 0,;
          CTAXRULE  With m.CTAXRULE  ,;
          Status    With m.Status    ,;
          NOTE1     With m.NOTE1     ,;
          NOTE2     With m.NOTE2     ,;
          REP1      With m.REP1      ,;
          Comm1     With 0           ,;
          REP2      With m.REP2      ,;
          Comm2     With 0           ,;
          DEPT      With m.DEPT      ,;
          CFACCODE  With m.CFACCODE  ,;
          CWARECODE With m.CWARECODE ,;
          DUEDATE   With m.DUEDATE   ,;
          TRDE_DISC With m.TRDE_DISC ,;
          APPROVAL  With m.APPROVAL  ,;
          SEASON    With m.SEASON    ,;
          CUSTPO    With m.CUSTPO    ,;
          COD_FLAG  With m.COD_FLAG  ,;
          LCOMPUPS  With .T.         ,;
          DADD_DATE With oAriaApplication.SystemDate   ,;
          CADD_TIME With Time()      ,;
          CADD_USER With oAriaApplication.USER_ID
        Replace DIST_CTR With Iif(llConsDist,m.DIST_CTR,"")
        If llConsStore
          Replace cConStore With  m.Store
        Endif
      Else  && if the CustPo Changed then blank the CustPo (MultiPo).
        Replace CUSTPO With Iif(m.CUSTPO <> CUSTPO,'',m.CUSTPO)
      Endif
      Replace appramt   With Iif(appramt>0 .And. Evaluate(lcConsInvH+'.consol')='Y',appramt,appramt+m.appramt) ,;
        SEASON    With Iif(SEASON=m.SEASON,SEASON,'*') ,;
        SHIP      With SHIP    + m.SHIP    ,;
        ORDERED   With ORDERED + m.ORDERED ,;
        SHIPAMT   With SHIPAMT + m.SHIPAMT ,;
        PICKED    With PICKED  + m.PICKED  ,;
        DISCOUNT  With DISCOUNT - m.SHIPAMT*m.DISCPCNT/100,;
        DISCPCNT  With Iif(SHIPAMT=0,0,Abs(DISCOUNT)*100/SHIPAMT)  ,;
        WEIGHT    With WEIGHT  + m.WEIGHT  ,;
        CARTONS   With CARTONS + m.CARTONS ,;
        NMERCHTAX With NMERCHTAX + m.NMERCHTAX ,;
        COD_AMT   With COD_AMT  + m.COD_AMT  ,;
        NTAXDUE   With NTAXDUE  + m.NTAXDUE

      =Strtofile("48","X:\aria4xp\LogEDI.txt",1)
      If llUseTradeDisc
        Replace TRDDSCAMNT With TRDDSCAMNT+m.TRDDSCAMNT,;
          TRDE_DISC  With  Iif((SHIPAMT+DISCOUNT)<>0,(TRDDSCAMNT/(SHIPAMT+DISCOUNT))*100,TRDE_DISC)
        m.TRDE_DISC = TRDE_DISC
      Endif

      lnOrdTax   = m.NMERCHTAX*(100-m.DISCPCNT)/100*(100-m.TRDE_DISC)/100
      lnTax_Amt  = lnTax_Amt  + lnOrdTax
      lnTOTALCHG = lnTOTALCHG + m.SHIPAMT+lnOrdTax+m.DISCOUNT

      Select (lcInvLine)
      =Seek(m.ACCOUNT+m.Order+m.Store)
     SET STEP ON
      Scan Rest While ACCOUNT+Order+Store+PIKTKT+Str(Lineno,6) = ;
          M.ACCOUNT+m.Order+m.Store
        Scatter Memvar
        Select (lcConsInvD)
        If !Seek('Y'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+m.CWARECODE+m.Style+m.Dyelot)
          Append Blank
          Replace Consol    With 'Y'         ,;
            ACCOUNT   With m.ACCOUNT   ,;
            CDIVISION With m.CDIVISION ,;
            cCurrCode With m.cCurrCode ,;
            CWARECODE With m.CWARECODE ,;
            Style     With m.Style     ,;
            ALTSTYLE  With m.ALTSTYLE  ,;
            Order     With lcOrderNo   ,;
            Dyelot    With m.Dyelot    ,;
            SEASON    With m.SEASON    ,;
            Scale     With m.Scale     ,;
            LTAXABLE  With m.LTAXABLE  ,;
            DIST_CTR  With Iif(llConsDist,m.DIST_CTR,"")
          Replace DESC1 With m.DESC1
          Replace OLDDYELOT With m.Dyelot
        Endif
        Replace BOOK1   With BOOK1   + m.BOOK1 ,;
          BOOK2   With BOOK2   + m.BOOK2 ,;
          BOOK3   With BOOK3   + m.BOOK3 ,;
          BOOK4   With BOOK4   + m.BOOK4 ,;
          BOOK5   With BOOK5   + m.BOOK5 ,;
          BOOK6   With BOOK6   + m.BOOK6 ,;
          BOOK7   With BOOK7   + m.BOOK7 ,;
          BOOK8   With BOOK8   + m.BOOK8 ,;
          TOTBOOK With TOTBOOK + m.TOTBOOK ,;
          PIK1   With PIK1   + m.PIK1 ,;
          PIK2   With PIK2   + m.PIK2 ,;
          PIK3   With PIK3   + m.PIK3 ,;
          PIK4   With PIK4   + m.PIK4 ,;
          PIK5   With PIK5   + m.PIK5 ,;
          PIK6   With PIK6   + m.PIK6 ,;
          PIK7   With PIK7   + m.PIK7 ,;
          PIK8   With PIK8   + m.PIK8 ,;
          TOTPIK With TOTPIK + m.TOTPIK ,;
          QTY1   With QTY1   + m.QTY1 ,;
          QTY2   With QTY2   + m.QTY2 ,;
          QTY3   With QTY3   + m.QTY3 ,;
          QTY4   With QTY4   + m.QTY4 ,;
          QTY5   With QTY5   + m.QTY5 ,;
          QTY6   With QTY6   + m.QTY6 ,;
          QTY7   With QTY7   + m.QTY7 ,;
          QTY8   With QTY8   + m.QTY8 ,;
          TOTQTY With TOTQTY + m.TOTQTY,;
          nNetAmnt   With nNetAmnt +m.TOTQTY*m.PRICE ,;
          nGrosAmnt  With nGrosAmnt+m.TOTQTY*m.GROS_PRICE ,;
          PRICE      With Iif(TOTQTY=0,0,nNetAmnt/TOTQTY)  ,;
          GROS_PRICE With Iif(TOTQTY=0,0,nGrosAmnt/TOTQTY) ,;
          DISC_PCNT  With Iif(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)*100/nGrosAmnt)
        *-- Update Packs fields in Temp consalidated invoice line
        lnComAmt1 = ((nNetAmnt - m.TOTQTY*m.PRICE) * Comm1 /100)+(m.TOTQTY*m.PRICE* m.Comm1 / 100)
        lnComAmt2 = ((nNetAmnt - m.TOTQTY*m.PRICE) * Comm2 /100)+(m.TOTQTY*m.PRICE* m.Comm2 / 100)
        Replace Comm1 With Iif(nNetAmnt <> 0,lnComAmt1 /nNetAmnt *100,0),;
          Comm2 With Iif(nNetAmnt <> 0,lnComAmt2 /nNetAmnt *100,0)
        If llUseTradeDisc
          Replace nTrdNetAmt With nTrdNetAmt +m.TOTQTY*m.TRD_PRICE ,;
            TRD_PRICE  With Iif(TOTQTY=0,0,nTrdNetAmt/TOTQTY)  ,;
            TRDE_DISC  With Iif(nGrosAmnt=0,0,(nGrosAmnt-nTrdNetAmt)*100/nGrosAmnt)
        Endif
        lcDetFile = lcConsInvD
      Endscan
    Endscan
    =Strtofile("49","X:\aria4xp\LogEDI.txt",1)
    Local lnSlct
    lnSlct = Select(0)
    Select (lcConsInvH)
    Replace TAX_AMT   With lnTax_Amt  ;
      TOTALCHG  With lnTOTALCHG
    Select (lnSlct)
  Enddo
  =Strtofile("50","X:\aria4xp\LogEDI.txt",1)
  Select (lcConsInvH)
  Scan
    *-- Store consolidated invoice line#
    llConsDist = Seek('M'+ACCOUNT,'Customer') And CUSTOMER.ConByDc $ 'SY'
    =Seek(Consol+ACCOUNT+CDIVISION+cCurrCode,lcConsInvD)
    Select (lcConsInvD)
    lnLineNo = 0
    Scan For Consol+ACCOUNT+CDIVISION+cCurrCode+CWARECODE+Style+Dyelot+DIST_CTR = ;
        &lcConsInvH..Consol+&lcConsInvH..ACCOUNT+&lcConsInvH..CDIVISION+&lcConsInvH..cCurrCode ;
        AND DIST_CTR = &lcConsInvH..DIST_CTR
      Scatter Memvar
      lnLineNo = lnLineNo + 1
      m.LineNo = lnLineNo
      m.OLDDYELOT = m.Dyelot
      Insert Into (lcInvLine) From Memvar
    Endscan
    Select (lcConsInvH)
    Scatter Memvar
    m.LASTLINE = lnLineNo
    Select (lcInvHdr)
    =Seek('N'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,""))
    If llUseTradeDisc
      Replace Rest Flag      With Iif(llConsDist,'C',Iif(Empty(m.Order),'A','O')) ,;
        CTERMCODE With m.CTERMCODE ,;
        COD_FLAG  With m.COD_FLAG  ,;
        DUEDATE   With m.DUEDATE   ,;
        SHIPDATE  With m.SHIPDATE  ;
        WHILE Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR=;
        'N'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,"")
    Else
      Replace Rest Flag      With Iif(llConsDist,'C',Iif(Empty(m.Order),'A','O')) ,;
        CTERMCODE With m.CTERMCODE ,;
        TRDE_DISC With m.TRDE_DISC ,;
        COD_FLAG  With m.COD_FLAG  ,;
        DUEDATE   With m.DUEDATE   ,;
        SHIPDATE  With m.SHIPDATE  ;
        WHILE Consol+ACCOUNT+CDIVISION+cCurrCode+DIST_CTR=;
        'N'+m.ACCOUNT+m.CDIVISION+m.cCurrCode+Iif(llConsDist,m.DIST_CTR,"")
    Endif
    m.NOLDCARTON = m.CARTONS
    m.NOLDWIGHT  = m.WEIGHT
    *MT
    m.Note2 = lcFileCode
    *MT
    Insert Into (lcInvHdr) From Memvar

  Endscan
  =Strtofile("51","X:\aria4xp\LogEDI.txt",1)
  Select (lcInvHdr)
  If llconsbydc
    Set Order To Tag ConsDist
  Else
    Set Order To Tag (lcInvHdr)
  Endif
  Go Top
*!*	  _SCreen.Visible =.t.
*!*	SET STEP ON 
  loFormSet = Createobject('Custom')
  loFormSet.AddProperty ('laEvntTrig[1]','')
  Declare laInv[1]
  Store '' To laInv
  lcGlSession  = gfSequence('GLSESSION')
  =Strtofile("52","X:\aria4xp\LogEDI.txt",1)
  Do gpSaveInv In (oAriaApplication.applicationhome+"AR"+'\ARINV.FXP') With ;
    lcInvHdr,lcInvLine,'','','',;
    '','','',;
    lcGlSession  ,'laInv',.F.,.F.,loFormSet
  If !Empty(Alltrim(laInv[1]))
    *MT
    SEleCT  'RemitsInv'
    LOCATE
    *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
    *SCAN
    *XX1
    *SCAN FOR RemitsInv.Include 
    SCAN
    *XX1
    *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
      lcINVSMARTINV = allt(RemitsInv.crdrno)
      lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInv[1]+;
      "', Status ='C' Where SMART_INVOICE_NO = '"+lcINVSMARTINV+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
      "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
      If lnResult<>1
        oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
        Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
        loop
      ELSE
       lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInv[1]+;
                  "' Where SMART_INVOICE_NO = '"+lcINVSMARTINV+"' and [Order]<>''  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
        If lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
          loop
        Endif
      Endif
    ENDSCAN
    *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][Start]
    IF ALEN(laInv,1) > 1
      *MMT
      *FOR lnd = 1 TO ALEN(laInv,1) 
      FOR lnd = 2 TO ALEN(laInv,1) 
      *MMT
        INSERT INTO  'CRDIRECT' VALUES (laInv[lnd],0,'I', '', '')
      ENDFOR
    ENDIF
    *! B611150,2 MMT 06/05/2016 Issue#8 - 820 program looks for zero amount invoice in ebremitt table [T20151014.0017 - Issue#8][End]
    *MT
    if used(lcInvHdr)
      USE IN (lcInvHdr)
    ENDIF
    if used(lcConsInvH)
      USE IN (lcConsInvH)
    ENDIF
    if USED(lcConsInvD)
      USE IN (lcConsInvD)
    ENDIF
    if USED(lcInvLine)
      USE IN (lcInvLine)
    ENDIF 
    *MT
    Return laInv[1]
    *MT
*!*	    lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_HEADER Set INVOICE ='"+laInv[1]+;
*!*	      "', Status ='C' Where SMART_INVOICE_NO >= '"+lcFirstInv+"' AND SMART_INVOICE_NO <='"+lcLastInv+"' AND  Invoice ='' AND Status !='C'",'SMRTSHPHDR' , ;
*!*	      "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
*!*	    If lnResult<>1
*!*	      oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*	      Wait Window 'SMART_INVOICE_HEADER'+'.SqlRun()'
*!*	      Return .F.
*!*	    ELSE
*!*	       lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInv[1]+;
*!*	                 "' Where SMART_INVOICE_NO >= '"+lcFirstInv+"' AND SMART_INVOICE_NO <='"+lcLastInv+"' and [Order]<>'' and ctrCode = '' ",'SMRTSHPLINUP' , ;
*!*	                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
*!*	        If lnResult<>1
*!*	          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
*!*	          Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
*!*	          Return .F.
*!*	        Endif
*!*	      Return laInv[1]
*!*	    Endif
  Else
    Return .F.
  Endif
  ****************************************************************************************
Function lfKeyOff
  Lparameters lcAccountNO,lcInvoice,lcFileCode
*set step oN 
  lnChoice = 1  
  If !Used('SYUSTATC')
    Use (aria27sysfilespath +"SYUSTATC.DBF") Shared In 0 Order CUSER_ID
    Select SYUSTATC
    CursorSetProp("Buffering" ,3)
  Endif
  If !Used('Credit')
    Use (dbfspath +'Credit.dbf') In 0 Shared Orde Credit &&ACCOUNT+TRAN+DTOS(TRANDATE)
  Endif
  if !used('EBPAYMT')
    Use (dbfspath +'EBPAYMT.dbf') In 0 Shared Order EBPAYMT&& ACCOUNT+REFERENCE
  ENDIF
  Private lnAppCrdt,lnAlias
  lnAlias = Select()
  lcOldOrd = Order('INVHDR')
  Set Order To InvHdr In InvHdr
  =Seek(lcInvoice,'INVHDR')
*!*    **mmmt
*!*    Select EBPAYMT
*!*    =Seek(lcAccountNO)
*!*    Scan While ACCOUNT+Reference = lcAccountNO For cfileCode = lcFileCode And  Invoiced And EBPAYMT.namount > 0
*!*      Select Credit
*!*      Set Order To Credit
*!*      =Seek(lcAccountNO)
*!*      Locate Rest While ACCOUNT+Tran+Dtos(TranDate) = lcAccountNO For TranType ='4' And;
*!*        Store = Substr(EBPAYMT.Reference,1,8) And  DPOSTDATE = EBPAYMT.dapdtrdat   And  Amount = -1 * Abs(EBPAYMT.namount) And !Deleted()
*!*      If Found()
*!*        Scatter Memo Memvar
*!*        m.nTrnNewAmn = 0 &&m.Amount
*!*        m.cShToOpn   = Iif(m.nTrnNewAmn<0,'N','Y')
*!*        Insert Into (lcAppCrdt) From Memvar
*!*      Endif
*!*    ENDSCAN
*!*    ***mmmt
 *MMMMT
  IF USED('CRDIRECT') AND RECCOUNT('CRDIRECT')>0
    SELECT 'CRDIRECT' 
    SCAN FOR TYPE ='C'
       SELECT Credit 
       SET ORDER to  CRTRAN   && TRANTYPE+TRAN
       =SEEK('0'+CRDIRECT.CtrCode)
       LOCATE REST WHILE TRANTYPE+TRAN ='0'+CRDIRECT.CtrCode   FOR Account =lcAccountNO
       IF FOUND()
         SCATTER MEMO MEMVAR 
         m.nTrnNewAmn = 0 &&m.Amount
         m.cShToOpn   = Iif(m.nTrnNewAmn<0,'N','Y')
         Insert Into (lcAppCrdt) From Memvar
       ENDIF
       SELECT Credit 
       SET ORDER TO CREDIT   && ACCOUNT+TRAN+DTOS(TRANDATE)
    ENDSCAN 
  ENDIF
  *MMMMMT 
  *MMMMT
  IF USED('CRDIRECT') AND RECCOUNT('CRDIRECT')>0
    SELECT 'CRDIRECT' 
    SCAN FOR TYPE ='I'
       SELECT Debit 
       SET ORDER TO DRTRAN   && TRANTYPE+TRAN+CINSTALNO
       =SEEK('1'+CRDIRECT.CTRCODE)
       LOCATE REST WHILE TRANTYPE+TRAN+CINSTALNO ='1'+CRDIRECT.CtrCode For Account =lcAccountNO
       IF FOUND()
         SCATTER MEMO MEMVAR 
         m.nTrnNewAmn = 0 &&m.Amount
         m.cShToOpn   = Iif(m.nTrnNewAmn>0,'N','Y')
         m.Order = CRDIRECT.Order
         m.Store = CRDIRECT.Store
         Insert Into (lcAppCrdt) From Memvar
       ENDIF
    ENDSCAN 
    SELECT Debit
    Set ORDER to DEBIT   && ACCOUNT+TRAN+CINSTALNO+DTOS(TRANDATE) 
  ENDIF
  Select (lcAppCrdt)
  =Seek(lcAccountNO)
  SUM  Amount To lninAmt Rest While ACCOUNT+Tran = lcAccountNO For !Deleted() AND nTrnNewAmn > 0
    *MMMMMT
  Select DEBIT
  =Seek(InvHdr.ACCOUNT+lcInvoice)
*!*	  lnRemAmt = -1 * (DEBIT.Amount+lninAmt)
*!*	  lcDelSt=SET("Deleted")
*!*	  SET DELETED OFF
*!*	  Select (lcAppCrdt)
*!*	  =Seek(lcAccountNO)
*!*	  Scan Rest While ACCOUNT+Tran = lcAccountNO FOR !DELETED() AND TRANTYPE <>'1'
*!*	    If lnRemAmt  = 0
*!*	      DELETE
*!*	      loop
*!*	    Endif
*!*	    Replace nTrnNewAmn With Min(0, nTrnNewAmn - lnRemAmt),;
*!*	      cShToOpn With Iif(nTrnNewAmn<0,'N','Y')
*!*	    lnRemAmt = lnRemAmt - (Amount-nTrnNewAmn)
*!*	   * Replace Amount With nTrnNewAmn
*!*	  ENDSCAN
*!*	  SET DELETED &lcDelSt.
  lnAppCrdt  = 0
  lnInvAmt  = 0
  Select (lcAppCrdt)
  =Seek(lcAccountNO)
  Sum Amount To lnAppCrdt Rest While ACCOUNT+Tran = lcAccountNO For !Deleted()
  If Abs(lnAppCrdt) > 0
    Select DEBIT
    If Seek(InvHdr.ACCOUNT+lcInvoice)
      lnTotCredit = Abs(lnAppCrdt)
      Locate Rest While ACCOUNT+Tran+cinstalno+Dtos(TranDate) = InvHdr.ACCOUNT+lcInvoice For TranType ='1'
      If Found()
        Scatter Memvar
        m.nTrnNewAmn = 0 &&Min(lnRemAmt ,0)&& m.Amount - lnTotCredit
        m.cShToOpn   = Iif(m.nTrnNewAmn>0,'N','Y')
        m.Order = InvHdr.Order
        m.Store = InvHdr.Store
        Insert Into (lcAppCrdt) From Memvar
        lnInvAmt  = DEBIT.Amount
        *  lnTotCredit = lnTotCredit - m.Amount
        *ENDSCAN
      Endif
    ENDIF
   
     
    
    **mmmt
    *XX1
    SELECT (lcAppCrdt) 
    SUM Amount TO lnTotalAmtPay 
    lnCashAmount = 0
    ldTrDate = {} 
    *XX1
    Select (lcAppCrdt)
    =Seek(lcAccountNO)
    Sum Amount To lnAppCrdt Rest While ACCOUNT+Tran = lcAccountNO For !Deleted()
    llPaymentFound =.F.
    Select EBPAYMT
    =Seek(lcAccountNO)
    Scan While ACCOUNT+Reference = lcAccountNO For cfileCode = lcFileCode And Invoiced And EBPAYMT.namount > 0
      Select Credit
      Set Order To Credit
      =Seek(lcAccountNO)
      Locate Rest While ACCOUNT+Tran+Dtos(TranDate) = lcAccountNO For TranType ='4' And;
        Store = Substr(EBPAYMT.Reference,1,8) And  DPOSTDATE = EBPAYMT.dapdtrdat   And  Amount = -1 * Abs(EBPAYMT.namount) And !Deleted()
      If Found()
        Scatter Memo Memvar
        m.nTrnNewAmn = 0 &&m.Amount
        *XX1
        *m.nTrnNewAmn = m.Amount + lnAppCrdt 
        *XX1
        m.cShToOpn   = IIF(m.nTrnNewAmn<0,'N','Y')
        Insert Into (lcAppCrdt) From Memvar
        llPaymentFound =.T.
        *XX1
        ldTrDate = EBPAYMT.dapdtrdat
        lnCashAmount = EBPAYMT.namount
        *XX1
      ENDIF  
    ENDSCAN
    if !llPaymentFound 
      Messageb("Cash Receipt is missing, cannot Key off.")  
      return .F.
    Endif
    ***mmmt
    *XX1
    IF lnTotalAmtPay <> lnCashAmount 
        &&if ABS(lnTotalAmtPay) > ABS(lnCashAmount) && Paid less sales, create charge back (Debit on Account),sales less paid, create Credit on Account
        lnDiff =  ABS(lnCashAmount) - ABS(lnTotalAmtPay)
        IF !USED('Codes_DEF')
          Use (dbfspath +'CODES.dbf') In 0 Shared AGAIN Alias Codes_DEF  Order CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
        ENDIF
        IF !USED('Customer_Def')
          Use (dbfspath +'Customer.dbf') In 0 Shared AGAIN ALIAS Customer_Def  Order Customer
        ENDIF
        IF !USED('FACTOR_Def')
          Use (dbfspath +'FACTOR.dbf') In 0 Shared AGAIN ALIAS FACTOR_Def  Order FACTOR
        ENDIF
        IF !USED('GL_LINK_DEF')
          USE (dbfspath +'GL_LINK.dbf') In 0 Shared AGAIN ALIAS GL_LINK_DEF Order GL_LINK   && LINK_CODE+CATGKEY
        ENDIF
    
        lcOnAcGLAc = ''
        =SEEK('M'+lcAccount,'Customer_Def','Customer')
        lcCreditReason = ''
        lcDebitReason = ''
        lcLnkCod =''
        lcFactor = Customer_Def.cFacCode
        IF !EMPTY(lcFactor)
          =SEEK(lcFactor ,'FACTOR_Def','FACTOR')
          lcLnkCod = FACTOR_Def.Link_code 
        ELSE
          lcLnkCod = Customer_Def.Link_Code
        ENDIF
        lcCusLnk =  lcLnkCod 
        lcOnAcGLAc = ''
        IF SEEK("M" + lcAccount, "Customer")
          IF SEEK(lcCusLnk+"001","GL_LINK_DEF")
            lcOnAcGLAc = GL_LINK_DEF.GLAcnt
          ENDIF
        ENDIF  
        
        =SEEK('D'+"CCREDITCOD",'Codes_DEF','CCODE_NO')
        lcCreditReason = Codes_DEF.CCODE_NO
        =SEEK('D'+"TRANCODE",'Codes_DEF','CCODE_NO')
        lcDebitReason =  Codes_DEF.CCODE_NO
        
        use in 'Codes_DEF'
        use in 'Customer_Def'
        use in 'GL_LINK_DEF'
        use in 'FACTOR_Def'
        
        
        SELECT (oKeyOff.cDCOnATmp)
        lcDCOnATmp = oKeyOff.cDCOnATmp
        APPEND BLANK 
        REPLACE Account    WITH lcAccountNO,;
                Tran       WITH 'T'+SUBSTR(SYS(3),4,5) ,;
                TranDate   WITH ldTrDate ,;
                Amount     WITH 0                      ,;
                cCurrCode  WITH 'USD'              ,;
                cFacCode   WITH lcFactor          ,;
                nExRate    WITH 1               ,;
                nCurrUnit  WITH 1             ,;
                cTranDesc  WITH 'N/A' 
        REPLACE &lcDCOnATmp..TranType   WITH IIF(lnDiff > 0,'6','3'),;
              &lcDCOnATmp..cReason    WITH IIF(lnDiff  > 0,lcCreditReason ,lcDebitReason ),;
              &lcDCOnATmp..Desc       WITH ''               ,;
              &lcDCOnATmp..ChgBk_Date WITH IIF(TranType="3", RemitCHB.trandate, CTOD('')) ,;
              &lcDCOnATmp..Credt_Date WITH IIF(TranType="6",  RemitCHB.trandate, CTOD('')) ,;
              &lcDCOnATmp..cFacCode   WITH lcFactor ,;
              &lcDCOnATmp..Reference  WITH lcFileCode ,;
              &lcDCOnATmp..Amount WITH IIF(TranType="6", -1 * lnDiff  ,ABS(lnDiff))
       INSERT INTO 'TmpExcel' (Issue,SMART_INVOICE_NO,Paid_Amount) VALUES (IIF(lnDiff  > 0,'Total - Credit on Account# ','Total - Chargeback# '),'',lnDiff)
    ENDIF
    *XX1

    Use In DEBIT
    Use In Credit
    
    Select (lcAppCrdt)
    =Seek(InvHdr.ACCOUNT+lcInvoice)
    *oKeyOff = Newobject("ARKEYOFF",oAriaApplication.lcAria4Class+"AR.VCX")
    lcErroHand = On('Error')
    On Error x=10

    
    oKeyOff.mKeyOff(InvHdr.ACCOUNT,InvHdr.INVDATE,lnInvAmt,lnAppCrdt,lcAppCrdt,.F.,.F.,.F.,.F.,.F.,.F.,.T.)
    
    On Error &lcErroHand.
    Select (oKeyOff.cArHistFile)
    Tableupdate(.T.)

    Select (oKeyOff.cCreditFile)
    Tableupdate(.T.)

    Select (oKeyOff.cCustHisFile)
    Tableupdate(.T.)

    Select (oKeyOff.cDebitFile)
    Tableupdate(.T.)

    Select GLDIST
    
   *C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[Begin]
    *Tableupdate(.T.)
gftableupdate()
*C202167,1 ES 20/05/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
   
    Release oKeyOff
  Endif

  Set Order To (lcOldOrd) In InvHdr
  Select (lnAlias)
 
************************************************************************************************************************************************
FUNCTION  lfAddStore
PARAMETERS lcAccountID ,lcSTORENo
lnCurAls = SELECT()
IF SEEK('M'+lcAccountID ,'CustStore' ,'Customer')
  SCATTER MEMO MEMVAR 
  m.TYPE ='S'
  m.Store = lcSTORENo
  m.StName = lcSTORENo
  INSERT INTO 'CustStore' FROM MEMVAR
ENDIF
SELECT(lnCurAls)

FUNCTION lfCreateCreditMemo
PARAMETERS lcAccountID 
*SET STEP ON 
lcOldhand = ON("Error")
   ON ERROR x=10
Select SMRTSHPLIN
LOCATE FOR totqty < 0
IF FOUND()


 SELECT SMRTSHPLIN
 *MT
*  SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty < 0 INTO CURSOR 'CrStore'
 *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
 *SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty < 0 and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') INTO CURSOR 'CrStore'
 *MMT
 *SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty < 0 and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include INTO CURSOR 'CrStore'
 *XX1
 *SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty < 0 and (IIF(Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv','RemitsInv'),RemitsInv.Include,.F.))  INTO CURSOR 'CrStore'
 SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty < 0  INTO CURSOR 'CrStore'
 *XX1
 *MMT 
 *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
 *MT
 loCrMem =''
 IF TYPE('loCrMem') <> "O"
    DO FORM (oAriaApplication.ScreenHome+"RMCRMEM.scx") NOSHOW NAME loCrMem LINKED
 ENDIF 
 *MT
 lnCntCr = 0
 *MT 
 SELECT 'CrStore'
 LOCATE 
 SCAN 
  *MT
 lnCntCr = lnCntCr + 1
 *MT 

  Wait window 'Create Credit Memo...'+Allt(STR((lnCntCr /Reccount('CrStore'))*100,3))+"%" nowait 
*!*	   loCrMem =''
   lnChoice = 1  
   
*!*	   IF TYPE('loCrMem') <> "O"
*!*	     DO FORM (oAriaApplication.ScreenHome+"RMCRMEM.scx") NOSHOW NAME loCrMem LINKED
*!*	   ENDIF 
*!*	  
   loCrMem.ChangeMode ('A')
   *MT 
*!*	   loCrMem.AriaForm1.pgfReCrmem.pgheader.cntHeader.DtCrdDate.Text1.VALUE  = Date() 
*!*	   loCrMem.AriaForm1.DtPostDate.Text1.VALUE = Date() 
   loCrMem.AriaForm1.pgfReCrmem.pgheader.cntHeader.DtCrdDate.Text1.VALUE  =ldPaymntDate 
   loCrMem.AriaForm1.DtPostDate.Text1.VALUE = ldPaymntDate 
   *MT
   loCrMem.AriaForm1.KBAccount.keyTextbox.VALUE = lcAccountID 
   loCrMem.AriaForm1.KBAccount.keyTextbox.oldValue = SPACE(5)
   loCrMem.AriaForm1.KBAccount.keyTextbox.VALID
   loCrMem.ariaform1.KBSTORE.keytextbox.oldValue = SPACE(5)
   loCrMem.ariaform1.KBSTORE.keytextbox.VALUE = CrStore.Store
   loCrMem.ariaform1.KBSTORE.keytextbox.VALID
   loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboDivision.VALUE =  CrStore.CDIVISION
   loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE =  loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.CodeDefaultValue
   loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboLocation.VALUE =  SUBSTR(CrStore.Store,1,6)
   loCrMem.ariaForm1.rmCRMBUS.mUpdHdrFl(loCrMem.ariaForm1.rmCRMBUS.lcCrMemHdr)
   loCrMem.ariaForm1.pgfReCrmem.pgHeader.cntHeader.txtpONo.Value= ''
   loCrMem.ariaForm1.pgfReCrmem.pgHeader.cntHeader.txtpONo.Valid
   =SEEK('S'+lcAccountID +CrStore.Store,'CUSTOMER','CUSTOMER')
   lnDisc = Customer.Disc
   lnTotAmt = 0
   SELECT SMRTSHPLIN
   *MT
   *SCAN FOR Store = CrStore.Store AND  CDIVISION = CrStore.CDIVISION AND TotQty < 0 
   *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start] 
   *SCAN FOR Store = CrStore.Store AND  CDIVISION = CrStore.CDIVISION AND TotQty < 0 and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') 
   *XX1
   *SCAN FOR Store = CrStore.Store AND  CDIVISION = CrStore.CDIVISION AND TotQty < 0 and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include 
   SCAN FOR Store = CrStore.Store AND  CDIVISION = CrStore.CDIVISION AND TotQty < 0 &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') 
   *XX1
   *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
   *MT
     lnTotAmt = lnTotAmt + (ABS(SMRTSHPLIN.TotQty) * SMRTSHPLIN.Price)
     lnRetPriceValue = SMRTSHPLIN.PRICE
     loCrMem.ariaForm1.rmCRMBUS.llAddLine = .T.
     WITH loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail
       .cboReason.VALUE = loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE
       .cboReason.REQUERY()
       .cboQuality.VALUE = loCrMem.laStyGrade[1,2]
       .cboQuality.REQUERY()
       STORE " " TO .KbStyle.VALUE , .txtStyDesc.VALUE , loCrMem.ariaForm1.rmCRMBUS.lcCurLine ,;
       loCrMem.ariaForm1.rmCRMBUS.lcTranCd , .KbStyRetTo.VALUE ,;
       loCrMem.ariaForm1.rmCRMBUS.lcScale  , .kbConfig.KeyTExtBOx.VALUE , .kbDyelot.KeyTExtBOx.VALUE
       STORE 0   TO .txtTotAlQty.VALUE , .txtPrice.VALUE , .txtTotAmount.VALUE , .txtGrsPrice.VALUE , .txtDiscount.VALUE ,;
            .txtHstRat.VALUE , .txtPstRat.VALUE ,;
       loCrMem.ariaForm1.rmCRMBUS.lnScaleCnt , loCrMem.ariaForm1.rmCRMBUS.lnCost , loCrMem.ariaForm1.rmCRMBUS.lnDisc_Amt , loCrMem.ariaForm1.rmCRMBUS.lnTrde_Amt ,;
       loCrMem.ariaForm1.rmCRMBUS.lnPstTotal , loCrMem.ariaForm1.rmCRMBUS.lnDiscPcnt
       STORE '' TO .cboEmpl.VALUE
       STORE 0 TO .txtCost.VALUE
       .SbrkBackToStk.SCALE = ""
       FOR lnI = 1 TO 8
         lcI = STR(lnI,1)
         .SbrkBackToStk.txtQty&lcI..VALUE = 0
         .SbrkBackToStk.txtsizelbl&lcI..VALUE = ""
       ENDFOR
       .SbrkBackToStk.txtTotQty.VALUE =  0
       .SbrkBackToStk.txtTotQty.ENABLED = .F.
     ENDWITH
     loCrMem.AriaForm1.llLinStat  = .T.
     STORE .F. TO loCrMem.AriaForm1.llRetStat  ,loCrMem.AriaForm1.llDyeStat  , loCrMem.AriaForm1.llsizestat
     loCrMem.AriaForm1.mShowObj()
     loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = " "
     loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.TxtItem.VALUE =SMRTSHPLIN.STYLE
     *llOldCst = loCrMem.Ariaform1.RMCRMBUS.llStdCost
     *loCrMem.Ariaform1.RMCRMBUS.llStdCost = .T.
     loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALID(.T.,0,SMRTSHPLIN.STYLE,SPACE(19),SMRTSHPLIN.STYLE)
     *loCrMem.Ariaform1.RMCRMBUS.llStdCost = llOldCst
     lnCstValue = SMRTSHPLIN.Price
*!*       lcDatse = SET("Datasession")
*!*       SET DATASESSION TO loCrMem.DATASESSIONID
*!*       lcCrMemLin = loCrMem.Ariaform1.RMCRMBUS.lcCrMemLin
*!*       loCrMem.Ariaform1.RMCRMBUS.lnCost = loCrMem.Ariaform1.RMCRMBUS.mcostassign(.T.)
*!*       loCrMem.Ariaform1.RMCRMBUS.lnCost = IIF(llDefaultSty ,lnCstValue ,loCrMem.Ariaform1.RMCRMBUS.lnCost)
*!*          REPLACE COST WITH IIF(llDefaultSty ,lnCstValue ,loCrMem.Ariaform1.RMCRMBUS.lnCost) IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
*!*          SET DATASESSION TO lcDatse
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALUE = loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.CodeDefaultValue
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALID()
        loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE= loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.CodeDefaultValue
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.OldValue = 0
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALUE = SMRTSHPLIN.PRICE
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALID
        IF TYPE('loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue') <> 'N'
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue = 0
        ENDIF
        *loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.VALUE = lnDisc
        *loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.VALID
        FOR lnX= 1 TO 8
          lcX = STR(lnX,1)
          IF SMRTSHPLIN.Qty&lcX. < 0
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..OldValue  = 0
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALUE  = ABS(SMRTSHPLIN.Qty&lcX.)
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALID
          ENDIF
        ENDFOR
   ENDSCAN
   lnChoice = 2
   lcCrMemo = ''
   loCrMem.SaveFiles(.F.)
*!*	   IF TYPE('loCrMem') = 'O'
*!*	     loCrMem.Release
*!*	     loCrMem= Null
*!*	   ENDIF

*!*	   lcDatse = SET("Datasession")
*!*	   SET DATASESSION TO loCrMem.DATASESSIONID
*!*	   SELECT Credit 
*!*	   =gfTableUpdate()
*!*	   TABLEUPDATE(.T.)
*!*	   SET DATASESSION TO lcDatse
   IF !EMPTY(lcCrMemo) 
     INSERT INTO 'CRDIRECT' Values(lcCrMemo,lnTotAmt,'C',SPACE(6),CrStore.Store)
     Select SMRTSHPLIN
     LOCATE
     *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
     *SCAN FOR Store = CrStore.Store AND totqty < 0 AND EMPTY(Order) AND CDIVISION = CrStore.CDIVISION and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
     *XX1
     *SCAN FOR Store = CrStore.Store AND totqty < 0 AND EMPTY(Order) AND CDIVISION = CrStore.CDIVISION and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include 
     SCAN FOR Store = CrStore.Store AND totqty < 0 AND EMPTY(Order) AND CDIVISION = CrStore.CDIVISION &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
     *XX1
     *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
        lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+lcCrMemo+;
                 "'  Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO +"' AND STYLE ='"+SMRTSHPLIN.STYLE+"' AND  [Order]='' AND TotQty < 0  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
        If lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
          Return .F.
        Endif
     ENDSCAN 
   ENDIF 
 ENDSCAN 
   IF TYPE('loCrMem') = 'O'
     loCrMem.Release
     loCrMem= Null
   ENDIF

ENDIF
 On Error &lcOldhand.



FUNCTION lfCreateDirectInvoice
PARAMETERS lcAccountID 
 lcOldhand = ON("Error")
 ON ERROR x=10
 *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
 * SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty > 0 AND EMPTY(Order) INTO CURSOR 'INVStore'
 *SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty > 0 AND EMPTY(Order) and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')  INTO CURSOR 'INVStore'
 *MMT
 *SELECT Distinct Store,CDIVISION FROM SMRTSHPLIN WHERE totqty > 0 AND EMPTY(Order) and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include INTO CURSOR 'INVStore'
 *XX1
 *SELECT Distinct Store,CDIVISION  FROM SMRTSHPLIN WHERE totqty > 0 AND EMPTY(Order) and (IIF(Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv','RemitsInv'),RemitsInv.Include,.F.)) INTO CURSOR 'INVStore'
 SELECT Distinct Store,CDIVISION  FROM SMRTSHPLIN WHERE totqty > 0 AND EMPTY(Order) INTO CURSOR 'INVStore'
 *XX1
 *MMT
 *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
 loDInv =''
 IF TYPE('loDInv') <> "O"
     DO FORM (oAriaApplication.ScreenHome+"ar\ardinv.scx") NOSHOW NAME loDInv LINKED
 ENDIF
 *MMT
 lnCountLine = 0
 *MMT  
 SELECT 'INVStore'
 SCAN 
   lnCountLine = lnCountLine + 1
   Wait window 'Create Direct Invoice...'+Allt(STR((lnCountLine /Reccount('INVStore'))*100,3))+"%" nowait 
*!*	   loDInv =''
   lnChoice = 1  
   
*!*	   IF TYPE('loDInv') <> "O"
*!*	     DO FORM (oAriaApplication.ScreenHome+"ar\ardinv.scx") NOSHOW NAME loDInv LINKED
*!*	   ENDIF
   
   loDInv.changemode ('A')
   *MT 
*!*	   loDInv.DefaultInvoiceDate = DATE()
*!*	   loDInv.DefaultPostingDate = DATE()
   loDInv.DefaultInvoiceDate = ldPaymntDate 
   loDInv.DefaultPostingDate = ldPaymntDate 
   *MT
   loDInv.laSetups[18,2] = 'N'
   loDInv.mCreateTempfiles
   loDInv.DefaultWarecode = SUBSTR(INVStore.Store,1,6)
   WITH loDInv.AriaForm1.AriaPageframe1.Page2.InvoiceEditRegion1
     STORE 0 TO .TaxDueAmount, .Merchandisetax, .TotalCartons
   ENDWITH
   loDInv.DefaultSeason =  '*'
   loDInv.DefaultDivision = INVStore.CDIVISION &&loDInv.AriaForm1.AriaPageFrame1.Page1.cboDivision.CodeDefaultValue
   loDInv.ariaform1.keyAccount.keytextbox.oldValue = SPACE(5)
   loDInv.ariaform1.keyAccount.keytextbox.VALUE = lcAccountID 
   loDInv.ariaform1.keyAccount.keytextbox.VALID
   loDInv.ariaform1.keySTORE.keytextbox.oldValue = SPACE(5)
   loDInv.ariaform1.keySTORE.keytextbox.VALUE = INVStore.Store
   loDInv.ariaform1.keySTORE.keytextbox.VALID
   =SEEK('S'+lcAccountID +INVStore.Store,'CUSTOMER','CUSTOMER')
   lnDisc = Customer.Disc
   Select SMRTSHPLIN
   lnTotAmt = 0
   *MT
*   SCAN FOR Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION 
   *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
   *SCAN FOR Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') 
   *XX1
   *SCAN FOR Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include  
   SCAN FOR Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
   *XX1
   *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
   *mt  
     lnTotAmt = lnTotAmt + SMRTSHPLIN.PRICE * SMRTSHPLIN.TotQty
     lnRetPriceValue = SMRTSHPLIN.PRICE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()
     STORE .T. TO loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llNewline,loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llAddLine
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.ENABLED = .T.
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.TxtItem.VALUE = SMRTSHPLIN.STYLE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.VALID(.T.,0,SMRTSHPLIN.STYLE,SPACE(19),SMRTSHPLIN.STYLE)
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()

     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.OldValue  = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.VALUE  = SMRTSHPLIN.PRICE
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.LOSTFOCUS()
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.oldValue = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.VALUE = 0
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.LOSTFOCUS()
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty1.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty2.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty3.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty4.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty5.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty6.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty7.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty8.CONTROLSOURCE = ''
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtTotQty.CONTROLSOURCE = ''
     FOR lnX= 1 TO 8
       lcX = STR(lnX,1)
       IF SMRTSHPLIN.Qty&lcX. > 0
         loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALUE  =  SMRTSHPLIN.Qty&lcX.
         loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALID
       ENDIF
     ENDFOR
     loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.LOSTFOCUS()
      =gfAdd_Info(loDInv.lcInvLine,loDInv)
   ENDSCAN
*!*	   lcDatse = SET("Datasession")
*!*	   SET DATASESSION TO loDInv.DATASESSIONID
*!*	   *lcOrderNm = gfSequence('ORDER','','',EVALUATE(loDInv.lcInvHdr+'.cDivision')) 
*!*	   REPLACE ORDER WITH lcOrderNm IN (loDInv.lcInvHdr)
*!*	   SET DATASESSION TO lcDatse
   loDInv.AriaForm1.AriaPageFrame1.Page3.ACTIVATE()
   lnChoice = 2
   DIMENSION  laInv[1]
   laInv[1] = ''  
   IF lnTotAmt <> 0
     loDInv.HIDE()
     loDInv.SaveFiles(.F.)
   ENDIF  
*!*	   IF TYPE('loDInv') ='O'
*!*	     loDInv.Release()
*!*	     loDInv = Null
*!*	   ENDIF  
*!*	   lcDatse = SET("Datasession")
*!*	   SET DATASESSION TO loDInv.DATASESSIONID
*!*	   SELECT Debit 
*!*	   TABLEUPDATE(.T.)
*!*	   SET DATASESSION TO lcDatse
   IF !EMPTY(laInv[1])
     INSERT INTO  'CRDIRECT' VALUES (laInv[1],lnTotAmt,'I', '', CrStore.Store)
     Select SMRTSHPLIN
     LOCATE
     *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][Start]
     *SCAN FOR Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')  
     *XX1
     *SCAN FOR Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv') AND RemitsInv.Include    
     SCAN FOR Store = INVStore.Store AND totqty > 0 AND EMPTY(Order) AND CDIVISION = INVStore.CDIVISION &&and Seek(SMRTSHPLIN.SMART_INVOICE_NO ,'RemitsInv')
     *Xx1
     *B611150,1 MMT 06/01/2016 Issue#8 - 820 program create invoices for smart invoices have issue in ebremitt table[T20151014.0017 - Issue#8][End]
        lnResult=oAriaApplication.remotecompanydata.SqlRun("Update SMART_INVOICE_LINES Set cTrCode ='"+laInv[1]+;
                 "'  Where SMART_INVOICE_NO = '"+SMRTSHPLIN.SMART_INVOICE_NO +"' AND STYLE ='"+SMRTSHPLIN.STYLE+"' AND  [Order]='' AND TotQty > 0  AND (CTRCODE ='' OR ISNULL(CTRCODE,'S')='S')",'SMRTSHPLINUP' , ;
                 "SMART_INVOICE_HEADER",oAriaApplication.activecompanyconstr,3,'SAVE',Set("Datasession"))
        If lnResult<>1
          oAriaApplication.remotecompanydata.CheckRetResult("sqlrun",lnResult,.T.)
          Wait Window 'SMART_INVOICE_LINES'+'.SqlRun()'
          Return .F.
        Endif
     ENDSCAN 
   ENDIF  
 ENDSCAN 
 IF TYPE('loDInv') ='O'
   loDInv.Release()
   loDInv = Null
 ENDIF 
On Error &lcOldhand.
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