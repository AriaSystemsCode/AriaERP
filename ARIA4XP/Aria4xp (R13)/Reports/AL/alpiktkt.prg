*:***************************************************************************
*: Program file  : ALPIKTKT
*: Program desc. : MOMENTRENDS ALLOCATION PIKTICKT LOG
*! Date          : 12/27/04
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar - (MMT) Due to issue:037542
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPIKTKT
*:***************************************************************************
*: Modifications:
*: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [T20080603.0001]
*: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[T20110106.0004]
*: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006]
*: B610679,1 TMI 02/18/2014 Modify report program to apply 'has packing list' filtering code in case of exporting to excel  [T20140209.0008]
*: E303473,1 MMT 05/13/2014 Add complete and start dates to Picking ticket log report[T20131209.0045]
*: B610932,1 MMT 01/20/2015 Picking ticket log report is not exporting Start date field to Excel[T20150116.0002]
*: B611514,1,HMS 12/2/2018  Strangeness in the Pick Ticket Log - company J1 [T20171130.0022]
*:***************************************************************************
*--Section of Variables

lcTime = Time()         && Variable to hold the Time

* N000682,1 31/12/2012 Thabet  Handle globalization issues [Start]
#Include R:\aria4xp\Reports\AL\alpiktkt.h
* N000682,1 31/12/2012 Thabet  Handle globalization issues [END]
loOgScroll.cCRorientation = 'P'
*To get the date in a variables
lnPosDate = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
If lnPosDate > 0
  lnPosDate = Asubscript(loOgScroll.laOgFXFlt,lnPosDate,1)
  LDATE = Ctod(Alltrim(Substr(laOgFXFlt[lnPosDate,6],1,10)))
  HDATE = Ctod(Alltrim(Substr(laOgFXFlt[lnPosDate,6],12,20)))
Endif

If loOgScroll.llOGFltCh
  * N000682,1 31/12/2012 Thabet  Handle globalization issues [Start]
  *  WAIT WINDOW "Collecting Data......." NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT WINDOW LANG_COLLECT_DATA NOWAIT
  Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECT_DATA,oAriaApplication.GetHeaderText("LANG_COLLECT_DATA",AHEADERFILE)) Nowait
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682,1 31/12/2012 Thabet  Handle globalization issues [END]
  If Used(lcTempOrdHdr)
    Use In (lcTempOrdHdr)
    *: E303473,1 MMT 05/13/2014 Add complete and start dates to Picking ticket log report[Start]
    *SELECT CORDTYPE,ACCOUNT,ORDER,cCurrCode,Nexrate,NcurrUnit,SHIPVIA FROM &lcTempOrdhd WHERE .F. into CURSOR &lcTempOrdHdr READWRITE
    Select CORDTYPE,ACCOUNT,Order,cCurrCode,Nexrate,NcurrUnit,SHIPVIA,Start,Complete From &lcTempOrdhd Where .F. Into Cursor &lcTempOrdHdr Readwrite
    *: E303473,1 MMT 05/13/2014 Add complete and start dates to Picking ticket log report[End]
    =lfMakeIndex(lcTempOrdHdr)
  Endif
  If Used(lcPickTkTmp)
    Select &lcTempPikTkt..*  From  &lcTempPikTkt Where .F. Into Cursor  &lcPickTkTmp Readwrite
    =lfMakeIndex(lcPickTkTmp)
  Endif
  If Used(lcTempCustomer)
    Select Type,ACCOUNT,Store,BTNAME From &lcTempCust Where .F. Into Cursor &lcTempCustomer Readwrite
    =lfMakeIndex(lcTempCustomer)
  Endif
  Do lpCreaTemp  && Create Temp Cursor
  Do lpColect    && Collect data
Endif

Select (lcPickTmp)   &&Temp. File Hold The Records That Satisfy Requiments
Set Order To Tag &lcPickTmp

llEndReprt = .F.
Go Bottom
Replace &lcPickTmp..lEndRep With .T.

Select(lcPickTmp)
Set Relation To 'O' + Order Into &lcTempOrdHdr Additive
Set Relation To Iif(Empty(Store) , 'M' + ACCOUNT ,;
  'S' + ACCOUNT + Store) Into &lcTempCustomer Additive
Locate

lcCurrExpr = Iif(llMultCurr And lcRpCurr="F",[EVALUATE(lcTempOrdHdr+'.CCURRCODE')],[oAriaApplication.BaseCurrency ])
lcCurrCode = Evaluate(lcCurrExpr)
llCurrChg  = .F.
lcTime1 = Time()         && Variable to hold the Time

*!*	IF RECCOUNT(lcPickTmp)> 0
*!*	  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPickTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcTime,lcTime1),6,2)) + ' Seconds...' TIMEOUT 2
*!*	ENDIF
Select(lcPickTmp)
Set Filter To

* B611514 ,1, HMS ,  Strangeness in the Pick Ticket Log - company J1 [T20171130.0022][Begin]

*!*	llHasPack_List = .F.
*!*	IF lcRPPCKLST  <> 'B'
*!*	  llHasPack_List  = IIF(lcRPPCKLST = 'Y',.T.,.F.)
*!*	  SELECT(lcPickTmp)
*!*	  IF llHasPack_List
*!*	    SET FILTER TO loPack_Hdr.Seek(&lcPickTmp..piktkt)
*!*	  ELSE
*!*	    SET FILTER TO !loPack_Hdr.Seek(&lcPickTmp..piktkt)
*!*	  ENDIF
*!*	ENDIF
* B611514 ,1, HMS ,  Strangeness in the Pick Ticket Log - company J1 [T20171130.0022][END]

Select(lcPickTmp)
*B610679,1 TMI 02/18/2014 17:19 [Start] get the filter to use in the select statement that exports to Excel
lcSvFlt = Upper(Filter())
*B610679,1 TMI 02/18/2014 17:19 [End  ]
Locate
If Eof()
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  Return
Endif
*: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[Start]
If (oAriaApplication.gcDevice = "FILE" .And. loOgScroll.cTextRepType = "EXCEL")
  lcOrderBExp = Iif(llMultCurr And (lcRpCurr = "F") ,Iif(LCRPSORT='P',"cCurrCode , PIKTKT","cCurrCode , SHIPVIA,PIKTKT"),;
    IIF(LCRPSORT='P',"PIKTKT","SHIPVIA,PIKTKT"))
  *: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006][Begin]
  *SELECT *, 0000000 as TotQty, 00000000000.00 as Amount FROM (lcPickTmp) INTO CURSOR 'TmpExcl' READWRITE ;
  *ORDER BY &lcOrderBExp.
  *: B610932,1 MMT 01/20/2015 Picking ticket log report is not exporting Start date field to Excel[T20150116.0002][Start]
  *!*		SELECT *, 0000000 as TotQty, 00000000000.00 as Amount, SPACE(30) as BtName FROM (lcPickTmp) INTO CURSOR 'TmpExcl' READWRITE ;
  *!*		ORDER BY &lcOrderBExp.
  Select *, 0000000 As TotQty, 00000000000.00 As Amount, Space(30) As BTNAME,{} As Start,{} As Complete From (lcPickTmp) Into Cursor 'TmpExcl' Readwrite ;
    ORDER By &lcOrderBExp.
  *: B610932,1 MMT 01/20/2015 Picking ticket log report is not exporting Start date field to Excel[T20150116.0002][End]
  *: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006] [End]
  Set Relation To Iif(llMultCurr And (lcRpCurr = "F") ,Iif(LCRPSORT='P',cCurrCode + PIKTKT,cCurrCode + SHIPVIA+PIKTKT),;
    IIF(LCRPSORT='P',PIKTKT,SHIPVIA+PIKTKT)) Into (lcPickTmp)
  Select 'TmpExcl'
  *B610679,1 TMI 02/18/2014 17:20 [Start] select the filtered lines only if the filter is applied
  If !Empty(lcSvFlt)
    lcSvFlt = Strtran(lcSvFlt,Upper(lcPickTmp),'TmpExcl')
    Set Filter To Evaluate(lcSvFlt)
  Endif
  *B610679,1 TMI 02/18/2014 17:20 [end ]
  Locate
  Scan
    Store 0 To lnTotQty,lnTotAmnt
    lfSumPik()
    Replace TotQty With lnTotQty,Amount With lnTotAmnt In  'TmpExcl'
    *: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006] [Begin]
    Replace BTNAME With &lcTempCustomer..BTNAME
    *: E303148,1 HIA 05/14/2012 Export Customer name to Excel[T20120425.0006] [End]
    *: B610932,1 MMT 01/20/2015 Picking ticket log report is not exporting Start date field to Excel[T20150116.0002][Start]
    Replace Start With  &lcTempOrdHdr..Start
    Replace Complete With  &lcTempOrdHdr..Complete
    *: B610932,1 MMT 01/20/2015 Picking ticket log report is not exporting Start date field to Excel[T20150116.0002][End]
  Endscan

  Set Relation Off Into (lcPickTmp)
Endif
Store 0 To lnTotQty,lnTotAmnt
*: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[End]
Do gfDispRe With Evaluate('lcRpName')

*-- Clear relation
Select (lcPickTmp)
Set Relation To


*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from        : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : =fSeTOrdr()
*!**************************************************************************
*! Note               : symbol is [S,Set- R,ReSet]
*!**************************************************************************
Function lfSeTOrdr
Parameters OpGrdParm
Select ORDHDR
Do Case
Case OpGrdParm = 'S'
  lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
  Set Order To Customer In Customer
  Set Relation To &lcRelation Into Customer
  Go Top
Case OpGrdParm = 'R'
  Select ORDHDR
  Set Relation Off Into Customer
Endcase

*!**************************************************************************
*! Name      : lfSumPik
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : To return the Total Amount and Total Quantity
*!**************************************************************************
*! Calls     :
*!**************************************************************************
*! Called from        : ALPKTMTD.FRX
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : lfSumPik()
*!**************************************************************************
*! Note               :
*!**************************************************************************
Function lfSumPik

Private lcExpList,lnCurAlias,lnPrice
lnCurAlias = Select(0)
lcExpList = "TotPik,TotPik*Price"
*--
If &lcPickTmp..Status $ 'CX'

  loDBFPikLine.Seek(&lcPickTmp..PIKTKT)
  Select(lcTempPikLine)
  Sum &lcExpList Rest While PIKTKT+Order+Str(Lineno,6) = &lcPickTmp..PIKTKT ;
    TO lnTotQty,lnTotAmnt

Else

  loDBFOrdline.Seek('O'+&lcPickTmp..Order+&lcPickTmp..Store)
  Select(lcTempOrdline)
  Sum &lcExpList Rest While CORDTYPE+Order+Store+Style+Str(Lineno,6) = ;
    'O'+&lcPickTmp..Order+&lcPickTmp..Store ;
    FOR PIKTKT = &lcPickTmp..PIKTKT To lnTotQty,lnTotAmnt
Endif

If llMultCurr And (lcRpCurr <> "F") And;
    (lnTotAmnt <> 0) And (Evaluate(lcTempOrdHdr+'.CCURRCODE') <> oAriaApplication.BaseCurrency)
  lnTotAmnt = gfAmntDisp(lnTotAmnt,lcRpCurr,ldRpExDate,;
    lcRpTmpNam,.F.,lcTempOrdHdr)
Endif

If !llCurrChg
  llCurrChg = !(Evaluate(lcCurrExpr) == lcCurrCode)
  lcCurrCode = Evaluate(lcCurrExpr)
Endif

llEndReprt =  &lcPickTmp..lEndRep
Select (lnCurAlias)
Return 0

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
Function lfwRepWhen

= lfvInvoice()
*--Ordline File
loDBFOrdline = Createobject("RemoteTable","Ordline","ORDLINST",lcTempOrdline,Set("DATASESSION"))

*--PikLine File
loDBFPikLine = Createobject("RemoteTable","PikLine","PikLine",lcTempPikLine,Set("DATASESSION"))

*--Ordhdr File
loDBFOrdhdr   = Createobject("RemoteTable","Ordhdr","Ordhdr",lcTempOrdhd,Set("DATASESSION"))
*: E303473,1 MMT 05/13/2014 Add complete and start dates to Picking ticket log report[Start]
*SELECT CORDTYPE,ACCOUNT,ORDER,cCurrCode,Nexrate,NcurrUnit,SHIPVIA FROM &lcTempOrdhd WHERE .F. into CURSOR &lcTempOrdHdr READWRITE
Select CORDTYPE,ACCOUNT,Order,cCurrCode,Nexrate,NcurrUnit,SHIPVIA,Start,Complete From &lcTempOrdhd Where .F. Into Cursor &lcTempOrdHdr Readwrite
*: E303473,1 MMT 05/13/2014 Add complete and start dates to Picking ticket log report[End]
=lfMakeIndex(lcTempOrdHdr)

*--PikTkt file
loDBFPikTkt  = Createobject("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,Set("DATASESSION"))
Select &lcTempPikTkt..*  From  &lcTempPikTkt Where .F. Into Cursor  &lcPickTkTmp Readwrite
=lfMakeIndex(lcPickTkTmp)
*--Customer file
loDBFCust     = Createobject("RemoteTable","Customer","Customer",lcTempCust,Set("DATASESSION"))
Select Type,ACCOUNT,Store,BTNAME From &lcTempCust Where .F. Into Cursor &lcTempCustomer Readwrite
=lfMakeIndex(lcTempCustomer)

If Type('loPack_Hdr') <> 'O'
  loPack_Hdr = Createobject("RemoteTable","Pack_hdr","Pack_hdr",'Pack_hdr',Set("DATASESSION"))
Endif

*!*************************************************************
*! Name      : lfvInvoice
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
Function lfvInvoice

If lcRPInv = 'Y'
  llRPRelPT = .F.
Endif
lnRelPTPo = Asubscript(laOGObjType,Ascan(laOGObjType,'LLRPRELPT'),1)
laOGObjCnt[lnRelPTPo] = lcRPInv $ 'BN'
= lfOGShowGet('LLRPRELPT')

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
Function lfsrAcc
Parameters lcParm
Select Customer
Set Order To Customer
Go Top
*-- End of lfsrAcc.

*!*************************************************************
*! Name      : lfsrPkt
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrPkt()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol is Reset
*!*************************************************************
Function lfsrPkt
Parameters lcParm
Select PIKTKT
Locate
*-- End of lfsrPkt.
*!*************************************************************
*! Name      : lfFillVars
*: Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
Function lfFillVars

If llMultCurr
  *-- Fill Currency arrays [Begin]
  Dimension laCurrVal[1,1]
  *-- Open Currency file.
  If !Used('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  Else
    Select SYCCURR
    Set Order To cCurrCode  && To VALIDATE currency code.
  Endif

  Select Distinct cCurrCode From SYCCURR Order By cCurrCode Into Array laCurrVal
  Dimension laCurrDesc[ALEN(laCurrVal,1),1]

  For lnI = 1 To Alen(laCurrVal,1)
    = Seek(Alltrim(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = Padr(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = cCurrCode + ' - ' + Alltrim(CCURRDESC)
  Endfor
  *-- Fill Currency arrays [End  ]
Endif

*-- End Of lfFillVars.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
Function lfClearRep
llClearFn = .T.  &&You erase temporary file.
*-- Close temp. opended files, if it used.

If llMultCurr
  Set Currency To lcCurrSymb
  Set Currency &lcCurAlign
Endif

*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfGtCurDes
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Function Get the Description ofthe Currency
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGtCurDes()
*!*************************************************************
Function lfGtCurDes

If lcRpCurr <> "F"
  Return oAriaApplication.BaseCurrency
Else
  If llMultCurr And Seek(&lcTempOrdHdr..cCurrCode,'SYCCURR')
    Return SYCCURR.CCURRDESC
  Endif
Endif

Return ""
*-- end of lfGtCurDes.

*!**************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Procedure to create Temp. File
*!**************************************************************************
*! Example   : DO lpCreaTemp
*!**************************************************************************
Procedure lpCreaTemp

*-- check If File is created or not
If Used(lcPickTmp) And Reccount(lcPickTmp) > 0
  Use In (lcPickTmp)
Endif
*-- Create File
If !Used(lcPickTmp)
  If Type("laTempStru[1,1]") $ "UL"
    Dimension laTempStru[1,18]
    Select(lcTempPikTkt)
    =Afields(laTempStru)
    =lfAddField("laTempStru","cCurrCode","C",3,0)
    =lfAddField("laTempStru","lEndRep","L",1,0)
    =lfAddField("laTempStru","SHIPVIA","C",15,0)
  Endif

  *!*	  IF llMultCurr AND (lcRpCurr = "F")
  *!*	    =gfCrtTmp(lcPickTmp,@laTempStru,"cCurrCode + PIKTKT",lcPickTmp,.T.)
  *!*	  ELSE
  *!*	    =gfCrtTmp(lcPickTmp,@laTempStru,"PIKTKT",lcPickTmp,.T.)
  *!*	  ENDIF
  If llMultCurr And (lcRpCurr = "F")
    *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[Start]
    * =gfCrtTmp(lcPickTmp,@laTempStru,IIF(LCRPSORT='P',"cCurrCode + PIKTKT","cCurrCode + SHIPVIA"),lcPickTmp,.T.)
    =gfCrtTmp(lcPickTmp,@laTempStru,Iif(LCRPSORT='P',"cCurrCode + PIKTKT","cCurrCode + SHIPVIA+ PIKTKT"),lcPickTmp,.T.)
    *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[END]
  Else
    *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[Start]
    *=gfCrtTmp(lcPickTmp,@laTempStru,IIF(LCRPSORT='P',"PIKTKT","SHIPVIA"),lcPickTmp,.T.)
    =gfCrtTmp(lcPickTmp,@laTempStru,Iif(LCRPSORT='P',"PIKTKT","SHIPVIA+PIKTKT"),lcPickTmp,.T.)
    *: B609520,1 MMT 02/07/2011 Export Total Qty and Amount to Excel[ENd]
  Endif

  Select(lcPickTmp)
  Zap

Endif
*-- End of lpCreaTemp.

*!**************************************************************************
*! Name      : lpColect
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Procedure TO Colecte Data
*!**************************************************************************
*! Example   : Do lpColect()
*!**************************************************************************

Procedure lpColect


*-- Check If user select a piktkt no. or not
lnPosPikTkt = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
If lnPosPikTkt > 0
  lnPosPikTkt = Asubscript(loOgScroll.laOgFXFlt,lnPosPikTkt,1)
  lcPikTktSel =Iif(!Empty(laOgFXFlt[lnPosPikTkt,6]),laOgFXFlt[lnPosPikTkt,6],'')
  If !Empty(lcPikTktSel)
    Select(lcPikTktSel)
    Locate
    If !Eof()
      Scan
        loDBFPikTkt.Seek(&lcPikTktSel..PIKTKT)
        Select(lcTempPikTkt)
        Scatter Memo Memvar
        Insert Into (lcPickTkTmp) From Memvar
      Endscan
      lcSelFlds  = "&lcPickTkTmp..*"
      lcSelFiles = lcPickTkTmp
      lcSeleCond = ""
      *--Check if user select order no.
      lnPosOrder = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.ORDER")
      If lnPosOrder > 0
        lnPosOrder = Asubscript(loOgScroll.laOgFXFlt,lnPosOrder,1)
        lcOrderSel =Iif(!Empty(laOgFXFlt[lnPosOrder,6]),laOgFXFlt[lnPosOrder,6],'')
        If !Empty(lcOrderSel)
          Select(lcOrderSel)
          Locate
          If !Eof()
            lcSelFiles = lcSelFiles + "," + lcOrderSel
            lcSeleCond = lcSeleCond + Iif(Empty(lcSeleCond),""," AND ") + lcPickTkTmp + ".ORDER = " + lcOrderSel + ".ORDER"
          Endif
        Endif
      Endif
      *--Check if user select warecode
      lnPosWare = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
      If lnPosWare > 0
        lnPosWare = Asubscript(loOgScroll.laOgFXFlt,lnPosWare,1)
        lcWareSel =Iif(!Empty(laOgFXFlt[lnPosWare,6]),laOgFXFlt[lnPosWare,6],'')
        If !Empty(lcWareSel)
          Select(lcWareSel)
          Locate
          If !Eof()
            lcSelFiles = lcSelFiles + "," + lcWareSel
            lcSeleCond = lcSeleCond + Iif(Empty(lcSeleCond),""," AND ") + lcPickTkTmp + ".CWARECODE = " + lcWareSel + ".CWARECODE"
          Endif
        Endif
      Endif
      *--Check if user select accounts
      lnPosAcc = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
      If lnPosAcc > 0
        lnPosAcc = Asubscript(loOgScroll.laOgFXFlt,lnPosAcc,1)
        lcAccSel =Iif(!Empty(laOgFXFlt[lnPosAcc,6]),laOgFXFlt[lnPosAcc,6],'')
        If !Empty(lcAccSel)
          Select(lcAccSel)
          Locate
          If !Eof()
            lcSelFiles = lcSelFiles + "," + lcAccSel
            lcSeleCond = lcSeleCond + Iif(Empty(lcSeleCond),""," AND ") + lcPickTkTmp + ".ACCOUNT = " + lcAccSel + ".ACCOUNT"
          Endif
        Endif
      Endif
      *--Check Date
      lnPosDate = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
      If lnPosDate > 0
        lnPosDate = Asubscript(loOgScroll.laOgFXFlt,lnPosDate,1)
        SDATE = Substr(laOgFXFlt[lnPosDate,6],1,10)
        EDATE = Substr(laOgFXFlt[lnPosDate,6],12,20)

        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        If Substr(laOgFXFlt[lnPosDate,6],1,1) = "|"
          SDATE = Dtoc({})
          EDATE = Substr(laOgFXFlt[lnPosDate,6],2,11)
        Endif
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]


        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        If (!Empty(EDATE) And !Empty(SDATE)) Or (!Empty(EDATE) And Empty(SDATE))
          *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

          lcSeleCond = lcSeleCond + Iif(Empty(lcSeleCond),""," AND ")+ "BETWEEN(&lcPickTkTmp..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        Endif
      Endif
      Select(lcPickTkTmp)
      Locate
      If !Empty(lcSeleCond)
        Select &lcSelFlds From &lcSelFiles Where &lcSeleCond Into Cursor &lcFnlPikTKT
      Else
        Select &lcSelFlds From &lcSelFiles Into Cursor &lcFnlPikTKT
      Endif
      Select(lcFnlPikTKT)
      =lfGetOrdhdrFile()
      Return
    Endif
  Endif
Endif
*--check if user select order no.
lnPosOrder = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.ORDER")
If lnPosOrder > 0
  lnPosOrder = Asubscript(loOgScroll.laOgFXFlt,lnPosOrder,1)
  lcOrderSel =Iif(!Empty(laOgFXFlt[lnPosOrder,6]),laOgFXFlt[lnPosOrder,6],'')
  If !Empty(lcOrderSel)
    Select(lcOrderSel)
    Locate
    If !Eof()
      loDBFPikTkt.SetOrder("ORDPIK")
      Scan
        loDBFPikTkt.Seek(&lcOrderSel..Order)
        Select(lcTempPikTkt)
        Scan Rest While Order+PIKTKT = &lcOrderSel..Order
          Scatter Memo Memvar
          Insert Into(lcPickTkTmp) From Memvar
        Endscan
      Endscan
      loDBFPikTkt.SetOrder("PIKTKT")
      lcSelFiles = lcPickTkTmp
      lcSeleCond = ""
      lcSelFlds  = "&lcPickTkTmp..*"
      *--Check if user select warecode
      lnPosWare = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
      If lnPosWare > 0
        lnPosWare = Asubscript(loOgScroll.laOgFXFlt,lnPosWare,1)
        lcWareSel =Iif(!Empty(laOgFXFlt[lnPosWare,6]),laOgFXFlt[lnPosWare,6],'')
        If !Empty(lcWareSel)
          Select(lcWareSel)
          Locate
          If !Eof()
            lcSelFiles = lcSelFiles + "," + lcWareSel
            lcSeleCond = lcSeleCond + Iif(Empty(lcSeleCond),""," AND ") + lcPickTkTmp + ".CWARECODE = " + lcWareSel + ".CWARECODE"
          Endif
        Endif
      Endif
      *--Check if user select accounts
      lnPosAcc = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
      If lnPosAcc > 0
        lnPosAcc = Asubscript(loOgScroll.laOgFXFlt,lnPosAcc,1)
        lcAccSel =Iif(!Empty(laOgFXFlt[lnPosAcc,6]),laOgFXFlt[lnPosAcc,6],'')
        If !Empty(lcAccSel)
          Select(lcAccSel)
          Locate
          If !Eof()
            lcSelFiles = lcSelFiles + "," + lcAccSel
            lcSeleCond = lcSeleCond + Iif(Empty(lcSeleCond),""," AND ") + lcPickTkTmp + ".ACCOUNT = " + lcAccSel + ".ACCOUNT"
          Endif
        Endif
      Endif
      *--Check Date
      lnPosDate = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
      If lnPosDate > 0
        lnPosDate = Asubscript(loOgScroll.laOgFXFlt,lnPosDate,1)
        SDATE = Substr(laOgFXFlt[lnPosDate,6],1,10)
        EDATE = Substr(laOgFXFlt[lnPosDate,6],12,20)


        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        If Substr(laOgFXFlt[lnPosDate,6],1,1) = "|"
          SDATE = Dtoc({})
          EDATE = Substr(laOgFXFlt[lnPosDate,6],2,11)
        Endif
        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

        *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
        *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        If (!Empty(EDATE) And !Empty(SDATE)) Or (!Empty(EDATE) And Empty(SDATE))
          *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

          lcSeleCond = lcSeleCond  + Iif(Empty(lcSeleCond),""," AND ")+ "BETWEEN(&lcPickTkTmp..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
        Endif
      Endif
      Select(lcPickTkTmp)
      Locate
      If !Empty(lcSeleCond)
        Select &lcSelFlds From &lcSelFiles Where &lcSeleCond Into Cursor &lcFnlPikTKT
      Else
        Select &lcSelFlds From &lcSelFiles Into Cursor &lcFnlPikTKT
      Endif
      Select(lcFnlPikTKT)
      =lfGetOrdhdrFile()
      Return
    Endif
  Endif
Endif
*--if user select account,warehous,date
If loDBFPikTkt.llnative
  lcSelectFile = lcTempPikTkt
  lcSelectCond = ""
  lcSelFields = lcTempPikTkt+".*"
  lnPosWare = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
  If lnPosWare > 0
    lnPosWare = Asubscript(loOgScroll.laOgFXFlt,lnPosWare,1)
    lcWareSel =Iif(!Empty(laOgFXFlt[lnPosWare,6]),laOgFXFlt[lnPosWare,6],'')
    If !Empty(lcWareSel)
      Select(lcWareSel)
      Locate
      If !Eof()
        lcSelectFile = lcSelectFile + "," + lcWareSel
        lcSelectCond = lcSelectCond + Iif(Empty(lcSelectCond),""," AND ") + lcTempPikTkt + ".CWARECODE = " + lcWareSel + ".CWARECODE"
      Endif
    Endif
  Endif
  *--Check if user select accounts
  lnPosAcc = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
  If lnPosAcc > 0
    lnPosAcc = Asubscript(loOgScroll.laOgFXFlt,lnPosAcc,1)
    lcAccSel =Iif(!Empty(laOgFXFlt[lnPosAcc,6]),laOgFXFlt[lnPosAcc,6],'')
    If !Empty(lcAccSel)
      Select(lcAccSel)
      Locate
      If !Eof()
        lcSelectFile = lcSelectFile + "," + lcAccSel
        lcSelectCond = lcSelectCond + Iif(Empty(lcSelectCond ),""," AND ") + lcTempPikTkt + ".ACCOUNT = " + lcAccSel + ".ACCOUNT"
      Endif
    Endif
  Endif
  *--Check Date
  lnPosDate = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
  If lnPosDate > 0
    lnPosDate = Asubscript(loOgScroll.laOgFXFlt,lnPosDate,1)
    SDATE = Substr(laOgFXFlt[lnPosDate,6],1,10)
    EDATE = Substr(laOgFXFlt[lnPosDate,6],12,20)

    *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
    If Substr(laOgFXFlt[lnPosDate,6],1,1) = "|"
      SDATE = Dtoc({})
      EDATE = Substr(laOgFXFlt[lnPosDate,6],2,11)
    Endif
    *: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]


    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
    *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
    If (!Empty(EDATE) And !Empty(SDATE)) Or (!Empty(EDATE) And Empty(SDATE))
      **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]
      lcSelectCond = lcSelectCond + Iif(Empty(lcSelectCond ),""," AND ")+ "BETWEEN(&lcTempPikTkt..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
    Endif
  Endif
  *!*	  SELECT(lcPickTkTmp)
  *!*	  LOCATE
  If !Empty(lcSelectCond)
    Select &lcSelFields From &lcSelectFile Where  &lcSelectCond  Into Cursor &lcFnlPikTKT
  Else
    Select &lcSelFields From &lcSelectFile  Into Cursor &lcFnlPikTKT
  Endif
  Select(lcFnlPikTKT)
  =lfGetOrdhdrFile()
  Return
Else
  lcSelectFile = PIKTKT
  lcSelectCond = ""
  lcSelFields = "PikTkt.*"
  lnPosWare = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
  If lnPosWare > 0
    lnPosWare = Asubscript(loOgScroll.laOgFXFlt,lnPosWare,1)
    lcWareSel =Iif(!Empty(laOgFXFlt[lnPosWare,6]),laOgFXFlt[lnPosWare,6],'')
    If !Empty(lcWareSel)
      Select(lcWareSel)
      Locate
      If !Eof()
        lcCurName = lcWareSel
        If !Empty(lcCurName)
          Select &lcCurName
          If (Reccount() > 0)
            lcSQLWare = loOgScroll.gfSQLTempName('','Cwarecode C(10)',lcCurName,'Cwarecode')
            lcSelectFile = lcSelectFile + "," + lcSQLWare
            lcSelectCond =Iif(Empty(lcSelectCond),""," AND ") + "PikTkt.CWARECODE = " + lcSQLWare + ".CWARECODE"
          Endif
        Endif
      Endif
    Endif
  Endif
  *--Check if user select accounts
  lnPosAcc = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
  If lnPosAcc > 0
    lnPosAcc = Asubscript(loOgScroll.laOgFXFlt,lnPosAcc,1)
    lcAccSel =Iif(!Empty(laOgFXFlt[lnPosAcc,6]),laOgFXFlt[lnPosAcc,6],'')
    If !Empty(lcAccSel)
      Select(lcAccSel)
      Locate
      If !Eof()
        lcCurName = lcAccSel
        If !Empty(lcCurName)
          Select &lcCurName
          If (Reccount() > 0)
            lcSQLAcc = loOgScroll.gfSQLTempName('','Account C(5)',lcCurName,'Account')
            lcSelectFile = lcSelectFile + "," + lcSQLAcc
            lcSelectCond = Iif(Empty(lcSelectCond ),""," AND ") + "PikTkt.ACCOUNT = " + lcSQLAcc + ".ACCOUNT"
          Endif
        Endif
      Endif
    Endif
  Endif
  *--Check Date
  lnPosDate = Ascan(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
  If lnPosDate > 0
    lnPosDate = Asubscript(loOgScroll.laOgFXFlt,lnPosDate,1)
    SDATE = Substr(laOgFXFlt[lnPosDate,6],1,10)
    EDATE = Substr(laOgFXFlt[lnPosDate,6],12,20)

    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
    If Substr(laOgFXFlt[lnPosDate,6],1,1) = "|"
      SDATE = Dtoc({})
      EDATE = Substr(laOgFXFlt[lnPosDate,6],2,11)
    Endif
    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

    **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [Start]
    *IF !EMPTY(EDATE) AND !EMPTY(SDATE)
    If (!Empty(EDATE) And !Empty(SDATE)) Or (!Empty(EDATE) And Empty(SDATE))
      **: B608620,1 MMT 07/20/2008 Fix bug if date range with lower date value [End]

      lcSelectCond = Iif(Empty(lcSelectCond ),""," AND ")+ " PikTkt..DATE BETWEEN "+Ctod(SDATE)+" AND "+Ctod(EDATE)
    Endif
  Endif
  =lfOpenSql(lcSelFields ,lcSelectFile  ,lcFnlPikTKT,lcSelectCond)
  Select(lcFnlPikTKT)
  =lfGetOrdhdrFile()
  Return
Endif
*------------------------
*---------------------------------
****************************************************************
*!*  PRIVATE  lcPikTktSel,lcOrderSel,lcAccSel,lcWareSel
*!*  lcWherePik = "Piktkt.Piktkt # '******' .AND. !EMPTY(Piktkt.Piktkt)"
*!*  lcTablePik =  "piktkt"
*!*  *--Getting the file which Contains the selected picking tickets
*!*  lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
*!*  IF lnPosDate > 0
*!*    lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
*!*    SDATE = SUBSTR(laOgFxFlt[lnPosDate,6],1,10)
*!*    EDATE = SUBSTR(laOgFxFlt[lnPosDate,6],12,20)
*!*    IF !EMPTY(EDATE) AND !EMPTY(SDATE)
*!*      lcWherePik = lcWherePik+IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+"BETWEEN(PIKTKT.DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
*!*    ENDIF
*!*  ENDIF
*!*  lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
*!*  IF lnPosPikTkt > 0
*!*    lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
*!*    lcPikTktSel =IIF(!EMPTY(laOgFxFlt[lnPosPikTkt,6]),laOgFxFlt[lnPosPikTkt,6],'')
*!*    IF !EMPTY(lcPikTktSel)
*!*      SELECT(lcPikTktSel)
*!*      LOCATE
*!*      IF !EOF()
*!*  	  COPY TO oariaapplication.workdir+lcTempPiktk +".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempPiktk +".dbf'"
*!*  	  lcWherePik = lcWherePik+IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.PIKTKT = "+lcTempPiktk +".PIKTKT"
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF
*!*  *--Getting the file which Contains the selected orders
*!*  lnPosOrder = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ORDER")
*!*  IF lnPosOrder > 0
*!*    lnPosOrder = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosOrder,1)
*!*    lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnPosOrder,6]),laOgFxFlt[lnPosOrder,6],'')
*!*    IF !EMPTY(lcOrderSel)
*!*      SELECT(lcOrderSel)
*!*      LOCATE
*!*      IF !EOF()

*!*  	  COPY TO oariaapplication.workdir+lcTempOrder +".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempOrder +".dbf'"
*!*  	  lcWherePik = lcWherePik +IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.ORDER = "+lcTempOrder +".ORDER"
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF
*!*  *--Getting the file which Contains the selected warehouses
*!*  lnPosWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
*!*  IF lnPosWare > 0
*!*    lnPosWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosWare,1)
*!*    lcWareSel =IIF(!EMPTY(laOgFxFlt[lnPosWare,6]),laOgFxFlt[lnPosWare,6],'')
*!*    IF !EMPTY(lcWareSel)
*!*      SELECT(lcWareSel)
*!*      LOCATE
*!*      IF !EOF()

*!*  	  COPY TO oariaapplication.workdir+lcTempWare+".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempWare+".dbf'"
*!*  	  lcWherePik = lcWherePik +IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.cwarecode = "+lcTempWare +".cwarecode"
*!*      ENDIF
*!*      ENDIF
*!*  ENDIF
*!*  *--Getting the file which Contains the selected accounts
*!*  lnPosAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
*!*  IF lnPosAcc > 0
*!*    lnPosAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosAcc,1)
*!*    lcAccSel =IIF(!EMPTY(laOgFxFlt[lnPosAcc,6]),laOgFxFlt[lnPosAcc,6],'')
*!*    IF !EMPTY(lcAccSel)
*!*      SELECT(lcAccSel)
*!*      LOCATE
*!*      IF !EOF()
*!*
*!*  	  COPY TO oariaapplication.workdir+lcTempacc+".dbf"
*!*        lcTablePik = lcTablePik  +",'"+oariaapplication.workdir+lcTempacc+".dbf'"
*!*  	  lcWherePik = lcWherePik + IIF(!EMPTY(lcWherePik),' AND  ' ,"" )+ "PIKTKT.account = "+lcTempacc +".account"
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF

*!*  lcSlctFldSPik = "piktkt.*"
*!*  =lfOpenFox(lcSlctFldSPik,lcTablePik ,lcPickTkTmp,lcWherePik)
*!*  *lcTempTktPick = loOGScroll.gfTempName()
*!*  SELECT(lcPickTkTmp)
*!*  COPY TO oariaapplication.workdir+lcTempTktPick+".dbf"
*!*  lcSeleOrdfld = "Ordhdr.ACCOUNT,ORDHDR.ORDER,ORDHDR.CORDTYPE,ORDHDR.cCurrCode"
*!*  lcSelordcon = "ordhdr.cordtype+ordhdr.order ='O'+"+lcTempTktPick+".ORDER "
*!*  lcSelTable = "ordhdr,'"+oariaapplication.workdir+lcTempTktPick+".dbf'"
*!*  =lfOpenFox(lcSeleOrdfld ,lcSelTable ,lcTempOrdHdr,lcSelordcon )

*-- end of lpColect.

*!**************************************************************************
*! Name      : lfAddField
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Add fields to the the array of file structer
*!**************************************************************************
*! Example   : =lfAddField()
*!**************************************************************************
*!E301439,1

Function lfAddField
Parameters lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec
lnFldPos  = Alen(&lcStruArry,1) + 1
Dimension &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec
Store ' ' To  &lcStruArry[lnFldPos,7],&lcStruArry[lnFldPos,8],;
  &lcStruArry[lnFldPos,9],&lcStruArry[lnFldPos,10],;
  &lcStruArry[lnFldPos,11],&lcStruArry[lnFldPos,12],;
  &lcStruArry[lnFldPos,13],&lcStruArry[lnFldPos,14],;
  &lcStruArry[lnFldPos,15],&lcStruArry[lnFldPos,16]
Store 0 To    &lcStruArry[lnFldPos,17] ,&lcStruArry[lnFldPos,18]

*-- end of lfAddField.


*!*************************************************************
*! Name      : lfvRepCurr
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   :
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRepCurr()
*!*************************************************************
Function lfvRepCurr

llOGFltCh = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfOpenFox
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
Function lfOpenFox
Lparameters lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial

Local lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
Private laIndex
Dimension laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + Iif(Type('lcWhereCond') = 'C' And !Empty(lcWhereCond)," WHERE " + lcWhereCond ,"")


Local lnConNum
lnConNum = 0
lnConnectionHandlar = loOgScroll.oRDA.SqlRun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.cAriaNativeDataFilesConStr,3,;
  'SAVE',Set("DATASESSION"),.F.,@lnConNum)
If lnConnectionHandlar = 1
  lnBuffering = CursorGetProp("Buffering",lcCursor)
  =CursorSetProp("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  Select (lcCursor)
  For lnI = 1 To Alen(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    Index On &lcIndex. Tag (lcTag) &&OF (lcCursor)
  Endfor
  lcTag = laIndex[1,2]
  Set Order To Tag (lcTag)
  =CursorSetProp("Buffering",5,lcCursor)
  * loOGScroll.oRDA.oConnectionsClass.Close(lnConNum)
Else
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  *  loOGScroll.oRDA.oConnectionsClass.Close(lnConNum)
  Return .F.
Endif

*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
Function lfCrtindex

Lparameters lcTable
Do Case

  *--temp. Customer File
Case Upper(lcTable) = lcTempCustomer
  Dimension laIndex[1,2]
  laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
  laIndex[1,2] = lcTempCustomer

  *--temp. ordhdr file
Case Upper(lcTable) =  lcTempOrdHdr
  Dimension laIndex[1,2]
  laIndex[1,1] = 'CORDTYPE+ORDER'
  laIndex[1,2] = lcTempOrdHdr

  *--temp. ordline file
Case Upper(lcTable) =  lcTempOrdline
  Dimension laIndex[1,2]

  laIndex[1,1] = 'CORDTYPE+ORDER+STORE'
  laIndex[1,2] = lcTempOrdline

  *-- temp. piktkt file lcPickTkTmp
Case Upper(lcTable) = lcPickTkTmp
  Dimension laIndex[4,2]
  laIndex[1,1] = 'PIKTKT'
  laIndex[1,2] = lcPickTkTmp
  laIndex[2,1] = 'ORDER'
  laIndex[2,2] = lcIndOrd
  laIndex[3,1] = 'CWARECODE'
  laIndex[3,2] = lcIndWare
  laIndex[4,1] = 'ACCOUNT'
  laIndex[4,2] = lcIndAcc

  *--temp. pikline file
Case Upper(lcTable) =  lcTempPikLin
  Dimension laIndex[1,2]
  laIndex[1,1] = 'piktkt'
  laIndex[1,2] = lcTempPikLin


Endcase
*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/27/04
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
Function lfCollTime
Parameters lcStart,lcEnd
lnStHour  = Iif(Val(Left(lcStart,2)) = 0,Val(Left(lcStart,2))+24,Val(Left(lcStart,2)))
lnEndHour = Iif(Val(Left(lcEnd,2))   = 0,Val(Left(lcEnd,2))  +24,Val(Left(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * Val(Substr(lcStart,4,2)) + Val(Right(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * Val(Substr(lcEnd,4,2))   + Val(Right(lcEnd,2))
Return (lnEnd - lnStart)
*-- END OF lfCollTime.
*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to make index on a temp. file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
Function lfMakeIndex
Parameters lcTempName
Private laIndex
Dimension laIndex[1,2]

lcCursor = lcTempName
lnBuffering = CursorGetProp("Buffering",lcCursor)
=CursorSetProp("Buffering",3,lcCursor)
*-- To initialize the indecis that will be created for each file
=lfCrtindex(lcCursor)
Select (lcCursor)
For lnI = 1 To Alen(laIndex,1)
  lcIndex = laIndex[lnI,1]
  lcTag   = laIndex[lnI,2]
  Index On &lcIndex. Tag (lcTag) &&OF (lcCursor)
Endfor
lcTag = laIndex[1,2]
Set Order To Tag (lcTag)

*!*************************************************************
*! Name      : lfGetOrdhdrFile
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
*! Purpose   : function to get Orhdr Data
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
Function lfGetOrdhdrFile

lcScanExp = "IIF(lcRpInv = 'B',IIF(llRPRelPT,.T.,&lcFnlPikTKT..Status $ 'OCHP'),"
lcScanExp = lcScanExp +" IIF(lcRpInv = 'Y' ,  &lcFnlPikTKT..Status$'C',"
lcScanExp = lcScanExp +"IIF(llRPRelPT,&lcFnlPikTKT..Status $ 'OHPX',&lcFnlPikTKT..Status $ 'OHP' ))) =  .T.  AND"
lcScanExp = lcScanExp +" IIF(lcRpPrint='B',.T.,"
lcScanExp = lcScanExp +"IIF(lcRpPrint='Y' , &lcFnlPikTKT..PrtFlag= 'P',&lcFnlPikTKT..PrtFlag<>'P')) =  .T."

Select(lcFnlPikTKT)
Locate
Scan
  *loDBFOrdhdr   = CreateObject("RemoteTable","Ordhdr","Ordhdr",lcTempOrdhd,SET("DATASESSION"))
  loDBFOrdhdr.Seek("O"+&lcFnlPikTKT..Order)
  Select(lcTempOrdhd)
  *: E303473,1 MMT 05/13/2014 Add complete and start dates to Picking ticket log report[Start]
  *SCATTER FIELDS ACCOUNT,ORDER,CORDTYPE,cCurrCode,Nexrate,NcurrUnit,SHIPVIA MEMO MEMVAR
  Scatter Fields ACCOUNT,Order,CORDTYPE,cCurrCode,Nexrate,NcurrUnit,SHIPVIA,Start,Complete Memo Memvar
  *: E303473,1 MMT 05/13/2014 Add complete and start dates to Picking ticket log report[End]
  Insert Into (lcTempOrdHdr) From Memvar
Endscan
*--get Customer file
Select(lcFnlPikTKT)
Locate
Scan
  If Empty(&lcFnlPikTKT..Store)
    loDBFCust.Seek("M"+&lcFnlPikTKT..ACCOUNT+&lcFnlPikTKT..Store)
  Else
    loDBFCust.Seek("S"+&lcFnlPikTKT..ACCOUNT+&lcFnlPikTKT..Store)
  Endif
  Select(lcTempCust)
  Scatter Fields Type,ACCOUNT,Store,BTNAME Memo Memvar
  Insert Into (lcTempCustomer) From Memvar
Endscan
   
Select(lcFnlPikTKT)
Set Relation To 'O' + Order Into &lcTempOrdHdr Additive



* B611514 ,1 HMS , 2/12/2018 - Strangeness in the Pick Ticket Log - company J1 [T20171130.0022][Begin]
  llHasPack_List = .F.
  If lcRPPCKLST  <> 'B'
    llHasPack_List  = Iif(lcRPPCKLST = 'Y',.T.,.F.)
ENDIF 

* B611514 ,1 HMS , 2/12/2018 - Strangeness in the Pick Ticket Log - company J1 [T20171130.0022][END]

Select(lcFnlPikTKT)
Scan For &lcScanExp And PIKTKT # '******' .And. !Empty(PIKTKT)
  Scatter Memvar Memo
  m.cCurrCode = &lcTempOrdHdr..cCurrCode
  m.SHIPVIA=   Substr(gfCodDes(Eval(lcTempOrdHdr+'.SHIPVIA'),'SHIPVIA'),1,20)
  
* B611514 ,1 HMS , 2/12/2018 - Strangeness in the Pick Ticket Log - company J1 [T20171130.0022][Begin]
    IF lcRPPCKLST  <> 'B' AND ((llHasPack_List And !loPack_Hdr.Seek(&lcFnlPikTKT..PIKTKT)) Or (!llHasPack_List And loPack_Hdr.Seek(&lcFnlPikTKT..PIKTKT)))
      Loop
    Endif
* B611514 ,1 HMS , 2/12/2018 - Strangeness in the Pick Ticket Log - company J1 [T20171130.0022][END]
    Insert Into (lcPickTmp) From Memvar
  Endscan
  Set Relation To
  *!*************************************************************
  *! Name      : lfOpenSql
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 09/08/2004
  *! Purpose   : function to open SQL tables
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
Function lfOpenSql

Lparameters lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
Local lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
Private laIndex
Dimension laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + Iif(Type('lcWhereCond') = 'C' And !Empty(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOgScroll.oRDA.SqlRun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
  'BROWSE',Set("DATASESSION"))

If lnConnectionHandlar = 1
  lnBuffering = CursorGetProp("Buffering",lcCursor)
  =CursorSetProp("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  Select (lcCursor)
  For lnI = 1 To Alen(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    Index On &lcIndex. Tag (lcTag) &&OF (lcCursor)
  Endfor
  lcTag = laIndex[1,2]
  Set Order To Tag (lcTag)

Else
  =loOgScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  Return .F.
Endif
*-- end of lfOpenSql.
