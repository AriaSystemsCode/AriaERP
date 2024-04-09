****************************************************************************************
*! Program   : SOIMPCS.PRG
*! Developer : Mohamedhamdy
*! Date      : 21/11/2017
*! Purpose   : Import Sales Order from Excel sheet for glf
*! Entry#    : C202093 [P20170913.0001]
****************************************************************************************
*! Modifications:
*B611222,1 MMT 10/27/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161025.0008]
*C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045]
*B611253,1 MMT 1/23/2017 Issue#5: Import SO from CSV program rounds style price[P20161109.0001]
*B611273,MHM 26/02/2017 Modify the custom SO importing program for ERI02 to update BULK field with value N [T20170209.0019]
****************************************************************************************
Do Form (oAriaApplication.ClientScreenHome+"SO\SOIMPCSV.scx")
****************************************************************************************
*! Name      : lfGetFile
*! Developer : Mohamedhamdy
*! Date      : 21/11/2017
*! Purpose   : Get File
****************************************************************************************
Function lfGetFile
Parameters loFormSet
lcFile = Getfile('XLS','Browse a .XLS file:','Import',0,'Select File to be Imported')
loFormSet.AriaForm1.txtFileName.Value = lcFile

****************************************************************************************
*! Name      : lfProceed
*! Developer : Mohamedhamdy
*! Date      : 10/18/2017
*! Purpose   : Proceed after file selection
****************************************************************************************
Function lfProceed
Parameters loFormSet
If Empty(Alltrim(loFormSet.AriaForm1.txtFileName.Value)) Or (!Empty(Alltrim(loFormSet.AriaForm1.txtFileName.Value)) And !File(Alltrim(loFormSet.AriaForm1.txtFileName.Value)))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid file')
  Return .F.
Endif

If Upper(Justext(Alltrim(loFormSet.AriaForm1.txtFileName.Value))) <> 'XLS'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid file format')
  Return .F.
Endif
Create Cursor TMPSTR (mStrRep M(10) ,Type C(15),Description C(30),Current C(30),newvalue C(30),CustomerPo C(15), Account C(5),storee C(8))
Set Step On
lcSOTmp = gfTempName()
Create Cursor (lcSOTmp) ;
  ( cvensty C (19) ,Style C (19),Desc C (20), ccolor C(20),CCLRDESC C(30),CSTYGROUPP C (6),ROYALTYY C (6),nsugretpr N (12),Image C(5),;
  Ats1 N(7), Ats2 N(7), Ats3 N(7), Ats4 N(7), Ats5 N(7), Ats6 N(7), Ats7 N(7), Ats8 N(7), Ats9 C(7), Ats10 C(7), Ats11 C(7),;
  Ats12 C(7), Ats13 C(7), Ats14 C(7), Ats15 C(7), Ats16 C(7), Ats17 C(7), Ats18 C(7), Ats19 C(7), Ats20 C(7), Ats21 C(7), Ats22 C(7),;
  Ats23 C(7), Ats24 C(7), Ats25 C(7), Ats26 C(7), Ats27 C(7), Ats28 C(7), Ats29 C(7), Ats30 C(7), Ats31 C(7), Ats32 C(7),;
  Atstotal N(10),TotalAmount N(15),;
  DATE C(8),Time C(8),locationn C (8), CSTNAM C(30),storee C(8),CustomerPo C(15),StartDate C(20), CompleteDate C(20),Terms C(30),SHIPVIA C(30),salesrep1 C(15),salesrep2 C(15),;
  Ordered1 C(7),Ordered2 C(7),Ordered3 C(7),Ordered4 C(7),Ordered5 C(7),Ordered6 C(7),Ordered7 C(7),Ordered8 C(7),;
  Ordered9 C(7),Ordered10 C(7),Ordered11 C(7),Ordered12 C(7),Ordered13 C(7),Ordered14 C(7),Ordered15 C(7),;
  Ordered16 C(7),Ordered17 C(7),Ordered18 C(7),Ordered19 C(7),Ordered20 C(7),Ordered21 C(7),Ordered22 C(7),Ordered23 C(7),;
  Ordered24 C(7),Ordered25 C(7),Ordered26 C(7),Ordered27 C(7),Ordered28 C(7),Ordered29 C(7),Ordered30 C(7),Ordered31 C(7),;
  Ordered32 C(7),DiscouCt C(15),DiscouCtCost C(30), season C(6), cdivision C(6),qty1 N(6),price N(12,2),caddress1 C(30), ;
  caddress2 C(30), caddress3 C(30), caddress4 C(30), caddress5 C(30),Termss C(30),SHIPVIAA C(30))
lnerror=0
Try
  Append From (loFormSet.AriaForm1.txtFileName.Value) Type Xls
Catch To lnerror
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'"No lines to import"')
  Return .F.
Endtry
lcSOCSVTmp = gfTempName()
Create Cursor (lcSOCSVTmp) ;
  ( cvensty C (19) ,Style C (19),Desc C (20), ccolor C(20),CCLRDESC C(30),CSTYGROUPP C (6),ROYALTYY C (6),nsugretpr N (12),Image C(5),;
  Ats1 N(7), Ats2 N(7), Ats3 N(7), Ats4 N(7), Ats5 N(7), Ats6 N(7), Ats7 N(7), Ats8 N(7), Ats9 N(7), Ats10 N(7), Ats11 N(7),;
  Ats12 N(7), Ats13 N(7), Ats14 N(7), Ats15 N(7), Ats16 N(7), Ats17 N(7), Ats18 N(7), Ats19 N(7), Ats20 N(7), Ats21 N(7), Ats22 N(7),;
  Ats23 N(7), Ats24 N(7), Ats25 N(7), Ats26 N(7), Ats27 N(7), Ats28 N(7), Ats29 N(7), Ats30 N(7), Ats31 N(7), Ats32 N(7),;
  Atstotal N(10),TotalAmount N(15),;
  DATE C(8),Time C(8),locationn C (8), CSTNAM C(30),storee C(8),CustomerPo C(15),StartDate C(20), CompleteDate C(20),Terms C(30),SHIPVIA C(30),salesrep1 C(15),salesrep2 C(15),;
  Ordered1 N(7),Ordered2 N(7),Ordered3 N(7),Ordered4 N(7),Ordered5 N(7),Ordered6 N(7),Ordered7 N(7),Ordered8 N(7),;
  Ordered9 N(7),Ordered10 N(7),Ordered11 N(7),Ordered12 N(7),Ordered13 N(7),Ordered14 N(7),Ordered15 N(7),;
  Ordered16 N(7),Ordered17 N(7),Ordered18 N(7),Ordered19 N(7),Ordered20 N(7),Ordered21 N(7),Ordered22 N(7),Ordered23 N(7),;
  Ordered24 N(7),Ordered25 N(7),Ordered26 N(7),Ordered27 N(7),Ordered28 N(7),Ordered29 N(7),Ordered30 N(7),Ordered31 N(7),;
  Ordered32 N(7),Discount C(15),DiscountCost C(30), season C(6), cdivision C(6),qty1 N(6),price N(12,2),caddress1 C(30), ;
  caddress2 C(30), caddress3 C(30), caddress4 C(30), caddress5 C(30),Termss C(30),SHIPVIAA C(30))

*B611253,1 MMT 1/23/2017 Issue#5: Import SO from CSV program rounds style price[P20161109.0001][End]
*C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][End]

Select(lcSOCSVTmp)
Index On CustomerPo + CSTNAM + storee Tag(lcSOCSVTmp)

Select (lcSOTmp)
Scan
  Scatter Memvar Memo
  For i=1 To 32
    lcstr= Alltrim(Str(i))
    If !Empty((Alltrim(m.ordered&lcstr)))
      m.ordered&lcstr = Val(Alltrim(m.ordered&lcstr))
    Else
      m.ordered&lcstr=0
    Endif

  Endfor
  Select (lcSOCSVTmp)
  Append Blank
  Gather Memvar Memo
Endscan
lnerror=0
Try
  Append From (loFormSet.AriaForm1.txtFileName.Value) Type Xls
Catch To lnerror
  Wait Wind lnerror
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'"No lines to import"')
Endtry

*B611222,1 MMT 10/27/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161025.0008][Start]
Select(lcSOCSVTmp)
Delete For Alltrim(cvensty)=Alltrim("Vendor Style")
Delete For Empty(Alltrim(CustomerPo ))
*-----Validations-------*
Select Distinct CustomerPo,storee,CSTNAM From (lcSOCSVTmp) Where !Deleted() Into Cursor 'AccTmp'
If !Used('CUSTOMER')
  =gfOpenTable("Customer","Customer")
Endif
Select 'AccTmp'
Locate
Scan
  If !gfSeek('M'+ Alltrim(AccTmp.CSTNAM),'Customer','Customer') Or Empty(Alltrim(AccTmp.CSTNAM))
    Select TMPSTR
    Locate
    Append Blank
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(AccTmp.CustomerPo)+", CSTNAM: "+ AccTmp.CSTNAM+" is not found in Customer file"+Chr(13)+Chr(10)
    Replace Type With  'Customer'
    Replace Description  With "is not found in Customer file"+Chr(13)+Chr(10)
    Replace Current With  Alltrim(AccTmp.CSTNAM)
    Replace CustomerPo With Alltrim(AccTmp.CustomerPo)
  Endif
Endscan

Select Distinct CustomerPo,CSTNAM,storee From (lcSOCSVTmp) Where !Deleted() Into Cursor 'storeeTmp'
Select 'storeeTmp'
Locate
Scan
  lcaccount = "     "
  If  gfSeek('M'+ Alltrim(storeeTmp.CSTNAM),'Customer','Customer')
    lcaccount = Customer.Account

    If  !gfSeek('S'+ Alltrim(storeeTmp.CSTNAM),'Customer','Customer')
      Exit
    Endif

  Endif
  If !gfSeek('S'+ lcaccount +storeeTmp.storee,'Customer','Customer')

    Select TMPSTR
    Locate
    Append Blank
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(AccTmp.CustomerPo)+", CSTNAM: "+ AccTmp.CSTNAM+" is not found in Customer file"+Chr(13)+Chr(10)
    Replace Type With  'store'
    Replace Description  With "is not found in Customer file"+Chr(13)+Chr(10)
    Replace Current With  Alltrim(storeeTmp.storee)
    Replace CustomerPo    With Alltrim(storeeTmp.CustomerPo)
    Replace storee  With Alltrim(storeeTmp.storee)
    Replace Account With lcaccount
  Endif
Endscan

Select Distinct CustomerPo,SHIPVIA From (lcSOCSVTmp) Where !Deleted()  Into Cursor 'Shiptmp2'
Select ShipTmp2
Select Distinct CustomerPo,Max(SHIPVIA) As SHIPVIA , Count(*) As counter From 'Shiptmp2' Where !Deleted()  Group By CustomerPo Into Cursor 'Shiptmp'
Select Shiptmp
Locate
Scan
  If counter > 1
    Select TMPSTR
    Locate
    Append Blank
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(Shiptmp.CustomerPo)+", ShipVia: "+ Alltrim(Shiptmp.SHIPVIA)+" is not found in Codes file"+Chr(13)+Chr(10)
    Replace Type With  'ShipVia'
    Replace Description  With " is not repeated in Codes file"+Chr(13)+Chr(10)
    Replace Current With  Alltrim(Shiptmp.SHIPVIA)
    Replace CustomerPo With Alltrim(Shiptmp.CustomerPo)
  Else
    lcCodSh = lfGeCodeNO(Alltrim(Shiptmp.SHIPVIA),'SHIPVIA')
    If Empty(Alltrim(lcCodSh)) Or Empty(Alltrim(Shiptmp.SHIPVIA))
      Select TMPSTR
      Locate
      Append Blank
      Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(Shiptmp.CustomerPo)+", ShipVia: "+ Alltrim(Shiptmp.SHIPVIA)+" is not found in Codes file"+Chr(13)+Chr(10)
      Replace Type With  'ShipVia'
      Replace Description  With " is not found in Codes file"+Chr(13)+Chr(10)
      Replace Current With  Alltrim(Shiptmp.SHIPVIA)
      Replace CustomerPo With Alltrim(Shiptmp.CustomerPo)
    Endif
  Endif
Endscan

Select Distinct CustomerPo,Terms  From (lcSOCSVTmp) Where !Deleted()  Into Cursor 'termsTmp2'
Select termsTmp2
Select Distinct CustomerPo,Max(Terms) As Terms , Count(*) As counter  From 'termsTmp2' Where !Deleted()  Group By CustomerPo Into Cursor 'termsTmp'
Select termsTmp
Locate
Scan
  If counter > 1
    Select TMPSTR
    Locate
    Append Blank
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(termsTmp.CustomerPo)+", Term Code: "+ Alltrim(termsTmp.Terms)+" is not found in Codes file"+Chr(13)+Chr(10)
    Replace Type With  'Terms'
    Replace Description  With "is repeated found in Codes file"+Chr(13)+Chr(10)
    Replace Current With  Alltrim(termsTmp.Terms)
    Replace CustomerPo With Alltrim(termsTmp.CustomerPo)
  Else
    lcCodSh = lfGeCodeNO(Alltrim(termsTmp.Terms) ,'CTERMCODE')
    If Empty(Alltrim(lcCodSh)) Or Empty(Alltrim(termsTmp.Terms))
      Select TMPSTR
      Locate
      Append Blank
      Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(termsTmp.CustomerPo)+", Term Code: "+ Alltrim(termsTmp.Terms)+" is not found in Codes file"+Chr(13)+Chr(10)
      Replace Type With  'Terms'
      Replace Description  With " is not found in Codes file"+Chr(13)+Chr(10)
      Replace Current With  Alltrim(termsTmp.Terms)
      Replace CustomerPo With Alltrim(termsTmp.CustomerPo)
    Endif
  Endif

Endscan

Select Distinct CustomerPo,locationn From (lcSOCSVTmp) Where !Deleted() Into Cursor 'locationnTmp'
If !Used('WAREHOUS')
  =gfOpenTable("Warehous","Warehous")
Endif
Select 'locationnTmp'
Locate
Scan
  If !gfSeek(Alltrim(locationnTmp.locationn),'Warehous','Warehous') Or Empty(Alltrim(locationnTmp.locationn))
    Select TMPSTR
    Locate
    Append Blank
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(locationnTmp.CustomerPo)+", Warehouse: "+ locationnTmp.locationn +" is not found in Warehouse file"+Chr(13)+Chr(10)
    Replace Type With  'Location'
    Replace Description  With "is not found in Warehouse file"+Chr(13)+Chr(10)
    Replace Current With  Alltrim(locationnTmp.locationn)
    Replace CustomerPo With Alltrim(locationnTmp.CustomerPo)
  Endif
Endscan

Select Distinct CustomerPo, salesrep1  From (lcSOCSVTmp) Where !Deleted() Into Cursor 'salesrepTmp1'
If !Used('SALESREP')
  =gfOpenTable("SALESREP","SALESREP")
Endif
Select 'salesrepTmp1'
Select Distinct CustomerPo, Max(salesrep1) As salesrep1,Count(*) As counter   From 'salesrepTmp1' Where !Deleted() Group By CustomerPo Into Cursor 'salesrepTmp2'
Select salesrepTmp2
Locate
Scan
  If counter > 1
    Select TMPSTR
    Locate
    Append Blank
    Replace mStrRep     With mStrRep +"CustomerPo#:"+Alltrim(salesrepTmp2.CustomerPo)+", Sales Rep: "+ salesrepTmp2.salesrep1 +" is not found in Sales Rep file"+Chr(13)+Chr(10)
    Replace Type        With 'SalesRep1'
    Replace Description With "is not found in salesrep file"+Chr(13)+Chr(10)
    Replace Current     With Alltrim(salesrepTmp2.salesrep1)
    Replace CustomerPo  With Alltrim(salesrepTmp2.CustomerPo)
  Else
    If !gfSeek(Alltrim(salesrepTmp2.salesrep1),'SALESREP','SALESREP')
      Select TMPSTR
      Locate
      Append Blank
      Replace mStrRep     With mStrRep +"CustomerPo#:"+Alltrim(salesrepTmp2.CustomerPo)+", Sales Rep: "+ salesrepTmp2.salesrep1 +" is not found in Sales Rep file"+Chr(13)+Chr(10)
      Replace Type        With 'SalesRep1'
      Replace Description With "is not found in salesrep file"+Chr(13)+Chr(10)
      Replace Current     With Alltrim(salesrepTmp2.salesrep1)
      Replace CustomerPo  With Alltrim(salesrepTmp2.CustomerPo)
    Endif
  Endif
Endscan
Select Distinct CustomerPo, salesrep2  From (lcSOCSVTmp) Where !Deleted() Into Cursor 'salesrepTmp3'
If !Used('SALESREP')
  =gfOpenTable("SALESREP","SALESREP")
Endif
Select salesrepTmp3
Select Distinct CustomerPo, Max(salesrep2) As salesrep2,Count(*) As counter   From 'salesrepTmp3' Where !Deleted() Group By CustomerPo Into Cursor 'salesrepTmp4'
Select salesrepTmp4
Locate
Scan
  If counter > 1
    Select TMPSTR
    Locate
    Append Blank
    Replace mStrRep     With mStrRep +"CustomerPo#:"+Alltrim(salesrepTmp4.CustomerPo)+", Sales Rep: "+ salesrepTmp4.salesrep2 +" is not found in Sales Rep file"+Chr(13)+Chr(10)
    Replace Type        With 'SalesRep2'
    Replace Description With "is not found in salesrep file"+Chr(13)+Chr(10)
    Replace Current     With Alltrim(salesrepTmp4.salesrep2)
    Replace CustomerPo  With Alltrim(salesrepTmp4.CustomerPo)
  Else
    If !gfSeek(Alltrim(salesrepTmp4.salesrep2),'SALESREP','SALESREP')
      Select TMPSTR
      Locate
      Append Blank
      Replace mStrRep     With mStrRep +"CustomerPo#:"+Alltrim(salesrepTmp4.CustomerPo)+", Sales Rep: "+ salesrepTmp4.salesrep2 +" is not found in Sales Rep file"+Chr(13)+Chr(10)
      Replace Type        With 'SalesRep2'
      Replace Description With "is not found in salesrep file"+Chr(13)+Chr(10)
      Replace Current     With Alltrim(salesrepTmp4.salesrep2)
      Replace CustomerPo  With Alltrim(salesrepTmp4.CustomerPo)
    Endif
  Endif
Endscan

Select Distinct CustomerPo, Style From (lcSOCSVTmp) Where !Deleted() Into Cursor 'StyleTmp'
If !Used('STYLE')
  =gfOpenTable("style","style")
Endif
Select 'StyleTmp'
Locate
Scan
  If !gfSeek(Alltrim(StyleTmp.Style),'style','style')
    Select TMPSTR
    Locate
    Locate For Empty(Alltrim(Type))
    If Eof()
      Append Blank
    Endif
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(StyleTmp.CustomerPo)+", Style: "+ StyleTmp.Style +" is not found in Style file"+Chr(13)+Chr(10)
  Endif
Endscan

Select Distinct CustomerPo,season From (lcSOCSVTmp) Where !Deleted() And !Empty(season) Into Cursor 'SeasonTmp'
Select 'SeasonTmp'
Locate
If !Eof()
  Select 'SeasonTmp'
  Scan
    lcCodSh = lfGeCodeNO(Alltrim(SeasonTmp.season) ,'SEASON')
    If Empty(Alltrim(lcCodSh))
      Select TMPSTR
      Locate
      Locate For Empty(Alltrim(Type))
      If Eof()
        Append Blank
      Endif
      Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(SeasonTmp.CustomerPo)+", Season: "+ Alltrim(SeasonTmp.season)+" is not found in Codes file"+Chr(13)+Chr(10)
    Endif
  Endscan
Endif

Select Distinct CustomerPo,cdivision From (lcSOCSVTmp) Where !Deleted() And !Empty(cdivision) Into Cursor 'CDIVSIONTmp'
Select 'CDIVSIONTmp'
Locate
If !Eof()
  Select 'CDIVSIONTmp'
  Scan
    lcCodSh = lfGeCodeNO(Alltrim(CDIVSIONTmp.cdivision),'CDIVISION')
    If Empty(Alltrim(lcCodSh))
      Select TMPSTR
      Locate
      Locate For Empty(Alltrim(Type))
      If Eof()
        Append Blank
      Endif
      Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(CDIVSIONTmp.CustomerPo)+", Division: "+ Alltrim(CDIVSIONTmp.cdivision)+" is not found in Codes file"+Chr(13)+Chr(10)
    Endif
  Endscan
Endif

Select Distinct CustomerPo,StartDate From (lcSOCSVTmp) Where !Deleted() And Empty(Alltrim(StartDate)) Into Cursor 'Dattemp'
If _Tally > 0
  Select Dattemp
  Scan
    Select TMPSTR
    Locate
    Locate For Empty(Alltrim(Type))
    If Eof()
      Append Blank
    Endif
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(Dattemp.CustomerPo)+", Missing Start Date."+Chr(13)+Chr(10)
  Endscan
Endif
Select Distinct CustomerPo,CompleteDate From (lcSOCSVTmp) Where !Deleted() And Empty(Alltrim(CompleteDate)) Into Cursor 'DatetempComp'
If _Tally > 0
  Select DatetempComp
  Scan
    Select TMPSTR
    Locate
    Locate For Empty(Alltrim(Type))
    If Eof()
      Append Blank
    Endif
    Replace mStrRep With mStrRep +"CustomerPo#:"+Alltrim(DatetempComp.CustomerPo)+", Missing Complete Date."+ Chr(13)+Chr(10)
  Endscan
Endif

*------ Call form ------------
Private llcontinue
llcontinue = .F.
Select TMPSTR
*  _screen.Visible = .t.

Locate For !Empty(Alltrim(Type))
If Found() And !Eof()
  Do Form (oAriaApplication.ClientScreenHome+"SO\form1.scx")
  If llcontinue = .F.
    Return .F.
  Endif
Endif
Select TMPSTR
Locate
Scan
  Select  (lcSOCSVTmp)
  Locate
  If Alltrim(TMPSTR.Type)='Customer'
    Replace &lcSOCSVTmp..CSTNAM With TMPSTR.newvalue For Alltrim(CustomerPo)= Alltrim(TMPSTR.CustomerPo)
  Endif
  If Alltrim(TMPSTR.Type)='SalesRep1'
    Replace &lcSOCSVTmp..salesrep1 With TMPSTR.newvalue For Alltrim(CustomerPo)= Alltrim(TMPSTR.CustomerPo)
  Endif
  If Alltrim(TMPSTR.Type)='SalesRep2'
    Replace &lcSOCSVTmp..salesrep2 With TMPSTR.newvalue For Alltrim(CustomerPo)= Alltrim(TMPSTR.CustomerPo)
  Endif
  If Alltrim(TMPSTR.Type)='Terms'
    Replace &lcSOCSVTmp..Termss With TMPSTR.newvalue For Alltrim(CustomerPo)= Alltrim(TMPSTR.CustomerPo)
  Endif
  If Alltrim(TMPSTR.Type)='ShipVia'
    Replace &lcSOCSVTmp..SHIPVIAA With TMPSTR.newvalue For Alltrim(CustomerPo)= Alltrim(TMPSTR.CustomerPo)
  Endif
  If Alltrim(TMPSTR.Type)='Location'
    Replace &lcSOCSVTmp..locationn With TMPSTR.newvalue For Alltrim(CustomerPo)= Alltrim(TMPSTR.CustomerPo)
  Endif
  If Alltrim(TMPSTR.Type)='store'
    Replace &lcSOCSVTmp..storee With TMPSTR.newvalue For Alltrim(CustomerPo)= Alltrim(TMPSTR.CustomerPo) And Alltrim(TMPSTR.storee) = Alltrim(&lcSOCSVTmp..storee)
  Endif
Endscan

Select TMPSTR
Delete For !Empty(Alltrim(Type))
*------ Call form ------------
Select TMPSTR
Locate
If !Eof()
  Do Form (oAriaApplication.ClientScreenHome+"SO\soimpre.scx")
  Return .F.
Endif
*-------------------------------------
Store 0 To lnClrLen ,lnClrPos ,lnSizeLen ,lnSizePos ,lnMajLen
Store '' To lcClrSpr
Dimension laItemSeg[1]
=gfItemMask(@laItemSeg)
For lnCount = 1 To Alen(laItemSeg,1)
  Do Case
  Case laItemSeg[lnCount,1]='F'
    lnMajLen = Len(laItemSeg[lnCount,3])

  Case laItemSeg[lnCount,1]='C'
    lnClrLen = Len(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = Allt(laItemSeg[lnCount,6])
  Case laItemSeg[lnCount,1]='S'
    lnSizeLen = Len(laItemSeg[lnCount,3])
    lnSizePos = laItemSeg[lnCount,4]
  Endcase
Endfor
LCORDLNTMP = gfTempName()
lcOrdHdrTmp= gfTempName()
lcT_BomVar= gfTempName()
Dimension laOrders[1]
laOrders = ''
If !Used('BomVar')
  =gfOpenTable(oAriaApplication.DataDir+'BomVar',oAriaApplication.DataDir+'BomVar','SH')
Endif
If !Used('Scale')
  =gfOpenTable('Scale','Scale')
Endif
If !Used('STYDYE')
  =gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
Endif

If !Used("ORDLINE")
  =gfOpenTable("ORDLINE","ORDLINE")
Endif

If !Used("ORDHDR")
  =gfOpenTable("ORDHDR","ORDHDR")
Endif

lfCreateOrdTmp()
lfCreateOrders()


If !Empty(laOrders[1])
  If Alen(laOrders,1)>1
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from order#'+laOrders[1]+' to order#'+laOrders[ALEN(laOrders,1)]+' are created.')
  Else
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order#'+laOrders[1]+' is created.')
  Endif
Endif

************************************************************
*! Name      : lfGeCodeNO
*! Developer : mohamedhamdy
*! Date      : 10/18/2017
*! Purpose   : Get Code No. from code description
************************************************************
Function lfGeCodeNO
Lparameters lcCodeNum,lcCodeName
If !Used('Codes')
  =gfOpenTable("Codes",'cCode_NO')
Endif
lcCodeNo = ''
Select Codes
=gfSetOrder('cCode_NO')
If gfSeek("N"+Padr(lcCodeName,10))
  Scan Rest While CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM  = "N"+Padr(lcCodeName,10)
    If Allt(lcCodeNum) == Alltrim(Codes.CDISCREP)
      lcCodeNo = Codes.CCODE_NO
      Exit
    Endif
  Endscan
Endif
Return (lcCodeNo)
************************************************************
*! Name      : lfCreateOrders
*! Developer : Mohamedhamdy
*! Date      : 10/18/2017
*! Purpose   : Create SO function
************************************************************
Function lfCreateOrders
loFormSet =  Createobject('Custom')
loFormSet.AddProperty('lcdeposittemp','')
loFormSet.AddProperty('laEvntTrig[1]','')
loFormSet.AddProperty('ActiveMode','A')
loFormSet.laEvntTrig[1] =Padr('SOIMPRO',10)
loFormSet.lcdeposittemp = gfTempName()
laOrders[1]=''
Select Distinct CustomerPo,CSTNAM From (lcSOCSVTmp) Where !Deleted() Into Cursor 'TmpOrder'
Select 'TmpOrder'
Locate
Scan
  lCustomerPo  = TmpOrder.CustomerPo
  =Seek(TmpOrder.CustomerPo +TmpOrder.CSTNAM ,lcSOCSVTmp)
  Store '' To m.cclass,M.Ccurrcode ,m.cdivision,m.Cordtype,m.Terms ,m.CustomerPo ,;
    M.Order,m.locationn,m.GL_Sales,m.LINK_Code, m.Multi,m.note1,;
    M.priority,m.season,m.SHIPVIA,m.spcinst ,m.Status ,m.StName ,lcOrder ,m.CCARTRACK
  Store .F. To m.alt_shpto ,m.lhasnotes,m.MultiPO,m.lfromweb
  Store '' To m.Bulk,m.creorder,m.Rep1,m.Rep2,m.Buyer,m.Phone
  *B611273,MHM 26/02/2017 Modify the custom SO importing program for ERI02 to update BULK field with value N [T20170209.0019][Begin]
  m.Bulk = "N"
  *B611273,MHM 26/02/2017 Modify the custom SO importing program for ERI02 to update BULK field with value N [T20170209.0019][End]
  Store 0 To m.comm1,m.comm2,m.Nexrate,m.disc,m.Appramt ,m.FRGHTAMT ,m.TAXAMOUNT
  Store 0 To  m.Book,m.BookAmt,lnLastLNo,m.lastline,m.Nexrate,m.NcurrUnit,m.OPenAmt,m.OPen, m.TotAmnt
  *C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][End]
  Select (lcSOCSVTmp)
  Scatter Memo Memvar
  m.Order = ''

  =gfSeek('M'+m.CSTNAM,'Customer','Customer')
  m.Ccurrcode = oAriaApplication.BaseCurrency
  m.NcurrUnit = 1
  m.Nexrate  = 1
  m.lfromweb = .F.
  m.Buyer = Customer.Buyer
  m.Phone = Customer.Phone1
  m.Appramt = 0
  =gfSeek(&lcSOCSVTmp..Style,'Style','Style')
  m.season = Style.season
  Select Distinct cdivision From (lcSOCSVTmp) Where CustomerPo = TmpOrder.CustomerPo And CSTNAM = m.CSTNAM Into Cursor 'TmpcDivision'
  If Reccount('TmpcDivision') > 0
    Select TmpcDivision
    Locate
    m.cdivision = TmpcDivision.cdivision
  Endif

  Select(lcSOCSVTmp)
  Count For CustomerPo = TmpOrder.CustomerPo And CSTNAM = m.CSTNAM  To lnLastLNo

  Select(lcSOCSVTmp)
  Sum Ordered1,;
    Ordered1*nsugretpr;
    FOR CustomerPo = TmpOrder.CustomerPo And CSTNAM = m.CSTNAM To M.Ordered1,m.TotAmnt

  m.MultiPO  = .F.
  m.Cordtype = 'O'
  m.Order = '' &&gfSequence('ORDER','','',m.cDivision)
  m.CwareCode = m.locationn
  = gfSeek('M'+Alltrim(m.CSTNAM),'CUSTOMER','CUSTOMER')
  m.Rep1 = Alltrim(m.salesrep1)
  m.comm1 = Customer.Comm
  m.comm2 = Customer.comm2
  m.Rep2=  Alltrim(m.salesrep2)
  If Empty(Alltrim(m.Terms))
    m.Terms = Customer.CTERMCODE
  Endif
  m.GL_Sales = Customer.cslsgllink
  m.LINK_Code = Customer.LINK_Code
  m.Multi = 'N'
  Select Distinct storee From (lcSOCSVTmp) Where CustomerPo  = TmpOrder.CustomerPo And CSTNAM = m.CSTNAM And !Empty(storee) Into Cursor 'Tmpcstoree'
  If Reccount('Tmpcstoree') > 1
    m.Multi = 'Y'
  Endif
  If Empty(m.priority)
    m.priority = Customer.priority
  Endif
  If Empty(Alltrim(m.SHIPVIA))
    m.SHIPVIA = Customer.SHIPVIA
  Endif
  If Empty(Alltrim(m.spcinst))
    m.spcinst = Customer.spcinst
  Endif
  m.Status = 'O'
  =gfSeek(Iif(!Empty(m.storee),"S","M")+Alltrim(m.CSTNAM)+Iif(!Empty(m.storee),m.storee,""),'CUSTOMER','CUSTOMER')
  *C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][Start]
  m.caddress1=Alltrim(Customer.caddress1)
  m.caddress2=Alltrim(Customer.caddress2)
  m.caddress3=Alltrim(Customer.caddress3)
  m.caddress4=Alltrim(Customer.caddress4)
  m.caddress5=Alltrim(Customer.caddress5)
  m.StName = Iif(Empty(m.StName) And !Empty(m.storee),Customer.StName,m.StName)
  If Customer.ntaxrate > 0 And m.TAXAMOUNT> 0
    lnChoice  =gfModalGen("INM00000B44009","Dialog","","","Customer "+m.CSTNAM+" has tax rate "+Alltrim(Str(Customer.ntaxrate,6,3))+;
      " and the imported order has tax amount "+Alltrim(Str(m.TAXAMOUNT,13,2))+;
      ", would you like to use imported order tax amount?")
    Do Case
    Case lnChoice = 1
      *Yes
    Case lnChoice = 2
      *No
      m.TAXAMOUNT= 0
    Endcase
  Endif
  If !Empty(m.storee) And !Empty(m.StName)
    If m.StName <> Customer.StName
      *Warning Message
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'storee '+Alltrim(m.storee)+' has Ship to name in system: '+ Alltrim(Customer.StName) +', and the imported order ship name is '+Alltrim(m.StName)+'.')
    Endif
  Endif
  *C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][End]
  m.alt_shpto = .F.
  m.lhasnotes = .F.
  m.Account=Alltrim(m.CSTNAM)
  m.store=Alltrim(m.storee)
  If m.Multi = 'Y'
    m.store=""
  Endif
  m.Custpo=Alltrim(m.CustomerPo)
  m.start=Ctod(m.StartDate)
  m.complete=Ctod(m.CompleteDate)
  If !Empty(Alltrim(m.Termss))
    m.CTERMCODE=Alltrim(m.Termss)
  Else
    m.CTERMCODE=lfGeCodeNO(Alltrim(m.Terms) ,'CTERMCODE')
  Endif
  If !Empty(Alltrim(m.SHIPVIAA))
    m.SHIPVIA=Alltrim(m.SHIPVIAA)
  Else
    m.SHIPVIA=lfGeCodeNO(Alltrim(m.SHIPVIA) ,'SHIPVIA')
  Endif

  m.BookAmt = m.TotalAmount
  m.lastline  = lnLastLNo
  m.Flag = 'N'
  m.Book = 0
  m.OPen = 0
  m.OPenAmt = 0
  m.Entered = Date()
  Insert Into (lcOrdHdrTmp) From Memvar
  lcOrder = m.Order
  *Inserting Lines in Ordline File
  Select(lcSOCSVTmp)
  lnLine = 1

  Scan For CustomerPo = TmpOrder.CustomerPo And CSTNAM =m.CSTNAM And (m.Ordered1+ m.Ordered2+m.Ordered3+m.Ordered4+m.Ordered5+m.Ordered6+m.Ordered7+m.Ordered8+;
      M.Ordered9+m.Ordered10+m.Ordered11+m.Ordered12+m.Ordered13+m.Ordered14+m.Ordered15+m.Ordered16+m.Ordered17+m.Ordered18+m.Ordered19+m.Ordered20+;
      M.Ordered21+m.Ordered22+m.Ordered23+m.Ordered24+m.Ordered25+m.Ordered26+m.Ordered27+m.Ordered28+m.Ordered29+m.Ordered30+m.Ordered31+m.Ordered32)>0

    Store 0 To m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8
    Store 0 To m.qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8
    Store 0 To m.comm1,m.comm2
    Scatter Memo Memvar
    m.qty1 = m.Ordered1
    m.Qty2 = m.Ordered2
    m.Qty3 = m.Ordered3
    m.Qty4 = m.Ordered4
    m.Qty5 = m.Ordered5
    m.Qty6 = m.Ordered6
    m.Qty7 = m.Ordered7
    m.Qty8 = m.Ordered8
    m.disc_pcnt = Val(m.Discount)
    m.note_mem = ''

    m.CSTNAM = Alltrim(&lcSOCSVTmp..CSTNAM)
    m.CwareCode = m.locationn
    m.Cordtype = 'O'
    lnStyleLenWithoutScale = 15
    =gfSeek(Padr(Alltrim(&lcSOCSVTmp..Style),lnStyleLenWithoutScale,' '),'Style','Style')
    m.style = Padr(Alltrim(&lcSOCSVTmp..Style),lnStyleLenWithoutScale,' ')
    m.nsugretpri = Style.nsugretpri
    m.nsugretpr = Style.nsugretpri
    m.price = Style.nsugretpri
    m.Gros_price = Style.nsugretpri
    m.Cost = Style.TOTCOST
    m.Desc1= Style.Desc1
    m.Flag = 'N'
    m.Gl_Cost = Style.LINK_Code
    m.GL_Sales = Customer.cslsgllink + Style.cslsgllink
    m.Scale = Style.Scale
    m.season = Style.season
    m.cdivision = Style.cdivision
    m.Group = Alltrim(m.CSTYGROUPP)
    If Empty(Alltrim(&lcOrdHdrTmp..season))
      Replace season With Style.season In (lcOrdHdrTmp)
    Else
      If !(Alltrim(Style.season)==Alltrim(&lcOrdHdrTmp..season))
        Replace season With '*' In (lcOrdHdrTmp)
      Endif
    Endif
    If Empty(Alltrim(&lcOrdHdrTmp..cdivision))
      Replace cdivision With Style.cdivision In (lcOrdHdrTmp)
    Else
      If !(Alltrim(Style.cdivision)==Alltrim(&lcOrdHdrTmp..cdivision ))
        Replace cdivision With '*' In (lcOrdHdrTmp)
      Endif
    Endif
    m.Book1 = m.qty1
    m.Book2 = m.Qty2
    m.Book3 = m.Qty3
    m.Book4 = m.Qty4
    m.Book5 = m.Qty5
    m.Book6 = m.Qty6
    m.Book7 = m.Qty7
    m.Book8 = m.Qty8
    m.totbook =  m.qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
    m.totQty =  m.qty1 + m.Qty2 + m.Qty3 + m.Qty4 + m.Qty5 + m.Qty6 + m.Qty7 + m.Qty8
    If &lcOrdHdrTmp..Multi=='Y'
      m.storee=&lcSOCSVTmp..storee
      m.store=&lcSOCSVTmp..storee
    Endif
    Set Step On
    If m.disc_pcnt <=0 And Val(m.DiscountCost) > 0
      Store 0 To llcstr,llcStrscal,llnScaleCount,llnlinetotal
      m.price = Val(m.DiscountCost)
    Endif
    Store 0 To lcstr,lcStrscal,lnScaleCount
    lcscale=Substr(Alltrim(m.Scale),1,2)
    Select Scale
    If Seek('S'+ lcscale)

      Scan Rest While Type+Scale+PREPAK = 'S'+ lcscale
        m.style = Padr(Alltrim(&lcSOCSVTmp..Style),lnStyleLenWithoutScale,' ')

        Store 0 To m.Book1,m.Book2,m.Book3,m.Book4,m.Book5,m.Book6,m.Book7,m.Book8
        Store 0 To m.qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.totbook, m.totQty
        For i = 1 To Scale.Cnt
          lcstr        = Alltrim(Str(i))
          lcStrscal    = Alltrim(Str(i+lnScaleCount*8))
          m.qty&lcstr  = m.ordered&lcStrscal.
          m.totQty     = m.totQty + m.ordered&lcStrscal.
          m.Book&lcstr = m.ordered&lcStrscal.
          m.totbook    = m.totbook +  m.qty&lcstr.
        Endfor
        m.style = m.style+Padr(Alltrim(Scale.Scale),4,' ')
        lnScaleCount = lnScaleCount + 1
        If m.totbook > 0
          lnLine = lnLine + 1
          If m.disc_pcnt >0
            m.price = m.price - (m.price*(m.disc_pcnt/100))
          Endif
          Insert Into (LCORDLNTMP) From Memvar
          Replace Book    With Book   + m.totbook In (lcOrdHdrTmp)
          Replace BookAmt With BookAmt +(m.totbook * m.nsugretpr)  In (lcOrdHdrTmp)
          Replace Open    With Open    + m.totbook In (lcOrdHdrTmp)
          Replace OPenAmt With OPenAmt +(m.totbook * m.nsugretpr)  In (lcOrdHdrTmp)
        Endif
      Endscan
    Endif
  Endscan
  llOrdSaved = .F.
  Set Order To oRDLINE In (LCORDLNTMP)
  Do lfSavScr In (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
    WITH .F., 'A', lcOrdHdrTmp,LCORDLNTMP,.F.,.F.,lcT_BomVar,loFormSet

  If llOrdSaved
    If !Empty(laOrders[1])
      Dimension laOrders[ALEN(laOrders,1)+1]
      laOrders[ALEN(laOrders,1)] = ORDHDR.Order
    Else
      laOrders[1] = ORDHDR.Order
    Endif
  Endif
  Select (LCORDLNTMP)
  Delete All
  Pack
  Select (lcOrdHdrTmp)
  Delete All
  Pack
Endscan
lfSavefiles()
************************************************************
*! Name      : lfCreateOrdTmp
*! Developer : Mohamedhamdy
*! Date      : 10/18/2017
*! Purpose   : Create Temp. Order cursors
************************************************************
Function lfCreateOrdTmp
Select oRDLINE
=Afields(laFileStru)
lnFileStru = Alen(laFileStru,1)
llTrade =  gfGetMemVar('M_TRDDISCL')
If llTrade
  If lnSizeLen  = 0
    Dimension laFileStru[lnFileStru+4,18]
  Else
    Dimension laFileStru[lnFileStru+5,18]
  Endif
Else
  If lnSizeLen  = 0
    Dimension laFileStru[lnFileStru+3,18]
  Else
    Dimension laFileStru[lnFileStru+4,18]
  Endif
Endif

laFileStru[lnFileStru+1,1] = 'lContract'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cMajor'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = lnMajLen
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'cNonMajor'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = lnClrLen
laFileStru[lnFileStru+3,4] = 0
If lnSizeLen  <> 0
  laFileStru[lnFileStru+4,1] = 'cMjrScale'
  laFileStru[lnFileStru+4,2] = 'C'
  laFileStru[lnFileStru+4,3] =  lnSizeLen
  laFileStru[lnFileStru+4,4] = 0
Endif
If llTrade
  If lnSizeLen  = 0
    laFileStru[lnFileStru+4,1] = 'TRD_price'
    laFileStru[lnFileStru+4,2] = 'N'
    laFileStru[lnFileStru+4,3] =  12
    laFileStru[lnFileStru+4,4] = 2

  Else

    laFileStru[lnFileStru+5,1] = 'TRD_price'
    laFileStru[lnFileStru+5,2] = 'N'
    laFileStru[lnFileStru+5,3] =  12
    laFileStru[lnFileStru+5,4] = 2
  Endif
Endif
For lnCount = 1 To Iif(lnSizeLen  = 0,Iif(llTrade ,4,3),Iif(llTrade ,5,4))
  Store '' To laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
    laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
    laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
    laFileStru[lnFileStru+lnCount,16]
  Store 0 To  laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
Endfor
Declare laIndex[4,2]
laIndex[1,1] = 'cOrdType+ORDER+store+STYLE+DYELOT+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+store+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
laIndex[4,1] = 'Order+store+STYLE+Dyelot+STR(LineNo,6)'
laIndex[4,2] = 'CONFIGLIN'

=gfCrtTmp(LCORDLNTMP,@laFileStru,@laIndex)
Set Order To Tag 'ORDLINE' In (LCORDLNTMP)

Select ORDHDR
=Afields(laFileStru)
lnFileStru = Alen(laFileStru,1)
Dimension laFileStru[lnFileStru,18]
Declare laIndex[1,2]
laIndex[1,1] = 'cordtype+order'
laIndex[1,2] = lcOrdHdrTmp
=gfCrtTmp(lcOrdHdrTmp,@laFileStru,@laIndex)


Select BomVar
=Afields(laFileStru)
lnFileStru = Alen(laFileStru,1)
Dimension laFileStru[lnFileStru+2,18]


laFileStru[lnFileStru+1,1] = 'nRecno'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cStatus'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0
For lnLoop = 1 To  2
  Store ' ' To  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
    laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
    laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
    laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
    laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  Store 0 To    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
Endfor
If Used(lcT_BomVar)
  Zap In (lcT_BomVar)
Else
  =gfCrtTmp(lcT_BomVar,@laFileStru,[cIdType+cCost_Id+STR(LineNo,6)],lcT_BomVar)
Endif

*!*************************************************************
*! Name      : lfSavefiles
*! Developer : mohamed hamdy
*! Date      : 10/18/2017
*! Purpose   : Function to save
*!*************************************************************
Function lfSavefiles
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
If Type('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  Return .F.
Endif
lnUpdated = 0
lnAryLen = Alen(oAriaApplication.laRemoteTable)
For lnCounter=1 To lnAryLen
  If oAriaApplication.laRemoteTable[lnCounter].lnDataSession == Set("Datasession" )
    If !oAriaApplication.laRemoteTable[lnCounter].Tableupdate(lcTranCode)
      lnUpdated=lnCounter
      Exit
    Endif
  Endif
Next
If lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  Messagebox('Saving Process is Rolled Back')
  Thisformset.Undo()
  Return
Else
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
Endif
************************************************************
*! Name      : lfcustmsg
*! Developer : Mohamedhamdy
*! Date      : 09/18/2016
*! Purpose   : Custom function to check if order is created or not
************************************************************
Function lfcustmsg
llOrdSaved = .T.
