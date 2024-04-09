***
*** ReFox XI+  #OE577650  Mariam  Mariam [VFP90]
***C2016652,AEG 1/26/2015 ASN has Lin segments less than PO1(s) comes in 850 ,modify to locate with order  line [T20150123.0002]
***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [T20150126.0029]
***C201869,1 AEG 11/09/2016 modify GMAINV to hande gros profit and sumary variables for 810 [P20160919.0001]
***C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [P20161215.0001]
***C201934,1 05/02/2017 MSH - Handle Case Of Saving IN EDISV [P20161215.0001]
***C202342,1 06/16/2020 HIA fix partner setup screen not open, missing class [P20200506.0003]
***C202353,1 06/16/2020 HIA add missing standard fields [P20200506.0003]
***C202384,1 09/15/2020 HIA trigger to re-format the sku as style when procress Shopify [P20200506.0003]
************************************************************************************************************
Parameter lcevntfun, lcfunpars
lcfunpars = Iif(Type('lcFunPars') = 'C', lcfunpars, '')
lcfuntorun = 'lf' + Alltrim(lcevntfun) + '(' + lcfunpars + ')'
llretvalue = Evaluate(lcfuntorun)
Return llretvalue
**
Procedure lfPRPack
  If lrange_process .And. !Empty(cblkord)
    Select ordhdr
    lnstqty_hld = lnqty
    If Seek(Padr(Alltrim(maccount), 5) + 'O' + cblkord, 'ORDHDR', 'ORDACCT')
      lminflag = .F.
      Select ordline
      = Seek('O' + cblkord, 'OrdLine', 'OrdLine')
      Scan Rest While cordtype + Order + Str(Lineno, 6) = "O" + cblkord
        If totbook > 0 .And. Seek(mtype + mkey + ordline.Style, 'Spck_Lin', 'Spck_LinVr')
          Store 0 To ntotbook_sum, book1_sum, book2_sum, book3_sum, book4_sum, book5_sum, book6_sum, book7_sum, book8_sum
          Select blkordln
          = Seek('T' + cblkord + Str(ordline.Lineno, 6), 'BlkOrdLn', 'OrdBulk')
          Sum totbook, book1, book2, book3, book4, book5, book6, book7, book8 To ntotbook_sum, book1_sum, book2_sum, book3_sum, book4_sum, book5_sum, book6_sum, book7_sum, book8_sum Rest While cordtype + Order + Str(Lineno, 6) = 'T' + cblkord + Str(ordline.Lineno, 6)
          Select ediordln
          lntotusdqty = 0
          Store 0 To lnusedqty1, lnusedqty2, lnusedqty3, lnusedqty4, lnusedqty5, lnusedqty6, lnusedqty7, lnusedqty8
          Sum qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8, totqty To lnusedqty1, lnusedqty2, lnusedqty3, lnusedqty4, lnusedqty5, lnusedqty6, lnusedqty7, lnusedqty8, lntotusdqty For ediordln.Style + ediordln.pack_id = ordline.Style + spck_lin.pack_id
          book1_sum = book1_sum + lnusedqty1
          book2_sum = book2_sum + lnusedqty2
          book3_sum = book3_sum + lnusedqty3
          book4_sum = book4_sum + lnusedqty4
          book5_sum = book5_sum + lnusedqty5
          book6_sum = book6_sum + lnusedqty6
          book7_sum = book7_sum + lnusedqty7
          book8_sum = book8_sum + lnusedqty8
          Select ordline
          navlblqty = ordline.totbook - ntotbook_sum - lntotusdqty
          If navlblqty > 0
            lminflag = Iif(navlblqty > lnstqty_hld, .T., .F.)
            xstyle = ordline.Style
            xpack = spck_lin.pack_id
            = Seek(xstyle, 'STYLE')
            m.gl_sales = lcslslink + Style.cslsgllink
            m.gl_sales = Iif(lllinktogl .And. Seek('02' + m.gl_sales, 'GL_LINK'), m.gl_sales, 'DEFDEF')
            xseason = Style.season
            xwarecode = Iif(Empty(lcdefware), Style.cdefware, lcdefware)
            xscale = Style.Scale
            Select ediordln
            lcpack = xpack + Padr(spck_lin.cpkcolor, 6) + Padr(spck_lin.cpcksize, 3) + Padr(spck_lin.cpkversion, 4)
            If !Seek(Padr(lcdumstore, 8) + xstyle + lcpack) .Or. (oobject.laslines .And. oobject.newpo1)
              Mline = Mline + 1
              Append Blank
              Replace account With maccount, custpo With mcustpo, Store With lcdumstore, Style With xstyle, pack_id With xpack, Order With morder, Lineno With Mline, Start With mstart, Complete With mcomplete, Scale With xscale, clinestat With mlnstatus, season With xseason, desc1 With Iif(Empty(mdesc1), Style.desc1, mdesc1), cwarecode With Iif(Empty(lcdefware), Style.cdefware, lcdefware), nsugretpri With msrp, gl_sales With m.gl_sales, cfromorder With cblkord, bulklineno With ordline.Lineno, cpkcolor With lcpack_clr, cpcksize With lcpack_sz, cpkversion With lcpack_vrn, npkslprice With mpck_price, dyelot With ordline.dyelot
              Replace lrange With llpack_rng, lpckprpiec With llpack_prpk
              Replace ediordln.npo1 With oobject.po1counter
              If xpoid = 'PC' .And. !Empty(mlnstatus)
                Replace desc1 With 'SIZES MODIFIED:'
              Endif
              If llcatdanial
                Replace cpack With mpack
              Endif
            Endif
            If oobject.laslines .And. ediordln.npo1 <> oobject.po1counter
              = Seek(Padr(lcdumstore, 8) + xstyle + lcpack)
              Scan Rest While Evaluate(Key()) = Padr(lcdumstore, 8) + xstyle + lcpack
                If ediordln.npo1 = oobject.po1counter
                  Exit
                Endif
              Endscan
            Endif
            If xpoid = 'PC' .And. !Empty(mlnstatus)
              lcsizesmod = ''
              For lncount = 1 To 8
                lccount = Str(lncount, 1)
                If spck_lin.qty&lccount <> 0
                  lcsizesmod = lcsizesmod + 'SIZE' + lccount + ','
                Endif
              Endfor
              Replace desc1 With Alltrim(desc1) + lcsizesmod
            Endif
            If lminflag
              For lncount = 1 To 8
                lccount = Str(lncount, 1)
                lndiff  = (ordline.book&lccount - book&lccount._sum )
                If lndiff < lnstqty_hld
                  Replace qty&lccount    With  qty&lccount  +lndiff book&lccount   With  book&lccount +lndiff
                  lnstqty_hld = lnstqty_hld - lndiff
                Else
                  Replace qty&lccount    With  qty&lccount  + lnstqty_hld book&lccount   With  book&lccount + lnstqty_hld
                  Exit
                Endif
              Endfor
              Replace totqty With totqty + lnstqty_hld, price With price + Iif(lnprice = 0, price, lnprice), note_mem With mlnotes + Chr(13) Additive
              Replace totbook With totbook + lnstqty_hld
              mbook = mbook + lnstqty_hld
              mbookamt = mbookamt + price * lnstqty_hld
              lnstqty_hld = 0
            Else
              Replace qty1 With qty1 + (ordline.book1 - book1_sum), qty2 With qty2 + (ordline.book2 - book2_sum), qty3 With qty3 + (ordline.book3 - book3_sum), qty4 With qty4 + (ordline.book4 - book4_sum), qty5 With qty5 + (ordline.book5 - book5_sum), qty6 With qty6 + (ordline.book6 - book6_sum), qty7 With qty7 + (ordline.book7 - book7_sum), qty8 With qty8 + (ordline.book8 - book8_sum), totqty With totqty + navlblqty, price With price + Iif(lnprice = 0, price, lnprice), note_mem With mlnotes + Chr(13) Additive
              Replace book1 With book1 + (ordline.book1 - book1_sum), book2 With book2 + (ordline.book2 - book2_sum), book3 With book3 + (ordline.book3 - book3_sum), book4 With book4 + (ordline.book4 - book4_sum), book5 With book5 + (ordline.book5 - book5_sum), book6 With book6 + (ordline.book6 - book6_sum), book7 With book7 + (ordline.book7 - book7_sum), book8 With book8 + (ordline.book8 - book8_sum), totbook With totbook + navlblqty
              mbook = mbook + navlblqty
              mbookamt = mbookamt + price * navlblqty
              lnstqty_hld = lnstqty_hld - navlblqty
            Endif
            If totqty = 0
              Delete
            Endif
            If lminflag
              Exit
            Endif
          Endif
        Endif
      Endscan
      If !lminflag .And. lnstqty_hld > 0
        = Fputs(ereport, 'File Code#: ' + lcfilecode + ', ACCOUNT#: ' + maccount + ' CUST. PO#: ' + Alltrim(mcustpo) + ', Pack Id: ' + spck_hdr.pack_id + ' have not added all ,as Bulk Order no ' + cblkord + ' Have not enough Quantity.')
      Endif
    Endif
  Else
    If !lmversion
      = oobject.processpack(lnqty, lcdumstore)
    Endif
  Endif
Endproc
**
Procedure lfAddV
  Local lccust_view, lcgma_view
  Store "NA" To lccust_view, lcgma_view
  llreject = .F.
  = Seek('P' + mkey, 'Spck_Hdr', 'Spck_HdrVr')
  If !spck_hdr.lrange
    Do Case
      Case mord = 3 .And. !spck_hdr.lqtypcs .And. Inlist(muom, 'EA')
        lccust_view = "Pack"
        lcgma_view = "Pack"
      Case mord = 3 .And. spck_hdr.lqtypcs .And. Inlist(muom, 'EA')
        If lflag = .T.
          lnqty = lnqty / mpckqty
          If Mod(lnqty, 1) <> 0
            llreject = .T.
          Endif
        Endif
        &lcgmaqty. = lnqty
        lccust_view = "Pieces"
        lcgma_view = "Pack"
      Case mord = 3 .And. Inlist(muom, 'CA', 'CT')
        lccust_view = "Pack"
        lcgma_view = "Pack"
        If lflag = .T.
          If mpri <> 0
            If mpo4qs <> 0 .Or. mpo4qi <> 0
              mpo4qs = Iif(mpo4qs = 0, 1, mpo4qs)
              lnqty = mpo4qi * mpo4qs * lnqty / mpckqty
              If Mod(lnqty, 1) <> 0
                llreject = .T.
              Endif
              &lcgmaqty. = lnqty
            Else
              If spck_hdr.lqtypcs
                lnqty = lnqty / mpckqty
                If Mod(lnqty, 1) <> 0
                  llreject = .T.
                Endif
                &lcgmaqty. = lnqty
                lccust_view = "Pieces"
                lcgma_view = "Pack"
              Endif
            Endif
          Else
            lnqty = lnqty * minerpack * mpackunit / mpckqty
            If Mod(lnqty, 1) <> 0
              llreject = .T.
            Endif
            &lcgmaqty. = lnqty
          Endif
        Endif
      Case Inlist(mord, 1, 2, 5) .And. price = lnprice .And. Inlist(muom, 'EA')
        lccust_view = "Style"
        lcgma_view = "Style"
      Case Inlist(mord, 1, 2, 5) .And. price <> lnprice .And. Inlist(muom, 'EA')
        lccust_view = "NA"
        lcgma_view = "Style"
      Case Inlist(mord, 1, 2, 5) .And. Inlist(muom, 'CT', 'CA')
        lccust_view = "NA"
        lcgma_view = "Style"
    Endcase
  Endif
  If lflag = .F.
    If mord = 3
      If !llreject
        Replace ccust_view With lccust_view, cgma_view With lcgma_view, cpkcolor With lcpack_clr, cpcksize With lcpack_sz, cpkversion With lcpack_vrn, cpckacc With Substr(mkey, 1, 5), npackno With lnqty, lrange With llpack_rng, lpckprpiec With llpack_prpk
        If lnprice = 0
          Replace npkslprice With 0
        Else
          lnpri = Iif(muom = 'EA', lnprice, lnpolnprc)
          If mpck_price = lnpri
            Replace npkslprice With mpck_price
          Else
            If rflag .And. edilibdt.ceditrntyp <> '860'
              = Fputs(ereport, 'File Code#: ' + lcfilecode)
              = Fputs(ereport, 'ACCOUNT#: ' + maccount)
              = Fputs(ereport, 'CUST. PO#: ' + Alltrim(mcustpo))
              Iif(!Empty(&lcstores..Store),Fputs(ereport ,'Store #: ' + Alltrim(&lcstores..Store)),.T.)
              Iif(!Empty(mupc), Fputs(ereport, 'UPC : ' + Alltrim(mupc)), .T.)
              Iif(!Empty(mstyle), Fputs(ereport, 'SKU : ' + Alltrim(mstyle + msiz)), .T.)
              = Fputs(ereport, 'Pack ID : ' + Alltrim(spck_hdr.pack_id))
              = Fputs(ereport, 'Price sent does match pack price for pack : ' + Alltrim(mkey) + '.')
              rflag = .F.
            Else
              If edilibdt.ceditrntyp = '860'
                Replace npkslprice With lnprice
              Endif
            Endif
          Endif
        Endif
        Replace price With spck_lin.npck_price, dyelot With spck_lin.dyelot
      Endif
    Else
      Replace ccust_view With lccust_view, cgma_view With lcgma_view
    Endif
  Endif
Endproc
**
Procedure lfOPNFLS
  lnarrlenr = Alen(oobject.filestoopen, 1)
  lnarrlenc = Alen(oobject.filestoopen, 2)
  Dimension oobject.filestoopen[lnarrlenr + 5, lnarrlenc]
  oobject.filestoopen[6, 5] = 'SPCK_HDRVR'
  oobject.filestoopen[7, 5] = 'SPCK_LINVR'
  lnarrlenr = lnarrlenr + 1
  oobject.filestoopen[lnarrlenr, 1] = 'D'
  oobject.filestoopen[lnarrlenr, 2] = 'ORDLINE'
  oobject.filestoopen[lnarrlenr, 3] = ''
  oobject.filestoopen[lnarrlenr, 4] = 'BlkORDLN'
  oobject.filestoopen[lnarrlenr, 5] = 'OrdBulk'
  oobject.filestoopen[lnarrlenr, 7] = 1
  lnarrlenr = lnarrlenr + 1
  oobject.filestoopen[lnarrlenr, 1] = 'D'
  oobject.filestoopen[lnarrlenr, 2] = 'ConflctH'
  oobject.filestoopen[lnarrlenr, 3] = ''
  oobject.filestoopen[lnarrlenr, 4] = 'ConflctH'
  oobject.filestoopen[lnarrlenr, 5] = 'ConflctH'
  oobject.filestoopen[lnarrlenr, 7] = 1
  lnarrlenr = lnarrlenr + 1
  oobject.filestoopen[lnarrlenr, 1] = 'D'
  oobject.filestoopen[lnarrlenr, 2] = 'ConflctL'
  oobject.filestoopen[lnarrlenr, 3] = ''
  oobject.filestoopen[lnarrlenr, 4] = 'ConflctL'
  oobject.filestoopen[lnarrlenr, 5] = 'ConflctL'
  oobject.filestoopen[lnarrlenr, 7] = 1
  lnarrlenr = lnarrlenr + 1
  oobject.filestoopen[lnarrlenr, 1] = 'D'
  oobject.filestoopen[lnarrlenr, 2] = 'CnfHdrAr'
  oobject.filestoopen[lnarrlenr, 3] = ''
  oobject.filestoopen[lnarrlenr, 4] = 'CnfHdrAr'
  oobject.filestoopen[lnarrlenr, 5] = ''
  oobject.filestoopen[lnarrlenr, 7] = 1
  lnarrlenr = lnarrlenr + 1
  oobject.filestoopen[lnarrlenr, 1] = 'D'
  oobject.filestoopen[lnarrlenr, 2] = 'CnfLinAr'
  oobject.filestoopen[lnarrlenr, 3] = ''
  oobject.filestoopen[lnarrlenr, 4] = 'CnfLinAr'
  oobject.filestoopen[lnarrlenr, 5] = 'ConflctL'
  oobject.filestoopen[lnarrlenr, 7] = 1
Endproc
**
Procedure lfBLKBRW
  Public llgma_browse
  llgma_browse = .T.
  Do Form oariaapplication.screenhome+"EBGmaAdp" With lcordline
  Release llgma_browse
Endproc
**
Procedure lfADJBRW
  If llgma_browse
    For lncount = 1 To (obrowse.ColumnCount - 1)
      obrowse.Columns(lncount).Visible = .T.
      obrowse.Columns(lncount).AddObject('TEXT2', 'GRIDCOLUMN')
      obrowse.Columns(lncount).text2.Visible = .T.
      obrowse.Columns(lncount).curnrentcontrol = 'TEXT2'
    Endfor
    obrowse.Columns(lncount).Visible = .T.
    obrowse.Columns(lncount).AddObject('TEXT2', 'BRWTXT')
    obrowse.Columns(lncount).text2.Visible = .T.
    obrowse.Columns(lncount).CurrentControl = 'TEXT2'
  Endif
Endproc
**
Procedure lfEDTBRW
  Browse Fields &tcbrowsefields  Window (pcwinname) In Window (oobject.Name)  Nowait Save Nodele Noapp   Name obrowse Title oobject.browsetitle  &lckey &lcfor
Endproc
**
Procedure lfUPCSKU
  Set Step On
  lcpartsetup = Iif(Empty(ediacprt.cporcvline), 'US', ediacprt.cporcvline)
  Do Case
      lcrcv_with = 'UPC'
    Case llupcuse .And. Seek(Padr(mupc, 13), 'StyleUpc') .And. !styleupc.lupcpack .And. (Inlist(lcpartsetup, 'UO') .Or. Empty(mstyle) .Or. Seek('S' + mkey, 'Spck_Lin')) .And. Inlist(lcpartsetup, 'UO', 'US', 'SU')
      lcrcv_with = 'UPC'
      mord = 5
    Case llupcuse .And. Seek(Padr(mupc, 13), 'StyleUpc') .And. styleupc.lupcpack .And. Seek('P' + '*****' + Padr(Alltrim(styleupc.Style), 16) + styleupc.cpkcolor + styleupc.cpksize, 'Spck_Hdr', 'Spck_hdrvr') .And. Inlist(lcpartsetup, 'UO', 'US', 'SU')
      If !Empty(mstyle) .And. Inlist(lcpartsetup, 'US', 'SU')
        If (Seek('S' + Padr(Alltrim(maccount), 5) + Padr(Alltrim(mstyle + msiz), 16), 'Spck_Lin', 'Spck_LinVr') .And. Seek('S' + Padr(Alltrim(maccount), 5) + spck_lin.Style + spck_lin.cpkcolor + spck_lin.cpcksize, 'Spck_Hdr', 'Spck_Hdrst') .And. spck_hdr.cskutype = "P")
          lcrcv_with = 'SKU'
          = Seek('P' + Padr(Alltrim(maccount), 5) + Padr(Alltrim(styleupc.Style), 16) + styleupc.cpkcolor + styleupc.cpksize, 'Spck_Hdr', 'Spck_hdrvr')
          lcpack_id = spck_hdr.pack_id
          lcpack_sz = spck_hdr.cpcksize
          lcpack_clr = spck_hdr.cpkcolor
          lcpack_vrn = spck_hdr.cpkversion
          llpack_rng = spck_hdr.lrange
          llpack_prpk = spck_hdr.lpckprpiec
          Select spck_lin
          lcorder = Set('Order')
          mkey = Padr(Alltrim(maccount), 5) + lcpack_id + lcpack_clr + lcpack_sz + lcpack_vrn
          mtype = 'P'
          mord = 3
          If Seek(lcfilecode + Padr(mcustpo, 15) + Padr(lcpack_id, 16) + '-' + lcpack_clr + '-' + lcpack_sz, 'ConflctL', 'ConflctL') .And. !Empty(conflctl.cpkversion)
            mkey = Padr(Alltrim(maccount), 5) + lcpack_id + lcpack_clr + lcpack_sz + conflctl.cpkversion
            lcpack_vrn = conflctl.cpkversion
          Endif
          = Seek('P' + mkey, 'Spck_Lin', 'Spck_linvr')
          Sum totqty To mpckqty Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + mkey
          = Seek('P' + mkey, 'Spck_Lin', 'Spck_linvr')
          = Seek('P' + mkey, 'Spck_HDR', 'Spck_hdrvr')
          mpck_price = spck_hdr.npkslprice
        Else
          = Fputs(ereport, ' SKU Send #:' + Alltrim(mstyle + msiz) + 'is not in our DB.')
          = Fputs(ereport, 'Pack does not exist. ')
        Endif
      Else
        If llupcuse .And. Seek(Padr(mupc, 13), 'StyleUpc') .And. styleupc.lupcpack .And. Seek('P' + Padr(Alltrim(maccount), 5) + Padr(Alltrim(styleupc.Style), 16) + styleupc.cpkcolor + styleupc.cpksize, 'Spck_Hdr', 'Spck_hdrvr')
          lcrcv_with = 'UPC'
          lcpack_id = spck_hdr.pack_id
          lcpack_sz = spck_hdr.cpcksize
          lcpack_clr = spck_hdr.cpkcolor
          lcpack_vrn = spck_hdr.cpkversion
          llpack_rng = spck_hdr.lrange
          llpack_prpk = spck_hdr.lpckprpiec
          Select spck_lin
          lcorder = Set('Order')
          mkey = Padr(Alltrim(maccount), 5) + lcpack_id + lcpack_clr + lcpack_sz + lcpack_vrn
          mtype = 'P'
          mord = 3
          If Seek(lcfilecode + Padr(mcustpo, 15) + Padr(lcpack_id, 16) + '-' + lcpack_clr + '-' + lcpack_sz, 'ConflctL', 'ConflctL') .And. !Empty(conflctl.cpkversion)
            mkey = Padr(Alltrim(maccount), 5) + lcpack_id + lcpack_clr + lcpack_sz + conflctl.cpkversion
            lcpack_vrn = conflctl.cpkversion
          Endif
          = Seek('P' + mkey, 'Spck_Lin', 'Spck_linvr')
          Sum totqty To mpckqty Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + mkey
          = Seek('P' + mkey, 'Spck_Lin', 'Spck_linvr')
          = Seek('P' + mkey, 'Spck_HDR', 'Spck_hdrvr')
          mpck_price = spck_hdr.npkslprice
        Else
          If llupcuse .And. Seek(Padr(mupc, 13), 'StyleUpc') .And. styleupc.lupcpack .And. Seek('P' + '*****' + Padr(Alltrim(styleupc.Style), 16) + styleupc.cpkcolor + styleupc.cpksize, 'Spck_Hdr', 'Spck_hdrvr')
            lcrcv_with = 'UPC'
            lcpack_id = spck_hdr.pack_id
            lcpack_sz = spck_hdr.cpcksize
            lcpack_clr = spck_hdr.cpkcolor
            lcpack_vrn = spck_hdr.cpkversion
            llpack_rng = spck_hdr.lrange
            llpack_prpk = spck_hdr.lpckprpiec
            Select spck_lin
            lcorder = Set('Order')
            mkey = '*****' + lcpack_id + lcpack_clr + lcpack_sz + lcpack_vrn
            mtype = 'P'
            mord = 3
            If Seek(lcfilecode + Padr(mcustpo, 15) + Padr(lcpack_id, 16) + '-' + lcpack_clr + '-' + lcpack_sz, 'ConflctL', 'ConflctL') .And. !Empty(conflctl.cpkversion)
              mkey = '*****' + lcpack_id + lcpack_clr + lcpack_sz + conflctl.cpkversion
              lcpack_vrn = conflctl.cpkversion
            Endif
            = Seek('P' + mkey, 'Spck_Lin', 'Spck_linvr')
            Sum totqty To mpckqty Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + mkey
            = Seek('P' + mkey, 'Spck_Lin', 'Spck_linvr')
            = Seek('P' + mkey, 'Spck_HDR', 'Spck_hdrvr')
            mpck_price = spck_hdr.npkslprice
          Else
            = Fputs(ereport, 'Pack does not exist. ')
          Endif
        Endif
      Endif
    Case Seek('S' + Padr(Alltrim(maccount), 5) + Padr(Alltrim(mstyle + msiz), 16), 'Spck_Lin', 'Spck_LinVr') .And. Seek('S' + Padr(Alltrim(maccount), 5) + spck_lin.Style + spck_lin.cpkcolor + spck_lin.cpcksize, 'Spck_Hdr', 'Spck_Hdrst') .And. spck_hdr.cskutype = "P" .And. Inlist(lcpartsetup, 'SO', 'US', 'SU')
      lcrcv_with = 'SKU'
      lcpack_id = Alltrim(spck_hdr.Style)
      lcpack_sz = spck_hdr.cpcksize
      lcpack_clr = spck_hdr.cpkcolor
      = Seek('P' + Padr(Alltrim(maccount), 5) + Padr(lcpack_id, 16) + lcpack_clr + lcpack_sz, 'Spck_Hdr', 'Spck_HdrVr')
      lcpack_id = spck_hdr.pack_id
      lcpack_vrn = spck_hdr.cpkversion
      llpack_rng = spck_hdr.lrange
      llpack_prpk = spck_hdr.lpckprpiec
      Select spck_lin
      mkey = Padr(Alltrim(maccount), 5) + spck_hdr.pack_id + lcpack_clr + lcpack_sz + lcpack_vrn
      mtype = 'P'
      mord = 3
      If Seek(lcfilecode + Padr(mcustpo, 15) + Padr(spck_hdr.pack_id, 16) + '-' + lcpack_clr + '-' + lcpack_sz, 'ConflctL', 'ConflctL') .And. !Empty(conflctl.cpkversion)
        mkey = Padr(Alltrim(maccount), 5) + spck_hdr.pack_id + lcpack_clr + lcpack_sz + conflctl.cpkversion
        lcpack_vrn = conflctl.cpkversion
        = Seek('P' + mkey, 'Spck_Hdr', 'Spck_HdrVr')
        llpack_rng = spck_hdr.lrange
        llpack_prpk = spck_hdr.lpckprpiec
      Endif
      = Seek('P' + Padr(Alltrim(maccount), 5) + spck_hdr.pack_id + lcpack_clr + lcpack_sz + lcpack_vrn, 'Spck_Lin', 'Spck_linvr')
      Sum totqty To mpckqty Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + mkey
      = Seek('P' + Padr(Alltrim(maccount), 5) + spck_hdr.pack_id + lcpack_clr + lcpack_sz + lcpack_vrn, 'Spck_Lin', 'Spck_linvr')
      = Seek('P' + mkey, 'Spck_HDR', 'Spck_hdrvr')
      mpck_price = spck_hdr.npkslprice
    Case Seek('S' + mkey, 'Spck_Lin') .And. Inlist(lcpartsetup, 'SO', 'US', 'SU')
      lcrcv_with = 'SKU'
      mtype = 'S'
      mprepck = (spck_lin.totqty = 0)
      mord = Iif(spck_lin.totqty = 0, 2, 1)
      If mmapset = 'GAP' .And. mord = 1
        lcpack_id = Space(16)
      Endif
    Otherwise
  Endcase
Endproc
**
Procedure lfErr
  Set Step On
  If Empty(mupc) .And. Empty(mstyle)
    = Fputs(ereport, ' UPC#: is not sent in the EDI file.')
    = Fputs(ereport, ' SKU#: is not sent in the EDI file.')
  Endif
  If Empty(mupc) .And. !Empty(mstyle)
    = Fputs(ereport, ' UPC#: is not sent in the EDI file.')
    = Fputs(ereport, ' SKU#: ' + Alltrim(mstyle + msiz))
  Endif
  If !Empty(mupc) .And. Empty(mstyle)
    = Fputs(ereport, ' UPC#: ' + Alltrim(mupc))
    = Fputs(ereport, ' SKU#: is not sent in the EDI file.')
  Endif
  If !Empty(mupc) .And. !Empty(mstyle)
    = Fputs(ereport, ' UPC#: ' + Alltrim(mupc))
    = Fputs(ereport, ' SKU#: ' + Alltrim(mstyle + msiz))
  Endif
  If !Empty(mstyl)
    = Fputs(ereport, '  Style: ' + Alltrim(mstyl))
  Endif
  If !Empty(mcolor)
    = Fputs(ereport, '  Color: ' + Alltrim(mcolor))
  Endif
  If !Empty(msize)
    = Fputs(ereport, '  Size : ' + Alltrim(msize))
  Endif
  = Fputs(ereport, '  Quantity Ordered: ' + Alltrim(Str(mqty, 10)) + ' ' + muom)
  If minerpack * mpackunit <> 1
    = Fputs(ereport, '  No. of inner packs: ' + Alltrim(Str(minerpack, 10)))
    = Fputs(ereport, '  No. of pieces per pack: ' + Alltrim(Str(mpackunit, 10)))
  Endif
  rflag = .F.
Endproc
**
Procedure lfChkRng
  lccuralias = Alias()
  If !Used(lctmperror)
    otmp = Createobject("GetTempName")
    lctmperror = otmp.Do()
    Create Table (oariaapplication.workdir + lctmperror) (ccode C (240))
    Index On ccode Tag (lctmperror)
    Release otmp
  Endif
  If !Empty(lccuralias)
    Select (lccuralias)
  Endif
  lrange_process = .F.
  cblkord = ""
  If mord = 3
    lmversion = .F.
    = Seek('P' + mkey, 'Spck_Hdr', 'Spck_HdrVr')
    If !spck_hdr.lrange
      Select spck_hdr
      lmaccount = Substr(mkey, 1, 5)
      = Seek('P' + Padr(Alltrim(lmaccount), 5) + Padr(lcpack_id, 16) + lcpack_clr + lcpack_sz, 'Spck_Hdr', 'Spck_HdrVr')
      lmversion = .F.
      lmaccount = Substr(mkey, 1, 5)
      Scan Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + lmaccount + Padr(lcpack_id, 16) + lcpack_clr + lcpack_sz
        If lcpack_vrn <> spck_hdr.cpkversion
          lmversion = .T.
          Exit
        Endif
      Endscan
      = Seek('P' + mkey, 'Spck_Hdr', 'Spck_HdrVr')
      If !lmversion
        If llreject
          If rflag
            = Fputs(ereport, 'File Code#: ' + lcfilecode)
            = Fputs(ereport, 'ACCOUNT#: ' + maccount)
            = Fputs(ereport, 'CUST. PO#: ' + Alltrim(mcustpo))
            Iif(!Empty(&lcstores..Store),Fputs(ereport ,'Store #: ' + Alltrim(&lcstores..Store)),.T.)
            Iif(!Empty(mupc), Fputs(ereport, 'UPC : ' + Alltrim(mupc)), .T.)
            Iif(!Empty(mstyle), Fputs(ereport, 'SKU : ' + Alltrim(mstyle + msiz)), .T.)
            = Fputs(ereport, 'Pack ID : ' + Alltrim(mkey))
            =Fputs(ereport ,'Qty Ordered: '+ Iif(Empty(&lcstores..Store),Alltrim(Str(mqty)),Alltrim(Str(&lcstores..nquantity*mslnqty))))
            = Fputs(ereport, 'Pack Qty : ' + Alltrim(Str(mpckqty)))
            = Fputs(ereport, 'Rejection Reasone : Qty ordered is not divisable by the Pack qty.')
            rflag = .F.
          Endif
          Return
        Endif
      Endif
    Endif
    If Seek(lcfilecode + Padr(mcustpo, 15) + Padr(lcpack_id, 16) + '-' + lcpack_clr + '-' + lcpack_sz, 'ConflctL', 'ConflctL')
      llcheck = Iif(spck_hdr.lrange, !Empty(conflctl.cblkord), !Empty(conflctl.cpkversion))
      If llcheck
        If llreject
          If rflag
            = Fputs(ereport, 'File Code#: ' + lcfilecode)
            = Fputs(ereport, 'ACCOUNT#: ' + maccount)
            = Fputs(ereport, 'CUST. PO#: ' + Alltrim(mcustpo))
            Iif(!Empty(&lcstores..Store),Fputs(ereport ,'Store #: ' + Alltrim(&lcstores..Store)),.T.)
            Iif(!Empty(mupc), Fputs(ereport, 'UPC : ' + Alltrim(mupc)), .T.)
            Iif(!Empty(mstyle), Fputs(ereport, 'SKU : ' + Alltrim(mstyle + msiz)), .T.)
            = Fputs(ereport, 'Pack ID : ' + Alltrim(mkey))
            =Fputs(ereport ,'Qty Ordered: '+ Iif(Empty(&lcstores..Store),Alltrim(Str(mqty)),Alltrim(Str(&lcstores..nquantity*mslnqty))))
            = Fputs(ereport, 'Pack Qty : ' + Alltrim(Str(mpckqty)))
            = Fputs(ereport, 'Rejection Reasone : Qty ordered is not divisable by the Pack qty.')
            rflag = .F.
          Endif
          Return
        Endif
        lcalias = Select()
        Select conflctl
        If spck_hdr.lrange
          lrange_process = .T.
          cblkord = conflctl.cblkord
        Else
          lmversion = .F.
          lcpack_vrn = conflctl.cpkversion
          mkey = Padr(Alltrim(maccount), 5) + lcpack_id + lcpack_clr + lcpack_sz + lcpack_vrn
          = Seek('P' + mkey, 'Spck_Hdr', 'Spck_HdrVr')
          llpack_rng = spck_hdr.lrange
          llpack_prpk = spck_hdr.lpckprpiec
        Endif
        Select (lcalias)
        Replace conflctl.ncounter With 0
      Else
        rflag = .F.
        If !Seek('File Code#: ' + lcfilecode + ', ACCOUNT#: ' + maccount + ' CUST. PO#: ' + Alltrim(mcustpo) + ', Pack Id: ' + spck_hdr.pack_id, (lctmperror))
          Insert Into (lctmperror) (ccode) Values ("File Code#: " + lcfilecode + ", ACCOUNT#: " + maccount + " CUST. PO#: " + Alltrim(mcustpo) + ", Pack Id: " + spck_hdr.pack_id)
          Replace conflctl.ncounter With conflctl.ncounter + 1
          lladdednew = .F.
          If spck_hdr.lrange
            = Fputs(ereport, 'File Code#: ' + lcfilecode + ', ACCOUNT#: ' + maccount + ' CUST. PO#: ' + Alltrim(mcustpo) + ', Pack Id: ' + spck_hdr.pack_id + ' Need to get bulk order to deplet from.')
          Else
            = Fputs(ereport, 'File Code#: ' + lcfilecode + ', ACCOUNT#: ' + maccount + ' CUST. PO#: ' + Alltrim(mcustpo) + ', Pack Id: ' + spck_hdr.pack_id + ' Pack has multi Version And user should specify pack verion.')
          Endif
        Endif
      Endif
    Else
      If spck_hdr.lrange .Or. lmversion
        rflag = .F.
        lcalias = Select()
        Select conflcth
        If !Seek(lcfilecode + mcustpo, 'ConflctH', 'ConflctH')
          Append Blank
          Replace cfilecode With lcfilecode, custpo With mcustpo, account With maccount, rcvedate With edilibdt.ddate, rcvetime With edilibdt.ctime, processdat With oariaapplication.systemdate, processtim With gfgettime(), cadd_user With oariaapplication.user_id
        Endif
        Select conflctl
        Append Blank
        Replace cfilecode With lcfilecode, cpack With spck_hdr.pack_id + '-' + spck_hdr.cpkcolor + '-' + spck_hdr.cpcksize, custpo With mcustpo, lrange With spck_hdr.lrange, cupc With mupc, csku With mstyle + msiz, cvendrnum With mvend, npckqty With mqty, npkslprice With mpri, cpkversion With Iif(spck_hdr.lrange, spck_hdr.cpkversion, ""), ncounter With 1, cadd_user With oariaapplication.user_id
        Replace npcknum With mpckqty
        lladdednew = .T.
        Select (lcalias)
        If !Seek('File Code#: ' + lcfilecode + ', ACCOUNT#: ' + maccount + ' CUST. PO#: ' + Alltrim(mcustpo) + ', Pack Id: ' + spck_hdr.pack_id, (lctmperror))
          Insert Into (lctmperror) (ccode) Values ("File Code#: " + lcfilecode + ", ACCOUNT#: " + maccount + " CUST. PO#: " + Alltrim(mcustpo) + ", Pack Id: " + spck_hdr.pack_id)
        Endif
        If spck_hdr.lrange
          = Fputs(ereport, 'File Code#: ' + lcfilecode + ', ACCOUNT#: ' + maccount + ' CUST. PO#: ' + Alltrim(mcustpo) + ', Pack Id: ' + spck_hdr.pack_id + ' Need to get bulk order to deplet from.')
        Else
          = Fputs(ereport, 'File Code#: ' + lcfilecode + ', ACCOUNT#: ' + maccount + ' CUST. PO#: ' + Alltrim(mcustpo) + ', Pack Id: ' + spck_hdr.pack_id + ' Pack has multi Version And user should specify pack verion.')
        Endif
      Endif
    Endif
  Endif
Endproc
**
Function lfGmaInv
  If mconsol = 'Y'
    Set Order In ordline To ORDLINST
    Select consinvh
    = Seek(xinvoice)
    Scan Rest While invoice + Store + Order = xinvoice
      Select ordline
      = Seek('O' + consinvh.Order + consinvh.Store)
      Scan Rest While cordtype + Order + Store + Style + Str(Lineno, 6) = 'O' + consinvh.Order + consinvh.Store
        If Seek(Order + Str(Lineno, 6) + xinvoice, 'Consinvl', 'CINVLINE')
          Select consinvl
          Scatter Memvar
        Else
          If llinvunship
            Scatter Memvar
            Store 0 To m.qty1, m.qty2, m.qty3, m.qty4, m.qty5, m.qty6, m.qty7, m.qty8, m.totqty, m.price
          Else
            Loop
          Endif
        Endif
        Select consinvl
        = Seek(m.style, 'Style')
        m.nsugretpri = Iif(ordline.nsugretpri = 0, Style.nsugretpri, ordline.nsugretpri)
        If Empty(m.pack_id)
          m.pack_id = Space(16)
          lcgenclrsty = Substr(m.style, 1, lnclrpos - 1) + lcgencolor + Substr(m.style, lnclrpos + lnclrlen)
          If llupcuse .And. Seek(lcgenclrsty, 'Style') .And. !Seek(m.style, 'StyleUpc') .And. Seek(lcgenclrsty, 'StyleUpc')
            m.style = lcgenclrsty
            If !Seek(m.pack_id + m.cpkcolor + m.cpcksize + m.cpkversion + lcgenclrsty, lctmpinvl)
              m.edi_pk_amt = m.totqty * m.price
              m.style = lcgenclrsty
              Insert Into (lctmpinvl) From Memvar
            Else
              Replace qty1 With qty1 + m.qty1, qty2 With qty2 + m.qty2, qty3 With qty3 + m.qty3, qty4 With qty4 + m.qty4, qty5 With qty5 + m.qty5, qty6 With qty6 + m.qty6, qty7 With qty7 + m.qty7, qty8 With qty8 + m.qty8, totqty With totqty + m.totqty, edi_pk_amt With edi_pk_amt + m.totqty * m.price, price With edi_pk_amt / totqty In (lctmpinvl)
            Endif
          Else
            If !Seek(m.pack_id + m.cpkcolor + m.cpcksize + m.cpkversion + m.style, lctmpinvl)
              m.edi_pk_amt = m.totqty * m.price
              m.nsugretamt = m.totqty * m.nsugretpri
              Insert Into (lctmpinvl) From Memvar
            Else
              Replace qty1 With qty1 + m.qty1, qty2 With qty2 + m.qty2, qty3 With qty3 + m.qty3, qty4 With qty4 + m.qty4, qty5 With qty5 + m.qty5, qty6 With qty6 + m.qty6, qty7 With qty7 + m.qty7, qty8 With qty8 + m.qty8, totqty With totqty + m.totqty, edi_pk_amt With edi_pk_amt + m.totqty * m.price, nsugretamt With nsugretamt + m.totqty * m.nsugretpri, price With edi_pk_amt / totqty, nsugretpri With nsugretamt / totqty In (lctmpinvl)
            Endif
          Endif
        Else
          If !Seek(m.pack_id + m.cpkcolor + m.cpcksize + m.cpkversion + Space(19), lctmpinvl)
            m.style = Space(19)
            m.edi_pk_amt = m.totqty * m.price
            Insert Into (lctmpinvl) From Memvar
            Replace &lctmpinvl..price  With invline.npkslprice
          Else
            Replace qty1 With qty1 + m.qty1, qty2 With qty2 + m.qty2, qty3 With qty3 + m.qty3, qty4 With qty4 + m.qty4, qty5 With qty5 + m.qty5, qty6 With qty6 + m.qty6, qty7 With qty7 + m.qty7, qty8 With qty8 + m.qty8, totqty With totqty + m.totqty, npackno With npackno + m.npackno, edi_pk_amt With edi_pk_amt + (m.totqty * m.price) In (lctmpinvl)
            Replace &lctmpinvl..price With &lctmpinvl..price +(m.totqty*m.price)
          Endif
        Endif
        If llinvunship
          Replace ord1 With ord1 + ordline.book1, ord2 With ord2 + ordline.book2, ord3 With ord3 + ordline.book3, ord4 With ord4 + ordline.book4, ord5 With ord5 + ordline.book5, ord6 With ord6 + ordline.book6, ord7 With ord7 + ordline.book7, ord8 With ord8 + ordline.book8, totord With totord + ordline.totbook In (lctmpinvl)
        Endif
      Endscan
    Endscan
    Set Order In ordline To ordline
  Else
    Select ordline
    Set Order In ordline To ORDLINST
    = Seek('O' + morder + mstore)
    Scan Rest While cordtype + Order + Store + Style + Str(Lineno, 6) = 'O' + morder + mstore
      If !Seek(xinvoice + Str(Lineno, 6), 'INVLINE') .And. !llinvunship
        Loop
      Endif
      Select invline
      Scatter Memvar
      = Seek(m.style, 'Style')
      m.nsugretpri = Iif(ordline.nsugretpri = 0, Style.nsugretpri, ordline.nsugretpri)
      If Empty(m.pack_id)
        lcgenclrsty = Substr(m.style, 1, lnclrpos - 1) + lcgencolor + Substr(m.style, lnclrpos + lnclrlen)
        If llupcuse .And. Seek(lcgenclrsty, 'Style') .And. !Seek(m.style, 'StyleUpc') .And. Seek(lcgenclrsty, 'StyleUpc')
          If !Seek(m.pack_id + m.cpkcolor + m.cpcksize + m.cpkversion + lcgenclrsty, lctmpinvl) .Or. lcmaps = 'SLC'
            m.edi_pk_amt = m.totqty * m.price
            m.nsugretamt = m.totqty * m.nsugretpri
            m.style = lcgenclrsty
            Insert Into (lctmpinvl) From Memvar
          Else
            Replace qty1 With qty1 + m.qty1, qty2 With qty2 + m.qty2, qty3 With qty3 + m.qty3, qty4 With qty4 + m.qty4, qty5 With qty5 + m.qty5, qty6 With qty6 + m.qty6, qty7 With qty7 + m.qty7, qty8 With qty8 + m.qty8, totqty With totqty + m.totqty, edi_pk_amt With edi_pk_amt + m.totqty * m.price, nsugretamt With nsugretamt + m.totqty * m.nsugretpri, price With edi_pk_amt / totqty, nsugretpri With nsugretamt / totqty In (lctmpinvl)
          Endif
        Else
          If !Seek(m.pack_id + m.cpkcolor + m.cpcksize + m.cpkversion + m.style, lctmpinvl) .Or. lcmaps = 'SLC'
            m.edi_pk_amt = m.totqty * m.price
            m.nsugretamt = m.totqty * m.nsugretpri
            Insert Into (lctmpinvl) From Memvar
          Else
            Replace qty1 With qty1 + m.qty1, qty2 With qty2 + m.qty2, qty3 With qty3 + m.qty3, qty4 With qty4 + m.qty4, qty5 With qty5 + m.qty5, qty6 With qty6 + m.qty6, qty7 With qty7 + m.qty7, qty8 With qty8 + m.qty8, totqty With totqty + m.totqty, edi_pk_amt With edi_pk_amt + m.totqty * m.price, nsugretamt With nsugretamt + m.totqty * m.nsugretpri, price With edi_pk_amt / totqty, nsugretpri With nsugretamt / totqty In (lctmpinvl)
          Endif
        Endif
      Else
        If !Seek(m.pack_id + m.cpkcolor + m.cpcksize + m.cpkversion + Space(19), lctmpinvl) .Or. lcmaps = 'SLC'
          m.style = ''
          m.edi_pk_amt = m.totqty * m.price
          m.nsugretamt = m.totqty * m.nsugretpri
          Insert Into (lctmpinvl) From Memvar
          Replace &lctmpinvl..price  With invline.npkslprice
        Else
          Replace qty1 With qty1 + m.qty1, qty2 With qty2 + m.qty2, qty3 With qty3 + m.qty3, qty4 With qty4 + m.qty4, qty5 With qty5 + m.qty5, qty6 With qty6 + m.qty6, qty7 With qty7 + m.qty7, qty8 With qty8 + m.qty8, totqty With totqty + m.totqty, nsugretamt With nsugretamt + (m.totqty * m.nsugretpri), edi_pk_amt With edi_pk_amt + (m.totqty * m.price) In (lctmpinvl)
          Replace &lctmpinvl..price With &lctmpinvl..price +(m.totqty*m.price)
        Endif
      Endif
      If llinvunship
        Replace ord1 With ord1 + ordline.book1, ord2 With ord2 + ordline.book2, ord3 With ord3 + ordline.book3, ord4 With ord4 + ordline.book4, ord5 With ord5 + ordline.book5, ord6 With ord6 + ordline.book6, ord7 With ord7 + ordline.book7, ord8 With ord8 + ordline.book8, totord With totord + ordline.totbook In (lctmpinvl)
      Endif
    Endscan
  Endif
  lntotqty = 0
  lntotpcs = 0
  lntotamnt = 0
  Select (lctmpinvl)
  Scan For Iif(llinvunship, .T., totqty <> 0)
    muom = Iif(!Empty(pack_id) .And. llpckdtl, 'AS', 'EA')
    morder = Order
    lcstyle = Style
    mlineno = Lineno
    = Seek('O' + morder + Str(mlineno, 6), 'ORDLINE', 'ORDLINE')
    mstyle = Substr(Style, 1, Len(lcmjrpic))
    mcolor = Iif(llcolor, Substr(Style, lnclrpos, lnclrlen), '')
    *C202353,1 HIA add missing standard fields [Begin]
    mclr = mcolor
    *C202353,1 HIA add missing standard fields [End]
    mscale = Scale
    msize = ''
    = Seek(lcstyle, 'Style')
    mstydesc = Style.Desc
    xsku = lcstyle
    If llcolor .And. Substr(Style, lnclrpos, lnclrlen) <> lcgencolor
      = Seek('N' + Substr(Style, lnclrpos, lnclrlen) + 'N' + 'COLOR', 'CODES', 'CODES')
      mcolor = codes.cdiscrep
    Endif
    Store Space(1) To xupc, xupc1, xupc2, xupc3, xupc4, xupc5, xupc6, xupc7, xupc8
    Store 1 To xpptot
    Store Space(1) To xkupc
    Select (lctmpinvl)
    lcupcstyle = Style
    If llcatdanial
      lcupcstyle = Alltrim(Substr(Style, 1, Len(lcmjrpic)))
      lcupcstyle = Padr(Substr(lcupcstyle, 1, Len(lcupcstyle) - 2), Len(lcmjrpic))
      lcupcstyle = lcupcstyle + Substr(Style, Len(lcmjrpic) + 1)
      lcupcstyle = Iif(Seek(lcupcstyle, 'STYLE'), lcupcstyle, Style)
    Endif
    If llupcuse .And. Seek(lcupcstyle, 'StyleUpc')
      xsku = '**BY SIZE**'
      Select styleupc
      Scan Rest While Style = lcupcstyle
        lcsize = Alltrim(Size)
        xupc&lcsize = cupcnum1 + cupcnum2 + cupcnum3
      Endscan
    Endif
    Select (lctmpinvl)
    = Seek('S' + mscale, 'Scale')
    mkey = account + Style
    If Empty(pack_id)
      Store Style To xsku1, xsku2, xsku3, xsku4, xsku5, xsku6, xsku7, xsku8
      Select spck_lin
      If Seek('S' + mkey)
        xsku = '**BY SIZE**'
        Scan Rest While Type + account + Style = 'S' + mkey
          If lforgsku(Alltrim(pack_id))
            If totqty = 0
              xsku = sku
              =Seek('P'+mscale+&lctmpinvl..prepak,'Scale','Scale')
              xpptot = Scale.pptot
              = Seek('S' + mscale, 'Scale')
            Else
              For lncount = 1 To Scale.Cnt
                lccount = Str(lncount, 1)
                If qty&lccount = 1
                  xsku&lccount = pack_id
                  Exit
                Endif
              Endfor
            Endif
          Endif
        Endscan
      Else
        If xsku <> '**BY SIZE**' And (Seek('P'+mkey) Or Seek('P'+'*****'+&lctmpinvl..Style))
          xsku = pack_id
        Endif
      Endif
    Else
      If Seek('S'+&lctmpinvl..account+Padr(&lctmpinvl..pack_id,19)+&lctmpinvl..cpkcolor+&lctmpinvl..cpcksize,'SPCK_LIN','SPCK_LINST')
        xsku = spck_lin.pack_id
      Else
        lcscaledesc = Iif(Seek('S'+Substr(&lctmpinvl..cpcksize,1,1),'Scale','Scale'),Eval("SCALE.sz" + Alltrim(Str(Val(Right(&lctmpinvl..cpcksize,2))))),"")
        xsku = Alltrim(pack_id) + "-"+Alltrim(&lctmpinvl..cpkcolor)+"-"+lcscaledesc
      Endif
    Endif
    lnpackqty = 1
    Select (lctmpinvl)
    If xsku = '**BY SIZE**'
      For lncount = 1 To Scale.Cnt
        lccount = Str(lncount, 1)
        If Iif(llinvunship,ord&lccount > 0,qty&lccount > 0)
          xtotqty = qty&lccount
          xsku    = xsku&lccount
          xupc    = Alltrim(xupc&lccount)
          xkupc = Iif(!Empty(Alltrim(xupc)), Padl(Alltrim(xupc), 14, '0'), ' ')
          msize   = Scale.sz&lccount
          xprice = price
          xsugretpri = nsugretpri
          xdiscpri = xprice * (100 - xmerdisc) / 100

          *!*C201869,1 AEG 11/09/2016 modi fy GMAINV to hande gros profit and sumary variables for 810 [P20160919.0001]
          xDisc_pcnt = Disc_pcnt
          xtrde_disc= trde_disc
          XGRSPRICE  = Gros_Price
          xlnDscAmt  = xtotqty*Gros_Price*Disc_pcnt/100
          *!*C201869,1 AEG 11/09/2016 modi fy GMAINV to hande gros profit and sumary variables for 810 [P20160919.0001]

          llwritedoc = Iif(ofield2seg.Do(lnfilepoint, '04', 'SycEdiFS', lcmaps), llwritedoc, .F.)
          mctt = mctt + 1
          lntotqty = lntotqty + xtotqty
          lntotpcs = lntotpcs + xtotqty
          lntotamnt = lntotamnt + xtotqty * xprice
        Endif
      Endfor
    Else
      If Iif(llinvunship, .T., totqty > 0)
        xtotqty = totqty
        xprice = price
        xsugretpri = nsugretpri
        lntotpcs = lntotpcs + xtotqty
        xdiscpri = xprice * (100 - xmerdisc) / 100
        Select spck_hdr
        Set Order To Spck_HdrVr
        Select spck_lin
        Set Order To Spck_linvr
        lcactkey = 'P'+&lctmpinvl..account+&lctmpinvl..pack_id+&lctmpinvl..cpkcolor+&lctmpinvl..cpcksize+&lctmpinvl..cpkversion
        lcgenkey = 'P'+'*****'+&lctmpinvl..pack_id+&lctmpinvl..cpkcolor+&lctmpinvl..cpcksize+&lctmpinvl..cpkversion
        lc2key   = &lctmpinvl..cpkcolor+&lctmpinvl..cpcksize+&lctmpinvl..cpkversion
        If Seek(lcactkey) .Or. Seek(lcgenkey)
          lcpackacc = account
          lcccpack_id = Padr(pack_id, 19)
          = Seek('P' + lcpackacc + Padr(Left(pack_id, skutmpl.len1 + skutmpl.len2 + skutmpl.len3), 16) + lc2key, 'SPCK_HDR')
          mstyle = pack_id + "-"+&lctmpinvl..cpkcolor+"-"+&lctmpinvl..cpcksize
          mcolor = ''
          *C202353,1 HIA add missing standard fields [Begin]
          mclr = mcolor
          *C202353,1 HIA add missing standard fields [End]
          msize = ''
          mstydesc = spck_hdr.Desc
          Sum Rest totqty To lnpackqty  While Type+account+pack_id+cpkcolor+cpcksize+cpkversion+Style = 'P'+lcpackacc+&lctmpinvl..pack_id+lc2key
          xpckqty = Iif(lnpackqty<>0, &lctmpinvl..npackno, xtotqty)
          If Empty(lcccpack_id)
            If llupcuse .And. Seek(xsku+&lctmpinvl..cpkcolor+&lctmpinvl..cpcksize,'StyleUpc') And styleupc.lupcpack
              xupc = Alltrim(styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3)
              xkupc = Iif(!Empty(Alltrim(xupc)), Padl(Alltrim(xupc), 14, '0'), ' ')
            Endif
          Else
            If llupcuse .And. Seek('*****'+lcccpack_id+&lctmpinvl..cpkcolor+&lctmpinvl..cpcksize,'StyleUpc','GMAPkUPc') And styleupc.lupcpack
              xupc = Alltrim(styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3)
              xkupc = Iif(!Empty(Alltrim(xupc)), Padl(Alltrim(xupc), 14, '0'), ' ')
            Endif
          Endif
          = Seek(lcactkey, 'SPCK_HDR') .Or. Seek(lcgenkey, 'SPCK_HDR')
          If Int(xpckqty) <> xpckqty
            xpckqty = 1
            xtotqty = Iif(&lctmpinvl..npkslprice = 0 Or &lctmpinvl..lrange Or &lctmpinvl..lpckprpiec, xtotqty,xpckqty)
          Else
            xtotqty = Iif(&lctmpinvl..npkslprice = 0 Or &lctmpinvl..lrange Or &lctmpinvl..lpckprpiec, xtotqty,xpckqty)
          Endif
          lnpackqty = 1
          xprice = Iif(xtotqty=0,xprice,&lctmpinvl..edi_pk_amt/xtotqty)
          xprice = Iif(&lctmpinvl..npkslprice = 0 Or &lctmpinvl..lrange Or &lctmpinvl..lpckprpiec, xprice,&lctmpinvl..npkslprice)
          xsugretpri = Iif(xtotqty=0,xsugretpri,&lctmpinvl..nsugretamt/xtotqty)
          lntotamnt = lntotamnt + Round(xtotqty * lnpackqty * Round(xprice / lnpackqty, 2), 2)
        Endif
        Set Order To SPCKLINS
        Select (lctmpinvl)
        llwritedoc = Iif(ofield2seg.Do(lnfilepoint, '04', 'SycEdiFS', lcmaps), llwritedoc, .F.)
        If !Used('SPCK_LIN1')
          Use Shared (oariaapplication.datadir + 'SPCK_LIN') Again Alias spck_lin1 Order 'Spcklins' In 0
        Else
          Select spck_lin1
          Set Order To SPCKLINS
        Endif
        Select spck_lin
        Set Order To Spck_linvr
        If Type('lcPackAcc')<>"U" And Seek('P'+lcpackacc+&lctmpinvl..pack_id+&lctmpinvl..cpkcolor+&lctmpinvl..cpcksize+&lctmpinvl..cpkversion)
          Store 0 To mslncounter
          Scan Rest While Type+account+pack_id+cpkcolor+cpcksize+cpkversion+Style = 'P'+lcpackacc+&lctmpinvl..pack_id+&lctmpinvl..cpkcolor+&lctmpinvl..cpcksize+&lctmpinvl..cpkversion
            Store ' ' To mslncolor, mslnstydesc
            = Seek(spck_lin.Style, 'Style')
            mslnstydesc = Style.Desc
            = Seek('S' + Style.Scale, 'Scale')
            = Seek(spck_lin.Style + xinvoice, 'InvLine', 'INVLINES')
            mslnprice = invline.price
            mslncolor = Iif(llcolor, Substr(spck_lin.Style, lnclrpos, lnclrlen), '')
            For lnslncount1 = 1 To Scale.Cnt
              Store ' ' To mslnsku, mslnupc, mslnsize
              Store 0 To mslnqty
              lcslncount1 = Str(lnslncount1, 1)
              Select spck_lin
              If qty&lcslncount1. <> 0
                mslnqty     = qty&lcslncount1.
                mslnsize    = Scale.sz&lcslncount1.
                mslncounter = mslncounter + 1
                mslnsize    = Scale.sz&lcslncount1.
                Select spck_lin1
                = Seek('S' + lcpackacc + spck_lin.Style, 'Spck_Lin1', 'SPCKLINS')
                Scan Rest While Type + account + Style + pack_id = 'S' + lcpackacc + spck_lin.Style
                  For lnslncount2 = 1 To 8
                    lcslncount2 = Str(lnslncount2, 1)
                    If qty&lcslncount2. = 1 And lnslncount2 = lnslncount1
                      mslnsku = pack_id
                      Exit
                    Endif
                  Endfor
                Endscan
                If Seek(spck_lin.Style + lcslncount1, 'STYLEUPC', 'STYLEUPC')
                  mslnupc = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
                Endif
                llwritedoc = Iif(ofield2seg.Do(lnfilepoint, '06', 'SycEdiFS', lcmaps), llwritedoc, .F.)
              Endif
            Endfor
            Select spck_lin
          Endscan
        Endif
        Select spck_lin1
        Use
        Select spck_lin
        Set Order To SPCKLINS
        Select (lctmpinvl)
        lntotqty = lntotqty + xtotqty
        mctt = mctt + 1
      Endif
    Endif
  Endscan
  Return llwritedoc
Endfunc
**
Procedure lfUccPck
  Local lntotqty, lntotpcs, lnrow, llfound
  lntotpcs = pack_lin.totqty
  llfound = .F.
  lcpkcolor = ordline.cpkcolor
  lcpcksize = ordline.cpcksize
  lcpkvrson = ordline.cpkversion
  lcpackidval = lcpackid + lcpkcolor + lcpcksize
  If !Seek('P' + maccount + lcpackid + lcpkcolor + lcpcksize + lcpkvrson, 'Spck_Lin', 'Spck_LinVr') .And. !Seek('P' + '*****' + lcpackid + lcpkcolor + lcpcksize + lcpkvrson, 'Spck_Lin', 'Spck_LinVr')
    oobject.processsizes()
    Return
  Endif
  For lnrow = 1 To lntmplbifo
    If latmplbifo(lnrow, 4) = lcpackidval .And. Left(latmplbifo(lnrow, 1), 2) = "$#"
      latmplbifo[lnrow, 7] = latmplbifo(lnrow, 7) + lntotpcs
      llfound = .T.
      If Seek('P' + maccount + lcpackid + lcpkcolor + lcpcksize + lcpkvrson + pack_lin.Style, 'Spck_Lin', 'Spck_linvr') .Or. Seek('P' + '*****' + lcpackid + lcpkcolor + lcpcksize + lcpkvrson + pack_lin.Style, 'Spck_Lin', 'Spck_linvr')
        latmplbifo[lntmplbifo, 8] = latmplbifo(lntmplbifo, 8) + spck_lin.totqty
      Endif
      Exit
    Endif
  Endfor
  If !llfound
    lntmplbifo = lntmplbifo + 1
    Dimension latmplbifo[lntmplbifo, MAX(9, ALEN(latmplbifo, 2))]
    If Seek('P' + maccount + lcpackid + lcpkcolor + lcpcksize + lcpkvrson + pack_lin.Style, 'Spck_Lin', 'Spck_linvr') .Or. Seek('P' + '*****' + lcpackid + lcpkcolor + lcpcksize + lcpkvrson + pack_lin.Style, 'Spck_Lin', 'Spck_linvr')
      latmplbifo[lntmplbifo, 8] = spck_lin.totqty
    Else
      latmplbifo[lntmplbifo, 8] = 0
    Endif
    latmplbifo[lntmplbifo, 1] = "$#" + spck_hdr.account
    latmplbifo[lntmplbifo, 2] = 0
    latmplbifo[lntmplbifo, 3] = ""
    latmplbifo[lntmplbifo, 4] = lcpackid + lcpkcolor + lcpcksize
    latmplbifo[lntmplbifo, 9] = lcpackid + lcpkcolor + lcpcksize
    latmplbifo[lntmplbifo, 5] = ""
    latmplbifo[lntmplbifo, 6] = 0
    latmplbifo[lntmplbifo, 7] = lntotpcs
    If Seek('S' + maccount + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'Spck_Lin', 'Spck_Linst')
      latmplbifo[lntmplbifo, 4] = spck_lin.pack_id
    Endif
    If llupcuse .And. ((Seek(maccount + Padr(lcpackid, 19) + lcpkcolor + lcpcksize, 'StyleUPC', 'GMAPKUPC') .And. styleupc.lupcpack) .Or. (Seek('*****' + Padr(lcpackid, 19) + lcpkcolor + lcpcksize, 'StyleUPC', 'GMAPKUPC') .And. styleupc.lupcpack))
      latmplbifo[lntmplbifo, 5] = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
    Endif
  Endif
Endproc
**
Procedure lfClcPck
  Set Order To Spck_linvr
  = Seek('P' + lctmp_account + lctmp_packid, 'SPCK_LIN')
  Sum totqty To lntmp_totpackqty Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion + Style = 'P' + lctmp_account + lctmp_packid
Endproc
**
Procedure lfCrnInf
  Private lnalias
  lnalias = Alias()
  Select (lctmpcrtninfo)
  Append Blank
  Replace bol_no With mbol, pack_no With mpackno, cart_no With lncartnum, lplt With lplt_flag .And. ediph.lpltshp, asn_ver With Iif(ediph.lpltshp, ediph.cpltlbl, Iif(bol_hdr.ctostorcn = 'C' .Or. pack_hdr.ctostorcn = 'C', ediph.casnlbl2, ediph.casnlbl1)), vnd_name With mshipname, vnd_addr1 With maddress1, vnd_addr2 With maddress2, vnd_city With mcity, vnd_state With mstate, vnd_zip With mzip, shp_name With mshtoname, shp_addr1 With mshtoadd1, shp_addr2 With mshtoadd2, shp_city With mshtocity, shp_state With mshtostat, shp_zip With mshtozip, custpo With mcustpo, dept With mdept
  Replace cststore With lcstore, cstname With lcstname, cstaddr1 With lcstaddr1, cstaddr2 With lcstaddr2, cstcity With lcstcity, cststate With lcststate, cstzip With lcstzip, manuf_id With mmvend, ucc9 With lcucc9no, ucc_check With lcuccchkno, event_cod With mevent_cod, shipvia With mshipvia, Store With mship_code, int_vend With mvend, note1 With mordnote1, note2 With mordnote2, cancelled With mordcanc
  Replace Style With lcstyle, cclrdesc With lcclrdesc, csizedesc With lcsizedesc, cartons With lncartons, totqty With lnqtypcrtn, pack_id With lccrtnsku, cupc With lcupc, cadd_user With oariaapplication.user_id, dadd_date With oariaapplication.systemdate, cadd_time With Time()
  Replace cpro_no With mcpro_no
  = lfsavpck()
  = lfClcPck()
  Select (lctmpcrtninfo)
  If Type("laLblInfo[1,4]") = "C" .And. !Empty(lalblinfo(1, 4))
    Save To Memo mlblinfo All Like lalblinfo
  Endif
  Select (lnalias)
Endproc
**
Procedure lfCrtons
  Private lnalias, lncount, lccount
  Local lctemp
  lnalias = Select()
  Zap In (lccartons)
  Select pack_lin
  = Seek(lcpackno)
  Scan Rest While pack_no = lcpackno
    = Seek('O' + pack_hdr.Order + pack_hdr.Store + Style + Str(nordlineno, 6), 'ORDLINE', 'ORDLINST')
    lcpackid = ordline.pack_id
    lcpkcolor = ordline.cpkcolor
    lcpcksize = ordline.cpcksize
    lcpkvrson = ordline.cpkversion
    If Empty(lcpackid) .And. (Seek('P' + lcaccount + Style, 'Spck_Lin') .Or. Seek('P' + '*****' + Style, 'Spck_Lin'))
      lcpackid = spck_lin.pack_id
    Endif
    lcsize = ' '
    For lncount = 1 To 8
      lccount = Str(lncount, 1)
      If qty&lccount = totqty
        lcsize = lccount
      Endif
    Endfor
    lcstyle = Substr(pack_lin.Style, 1, Len(lcmjrpic))
    If !Seek(Str(pack_lin.npltno, 4) + Str(pack_lin.no_cart, 4), lccartons)
      Insert Into (lccartons) (npltno, no_cart, Style, major, Size, pack_id, cpkcolor, cpcksize, cpkversion, totqty) Values (pack_lin.npltno, pack_lin.no_cart, pack_lin.Style, lcstyle, lcsize, lcpackid, lcpkcolor, lcpcksize, lcpkvrson, pack_lin.totqty)
    Else
      If lasetups(1, 2)
        If Left(Style, Len(Style) - 3) = Left(pack_lin.Style, Len(pack_lin.Style) - 3)
          lctemp = Style
        Else
          lctemp = ''
        Endif
        Replace Style With lctemp, major With Iif(major = Padr(lcstyle, 19), major, ''), Size With Iif(Size = lcsize, Size, ''), pack_id With Iif(pack_id = lcpackid, lcpackid, ''), cpkcolor With lcpkcolor, cpcksize With lcpcksize, cpkversion With lcpkvrson, totqty With totqty + pack_lin.totqty In (lccartons)
      Else
        Replace Style With Iif(Style = pack_lin.Style, Style, ''), major With Iif(major = Padr(lcstyle, 19), major, ''), Size With Iif(Size = lcsize, Size, ''), pack_id With Iif(pack_id = lcpackid, lcpackid, ''), cpkcolor With lcpkcolor, cpcksize With lcpcksize, cpkversion With lcpkvrson, totqty With totqty + pack_lin.totqty In (lccartons)
      Endif
    Endif
  Endscan
  Select (lnalias)
Endproc
**
Procedure lfCreate
  Create Table (oariaapplication.workdir + lccartons) (npltno N (4), no_cart N (4), Style C (19), major C (19), Size C (1), totqty N (6), pack_id C (16), cpkcolor C (6), cpcksize C (3), cpkversion C (4))
  Index On Str(npltno, 4) + Str(no_cart, 4) Tag (lccartons)
Endproc
**
Procedure lfUCCSEK
  If !Empty(pack_id) .And. Seek(maccount + Padr(pack_id, 19) + cpkcolor + Left(cpcksize, 1), 'StyleUpc', 'GMAPKUPC') .And. styleupc.lupcpack
    lcupc = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
  Endif
Endproc
**
Procedure lfAsnCrt
  Private lnalias, lncount, lccount
  lnalias = Select()
  lcold_order = Order('SPCK_LIN')
  Set Order In spck_lin To SPCKLINS
  Zap In (lctmppackline)
  Set Order In (lctmppackline) To 'STANDARD'
  Select pack_lin
  = Seek(lcpackno)
  Scan Rest While pack_no = lcpackno
    = Seek(lcpackno, 'Pack_Hdr')
    = Seek('O' + pack_hdr.Order + Str(pack_lin.nordlineno, 6), 'ORDLINE', 'ORDLINE')
    lcgensty = Substr(Style, 1, lnclrpos - 1) + lcgencolor + Substr(Style, lnclrpos + lnclrlen)
    If llupcuse .And. Seek(lcgensty, 'Style') .And. !Seek(Style, 'StyleUpc') .And. Seek(lcgensty, 'StyleUpc')
      For lncount = 1 To 8
        lccount = Str(lncount, 1)
        If qty&lccount > 0 And  Seek(lcgensty+lccount,'StyleUpc')
          If !Seek(Padr(styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3, 29) + Str(npltno, 4) + Str(no_cart, 4), lctmppackline)
            *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [BGN]
            *!*Insert Into (lctmppackline) (npltno, no_cart, sku, ccarrctnid) Values (pack_lin.npltno, pack_lin.no_cart, styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3, pack_lin.ccarrctnid)
            Insert Into (lctmppackline) (ccrtnvltyp,pltno, no_cart, sku, ccarrctnid) Values (pack_lin.ccrtnvltyp,pack_lin.npltno, pack_lin.no_cart, styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3, pack_lin.ccarrctnid)
            *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [END]
          Endif
          lnshipqty = 0
          lcordlinno = Str(ordline.Lineno, 6)
          If Seek(pack_hdr.Order + lcordlinno, "InvLine", "INVLINEO")
            Select invline
            Sum qty&lccount While Order + Str(Lineno,6) = pack_hdr.Order + lcordlinno To lnshipqty
            Select pack_lin
          Endif
          Replace totqty    With totqty   + pack_lin.qty&lccount , orgqty    With orgqty   + ordline.book&lccount , nshipqty  With nshipqty + lnshipqty            , nopenqty  With nopenqty + ordline.qty&lccount  , weight    With weight   + pack_lin.weight/pack_lin.totqty*pack_lin.qty&lccount, laltstyle With Iif(ordline.Style <> pack_lin.Style , .T. , laltstyle) In (lctmppackline)
        Endif
      Endfor
      Loop
    Endif
    lcprvlnord = Order('SPCK_LIN')
    Set Order In spck_lin To SPCK_LINST
    If !Empty(ordline.pack_id) .And. (Seek('P' + lcaccount + Style + ordline.cpkcolor + ordline.cpcksize + ordline.pack_id + ordline.cpkversion, 'SPCK_LIN', 'SPCK_LINST') .Or. Seek('P' + '*****' + Style + ordline.cpkcolor + ordline.cpcksize + ordline.pack_id + ordline.cpkversion, 'SPCK_LIN', 'SPCK_LINST'))
      Set Order To &lcprvlnord In spck_lin
      lcprvord = Order('STYLEUPC')
      Set Order In styleupc To GMAPKUPC
      lcupc = Iif(llupcuse .And. (Seek(lcaccount + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'StyleUpc', 'Gmapkupc') .Or. Seek('*****' + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'StyleUpc', 'Gmapkupc')), styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3, Space(13))
      Set Order To &lcprvord In styleupc
      If Seek('S' + lcaccount + ordline.pack_id + Space(3) + ordline.cpkcolor + ordline.cpcksize, 'SPCK_LIN', 'SPCK_LINST')
        If !Seek(Padr(spck_lin.pack_id + spck_lin.cpkcolor + spck_lin.cpcksize, 29) + Str(pack_lin.npltno, 4) + Str(pack_lin.no_cart, 4), lctmppackline)
          *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [BGN]
          *!*Insert Into (lctmppackline) (npltno, no_cart, sku, nordlineno, cupc, ccarrctnid) Values (pack_lin.npltno, pack_lin.no_cart, spck_lin.pack_id + spck_lin.cpkcolor + spck_lin.cpcksize, pack_lin.nordlineno, lcupc, pack_lin.ccarrctnid)
          Insert Into (lctmppackline) (ccrtnvltyp,npltno, no_cart, sku, nordlineno, cupc, ccarrctnid) Values (pack_lin.ccrtnvltyp,pack_lin.npltno, pack_lin.no_cart, spck_lin.pack_id + spck_lin.cpkcolor + spck_lin.cpcksize, pack_lin.nordlineno, lcupc, pack_lin.ccarrctnid)
          *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [END]
        Endif
        = Seek('P' + lcaccount + Style + ordline.cpkcolor + ordline.cpcksize + ordline.pack_id + ordline.cpkversion, 'SPCK_LIN', 'SPCK_LINST') .Or. Seek('P' + '*****' + Style + ordline.cpkcolor + ordline.cpcksize + ordline.pack_id + ordline.cpkversion, 'SPCK_LIN', 'SPCK_LINST')
      Else
        If !Seek(Padr(ordline.pack_id + ordline.cpkcolor + ordline.cpcksize, 29) + Str(pack_lin.npltno, 4) + Str(pack_lin.no_cart, 4), lctmppackline)
          *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [BGN]
          *!*Insert Into (lctmppackline) (npltno, no_cart, sku, nordlineno, cupc, ccarrctnid) Values (pack_lin.npltno, pack_lin.no_cart, ordline.pack_id + ordline.cpkcolor + ordline.cpcksize, pack_lin.nordlineno, lcupc, pack_lin.ccarrctnid)
          Insert Into (lctmppackline) ( ccrtnvltyp,npltno, no_cart, sku, nordlineno, cupc, ccarrctnid) Values (pack_lin.ccrtnvltyp,pack_lin.npltno, pack_lin.no_cart, ordline.pack_id + ordline.cpkcolor + ordline.cpcksize, pack_lin.nordlineno, lcupc, pack_lin.ccarrctnid)
          *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [END]
        Endif
      Endif
      lnshipqty = 0
      lcordlinno = Str(ordline.Lineno, 6)
      If Seek(pack_hdr.Order + lcordlinno, "InvLine", "INVLINEO")
        Select invline
        Sum totqty To lnshipqty While Order + Str(Lineno, 6) = pack_hdr.Order + lcordlinno
        Select pack_lin
      Endif
      Replace totqty With totqty + pack_lin.totqty, orgqty With orgqty + ordline.totbook, nshipqty With nshipqty + lnshipqty, nopenqty With nopenqty + ordline.totqty, weight With weight + pack_lin.weight, laltstyle With Iif(ordline.Style <> pack_lin.Style, .T., laltstyle) In (lctmppackline)
    Else
      llpersize = .F.
      lcupcstyle = pack_lin.Style
      For lncount = 1 To 8
        lccount = Str(lncount, 1)
        If pack_lin.qty&lccount > 0
          lcupc = Iif(llupcuse .And. Seek(lcupcstyle + lccount, 'StyleUpc', 'StyleUpc'), styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3, Space(13))
          Select spck_lin
          = Seek('S' + lcaccount + pack_lin.Style)
          Locate Rest  While Type+account+Style+pack_id = 'S'+lcaccount+pack_lin.Style  For   (qty&lccount = 1 Or totqty = 0) And oobject.orgsku(Alltrim(pack_id))
          lcsku = Iif(Found(), Padr(pack_id + cpkcolor + cpcksize, 29), Padr(lcupc, 29))
          If !Empty(lcsku)
            llpersize = .T.



            *C201652 AEG 1/26/2015 modify the locate statment to locate Sku and carton number and palet number and order line  [begin]
            *!*	          IF !SEEK(lcsku + STR(pack_lin.npltno, 4) + STR(pack_lin.no_cart, 4), lctmppackline)
            Select (lctmppackline)
            =Seek(lcsku+Str(pack_lin.npltno,4)+Str(pack_lin.no_cart,4))
            Locate Rest While sku+Str(npltno,4)+Str(no_cart,4) = lcsku+Str(pack_lin.npltno,4)+Str(pack_lin.no_cart,4);
              FOR nordlineno = pack_lin.nordlineno
            If !Found()
              *C201652 AEG 1/26/2015 modify the locate statment to locate Sku and carton number and palet number and order line  [END]

              *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [BGN]
              *!*Insert Into (lctmppackline) (npltno, no_cart, sku, nordlineno, cupc, ccarrctnid) Values (pack_lin.npltno, pack_lin.no_cart, lcsku, pack_lin.nordlineno, lcupc, pack_lin.ccarrctnid)
              Insert Into (lctmppackline) ( ccrtnvltyp,npltno, no_cart, sku, nordlineno, cupc, ccarrctnid) Values (pack_lin.ccrtnvltyp,pack_lin.npltno, pack_lin.no_cart, lcsku, pack_lin.nordlineno, lcupc, pack_lin.ccarrctnid)
              *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [END]
            Endif
            lnshipqty = 0
            lcordlinno = Str(ordline.Lineno, 6)
            If Seek(pack_hdr.Order + lcordlinno, "InvLine", "INVLINEO")
              Select invline
              Sum qty&lccount While Order + Str(Lineno,6) = pack_hdr.Order + lcordlinno To lnshipqty
            Endif
            Select (lctmppackline)
            Replace totqty    With totqty   + pack_lin.qty&lccount , orgqty    With orgqty   + ordline.book&lccount , nshipqty  With nshipqty + lnshipqty            , nopenqty  With nopenqty + ordline.qty&lccount  , weight    With weight   + pack_lin.weight/pack_lin.totqty*pack_lin.qty&lccount, laltstyle With Iif(ordline.Style <> pack_lin.Style , .T. , laltstyle)
          Endif
        Endif
      Endfor
      llfound = llpersize
      If !llpersize
        If Seek('P' + lcaccount + pack_lin.Style, 'Spck_Lin') .Or. Seek('P' + '*****' + pack_lin.Style, 'Spck_Lin')
          If !Seek(Padr(spck_lin.pack_id + spck_lin.cpkcolor + spck_lin.cpcksize, 29) + Str(pack_lin.npltno, 4) + Str(pack_lin.no_cart, 4), lctmppackline)
            *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [BGN]
            *!*Insert Into (lctmppackline) (npltno, no_cart, sku, nordlineno, ccarrctnid) Values (pack_lin.npltno, pack_lin.no_cart, spck_lin.pack_id + spck_lin.cpkcolor + spck_lin.cpcksize, pack_lin.nordlineno, pack_lin.ccarrctnid)
            Insert Into (lctmppackline) ( ccrtnvltyp,npltno, no_cart, sku, nordlineno, ccarrctnid) Values (pack_lin.ccrtnvltyp,pack_lin.npltno, pack_lin.no_cart, spck_lin.pack_id + spck_lin.cpkcolor + spck_lin.cpcksize, pack_lin.nordlineno, pack_lin.ccarrctnid)
            *C201914,1 AEG 1/00/2017 modify LFASNCRT to insert carton type in lctmpackline [END]
          Endif
          lnshipqty = 0
          lcordlinno = Str(ordline.Lineno, 6)
          If Seek(pack_hdr.Order + lcordlinno, "InvLine", "INVLINEO")
            Select invline
            Sum totqty To lnshipqty While Order + Str(Lineno, 6) = pack_hdr.Order + lcordlinno
          Endif
          Replace totqty With totqty + pack_lin.totqty, orgqty With orgqty + ordline.totbook, nshipqty With nshipqty + lnshipqty, nopenqty With nopenqty + ordline.totqty, weight With weight + pack_lin.weight, laltstyle With Iif(ordline.Style <> pack_lin.Style, .T., laltstyle) In (lctmppackline)
          llfound = .T.
        Endif
      Endif
    Endif
  Endscan
  Set Order In spck_lin To (lcold_order)
  Select (lnalias)
Endproc
**
Procedure lfAsnDoA
  Set Step On
  Select spck_lin
  lctest = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion
  *lctest = IIF(EMPTY(lctest),'',lctest )
  Set Order To Spck_linvr
  If Seek('P' + maccount + lctest) .Or. Seek('P' + '*****' + lctest)
    lcpackacc = account
    If Seek('S' + maccount + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'spck_lin', 'spck_linSt')
      msku = spck_lin.pack_id
    Endif
    = Seek('P' + lcpackacc + lctest)
    muom = 'PK'
    mstyle = pack_id
    = Seek(ediacprt.cpartcode, 'EDIPH', 'PARTNER')
    mlassort = (ediph.csnpkd856 = 'Y')
    mqtyppack = 0
    lcpackacc = account
    = Seek('P' + lcpackacc + lctest, 'Spck_Hdr', 'SPCK_HDRVR')
    mstydesc = spck_hdr.Desc
    Scan Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + lcpackacc + lctest
      mqtyppack = mqtyppack + totqty
      mpo401 = totqty
      = Seek(Style, 'Style')
      If Inlist(lcprilevel, 'A', 'B', 'C')
        mpri = mpri + Style.price&lcprilevel * totqty
      Endif
    Endscan
    Set Order In styleupc To GMAPKUPC
    If llupcuse .And. (Seek(lcpackacc + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'StyleUpc') .Or. Seek('*****' + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'StyleUpc')) .And. styleupc.lupcpack
      mupc = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
      mkupc = Iif(!Empty(Alltrim(mupc)), Padl(Alltrim(mupc), 14, '0'), ' ')
    Endif
    If (ediph.csnpkd856 <> 'Y')
      muom = 'EA'
      mqtyppack = 1
      minpckpcrt = 1
      mqtypcrt   = &lctmppackline..totqty
    Else
      mqty = mqty / mqtyppack
      morgqty = morgqty / mqtyppack
      minpckpcrt = &lctmppackline..totqty
      mqtypcrt = mqtyppack
    Endif
  Else
    *@
    *!*	    IF SEEK('S' + maccount + lcitem + lctest)
    *!*	       = SEEK('S' + maccount + PADR(LEFT(lcitem, skutmpl.len1 + skutmpl.len2 + skutmpl.len3), 16) + lctest, 'Spck_Hdr', 'SPCK_HDRVR')
    If Seek('S' + maccount + lcitem + Iif (Empty(lctest),'',lctest))

      = Seek('S' + maccount + Padr(Left(lcitem, skutmpl.len1 + skutmpl.len2 + skutmpl.len3), 16) + Iif (Empty(lctest),'',lctest), 'Spck_Hdr', 'SPCK_HDRVR')
      *@
      mstydesc = spck_hdr.Desc
      = Seek(Style, 'Style')
      If Empty(mstydesc)
        mstydesc = Style.Desc
      Endif
      If Inlist(lcprilevel, 'A', 'B', 'C')
        mpri = Style.price&lcprilevel
      Endif
      If totqty = 1
        For lncount = 1 To 8
          lccount = Str(lncount, 1)
          If qty&lccount = 1
            lcsize = lccount
            Exit
          Endif
        Endfor
      Else
        = Seek('P' + Style.Scale + ordline.prepak, 'Scale', 'Scale')
        xpptot = Scale.pptot
        = Seek('S' + Style.Scale, 'Scale', 'Scale')
      Endif
      mupc   = &lctmppackline..cupc
      mkupc = Iif(!Empty(Alltrim(mupc)), Padl(Alltrim(mupc), 14, '0'), ' ')
      mstyle = Substr(Style, 1, Len(lcmjrpic))
      mcolor = Substr(Style, lnclrpos, lnclrlen)
      If Seek(Style, 'Style') .And. !Empty(lcsize) .And. Seek('S' + Style.Scale, 'SCALE')
        msize = Scale.sz&lcsize
      Endif
      muom = 'EA'
      mlassort = .F.
      mqtyppack = 1
      mpo401 = 1
      minpckpcrt = 1
      mqtypcrt   = &lctmppackline..totqty
    Else
      If llupcuse .And. Seek(Alltrim(lcitem), 'StyleUpc', 'StyUpcN')
        mupc = lcitem
        mkupc = Iif(!Empty(Alltrim(mupc)), Padl(Alltrim(mupc), 14, '0'), ' ')
        msku = ''
        lcupcstyle = styleupc.Style
        If llcatdanial
          lcupcstyle = Substr(styleupc.Style, 1, Len(lcmjrpic))
          lcupcstyle = Padr(Alltrim(lcupcstyle) + ediacprt.cacccode, Len(lcmjrpic))
          lcupcstyle = lcupcstyle + Substr(styleupc.Style, Len(lcmjrpic) + 1)
          lcupcstyle = Iif(Seek(lcupcstyle, 'STYLE'), lcupcstyle, styleupc.Style)
        Endif
        mstyle = Substr(lcupcstyle, 1, Len(lcmjrpic))
        mcolor = Substr(lcupcstyle, lnclrpos, lnclrlen)
        = Seek(lcupcstyle, 'Style')
        If Inlist(lcprilevel, 'A', 'B', 'C')
          mpri = Style.price&lcprilevel
        Endif
        mstydesc = Style.Desc
        = Seek('S' + Style.Scale, 'SCALE')
        lcsize = Alltrim(styleupc.Size)
        msize  = Iif(Empty(lcsize),'',Scale.sz&lcsize)
        muom = 'EA'
        mlassort = .F.
        mqtyppack = 1
        mpo401 = 1
        minpckpcrt = 1
        mqtypcrt  = &lctmppackline..totqty
      Endif
    Endif
  Endif
  Select spck_lin
  Set Order To spck_lin
  *@
  MCONTLINE = MCONTLINE + 1
  MPARENTCT = MORDERCT
  MHLCOUNT  = MHLCOUNT + 1
  MITEMCT   = MHLCOUNT
  *@
Endproc
**
Procedure lfASNOPT
  lcalias = Select()
  Select spck_hdr
  Set Order To Spck_HdrVr
  Select spck_lin
  Set Order To Spck_linvr
  Select (lcalias)
Endproc
**
Procedure lfASNDOB
  lcoldorder = Order('SPCK_LIN')
  Set Order In spck_lin To Spck_linvr
  =Seek('O'+lcorder+Str(&lctmppackline..nordlineno,6),'ORDLINE','ORDLINE')
  lctest = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion
  If Seek('P' + maccount + lctest) .Or. Seek('P' + '*****' + lctest)
    lcpackacc = account
    Scan Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + lcpackacc + lctest
      = Seek(Style, 'Style')
      mqtyppack = mqtyppack + totqty
      mwghtppack = mwghtppack + totqty * Style.nstyweight
    Endscan
  Endif
  Set Order In spck_lin To lcoldorder
Endproc
**
Procedure lfASNDOC
  lctest = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion
  *C202353,1 HIA add missing standard fields [Begin]
  mscale=ordline.Scale
  *C202353,1 HIA add missing standard fields [End]
  Select spck_lin
  Set Order To Spck_linvr
  If Seek('P' + maccount + lctest) .Or. Seek('P' + '*****' + lctest)
    muom = 'PK'
    lcpackacc = account
    If Seek('S' + maccount + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'spck_lin', 'spck_linSt')
      msku = spck_lin.pack_id
    Endif
    = Seek('P' + lcpackacc + lctest)
    mstyle = pack_id
    = Seek(ediacprt.cpartcode, 'EDIPH', 'PARTNER')
    mlassort = (ediph.csnpkd856 = 'Y')
    mqtyppack = 0
    lcpackacc = account
    = Seek('P' + lcpackacc + lctest, 'Spck_Hdr', 'SPCK_HDRVR')
    mstydesc = spck_hdr.Desc
    Scan Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + lcpackacc + lctest
      mqtyppack = mqtyppack + totqty
      mpo401 = totqty
      = Seek(Style, 'Style')
      If Inlist(lcprilevel, 'A', 'B', 'C')
        mpri = mpri + Style.price&lcprilevel * totqty
      Endif
    Endscan
    Set Order In styleupc To GMAPKUPC
    If llupcuse .And. (Seek(lcpackacc + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'StyleUpc') .Or. Seek('*****' + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'StyleUpc')) .And. styleupc.lupcpack
      mupc = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
      mkupc = Iif(!Empty(Alltrim(mupc)), Padl(Alltrim(mupc), 14, '0'), ' ')
    Endif
    If (ediph.csnpkd856 <> 'Y')
      muom = 'EA'
      mqtyppack = 1
      minpckpcrt = 1
      mpo401 = 1
      mqtypcrt   = &lctmppackline..totqty
      lnqtypcrtn = lnqtypcrtn + mqtypcrt
    Else
      lnqtypcrtn = lnqtypcrtn + &lctmppackline..totqty
      mqty = mqty / mqtyppack
      morgqty = morgqty / mqtyppack
      minpckpcrt = &lctmppackline..totqty
      mqtypcrt = mqtyppack
    Endif
  Else
    If Seek('S' + maccount + Padr(msku, 16))
      = Seek('S' + maccount + Padr(Left(msku, skutmpl.len1 + skutmpl.len2 + skutmpl.len3), 16), 'Spck_Hdr')
      mstydesc = spck_hdr.Desc
      = Seek(Style, 'Style')
      If Empty(mstydesc)
        mstydesc = Style.Desc
      Endif
      If Inlist(lcprilevel, 'A', 'B', 'C')
        mpri = Style.price&lcprilevel
      Endif
      If totqty = 1
        For lncount = 1 To 8
          lccount = Str(lncount, 1)
          If qty&lccount = 1
            lcsize = lccount
            Exit
          Endif
        Endfor
      Else
        = Seek('P' + Style.Scale + ordline.prepak, 'Scale', 'Scale')
        xpptot = Scale.pptot
        = Seek('S' + Style.Scale, 'Scale', 'Scale')
      Endif
      mupc   = &lctmppackline..cupc
      mkupc = Iif(!Empty(Alltrim(mupc)), Padl(Alltrim(mupc), 14, '0'), ' ')
      mstyle = Substr(Style, 1, Len(lcmjrpic))
      mcolor = Substr(Style, lnclrpos, lnclrlen)
      If Seek(Style, 'Style') .And. !Empty(lcsize) .And. Seek('S' + Style.Scale, 'SCALE')
        msize = Scale.sz&lcsize
      Endif
      muom = 'EA'
      mlassort = .F.
      mqtyppack = 1
      minpckpcrt = 1
      mpo401 = 1
      mqtypcrt   = &lctmppackline..totqty
      lnqtypcrtn = lnqtypcrtn + mqtypcrt
    Else
      If llupcuse .And. Seek(Padr(msku, 13), 'StyleUpc', 'StyUpcN')
        mupc = Padr(msku, 13)
        mkupc = Iif(!Empty(Alltrim(mupc)), Padl(Alltrim(mupc), 14, '0'), ' ')
        msku = ''
        lcupcstyle = styleupc.Style
        If llcatdanial
          lcupcstyle = Substr(styleupc.Style, 1, Len(lcmjrpic))
          lcupcstyle = Padr(Alltrim(lcupcstyle) + ediacprt.cacccode, Len(lcmjrpic))
          lcupcstyle = lcupcstyle + Substr(styleupc.Style, Len(lcmjrpic) + 1)
          lcupcstyle = Iif(Seek(lcupcstyle, 'STYLE'), lcupcstyle, styleupc.Style)
        Endif
        mstyle = Substr(lcupcstyle, 1, Len(lcmjrpic))
        mcolor = Substr(lcupcstyle, lnclrpos, lnclrlen)
        = Seek(lcupcstyle, 'Style')
        If Inlist(lcprilevel, 'A', 'B', 'C')
          mpri = Style.price&lcprilevel
        Endif
        mstydesc = Style.Desc
        = Seek('S' + Style.Scale, 'SCALE')
        lcsize = Alltrim(styleupc.Size)
        msize  = Iif(Empty(lcsize),'',Scale.sz&lcsize)
        muom = 'EA'
        mlassort = .F.
        mqtyppack = 1
        minpckpcrt = 1
        mpo401 = 1
        mqtypcrt   = &lctmppackline..totqty
        lccrtnsku = ''
        lnqtypcrtn = lnqtypcrtn + mqtypcrt
      Endif
    Endif
  Endif
  Select spck_lin
  Set Order To spck_lin
Endproc
**
Function lforgsku
  Lparameters lcsku
  Return (Atcline('SKU~', ordline.note_mem) = 0 .Or. Atcline('SKU~' + lcsku, ordline.note_mem) > 0)
Endfunc
**
Function lfGetLogFac
  Lparameters lcaccount, lcinvoice
  Private lnalias, lcdistcntr, lncusrecno
  lcdistcntr = Space(8)
  lnalias = Select()
  lncusrecno = Recno('Customer')
  Select consinvh
  If Seek(lcinvoice) .And. Seek('S' + lcaccount + Store, 'Customer') .And. Seek('S' + lcaccount + customer.dist_ctr, 'Customer')
    lcdistcntr = customer.Store
    Scan Rest While invoice = lcinvoice
      If !Seek('S' + lcaccount + Store, 'Customer') .Or. !Seek('S' + lcaccount + customer.dist_ctr, 'Customer') .Or. customer.Store <> lcdistcntr
        lcdistcntr = Space(8)
        Exit
      Endif
    Endscan
  Endif
  If Between(lncusrecno, 1, Reccount('Customer'))
    Goto lncusrecno In 'Customer'
  Endif
  Select (lnalias)
  Return (lcdistcntr)
Endfunc
**
Procedure lfCrtTmp
  If File(oariaapplication.workdir + lctmplines + '.DBF')
    Use In (lctmplines)
    Erase (oariaapplication.workdir + lctmplines + '.*')
  Endif
  Create Table (oariaapplication.workdir + lctmplines) (custpo C (15), Order C (6), Store C (8), cwarecode C (6), Style C (19), pack_id C (16), price N (9, 2), edi_pk_amt N (12, 10), qty1 N (6), qty2 N (6), qty3 N (6), qty4 N (6), qty5 N (6), qty6 N (6), qty7 N (6), qty8 N (6), totqty N (7), ord1 N (6), ord2 N (6), ord3 N (6), ord4 N (6), ord5 N (6), ord6 N (6), ord7 N (6), ord8 N (6), totord N (7), cshpstatus C (2), Date D, reason C (6), csizes C (8), cpkcolor C (6), cpcksize C (3), cpkversion C (4), Lineno N (6))
  Index On custpo + Order + Store + cwarecode + pack_id + cpkcolor + cpcksize + cpkversion + Style Tag (lctmplines)
Endproc
**
Procedure lfGtPkInf
  Set Step On
  lcpklnord = Order('SPCK_LIN')
  Set Order In spck_lin To Spck_linvr
  lcpkhdord = Order('SPCK_HDR')
  Set Order In spck_hdr To Spck_HdrVr
  lcsupcord = Order('StyleUpc')
  Set Order In styleupc To GMAPKUPC
  Select spck_lin
  If Seek('P'+maccount+&lctmplines..pack_id+&lctmplines..cpkcolor+&lctmplines..cpcksize+&lctmplines..cpkversion) Or  Seek('P'+'*****'+&lctmplines..pack_id+&lctmplines..cpkcolor+&lctmplines..cpcksize+&lctmplines..cpkversion)
    lcpackacc = account
    = Seek('P' + lcpackacc + Padr(Left(pack_id, skutmpl.len1 + skutmpl.len2 + skutmpl.len3), 16) + cpkcolor + cpcksize + cpkversion, 'SPCK_HDR')
    mstydesc = spck_hdr.Desc
    Sum Rest totqty To lnpackqty  While Type+account+pack_id++cpkcolor+cpcksize+cpkversion='P'+lcpackacc+&lctmplines..pack_id+ &lctmplines..cpkcolor+&lctmplines..cpcksize+&lctmplines..cpkversion
    xpckqty = Iif(lnpackqty <> 0, xtotqty / lnpackqty, xtotqty)
    If Int(xpckqty) = xpckqty
      xtotqty = xpckqty
      xtotord = Iif(lnpackqty <> 0, xtotord / lnpackqty, xtotord)
      xprice  = Iif(xtotqty<>0,&lctmplines..edi_pk_amt/xtotqty,xprice)
    Else
      lnpackqty = 1
    Endif
    If llupcuse .And. Seek(lcpackacc+&lctmplines..pack_id+&lctmplines..cpcksize,'StyleUpc','Gmapkupc') And styleupc.lupcpack
      xupc = Alltrim(styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3)
      xkupc = Iif(!Empty(Alltrim(xupc)), Padl(Alltrim(xupc), 14, '0'), ' ')
    Endif
  Endif
  Set Order To &lcpklnord In spck_lin
  Set Order To &lcpkhdord In spck_hdr
  Set Order To &lcsupcord In styleupc
Endproc
**
Procedure lfGetSKU
  If Type('lcEdiTrnTyp') <> 'U'
    lcpklnord = Order('SPCK_LIN')
    Set Order In spck_lin To SPCK_LINST
    If Seek('S'+maccount+Padr(&lctmplines..pack_id,19)+&lctmplines..cpkcolor+&lctmplines..cpcksize,'SPCK_LIN') Or  Seek('S'+'*****'+Padr(&lctmplines..pack_id,19)+&lctmplines..cpkcolor+&lctmplines..cpcksize,'SPCK_LIN')
      xsku = spck_lin.pack_id
    Endif
    Set Order To &lcpklnord In spck_lin
  Endif
Endproc
**
Procedure lfUpdPck
  Replace &lctmplines..cpkcolor   With ordline.cpkcolor &lctmplines..cpcksize   With ordline.cpcksize &lctmplines..cpkversion With ordline.cpkversion
Endproc
**
Procedure lfChkErr
  If !Empty(ordline.pack_id) .And. !(ordline.lrange) .And. ordline.npkslprice <> 0
    lmsggma = .T.
    If Seek('P' + ordline.account + ordline.pack_id + ordline.cpkcolor + ordline.cpksize + ordline.cpkversion + ordline.Style, "Spck_lin", "SPCK_LINVR") .Or. Seek('P' + "*****" + ordline.pack_id + ordline.cpkcolor + ordline.cpksize + ordline.cpkversion + ordline.Style, "Spck_lin", "SPCK_LINVR")
      Replace ordline.price With spck_lin.npck_price, ordline.Gros_Price With lngrsprice, ordline.Disc_pcnt With lndiscount
    Endif
  Else
    lmsggma = .F.
  Endif
Endproc
**
Procedure lfSetWar
  For lni = 1 To ogmaobj.ariaform1.cbolocations.ListCount
    If ogmaobj.ariaform1.cbolocations.List(lni, 2) = "MAIN  "
      Exit
    Endif
  Endfor
  ogmaobj.ariaform1.cbolocations.ListIndex = lni
Endproc
**
Procedure lfGetFile
  If Type('lcEdiTrnTyp') = 'U'
    Return
  Endif
  If lceditrntyp = "856"
    lcoutflnam = Iif(Empty(edinet.c856file), edinet.coutfile, edinet.c856file)
  Endif
Endproc
**
Procedure lfCHKFLDRD
  If lnlockfile < 0 .Or. Inlist(lnlockfile, 1, 3, 5, 7)
    lcuser = ''
    lnuserfile = Fopen(Alltrim(oariaapplication.edipath) + "OUTBOX\" + Alltrim(Substr(lcoutflnam, 1, Rat('.', lcoutflnam))) + "usr")
    If lnuserfile > 0
      lcuser = Alltrim(Fgets(lnuserfile))
      = Fclose(lnuserfile)
    Endif
    lllocknet = .F.
    = oariaapplication.Messagebox('TRM00000B00000', 'ALERT', .F., .F., 'Outgoing file is in use by another user, or it is Read only.' + 'All the transactions that should be sent in this file will be ignored.')
  Endif
Endproc
**
Procedure lfINCRSRHC
  lcvalue = _Screen.ActiveForm.ActiveControl.Value
  lcfilename = _Screen.ActiveForm.filename
  lcfld = _Screen.ActiveForm.Field
  lciskey = _Screen.ActiveForm.iskey
  lcfilt = _Screen.ActiveForm.lcfilter
  If Empty(lcfilt)
    If lciskey
      = Seek(Alltrim(lcvalue), lcfilename)
    Else
      lcfield = lcfld + '="' + Alltrim(lcvalue) + '"'
      Select (lcfilename)
      Locate For &lcfield
    Endif
  Else
    lcfield = lcfilt + ' AND ' + lcfld + '="' + Alltrim(lcvalue) + '"'
    Select (lcfilename)
    Locate For &lcfield
  Endif
  If Type('_SCREEN.ACTIVEFORM.GRIDTORefresh') = 'O'
    _Screen.ActiveForm.gridtorefresh.Refresh()
  Endif
Endproc
**
Procedure lfCHNGINDX
  Index On Store + Style + pack_id + cpkcolor + cpcksize + cpkversion Tag ediordpk
  Set Order To ediordpk
Endproc
**
Procedure lfSETKEY
  lcpack = Iif(mord = 3, xpack + Padr(spck_lin.cpkcolor, 6) + Padr(spck_lin.cpcksize, 3) + Padr(spck_lin.cpkversion, 4), xpack)
Endproc
**
Procedure lfGTHPCK
  lccurralis = Alias()
  If Type('ll_Use_XXK') = 'L'
    If ll_use_xxk = .T.
      = Iif(!Empty(pack_lin.pack_id), lfUccPck(), oobject.processsizes())
    Else
      = oobject.processsizes()
    Endif
  Else
    If !Empty(pack_lin.pack_id)
      = lfUccPck()
    Endif
  Endif
  = oobject.processsizes()
  Select (lccurralis)
Endproc
**
Procedure lfsavpck
  lccurralis = Alias()
  Select (lctmpcrtninfo)
  Replace pack_id With &lccartons..pack_id, csku       With &lccartons..csku, cpckacc    With &lccartons..cpckacc  , cpkcolor   With &lccartons..cpkcolor , cpcksize   With &lccartons..cpcksize , cpkversion With &lccartons..cpkversion
  Select (lccurralis)
Endproc
**
Procedure lfCLMCRT
  lccurralis = Alias()
  If Used(lccartons)
    Alter Table (lccartons) Add Column cpckacc 'C' (5)
    Alter Table (lccartons) Add Column cpkcolor 'C' (6)
    Alter Table (lccartons) Add Column cpcksize 'C' (3)
    Alter Table (lccartons) Add Column cpkversion 'C' (4)
    Alter Table (lccartons) Add Column csku 'C' (16)
  Endif
  Select (lccurralis)
Endproc
**
Procedure lfGETCRT
  If Empty(ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion)
    If !Empty(lcsize) .And. Seek('S' + maccount + ordline.Style, 'spck_lin', 'Spcklins')
      Select spck_lin
      Scan Rest While Type + account + Style = 'S' + maccount + ordline.Style
        If spck_lin.qty&lcsize = 1
          Exit
        Endif
      Endscan
      msku = spck_lin.pack_id
      Select pack_lin
    Endif
  Else
    If Seek('S' + maccount + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'spck_lin', 'spck_linSt')
      msku = spck_lin.pack_id
    Endif
  Endif
  If !Seek(Str(pack_lin.npltno, 4) + Str(pack_lin.no_cart, 4), lccartons)
    Insert Into (lccartons) (npltno, no_cart, Style, major, Size, pack_id, totqty, cpckacc, cpkcolor, cpcksize, cpkversion, csku) Values (pack_lin.npltno, pack_lin.no_cart, pack_lin.Style, lcstyle, lcsize, lcpackid, pack_lin.totqty, ordline.cpckacc, ordline.cpkcolor, ordline.cpcksize, ordline.cpkversion, msku)
  Else
    If lasetups(1, 2)
      If Left(Style, Len(Style) - 3) = Left(pack_lin.Style, Len(pack_lin.Style) - 3)
        lctemp = Style
      Else
        lctemp = ''
      Endif
      llsame = (&lccartons..pack_id=lcpackid And &lccartons..cpkcolor = ordline.cpkcolor And &lccartons..cpcksize = ordline.cpcksize And &lccartons..cpkversion = ordline.cpkversion)
      Replace Style With lctemp, major With Iif(major = Padr(lcstyle, 19), major, ''), Size With Iif(!Empty(major) .And. Size = lcsize, Size, ''), pack_id With Iif(llsame, lcpackid, ''), totqty With totqty + pack_lin.totqty, cpkcolor With Iif(llsame, ordline.cpkcolor, ''), cpcksize With Iif(llsame, ordline.cpcksize, ''), cpkversion With Iif(llsame, ordline.cpkversion, ''), csku With Iif(Iif(Empty(lcpackid), !Empty(lctemp), llsame), msku, '') In (lccartons)
    Else
      llsame = (&lccartons..pack_id=lcpackid And &lccartons..cpkcolor = ordline.cpkcolor And &lccartons..cpcksize = ordline.cpcksize And &lccartons..cpkversion = ordline.cpkversion)
      Replace Style With Iif(Style = pack_lin.Style, Style, ''), major With Iif(major = Padr(lcstyle, 19), major, ''), Size With Iif(!Empty(Style) .And. Size = lcsize, Size, ''), pack_id With Iif(llsame, lcpackid, ''), totqty With totqty + pack_lin.totqty, cpkcolor With Iif(llsame, ordline.cpkcolor, ''), cpcksize With Iif(llsame, ordline.cpcksize, ''), cpkversion With Iif(llsame, ordline.cpkversion, ''), csku With Iif(Iif(Empty(lcpackid), Style = pack_lin.Style, llsame), msku, '') In (lccartons)
    Endif
  Endif
Endproc
**
Procedure lfPrcesPck
  Local lntotqty, lntotpcs, lnrow, llfound
  lntotpcs = pack_lin.totqty
  llfound = .F.
  For lnrow = 1 To lnlblinfo
    If lalblinfo(lnrow, 4) = lcpackid .And. Left(lalblinfo(lnrow, 1), 2) = "$#"
      lalblinfo[lnrow, 7] = lalblinfo(lnrow, 7) + lntotpcs
      llfound = .T.
      If Seek('P' + maccount + ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion + pack_lin.Style, 'Spck_Lin', 'Spck_LinVr') .Or. Seek('P' + '*****' + ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion + pack_lin.Style, 'Spck_Lin', 'Spck_LinVr')
        lalblinfo[lnlblinfo, 8] = lalblinfo(lnlblinfo, 8) + spck_lin.totqty
      Endif
      Exit
    Endif
  Endfor
  lcsku = ""
  If Seek('S' + maccount + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'Spck_Lin', 'Spck_LinSt') .Or. Seek('S' + '*****' + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize, 'Spck_Lin', 'Spck_LinSt')
    lcsku = spck_lin.pack_id
  Endif
  If !llfound
    lnlblinfo = lnlblinfo + 1
    Dimension lalblinfo[lnlblinfo, 8]
    If Seek('P' + maccount + ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion + pack_lin.Style, 'Spck_Lin', 'Spck_LinVr') .Or. Seek('P' + '*****' + ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion + pack_lin.Style, 'Spck_Lin', 'Spck_LinVr')
      lalblinfo[lnlblinfo, 8] = spck_lin.totqty
    Else
      lalblinfo[lnlblinfo, 8] = 0
    Endif
    lalblinfo[lnlblinfo, 1] = lcsku
    lalblinfo[lnlblinfo, 2] = 0
    lalblinfo[lnlblinfo, 3] = ""
    lalblinfo[lnlblinfo, 4] = Alltrim(ordline.pack_id) + '-' + Alltrim(ordline.cpkcolor) + '-' + Alltrim(ordline.cpcksize) + '-' + Alltrim(ordline.cpkversion)
    lalblinfo[lnlblinfo, 5] = ""
    lalblinfo[lnlblinfo, 6] = 0
    lalblinfo[lnlblinfo, 7] = lntotpcs
    If llupcuse .And. ((Seek(maccount + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion, 'StyleUPC', 'Gmapkupc') .And. styleupc.lupcpack) .Or. (Seek('*****' + Padr(ordline.pack_id, 19) + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion, 'StyleUPC', 'Gmapkupc') .And. styleupc.lupcpack))
      lalblinfo[lnlblinfo, 5] = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
    Endif
  Endif
Endproc
**
Procedure LFORDRADDSKU
  oobject.addarrayline('Sku1', 'C', 16)
  oobject.addarrayline('Sku2', 'C', 16)
  oobject.addarrayline('Sku3', 'C', 16)
  oobject.addarrayline('Sku4', 'C', 16)
  oobject.addarrayline('Sku5', 'C', 16)
  oobject.addarrayline('Sku6', 'C', 16)
  oobject.addarrayline('Sku7', 'C', 16)
  oobject.addarrayline('Sku8', 'C', 16)
Endproc
**
Procedure LFORDRGETSKU
  Store '' To m.sku1, m.sku2, m.sku3, m.sku4, m.sku5, m.sku6, m.sku7, m.sku8
  If Seek(ordrepln.account + Padr(ordrepln.Store, 8) + ordrepln.Order + Str(ordrepln.Lineno, 6), 'ORDLINE', 'OrdLAcc')
    If !Empty(ordline.pack_id)
      If Seek('S' + ordline.account + Padr(ordline.pack_id, 16) + ordline.cpkcolor + ordline.cpcksize, 'Spck_Lin', 'Spck_LinVr')
        m.sku1 = Alltrim(spck_lin.pack_id)
      Endif
    Else
      lcsize = '1 '
      For lncount = 1 To 8
        lccount = Str(lncount, 1)
        If ordline.book&lccount <> 0
          lcsize = lccount
          If !Empty(lcsize) .And. Seek('S' + ordrepln.account + ordrepln.Style, 'spck_lin', 'Spcklins')
            Select spck_lin
            Locate Rest While Type+account+Style = 'S'+ordrepln.account+ordrepln.Style For qty&lcsize = 1
            If Found()
              m.sku&lcsize = spck_lin.pack_id
            Endif
            Select ordrepln
          Endif
        Endif
      Endfor
    Endif
  Endif
  Store ' ' To m.cordinqno, m.ctrnpurpos
  Store {} To m.tran_date
  If Seek(ordrepln.cordrepno, 'ORDREPHD')
    m.cordinqno = ordrephd.cordinqno
    m.tran_date = ordrephd.tran_date
    m.ctrnpurpos = oobject.lapurpose(Val(ordrephd.ctrnpurpos))
  Endif
  Insert Into (tmpordrepln) From Memvar
Endproc
**
Procedure LFORDSTRP
  oobjariarep.reportfilename = oariaapplication.reporthome + 'EB\GMAOrdRp.RPT'
Endproc
**
Procedure LFSLCATPCK
  lccurralis = Alias()
  lcpkclrcat = &lcselcodes..cpkcolor
  lcpkszcat  = &lcselcodes..cpksize
  lcoldord = Order('SPCK_HDR')
  Set Order In spck_hdr To Spck_HdrVr
  If Seek('P' + styleupc.account + lcpack + lcpkclrcat + lcpkszcat, 'SPCK_HDR', 'SPCK_HDRVR')
    If !Empty(spck_hdr.cpcksize)
      lcscale = Left(spck_hdr.cpcksize, 1)
      msizenrf = ''
      msizedesc = ''
      = Seek('S' + lcscale, 'scale', 'scale')
      For lncount = 1 To 8
        If Str(lncount, 1) = Right(spck_hdr.cpcksize, 1)
          lcsize = Str(lncount, 1)
          Exit
        Endif
      Endfor
      If !Empty(lcscale) .And. Inlist(Alltrim(lcsize), '1', '2', '3', '4', '5', '6', '7', '8') .And. Seek('S' + lcscale, 'Scale')
        msizenrf  = Scale.cnrfcode&lcsize
        msizedesc = Scale.sz&lcsize+ Alltrim(Scale.cscl_desc)
      Else
        lcsize = ''
        msizenrf = ''
        msizedesc = ''
      Endif
    Endif
    If llcolor .And. !Empty(spck_hdr.cpkcolor)
      lccolor = spck_hdr.cpkcolor
      = orelatedflds.Do(lccolor, @lacrltfld, 'COLOR')
      = Seek('N' + lccolor + 'N' + 'COLOR', 'CODES', 'CODES')
      mclrdesc = codes.cdiscrep
    Endif
  Endif
  Set Order In spck_hdr To (lcoldord)
  Select (lccurralis)
Endproc
**
Procedure lfSLCATTRN
  lccurralis = Alias()
  = Seek(Type + cpartner + cselccode + cupc, 'EDICATGD', 'UPCNUM')
  lckey = Type + cpartner + cselccode + cupc
  Select edicatgd
  Locate Rest For Type + cpartner + cselccode + cupc = lckey .And. cedidstat = lcedistatus
  Select (lccurralis)
Endproc
**
Procedure lfERRDPT
  lccurralis = Alias()
  If llfrmup .Or. llmultiup
    If !Empty(ordhdr.dept) .And. !Seek(ordhdr.account + ordhdr.dept, 'CUSTDEPT')
      llerror = .T.
      Messagebox("Cannot update the temp order# " + Alltrim(edilibdt.ceditranno) + " untill department is created.", 16, _Screen.Caption)
      If !Seek('D' + Alltrim(ordhdr.dept), lcerrsf)
        = Fputs(lnerfl, ' Dept ' + Alltrim(ordhdr.dept) + ' does not exist in the system. ')
        Insert Into (lcerrsf) (ctype, ccode) Values ('D', Alltrim(ordhdr.dept))
      Endif
    Endif
  Else
    If !Empty(ordhdr.dept)
      If !Seek(ordhdr.account + ordhdr.dept, 'CUSTDEPT')
        llerror = .T.
        If !Seek('D' + Alltrim(ordhdr.dept), lcerrsf)
          = Fputs(lnerfl, ' Dept ' + Alltrim(ordhdr.dept) + ' does not exist in the system. ')
          Insert Into (lcerrsf) (ctype, ccode) Values ('D', Alltrim(ordhdr.dept))
        Endif
      Endif
    Endif
  Endif
  Select (lccurralis)
Endproc
**
Procedure lfERRDPTP
  lccurralis = Alias()
  If !Empty(mdept)
    If !Seek(maccount + mdept, 'CUSTDEPT')
      llerror = .T.
      = Fputs(lnerfl, ' Dept ' + Alltrim(mdept) + ' does not exist in the system. ')
    Endif
  Endif
  If !Empty(mdept) .And. !Empty(mdeptdesc) .And. Seek(maccount + mdept, 'CUSTDEPT')
    If Alltrim(custdept.cdeptdesc) <> Alltrim(mdeptdesc)
      llerror = .T.
      = Fputs(lnerfl, 'Income Dept. description  does not match exist dept. description in the system. ')
      rflag = .F.
    Endif
  Endif
  Select (lccurralis)
Endproc
**
Procedure lfAssgnOrd
  If llfromedi
    If Type('TmpWebOrd') <> 'U'
      ladata[1] = tmpwebord.Order
    Else
      ladata[1] = Iif(Type('lcEdiOrder') <> 'U', lcediorder, ladata(1))
    Endif
  Endif
Endproc
**
Procedure lfShViaSt
  ***C202342,1 06/16/2020 HIA fix partner setup screen not open, missing class [P20200506.0003][begin]
  gmaediclass = oariaapplication.clientclassdir+"GMAEDI.VCX"
  Set Classlib To  &gmaediclass. Additive
  ***C202342,1 06/16/2020 HIA fix partner setup screen not open, missing class [P20200506.0003][end]
  oformset.ariaform1.PageFrame.page2.AddObject('lblShipVia', 'ARIALABEL')
  With oformset.ariaform1.PageFrame.page2.lblshipvia
    .Caption = 'Ship Via'
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.left = 490
    *.top = 360


    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Top = 377
    *!*	    .Left = 40
    .Left = 8
    .Top = oformset.ariaform1.PageFrame.Height - 15
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]

    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Visible = .T.
  Endwith
  oformset.ariaform1.PageFrame.page2.AddObject('CMBSHVIAST', 'CMBOBOX')
  With oformset.ariaform1.PageFrame.page2.cmbshviast
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.top = 360
    *.left = 542

    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Top = 374
    *!*	.Left = 100
    .Left = 70
    .Top = oformset.ariaform1.PageFrame.Height - 17
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]


    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Visible = .T.
    .Enabled = .F.
    .Width = 122
  Endwith
  With oformset.ariaform1.PageFrame.page2.ariacheckbox1
    .Width = 342
    .WordWrap = .T.
    .Height = 35
  Endwith
  oformset.ariaform1.PageFrame.page2.AddObject('ShpPacks', 'ARIASHAPE')
  With oformset.ariaform1.PageFrame.page2.shppacks
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.left = 360
    *.top = 293



    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Top = 360
    *!*	.Left = 40
    .Left = 10
    .Top = oformset.ariaform1.PageFrame.Height - 15
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]

    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Visible = .F.
    .Width = 400
    .Height = 59
    .BackStyle = 0
  Endwith
  oformset.ariaform1.PageFrame.page2.AddObject('lblSnPkD', 'ARIALABEL')
  With oformset.ariaform1.PageFrame.page2.lblsnpkd
    .Caption = 'Send pack details : '
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.left = 368
    *.top = 296



    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Left = 225
    *!*	    .Top = 377
    .Left = 215
    .Top = oformset.ariaform1.PageFrame.Height - 15
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]

    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Visible = .T.
  Endwith
  oformset.ariaform1.PageFrame.page2.AddObject('lblSnPkD810', 'ARIALABEL')
  With oformset.ariaform1.PageFrame.page2.lblsnpkd810
    .Caption = '810 : '
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.left = 490
    *.top = 296
    .Left = 315

    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Top = 377
    .Top = oformset.ariaform1.PageFrame.Height - 15
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]


    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Visible = .T.
  Endwith
  oformset.ariaform1.PageFrame.page2.AddObject('cmbSnPkD810', 'ARIACOMBOBOX')
  With oformset.ariaform1.PageFrame.page2.cmbsnpkd810
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.left = 542
    *.top = 296
    .Left = 340

    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Top = 374
    .Top = oformset.ariaform1.PageFrame.Height - 20
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]

    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Enabled = .F.
    .Visible = .T.
    .BoundColumn = 2
    .ColumnCount = 2
    .ColumnLines = .F.
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    * .columnwidths = "130,0"
    .ColumnWidths = "60,0"
    *xxxGMA WLD adjust position 02/04/2014 [End]
    .ControlSource = "EDIPH.cSnPkD810"
    .RowSource = "Yes,Y,No,N"
    .RowSourceType = 1
    .Style = 2
    .Value = ''
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.width = 122
    .Width = 60
    *xxxGMA WLD adjust position 02/04/2014 [End]
  Endwith
  oformset.ariaform1.PageFrame.page2.AddObject('lblSnPkD856', 'ARIALABEL')
  With oformset.ariaform1.PageFrame.page2.lblsnpkd856
    .Caption = '856 : '
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.left = 490
    *.top = 322
    .Left = 420

    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Top = 377
    .Top = oformset.ariaform1.PageFrame.Height - 15
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]

    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Visible = .T.
  Endwith
  oformset.ariaform1.PageFrame.page2.AddObject('cmbSnPkD856', 'ARIACOMBOBOX')
  With oformset.ariaform1.PageFrame.page2.cmbsnpkd856
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.left = 542
    *.top = 322
    .Left = 450
    .Top = 374

    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
    *!*	    .Top = 374
    .Top = oformset.ariaform1.PageFrame.Height - 20
    ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]

    *xxxGMA WLD adjust position 02/04/2014 [End]
    .Enabled = .F.
    .Visible = .T.
    .BoundColumn = 2
    .ColumnCount = 2
    .ColumnLines = .F.
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.columnwidths = "130,0"
    .ColumnWidths = "60,0"
    *xxxGMA WLD adjust position 02/04/2014 [End]
    .ControlSource = "EDIPH.cSnPkD856"
    .RowSource = "Yes,Y,No,N"
    .RowSourceType = 1
    .Style = 2
    .Value = ''
    *xxxGMA WLD adjust position 02/04/2014 [Begin]
    *.width = 122
    .Width = 60
    *xxxGMA WLD adjust position 02/04/2014 [End]
  Endwith
  *xxxGMA WLD adjust position 02/04/2014 [Begin]
  *oformset.ariaform1.height = 465
  *oformset.ariaform1.pageframe.height = 425
  *oformset.ariaform1.pageframe.page1.grdpartnertransactions.height = 295
  *oformset.ariaform1.pageframe.page3.grdlabels.height = 370

  ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [begin]
  *!*	oformset.ariaform1.Height = 470
  *!*	oformset.ariaform1.PageFrame.Height = 430
  *!*	oformset.ariaform1.PageFrame.page1.grdpartnertransactions.Height = 300
  *!*	oformset.ariaform1.PageFrame.page3.grdlabels.Height = 200

  oformset.ariaform1.Height = oformset.ariaform1.Height+ 50
  oformset.ariaform1.PageFrame.Height = oformset.ariaform1.PageFrame.Height+ 50
  oformset.ariaform1.PageFrame.page1.grdpartnertransactions.Height = oformset.ariaform1.PageFrame.page1.grdpartnertransactions.Height+50
  oformset.ariaform1.PageFrame.page1.APFSpecialInstruction.Top=oformset.ariaform1.PageFrame.page1.APFSpecialInstruction.Top+50
  oformset.ariaform1.PageFrame.page3.EdtStndIns.Height = oformset.ariaform1.PageFrame.page3.EdtStndIns.Height+50

  ***C201657,RAS 03/12/2015 modi partner setup screen cortol position depend on screen height [end]

  *xxxGMA WLD adjust position 02/04/2014 [End]
Endproc
**
Procedure lfShpRfrsh
  oformset.ariaform1.PageFrame.page2.cmbshviast.Enabled = Inlist(oformset.activemode, 'A', 'E')
  oformset.ariaform1.PageFrame.page2.cmbsnpkd810.Enabled = Inlist(oformset.activemode, 'A', 'E')
  oformset.ariaform1.PageFrame.page2.cmbsnpkd856.Enabled = Inlist(oformset.activemode, 'A', 'E')
Endproc
**
Procedure lfUpdShpv
  lcalis = Alias()
  Select ediph
  Replace cshviast With 'S'
  Replace csnpkd810 With 'N', csnpkd856 With 'N'
  Select (lcalis)
Endproc
**
Procedure lfUpOrdShV
  Replace shipvia With Iif(ediph.cshviast = 'S', '*', lcshipvia)
  Set Order In conflctl To conflctl
  Set Order In conflcth To conflcth
  If Seek(edilibdt.cfilecode + mcustpo, 'CONFLCTL')
    Select conflctl
    Delete Rest While cfilecode + custpo + cpack = edilibdt.cfilecode + mcustpo
    Select conflcth
    If Seek(edilibdt.cfilecode + mcustpo)
      Delete
    Endif
    Select ordhdr
  Endif
Endproc
**
Procedure lfBatchPrc
  oobj = Createobject('EDIMAIN')
  llbatch = Iif(Type('lnSelecet') = "U", llbatch, lnselecet > 1)
  If Type('lnSelecet') = "U"
    llfrstpo = .T.
  Endif
  Do While cfilecode+cpartcode+cintchgseq+cgroupseq+ceditrntyp=Padr(lcfilecode, 6)
    lcgroupacc = cfilecode + cpartcode
    = Seek(cpartcode + '850', 'EDIPD', 'PARTTRANS')
    = Seek(cpartcode, 'EDIPH')
    = Seek(cpartcode, 'EdiAcPrt', 'PARTNER')
    oobject.laslines = Iif(Isnull(ediacprt.laslines), .F., ediacprt.laslines)
    oobject.po1counter = 0
    Store .F. To lldefaccloc
    Do While cfilecode+cpartcode+cintchgseq+cgroupseq+ceditrntyp=lcgroupacc
      lcgroupkey = cfilecode + cpartcode + cintchgseq + cgroupseq + ceditrntyp
      Scan Rest While cfilecode+cpartcode+cintchgseq+cgroupseq+ceditrntyp = lcgroupkey  For   ceditrnst <> 'R' .And. cstatus <> 'U' .And. &lcfilter
        If llmultiware .And. !lldeffileloc .And. !lldefaccloc
          If llfrstpo
            Do Form oariaapplication.screenhome+'EB\ebShpWar.scx' To lcwareset With llbatch, Alltrim(edilibdt.cediref), edilibdt.cpartcode
            llfrstpo = .F.
            lcwarehous = lcwareset
          Endif
          If Empty(lcwarehous)
            Loop
          Endif
          oobject.location = Left(lcwarehous, 6)
          lldefaccloc = Substr(lcwarehous, 7, 1) = 'Y'
          lldeffileloc = Substr(lcwarehous, 8, 1) = 'Y'
        Endif
        = oobj.Process()
        llreturn = oobject.processpo(llbatch)
        = oobject.closeedifile()
        lctran = edilibdt.mtrantext
        lntransno = lntransno + Val(Substr(lctran, At('|LENGTH|', lctran) + 8))
        Set Order In edilibdt To FILEISAGS
      Endscan
    Enddo
  Enddo
Endproc
**
Procedure lfCrTmpOrd

  Do Case
    Case clinestat = 'CT'
      If Seek('O' + ordhdr1.Order + ordline.Store + ordline.Style, 'ORDLINE1', 'ORDLINST')
        llfound = .T.
        If ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion <> ordline1.pack_id + ordline1.cpkcolor + ordline1.cpcksize + ordline1.cpkversion
          llfound = .F.
          Select ordline1
          Scan Rest For pack_id + cpkcolor + cpcksize + cpkversion = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion While cordtype + Order + Store + Style = 'O' + ordhdr1.Order + ordline.Store + ordline.Style
            llfound = .T.
            Exit
          Endscan
          Select ordline
        Endif
        If llfound
          If !Seek('O' + ordline1.Order + Str(ordline1.Lineno, 6), lcordline)
            Append In (lcordline) Blank
            *@
            Select ordline1
          Else
            Select (lcordline)
            *@
          Endif
          *@
          *!*	    SELECT ordline1
          *@
          Scatter Memo Memvar
          m.flag = 'M'
          m.start = Iif(!Empty(ordline.Start), ordline.Start, m.start)
          m.complete = Iif(!Empty(ordline.Complete), ordline.Complete, m.complete)
          Select (lcordline)
          Gather Memo Memvar
        Endif
      Endif
    Case Inlist(clinestat, 'QI', 'QD')
      If Seek('O' + ordhdr1.Order + ordline.Store + ordline.Style, 'ORDLINE1', 'ORDLINST')
        llfound = .T.
        If ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion <> ordline1.pack_id + ordline1.cpkcolor + ordline1.cpcksize + ordline1.cpkversion
          llfound = .F.
          Select ordline1
          Scan Rest For pack_id + cpkcolor + cpcksize + cpkversion = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion While cordtype + Order + Store + Style = 'O' + ordhdr1.Order + ordline.Store + ordline.Style
            llfound = .T.
            Exit
          Endscan
          Select ordline
        Endif
        If llfound
          If !Seek('O' + ordline1.Order + Str(ordline1.Lineno, 6), lcordline)
            Append In (lcordline) Blank
            *@
            Select ordline1
          Else
            Select (lcordline)
            *@
          Endif
          *@
          *!*	    SELECT ordline1
          *@
          Scatter Memo Memvar
          m.flag = 'M'
          m.qty1 = Max(m.qty1 + ordline.qty1, 0)
          m.qty2 = Max(m.qty2 + ordline.qty2, 0)
          m.qty3 = Max(m.qty3 + ordline.qty3, 0)
          m.qty4 = Max(m.qty4 + ordline.qty4, 0)
          m.qty5 = Max(m.qty5 + ordline.qty5, 0)
          m.qty6 = Max(m.qty6 + ordline.qty6, 0)
          m.qty7 = Max(m.qty7 + ordline.qty7, 0)
          m.qty8 = Max(m.qty8 + ordline.qty8, 0)
          m.totqty = m.qty1 + m.qty2 + m.qty3 + m.qty4 + m.qty5 + m.qty6 + m.qty7 + m.qty8
          Select (lcordline)
          Gather Memo Memvar
          If m.totqty = 0
            Replace clinestat With 'DI'
          Endif
        Endif
      Endif
    Case Inlist(clinestat, 'PQ', 'RZ', 'CA')
      Replace dept With Iif(!Empty(ordhdr.dept),ordhdr.dept,&lcordhdr..dept) In (lcordhdr)
      If !Seek('O' + ordhdr1.Order + ordline.Store + ordline.Style, 'ORDLINE1', 'ORDLINST')
        Select ordline
        Replace clinestat With "AI"
        Scatter Memo Memvar
        m.order = ordhdr1.Order
        m.cordtype = "O"
        m.flag = 'N'
        m.clinestat = 'AI'
        m.lineno = ordhdr1.lastline + i
        i = i + 1
        Append In (lcordline) Blank
        Select (lcordline)
        Gather Memo Memvar
      Else
        llfound = .T.
        If ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion <> ordline1.pack_id + ordline1.cpkcolor + ordline1.cpcksize + ordline1.cpkversion
          llfound = .F.
          Select ordline1
          Scan Rest For pack_id + cpkcolor + cpcksize + cpkversion = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion While cordtype + Order + Store + Style = 'O' + ordhdr1.Order + ordline.Store + ordline.Style
            llfound = .T.
            Exit
          Endscan
          Select ordline
        Endif
        If llfound
          If !Seek('O' + ordline1.Order + Str(ordline1.Lineno, 6), lcordline)
            Append In (lcordline) Blank
            *@
            Select ordline1
          Else
            Select (lcordline)
            *@
          Endif
          *@
          *!*	    SELECT ordline1
          *@
          Scatter Memo Memvar
          m.flag = 'M'
          m.qty1 = Iif(ordline.qty1 = 0 .And. !('SIZE1' $ ordline.desc1), m.qty1, ordline.qty1)
          m.qty2 = Iif(ordline.qty2 = 0 .And. !('SIZE2' $ ordline.desc1), m.qty2, ordline.qty2)
          m.qty3 = Iif(ordline.qty3 = 0 .And. !('SIZE3' $ ordline.desc1), m.qty3, ordline.qty3)
          m.qty4 = Iif(ordline.qty4 = 0 .And. !('SIZE4' $ ordline.desc1), m.qty4, ordline.qty4)
          m.qty5 = Iif(ordline.qty5 = 0 .And. !('SIZE5' $ ordline.desc1), m.qty5, ordline.qty5)
          m.qty6 = Iif(ordline.qty6 = 0 .And. !('SIZE6' $ ordline.desc1), m.qty6, ordline.qty6)
          m.qty7 = Iif(ordline.qty7 = 0 .And. !('SIZE7' $ ordline.desc1), m.qty7, ordline.qty7)
          m.qty8 = Iif(ordline.qty8 = 0 .And. !('SIZE8' $ ordline.desc1), m.qty8, ordline.qty8)
          m.totqty = m.qty1 + m.qty2 + m.qty3 + m.qty4 + m.qty5 + m.qty6 + m.qty7 + m.qty8
          m.clinestat = Iif(m.totqty = 0, 'DI', m.clinestat)
          Select (lcordline)
          m.desc1 = ordline.desc1
          Gather Memo Memvar
        Endif
      Endif
    Case clinestat = 'DI'
      *@
      *!*	  IF SEEK('O' + ordhdr1.order + ordline.store + ordline.style, 'ORDLINE1', 'ORDLINST')
      If Seek('O' + ordhdr1.Order + ordline.Style, 'ORDLINE1', 'ORDBLKST')
        *@
        llfound = .T.
        If ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion <> ordline1.pack_id + ordline1.cpkcolor + ordline1.cpcksize + ordline1.cpkversion
          llfound = .F.
          Select ordline1
          Scan Rest For pack_id + cpkcolor + cpcksize + cpkversion = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion While cordtype + Order + Store + Style = 'O' + ordhdr1.Order + ordline.Store + ordline.Style
            llfound = .T.
            Exit
          Endscan
          Select ordline
        Endif
        If llfound
          If !Seek('O' + ordline1.Order + Str(ordline1.Lineno, 6), lcordline)
            Select ordline1
            *@
            Append Blank In (lcordline)
          Else
            Select (lcordline)
          Endif
          *@
          Scatter Memo Memvar
          *@
          *!*	          STORE 0 TO m.qty1, m.qty2, m.qty3, m.qty4, m.qty5, m.qty6, m.qty7, m.qty8, m.totqty
          *!*	          m.flag = 'M'
          *!*	          m.clinestat = 'DI'
          *!*	     INSERT INTO (lcordline) FROM MEMVAR
          For lncount = 1 To 8
            lcsize = Str(lncount,1)
            If "SIZE"+lcsize $ ordline.desc1
              m.qty&lcsize = Iif(ordline.qty&lcsize=0,0,m.qty&lcsize)
              m.flag = 'M'
              m.clinestat = 'DI'
            Endif
          Endfor
          m.totqty = m.qty1+ m.qty2+ m.qty3+ m.qty4+ m.qty5+ m.qty6+ m.qty7+ m.qty8
          Select (lcordline)
          Gather Memvar Memo
          *!*	        ENDIF
          *@

        Endif
      Endif
    Case clinestat = 'PC'
      If Empty(ordline.Store) .And. ordhdr.Multi <> "Y"
        Select ordline1
        = Seek('O' + ordhdr1.Order)
        Scan Rest For Style = ordline.Style While cordtype + Order + Store + Style + Str(Lineno, 6) = 'O' + ordhdr1.Order
          If !Seek('O' + ordline1.Order + Str(ordline1.Lineno, 6), lcordline)
            Append In (lcordline) Blank
            *@
          Else
            Select (lcordline)
            *@
          Endif
          Scatter Memo Memvar
          m.price = Iif(ordline.price = 0, m.price, ordline.price)
          m.Gros_Price = Iif(ordline.Gros_Price = 0, m.Gros_Price, ordline.Gros_Price)
          m.flag = 'M'
          m.Disc_pcnt = Iif(m.Gros_Price = 0, 0, 100 - m.price * 100 / m.Gros_Price)
          m.npkslprice = Iif(ordline.npkslprice = 0, m.npkslprice, ordline.npkslprice)
          m.nsugretpri = Iif(ordline.nsugretpri = 0, m.nsugretpri, ordline.nsugretpri)
          m.clinestat = ordline.clinestat
          Replace bookamt With bookamt - (totqty * ordline1.price) + (totqty * Iif(ordline.price = 0, ordline1.price, ordline.price)), openamt With openamt - (totqty * ordline1.price) + (totqty * Iif(ordline.price = 0, ordline1.price, ordline.price)) In (lcordhdr)
          Select (lcordline)
          Gather Memo Memvar
          Replace note_mem With Chr(13) + "Modifications:" + Chr(13) + ordline.note_mem Additive
          Select ordline1
        Endscan
      Else
        lcprevord = Order('ORDLINE1')
        If !Empty(ordline.pack_id)
          Set Order In ordline1 To Ordblkst
          If Seek('O' + ordhdr1.Order + ordline.Style + ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion + ordline.Store, 'ORDLINE1', 'Ordblkst')
            If !Seek('O' + ordline1.Order + Str(ordline1.Lineno, 6), lcordline)
              Append In (lcordline) Blank
              *@
              Select ordline1
            Else
              Select (lcordline)
              *@
            Endif
            *@
            *!*	     SELECT ordline1
            *@
            Scatter Memo Memvar
            m.price = Iif(ordline.price = 0, m.price, ordline.price)
            m.Gros_Price = Iif(ordline.Gros_Price = 0, m.Gros_Price, ordline.Gros_Price)
            m.flag = 'M'
            m.Disc_pcnt = Iif(m.Gros_Price = 0, 0, 100 - m.price * 100 / m.Gros_Price)
            m.npkslprice = Iif(ordline.npkslprice = 0, m.npkslprice, ordline.npkslprice)
            m.nsugretpri = Iif(ordline.nsugretpri = 0, m.nsugretpri, ordline.nsugretpri)
            m.clinestat = ordline.clinestat
            Replace bookamt With bookamt - (totqty * ordline1.price) + (totqty * Iif(ordline.price = 0, ordline1.price, ordline.price)), openamt With openamt - (totqty * ordline1.price) + (totqty * Iif(ordline.price = 0, ordline1.price, ordline.price)) In (lcordhdr)
            Select (lcordline)
            Gather Memo Memvar
          Endif
        Else
          Set Order In ordline1 To ORDLINST
          If Seek('O' + ordhdr1.Order + ordline.Store + ordline.Style, 'ORDLINE1', 'ORDLINST')
            If !Seek('O' + ordline1.Order + Str(ordline1.Lineno, 6), lcordline)
              Append In (lcordline) Blank
              *@
              Select ordline1
            Else
              Select (lcordline)
              *@
            Endif
            *@
            *!*	     SELECT ordline1
            *@
            Scatter Memo Memvar
            m.price = Iif(ordline.price = 0, m.price, ordline.price)
            m.Gros_Price = Iif(ordline.Gros_Price = 0, m.Gros_Price, ordline.Gros_Price)
            m.flag = 'M'
            m.Disc_pcnt = Iif(m.Gros_Price = 0, 0, 100 - m.price * 100 / m.Gros_Price)
            m.npkslprice = Iif(ordline.npkslprice = 0, m.npkslprice, ordline.npkslprice)
            m.nsugretpri = Iif(ordline.nsugretpri = 0, m.nsugretpri, ordline.nsugretpri)
            m.clinestat = ordline.clinestat
            Replace bookamt With bookamt - (totqty * ordline1.price) + (totqty * Iif(ordline.price = 0, ordline1.price, ordline.price)), openamt With openamt - (totqty * ordline1.price) + (totqty * Iif(ordline.price = 0, ordline1.price, ordline.price)) In (lcordhdr)
            Select (lcordline)
            Gather Memo Memvar
          Endif
        Endif
        Select ordline1
        Set Order To &lcprevord
      Endif
    Case clinestat = 'AI'
      If Seek('O' + ordhdr1.Order + ordline.Store + ordline.Style, 'ORDLINE1', 'ORDLINST') .And. price = ordline1.price .And. ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion = ordline1.pack_id + ordline1.cpkcolor + ordline1.cpcksize + ordline1.cpkversion
        Select ordline1
        Scatter Memo Memvar
        Select ordline
        m.flag = 'M'
        m.qty1 = Iif(qty1 = 0, m.qty1, qty1)
        m.qty2 = Iif(qty2 = 0, m.qty2, qty2)
        m.qty3 = Iif(qty3 = 0, m.qty3, qty3)
        m.qty4 = Iif(qty4 = 0, m.qty4, qty4)
        m.qty5 = Iif(qty5 = 0, m.qty5, qty5)
        m.qty6 = Iif(qty6 = 0, m.qty6, qty6)
        m.qty7 = Iif(qty7 = 0, m.qty7, qty7)
        m.qty8 = Iif(qty8 = 0, m.qty8, qty8)
        m.totqty = m.qty1 + m.qty2 + m.qty3 + m.qty4 + m.qty5 + m.qty6 + m.qty7 + m.qty8
      Else
        Scatter Memo Memvar
        m.flag = 'N'
        If !Eof('Ordline1')
          Select ordline1
          Scan Rest For pack_id + cpkcolor + cpcksize + cpkversion = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion While cordtype + Order + Store + Style = 'O' + ordhdr1.Order + ordline.Store + ordline.Style
            Scatter Memo Memvar
            m.flag = 'M'
            Exit
          Endscan
          Select ordline
        Endif
      Endif
      Insert Into (lcordline) From Memvar
    Case clinestat = 'CA'
      Replace dept With ordhdr.dept In (lcordhdr)
  Endcase
Endproc
**
Procedure lfChkDigit
  Local lnchkdigit, lnsumodd, lnsumeven, lncount
  Store 0 To lnchkdigit, lnsumodd, lnsumeven, lntop
  mucb = Substr(mucb, 1, 16)
  lntop = Len(mucb)
  For lncount = 1 To lntop Step 2
    lnsumodd = lnsumodd + Val(Substr(mucb, lncount, 1))
    lnsumeven = lnsumeven + Val(Substr(mucb, lncount + 1, 1))
  Endfor
  lnchkdigit = Mod(lnsumodd + (lnsumeven * 3), 10)
  mucb = mucb + Iif(lnchkdigit = 0, '0', Str(Int(10 - lnchkdigit), 1))
Endproc
**
Procedure lfAssPck
  Select spck_lin
  lctest = ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion
  Set Order To Spck_linvr
  If Seek('P' + maccount + lctest) .Or. Seek('P' + '*****' + lctest)
    lcpackacc = account
    If !Used('SPCK_LIN1')
      Use Shared (oariaapplication.datadir + 'SPCK_LIN') Again Alias spck_lin1 Order 'Spcklins' In 0
    Else
      Select spck_lin1
      Set Order To SPCKLINS
    Endif
    Store 0 To mslncounter
    lcstyord = Order('Style')
    lcsclord = Order('Scale')
    Set Order In Style To Style
    Set Order In Scale To Scale
    Scan Rest While Type + account + pack_id + cpkcolor + cpcksize + cpkversion = 'P' + lcpackacc + lctest
      = Seek(spck_lin.Style, 'Style') .And. Seek('S' + Style.Scale, 'Scale')
      For lnslncount1 = 1 To Scale.Cnt
        Store ' ' To mslnsku, mslnupc
        Store 0 To mslnqty
        lcslncount1 = Str(lnslncount1, 1)
        Select spck_lin
        If qty&lcslncount1. <> 0
          mslnqty     = qty&lcslncount1.
          mslncounter = mslncounter + 1
          mslnsize    = Scale.sz&lcslncount1.
          Select spck_lin1
          = Seek('S' + lcpackacc + spck_lin.Style, 'Spck_Lin1', 'SPCKLINS')
          Scan Rest While Type + account + Style + pack_id = 'S' + lcpackacc + spck_lin.Style
            For lnslncount2 = 1 To 8
              lcslncount2 = Str(lnslncount2, 1)
              If qty&lcslncount2. = 1 And lnslncount2 = lnslncount1
                mslnsku = pack_id
                Exit
              Endif
            Endfor
          Endscan
          If Seek(spck_lin.Style + lcslncount1, 'STYLEUPC', 'STYLEUPC')
            mslnupc = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
          Endif
          llwritedoc = Iif(ofield2seg.Do(lnfilepoint, '061', 'SycEdiSH', lcmaps), llwritedoc, .F.)
        Endif
      Endfor
    Endscan
    Select spck_lin1
    Use
    Set Order To &lcstyord In Style
    Set Order To &lcsclord In Scale
    Select spck_lin
  Endif
Endproc
**
Procedure lfUpdConf
  Set Order In conflctl To conflctl
  Set Order In conflcth To conflcth
  If Seek(edilibdt.cfilecode + Padr(edilibdt.cediref, 15), 'CONFLCTL')
    Select conflctl
    Scan Rest While cfilecode + custpo + cpack = edilibdt.cfilecode + Padr(edilibdt.cediref, 15)
      If ncounter > 0
        Return
      Endif
    Endscan
    Delete For cfilecode + custpo + cpack = edilibdt.cfilecode + Padr(edilibdt.cediref, 15)
    Select conflcth
    Delete For cfilecode + custpo = edilibdt.cfilecode + Padr(edilibdt.cediref, 15)
  Endif
Endproc
**
Procedure lfUpdPkInf
  Replace cpkcolor With styleupc.cpkcolor, cpksize With styleupc.cpksize
Endproc
**
Procedure lfGetUccNO

  lcalias = Alias()
  If !Used('EDICRTSQ')
    Use (oariaapplication.datadir + 'EDICRTSQ') Order PCKCRTSQ In 0
  Endif
  If Seek(mpackno + Str(lncarton, 6), 'EDICRTSQ')
    Select edicrtsq
    Replace ship_date With bol_hdr.ship_date
    lcucc9 = edicrtsq.ucc9
  Endif
  Select (lcalias)
Endproc
**
Procedure lfUpdUccNo
  Local lcalias, lcaccount
  If oformset.ariaform1.cboacctype.Value = 'A'
    lcalias = Alias()
    lcaccount = Padr(oformset.ariaform1.keycustomercode.keytextbox.Value, 5)
    If !Used('EDICRTSQ')
      Use (oariaapplication.datadir + 'EDICRTSQ') Order edicrtsq In 0
    Endif
    If Seek('A' + lcaccount, 'EDIACPRT', 'Accfact') .And. Empty(ediacprt.ucc9)
      Select Max(edicrtsq.ucc9) As ucc9 From edicrtsq Where account + ucc9 = lcaccount Into Cursor lcTempUcc
      Select lcTempUcc
      Locate
      If !Eof()
        Select ediacprt
        Replace ediacprt.ucc9 With Nvl(lcTempUcc.ucc9, '000000000')
      Else
        Select ediacprt
        Replace ediacprt.ucc9 With '000000000'
      Endif
    Endif
    Select (lcalias)
  Endif
Endproc
**
Procedure lfCrtFile
  Create Table (oariaapplication.workdir + lctmppackline) (npltno N (4), no_cart N (4), sku C (29), totqty N (6), orgqty N (6), nshipqty N (6), nopenqty N (6), nordlineno N (6), weight N (10, 2), cupc C (13), laltstyle L, ccrtnvltyp C (6), ccarrctnid C (25))
Endproc
**
Procedure lfCHKARRY
  For lnrow = 1 To lntmplbifo
    If latmplbifo(lnrow, 9) = pack_lin.Style + pack_lin.pack_id + pack_lin.cpkcolor + pack_lin.cpcksize .And. latmplbifo(lnrow, 6) = lnsizecnt
      latmplbifo[lnrow, 2] = latmplbifo[lnrow, 2] + pack_lin.qty&lcsz
      llfound = .T.
      Exit
    Endif
  Endfor
Endproc
**
Procedure lfADDPCKKEY
  latmplbifo[lntmplbifo, 9] = pack_lin.Style + pack_lin.pack_id + pack_lin.cpkcolor + pack_lin.cpcksize
Endproc
**
Procedure lfASSINDEX
  lcindex = 'PACKSTYPK'
Endproc
**
Procedure lfSETINDEX
  Set Order To PACKSTYPK
Endproc
**
Procedure lfGTPO1UPC
  If mver = 'WLM'
    = Seek(ediacprt.cpartcode + '850', 'EDIPD', 'PARTTRANS')
    m.cupc = lfread(9, Alltrim(asn_ship.custpo) + msku, 'Detail', '850', 'PO1')
  Endif
Endproc
**
Procedure lfCstDiv
  Replace ccustdiv With mcust_div
Endproc
**
Procedure LFGETCSKU
  msku = m.csku
Endproc
**
Function lfGMAFOUND
  llgmasys = File(oariaapplication.datadir + 'GMAACTI.DBF')
  Return llgmasys
Endfunc
**
Procedure lfCRTGMAACTI

  lcgmaacti = ogettempname.Do()
  Create Table (oariaapplication.workdir + lcgmaacti) (account C (5), dprd_bgn D, dprd_end D, dept C (5), Store C (8), tran_type C (2), uom C (3), Lineno N (6), ntotactqty N (12), pack_id C (16), cupc C (13), Style C (19), ccolor C (6), csizedesc C (5), price N (12, 2), nprcpro N (12, 2))
  *@
  Index On account + Dtos(dprd_bgn) + Dtos(dprd_end) + dept + Store + Str(Lineno, 6) + tran_type + cupc + pack_id Tag (lcgmaacti)
  *!*	  INDEX ON account + DTOS(dprd_bgn) + DTOS(dprd_end) + dept + STORE + STR(LINENO, 6) + tran_type TAG (lcgmaacti)
  *@
Endproc
**
Procedure lfUPDATEACTI
  Set Step On
  Select (lcgmaacti)
  Scan
    Scatter Memvar
    *@
    *!*	    IF !SEEK(m.account + DTOS(m.dprd_bgn) + DTOS(m.dprd_end) + m.dept + m.store + STR(m.lineno) + m.tran_type , 'GMAACTI')
    If !Seek(m.account + Dtos(m.dprd_bgn) + Dtos(m.dprd_end) + m.dept + m.store + Str(m.lineno) + m.tran_type + m.cupc+ m.pack_id, 'GMAACTI')
      *@
      Insert Into GMAACTI From Memvar
      Replace cadd_user With 'ADD' In GMAACTI
    Else
      Select GMAACTI
      Gather Memvar
      Replace cadd_user With 'EDT'
    Endif
  Endscan
  Select (lctmpprodacti)
  Scan
    Scatter Memvar
    Select prodacti
    = Seek(m.account + Dtos(m.dprd_bgn) + Dtos(m.dprd_end) + Padr(m.dept, 5) + m.store + m.style + m.tran_type)
    Locate Rest For pack_id = m.pack_id While account + Dtos(dprd_bgn) + Dtos(dprd_end) + dept + Store + Style + tran_type = m.account + Dtos(m.dprd_bgn) + Dtos(m.dprd_end) + Padr(m.dept, 5) + m.store + m.style + m.tran_type
    If !Found()
      Insert Into prodacti (account, dprd_bgn, dprd_end, dept, Store, Style, tran_type, pack_id) Values (m.account, m.dprd_bgn, m.dprd_end, m.dept, m.store, m.style, m.tran_type, m.pack_id)
      = oadduserinfo.Do('ProdActi')
    Endif
    Select prodacti
    Replace pack_id With m.pack_id, Lineno With m.lineno, nactiqty1 With Iif(m.nactiqty1 = 0, nactiqty1, m.nactiqty1), nactiqty2 With Iif(m.nactiqty2 = 0, nactiqty2, m.nactiqty2), nactiqty3 With Iif(m.nactiqty3 = 0, nactiqty3, m.nactiqty3), nactiqty4 With Iif(m.nactiqty4 = 0, nactiqty4, m.nactiqty4), nactiqty5 With Iif(m.nactiqty5 = 0, nactiqty5, m.nactiqty5), nactiqty6 With Iif(m.nactiqty6 = 0, nactiqty6, m.nactiqty6), nactiqty7 With Iif(m.nactiqty7 = 0, nactiqty7, m.nactiqty7), nactiqty8 With Iif(m.nactiqty8 = 0, nactiqty8, m.nactiqty8), ntotactqty With nactiqty1 + nactiqty2 + nactiqty3 + nactiqty4 + nactiqty5 + nactiqty6 + nactiqty7 + nactiqty8
  Endscan
Endproc
**
Procedure lfPROACTI

  Insert Into (lcgmaacti) (account, dprd_bgn, dprd_end, dept, Store, tran_type, uom, Lineno, ntotactqty, pack_id, cupc, Style, ccolor, csizedesc, price, nprcpro) Values (lcaccount, ldprdbgn, ldprdend, lcdept, lcstore, lcacttype, muom, lnlineno, lnqty, mstyle, mupc, mstyl, mcolor, msize, msrp, mppro)
  If Empty(lcorder)
    Return
  Endif
Endproc
**
Procedure lfAddActi

  = oobjct.addactivity(maccount, mdept, Padr(mstore, 8), mprdbgn, mprdend, lcorder, lcitemkey, mqty, macttype)
Endproc
**
Procedure lfEdtData
  oformset.ariaform1.AddObject('chkEditData', 'ARIACHECKBOX')
  With oformset.ariaform1.chkeditdata
    .Caption = 'Edit Data'
    .Left = 11
    .Top = 137
    .Visible = .T.
    .Value = .F.
    .TabIndex = 9
  Endwith
Endproc
**
Procedure lfOpnEdtData
  If ovslblform.ariaform1.chkeditdata.Value
    Select lcaliastmprpt
    tgmaselect = Select()
    Select (ovslblform.tmpouth)
    Do Form oariaapplication.screenhome+'AS\GMAEDTDT.scx' With ovslblform.tmpouth, tgmaselect, Iif(ovslblform.ariaform1.chkprintdetailedlabel.Value, 1, 0)
  Endif
Endproc
**
Procedure lfINVCRT
  Create Table (oariaapplication.workdir + lctmpinvl) (Order C (6), account C (5), Style C (19), pack_id C (16), cpkcolor C (6), cpcksize C (3), cpkversion C (4), Lineno N (6), price N (9, 2), Scale C (3), edi_pk_amt N (12, 10), qty1 N (6), qty2 N (6), qty3 N (6), qty4 N (6), qty5 N (6), qty6 N (6), qty7 N (6), qty8 N (6), totqty N (7), ord1 N (6), ord2 N (6), ord3 N (6), ord4 N (6), ord5 N (6), ord6 N (6), ord7 N (6), ord8 N (6), totord N (7), prepak C (1), nsugretpri N (8, 2), nsugretamt N (12, 2), npkslprice N (8, 2), npackno N (6), lrange L (1), lpckprpiec L (1), Gros_Price N (9, 2), Disc_pcnt N (6, 2), mcon_sku M)
  Index On pack_id + cpkcolor + cpcksize + cpkversion + Style Tag (lctmpinvl)
  lctmpstores = ogettempname.Do()
  Create Table (oariaapplication.workdir + lctmpstores) (Style C (19), pack_id C (16), cpkcolor C (6), cpcksize C (3), cpkversion C (4), Store C (8), qty1 N (6), qty2 N (6), qty3 N (6), qty4 N (6), qty5 N (6), qty6 N (6), qty7 N (6), qty8 N (6), totqty N (7))
  Index On pack_id + cpkcolor + cpcksize + cpkversion + Style + Store Tag (lctmpstores)
Endproc
**
Procedure lfSetVar
  = Seek(ediacprt.cpartcode, 'EDIPH', 'PARTNER')
  llpckdtl = (ediph.csnpkd810 = 'Y')
Endproc
**
Procedure lfSetSTR
  If invhdr.Consol = 'Y' .And. Seek(invhdr.invoice, 'ConsInvH')
    mstore = consinvh.Store
  Else
    mstore = invhdr.Store
  Endif
Endproc
**
Procedure lfCRTGMAACT2
  lcgmaact2 = ogettempname.Do()
  Create Table (oariaapplication.workdir + lcgmaact2) (cpartcode C (6), dprd_bgn D, dprd_end D, dept C (5), Store C (8), Lineno N (6, 0), tran_type C (2), pack_id C (16), cupc C (13), uom C (3), ntotactqty N (12, 0), Style C (19), ccolor C (6), csizedesc C (5), price N (12, 2), nprcpro N (12, 2), cfilecode C (6), cintchgseq C (9), cgroupseq C (9), ctranseq C (14), cust_sku C (16))
  Index On cpartcode + Dtos(dprd_bgn) + Dtos(dprd_end) + dept + Store + Str(Lineno, 6) + tran_type Tag (lcgmaact2)
  Select (lcgmaact2)
  = CursorSetProp("Buffering", 3, lcgmaact2)
  = CursorSetProp("Buffering", 5, lcgmaact2)
Endproc
**
Function lfADDACT2

  Insert Into (lcgmaact2) (cpartcode, dprd_bgn, dprd_end, dept, Store, tran_type, uom, Lineno, ntotactqty, pack_id, cupc, Style, ccolor, csizedesc, price, nprcpro, cfilecode, cintchgseq, cgroupseq, ctranseq, cust_sku) Values (lcgmapartcode, ldgmaprdbgn, ldgmaprdend, lcgmadept, lcgmastore, lcgmaacttype, muom, lngmalineno, lngmaqty, mstyle, mupc, mstyl, mcolor, msize, msrp, mppro, edilibdt.cfilecode, edilibdt.cintchgseq, edilibdt.cgroupseq, edilibdt.ctranseq, mstyle)
  Return .T.
Endfunc
**
Procedure lfUPDATEACT2
  Select (lcgmaact2)
  Goto Top
  If Reccount() > 0
    lcsetclass = Set('CLASSLIB')
    Set Classlib To (oariaapplication.lcaria4class + "MAIN.VCX"), (oariaapplication.lcaria4class + "UTILITY.VCX")
    lctrancode = oariaapplication.remotecompanydata.begintran(oariaapplication.activecompanyconstr, 3, '', .T.)
    Set Classlib To &lcsetclass.
    lcpk_expr = 'cPartcode+Dtos(dPrd_Bgn)+Dtos(dPrd_End)+DEPT+Store+Str(Lineno,6)+Tran_Type'
    lnresult = oobjct.sql_update(lcgmaact2, lctrancode, Set("DATASESSION"), lcpk_expr, 'GMAACT2')
    If lnresult <= 0
      = oariaapplication.remotecompanydata.checkretresult("SQLUPDATE", lnresult, .T.)
      = oariaapplication.remotecompanydata.rollbacktran(lctrancode)
    Else
      If oariaapplication.remotecompanydata.committran(lctrancode, .T.) = 1
      Else
        = oariaapplication.remotecompanydata.checkretresult("COMMITTRAN", lnresult, .T.)
      Endif
    Endif
    Use In (lcgmaact2)
    Erase (lcgmaact2 + '.*')
  Endif
Endproc
**
Procedure lfRPLCPk
  Select (othis.tmpouth)
  Replace pack_id With Iif(!Empty(pack_id) .And. !Empty(csku), csku, pack_id) All
Endproc
**
Procedure lfSTUNLSH
  munlocode = Alltrim(warehous.cunloc)
Endproc
**
Procedure lfSTUNLRS
  Select (lctmplines)
  Goto Top
  If !Eof()
    mwarecode = cwarecode
    = Seek(mwarecode, 'WareHous')
    munlocode = Alltrim(warehous.cunloc)
  Endif
Endproc
**
Procedure lfADDSLNSV
  If mmapset = 'LBY' And ediph.cRcvSln = 'N'
    =othis.SegToField()
  Endif
Endproc
**
***
*** ReFox - all is not lost
***

***C202384,1 09/15/2020 HIA trigger to re-format the sku as style when procress Shopify [P20200506.0003]
*** Developer: Hassan.I
*** Created: 09/15/2020
*** Purpose: re-format the sku as style when procress Shopify
*** Parameter: SKU comes from shoify
*** Return: SKU comes from shopify, in format of Aria Style
Procedure lfSKUSTY

  Store 0 To lnclrlen ,lnclrpos ,lnSizeLen ,lnSizePos ,lnMajLen
  Store '' To lcClrSpr
  Dimension laItemSeg[1]
  loGetItemMask = Createobject("GetItemMask")
  =loGetItemMask.Do(@laItemSeg)
  For lncount = 1 To Alen(laItemSeg,1)
    Do Case
      Case laItemSeg[lnCount,1]='F'
        lnMajLen = Len(laItemSeg[lnCount,3])
        lcClrSpr = Allt(laItemSeg[lnCount,6])
      Case laItemSeg[lnCount,1]='C'
        lnclrlen = Len(laItemSeg[lnCount,3])
        lnclrpos = laItemSeg[lnCount,4]
      Case laItemSeg[lnCount,1]='S'
        lnSizeLen = Len(laItemSeg[lnCount,3])
        lnSizePos = laItemSeg[lnCount,4]
    Endcase
  Endfor
  major = ''
  If !Empty(mstyl)
    If !Empty(lcClrSpr) And lcClrSpr $ mstyl
      major = Substr(mstyl,1,At(lcClrSpr, mstyl)-1)
      RestStylePart = Substr(mstyl,At(lcClrSpr, mstyl))
      If !Empty(major)
        major = Padr(major, lnMajLen, ' ') + RestStylePart
      Endif
    Endif
    If !Empty(major)
      mstyl = major
    Endif
  ENDIF
ENDPROC
