*!**************************************************************************
*! Name      : ARPPRO.prg
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Program For Puffa (UK) (Pro_Forma)C200858 [T20070607.0002]
*!**************************************************************************
*! Modifications
*B608379,1 MMT 12/11/2007 fix bug of wrong Vat Amount[T20071025.0005]
*B609529,1 SAB 12/11/2007 Print Pik-Qty in pre-forma report in partial pikking case[T20101027.0012]
*!**************************************************************************


  llTax     = (gfGetMemVar('M_TAX') = 'Y')
  lcTaxDesc = gfGetMemVar('M_TAX_DESC')
  lcTaxMeth = gfGetMemVar('M_TAX_METH')

  STORE 0 TO lnTerDiscR, lnTaxRate
  DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laFactor[5,1],laEngStyTax[1,2]
  laFactor = ''
  loogScroll.cCROrientation = 'P'
  laCompAdd = ''          && Array to hold the Company address
  laSoldTo = ''           && Array to hold the Sold To address
  laShipTo = ''           && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  lcSolTName = ''        && Variable to hold the Sold to name
  lcShpTName = ''        && Variable to hold the Ship to name
  lcShipVia = ''         && Variable to hold the Ship Via Description
  lcTerms = ''           && Variable to hold the Terms Description
  lcSpkLin = ''          && Variable to hold the Size # of the SKU
  llEndGroup = .F.       && Flag to know if we are at the end of the Group
  lcDivLName = ''        && Variable to hold the Division long name

  lcFacName  = ''

  =gfOpenFile(gcsyshome+'SYCFACT',gcsyshome+'cfaccode','SH')

  llNotePad  = llRpInvNot
  llLineNote = .F.

  DIME laSku[8]
  STORE '' TO laSku
  llEndGroup = .F.       && Flag to know if we are at the end of the Group
  STORE .F. TO llPrtSku
  STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl
  STORE '' TO lcStrToPrn

  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lnClrLnGl  = LEN(laItemSeg[lnCount,3])
      lnClrPosGL = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR


  DECLARE laItemSeg[1]
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='F'
      lnStyLnGl  = LEN(laItemSeg[lnCount,3])
      lnStyPosGl = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR


  llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
               'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo


  LOCAL lnRemoteResult, lcSelectCommand
  lcSelectCommand = [SELECT * FROM SYCCOMP WHERE CCOMP_ID = '] + oAriaApplication.ActiveCompanyID+ [']

  lnRemoteResult = loOGScroll.SQLExecute("SYCCOMP", lcSelectCommand,"","SYCCOMP","",;
      oAriaApplication.SystemConnectionString,3,"")
  IF lnRemoteResult >= 1 
    SELECT SYCCOMP
    LOCATE 
    IF EOF()
      RETURN .F.
    ENDIF 
  ELSE
    RETURN .F.
  ENDIF 

  *-- Open the company table remotly ..... END
  llMulCurr = gfGetMemVar('llMulCurr',gcAct_Comp)
  lcCurrPost = "LEFT "                              && Default Value.
  IF llMulCurr
    lcSelectCommand = [SELECT cCurrency, cCurrencyI FROM SYCINT WHERE cCurrCode = '] + SycComp.cCont_Code + [']
    lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand,"","SYCINT","",;
        oAriaApplication.SystemConnectionString,3,"")

    IF lnRemoteResult >= 1 
      SELECT SYCINT
      LOCATE 
      IF FOUND()
        lcCurrPost = SycInt.cCurrency
      ENDIF 
    ENDIF 
    IF !USED('SYCCURR')
      =gfOpenFile(gcsyshome + "SYCCURR" , "CCURRCODE" , 'SH')
    ENDIF
  ENDIF



  lcTaxRefDs = ALLTRIM(gfGetMemVar('M_TAX_REFE'))



  SELECT SYCCOMP

  lcCompName = cCom_Name             && Variable to hold the Company Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  lcCompFax = cCom_Fax               && Variable to hold the Company Fax
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)

  lcCompFax = TRANSFORM(lcCompFax , '@R ' +  lcPhonPict)  && Fax No. Pic


  =lfAdrShift('laCompAdd')

  lcCompPhone = TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)
  
IF looGscroll.llOGFltCh

  DIMENSION laStructOrdhdr[1,18]
  SELECT Ordhdr
  lnLength = AFIELDS(laStructOrdhdr)
  DIMENSION laStructOrdhdr[lnLength +6,18] 

  laStructOrdhdr[lnLength +1,1] = 'PIKTKT'
  laStructOrdhdr[lnLength +1,2] = 'C'
  laStructOrdhdr[lnLength +1,3] = 6
  laStructOrdhdr[lnLength +1,4] = 0
    
  laStructOrdhdr[lnLength +2,1] = 'nTrde_Disc'
  laStructOrdhdr[lnLength +2,2] = 'N'
  laStructOrdhdr[lnLength +2,3] = 5
  laStructOrdhdr[lnLength +2,4] = 2

  laStructOrdhdr[lnLength +3,1] = 'nMerchTax'
  laStructOrdhdr[lnLength +3,2] = 'N'
  laStructOrdhdr[lnLength +3,3] = 13
  laStructOrdhdr[lnLength +3,4] = 4

  laStructOrdhdr[lnLength +4,1] = 'Tax_Amt'
  laStructOrdhdr[lnLength +4,2] = 'N'
  laStructOrdhdr[lnLength +4,3] = 13
  laStructOrdhdr[lnLength +4,4] = 2
  
  
  laStructOrdhdr[lnLength +5,1] = 'nShipAmt'
  laStructOrdhdr[lnLength +5,2] = 'N'
  laStructOrdhdr[lnLength +5,3] = 14
  laStructOrdhdr[lnLength +5,4] = 2

  laStructOrdhdr[lnLength +6,1] = 'Discount'
  laStructOrdhdr[lnLength +6,2] = 'N'
  laStructOrdhdr[lnLength +6,3] = 13
  laStructOrdhdr[lnLength +6,4] = 2

  
  

  FOR lnCount = 1 TO 6 
    FOR lnInc=7 TO 16
      STORE SPACE(1) TO laStructOrdhdr[lnLength +lnCount ,lnInc]
    ENDFOR 
    STORE 0  TO laStructOrdhdr[lnLength +lnCount ,17], laStructOrdhdr[lnLength +lnCount ,18]
  ENDFOR  

  =gfCrtTmp(lcTmpOrdhdr ,@laStructOrdhdr,IIF(lcRpPrnBy = 'S','order',"PIKTKT"),lcTmpOrdhdr,.T.)





  DIMENSION laStructLine[1,18]
  SELECT Ordline
  lnLengthL = AFIELDS(laStructLine)
  DIMENSION laStructLine[lnLengthL +8,18]

  laStructLine[lnLengthL +1,1] = 'SZ1'
  laStructLine[lnLengthL +1,2] = 'C'
  laStructLine[lnLengthL +1,3] = 5
  laStructLine[lnLengthL +1,4] = 0

  laStructLine[lnLengthL +2,1] = 'SZ2'
  laStructLine[lnLengthL +2,2] = 'C'
  laStructLine[lnLengthL +2,3] = 5
  laStructLine[lnLengthL +2,4] = 0

  laStructLine[lnLengthL +3,1] = 'SZ3'
  laStructLine[lnLengthL +3,2] = 'C'
  laStructLine[lnLengthL +3,3] = 5
  laStructLine[lnLengthL +3,4] = 0

  laStructLine[lnLengthL +4,1] = 'SZ4'
  laStructLine[lnLengthL +4,2] = 'C'
  laStructLine[lnLengthL +4,3] = 5
  laStructLine[lnLengthL +4,4] = 0

  laStructLine[lnLengthL +5,1] = 'SZ5'
  laStructLine[lnLengthL +5,2] = 'C'
  laStructLine[lnLengthL +5,3] = 5
  laStructLine[lnLengthL +5,4] = 0

  laStructLine[lnLengthL +6,1] = 'SZ6'
  laStructLine[lnLengthL +6,2] = 'C'
  laStructLine[lnLengthL +6,3] = 5
  laStructLine[lnLengthL +6,4] = 0

  laStructLine[lnLengthL +7,1] = 'SZ7'
  laStructLine[lnLengthL +7,2] = 'C'
  laStructLine[lnLengthL +7,3] = 5
  laStructLine[lnLengthL +7,4] = 0

  laStructLine[lnLengthL +8,1] = 'SZ8'
  laStructLine[lnLengthL +8,2] = 'C'
  laStructLine[lnLengthL +8,3] = 5
  laStructLine[lnLengthL +8,4] = 0

  FOR lnI = 1 TO 8
    FOR lnInc = 7 TO 16
      STORE SPACE(1) TO laStructLine[lnLengthL +lnI,lnInc]
    ENDFOR 
    STORE 0  TO laStructLine[lnLengthL +lnI,17], laStructLine[lnLengthL +lnI,18]
  ENDFOR

  =gfCrtTmp(lcTmpOrdline,@laStructLine,IIF(lcRpPrnBy = 'S','order','PIKTKT')+'+STYLE',lcTmpOrdline,.T.)


  *Customr check
  lnPosCust = ASCAN(loOGScroll.laogFXflt,'CUSTOMER.ACCOUNT')
  lcCustFile = ''
  llSelCust = .F.
  IF lnPosCust <> 0 
    lnPosCust = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosCust,1)
    lcCustFile    =  loOGScroll.laogFXflt[lnPosCust,6]
    IF !EMPTY(lcCustFile) AND USED(lcCustFile)
      SELECT (lcCustFile)
      LOCATE 
      IF !EOF()
        llSelCust = .T.
      ENDIF
    ENDIF 
  ENDIF 

  IF lcRpPrnBy = 'S'
    lnPosSO = ASCAN(loOGScroll.laogFXflt,'ORDHDR.ORDER')
    lcSOFile = ''
    llSelSO = .F.
    IF lnPosSO <> 0 
      lnPosSO = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosSO,1)
      lcSOFile =  loOGScroll.laogFXflt[lnPosSO,6]
      IF !EMPTY(lcSOFile) AND USED(lcSOFile)
        SELECT (lcSOFile)
        LOCATE 
        IF !EOF()
          llSelSO = .T.
        ENDIF
      ENDIF 
    ENDIF 
  ELSE
    lnPosPIK = ASCAN(loOGScroll.laogFXflt,'PIKTKT.PIKTKT')
    lcPIKFile = ''
    llSelPik = .F.
    IF lnPosPIK <> 0 
      lnPosPIK= ASUBSCRIPT(loOGScroll.laogFXflt,lnPosPIK,1)
      lcPIKFile  =  loOGScroll.laogFXflt[lnPosPIK,6]
      IF !EMPTY(lcPIKFile)  AND USED(lcPIKFile)
        SELECT (lcPIKFile)
        LOCATE 
        IF !EOF()
          llSelPik = .T.
        ENDIF
      ENDIF 
    ENDIF 
  ENDIF 

  IF lcRpPrnBy = 'S' AND llSelSO
    SELECT(lcSOFile)
    SCAN
      IF gfSeek('O'+&lcSOFile..order,'ORDHDR') AND ordhdr.status $ 'OH' AND IIF(llSelCust,SEEK(ORDHDR.ACCOUNT,lcCustFile),.T.) 
        SELECT ORDHDR
        SCATTER MEMO MEMVAR
        SELECT(lcTmpOrdhdr)
        APPEND BLANK 
        GATHER MEMO MEMVAR
        SELECT Ordline
        IF gfSeek('O'+ Ordhdr.order,'ORDLINE')
          SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ Ordhdr.order FOR totqty > 0
            SCATTER MEMO MEMVAR 
            =gfSeek(ordline.style,'Style')
            m.Scale = style.scale
            =gfSeek('S'+ m.Scale,'Scale')
            m.SZ1 = Scale.sz1
            m.SZ2 = Scale.sz2
            m.SZ3 = Scale.sz3
            m.SZ4 = Scale.sz4
            m.SZ5 = Scale.sz5
            m.SZ6 = Scale.sz6
            m.SZ7 = Scale.sz7
            m.SZ8 = Scale.sz8
            SELECT(lcTmpOrdline) 
            APPEND BLANK 
            GATHER MEMO MEMVAR
          ENDSCAN 
        ENDIF
      ENDIF
    ENDSCAN 
  ELSE
   IF lcRpPrnBy = 'P' AND llSelPik
     SELECT (lcPIKFile)
     SCAN
       IF gfSeek(&lcPIKFile..PIKTKT,'PIKTKT') AND piktkt.status = 'O' AND ;
          IIF(llSelCust,SEEK(PIKTKT.ACCOUNT,lcCustFile),.T.) AND;
          gfSeek('O'+piktkt.order,'ordhdr') AND ordhdr.status $ 'OH'
         SELECT ORDHDR
         m.piktkt = &lcPIKFile..PIKTKT
         SCATTER MEMO MEMVAR
         SELECT(lcTmpOrdhdr)
         APPEND BLANK 
         GATHER MEMO MEMVAR
         SELECT Ordline
         IF gfSeek('O'+ Ordhdr.order,'ORDLINE')
           SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ Ordhdr.order FOR piktkt =&lcPIKFile..PIKTKT AND totqty > 0
             SCATTER MEMO MEMVAR 
             =gfSeek(ordline.style,'Style')
             m.Scale = style.scale
             =gfSeek('S'+ m.Scale,'Scale')
             m.SZ1 = Scale.sz1
             m.SZ2 = Scale.sz2
             m.SZ3 = Scale.sz3
             m.SZ4 = Scale.sz4
             m.SZ5 = Scale.sz5
             m.SZ6 = Scale.sz6
             m.SZ7 = Scale.sz7
             m.SZ8 = Scale.sz8

             *B609529,1 SAB 12/11/2007 Print Pik-Qty in pre-forma report in partial pikking case[T20101027.0012][Start]
             DO ConvertToPick
             *B609529,1 SAB 12/11/2007 Print Pik-Qtyin pre-forma report in partial pikking case[T20101027.0012][End]

             SELECT(lcTmpOrdline) 
             APPEND BLANK 
             GATHER MEMO MEMVAR
           ENDSCAN 
         ENDIF
       ENDIF
     ENDSCAN 
   ELSE
     IF llSelCust
       IF lcRpPrnBy = 'S' 
         SELECT ordhdr
         gfsetorder('ORDACCT')   && ACCOUNT+CORDTYPE+ORDER
         SELECT (lcCustFile)
         SCAN 
           IF gfSeek(&lcCustFile..Account+'O','ordhdr')
             SELECT ordhdr
             SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcCustFile..Account+'O' FOR ordhdr.status $ 'OH'
               SCATTER MEMO MEMVAR
               SELECT(lcTmpOrdhdr)
               APPEND BLANK 
               GATHER MEMO MEMVAR
               SELECT Ordline
               IF gfSeek('O'+ Ordhdr.order,'ORDLINE')
                 SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ Ordhdr.order FOR totqty > 0
                   SCATTER MEMO MEMVAR 
                   =gfSeek(ordline.style,'Style')
                   m.Scale = style.scale
                   =gfSeek('S'+ m.Scale,'Scale')
                   m.SZ1 = Scale.sz1
                   m.SZ2 = Scale.sz2
                   m.SZ3 = Scale.sz3
                   m.SZ4 = Scale.sz4
                   m.SZ5 = Scale.sz5
                   m.SZ6 = Scale.sz6
                   m.SZ7 = Scale.sz7
                   m.SZ8 = Scale.sz8
                   SELECT(lcTmpOrdline) 
                   
                   APPEND BLANK 
                   GATHER MEMO MEMVAR
                 ENDSCAN 
               ENDIF
             ENDSCAN 
           ENDIF 
         ENDSCAN   
       ELSE
         SELECT Piktkt
         SCAN FOR piktkt.status = 'O' AND SEEK(piktkt.account,lcCustFile) ;
           AND gfSeek('O'+piktkt.order,'ordhdr') AND ordhdr.status $ 'OH'
           SELECT ORDHDR
           m.piktkt = Piktkt.PIKTKT
           SCATTER MEMO MEMVAR
           SELECT(lcTmpOrdhdr)
           APPEND BLANK 
           GATHER MEMO MEMVAR
           SELECT Ordline
           IF gfSeek('O'+ Ordhdr.order,'ORDLINE')
             SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ Ordhdr.order FOR Piktkt = Piktkt.PIKTKT AND totqty > 0
               SCATTER MEMO MEMVAR 
               =gfSeek(ordline.style,'Style')
               m.Scale = style.scale
               =gfSeek('S'+ m.Scale,'Scale')
               m.SZ1 = Scale.sz1
               m.SZ2 = Scale.sz2
               m.SZ3 = Scale.sz3
               m.SZ4 = Scale.sz4
               m.SZ5 = Scale.sz5
               m.SZ6 = Scale.sz6
               m.SZ7 = Scale.sz7
               m.SZ8 = Scale.sz8

               *B609529,1 SAB 12/11/2007 Print Pik-Qty in pre-forma report in partial pikking case[T20101027.0012][Start]
               DO ConvertToPick
               *B609529,1 SAB 12/11/2007 Print Pik-Qtyin pre-forma report in partial pikking case[T20101027.0012][End]
               
               SELECT(lcTmpOrdline)                              
               APPEND BLANK 
               GATHER MEMO MEMVAR
             ENDSCAN 
           ENDIF
         ENDSCAN 
       ENDIF
     ELSE
       IF lcRpPrnBy = 'S' 
          SELECT ordhdr
          =gfsetorder('ORDHDR')
          =gfSeek('O')
          SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR ordhdr.status $ 'OH'
            SCATTER MEMO MEMVAR
            SELECT(lcTmpOrdhdr)
            APPEND BLANK 
            GATHER MEMO MEMVAR
            SELECT Ordline
            IF gfSeek('O'+ Ordhdr.order,'ORDLINE')
              SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ Ordhdr.order FOR totqty > 0
                SCATTER MEMO MEMVAR 
                =gfSeek(ordline.style,'Style')
                m.Scale = style.scale
                =gfSeek('S'+ m.Scale,'Scale')
                m.SZ1 = Scale.sz1
                m.SZ2 = Scale.sz2
                m.SZ3 = Scale.sz3
                m.SZ4 = Scale.sz4
                m.SZ5 = Scale.sz5
                m.SZ6 = Scale.sz6
                m.SZ7 = Scale.sz7
                m.SZ8 = Scale.sz8
                
                SELECT(lcTmpOrdline) 
                APPEND BLANK 
                GATHER MEMO MEMVAR
              ENDSCAN 
            ENDIF
         ENDSCAN 
       ELSE
         SELECT Piktkt
         SCAN FOR piktkt.status = 'O' ;
           AND gfSeek('O'+piktkt.order,'ordhdr') AND ordhdr.status $ 'OH'
           SELECT ORDHDR
           m.piktkt = PIKTKT.PIKTKT
           SCATTER MEMO MEMVAR
           SELECT(lcTmpOrdhdr)
           APPEND BLANK 
           GATHER MEMO MEMVAR
           SELECT Ordline
           IF gfSeek('O'+ Ordhdr.order,'ORDLINE')
             SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+ Ordhdr.order FOR Piktkt = Piktkt.PIKTKT AND totqty > 0
               SCATTER MEMO MEMVAR 
               =gfSeek(ordline.style,'Style')
               m.Scale = style.scale
               =gfSeek('S'+ m.Scale,'Scale')
               m.SZ1 = Scale.sz1
               m.SZ2 = Scale.sz2
               m.SZ3 = Scale.sz3
               m.SZ4 = Scale.sz4
               m.SZ5 = Scale.sz5
               m.SZ6 = Scale.sz6
               m.SZ7 = Scale.sz7
               m.SZ8 = Scale.sz8
               
               *B609529,1 SAB 12/11/2007 Print Pik-Qty in pre-forma report in partial pikking case[T20101027.0012][Start]
               DO ConvertToPick
               *B609529,1 SAB 12/11/2007 Print Pik-Qtyin pre-forma report in partial pikking case[T20101027.0012][End]
               
               SELECT(lcTmpOrdline)
               APPEND BLANK 
               GATHER MEMO MEMVAR
             ENDSCAN 
           ENDIF
         ENDSCAN 
       ENDIF
     ENDIF   
   ENDIF 
  ENDIF  
  SELECT(lcTmpOrdline)
  SCAN
    lnRecNum= RECNO()
    lfReplOHdr()
    IF BETWEEN(lnRecNum,1,RECCOUNT(lcTmpOrdline))
      GO lnRecNum IN (lcTmpOrdline)
    ENDIF
  ENDSCAN
ENDIF   


=lfOptProg()

lcRelation = IIF(lcRpPrnBy = 'S','order','PIKTKT')



SELECT(lcTmpOrdhdr)
SET RELATION TO &lcRelation. INTO (lcTmpOrdline) ADDITIVE 

SET SKIP TO (lcTmpOrdline)


SELECT(lcTmpOrdhdr)
SET FILTER TO !EOF(lcTmpOrdline)
LOCATE 

IF EOF() OR RECCOUNT() = 0
 =gfModalGen('TRM00052B40011','ALERT')
 SELECT(lcTmpOrdhdr)
 SET FILTER TO 
 SET RELATION TO 
 SET SKIP TO 
 RETURN
ENDIF 

=gfDispRe ()

SELECT(lcTmpOrdhdr)
SET FILTER TO 
SET RELATION TO 
SET SKIP TO 

SELECT ordhdr 
gfsetorder('ordhdr')





*!*************************************************************
*! Name      : lfWOgWhen
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : When Function of the option grid
*!*************************************************************
FUNCTION lfWOgWhen

= gfOpenTable(oAriaApplication.DataDir + 'SCALE' ,'SCALE','SH')
= gfOpenTable(oAriaApplication.DataDir + 'PIKTKT' ,'PIKTKT','SH')
= gfOpenTable(oAriaApplication.DataDir + 'ORDLINE' ,'ORDLINE','SH')
= gfOpenTable(oAriaApplication.DataDir + 'STYLE' ,'STYLE','SH','Style')
= gfOpenTable(oAriaApplication.DataDir + 'CUSTOMER' ,'CUSTOMER','SH')
= gfOpenTable(oAriaApplication.DataDir + 'ORDHDR' ,'ORDHDR','SH')
= gfOpenTable(oAriaApplication.DataDir + 'Spck_Lin' ,'Spck_Lin','SH')
= gfOpenTable(oAriaApplication.DataDir + 'SkuTmpl' ,'SkuTmpl','SH')
= gfOpenTable(oAriaApplication.DataDir + 'SPCK_HDR' ,'SPCK_HDR','SH')
= gfOpenTable(oAriaApplication.DataDir + 'OBJLINK' ,'OBJLNKTY','SH')
= gfOpenTable(oAriaApplication.DataDir + 'objects' ,'OBJECTID','SH')
= gfOpenTable(oAriaApplication.DataDir + 'NOTEPAD' ,'NOTEPAD','SH')

*!*************************************************************
*! Name      : lfvPrType
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Validate Type
*!*************************************************************
FUNCTION lfvPrType
CLEARREAD()



*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdr

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec


lcFacName   = ''
DECLARE laFactor[5]
laFactor = ''
*-- Fill laFactor with factor address
IF !EMPTY(EVALUATE(lcTmpOrdhdr+'.CFACCODE'))
  =SEEK(EVALUATE(lcTmpOrdhdr+'.CFACCODE'),'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.

=gfRltFld(EVALUATE(lcTmpOrdhdr+'.cDivision') , @laDivLName , 'CDIVISION')

lcShipVia = gfCodDes(EVALUATE(lcTmpOrdhdr+'.ShipVia') , 'SHIPVIA')
lcTerms = gfCodDes(EVALUATE(lcTmpOrdhdr+'.cTermCode') , 'CTERMCODE')

=gfSeek(IIF(EMPTY(EVALUATE(lcTmpOrdhdr+'.Store')) OR EVALUATE(lcTmpOrdhdr+'.Store') = "********",'M' +  EVALUATE(lcTmpOrdhdr+'.Account'),'S' +  EVALUATE(lcTmpOrdhdr+'.Account')+ EVALUATE(lcTmpOrdhdr+'.Store')),'CUSTOMER')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

SELECT CUSTOMER

IF EVALUATE(lcTmpOrdhdr+'.Alt_ShpTo')
  lcShpTName  =  EVALUATE(lcTmpOrdhdr+'.STName')
  laShipTo[1] = EVALUATE(lcTmpOrdhdr+'.cAddress1')
  laShipTo[2] = EVALUATE(lcTmpOrdhdr+'.cAddress2')
  laShipTo[3] = EVALUATE(lcTmpOrdhdr+'.cAddress3')
  laShipTo[4] = EVALUATE(lcTmpOrdhdr+'.cAddress4')
  laShipTo[5] = EVALUATE(lcTmpOrdhdr+'.cAddress5')
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr) AND !EVALUATE(lcTmpOrdhdr+'.lStrDirct')
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
*!*      =gfSEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(EVALUATE(lcTmpOrdhdr+'.STORE') = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(EVALUATE(lcTmpOrdhdr+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(EVALUATE(lcTmpOrdhdr+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(EVALUATE(lcTmpOrdhdr+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(EVALUATE(lcTmpOrdhdr+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(EVALUATE(lcTmpOrdhdr+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT(lcTmpOrdhdr)

RETURN ''

*!***************************************************************
*! Name      : lpPrtSku.
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Synopsis  : Print the style/color Skus for a specific account.
*!***************************************************************
FUNCTION lfPrtSku
PRIVATE lnPrevAl
STORE ' ' TO lcStrToPrn

STORE '' TO laSku

IF ! gfSEEK('S'+EVALUATE(lcTmpOrdline+'.Account')+EVALUATE(lcTmpOrdline+'.Style'),'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF
lnPrevAl = SELECT (0)
SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF gfSEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF

  IF llExtSize
    =gfSEEK(TYPE+ACCOUNT+SUBSTR(Style,1,LEN(ALLTRIM(Style))-4),'SPCK_HDR')
  ELSE
    =gfSEEK(TYPE+ACCOUNT+STYLE,'SPCK_HDR')
  ENDIF

  lnDime1 = MIN(lnDime1,LEN(ALLTRIM(SPCK_HDR.SKU)))

   SCAN WHILE Type+Account+Style = 'S'+EVALUATE(lcTmpOrdline+'.Account')+EVALUATE(lcTmpOrdline+'.Style') .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku[lnX]=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
  =gfSEEK('S'+EVALUATE(lcTmpOrdline+'.Account')+EVALUATE(lcTmpOrdline+'.Style'),'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+EVALUATE(lcTmpOrdline+'.Account')+EVALUATE(lcTmpOrdline+'.Style') .AND. lnI < 9
    lcStrToPrn = 'SKU # ' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+IEVALUATE(lcTmpOrdline+'.Account')+EVALUATE(lcTmpOrdline+'.Style') .AND. !EOF()
      lcI = STR(lnI,1)
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
  ENDDO
ENDIF
SELECT (lnPrevAl)
IF EMPTY(lcStrToPrn)
  STORE '' TO laSku
  llPrtSku = .F.
  RETURN .F.
ELSE
  llPrtSku = .T.
  RETURN .T.
ENDIF

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the INVHDR file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup
llEndGroup = .T.
RETURN ''

*!*************************************************************
*! Name      : lfPrtNotes
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Function to Evaluate Notes To be Printed
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA,ARPINVZ .FRX (Notes Expression)
*!*************************************************************
FUNCTION lfPrtNotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. SEEK('C' + INVHDR.Invoice , 'NOTEPAD')

    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)
  CASE llRpInvLNt .AND. !EMPTY(Invline.note_mem) .AND. LEFT(ALLTRIM(STRTRAN(Invline.note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(Invline.note_mem)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!**************************************************************************
*! Name      : lfGetSign
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Get the symbol of the used curr.
*!**************************************************************************
*! Example   : = lfGetSign()
*!**************************************************************************
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(EVALUATE(lcTmpOrdhdr+'.CCURRCODE'),'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVA.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6

  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])

    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfSRVPIK
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Set reset Function or PIKTKT#
*!*************************************************************
FUNCTION lfSRVPIK

PARAMETERS lcParm
dO CASE
  CASE lcParm = 'S'
    llSelCust = .F.
    lnPosCust = ASCAN(loOGScroll.laogFXflt,'CUSTOMER.ACCOUNT')
    lcCustFile = ''
    llSelCust = .F.
    IF lnPosCust <> 0 
      lnPosCust = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosCust,1)
      lcCustFile    =  loOGScroll.laogFXflt[lnPosCust,6]
      IF !EMPTY(lcCustFile) AND USED(lcCustFile)
        SELECT (lcCustFile)
        LOCATE 
        IF !EOF()
          llSelCust = .T.
        ENDIF
      ENDIF 
    ENDIF 
    IF llSelCust 
      SELECT PIKTKT
      SET FILTER TO  SEEK(PIKTKT.ACCOUNT,lcCustFile) AND !DELETED(lcCustFile) AND Status $ 'O' AND PIKTKT # '******'
    ENDIF
  CASE lcParm = 'R'
    SELECT PIKTKT
    SET FILTER TO
ENDCASE

*!*************************************************************
*! Name      : lfSRVORD
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Set reset Function or SO #
*!*************************************************************
FUNCTION lfSRVORD

PARAMETERS lcParm
dO CASE
  CASE lcParm = 'S'
    llSelCust = .F.
    lnPosCust = ASCAN(loOGScroll.laogFXflt,'CUSTOMER.ACCOUNT')
    lcCustFile = ''
    llSelCust = .F.
    IF lnPosCust <> 0 
      lnPosCust = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosCust,1)
      lcCustFile    =  loOGScroll.laogFXflt[lnPosCust,6]
      IF !EMPTY(lcCustFile) AND USED(lcCustFile)
        SELECT (lcCustFile)
        LOCATE 
        IF !EOF()
          llSelCust = .T.
        ENDIF
      ENDIF 
    ENDIF 
    IF llSelCust 
      SELECT ORDHDR
      SET FILTER TO CORDTYPE ='O' AND !DELETED(lcCustFile) AND  SEEK(ORDHDR.ACCOUNT,lcCustFile) AND Status $ 'OHB'
    ENDIF
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET FILTER TO
ENDCASE



*!*************************************************************
*! Name      : lfSRCUST  
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/18/2007
*! Purpose   : Set reset Function or Cust#
*!*************************************************************
FUNCTION lfSRCUST  

PARAMETERS lcParm
dO CASE
  CASE lcParm = 'R'
    llSelCust = .F.
    lnPosCust = ASCAN(loOGScroll.laogFXflt,'CUSTOMER.ACCOUNT')
    lcCustFile = ''
    llSelCust = .F.
    IF lnPosCust <> 0 
      lnPosCust = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosCust,1)
      lcCustFile    =  loOGScroll.laogFXflt[lnPosCust,6]
      IF !EMPTY(lcCustFile) AND USED(lcCustFile)
        SELECT (lcCustFile)
        LOCATE 
        IF !EOF()
          llSelCust = .T.
        ENDIF
      ENDIF 
    ENDIF 
    IF llSelCust 
      IF lcRpPrnBy = 'S'
        lnPosSO = ASCAN(loOGScroll.laogFXflt,'ORDHDR.ORDER')
        lcSOFile = ''
        llSelSO = .F.
        IF lnPosSO <> 0 
          lnPosSO = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosSO,1)
          lcSOFile =  loOGScroll.laogFXflt[lnPosSO,6]
          IF !EMPTY(lcSOFile) AND USED(lcSOFile)
            SELECT (lcSOFile)
            LOCATE 
            IF !EOF()
              SELECT (lcSOFile)
              SCAN FOR !DELETED()
                IF gfSeek('O'+&lcSOFile..order,'Ordhdr','Ordhdr') AND !SEEK(Ordhdr.Account ,lcCustFile)
                  DELETE
                ENDIF 
              ENDSCAN
            ENDIF
          ENDIF 
        ENDIF 
      ELSE
        lnPosPIK = ASCAN(loOGScroll.laogFXflt,'PIKTKT.PIKTKT')
        lcPIKFile = ''
        llSelPik = .F.
        IF lnPosPIK <> 0 
          lnPosPIK= ASUBSCRIPT(loOGScroll.laogFXflt,lnPosPIK,1)
          lcPIKFile  =  loOGScroll.laogFXflt[lnPosPIK,6]
          IF !EMPTY(lcPIKFile)  AND USED(lcPIKFile)
            SELECT (lcPIKFile)
            LOCATE 
            IF !EOF()
              SELECT (lcPIKFile)
              SCAN FOR !DELETED()
                IF gfSeek(&lcPIKFile..PIKTKT,'PIKTKT','PIKTKT') AND !SEEK(PIKTKT.Account ,lcCustFile)
                  DELETE
                ENDIF 
              ENDSCAN
            ENDIF
          ENDIF 
        ENDIF 
      ENDIF 
      =loOGScroll.RefreshScroll()
    ENDIF
ENDCASE

*!**************************************************************************
*! Name      : lfReplOHdr
*! Developer : Mariam Mazhar[MMT]
*: Date      : 09/20/2007
*! Purpose   : To calculate the tax rate, tax amount and total amount
*!**************************************************************************
*! Example   : =lfReplOHdr()
*!**************************************************************************
FUNCTION lfReplOHdr
PRIVATE lnAlias

lnAlias = SELECT(0)


=gfSEEK(Style,'Style','Style')
laEngStyTax[1,1] = 'NTAXRATE'
laEngStyTax[1,2] = 'lnTaxRate'
STORE 0 TO lnTaxQty,lnTaxRate

*B608379,1 MMT 12/11/2007 fix bug of wrong Vat Amount[Start]
*=gfSeek(IIF(EMPTY(EVALUATE(lcTmpOrdhdr+'.Store')) OR EVALUATE(lcTmpOrdhdr+'.Store') = "********",'M' +  EVALUATE(lcTmpOrdhdr+'.Account'),'S' +  EVALUATE(lcTmpOrdhdr+'.Account')+ EVALUATE(lcTmpOrdhdr+'.Store')),'CUSTOMER')
=gfSeek(IIF(EMPTY(EVALUATE(lcTmpOrdline+'.Store')) OR EVALUATE(lcTmpOrdline+'.Store') = "********",'M' +  EVALUATE(lcTmpOrdline+'.Account'),'S' +  EVALUATE(lcTmpOrdline+'.Account')+ EVALUATE(lcTmpOrdline+'.Store')),'CUSTOMER')
*B608379,1 MMT 12/11/2007 fix bug of wrong Vat Amount[END]

IF !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
  =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')
  FOR lnCount = Style.nTaxBreak TO 8
    lnTaxQty = lnTaxQty + EVALUATE(lcTmpOrdline+'.Qty'+STR(lncount,1))
  ENDFOR
ENDIF


SELECT (lcTmpOrdhdr)
=SEEK(IIF(lcRpPrnBy = 'S' ,&lcTmpOrdline..Order,&lcTmpOrdline..PIKTKT))
DIMENSION laTRltFld[1,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnTerDiscR'
=gfRltFld(cTermCode,@laTRltFld,'CTERMCODE')


REPLACE nTrde_Disc WITH lnTerDiscR                                           ,;
          nShipAmt   WITH nShipAmt  + &lcTmpOrdline..TotQty * &lcTmpOrdline..Price   ,;
          Discount   WITH -nShipAmt * Disc/100                          ,;
          nMerchTax  WITH nMerchTax + lnTaxQty*&lcTmpOrdline..Price*lnTaxRate/100 ,;
          Tax_Amt    WITH nMerchTax*(100-Disc)/100*(100-nTrde_Disc)/100 

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfCurrDesc
*! Developer : Mariam Mazhar
*! Date      : 09/20/2007
*! Purpose   : Get the currency description
*!**************************************************************************
*! Example   : = lfCurrDesc()
*!**************************************************************************
FUNCTION lfCurrDesc
PRIVATE lcCurrDsc

lcCurrDsc = ''
lcCurrDsc = IIF(SEEK(&lcTmpOrdhdr..CCURRCODE,'SYCCURR'),SYCCURR.cCurrDesc,lcCurrDsc)

RETURN ALLTRIM(lcCurrDsc)

*!**************************************************************************
*! Name      : ConvertToPick
*! Developer : Saber Saber
*! Date      : 02/24/2011
*! Purpose   : Replace the Quantity By Pikked Quantity
*!**************************************************************************
*! Example   : = ConvertToPick()
*!**************************************************************************
PROCEDURE ConvertToPick

m.Qty1 = m.Pik1
m.Qty2 = m.Pik2
m.Qty3 = m.Pik3
m.Qty4 = m.Pik4
m.Qty5 = m.Pik5
m.Qty6 = m.Pik6
m.Qty7 = m.Pik7
m.Qty8 = m.Pik8
m.TotQty = m.TotPik

ENDPROC 