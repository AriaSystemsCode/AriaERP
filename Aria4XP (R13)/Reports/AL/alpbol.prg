*:***************************************************************************
*: Program file       : ALPRTBOL
*: Program description: Bill of Lading Report
*: Module             : Sales Order Allocation (AL)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: E303077.EXE
*: Date               : 02/22/2012
*:***************************************************************************
*Modifications:
*!B609980,1 SAB 06/27/2012 Fix error that occure when run BOL report [Media R12 Issues]
*!E303357,1 SAB 02/24/2013 Modify BOL report to run the optional programs [T20121017.0023]
*!C201586,1 SAB 07/09/2013 Fix issue 3 in IKE00 customization [T20121017.0023]
*!B611224,1 MAA 01/11/2016 Fix issue 2 Missing data from Report [P20160428.0001]
*:***************************************************************************
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
#INCLUDE r:\aria4xp\reports\al\alpbol.H
*N000682,1 MMT 02/06/2013 Globalization changes[End]
PRIVATE laCodes, laCrstlSet
*!B609980,1 SAB 06/27/2012 Fix error that occure when run BOL report [Start]
STORE '' TO laCodes
*!B609980,1 SAB 06/27/2012 Fix error that occure when run BOL report [End]
DIMENSION laCrstlSet[8]

lcRepDir   = gfTempName()
TmpBol_Hdr = gfTempName()

=lfCreateTemp()
=lfCollectData()

IF RECCOUNT('BolHdr') <= 0
  *- There are no records to display.
  =gfModalGen('TRM00052B40011','ALERT')
  *- Remove Temporary files
  IF USED('BOLHDR')
    USE IN BOLHDR
  ENDIF
  IF USED('BOLLIN')
    USE IN BOLLIN
  ENDIF
  ERASE oAriaApplication.WorkDir+ lcRepDir+'\BOLHDR.*'
  ERASE oAriaApplication.WorkDir+ lcRepDir+'\BOLLIN.*'
  IF USED(TmpBol_Hdr)
    USE IN (TmpBol_Hdr)
  ENDIF
  ERASE (oAriaApplication.WorkDir+ TmpBol_Hdr+".dbf")
  ERASE (oAriaApplication.WorkDir+ TmpBol_Hdr+".cdx")
  RETURN .F.
ENDIF

=lfAdjustCRSettings()

*!E303357,1 SAB 02/24/2013 Modify BOL report to run the optional programs [T20121017.0023][Start]
=lfOptProg()
*!E303357,1 SAB 02/24/2013 Modify BOL report to run the optional programs [T20121017.0023][End]

IF USED('BolHdr')
  USE IN ('BolHdr')
ENDIF
IF USED('BolLin')
  USE IN ('BolLin')
ENDIF
IF USED('tmpEdiCrtSq')
  USE IN ('tmpEdiCrtSq')
ENDIF

DO gfDispRe WITH EVAL('LCRPFORM')
SET RELATION TO

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Option Grid When function
*!*************************************************************
FUNCTION lfwRepWhen

ENDFUNC


*!*************************************************************
*! Name      : lfRepShow
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Option Grid Show
*!*************************************************************
FUNCTION lfRepShow

ENDFUNC


*!*************************************************************
*! Name      : lfGetCarrier
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Get Carrier Desc.
*!*************************************************************
FUNCTION lfGetCarrier

=SEEK('N'+ShipVia+'N'+'SHIPVIA','CODES','CODES')
RETURN Codes.cDiscRep

ENDFUNC


*!*************************************************************
*! Name      : lfGetAddress
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Get Address.
*!*************************************************************
FUNCTION lfGetAddress
lPARAMETERS lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo,lcAddGrp,lcCurrCode

PRIVATE lnSavIntTg, lnSavCmpTg, lcCurAlias, lnOldTag,;
        llOpenInt, llOpenCmp, llContinue, lnCompRec,lcCurrCode
 IF EMPTY(lcAlias) .OR. TYPE('lcTag') <> 'C'
   IF EMPTY(ALIAS())
     RETURN .F.
   ELSE
     lcAlias = ALIAS()
   ENDIF
 ENDIF
 IF EMPTY(lcAddGrp) OR TYPE('lcAddGrp') <> 'C'
   lcAddGrp  = ''
   lcGrpCode = 'E'
 ELSE
   lcAddGrp  = ALLTRIM(lcAddGrp)
   lcGrpCode = lcAddGrp
 ENDIF
 lcCurAlias = ALIAS()
 SELECT (lcAlias)
 lnOldTag = VAL(SYS(21))
 IF EMPTY(lcAdrCode)
   IF !EMPTY(lcKeyCode) .AND. TYPE('lcKeyCode') <> 'C'
     DO CASE
       CASE (EMPTY(lcTag) .OR. TYPE('lcTag') <> 'C') AND EMPTY(SYS(22))
         SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)
         RETURN .F.

       CASE !EMPTY(lcTag)
         SET ORDER TO TAG (lcTag)
         IF !SEEK(lcKeyCode)
           SET ORDER TO lnOldTag
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)
           RETURN .F.
         ENDIF

       OTHERWISE
         IF EMPTY(SYS(22))
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)
           RETURN .F.
         ENDIF
         IF !SEEK(lcKeyCode)
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)
           RETURN .F.
         ENDIF
     ENDCASE
   ENDIF
   lcAdrCode = cCont_Cod&lcGrpCode
 ENDIF

DECLARE laAddress[6,3]
laAddress = " "
lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
STORE .F. TO llOpenInt, llOpenCmp

IF !USED('SYCINT')  && Check if the internationals file is open or not.
  llOpenInt  = .T.     && Indicates that the file is open by the function.
  USE (oAriaApplication.SysPath+'SYCINT') ORDER TAG cContCode IN 0
ELSE
  SELECT SYCINT
  lnSavIntTg = VAL(SYS(21))
  SET ORDER TO TAG cContCode   && Change the order
ENDIF

IF !USED('SYCCOMP')  && Check if the internationals file is open or not.
  llOpenCmp  = .T.     && Indicates that the file is open by the function.
  USE (oAriaApplication.SysPath+'SYCCOMP') ORDER TAG cComp_ID IN 0
ELSE
  SELECT SYCCOMP
  lnSavCmpTg = VAL(SYS(21))
  lnCompRec  = RECNO()
  SET ORDER TO TAG cComp_ID   && Change the order
ENDIF

*SET STEP ON
IF SEEK(lcAdrCode,'SYCINT') .OR. (SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP') ;
   .AND.  SEEK(SYCCOMP.cCont_Code,'SYCINT'))
  laAddress[1,1] = SYCINT.nPart1Ord
  laAddress[1,2] = EVAL(lcAlias+'.cAddress1'+lcAddGrp)
  laAddress[1,3] = SYCINT.nPart1LEN
  laAddress[2,1] = SYCINT.nPart2Ord
  laAddress[2,2] = EVAL(lcAlias+'.cAddress2'+lcAddGrp)
  laAddress[2,3] = SYCINT.nPart2LEN
  laAddress[3,1] = SYCINT.nPart3Ord
  laAddress[3,2] = EVAL(lcAlias+'.cAddress3'+lcAddGrp)
  laAddress[3,3] = SYCINT.nPart3LEN
  laAddress[4,1] = SYCINT.nPart4Ord
  laAddress[4,2] = EVAL(lcAlias+'.cAddress4'+lcAddGrp)
  laAddress[4,3] = SYCINT.nPart4LEN
  laAddress[5,1] = SYCINT.nPart5Ord
  laAddress[5,2] = EVAL(lcAlias+'.cAddress5'+lcAddGrp)
  laAddress[5,3] = SYCINT.nPart5LEN
  laAddress[6,1] = SYCINT.nPart6Ord
  laAddress[6,2] = EVAL(lcAlias+'.cAddress6'+lcAddGrp)
  laAddress[6,3] = SYCINT.nPart6LEN
  IF TYPE("lcCurrCode") = 'C'
    &lcCurrCode    = SYCINT.cCurrCode
    SHOW GET (lcCurrCode)
  ENDIF
  =ASORT(laAddress,1)
  lcRetVal=''
  FOR lnCount = 1 TO ALEN(laAddress,1)
    IF laAddress[lnCount,1] = lnLineNo
      lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
      lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',', ') + lcAddPart
     ENDIF
  ENDFOR
ELSE
  lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
ENDIF

IF USED('SYCCOMP')
  IF llOpenCmp
    USE IN SYCCOMP
  ELSE
    SET ORDER TO lnSavCmpTg IN SYCCOMP
    IF BETWEEN(lnCompRec,1,RECCOUNT("SYCCOMP"))
      GOTO lnCompRec IN SYCCOMP
    ENDIF
  ENDIF
ENDIF

IF USED('SYCINT')
  IF llOpenInt
    USE IN SYCINT
  ELSE
    SET ORDER TO lnSavIntTg IN SYCINT
  ENDIF
ENDIF
SET ORDER TO lnOldTag IN (lcAlias)
SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)
RETURN lcRetVal

ENDFUNC


*!*************************************************************
*! Name      : lfGetCheckDigit
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Check Carton Digit.
*!*************************************************************
FUNCTION lfGetCheckDigit
LPARAMETER lcUccNo, lcType

lcType = IIF(TYPE('lcType')='C',lcType,'O')

LOCAL      lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

IF TYPE('lcUccNo') = 'C'
  lnTop = LEN(lcUccNo)
ENDIF

FOR lnCount = 1 TO lnTop STEP 2
  lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcUccNo,lnCount     , 1))
  lnSumEven = lnSumEven + VAL(SUBSTR(lcUccNo,lnCount + 1 , 1))
ENDFOR

IF lcType = 'O'
 lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)
ELSE
 lnChkDigit = MOD(lnSumOdd + lnSumEven*3 , 10)
ENDIF

RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

ENDFUNC


*!*************************************************************
*! Name      : lfCollectData
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Procedure to make replenishment table have 14 size per line
*!*************************************************************
PROCEDURE lfCollectData

DECLARE laAllSets[1,3]
STORE '' TO laAllSets
STORE .T. TO llPrntComp

LOCAL lcUccNo
lcUccNo = gfGetMemVar('XMANUFID')

=lfFillBolHdr()

SELECT tmpBolHd

COPY TO (oAriaApplication.WorkDir+lcRepDir+'\BolHdr.DBF') TYPE FOX2X WITH CDX
USE (oAriaApplication.WorkDir+lcRepDir+'\BolHdr.DBF') IN 0 EXCL

IF UPPER(RIGHT(lcRpForm,1))='C'
  SELECT ('edicrtsq1')
  COPY TO (oAriaApplication.WorkDir+lcRepDir+'\tmpEdiCrtSq') TYPE FOX2X
  USE (oAriaApplication.WorkDir+lcRepDir+'\tmpEdiCrtSq') IN 0 EXCL
  SELECT ('tmpedicrtsq')
  INDEX ON PACK_NO+ACCOUNT+STR(CART_NO,6) TAG pack_no
  SELECT (TmpBol_Hdr)
ENDIF
SELECT BolHdr
INDEX ON BOL_NO TAG BOL_HDR

SELECT tmpBolLn
COPY TO (oAriaApplication.WorkDir+lcRepDir+'\BolLin.DBF') TYPE FOX2X WITH CDX
USE (oAriaApplication.WorkDir+lcRepDir+'\BolLin.DBF') IN 0 EXCL
SELECT BolLin

INDEX ON BOL_NO + ORDER TAG BOL_LN

DECLARE laAddress[6,3]
SELECT (TmpBol_Hdr)
lcOrder = ORDER()
SET ORDER TO TAG LLBLPRINT

llSelBol = .F.

=SEEK(.T.)
SCAN REST WHILE LLBLPRINT = .T.
  llSelBol = .T.

  SCATT MEMVAR MEMO
  =SEEK(EVAL(TmpBol_Hdr+'.BOL_NO'),'BOL_HDR','BOL_HDR')
  m.CPUA = BOL_HDR.CPUA
  m.cMasterbol = BOL_HDR.cMasterbol

  IF !USED('WareHous')
    =gfOpenTable('WareHous', 'WAREHOUS', 'SH')   && CWARECODE
  ENDIF
  =SEEK(m.W_Code, 'WareHous')
  m.whname = WareHous.cDesc
  *!C201586,1 SAB 07/09/2013 Fix issue 3 in IKE00 customization [T20121017.0023][Start]
  **!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][Start]
  **IF UPPER(RIGHT(lcRpForm,1))='B'
  *IF UPPER(RIGHT(lcRpForm,1))='B' .OR. UPPER(RIGHT(lcRpForm,2))='IK'
  **!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][End]  
  IF UPPER(RIGHT(lcRpForm,1))='B'
  *!C201586,1 SAB 07/09/2013 Fix issue 3 in IKE00 customization [T20121017.0023][End]
    m.whname =oAriaApplication.ActiveCompanyName
  ENDIF
  STORE '' TO m.whADD1,m.whADD2,m.whADD3,m.whADD4,m.whADD5,m.whADD6,laAddress
  =lfGetAddress('WareHous','','','',1,'')
  IF !EMPTY(laAddress)
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      m.whADD&lcCount = m.whADD&lcCount + IIF(EMPTY(m.whADD&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
  ENDIF
  IF SEEK('M'+m.account,'Customer')
    m.CCUSVEND = Customer.CCUSVEND
  ENDIF

  =IIF(EMPTY(m.Store),SEEK('M'+m.account,'Customer'),SEEK('S'+m.account+m.Store,'Customer'))
  IF ALLTRIM(m.Store)='D2S'
    =SEEK(m.BOL_NO,'BOL_LIN') and SEEK(BOL_LIN.pack_no,'PACK_HDR','pack_hdr') and SEEK('S'+m.account+PACK_HDR.Store,'Customer')
  ENDIF
  m.STName  = Customer.STName
  STORE '' TO m.STAdd1,m.STAdd2,m.STAdd3,m.STAdd4,m.STAdd5,m.STAdd6,laAddress
  =lfGetAddress('Customer','','','',1,'')
  IF !EMPTY(laAddress)
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      m.STAdd&lcCount = m.STAdd&lcCount + IIF(EMPTY(m.STAdd&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
  ENDIF

  IF !USED('EdiAcPrt')
    =gfOpenTable('EdiAcPrt', 'ACCFACT', 'SH')   && TYPE+CPARTNER
  ENDIF
  IF SEEK("A"+account,'EdiAcPrt','ACCFACT')
    m.lFob = IIF(!EMPTY(EdiAcPrt.cEdiFob),.T.,.F.)
  ENDIF
  XBOX_SER    = PADL(lcUccNo,7,'0') + PADL(BOL_NO,9,'0')

  XMOD10      = lfGetCheckDigit(XBOX_SER,'E')

  m.Bolno_Std =  ("402"+XBOX_SER + XMOD10)
  DIME Arr(1,6)
  STORE .F. TO Arr
  INSERT INTO ('BolHdr') FROM MEMVAR
  SELECT BOL_LIN
  =SEEK(m.BOL_NO)
  SCAN REST WHILE BOL_NO+ORDER+pack_no = m.BOL_NO
    IF !USED('PACK_HDR')
      =gfOpenTable('PACK_HDR', 'PACK_HDR', 'SH')   && PACK_NO
    ENDIF
    IF !USED('Ordhdr')
      =gfOpenTable('Ordhdr', 'ORDHDR', 'SH')   && CORDTYPE+ORDER
    ENDIF
    =SEEK(pack_no,'PACK_HDR','pack_hdr')
    =SEEK('O'+ORDER,'Ordhdr','OrdHdr')
    IF !EMPTY(BOL_Hdr.C3rdBT) AND SEEK('S'+m.Account+ALLTRIM(BOL_Hdr.C3rdBT),'Customer')
      REPLACE BTNAME  WITH  ALLTRIM(Customer.BTNAME) ;
				      BTADDR1 WITH  ALLTRIM(Customer.cAddress1);
				      BTADDR2 WITH  ALLTRIM(Customer.cAddress2);
				      BTCITY  WITH  ALLTRIM(Customer.cAddress3);
				      BTSTATE WITH  ALLTRIM(Customer.cAddress4);
				      BTZIP   WITH  ALLTRIM(Customer.cAddress5) IN BolHdr
    ENDIF
    lcCustPo = OrdHdr.CustPo
    IF !USED('OrdLine')
      =gfOpenTable('OrdLine', 'OrdLinSt', 'SH')   && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
    ENDIF
    IF !USED('PACK_LIN')
      =gfOpenTable('PACK_LIN', 'PACK_LIN', 'SH')   && PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR
    ENDIF
    IF OrdHdr.MultiPo AND SEEK('O'+OrdHdr.ORDER+Pack_Hdr.STORE,'OrdLine','OrdLinSt')
      lcCustPo = OrdLine.CustPo
    ENDIF
    m.lplt = SEEK(BOL_LIN.pack_no,'PACK_LIN','PACK_LIN') AND Pack_Lin.npltno != 0
    *!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][Start]
    *IF UPPER(RIGHT(lcRpForm,1))='B'
    
    *B611224,1 MAA 01/11/2016 Fix issue 2 Missing data from Report [P20160428.0001][Start]
    * IF UPPER(RIGHT(lcRpForm,1))='B' .OR. UPPER(RIGHT(lcRpForm,2))='IK' 
    IF UPPER(RIGHT(lcRpForm,1))='B' .OR. UPPER(RIGHT(lcRpForm,2))='IK' .OR. UPPER(RIGHT(lcRpForm,2))='KK'  
    *B611224,1 MAA 01/11/2016 Fix issue 2 Missing data from Report [P20160428.0001] [END]
    
    *!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][End]
      IF M.lplt = .T.
        REPLACE BolHdr.h_pltqty WITH Pack_Hdr.Tot_Cart + BolHdr.h_pltqty,;
          BolHdr.p_pltqty WITH Pack_Hdr.Tot_Cart + BolHdr.p_pltqty,;
          BolHdr.pltwgh   WITH Pack_Hdr.Tot_Wght + BolHdr.pltwgh IN BolHdr
      ELSE
        REPLACE BolHdr.h_ctnqty WITH Pack_Hdr.Tot_Cart + BolHdr.h_ctnqty,;
          BolHdr.p_ctnqty WITH Pack_Hdr.Tot_Cart + BolHdr.p_ctnqty,;
          BolHdr.ctnwgh   WITH Pack_Hdr.Tot_Wght + BolHdr.ctnwgh IN BolHdr
      ENDIF
      lnPos = ASCAN(Arr ,lcCustPo)
      IF lnPos = 0 AND ALEN(Arr,1)<7
        lnPos = ALEN(Arr,1)
        Arr(lnPos,1)= lcCustPo
        Arr(lnPos,2)= Pack_Hdr.Tot_Cart
        Arr(lnPos,3)= Pack_Hdr.Tot_Wght
        Arr(lnPos,4)= m.lplt
        Arr(lnPos,5)= OrdHdr.Dept
        Arr(lnPos,6)= Pack_Hdr.STORE
        DIME Arr(lnPos+1,6)
      ELSE
        IF lnPos <> 0
          Arr(lnPos+1)= Arr(lnPos+1) + Pack_Hdr.Tot_Cart
          Arr(lnPos+2)= Arr(lnPos+2) + Pack_Hdr.Tot_Wght
          Arr(lnPos+3)= IIF(Arr(lnPos+3),.T.,m.lplt)
        ENDIF
      ENDIF
    ENDIF
    INSERT INTO ('BolLin');
      (BOL_NO,pack_no,ORDER,nCartons,nWeight,nPieces,Dept,CustPo,STORE,lplt) VALUES ;
      (BOL_LIN.BOL_NO,BOL_LIN.pack_no,BOL_LIN.ORDER,Pack_Hdr.Tot_Cart,Pack_Hdr.Tot_Wght,Pack_Hdr.Tot_Pcs,;
      OrdHdr.Dept,lcCustPo,Pack_Hdr.STORE,M.lplt)

    IF UPPER(RIGHT(lcRpForm,1))='C'
      SELECT edicrtsq
      *-SABER
      SET ORDER TO PCKCRTSQ   && PACK_NO+STR(CART_NO,6)
      IF SEEK(BOL_LIN.pack_no)
        SCAN REST WHILE pack_no=BOL_LIN.pack_no
          SCATTER MEMVAR MEMO
          SELECT tmpedicrtsq
          APPEND BLANK
          GATHER MEMVAR MEMO
          SELECT edicrtsq
        ENDSCAN
      ENDIF
    ENDIF

  ENDSCAN

  *!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][Start]
  *IF UPPER(RIGHT(lcRpForm,1))='B'
  *B611224,1 MAA 01/11/2016 Fix issue 2 Missing data from Report [P20160428.0001][Start]
  *IF UPPER(RIGHT(lcRpForm,1))='B' .OR. UPPER(RIGHT(lcRpForm,2))='IK'
  IF UPPER(RIGHT(lcRpForm,1))='B' .OR. UPPER(RIGHT(lcRpForm,2))='IK' .OR. UPPER(RIGHT(lcRpForm,2))='KK' 
  *B611224,1 MAA 01/11/2016 Fix issue 2 Missing data from Report [P20160428.0001] [END]
  *!C201554,2 SAB 04/14/2013 Fix issue 1 and 2 on the project [T20121017.0023][End]
    REPLACE BolHdr.OrdCount WITH IIF(!EMPTY(Arr(1,1)),ALEN(Arr,1)-1,0) IN BolHdr
    IF ALEN(Arr,1)<=6 AND !EMPTY(Arr(1,1))
      FOR I = 1 TO (ALEN(Arr,1)-1)
        lStr=PADL(I,1,' ')
        REPLACE BolHdr.ord&lStr.     WITH Arr(I,1),;
          BolHdr.PKG_ord&lStr. WITH Arr(I,2),;
          BolHdr.W_ord&lStr.   WITH Arr(I,3),;
          BolHdr.PLT_ord&lStr. WITH Arr(I,4) IN BolHdr
        REPLACE BolHdr.Dept&lStr.     WITH Arr(I,5),;
          BolHdr.STORE&lStr. WITH Arr(I,6) IN BolHdr
      ENDFOR
    ENDIF
  ENDIF
ENDSCAN

SET ORDER TO TAG &lcOrder
STORE '' TO  XBOX_SER,XMOD10

llCompUsed = .T.
IF !USED('SYCCOMP')
  USE (oAriaApplication.SysPath+'SYCCOMP') ORDER TAG CCOMP_ID IN 0
  llCompUsed = .F.
ENDIF
=SEEK(oAriaApplication.ActiveCompanyId,'SYCCOMP','CCOMP_ID')
STORE '' TO m.STAdd1,m.STAdd2,m.STAdd3,m.STAdd4,m.STAdd5,m.STAdd6,laAddress
=lfGetAddress('SYCCOMP','','','',1,'')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  m.STAdd&lcCount = m.STAdd&lcCount + IIF(EMPTY(m.STAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
m.Duns = gfGetMemVar('XDUNS     ')

laCrstlSet[1] = IIF(llPrntComp, SycComp.cCom_Name, '')
laCrstlSet[2] = IIF(llPrntComp, m.STAdd1, '')
laCrstlSet[3] = IIF(llPrntComp, m.STAdd2, '')
laCrstlSet[4] = IIF(llPrntComp, m.STAdd3, '')
laCrstlSet[5] = IIF(llPrntComp, m.STAdd4, '')
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
*laCrstlSet[6] = IIF(llPrntComp, 'DUNS#: '+m.Duns, '')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCrstlSet[6] = IIF(llPrntComp, LANG_DUNSNO+' '+m.Duns, '')
laCrstlSet[6] = IIF(llPrntComp, IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DUNSNO,oAriaApplication.GetHeaderText("LANG_DUNSNO",AHEADERFILE))+' '+m.Duns, '')
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/06/2013 Globalization changes[END]
laCrstlSet[7] = IIF(llPrntComp, TRANSFORM(SycComp.cCom_Phon,"@R XXX-XXX-XXXX/XXXXX"), '')
laCrstlSet[8] = ''

IF !llCompUsed
  USE IN SycComp
ENDIF

ENDPROC


*!*************************************************************
*! Name      : lfFillBolHdr
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Fill BOL Header temp table
*!*************************************************************
PROCEDURE lfFillBolHdr
llUseWareHouse = (gfGetMemVar('M_WAREHOUS') == 'Y')

*- Add status filter to lcRpExp
LOCAL lcStatusFlt
lcStatusFlt = IIF(lcRpStatus == 'S', " AND BOL_HDR.STATUS == 'C'", IIF(lcRpStatus == 'N', " AND EMPTY(BOL_HDR.STATUS)", ''))
lcRpExp = lcRpExp + lcStatusFlt

SELECT BOL_HDR
SET ORDER TO TAG BOL_HDR
SCAN FOR &lcRpExp
  m.lLblPrint  = .T.
  m.BOL_NO     = BOL_HDR.BOL_NO
  m.ACCOUNT    = BOL_HDR.ACCOUNT
  m.BOLDATE    = BOL_HDR.BOLDATE
  m.STORE      = BOL_HDR.STORE
  m.TOT_WGHT   = BOL_HDR.TOT_WGHT
  m.TOT_CART   = BOL_HDR.TOT_CART
  m.TOT_PCS    = BOL_HDR.TOT_PCS
  m.trailer_no = BOL_HDR.trailer_no
  m.SHIPVIA    = BOL_HDR.SHIPVIA
  m.Carrier    = BOL_HDR.Carrier
  m.W_Code     = BOL_HDR.W_Code
  m.cSeal_no   = BOL_HDR.cSeal_no
  m.cPro_no    = BOL_HDR.cPro_no
  m.cfchrgtrm  = BOL_HDR.cfchrgtrm
  m.mspc_inst  = BOL_HDR.mspc_inst
  m.ccomm_desc = BOL_HDR.ccomm_desc
  m.nNMFC_No   = BOL_HDR.nNMFC_No
  m.nClass     = BOL_HDR.nClass
  m.Carriercod = BOL_HDR.Carriercod
  IF EMPTY(m.Carrier)
    lnLoc = ASCAN(laCodes,m.SHIPVIA)
    IF lnLoc > 0 AND lnLoc < ALEN(laCodes)
      m.Carrier = laCodes[lnLoc+1]
    ENDIF
  ENDIF
  m.cFactBOL  = BOL_HDR.cFactBOL

  INSERT INTO (TmpBol_Hdr) FROM MEMVAR
ENDSCAN

ENDPROC

***************************************************************
*! Name      : lfAdjustCRSettings
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : To set the report data files and parameters
*!*************************************************************
PROCEDURE lfAdjustCRSettings

*!B609980,1 SAB 06/27/2012 Fix error that occure when run BOL report [Start]
*DIMENSION loOgScroll.laCRTables[3]
*DIMENSION loOgScroll.laCRParams[7,2]
*IF UPPER(RIGHT(lcRpForm,1))='B'
*  loOGScroll.cCROrientation='P'
*ELSE
*  loOGScroll.cCROrientation='P'
*ENDIF
*loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcRepDir + '\BolHdr.DBF'
*loOgScroll.laCRTables[2] = oAriaApplication.WorkDir + lcRepDir + '\BolLin.DBF'
*IF UPPER(RIGHT(lcRpForm,1))='C'
*  loOgScroll.laCRTables[3] = oAriaApplication.WorkDir + lcRepDir + '\tmpEdiCrtSq.DBF'
*ENDIF
DIMENSION loOgScroll.laCRTables[2]
DIMENSION loOgScroll.laCRParams[7,2]

loOGScroll.cCROrientation='P'

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcRepDir + '\BolHdr.DBF'
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir + lcRepDir + '\BolLin.DBF'
IF UPPER(RIGHT(lcRpForm,1))='C'
  DIMENSION loOgScroll.laCRTables[3]
  loOgScroll.laCRTables[3] = oAriaApplication.WorkDir + lcRepDir + '\tmpEdiCrtSq.DBF'
ENDIF
*!B609980,1 SAB 06/27/2012 Fix error that occure when run BOL report [End]

LOCAL lnI
lnI  = 0
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'CMPADDR1'
loOgScroll.laCRParams[lnI, 2] = laCrstlSet[2]

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'CMPADDR2'
loOgScroll.laCRParams[lnI, 2] = laCrstlSet[3]

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'CMPADDR3'
loOgScroll.laCRParams[lnI, 2] = laCrstlSet[4]

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'CMPADDR4'
loOgScroll.laCRParams[lnI, 2] = laCrstlSet[5]

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'CMPDUNS'
loOgScroll.laCRParams[lnI, 2] = laCrstlSet[6]

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'CMPPHONE'
loOgScroll.laCRParams[lnI, 2] = laCrstlSet[7]

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'PAGE'
loOgScroll.laCRParams[lnI, 2] = laCrstlSet[8]

ENDPROC


*!*************************************************************
*! Name      : lfCreateTemp
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/22/2012
*! Purpose   : Procedure to create temp.file
*!*************************************************************
PROCEDURE lfCreateTemp

MKDIR (oAriaApplication.WorkDir+lcRepDir)
SELECT BOL_HDR
=AFIELDS(laFields)

lnALen = ALEN(laFields,1)
lnBolHdrFlds = ALEN(laFields,1)
DIMENSION laFields(lnALen+44,ALEN(laFields,2))
lnALen = lnALen + 1
laFields(lnALen,1) = 'STNAME'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'STADD1'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'STADD2'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'STADD3'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'STADD4'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'WHNAME'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'WHADD1'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'WHADD2'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'WHADD3'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'WHADD4'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'lFob'
laFields(lnALen,2) = 'L'
laFields(lnALen,3) = 0
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Bolno_Std'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 20
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'ordcount'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 3
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'ord1'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 15
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'ord2'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 15
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'ord3'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 15
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'ord4'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 15
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'ord5'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 15
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Pkg_ord1'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Pkg_ord2'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Pkg_ord3'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Pkg_ord4'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Pkg_ord5'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'W_ord1'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 13
laFields(lnALen,4) = 2
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'W_ord2'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 13
laFields(lnALen,4) = 2
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'W_ord3'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 13
laFields(lnALen,4) = 2
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'W_ord4'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 13
laFields(lnALen,4) = 2
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'W_ord5'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 13
laFields(lnALen,4) = 2
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Plt_ord1'
laFields(lnALen,2) = 'L'
laFields(lnALen,3) = 0
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Plt_ord2'
laFields(lnALen,2) = 'L'
laFields(lnALen,3) = 0
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Plt_ord3'
laFields(lnALen,2) = 'L'
laFields(lnALen,3) = 0
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Plt_ord4'
laFields(lnALen,2) = 'L'
laFields(lnALen,3) = 0
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'Plt_ord5'
laFields(lnALen,2) = 'L'
laFields(lnALen,3) = 0
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'h_pltqty'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'h_ctnqty'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'p_pltqty'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'p_ctnqty'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 8
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'pltwgh'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 13
laFields(lnALen,4) = 2
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'ctnwgh'
laFields(lnALen,2) = 'N'
laFields(lnALen,3) = 13
laFields(lnALen,4) = 2
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'CshpInfo1'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 60
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'CshpInfo2'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 60
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'CshpInfo3'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 60
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'CshpInfo4'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 60
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'CshpInfo5'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 60
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = ALEN(laFields,1)+1
DIMENSION laFields[lnALen,ALEN(laFields,2)]
laFields(lnALen,1) = 'CCUSVEND'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 15
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

FOR lnI=1 TO 5
  lnALen = ALEN(laFields,1)+1
  DIMENSION laFields[lnALen,ALEN(laFields,2)]
  laFields(lnALen,1) = 'DEPT'+STR(lnI,1)
  laFields(lnALen,2) = 'C'
  laFields(lnALen,3) = 5
  laFields(lnALen,4) = 0
  laFields(lnALen,17) = 0
  laFields(lnALen,18) = 0
ENDFOR
FOR lnI=1 TO 5
  lnALen = ALEN(laFields,1)+1
  DIMENSION laFields[lnALen,ALEN(laFields,2)]
  laFields(lnALen,1) = 'STORE'+STR(lnI,1)
  laFields(lnALen,2) = 'C'
  laFields(lnALen,3) = 8
  laFields(lnALen,4) = 0
  laFields(lnALen,17) = 0
  laFields(lnALen,18) = 0
ENDFOR

FOR lnCount = lnBolHdrFlds+1 TO ALEN(laFields,1)
  laFields(lnCount,7) = ''
  laFields(lnCount,8) = ''
  laFields(lnCount,9) = ''
  laFields(lnCount,10) = ''
  laFields(lnCount,11) = ''
  laFields(lnCount,12) = ''
  laFields(lnCount,13) = ''
  laFields(lnCount,14) = ''
  laFields(lnCount,15) = ''
  laFields(lnCount,16) = ''
  laFields(lnCount,17) = 0
  laFields(lnCount,18) = 0
ENDFOR
lnALen = ALEN(laFields,1)
DIMENSION laFields(lnALen+6,ALEN(laFields,2))
lnALen = lnALen + 1
laFields(lnALen,1) = 'BTNAME'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'BTADDR1'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'BTADDR2'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'BTCITY'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'BTSTATE'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

lnALen = lnALen + 1
laFields(lnALen,1) = 'BTZIP'
laFields(lnALen,2) = 'C'
laFields(lnALen,3) = 30
laFields(lnALen,4) = 0
laFields(lnALen,17) = 0
laFields(lnALen,18) = 0

FOR lnCount = lnALen - 6 TO lnALen
  laFields(lnCount,7) = ''
  laFields(lnCount,8) = ''
  laFields(lnCount,9) = ''
  laFields(lnCount,10) = ''
  laFields(lnCount,11) = ''
  laFields(lnCount,12) = ''
  laFields(lnCount,13) = ''
  laFields(lnCount,14) = ''
  laFields(lnCount,15) = ''
  laFields(lnCount,16) = ''
  laFields(lnCount,17) = 0
  laFields(lnCount,18) = 0
ENDFOR

CREATE TABLE (oAriaApplication.WorkDir+lcRepDir+'\tmpBolHd') FROM ARRAY laFields

CREATE TABLE (oAriaApplication.WorkDir+lcRepDir+'\tmpBolLn') ;
             (BOL_NO C(6),Pack_NO C(6),Order C(6),nCartons N(8),nWeight N(13,2),nPieces N(8),DEPT C(5),;
              CUSTPO C(15),Store C(8), lplt L)
SELECT CODES
SET ORDER TO TAG IDRLTFNAME
LOCAL I
I = 1
DIMENSION laCodes[I,2]
laCodes = ' '
SEEK 'NNSHIPVIA'
SCAN REST WHILE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNSHIPVIA'
  DIMENSION laCodes[I,2]
  laCodes[I,1] = CODES.CCODE_NO
  laCodes[I,2] = CODES.CDISCREP
  I = I + 1
ENDSCAN

CREATE TABLE (oAriaApplication.WorkDir+TmpBol_Hdr) ;
(lLblPrint L,BOL_NO C(6),ACCOUNT C(5),BOLDATE D,STORE C(8),TOT_WGHT N(13,2),TOT_CART N(8),TOT_PCS N(8),;
SHIPVIA C(6),Carrier C(30),trailer_no C(20),W_Code C(6),cSeal_no c(20),cPro_no c(20),cfchrgtrm c(1),;
mspc_inst m,ccomm_desc c(50),nNMFC_No n(10,2),nClass n(6,2),Carriercod c(4),cFactBOL c(25))

INDEX ON lLblPrint TAG lLblPrint
INDEX ON BOL_NO TAG (TmpBol_Hdr)

SELECT EDICRTSQ
COPY STRUCTURE TO (oAriaApplication.WorkDir+lcRepDir+'\edicrtsq1')
USE (oAriaApplication.WorkDir+lcRepDir+'\edicrtsq1') in 0
SELECT (TmpBol_Hdr)

ENDPROC
