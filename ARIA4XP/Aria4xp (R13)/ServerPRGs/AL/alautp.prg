*B612249,1 MMT 11/04/2020 if user checked Generate BOL check box, the Selected Order got picked only without Packing list and empty BOL is created[T20201022.0004]
PARAMETERS lcRequestID, loXMLPackingOptionPointer

PUBLIC  oAriaEnvironments
oAriaEnvironment = CREATEOBJECT("ariamain.AriaEnvironment", lcRequestID)
oAriaEnvironment.User_ID = oAriaapplication.User_ID

PRIVATE loAddUserInfo
loAddUserInfo = CREATEOBJECT('AddUserInfo')


PRIVATE poformclass, poAlClass, llRejForPr, lnRpWgh, lcRpBrkTyp, lnClrLen, lnClrPos, lcRejPkTmp, llExtSizSc, lcPakCur, lcExpr

*-- Option grid var in addition to llGenerateBOL (Ask to Generate BOL)
PRIVATE lnRpGenNew, llRpPkHPck, lnRpWght, llRpBolAsi, llRpUseExs, llRpUsePre, llRp1BoxPk, llRpMltSku, lnRpTtCrtP, ;
        lcRpBrkTyp, lnRpMnlQty, lcRpSortBy, lcRPCart, llGenerateBOL 

RELEASE laoghdflt, laogfxflt, laogvrflt
DECLARE laoghdflt[1, 8]
DECLARE laogfxflt[1, 8]
DECLARE laogvrflt[1, 8]

lfinit()

**********************************************************************************************************************************************************

FUNCTION lfinit

OpenTables()

lcRejPkTmp = oAriaEnvironment.Cursors.GetCursorTempName()
poformclass = CREATEOBJECT('serveralautpbclass')
poformclass.mcreatetempfiles()
CREATE TABLE (oAriaEnvironment.WorkDir+lcRejPkTmp) ;
  (PikTkt C(6), PikDate D, Order C(6), Account C(5), AccName C(30), Store C(8), CReason C(50))
  INDEX ON PikTkt TAG (lcRejPkTmp)
  
poAlClass = CREATEOBJECT('serverAL')

moptiongrid()
poformclass.msavepack()
oAriaEnvironment.SaveTables()

*TABLEUPDATE(.T.,.T.,'pack_lin')
*TABLEUPDATE(.T.,.T.,'pack_hdr')

**********************************************************************************************************************************************************

FUNCTION lfaddcrt

*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
PARAMETERS lnCrtNmbr
PRIVATE lnSlct
lnSlct = SELECT()
  =SEEK(ORDLINE.STYLE,'STYLE')
  SELECT (lcpakcur)
  APPEND BLANK
  REPLACE PACK_NO WITH ORDLINE.PIKTKT ;
          CARTON  WITH lnCrtNmbr
  IF lcRpBrkTyp = 'S'
    REPLACE STYLE WITH ORDLINE.STYLE ;
            BREAK WITH STYLE.QTY_CTN    
  ELSE
    REPLACE BREAK WITH IIF(lcRpBrkTyp='W',lnRpWght,lnRpMnlQty) 
    lnNoCrt = lnNoCrt + 1
  ENDIF
SELECT (lnSlct)
*-- end of lfAddCrt.
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

**********************************************************************************************************************************************************

FUNCTION lfcheckper 
LPARAMETER lnChange

PRIVATE llCheck
llCheck = .F.
lnTotPrk = INT(Ordline.Totpik/Scale.PPTot)
*-- in case of less than one carton deal as only one prepack
lnTotPrk = IIF(lnTotPrk = 0 ,1,lnTotPrk)
lnFrcQty = MOD(Ordline.Totpik/Scale.PPTot,1)
lnAdd1   = IIF(lnFrcQty<>0,1,0)
*-- Do not give value to the additional carton when we back the first carton.
lnAdd1 = IIF(lnChange="2" .AND. lnTotPrk=0,0,lnAdd1)
FOR lnCount = 1 TO 8
  lnCont = STR(lnCount,1)
  IF !EMPTY(SCALE.Pp&lnCont)
    IF !EMPTY(SCALE.Pp&lnCont) .AND. (lnTotPrk <= (Ordline.pik&lnCont/Scale.Pp&lnCont) AND ;
			(Ordline.pik&lnCont/Scale.Pp&lnCont) <= (lnTotPrk + lnAdd1) )
      IF lnFrcQty <> 0
		*-- Check if the total prepack = 0
		IF lnTotPrk <> 0
          llCheck2 = .T.
        ELSE
          llCheck2 = .F.
        ENDIF
      ENDIF
      IF lnChange = '1'
        IF (lnTotPrk <> (Ordline.pik&lnCont/Scale.Pp&lnCont) AND (Ordline.pik&lnCont/Scale.Pp&lnCont) > (lnTotPrk + lnAdd1))
          llCheck = .T.
        ENDIF
      ELSE
        IF lnTotPrk = (Ordline.pik&lnCont/Scale.Pp&lnCont)
          *--- If true Do not change prepack
          llCheck = .T.
        ELSE
          IF lnTotPrk < ((Ordline.pik&lnCont/Scale.Pp&lnCont) +1)
            llCheck = .T.
          ELSE
            llCheck = .F.
          ENDIF
        ENDIF
      ENDIF
    ELSE
      IF lnChange = '1'
        llCheck = .T.
        EXIT
      ELSE
        IF !EMPTY(Scale.Pp&lnCont)
          llCheck = .F.
          EXIT
        ENDIF
      ENDIF
    ENDIF
  ELSE
	IF !(Ordline.pik&lnCont = 0 .AND. SCALE.PP&lnCont = 0)
      IF lnChange = '1'
        llCheck =  Ordline.pik&lnCont<>0 .OR. SCALE.PP&lnCont = 0
      ENDIF
    ENDIF
  ENDIF
ENDFOR
*-- If pack first carton and totpik weight < carton preak weight
*-- do not change prepack.
lcCrit = IIF(lcRpBrkTyp='W','Ordline.TotPik * style.nStyWeight < lnRpWght',;
	IIF(lcRpBrkTyp='S','Ordline.TotPik < STYLE.QTY_CTN',;
	'Ordline.TotPik < lnRpMnlQty'))
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
IF !llExtSizSc 
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
  IF &lcCrit .AND. lnStorFrm = 0
    llCheck = (lnChange = '1')
  ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
ENDIF 
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
RETURN llCheck

**********************************************************************************************************************************************************

FUNCTION lfculclt
LPARAMETER lnNoCrt, lnFrmCrt, lnToCrt

PRIVATE lnNCartn , lnRemend ,lnDeffPc ,lnOldNo ,lnFlag ,lnCurAlias
STORE 0 TO lnNCartn , lnRemend ,lnDeffPc ,lnOldNo ,lnFlag
STORE .T. TO llChckCart
DIMENSION laScalPr[9]
DIMENSION laScalPr2[9]
STORE 0 TO laScalPr,laScalPr2
* lnNCartn   ----------> Variable to Get Total Qty in Carton
* lnOldNo    ----------> Variable to Gat old Number of Carton
* lnCompCrt   ----------> variable to get completed carton
* lnFrcCrt   ----------> variable to get fraction in last carton
* llChckCart ----------> variable to check if there is fraction in cartons.

*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
IF TYPE('lnCrtNo') = 'U' 
  lnCrtNo = 0
ENDIF
IF TYPE('lnRealWght') = 'U' 
  lnRealWght = 1
ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]


IF lcRpBrkTyp = 'W'
  lnCompCrt  = INT((OrdLine.TotPik * Style.nstyweight) / lnRealWght)
  lnFrcCrt   = MOD((OrdLine.TotPik * Style.nstyweight) / lnRealWght,1)
ELSE
  lnCompCrt  = INT(OrdLine.TotPik / (lnCrtNo*SCALE.Pptot) )
  lnFrcCrt   = MOD(OrdLine.TotPik / (lnCrtNo*SCALE.Pptot) ,1)
ENDIF
*--if it is not equal one CARTON
lnFlag   = 1
IF lnFrcCrt <> 0 AND lnCompCrt  > 0
  llChckCart = .T.
ELSE
  llChckCart = .F.
ENDIF
IF lnCompCrt  = 0
  lnCompCrt   = 1
  llChckCart = .F.
  *--- Pack in the last carton.
  lnFlag     = 2
  lnLastCrt = OrdLine.TotPik
ENDIF

*--calculate Total carton and Distrebute it as Equation
*--Total Weight / Break Weight = Total Cartons
*--lcOldPr : Old PrePack
*lcOldPr = Scale.prepak
*--llChngPrpk : variable to check if prepack changed
IF llChngPrpk
  RETURN .F.
ENDIF
lnOldNo  = lnNoCrt
FOR lnCount = 1 TO 8
  lnCont = STR(lnCount,1)
  laScalPr[lnCount] = SCALE.Pp&lnCont
ENDFOR
laScalPr[9] = SCALE.PpTot
*--- There is freactoin

*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
PRIVATE lcI,lnDivRet
lnDivRet = ORDLINE.TOTPIK/SCALE.PPTOT
FOR lnCount = 1 TO 8
  lcI = STR(lnCount,1)
  IF SCALE.PP&lcI>0 AND ORDLINE.PIK&lcI/SCALE.PP&lcI <> lnDivRet 
    RETURN .F.
  ENDIF
ENDFOR

*- if case of extended use another approach 
IF llExtSizSc 

  IF !lfUpdTmpEx()
    RETURN .F.
  ENDIF
  
ELSE
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
  IF llChckCart
    lnFrmCrt = IIF(lnFrmCrt = 1 ,lnFrmCrt,lnFrmCrt )
    lnToCrt  = lnToCrt -1
    lnNoCrt  = lnToCrt -1
  ENDIF
  =lfUpdTmp(lnNoCrt , lnFrmCrt , lnToCrt,lnFlag)
  IF llChckCart
    lnFrmCrt = lnToCrt +1
    lnToCrt  = lnToCrt +1
    lnNoCrt  = lnToCrt +1
    *--- Last Carton (piecies) = Total to Pack - (Total PrePack * Prepack PerCarton * No. of Completed Carton.)
    lnLastCrt = OrdLine.TotPik - laScalPr[9]*lnCrtNo*lnCompCrt
    lnFlag   = 2
    llCheck2 = .T.
    =lfUpdTmp(lnNoCrt , lnFrmCrt , lnToCrt,lnFlag)
  ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
ENDIF 
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
*-- store from carton to use again
lnStorFrm = lnToCrt +1
RETURN .T.

**********************************************************************************************************************************************************

FUNCTION lfpakbysz
PRIVATE lnIndex , lcIndex , lcCrit

*--- lnNoCrt,lnFrmCrt , lnToCrt
DIMENSION laQtyArr[9]
STORE 0 TO laQtyArr

IF lnFrmCrt < 2
  STORE 1 TO lnFrmCrt , lnToCrt
ENDIF
lnToCrt = IIF(lnToCrt =0,1,lnToCrt)
IF lnToCrt <> 0 .OR. lnFrmCrt >= 2
  lnFrmCrt = IIF (lnStorFrm = 0, IIF(lnFrmCrt = 0 ,1,lnFrmCrt), lnStorFrm)
  lnToCrt  = IIF (lnStorFrm = 0, IIF(lnToCrt = 0 ,1,lnToCrt), lnFrmCrt)
ENDIF
	
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
IF llRpMltSKU AND llRpUsePre 
  IF EMPTY(Style.PrePak) 
    =lfUpRej()
    llRejForPr = .T.  
    RETURN
  ELSE  
    IF !(lfClcult(lnNoCrt,lnFrmCrt,lnToCrt)) 
      =lfUpRej()
      llRejForPr = .T.  
      RETURN
    ENDIF
  ENDIF 
ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]

*-- Criteria used to close current carton and skip to the next
lcCrit = IIF(lcRpBrkTyp='W','lnWgInCrt  > lnRpWght',;
	IIF(lcRpBrkTyp='S','lnQtyInCrt > STYLE.QTY_CTN',;
	'lnQtyInCrt > lnRpMnlQty'))

=SEEK('S'+Style.Scale,'Scale')
FOR lnIndex = 1 TO SCALE.CNT
  lcIndex = STR(lnIndex,1)
  lnTempWgQt = OrdLine.Pik&lcIndex * IIF(lcRpBrkTyp='W',Style.nStyWeight,1)
  IF lnTempWgQt>0
    DO WHILE lnTempWgQt > 0
      lnWgInCrt = lnWgInCrt + IIF(Style.nStyWeight>0,Style.nStyWeight,1)
      lnQtyInCrt = lnQtyInCrt + 1
      IF &lcCrit .OR. IIF(llRpMltSku,.F.,lnQtyInCrt > OrdLine.Pik&lcIndex)
        IF laQtyArr[9] > 0
          =lfUpdCart(lnFrmCrt,lnToCrt,@laQtyArr)
          IF EOF("Style")
            =SEEK(OrdLine.Style,'Style')
          ENDIF
        ENDIF
        FOR lnInde1 = 1 TO 9
          laQtyArr[lnInde1] = 0
        ENDFOR
        lnToCrt  = lnToCrt + 1
        lnFrmCrt = lnToCrt
        STORE 0 TO lnWgInCrt,lnQtyInCrt
      ELSE
        laQtyArr[lnIndex] = laQtyArr[lnIndex]+1
        laQtyArr[9] = laQtyArr[9] + 1
        lnTempWgQt  = MAX(lnTempWgQt - IIF(lcRpBrkTyp='W',Style.nStyWeight,1) ,0)
      ENDIF
    ENDDO
    IF !llRpMltSku
      IF laQtyArr[9] > 0
        =lfUpdCart(lnFrmCrt,lnToCrt,@laQtyArr)
        IF EOF("Style")
          =SEEK(OrdLine.Style,'Style')
        ENDIF
      ENDIF
      STORE 0 TO laQtyArr
      lnToCrt  = lnToCrt + 1
      lnFrmCrt = lnToCrt
      lnWgInCrt = 0
      lnQtyInCrt = 0
    ENDIF
  ENDIF
ENDFOR
IF !llRpMltSku
  lnToCrt  = lnToCrt - 1
  lnFrmCrt = lnFrmCrt - 1
ENDIF
IF laQtyArr[9] > 0
  =lfUpdCart(lnFrmCrt,lnToCrt,@laQtyArr)
  IF EOF("Style")
    =SEEK(OrdLine.Style,'Style')
  ENDIF
ENDIF
lnStorFrm = lnFrmCrt

**********************************************************************************************************************************************************

FUNCTION lfpredist
*--llChngPrpk : variable to check if prepack changed
llChngPrpk = .F.
lcOldPr = Scale.prepak
lcPrePak= Scale.prepak
llChngPrRk =lfCheckPer('1')
IF llChngPrRk
  lnCurAlias  =SELECT(0)
  SELECT SCALE
  =SEEK('P'+Style.Scale)
  *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
  *SCAN FOR TYPE+Scale = 'P'+Style.Scale
  SCAN REST WHILE TYPE+Scale = 'P'+Style.Scale
  *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
 	llChngPrpk=lfCheckPer('2')
	IF llChngPrpk
      llChngPrpk = .F.
      EXIT
	ENDIF
  ENDSCAN
  IF EOF()
	llChngPrpk = .T.
  ENDIF
  SELECT (lnCurAlias)
ENDIF

**********************************************************************************************************************************************************

FUNCTION lfsupdqty
LPARAMETER laQtyArr

*--we will get prepak qty first and applay it for all code
*--when we reach to it we calculate
REPLACE Qty1       WITH laQtyArr[1] ,;
	Qty2       WITH laQtyArr[2] ,;
	Qty3       WITH laQtyArr[3] ,;
	Qty4       WITH laQtyArr[4] ,;
	Qty5       WITH laQtyArr[5] ,;
	Qty6       WITH laQtyArr[6] ,;
	Qty7       WITH laQtyArr[7] ,;
	Qty8       WITH laQtyArr[8] ,;
	TotQty     WITH laQtyArr[9] IN (poformclass.lcPakLin)

**********************************************************************************************************************************************************

FUNCTION lfupdcart
LPARAMETER lnFrmCrt,lnToCrt, laQtyArr

*-- Increment lnPicQty to calculate carton QTY
lnPicQty   = lnPicQty + laQtyArr[9]
lnOldweght = lnOldweght + laQtyArr[9]*Style.nStyweight

SELECT (poformclass.lcPakLin)
APPEND BLANK
REPLACE Pack_No    WITH OrdLine.PikTkt  ,;
	No_Cart    WITH lnNoCrt         ,;
	From_Crt   WITH lnFrmCrt        ,;
	To_Crt     WITH lnToCrt         ,;
	STYLE      WITH OrdLine.Style   ,;
	nOrdLineNo WITH OrdLine.LineNo  ,;
	Scale      WITH OrdLine.Scale   ,;
	PrePak     WITH OrdLine.PrePak  ,;
	OrgOrd1    WITH OrdLine.Qty1    ,;
	OrgOrd2    WITH OrdLine.Qty2    ,;
	OrgOrd3    WITH OrdLine.Qty3    ,;
	OrgOrd4    WITH OrdLine.Qty4    ,;
	OrgOrd5    WITH OrdLine.Qty5    ,;
	OrgOrd6    WITH OrdLine.Qty6    ,;
	OrgOrd7    WITH OrdLine.Qty7    ,;
	OrgOrd8    WITH OrdLine.Qty8    ,;
	TotOrgOrd  WITH OrdLine.TotQty  ,;
	Ord1       WITH OrdLine.Qty1    ,;
	Ord2       WITH OrdLine.Qty2    ,;
	Ord3       WITH OrdLine.Qty3    ,;
	Ord4       WITH OrdLine.Qty4    ,;
	Ord5       WITH OrdLine.Qty5    ,;
	Ord6       WITH OrdLine.Qty6    ,;
	Ord7       WITH OrdLine.Qty7    ,;
	Ord8       WITH OrdLine.Qty8    ,;
	TotOrd     WITH OrdLine.TotQty  ,;
	Pik1       WITH OrdLine.Pik1    ,;
	Pik2       WITH OrdLine.Pik2    ,;
	Pik3       WITH OrdLine.Pik3    ,;
	Pik4       WITH OrdLine.Pik4    ,;
	Pik5       WITH OrdLine.Pik5    ,;
	Pik6       WITH OrdLine.Pik6    ,;
	Pik7       WITH OrdLine.Pik7    ,;
	Pik8       WITH OrdLine.Pik8    ,;
	TotPik     WITH OrdLine.TotPik  ,;
	cZeroQty   WITH 'N'

=lfSUpdQty(@laQtyArr)
*-- End lfUpdTmp

**********************************************************************************************************************************************************

FUNCTION lfupdhdr
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
IF llRejForPr
  llRejForPr = .F.
  RETURN
ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

IF !SEEK(OrdLine.PikTkt,poformclass.lcPakHdr)
  INSERT INTO (poformclass.lcPakHdr) (Pack_No,Order,Store,Account,AccName) VALUES ;
              (OrdLine.PikTkt,OrdLine.Order,OrdLine.Store,OrdLine.Account,Customer.btName)
ENDIF
REPLACE  Tot_Cart WITH lnToCrt        ,;
         Tot_Pcs  WITH Tot_Pcs  + OrdLine.TotPik ,;
         Tot_Wght WITH Tot_Wght + (OrdLine.TotPik * Style.nStyWeight);
         Pick_Qty WITH Pick_Qty + OrdLine.TotPik;
         Order_Qty WITH Order_Qty + OrdLine.TotQTY IN (poformclass.lcPakHdr)
         
**********************************************************************************************************************************************************

FUNCTION lfupdqty
LPARAMETER  lnFlag
PRIVATE lnQty1 , lnQty2, lnQty3, lnQty4, lnQty5, lnQty6, lnQty7, lnQty8
PRIVATE lnNCrton
STORE 0 TO lnTotQty
*--we will get prepak qty first and applay it for all code
*--when we reach to it we calculate
FOR lnCountr = 1 TO 8
  lnCotr= STR(lnCountr,1)
  IF lnFlag = 1
	lnQty&lnCotr = IIF(OrdLine.Pik&lnCotr <> 0 ,laScalPr[lnCountr]*lnCrtNo,0)
	lnTotQty = lnTotQty +lnQty&lnCotr
  ELSE
	IF llCheck2
      lnQty&lnCotr = IIF(OrdLine.Pik&lnCotr <> 0 ,OrdLine.Pik&lnCotr - laScalPr[lnCountr]*lnCrtNo*lnCompCrt,0)
	ELSE
      lnQty&lnCotr = IIF(OrdLine.Pik&lnCotr <> 0 .AND. laScalPr[9] <>0,laScalPr[lnCountr]*(lnLastCrt/laScalPr[9]),0)
    ENDIF
	lnTotQty = lnTotQty +lnQty&lnCotr
  ENDIF
ENDFOR
FOR lnTInd = 1 TO 8
  lcTInd = STR(lnTInd,1)
  REPLACE Qty&lcTInd WITH ABS(lnQty&lcTInd)
ENDFOR
REPLACE TotQty WITH ABS(lnTotQty)
llCheck2 = .F.

**********************************************************************************************************************************************************

FUNCTION lfupdtmp
LPARAMETER lnNoCrt , lnFrmCrt , lnToCrt ,lnFlag

SELECT (poformclass.lcPakLin)
APPEND BLANK
REPLACE Pack_No    WITH OrdLine.PikTkt  ,;
	No_Cart    WITH lnNoCrt         ,;
	From_Crt   WITH lnFrmCrt        ,;
	To_Crt     WITH lnToCrt         ,;
	STYLE      WITH OrdLine.Style   ,;
	nOrdLineNo WITH OrdLine.LineNo  ,;
	Scale      WITH OrdLine.Scale   ,;
	PrePak     WITH OrdLine.PrePak  ,;
	OrgOrd1    WITH OrdLine.Qty1    ,;
	OrgOrd2    WITH OrdLine.Qty2    ,;
	OrgOrd3    WITH OrdLine.Qty3    ,;
	OrgOrd4    WITH OrdLine.Qty4    ,;
	OrgOrd5    WITH OrdLine.Qty5    ,;
	OrgOrd6    WITH OrdLine.Qty6    ,;
	OrgOrd7    WITH OrdLine.Qty7    ,;
	OrgOrd8    WITH OrdLine.Qty8    ,;
	TotOrgOrd  WITH OrdLine.TotQty  ,;
	Ord1       WITH OrdLine.Qty1    ,;
	Ord2       WITH OrdLine.Qty2    ,;
	Ord3       WITH OrdLine.Qty3    ,;
	Ord4       WITH OrdLine.Qty4    ,;
	Ord5       WITH OrdLine.Qty5    ,;
	Ord6       WITH OrdLine.Qty6    ,;
	Ord7       WITH OrdLine.Qty7    ,;
	Ord8       WITH OrdLine.Qty8    ,;
	TotOrd     WITH OrdLine.TotQty  ,;
	Pik1       WITH OrdLine.Pik1    ,;
	Pik2       WITH OrdLine.Pik2    ,;
	Pik3       WITH OrdLine.Pik3    ,;
	Pik4       WITH OrdLine.Pik4    ,;
	Pik5       WITH OrdLine.Pik5    ,;
	Pik6       WITH OrdLine.Pik6    ,;
	Pik7       WITH OrdLine.Pik7    ,;
	Pik8       WITH OrdLine.Pik8    ,;
	TotPik     WITH OrdLine.TotPik  ,;
	cZeroQty   WITH 'N'
=lfUpdQty(lnFlag)

**********************************************************************************************************************************************************

FUNCTION lfupdtmpex
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
PRIVATE lnSlct
lnSlct = SELECT()

lcPakLin   = poformclass.lcPakLin

lnNoCrt = 0
m.No_Cart = 1

SELECT ORDLINE 
SCATTER MEMVAR

*- initiate varibles that will added to lcPakLin temp file  
M.Pack_No    = OrdLine.PikTkt
m.nOrdLineNo = OrdLine.LineNo 
STORE OrdLine.Qty1   TO m.OrgOrd1,m.Ord1
STORE OrdLine.Qty2   TO m.OrgOrd2,m.Ord2
STORE OrdLine.Qty3   TO m.OrgOrd3,m.Ord3
STORE OrdLine.Qty4   TO m.OrgOrd4,m.Ord4
STORE OrdLine.Qty5   TO m.OrgOrd5,m.Ord5
STORE OrdLine.Qty6   TO m.OrgOrd6,m.Ord6
STORE OrdLine.Qty7   TO m.OrgOrd7,m.Ord7
STORE OrdLine.Qty8   TO m.OrgOrd8,m.Ord8
STORE OrdLine.TotQty TO m.TotOrgOrd,m.TotOrd 
m.cZeroQty   = 'N'

STORE ORDLINE.TOTPIK TO m.OrgTotPk
STORE 0 TO m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TotQty

*- create temp cursor that store cartons data
IF !USED(lcPakCur)
  CREATE CURSOR (lcPakCur) (PACK_NO C(6),CARTON N(4),STYLE C(19),BREAK N(6),CURRENT N(6))
  INDEX ON PACK_NO+STYLE+STR(CARTON,4) TAG (lcPakCur)
ENDIF

SELECT &lcPakLin
lcSvOrd = ORDER()
*- Use the descending order to get the last carton # used
SET ORDER TO &lcSvOrd DESC
LOCATE
IF !SEEK(OrdLine.PikTkt,lcPakLin)
  STORE 1 TO m.From_Crt,m.To_Crt
  =lfAddCrt(1)
ELSE
  STORE &lcPakLin..To_Crt TO m.From_Crt,m.To_Crt
  GO BOTTOM IN (lcPakCur)
  IF lcRpBrkTyp = 'S'
    
    *- in this case a style is added to a carton noting the max qty needed for this style
    *- only one style per carton maximum
    
    *- if the style is added befor get the first carton # to use , 
    *-    if it is the same scale size (i.e. the same style - color - scale ) is repeated in the order twice , this case is not included in the code
    *-    if the style-color is added befor but this is another scale then user the first carton # for the first style-color-scale added , 
    *-*   this behaviour means that we look at extended case style as a one unit , and it will be added to the same carton based on ctn_qty 
    *-*   field per scale
    IF SEEK( ORDLINE.PIKTKT+SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnCLrLen-1) , lcPakCur )
      STORE EVALUATE(lcPakCur+'.CARTON') TO m.From_Crt, m.To_Crt
      IF !SEEK(ORDLINE.PIKTKT+ORDLINE.STYLE,lcPakCur)
        =lfAddCrt(m.To_Crt)
      ENDIF
    ELSE
      *- if this is a new style added then get the max carton # used for this pack and the add 1 to it , use this number for this added style
      SELECT (lcPakCur)
      =SEEK(ORDLINE.PIKTKT,lcPakCur)
      CALCULATE MAX(CARTON+1) REST WHILE PACK_NO+STYLE+STR(CARTON,4) = ORDLINE.PIKTKT TO lnNewNum
      STORE lnNewNum TO m.From_Crt, m.To_Crt
      =lfAddCrt(m.To_Crt)
    ENDIF
  ENDIF
ENDIF

*- reset the order to the ascending
SELECT &lcPakLin
SET ORDER TO &lcSvOrd ASCENDING
LOCATE

*-- case of lcRpBrkTyp is 'W','M' or 'S'  
=SEEK(ORDLINE.STYLE,'STYLE')
lnStyW = IIF(lcRpBrkTyp = 'W' , STYLE.NSTYWEIGHT , 1 )
  
*-  
  *- loop while there is still qty not packed, we base on the break and current fields in lcPakCur,
  *- if the new pack qty ,( or wight if multiplied by lnStyW ) does not fit in the carton , then 
  *- this is a line that will be added to lcPackLin and add a new carton and start filling it until you reach the break value
DO WHILE m.OrgTotPk > 0
  
  *- Check if current is 0 and the laScalePr[9]*lnStyW does not fit in the carton then reject
  IF EVALUATE(lcPakCur+'.CURRENT') = 0 .AND. laScalPr[9]*lnStyW > EVALUATE(lcPakCur+'.BREAK')
    RETURN .F.
  ENDIF
    
  llAddLine = .F.
  
  *- put all the quantities in ordline file into cartons, if a carton is filled skip to another carton    
  IF EVALUATE(lcPakCur+'.BREAK') >= EVALUATE(lcPakCur+'.CURRENT') + laScalPr[9]*lnStyW
    
    m.Qty1 = m.Qty1 + laScalPr[1]
    m.Qty2 = m.Qty2 + laScalPr[2]
    m.Qty3 = m.Qty3 + laScalPr[3]
    m.Qty4 = m.Qty4 + laScalPr[4]
    m.Qty5 = m.Qty5 + laScalPr[5]
    m.Qty6 = m.Qty6 + laScalPr[6]
    m.Qty7 = m.Qty7 + laScalPr[7]
    m.Qty8 = m.Qty8 + laScalPr[8]
    m.TotQty = m.TotQty + laScalPr[9]
    
    m.OrgTotPk = m.OrgTotPk - laScalPr[9]
    
    SELECT (lcPakCur)
    REPLACE CURRENT WITH CURRENT + laScalPr[9]*lnStyW
    
  ELSE      
    
    llAddLine = .T.
    
  ENDIF
  
  *- go here if break value is reached or the qty needed to be packed if exhusted
  IF llAddLine .OR. m.OrgTotPk = 0
    
    IF m.TotQty > 0

       *- if the same style added and same pack distribution then just increase the To_crt field    
       IF M.STYLE = &lcPakLin..STYLE .AND. ;
         M.QTY1 = &lcPakLin..QTY1 .AND. ;
         M.QTY2 = &lcPakLin..QTY2 .AND. ;
         M.QTY3 = &lcPakLin..QTY3 .AND. ;
         M.QTY4 = &lcPakLin..QTY4 .AND. ;
         M.QTY5 = &lcPakLin..QTY5 .AND. ;
         M.QTY6 = &lcPakLin..QTY6 .AND. ;
         M.QTY7 = &lcPakLin..QTY7 .AND. ;
         M.QTY8 = &lcPakLin..QTY8
        
        SELECT &lcPakLin
        REPLACE To_Crt  WITH To_Crt + 1  ;
                No_Cart WITH No_Cart + 1
      ELSE           
        INSERT INTO &lcPakLin FROM MEMVAR
      ENDIF
    ENDIF
    
    *- if there is still qty not packed then add a new carton to pack it in it
    IF m.OrgTotPk > 0
      
      STORE m.To_Crt + 1 TO m.From_Crt, m.To_Crt   
      =lfAddCrt(m.To_Crt)
      
      STORE 0 TO m.QTY1,m.QTY2,m.QTY3,m.QTY4,m.QTY5,m.QTY6,m.QTY7,m.QTY8,m.TotQty      
      
    ENDIF
    
  ENDIF    
  
ENDDO

*- Get number of cartons for the case of break type 'S'
IF lcRpBrkTyp = 'S'
  SELECT (lcPakCur)
  =SEEK(ORDLINE.PIKTKT,lcPakCur)
  CALCULATE MAX(CARTON) REST WHILE PACK_NO+STYLE+STR(CARTON,4) = ORDLINE.PIKTKT TO lnNoCrt
ENDIF
  
SELECT (lnSlct)
*-- end of lfUpdTmpEx.
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

**********************************************************************************************************************************************************

FUNCTION lfuprej
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
*lcReason = IIF(This.lcRpBrkTyp='W',IIF(Style.nstyweight > This.lnRpWght,;
	"Style Weight is greater than break weight.",;
	"There is no style weight."),;
	IIF(This.lcRpBrkTyp='S',"Style Qty per carton is 0.",''))
*B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start] 
*lcReason = IIF(lcRpBrkTyp='W',IIF(Style.nstyweight > lnRpWght,;
	"Style Weight is greater than break weight.",IIF(Style.nstyweight > 0 ,"Failed Transfere To Perpack Qty",;
	"There is no style weight.")),;
	IIF(lcRpBrkTyp='S' AND  STYLE.QTY_CTN = 0 ,"Style Qty per carton is 0.",'Failed Transfere To Perpack Qty '))	
	
lcReason = IIF(lcRpBrkTyp='W',IIF(Style.nstyweight > lnRpWght,;
  "Style Weight is greater than break weight.",IIF(Style.nstyweight > 0 ,"Failed Transfere To Perpack Qty",;
  "There is no style weight.")),;
  IIF(lcRpBrkTyp='S' AND  STYLE.QTY_CTN = 0 ,"Style Qty per carton is 0.",IIF(lcRpBrkTyp = 'P','No packs assigned to this pick ticket','Failed Transfere To Perpack Qty')))  
*B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End] 
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]	
*B612249,1 MMT 11/04/2020 if user checked Generate BOL check box, the Selected Order got picked only without Packing list and empty BOL is created[Start]
lcBolNo = EVALUATE(poformclass.lcPakHdr+".bill_ladg")
*B612249,1 MMT 11/04/2020 if user checked Generate BOL check box, the Selected Order got picked only without Packing list and empty BOL is created[End]
lcDelPik = OrdLine.PikTkt
IF SEEK(lcDelPik,poformclass.lcPakHdr)
  SELECT (poformclass.lcPakHdr)
  DELETE REST WHILE Pack_No = lcDelPik
ENDIF
IF SEEK(lcDelPik,poformclass.lcPakLin)
  SELECT (poformclass.lcPakLin)
  DELETE REST WHILE Pack_No = lcDelPik
ENDIF
*B612249,1 MMT 11/04/2020 if user checked Generate BOL check box, the Selected Order got picked only without Packing list and empty BOL is created[Start]
IF llEdiSys AND llRpBolAsi
  STORE .F. TO llOpenBOl_HDR,llOpenBOl_LIN
  IF !USED('BOL_HDR')
    =gfOpenTable('BOL_HDR','BOL_HDR','SH')
    llOpenBOl_HDR = .T.
  ENDIF
  IF !USED('BOL_LIN')
    =gfOpenTable('BOL_LIN','BOL_LIN','SH')
    llOpenBOl_LIN = .T.
  ENDIF
  SELECT BOL_HDR
  IF gfSeek(lcBolNo,'BOL_HDR','BOL_HDR') AND !gfSeek(lcBolNo,'BOL_LIN','BOL_LIN')
    SELECT BOL_HDR
    =gfDelete()
    =gfTableUpdate()
  ENDIF
  IF llOpenBOl_HDR
    =gfCloseTable("BOL_HDR")
  ENDIF  
  IF llOpenBOl_LIN  
    =gfCloseTable("BOL_LIN")  
  ENDIF  
ENDIF
*B612249,1 MMT 11/04/2020 if user checked Generate BOL check box, the Selected Order got picked only without Packing list and empty BOL is created[End]
IF !SEEK(lcDelPik,lcRejPkTmp)
  INSERT INTO (lcRejPkTmp) (PikTkt,PikDate,Order,Account,AccName,cReason,Store) VALUES ;
  (OrdLine.PikTkt,OrdLine.PikDate,OrdLine.Order,OrdLine.Account, Customer.BtName,lcReason,OrdLine.Store)
ENDIF

**********************************************************************************************************************************************************

FUNCTION massignbol
LPARAMETERS lcSeekOrd, lcAccount, lcStore

lnActvAlis = SELECT()
*-- if this Piktkt is in ORDHDR get the ship via
IF SEEK(lcSeekOrd,'ORDHDR','ORDHDR')
  *--Get or Create BOL for this customer,Dist. Center,store,and shipvia
  PRIVATE laFields
  *-- array to hold defulted fields for BOL
  DIMENSION laFields[3,2]
  laFields[1,1] = "cgronhang"
  laFields[1,2] = "N"
  laFields[2,1] = "ctranmthd"
  laFields[2,2] = "M"
  laFields[3,1] = "packtype"
  laFields[3,2] = "CTN25"
  lcTitle = "for Packing lst " + Ordline.Piktkt
  lcShipVia = OrdHdr.ShipVia
  lcDistCtr = lcStore
  *-- Get the value of Distrepution center From Customer file
  IF SEEK('S'+lcAccount+lcStore,'Customer')
    lcDistCtr = IIF(EMPTY(Customer.Dist_Ctr),Customer.Store,Customer.Dist_Ctr)
    lcShipVia = IIF(ALLTRIM(lcShipVia)='*',Customer.ShipVia,lcShipVia)
  ENDIF
  =SEEK('A' + lcAccount , 'EDIACPRT') .AND. SEEK(EDIACPRT.cPartCode , 'EDIPH')
  lcTitle = "for Packing lst " + &lcPakHdr..Pack_No
  lcBol = poAlClass.lfGetBOL(lcTitle,lcAccount,lcDistCtr,OrdHdr.cWareCode,lcShipVia,IIF(EDIPH.cCrtnType='S','Y','N'),'laFields',IIF(lcRpBrkTyp = 'I',.T.,.F.),.F.,'S')
  
  REPLACE Bill_ladg WITH lcBol ,;
          cWareCode WITH OrdHdr.cWareCode ,;
          ShipVia   WITH lcShipVia ,;
          lStandCtn WITH EDIPH.cCrtnType='S' IN (poformclass.lcPakHdr)
ENDIF  && end if this Piktkt is in ORDHDR get the ship via
SELECT (lnActvAlis)

**********************************************************************************************************************************************************

FUNCTION mchangemode
LPARAMETERS lcModetoChange
IF lcModetoChange='S'
  ariapageframe1.ariapage1.grdRejected.RecordSource = ''
  ariapageframe1.ariapage2.grdPckLst.RecordSource   = ''
  ariapageframe1.ariapage3.grdPckDetail.RecordSource = ''
  poformclass.mCreateTempFiles()
  CREATE TABLE (oAriaEnvironment.WorkDir+lcRejPkTmp) ;
  (PikTkt C(6), PikDate D, Order C(6), Account C(5), AccName C(30), Store C(8), CReason C(50))
  INDEX ON PikTkt TAG (lcRejPkTmp)
ENDIF
WITH ariapageframe1.ariapage2
  STORE .F. TO .keyStore.Enabled, .keyPikTKt.Enabled, .keyOrder.Enabled, .keyBOL.Enabled, .keyAccount.Enabled,;
               .txtAccName.Enabled, .txtStoreName.Enabled
ENDWITH
WITH ariapageframe1.ariapage3
  STORE .F. TO .keyItem.Enabled, .txtItemDesc.Enabled, .txttotQty.Enabled
ENDWITH
mRecordChanged()

**********************************************************************************************************************************************************

FUNCTION mgtclrdt
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
llUseColor = .F.
DECLARE laItemSeg[1]
* HUSSEIN
*=gfItemMask(@laItemSeg)
LOCAL loGetItemMask
loGetItemMask = CREATEOBJECT('ariamain.GetItemMask')
loGetItemMask.Do(@laItemSeg)
* HUSSEIN

FOR lnCount = 1 TO ALEN(laItemSeg , 1)
	IF laItemSeg[lnCount,1] = 'C'
		llUseColor = .T.
		lnClrLen   = LEN(laItemSeg[lnCount,3])
		lnClrPos   = laItemSeg[lnCount,4]
		EXIT
	ENDIF
ENDFOR

*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

**********************************************************************************************************************************************************

FUNCTION mlblsetup
PRIVATE lcOldPort , llOldPrn , llCancel
lcOldPort = ThisFormset.lcSndPort
llOldPrn  = ThisFormset.llScrPrnLb
llCancel = .F.
DO FORM (oAriaEnvironment.ScreenHome+ 'ALOUTPRT.SCX') WITH ThisFormset TO llCancel
IF llCancel
  ThisFormset.lcSndPort  = lcOldPort
  ThisFormset.llScrPrnLb = llOldPrn
ENDIF
*poformclass.lcSndPort =  ThisFormset.lcSndPort
*This.poformclass.llScrPrnLb =  ThisFormset.llScrPrnLb 

**********************************************************************************************************************************************************

FUNCTION moptiongrid

*--Define variables that used in OG

STORE 0 TO lnRpWght, lnRpTtCrtP, lnRpMnlQty
STORE .F. TO llRpMltSku, llRp1BoxPk
STORE ' ' TO lcRpBrkTyp, lcRpSortBy, lcRPCart
STORE .F. TO llRpBolAsi, llRpUseExs, llRpUsePre
STORE .F. TO llCheck2 ,llChngPrpk

STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt
*--variable to Store the FRom Carton Number
*-- initialize variables
STORE 0 TO lnRemendr , lnPicQty
STORE 0 TO lnStorFrm
STORE 0 TO lnOldCrtNo, lnOldweght, lnWgInCrt, lnQtyInCrt
lcLastSty = ' '
llEdiSys  =  ('AS' $ oAriaEnvironment.CompanySetupModules)
lcExpr = '.F.'

lcPakHdr = poformclass.lcPakHdr
lcPakLin = poformclass.lcPakLin
STORE SPACE(6) TO lcDelPik,lcPikTkt

*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
STORE .F. TO llRejForPr
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]. 


*lcEscTrap = ON("KEY","ESC")

* MAH ADD
RELEASE  laoghdflt, laogfxflt, laogvrflt
DECLARE laoghdflt[1, 8]
DECLARE laogfxflt[1, 8]
DECLARE laogvrflt[1, 8]
oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(loXMLPackingOptionPointer), .F.)

lcExpr = lcrpExp

IF lcRpBrkTyp = 'I'
	poformclass.CartonId = lcRPCart
	poformclass.AssignBol = llRpBolAsi
	poformclass.CrtnPerPallete = lnRpTtCrtP
	lcRpBrkTyp = lcRpBrkTyp
	lnRpWght = lnRpWght
	*--if the sort is by order line no change the order in the ordline table to be on ordline not in style
	IF lcRpSortBy = 'O'
	  SET ORDER TO TAG Ordline IN Ordline
	ENDIF
	*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
	STORE 0 TO lnclrlen ,lnclrpos 
	mgtclrdt ()
	*--check for Extended Size Scale
	llextsizsc = oAriaEnvironment.setups.getSetting('M_USEEXSSC',oAriaEnvironment.ActiveCompanyID)
	*- Temp cursor used to store carton data while filling 
	lcPakCur = '_'+SUBSTR(lcPakLin,2)                 
	IF USED(lcPakCur)
	  USE IN (lcPakCur)
	ENDIF  

  SELECT PikTkt
  SET ORDER TO 0
  SET RELATION TO  IIF(EMPTY(STORE),'M'+Account,"S"+Account+STORE) INTO Customer ADDITIVE
  SET RELATION TO 'O' + ORDER INTO OrdLine ADDITIVE
  SET SKIP TO OrdLine
  
  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start] 
  *lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
          	  IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0','.F.'))
  lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
          	  IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0',IIF(lcRpBrkTyp = 'P','EMPTY(ordline.pack_id)','.F.')))
  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End] 
  
  *-- Don't allow creating packing list on completed or Hold pick ticket
  SCAN FOR &lcExpr AND PIKTKT # '******' AND PikTkt = OrdLine.PikTkt AND STATUS $ 'OP'
	*WAIT "Preparing Data. Picking ticket# " + PikTkt.PikTkt WINDOW NOWAIT
	IF lcPikTkt # OrdLine.PikTkt
      STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt
      lcPikTkt = OrdLine.PikTkt
      STORE 0 TO lnStorFrm ,lnOldCrtNo ,lnPicQty ,lnOldweght
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
      *lcLastSty = ORDLINE.STYLE
      lcLastSty = SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnClrLen-1)
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
      lnWgInCrt = 0
      lnQtyInCrt = 0
      *-- Save BOL on Packing Slip level.
      SELECT (lcPakHdr)
      IF !SEEK(OrdLine.PikTkt)
        APPEND BLANK
    		REPLACE Pack_No  WITH OrdLine.PikTkt  ,;
          			ORDER    WITH OrdLine.Order   ,;
          			STORE    WITH OrdLine.Store   ,;
        				Account  WITH OrdLine.Account ,;
				        AccName  WITH IIF(Customer.Type="M",Customer.BtName,Customer.StName)
		=IIF(llEdiSys AND llRpBolAsi,mAssignBol(OrdLine.cOrdType + OrdLine.Order, OrdLine.Account, OrdLine.Store),.T.)
	   ENDIF
     ENDIF
   ENDSCAN
   
   SELECT PikTkt
   SET RELATION TO
   = PackByOdrerPackingInfo()

   RETURN
ENDIF



* MAH ADD
*lcExpr = gfOpGrid('ALAUTP',.T. ,.F. ,.F. ,.T. ,.T.)

*mOptionGrid()


*ON KEY LABEL ESCAPE &lcEscTrap
poformclass.CartonId = lcRPCart
poformclass.AssignBol = llRpBolAsi
poformclass.CrtnPerPallete = lnRpTtCrtP
lcRpBrkTyp = lcRpBrkTyp
lnRpWght = lnRpWght
*--if the sort is by order line no change the order in the ordline table to be on ordline not in style
IF lcRpSortBy = 'O'
  SET ORDER TO TAG Ordline IN Ordline
ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
STORE 0 TO lnclrlen ,lnclrpos 
mgtclrdt ()
*--check for Extended Size Scale
llextsizsc = oAriaEnvironment.setups.getSetting('M_USEEXSSC',oAriaEnvironment.ActiveCompanyID)
*- Temp cursor used to store carton data while filling 
lcPakCur = '_'+SUBSTR(lcPakLin,2)                 
IF USED(lcPakCur)
  USE IN (lcPakCur)
ENDIF  
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

IF !(TYPE('lcExpr') $ 'UL') AND lcExpr <> '.F.'
  SELECT PikTkt
  SET ORDER TO 0
  SET RELATION TO  IIF(EMPTY(STORE),'M'+Account,"S"+Account+STORE) INTO Customer ADDITIVE
  SET RELATION TO 'O' + ORDER INTO OrdLine ADDITIVE
  SET SKIP TO OrdLine
  
  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start] 
  *lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
          	  IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0','.F.'))
  lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
          	  IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0',IIF(lcRpBrkTyp = 'P','EMPTY(ordline.pack_id)','.F.')))
  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End] 
  
  *-- Don't allow creating packing list on completed or Hold pick ticket
  SCAN FOR &lcExpr AND PIKTKT # '******' AND PikTkt = OrdLine.PikTkt AND STATUS $ 'OP'
	*WAIT "Preparing Data. Picking ticket# " + PikTkt.PikTkt WINDOW NOWAIT
	IF lcPikTkt # OrdLine.PikTkt
      STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt
      lcPikTkt = OrdLine.PikTkt
      STORE 0 TO lnStorFrm ,lnOldCrtNo ,lnPicQty ,lnOldweght
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
      *lcLastSty = ORDLINE.STYLE
      lcLastSty = SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnClrLen-1)
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
      lnWgInCrt = 0
      lnQtyInCrt = 0
      *-- Save BOL on Packing Slip level.
      SELECT (lcPakHdr)
      IF !SEEK(OrdLine.PikTkt)
        APPEND BLANK
    		REPLACE Pack_No  WITH OrdLine.PikTkt  ,;
          			ORDER    WITH OrdLine.Order   ,;
          			STORE    WITH OrdLine.Store   ,;
        				Account  WITH OrdLine.Account ,;
				        AccName  WITH IIF(Customer.Type="M",Customer.BtName,Customer.StName)
		=IIF(llEdiSys AND llRpBolAsi,mAssignBol(OrdLine.cOrdType + OrdLine.Order, OrdLine.Account, OrdLine.Store),.T.)
	 ENDIF
   SELECT PikTkt
ENDIF
   =SEEK(OrdLine.Style,'Style')
	*-- The case that all the piktkt is shipped in one carton - this is unusual case
	*-- In this case no check will be performed on style.nstyweight or style.qty_ctn fields
	IF llRp1BoxPk
	  SELECT ORDLINE
	  SCATTER FIELDS Pik1, Pik2, Pik3, Pik4, Pik5, Pik6, Pik7, Pik8, TotPik TO laQtyArr
	  IF laQtyArr[9] > 0
    	lfUpdCart(1,1,@laQtyArr)
      ENDIF 
      REPLACE  Tot_Cart  WITH 1  ,;
               Tot_Pcs   WITH Tot_Pcs   + OrdLine.TotPik ,;
               Tot_Wght  WITH Tot_Wght  + (OrdLine.TotPik * Style.nStyWeight);
               Pick_Qty  WITH Pick_Qty  + OrdLine.TotPik;
               Order_Qty WITH Order_Qty + OrdLine.TotQTY IN (lcPakHdr)
      LOOP
	ENDIF
	IF lcRpBrkTyp = 'P'
	  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start] 
	  *=SEEK('P'+ordline.account+ordline.pack_id+ordline.style,'SPCK_LIN','SPCK_LIN')
	  IF !&lcRejCrit AND SEEK('P'+ordline.account+ordline.pack_id+ordline.style,'SPCK_LIN','SPCK_LIN')
	  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End] 
    
   	  SELECT SPCK_LIN
  	  SCATTER FIELDS qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8, Totqty TO laQtyArr
        lnStorFrm = IIF(ORDLINE.pack_id = lcLastSty,lnStorFrm ,lnToCrt + 1)
        lnNoCrt = ordline.totpik/spck_lin.totqty
        lnToCrt =  lnStorFrm + lnNoCrt - 1
        lcLastSty = ordline.pack_id
  	  IF laQtyArr[9] > 0
      	lfUpdCart(lnStorFrm,lnToCrt,@laQtyArr)
        ENDIF 
        REPLACE  Tot_Cart  WITH lnToCrt  ,;
                 Tot_Pcs   WITH Tot_Pcs   + laQtyArr[9] ,;
                 Tot_Wght  WITH Tot_Wght  +(laQtyArr[9] * Style.nStyWeight);
                 Pick_Qty  WITH Pick_Qty  + laQtyArr[9];
                 Order_Qty WITH Order_Qty + laQtyArr[9] IN (lcPakHdr)
  	  LOOP
      
     *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start] 
     ELSE
       =lfUpRej()
       LOOP 
     ENDIF 
     *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End] 
     
	ENDIF
    *-- If style changed , pack the new style in another carton
	IF lcRpBrkTyp $ 'S' .OR. llRpUsePre
      lnStorFrm = IIF(ORDLINE.STYLE = lcLastSty,lnStorFrm ,lnToCrt + 1)
      lnQtyInCrt = 0
      lnWgInCrt = 0
    ENDIF
    *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
	*lcLastSty = ORDLINE.STYLE
	lcLastSty = SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnClrLen-1)
	*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
    *-- Single Sku in each carton
	IF !llRpMltSku
      IF &lcRejCrit
		=lfUpRej()
      ELSE
		*-- Pack by size with option "Multiple SKU = 'N' "
		IF OrdLine.PikTkt <> lcDelPik
          =lfPakbySz()
          =lfUpdHdr()
		ENDIF
      ENDIF
      LOOP
	ENDIF

    IF SEEK(OrdLine.Style,'Style') .AND. SEEK('P'+Style.Scale+Style.PrePak,'Scale')
      SELECT Scale
      IF Scale.PPTot <> 0
        lcBrkTypCr = IIF(lcRpBrkTyp='W','Style.nStyWeight <> 0 .AND. Style.nStyWeight <= lnRpWght',;
                     IIF(lcRpBrkTyp='S','Style.QTY_CTN    <> 0 ','.T.'))
        IF &lcBrkTypCr .AND. !SEEK(lcPikTkt,lcRejPkTmp)
          lcPreWght = Scale.PPTot * Style.nstyweight
          IF OrdLine.PikTkt <> lcDelPik AND ;
            IIF(lcRpBrkTyp = 'W',lnRpWght > Style.nstyweight AND lnRpWght >= lcPreWght,;
            IIF(lcRpBrkTyp = 'S',Style.QTY_CTN >=SCALE.Pptot,lnRpMnlQty >=SCALE.Pptot))
            =lfPreDist()
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly
            *! when choosing by style quantit and by preback =yes  ----  [start]
            =SEEK('P'+Style.Scale+Style.PrePak,'Scale')
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly ----  [end]
            lcPreWght = Scale.PPTot * Style.nstyweight
            *--- lnRpWght: Carton Wieght.
            *--- lcPreWght: PrePack Wieght.
            *--- lnCrtNo: TotNo of PrePack per carton.
            STORE 0 TO lnCrtNo,lnNoCrt
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly
            *! when choosing by style quantit and by preback =yes  ----  [start]
            *!*	  IF lcPreWght = 0 
            IF lcPreWght = 0 AND !lcRpBrkTyp = 'S'
              llChngPrpk = .T.
              lcPreWght = Style.nstyweight
            ENDIF
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly ----  [end]
            
            *-- Check if the totpick weight < Break weight
            *-- calculate carton number depend on the totpick
            *-- else calculate depend on the break weight.
            lnHQty = IIF(lcRpBrkTyp = 'S',STYLE.QTY_CTN,lnRpMnlQty)
            IF lcRpBrkTyp = 'W'
              IF Ordline.TotPik * Style.nstyweight < lnRpWght
                lnCrtNo = INT(Ordline.TotPik * Style.nstyweight / lcPreWght)
              ELSE
                lnCrtNo = INT(lnRpWght / lcPreWght)
              ENDIF
            ELSE
              IF !EOF('SCALE')
                IF Ordline.TotPik < lnHQty
                  lnCrtNo = INT(Ordline.TotPik/SCALE.Pptot)
                ELSE
                  lnCrtNo = INT(lnHQty/SCALE.Pptot)
                ENDIF
              ENDIF
            ENDIF
            *--If the carton no = 0 then change to case 3 or case 6
            IF lnCrtNo = 0
              llChngPrpk = .T.
            ENDIF
            IF lcRpBrkTyp = 'W'
              *--- lnRealWght : (No. of prepack per carton* PrePack Weight)
              *-- Check if the carton=0 replace it with 1
              lnRealWght = IIF(lnCrtNo>0,lnCrtNo,1) * lcPreWght
              *--- lnNoCrt : Tot No of Carton = (Total style weight / Weight per carton)
              lnNoCrt = CEILING((OrdLine.TotPik * Style.nstyweight) / lnRealWght)
            ELSE
              *--- lnNoCrt : Tot No of Carton = (Total style qty / qty per carton)
              IF lnCrtNo <> 0 AND !EOF('SCALE')
                lnNoCrt = CEILING( OrdLine.TotPik / (lnCrtNo*SCALE.Pptot))
              ELSE
                lnNoCrt = 1
              ENDIF
            ENDIF
            lnFrmCrt = lnToCrt  + 1
            lnToCrt  = lnFrmCrt + lnNoCrt -  1
            *-- Function to calculate cartons
            *-- Add Option to pack with Prepack or without Prepack in the option grid.
            
            *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
            *llReturn = llRpUsePre AND this.lfClcult(lnNoCrt,lnFrmCrt,lnToCrt)
            IF llExtSizSc
          	  llReturn = llRpUsePre AND lfClcult(@lnNoCrt,lnFrmCrt,lnToCrt)
   			ELSE
   	          llReturn = llRpUsePre AND lfClcult(lnNoCrt,lnFrmCrt,lnToCrt)
      		ENDIF
            *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
            
            IF llReturn
              SELECT (lcPakHdr)
              IF !SEEK(OrdLine.PikTkt)
                APPEND BLANK
                REPLACE Pack_No  WITH OrdLine.PikTkt  ,;
                        ORDER    WITH OrdLine.Order   ,;
                        STORE    WITH OrdLine.Store   ,;
                        Account  WITH OrdLine.Account ,;
                        AccName  WITH Customer.BtName
              ENDIF
              REPLACE  Tot_Cart WITH Tot_Cart + lnNoCrt        ,;
                       Tot_Pcs  WITH Tot_Pcs  + OrdLine.TotPik ,;
                       Tot_Wght WITH Tot_Wght + (OrdLine.TotPik * Style.nStyWeight);
                       Pick_Qty WITH  Pick_Qty + OrdLine.TotPik;
                       Order_Qty WITH Order_Qty + OrdLine.TotQTY

              *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
              IF llExtSizSc .AND. lcRpBrkTyp = 'S'
                REPLACE  Tot_Cart  WITH lnNoCrt
              ENDIF
              *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
              
                       
            ELSE
              IF &lcRejCrit
                =lfUpRej()
              ELSE
                =lfPakbySz()
                =lfUpdHdr()
              ENDIF
            ENDIF
          ELSE
            *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
            *  IF &lcRejCrit
			IF &lcRejCrit OR (Style.QTY_CTN < SCALE.Pptot AND llRpMltSKU AND llRpUsePre) 
			*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
              =lfUpRej()
            ELSE
              =lfPakbySz()
              =lfUpdHdr()
            ENDIF
          ENDIF
        ELSE
          =lfUpRej()
        ENDIF
      ELSE
        IF &lcRejCrit
          =lfUpRej()
        ELSE
          =lfPakbySz()
          =lfUpdHdr()
        ENDIF
      ENDIF
    ELSE
      IF !SEEK('P'+Style.Scale+Style.PrePak,'Scale')
        IF SEEK('S'+Style.Scale,'Scale')
          IF &lcRejCrit
            =lfUpRej()
          ELSE
            =lfPakbySz()
            =lfUpdHdr()
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  *WAIT CLEAR
  GO TOP IN (lcPakHdr)
  GO TOP IN (lcPakLin)
  SELECT PikTkt
  SET RELATION OFF INTO OrdLine
  SET RELATION OFF INTO Customer
  RETURN .T.
ENDIF
RETURN .F.

**********************************************************************************************************************************************************

FUNCTION moptionspad
LOCAL lcHostFormName

lcHostFormName = '[' + ThisFormSet.cHostFormName + ']'

DEFINE PAD _Option OF (ThisFormSet.cHostFormName) PROMPT 'Options' KEY ALT+P , ' '
ON PAD _Option OF (ThisFormSet.cHostFormName) ACTIVATE POPUP _OPTIONPOP 
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT "Print Labels Setup"  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. ;
  _screen.ActiveForm.Parent.ActiveMode <> "A"
ON SELECTION POPUP _OPTIONPOP _screen.ActiveForm.mLblSetup()
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP

**********************************************************************************************************************************************************

FUNCTION mrecordchanged
lcPakHdr   = poformclass.lcPakHdr
lcPakLin   = poformclass.lcPakLin
lcRejPkTmp = lcRejPkTmp

WITH This.Ariapageframe1.AriaPage1.grdRejected  
  .RecordSource = lcRejPkTmp
  .column1.ControlSource  = lcRejPkTmp + '.PikTkt' 
  .column2.ControlSource  = lcRejPkTmp + '.PikDate' 
  .column3.ControlSource  = lcRejPkTmp + '.Order' 
  .column4.ControlSource  = lcRejPkTmp + '.Account' 
  .column5.ControlSource  = lcRejPkTmp + '.Store' 
  .column6.ControlSource  = lcRejPkTmp + '.AccName' 
  .column7.ControlSource  = lcRejPkTmp + '.cReason' 
  .SetAll('READONLY',.T.,'Column')
ENDWITH
WITH This.Ariapageframe1.AriaPage2.grdPckLst
  .RecordSource = lcPakHdr
  .column1.ControlSource  = lcPakHdr+ '.Pack_No' 
  .column2.ControlSource  = lcPakHdr+ '.Order' 
  .column3.ControlSource  = lcPakHdr+ '.Account' 
  .column4.ControlSource  = lcPakHdr+ '.Store' 
  .column5.ControlSource  = lcPakHdr+ '.AccName' 
  .column6.ControlSource  = lcPakHdr+ '.Tot_Cart' 
  .column7.ControlSource  = lcPakHdr+ '.Tot_Pcs' 
  .column8.ControlSource  = lcPakHdr+ '.Pick_Qty' 
  .column9.ControlSource  = lcPakHdr+ '.Order_Qty' 
  .column10.ControlSource = lcPakHdr+ '.Tot_wght' 
  .SetAll('READONLY',.T.,'Column') 
ENDWITH
WITH This.Ariapageframe1.AriaPage3.grdPckDetail
  .RecordSource = lcPakLin
  .column1.ControlSource  = lcPakLin+ '.From_Crt' 
  .column30.ControlSource  = lcPakLin+ '.To_Crt' 
  .column2.ControlSource  = lcPakLin+ '.Style' 
  .Column2.header1.Caption = This.ariapageframe1.ariapage3.keyItem.lcimjrheader 
  .column3.ControlSource  = lcPakLin+ '.Qty1' 
  .column4.ControlSource  = lcPakLin+ '.Qty2' 
  .column5.ControlSource  = lcPakLin+ '.Qty3' 
  .column6.ControlSource  = lcPakLin+ '.Qty4' 
  .column7.ControlSource  = lcPakLin+ '.Qty5' 
  .column8.ControlSource  = lcPakLin+ '.Qty6' 
  .column9.ControlSource  = lcPakLin+ '.Qty7' 
  .column10.ControlSource  = lcPakLin+ '.Qty8' 
  .column11.ControlSource  = lcPakLin+ '.TotQty' 
  .column12.ControlSource  = lcPakLin+ '.OrgOrd1' 
  .column13.ControlSource  = lcPakLin+ '.OrgOrd2' 
  .column14.ControlSource  = lcPakLin+ '.OrgOrd3' 
  .column15.ControlSource  = lcPakLin+ '.OrgOrd4' 
  .column16.ControlSource  = lcPakLin+ '.OrgOrd5' 
  .column17.ControlSource  = lcPakLin+ '.OrgOrd6' 
  .column18.ControlSource  = lcPakLin+ '.OrgOrd7' 
  .column19.ControlSource  = lcPakLin+ '.OrgOrd8' 
  .column20.ControlSource  = lcPakLin+ '.TotOrgOrd' 
  .column21.ControlSource  = lcPakLin+ '.Pik1' 
  .column22.ControlSource  = lcPakLin+ '.Pik2' 
  .column23.ControlSource  = lcPakLin+ '.Pik3' 
  .column24.ControlSource  = lcPakLin+ '.Pik4' 
  .column25.ControlSource  = lcPakLin+ '.Pik5' 
  .column26.ControlSource  = lcPakLin+ '.Pik6' 
  .column27.ControlSource  = lcPakLin+ '.Pik7' 
  .column28.ControlSource  = lcPakLin+ '.Pik8' 
  .column29.ControlSource  = lcPakLin+ '.TotPik'
  .SetAll('READONLY',.T.,'Column')
ENDWITH

**********************************************************************************************************************************************************

FUNCTION opentables
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'OrdHdr',oAriaEnvironment.DataDir+'OrdHdr','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'OrdLine',oAriaEnvironment.DataDir+'OrdLinSt','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Style',oAriaEnvironment.DataDir+'Style','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'StyDye',oAriaEnvironment.DataDir+'StyDye','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Scale',oAriaEnvironment.DataDir+'Scale','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Customer',oAriaEnvironment.DataDir+'Customer','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'PikTkt',oAriaEnvironment.DataDir+'OrdPik','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Pack_Hdr',oAriaEnvironment.DataDir+'Pack_Hdr','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Pack_Lin',oAriaEnvironment.DataDir+'Pack_Lin','SH')
*--Open ASN_SHIP & WAREHOUS files and create temp ASN_SHIP file [Begin]

* MAH
*WLD
*=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'PACKINFO','PACKINFO','SH')
=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'EDIPKINF','EDIPKINF','SH')
* MAHXXX
*-- USE d:\testing\01\EDIPKINF IN 0 SHARED 
*-- LOCAL lnSelected
*-- lnSelected = SELECT()
*-- SELECT EDIPKINF 
*-- SET ORDER TO EDIPKINF 
*-- SELECT EDIPKINF 
*-- SELECT(lnSelected)
* MAHXXX

* MAH

=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Asn_Ship','Asn_Ship','SH')
IF ('AS' $ oAriaEnvironment.CompanySetupModules )
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'EDIACPRT' , oAriaEnvironment.DataDir + 'ACCFACT' , 'SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'EDIPH' , oAriaEnvironment.DataDir + 'PARTNER' , 'SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'WareHous','WareHous','SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Spck_Lin','Spcklins','SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Spck_Hdr','SPCK_HDR','SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'StyleUpc','StyleUpc','SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir +'CUSTDEPT',oAriaEnvironment.DataDir+'CUSTDEPT' ,'SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'EDIPD' , oAriaEnvironment.DataDir + 'PARTTRANS', 'SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir +'BOL_HDR',oAriaEnvironment.DataDir+'BOL_HDR' ,'SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'Bol_Lin',oAriaEnvironment.DataDir+'Bol_Lin','SH')
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir +'EdiSv',oAriaEnvironment.DataDir+'Key' ,'SH')
ENDIF
* HUSSIEN
* IF gfUserPriv('AL','ALAUPC','PRNPACKING')
IF .F.
* HUSSIEN
  =oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.SysPath+'SYCASNLB','ASNlbl','SH')
ENDIF

**********************************************************************************************************************************************************

FUNCTION PackByOdrerPackingInfo

lnSelected = SELECT()

SELECT('piktkt')
SET FILTER TO &lcExpr.

PRIVATE lnFromCarton, lnToCarton
lnFromCarton = 1
lnToCarton = 1


SCAN
    RELEASE laUsedPacks
    DECLARE laUsedPacks[1]

    lnFromCarton = 0
    lnToCarton = 0
    =SEEK('O' + piktkt.order , 'ORDHDR', 'ORDHDR')
    
    SELECT('ordline')
    SET ORDER TO ORDLINST
    SEEK(ordhdr.cordtype + piktkt.order + piktkt.store)
    *SET FILTER TO CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = ordhdr.cordtype + piktkt.order + piktkt.store
	SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = ordhdr.cordtype + piktkt.order + piktkt.store
	  if ordline.style = 'AWIB82101A18-ASST'
	    *set step on
	    *set debug on
	  endif
      IF EMPTY(ordline.Pack_ID)
		  PackByPackingInfoBySize()
		ELSE
		  IF TYPE('laUsedPacks[1]') = 'L'
		    packByOrderPackingInfoByPack() 
		    laUsedPacks[1] = ordline.Pack_ID
		  ELSE
		    IF ASCAN(laUsedPacks, ordline.Pack_ID) = 0
		      packByOrderPackingInfoByPack() 
		      
		      DECLARE laUsedPacks[ALEN(laUsedPacks, 1) + 1]
		      laUsedPacks[ALEN(laUsedPacks, 1)] = ordline.Pack_ID
		    ENDIF
		  ENDIF
		ENDIF
	ENDSCAN 
	
	SELECT('piktkt')
ENDSCAN

SELECT(lnSelected)

**********************************************************************************************************************************************************

FUNCTION PackByPackingInfoBySize


LOCAL lcSelected,lnOrderNumber, lnStyleNumber, lcSize, lnQuantity, lnPackID, lnCartonNumber, LcOrderLine
lcSelected = SELECT()

SELECT('EDIPKINF')
SET ORDER TO EDIPKINF
*SET FILTER TO ORDER+STR(LINENO,6)+STYLE+SIZE+PACK_ID = ordline.order + STR(ordline.LINENO,6)
SEEK(ordline.order + STR(ordline.LINENO,6))

SCAN REST WHILE ORDER+STR(LINENO,6)+STYLE+SIZE+PACK_ID = ordline.order + STR(ordline.LINENO,6)
    RELEASE laQtyArr
    DIMENSION laQtyArr[9]
    STORE 0 TO laQtyArr
    
    lcSize = ALLTRIM(Size)
    
    LOCAL lnSize
    lnSize = VAL(lcSize)
    
    LOCAL lnPikQty
    lnPikQty = EVALUATE('ordline.pik'+lcSize)
    
    LOCAL lnCartonNumber
    lnCartonNumber = FLOOR(lnPikQty / (EDIPKINF.nPackUnits * EDIPKINF.nInnerPack))
    IF lnCartonNumber > 0
      lnFromCarton = lnToCarton + 1
      lnToCarton = lnFromCarton + lnCartonNumber - 1
      laQtyArr[lnSize] = (EDIPKINF.npackunits * EDIPKINF.ninnerpack)
      *lcStyle = SPCK_LIN.style
      lcStyle = OrdLine.style
      packByOrderPackingInfoUpdateCart()
    ENDIF
    
    LOCAL lnRemaining
    lnRemaining = lnPikQty % (EDIPKINF.nPackUnits * EDIPKINF.nInnerPack)
    IF lnRemaining > 0
      lnFromCarton = lnToCarton + 1
      lnToCarton = lnFromCarton
      laQtyArr[lnSize] = lnRemaining
      *lcStyle = SPCK_LIN.style
      lcStyle = OrdLine.style
      packByOrderPackingInfoUpdateCart()
    ENDIF
ENDSCAN

SELECT(lcSelected)


**********************************************************************************************************************************************************

FUNCTION packByOrderPackingInfoByPack


LOCAL lcSelected
lcSelected = SELECT()

SELECT('SPCK_LIN')
SET ORDER TO SPCKLINS
*SET FILTER TO (TYPE + ACCOUNT + STYLE + PACK_ID = 'P' + ordhdr.account + ordline.style + ordline.pack_id) .OR. ;
              (TYPE + ACCOUNT + STYLE + PACK_ID = 'P*****' + ordline.style + ordline.pack_id)

SEEK('P' + ordhdr.account + ordline.style + ordline.pack_id)
IF EOF()
  SEEK('P*****' + ordline.style + ordline.pack_id)
ENDIF

SELECT('EDIPKINF')

** SAPCE(1) can be modfied to SPACE(2)
*SET FILTER TO ORDER+STR(LINENO,6)+STYLE+SIZE+PACK_ID = ordline.order + STR(ordline.LINENO,6) + SPACE(19) + SPACE(2) + ordline.pack_id
SEEK(ordline.order + STR(ordline.LINENO,6) + SPACE(19) + SPACE(2) + ordline.pack_id)

IF !EOF('SPCK_LIN') .AND. !EOF('EDIPKINF')
  LOCAL lnPackNo, lnSPCKLINTotQty
  lnPackNo = FLOOR(ordline.TotPik / SPCK_LIN.TotQty)
  lnSPCKLINTotQty = SPCK_LIN.TotQty

  RELEASE laQtyArr
  DIMENSION laQtyArr[9]

  LOCAL lnCartonNumber
  *TEST lnCartonNumber = FLOOR(lnPackNo / EDIPKINF.nPackUnits)
  lnCartonNumber = lnPackNO
  
  IF lnCartonNumber > 0
    lnFromCarton = lnToCarton + 1
    lnToCarton = lnFromCarton + lnCartonNumber - 1
    
    SELECT('SPCK_LIN')
    SET ORDER TO SPCK_LIN
    *SET FILTER TO (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P' + ordhdr.account + ordline.pack_id) .OR. ;
                  (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P*****' + ordline.pack_id)
    =SEEK('P' + ordhdr.account + ordline.pack_id) .OR. SEEK('P*****' + ordline.pack_id)
    
    SCAN REST WHILE (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P' + ordhdr.account + ordline.pack_id) .OR. ;
                    (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P*****' + ordline.pack_id)
      FOR lnSize = 1 TO 8
        LOCAL lcSize
        lcSize = ALLTRIM(STR(lnSize))
        laQtyArr[lnSize] = SPCK_LIN.Qty&lcSize.
      ENDFOR
      *lcStyle = SPCK_LIN.style
      lcStyle = OrdLine.style
      packByOrderPackingInfoUpdateCart()
    ENDSCAN
    
  ENDIF
  
  LOCAL lnRemaining
  *TEST lnRemaining = lnPackNo % EDIPKINF.npackunits
  lnRemaining = ordline.TotPik % lnSPCKLINTotQty
  IF lnRemaining > 0
    lnFromCarton = lnToCarton + 1
    lnToCarton = lnFromCarton
    
    SELECT('SPCK_LIN')
    SET ORDER TO SPCK_LIN
    *SET FILTER TO (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P' + ordhdr.account + ordline.pack_id) .OR. ;
                  (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P*****' + ordline.pack_id)
    = SEEK('P' + ordhdr.account + ordline.pack_id) .OR. SEEK('P*****' + ordline.pack_id)
    
    SCAN REST WHILE (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P' + ordhdr.account + ordline.pack_id) .OR. ;
                    (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P*****' + ordline.pack_id)
      FOR lnSize = 1 TO 8
        LOCAL lcSize
        lcSize = ALLTRIM(STR(lnSize))
        laQtyArr[lnSize] = SPCK_LIN.Qty&lcSize.
      ENDFOR
   
      *lcStyle = SPCK_LIN.style
      lcStyle = OrdLine.style
      packByOrderPackingInfoUpdateCart()
    ENDSCAN
  ENDIF
ENDIF

SELECT(lcSelected)

**********************************************************************************************************************************************************

FUNCTION packByOrderPackingInfoUpdateCart

loAddUserInfo = CREATEOBJECT('AddUserInfo')

LOCAL lcSelected
lcSelected = SELECT()

SELECT(poformclass.lcPakHdr)
SET ORDER TO ORDERPCK 
= SEEK(ordline.order + ordline.store, poformclass.lcPakHdr, 'ORDERPCK')
SET ORDER TO (poformclass.lcPakHdr)
LOCAL llNew
llNew = .F.

IF EOF(poformclass.lcPakHdr)
  APPEND BLANK 
  REPLACE pack_no WITH ordline.piktkt, ;
         order WITH ordline.order, account WITH ordhdr.account , ;
         Store WITH ordline.store, Tot_cart WITH lnToCarton - lnFromCarton  + 1, ;
         tot_pcs WITH (laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8]) * ordline.nPackNo, ;
         shipvia WITH ordhdr.shipvia, cwarecode WITH ordline.cwarecode , piktkt WITH ordline.piktkt
  *loAddUserInfo.Do(poformclass.lcPakHdr,.null.)
  llNew = .T.
ELSE
  REPLACE Tot_cart WITH Tot_cart + lnToCarton - lnFromCarton  + 1, ;
          tot_pcs WITH tot_pcs + (laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + ;
          laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8]) * ordline.nPackNo
ENDIF

SELECT(poformclass.lcPakLin)
GOTO BOTTOM
LOCAL lnLastCartNo, lnLastLineNo
lnLastCartNo = no_cart
lnLastLineNo = line_no

APPEND BLANK 
REPLACE pack_no WITH ordline.piktkt, ;
        line_no WITH IIF(llNew, 1, lnLastLineNo + 1), ;
        from_crt WITH lnFromCarton, to_crt WITH lnToCarton, ;
        style WITH lcStyle, ;
        Qty1 WITH laQtyArr[1], ;
        Qty2 WITH laQtyArr[2], ;
        Qty3 WITH laQtyArr[3], ;
        Qty4 WITH laQtyArr[4], ;
        Qty5 WITH laQtyArr[5], ;
        Qty6 WITH laQtyArr[6], ;
        Qty7 WITH laQtyArr[7], ;
        Qty8 WITH laQtyArr[8], ;
        TotQty WITH laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8], ;
        nordlineno WITH ordline.lineno, no_cart WITH IIF(llNew, lnToCarton - lnFromCarton + 1, lnToCarton - lnFromCarton + 1 + lnLastCartNo), ;
        cZeroQty WITH "N", ;
        PACK_ID WITH ordline.PACK_ID, ;
        nPackNo WITH ordline.nPackNo
        
=SEEK(OrdLine.Style,'Style', 'Style')
SELECT(poformclass.lcPakHdr)
REPLACE Tot_Wght WITH Tot_Wght + Style.nStyWeight * ordline.totPik

*loAddUserInfo.Do('PACK_lin',.null.)

SELECT(lcSelected)

*        Weight WITH Style.nStyWeight * ordline.piktk, ;

*!*	loAddUserInfo = CREATEOBJECT('AddUserInfo')

*!*	LOCAL lcSelected
*!*	lcSelected = SELECT()

*!*	poformclass.lcPakHdr = "pack_hdr"
*!*	poformclass.lcPakLin = "pack_lin"

*!*	SELECT('PACK_Hdr')
*!*	= SEEK(ordline.order + ordline.store, 'PACK_Hdr', 'ORDERPCK')

*!*	LOCAL llNew
*!*	llNew = .F.

*!*	IF EOF('PACK_Hdr')
*!*	  APPEND BLANK 
*!*	  REPLACE pack_no WITH ordline.piktkt, ;
*!*	          order WITH ordline.order, account WITH ordhdr.account , ;
*!*	          Store WITH ordline.store, Tot_cart WITH lnToCarton - lnFromCarton  + 1, ;
*!*	          tot_pcs WITH laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8], ;
*!*	          shipvia WITH ordhdr.shipvia, cwarecode WITH ordline.cwarecode , piktkt WITH ordline.piktkt
*!*	  loAddUserInfo.Do('PACK_Hdr',.null.)
*!*	  llNew = .T.
*!*	ELSE
*!*	  REPLACE Tot_cart WITH PACK_Hdr.Tot_cart + lnToCarton - lnFromCarton  + 1, ;
*!*	          tot_pcs WITH PACK_Hdr.tot_pcs + laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + ;
*!*	          laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8]
*!*	ENDIF

*!*	SELECT('PACK_lin')
*!*	GOTO BOTTOM
*!*	LOCAL lnLastCartNo, lnLastLineNo
*!*	lnLastCartNo = no_cart
*!*	lnLastLineNo = line_no

*!*	APPEND BLANK 
*!*	REPLACE pack_no WITH ordline.piktkt, ;
*!*	        line_no WITH IIF(llNew, 1, lnLastLineNo + 1), ;
*!*	        from_crt WITH lnFromCarton, to_crt WITH lnToCarton, ;
*!*	        style WITH lcStyle, ;
*!*	        Qty1 WITH laQtyArr[1], ;
*!*	        Qty2 WITH laQtyArr[2], ;
*!*	        Qty3 WITH laQtyArr[3], ;
*!*	        Qty4 WITH laQtyArr[4], ;
*!*	        Qty5 WITH laQtyArr[5], ;
*!*	        Qty6 WITH laQtyArr[6], ;
*!*	        Qty7 WITH laQtyArr[7], ;
*!*	        Qty8 WITH laQtyArr[8], ;
*!*	        TotQty WITH laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8], ;
*!*	        nordlineno WITH ordline.lineno, no_cart WITH IIF(llNew, lnToCarton - lnFromCarton + 1, lnToCarton - lnFromCarton + 1 + lnLastCartNo)
*!*	loAddUserInfo.Do('PACK_lin',.null.)

*!*	SELECT(lcSelected)