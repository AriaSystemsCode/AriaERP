*:***************************************************************************
*: Program file  : ALAUTP
*: Program desc. : automatic Allocation program
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : Allocation (AL)
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*:***************************************************************************
*N000624,1 Convert Program to work from request Handler[T20080413.0001]
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.DESCRIPTION = "Opening Data Files..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
loEnvironment.ClientID = ClientID
loEnvironment.ConnectionsRefresh()
*T20100512.0026 Hassan 2010 05 23 [END]

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID), ClientID
*T20100512.0026 Hassan 2010 05 23 [END]
SET CLASSLIB TO (lcCurrentProcedure + "SRVClSS\AL\serveral.vcx") ADDITIVE
SET CLASSLIB TO (lcCurrentProcedure + "SRVClSS\AL\serveralautp.vcx") ADDITIVE


oariaenvironment.activeModuleID = 'AL'
PUBLIC gcAct_Appl
gcAct_Appl = "AL"

oariaenvironment.User_ID = oAriaapplication.User_ID

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

OPENTABLES()

lcRejPkTmp = oariaenvironment.CURSORS.GetCursorTempName()
poformclass = CREATEOBJECT('serveralautpbclass')
poformclass.mcreatetempfiles()
CREATE TABLE (oariaenvironment.WorkDir+lcRejPkTmp) ;
  (PikTkt C(6), PikDate D, ORDER C(6), Account C(5), AccName C(30), STORE C(8), CReason C(50))
INDEX ON PikTkt TAG (lcRejPkTmp)

poAlClass = CREATEOBJECT('serverAL')

moptiongrid()
poformclass.msavepack()
oariaenvironment.SaveTables()

*TABLEUPDATE(.T.,.T.,'pack_lin')
*TABLEUPDATE(.T.,.T.,'pack_hdr')

**********************************************************************************************************************************************************

FUNCTION lfaddcrt

*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
PARAMETERS lnCrtNmbr
PRIVATE lnSlct
lnSlct = SELECT()
=SEEK(ORDLINE.STYLE,'STYLE')
SELECT (lcPakCur)
APPEND BLANK
REPLACE PACK_NO WITH ORDLINE.PikTkt ;
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
lnTotPrk = INT(ORDLINE.Totpik/SCALE.PPTot)
*-- in case of less than one carton deal as only one prepack
lnTotPrk = IIF(lnTotPrk = 0 ,1,lnTotPrk)
lnFrcQty = MOD(ORDLINE.Totpik/SCALE.PPTot,1)
lnAdd1   = IIF(lnFrcQty<>0,1,0)
*-- Do not give value to the additional carton when we back the first carton.
lnAdd1 = IIF(lnChange="2" .AND. lnTotPrk=0,0,lnAdd1)
FOR lnCount = 1 TO 8
  lnCont = STR(lnCount,1)
  IF !EMPTY(SCALE.Pp&lnCont)
    IF !EMPTY(SCALE.Pp&lnCont) .AND. (lnTotPrk <= (ORDLINE.pik&lnCont/SCALE.Pp&lnCont) AND ;
        (ORDLINE.pik&lnCont/SCALE.Pp&lnCont) <= (lnTotPrk + lnAdd1) )
      IF lnFrcQty <> 0
        *-- Check if the total prepack = 0
        IF lnTotPrk <> 0
          llCheck2 = .T.
        ELSE
          llCheck2 = .F.
        ENDIF
      ENDIF
      IF lnChange = '1'
        IF (lnTotPrk <> (ORDLINE.pik&lnCont/SCALE.Pp&lnCont) AND (ORDLINE.pik&lnCont/SCALE.Pp&lnCont) > (lnTotPrk + lnAdd1))
          llCheck = .T.
        ENDIF
      ELSE
        IF lnTotPrk = (ORDLINE.pik&lnCont/SCALE.Pp&lnCont)
          *--- If true Do not change prepack
          llCheck = .T.
        ELSE
          IF lnTotPrk < ((ORDLINE.pik&lnCont/SCALE.Pp&lnCont) +1)
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
        IF !EMPTY(SCALE.Pp&lnCont)
          llCheck = .F.
          EXIT
        ENDIF
      ENDIF
    ENDIF
  ELSE
    IF !(ORDLINE.pik&lnCont = 0 .AND. SCALE.Pp&lnCont = 0)
      IF lnChange = '1'
        llCheck =  ORDLINE.pik&lnCont<>0 .OR. SCALE.Pp&lnCont = 0
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
  lnCompCrt  = INT((ORDLINE.Totpik * STYLE.nstyweight) / lnRealWght)
  lnFrcCrt   = MOD((ORDLINE.Totpik * STYLE.nstyweight) / lnRealWght,1)
ELSE
  lnCompCrt  = INT(ORDLINE.Totpik / (lnCrtNo*SCALE.PPTot) )
  lnFrcCrt   = MOD(ORDLINE.Totpik / (lnCrtNo*SCALE.PPTot) ,1)
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
  lnLastCrt = ORDLINE.Totpik
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
laScalPr[9] = SCALE.PPTot
*--- There is freactoin

*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
PRIVATE lcI,lnDivRet
lnDivRet = ORDLINE.Totpik/SCALE.PPTot
FOR lnCount = 1 TO 8
  lcI = STR(lnCount,1)
  IF SCALE.Pp&lcI>0 AND ORDLINE.pik&lcI/SCALE.Pp&lcI <> lnDivRet
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
    lnLastCrt = ORDLINE.Totpik - laScalPr[9]*lnCrtNo*lnCompCrt
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
IF llRpMltSku AND llRpUsePre
  IF EMPTY(STYLE.PrePak)
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

=SEEK('S'+STYLE.SCALE,'Scale')
FOR lnIndex = 1 TO SCALE.CNT
  lcIndex = STR(lnIndex,1)
  lnTempWgQt = ORDLINE.pik&lcIndex * IIF(lcRpBrkTyp='W',STYLE.nstyweight,1)
  IF lnTempWgQt>0
    DO WHILE lnTempWgQt > 0
      lnWgInCrt = lnWgInCrt + IIF(STYLE.nstyweight>0,STYLE.nstyweight,1)
      lnQtyInCrt = lnQtyInCrt + 1
      IF &lcCrit .OR. IIF(llRpMltSku,.F.,lnQtyInCrt > ORDLINE.pik&lcIndex)
        IF laQtyArr[9] > 0
          =lfUpdCart(lnFrmCrt,lnToCrt,@laQtyArr)
          IF EOF("Style")
            =SEEK(ORDLINE.STYLE,'Style')
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
        lnTempWgQt  = MAX(lnTempWgQt - IIF(lcRpBrkTyp='W',STYLE.nstyweight,1) ,0)
      ENDIF
    ENDDO
    IF !llRpMltSku
      IF laQtyArr[9] > 0
        =lfUpdCart(lnFrmCrt,lnToCrt,@laQtyArr)
        IF EOF("Style")
          =SEEK(ORDLINE.STYLE,'Style')
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
    =SEEK(ORDLINE.STYLE,'Style')
  ENDIF
ENDIF
lnStorFrm = lnFrmCrt

**********************************************************************************************************************************************************

FUNCTION lfpredist
*--llChngPrpk : variable to check if prepack changed
llChngPrpk = .F.
lcOldPr = SCALE.PrePak
lcPrePak= SCALE.PrePak
llChngPrRk =lfcheckper('1')
IF llChngPrRk
  lnCurAlias  =SELECT(0)
  SELECT SCALE
  =SEEK('P'+STYLE.SCALE)
  *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
  *SCAN FOR TYPE+Scale = 'P'+Style.Scale
  SCAN REST WHILE TYPE+SCALE = 'P'+STYLE.SCALE
    *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
    llChngPrpk=lfcheckper('2')
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

FUNCTION lfUpdCart
LPARAMETER lnFrmCrt,lnToCrt, laQtyArr

*-- Increment lnPicQty to calculate carton QTY
lnPicQty   = lnPicQty + laQtyArr[9]
lnOldweght = lnOldweght + laQtyArr[9]*STYLE.nstyweight

SELECT (poformclass.lcPakLin)
APPEND BLANK
REPLACE PACK_NO    WITH ORDLINE.PikTkt  ,;
  No_Cart    WITH lnNoCrt         ,;
  From_Crt   WITH lnFrmCrt        ,;
  To_Crt     WITH lnToCrt         ,;
  STYLE      WITH ORDLINE.STYLE   ,;
  nOrdLineNo WITH ORDLINE.LINENO  ,;
  SCALE      WITH ORDLINE.SCALE   ,;
  PrePak     WITH ORDLINE.PrePak  ,;
  OrgOrd1    WITH ORDLINE.Qty1    ,;
  OrgOrd2    WITH ORDLINE.Qty2    ,;
  OrgOrd3    WITH ORDLINE.Qty3    ,;
  OrgOrd4    WITH ORDLINE.Qty4    ,;
  OrgOrd5    WITH ORDLINE.Qty5    ,;
  OrgOrd6    WITH ORDLINE.Qty6    ,;
  OrgOrd7    WITH ORDLINE.Qty7    ,;
  OrgOrd8    WITH ORDLINE.Qty8    ,;
  TotOrgOrd  WITH ORDLINE.TotQty  ,;
  Ord1       WITH ORDLINE.Qty1    ,;
  Ord2       WITH ORDLINE.Qty2    ,;
  Ord3       WITH ORDLINE.Qty3    ,;
  Ord4       WITH ORDLINE.Qty4    ,;
  Ord5       WITH ORDLINE.Qty5    ,;
  Ord6       WITH ORDLINE.Qty6    ,;
  Ord7       WITH ORDLINE.Qty7    ,;
  Ord8       WITH ORDLINE.Qty8    ,;
  TotOrd     WITH ORDLINE.TotQty  ,;
  Pik1       WITH ORDLINE.Pik1    ,;
  Pik2       WITH ORDLINE.Pik2    ,;
  Pik3       WITH ORDLINE.Pik3    ,;
  Pik4       WITH ORDLINE.Pik4    ,;
  Pik5       WITH ORDLINE.Pik5    ,;
  Pik6       WITH ORDLINE.Pik6    ,;
  Pik7       WITH ORDLINE.Pik7    ,;
  Pik8       WITH ORDLINE.Pik8    ,;
  Totpik     WITH ORDLINE.Totpik  ,;
  cZeroQty   WITH 'N'

=lfsupdqty(@laQtyArr)
*-- End lfUpdTmp

**********************************************************************************************************************************************************

FUNCTION lfupdhdr
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
IF llRejForPr
  llRejForPr = .F.
  RETURN
ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

IF !SEEK(ORDLINE.PikTkt,poformclass.lcPakHdr)
  INSERT INTO (poformclass.lcPakHdr) (PACK_NO,ORDER,STORE,Account,AccName) VALUES ;
    (ORDLINE.PikTkt,ORDLINE.ORDER,ORDLINE.STORE,ORDLINE.Account,Customer.btName)
ENDIF
REPLACE  Tot_Cart WITH lnToCrt        ,;
  Tot_Pcs  WITH Tot_Pcs  + ORDLINE.Totpik ,;
  Tot_Wght WITH Tot_Wght + (ORDLINE.Totpik * STYLE.nstyweight);
  Pick_Qty WITH Pick_Qty + ORDLINE.Totpik;
  Order_Qty WITH Order_Qty + ORDLINE.TotQty IN (poformclass.lcPakHdr)

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
    lnQty&lnCotr = IIF(ORDLINE.pik&lnCotr <> 0 ,laScalPr[lnCountr]*lnCrtNo,0)
    lnTotQty = lnTotQty +lnQty&lnCotr
  ELSE
    IF llCheck2
      lnQty&lnCotr = IIF(ORDLINE.pik&lnCotr <> 0 ,ORDLINE.pik&lnCotr - laScalPr[lnCountr]*lnCrtNo*lnCompCrt,0)
    ELSE
      lnQty&lnCotr = IIF(ORDLINE.pik&lnCotr <> 0 .AND. laScalPr[9] <>0,laScalPr[lnCountr]*(lnLastCrt/laScalPr[9]),0)
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

FUNCTION lfUpdTmp
LPARAMETER lnNoCrt , lnFrmCrt , lnToCrt ,lnFlag

SELECT (poformclass.lcPakLin)
APPEND BLANK
REPLACE PACK_NO    WITH ORDLINE.PikTkt  ,;
  No_Cart    WITH lnNoCrt         ,;
  From_Crt   WITH lnFrmCrt        ,;
  To_Crt     WITH lnToCrt         ,;
  STYLE      WITH ORDLINE.STYLE   ,;
  nOrdLineNo WITH ORDLINE.LINENO  ,;
  SCALE      WITH ORDLINE.SCALE   ,;
  PrePak     WITH ORDLINE.PrePak  ,;
  OrgOrd1    WITH ORDLINE.Qty1    ,;
  OrgOrd2    WITH ORDLINE.Qty2    ,;
  OrgOrd3    WITH ORDLINE.Qty3    ,;
  OrgOrd4    WITH ORDLINE.Qty4    ,;
  OrgOrd5    WITH ORDLINE.Qty5    ,;
  OrgOrd6    WITH ORDLINE.Qty6    ,;
  OrgOrd7    WITH ORDLINE.Qty7    ,;
  OrgOrd8    WITH ORDLINE.Qty8    ,;
  TotOrgOrd  WITH ORDLINE.TotQty  ,;
  Ord1       WITH ORDLINE.Qty1    ,;
  Ord2       WITH ORDLINE.Qty2    ,;
  Ord3       WITH ORDLINE.Qty3    ,;
  Ord4       WITH ORDLINE.Qty4    ,;
  Ord5       WITH ORDLINE.Qty5    ,;
  Ord6       WITH ORDLINE.Qty6    ,;
  Ord7       WITH ORDLINE.Qty7    ,;
  Ord8       WITH ORDLINE.Qty8    ,;
  TotOrd     WITH ORDLINE.TotQty  ,;
  Pik1       WITH ORDLINE.Pik1    ,;
  Pik2       WITH ORDLINE.Pik2    ,;
  Pik3       WITH ORDLINE.Pik3    ,;
  Pik4       WITH ORDLINE.Pik4    ,;
  Pik5       WITH ORDLINE.Pik5    ,;
  Pik6       WITH ORDLINE.Pik6    ,;
  Pik7       WITH ORDLINE.Pik7    ,;
  Pik8       WITH ORDLINE.Pik8    ,;
  Totpik     WITH ORDLINE.Totpik  ,;
  cZeroQty   WITH 'N'
=lfupdqty(lnFlag)

**********************************************************************************************************************************************************

FUNCTION lfUpdTmpEx
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
PRIVATE lnSlct
lnSlct = SELECT()

lcPakLin   = poformclass.lcPakLin

lnNoCrt = 0
m.No_Cart = 1

SELECT ORDLINE
SCATTER MEMVAR

*- initiate varibles that will added to lcPakLin temp file
m.PACK_NO    = ORDLINE.PikTkt
m.nOrdLineNo = ORDLINE.LINENO
STORE ORDLINE.Qty1   TO m.OrgOrd1,m.Ord1
STORE ORDLINE.Qty2   TO m.OrgOrd2,m.Ord2
STORE ORDLINE.Qty3   TO m.OrgOrd3,m.Ord3
STORE ORDLINE.Qty4   TO m.OrgOrd4,m.Ord4
STORE ORDLINE.Qty5   TO m.OrgOrd5,m.Ord5
STORE ORDLINE.Qty6   TO m.OrgOrd6,m.Ord6
STORE ORDLINE.Qty7   TO m.OrgOrd7,m.Ord7
STORE ORDLINE.Qty8   TO m.OrgOrd8,m.Ord8
STORE ORDLINE.TotQty TO m.TotOrgOrd,m.TotOrd
m.cZeroQty   = 'N'

STORE ORDLINE.Totpik TO m.OrgTotPk
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty

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
IF !SEEK(ORDLINE.PikTkt,lcPakLin)
  STORE 1 TO m.From_Crt,m.To_Crt
  =lfaddcrt(1)
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
    IF SEEK( ORDLINE.PikTkt+SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnClrLen-1) , lcPakCur )
      STORE EVALUATE(lcPakCur+'.CARTON') TO m.From_Crt, m.To_Crt
      IF !SEEK(ORDLINE.PikTkt+ORDLINE.STYLE,lcPakCur)
        =lfaddcrt(m.To_Crt)
      ENDIF
    ELSE
      *- if this is a new style added then get the max carton # used for this pack and the add 1 to it , use this number for this added style
      SELECT (lcPakCur)
      =SEEK(ORDLINE.PikTkt,lcPakCur)
      CALCULATE MAX(CARTON+1) REST WHILE PACK_NO+STYLE+STR(CARTON,4) = ORDLINE.PikTkt TO lnNewNum
      STORE lnNewNum TO m.From_Crt, m.To_Crt
      =lfaddcrt(m.To_Crt)
    ENDIF
  ENDIF
ENDIF

*- reset the order to the ascending
SELECT &lcPakLin
SET ORDER TO &lcSvOrd ASCENDING
LOCATE

*-- case of lcRpBrkTyp is 'W','M' or 'S'
=SEEK(ORDLINE.STYLE,'STYLE')
lnStyW = IIF(lcRpBrkTyp = 'W' , STYLE.nstyweight , 1 )

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
          M.Qty1 = &lcPakLin..Qty1 .AND. ;
          M.Qty2 = &lcPakLin..Qty2 .AND. ;
          M.Qty3 = &lcPakLin..Qty3 .AND. ;
          M.Qty4 = &lcPakLin..Qty4 .AND. ;
          M.Qty5 = &lcPakLin..Qty5 .AND. ;
          M.Qty6 = &lcPakLin..Qty6 .AND. ;
          M.Qty7 = &lcPakLin..Qty7 .AND. ;
          M.Qty8 = &lcPakLin..Qty8

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
      =lfaddcrt(m.To_Crt)

      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty

    ENDIF

  ENDIF

ENDDO

*- Get number of cartons for the case of break type 'S'
IF lcRpBrkTyp = 'S'
  SELECT (lcPakCur)
  =SEEK(ORDLINE.PikTkt,lcPakCur)
  CALCULATE MAX(CARTON) REST WHILE PACK_NO+STYLE+STR(CARTON,4) = ORDLINE.PikTkt TO lnNoCrt
ENDIF

SELECT (lnSlct)
*-- end of lfUpdTmpEx.
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

**********************************************************************************************************************************************************

FUNCTION lfUpRej
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

lcReason = IIF(lcRpBrkTyp='W',IIF(STYLE.nstyweight > lnRpWght,;
  "Style Weight is greater than break weight.",IIF(STYLE.nstyweight > 0 ,"Failed Transfere To Perpack Qty",;
  "There is no style weight.")),;
  IIF(lcRpBrkTyp='S' AND  STYLE.QTY_CTN = 0 ,"Style Qty per carton is 0.",IIF(lcRpBrkTyp = 'P','No packs assigned to this pick ticket','Failed Transfere To Perpack Qty')))
*B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End]
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]

lcDelPik = ORDLINE.PikTkt
IF SEEK(lcDelPik,poformclass.lcPakHdr)
  SELECT (poformclass.lcPakHdr)
  DELETE REST WHILE PACK_NO = lcDelPik
ENDIF
IF SEEK(lcDelPik,poformclass.lcPakLin)
  SELECT (poformclass.lcPakLin)
  DELETE REST WHILE PACK_NO = lcDelPik
ENDIF
IF !SEEK(lcDelPik,lcRejPkTmp)
  INSERT INTO (lcRejPkTmp) (PikTkt,PikDate,ORDER,Account,AccName,CReason,STORE) VALUES ;
    (ORDLINE.PikTkt,ORDLINE.PikDate,ORDLINE.ORDER,ORDLINE.Account, Customer.btName,lcReason,ORDLINE.STORE)
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
  lcTitle = "for Packing lst " + ORDLINE.PikTkt
  lcShipVia = OrdHdr.ShipVia
  lcDistCtr = lcStore
  *-- Get the value of Distrepution center From Customer file
  IF SEEK('S'+lcAccount+lcStore,'Customer')
    lcDistCtr = IIF(EMPTY(Customer.Dist_Ctr),Customer.STORE,Customer.Dist_Ctr)
    lcShipVia = IIF(ALLTRIM(lcShipVia)='*',Customer.ShipVia,lcShipVia)
  ENDIF
  =SEEK('A' + lcAccount , 'EDIACPRT') .AND. SEEK(EDIACPRT.cPartCode , 'EDIPH')
  lcTitle = "for Packing lst " + &lcPakHdr..PACK_NO
  lcBol = poAlClass.lfGetBOL(lcTitle,lcAccount,lcDistCtr,OrdHdr.cWareCode,lcShipVia,IIF(EDIPH.cCrtnType='S','Y','N'),'laFields',IIF(lcRpBrkTyp = 'I',.T.,!llRpUseExs),.F.,'S')

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
  ariapageframe1.ariapage1.grdRejected.RECORDSOURCE = ''
  ariapageframe1.ariapage2.grdPckLst.RECORDSOURCE   = ''
  ariapageframe1.ariapage3.grdPckDetail.RECORDSOURCE = ''
  poformclass.mcreatetempfiles()
  CREATE TABLE (oariaenvironment.WorkDir+lcRejPkTmp) ;
    (PikTkt C(6), PikDate D, ORDER C(6), Account C(5), AccName C(30), STORE C(8), CReason C(50))
  INDEX ON PikTkt TAG (lcRejPkTmp)
ENDIF
WITH ariapageframe1.ariapage2
  STORE .F. TO .keyStore.ENABLED, .keyPikTKt.ENABLED, .keyOrder.ENABLED, .keyBOL.ENABLED, .keyAccount.ENABLED,;
    .txtAccName.ENABLED, .txtStoreName.ENABLED
ENDWITH
WITH ariapageframe1.ariapage3
  STORE .F. TO .keyItem.ENABLED, .txtItemDesc.ENABLED, .txttotQty.ENABLED
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
loGetItemMask.DO(@laItemSeg)
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
lcOldPort = THISFORMSET.lcSndPort
llOldPrn  = THISFORMSET.llScrPrnLb
llCancel = .F.
DO FORM (oariaenvironment.ScreenHome+ 'ALOUTPRT.SCX') WITH THISFORMSET TO llCancel
IF llCancel
  THISFORMSET.lcSndPort  = lcOldPort
  THISFORMSET.llScrPrnLb = llOldPrn
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
llEdiSys  =  ('AS' $ oariaenvironment.CompanySetupModules)
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

*oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(loXMLPackingOptionPointer), .F.)
oariaenvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.F.)

lcExpr = lcrpExp

IF lcRpBrkTyp = 'I'
  poformclass.CartonId = lcRPCart
  poformclass.AssignBol = llRpBolAsi
  poformclass.CrtnPerPallete = lnRpTtCrtP
  lcRpBrkTyp = lcRpBrkTyp
  lnRpWght = lnRpWght
  *--if the sort is by order line no change the order in the ordline table to be on ordline not in style
  IF lcRpSortBy = 'O'
    SET ORDER TO TAG ORDLINE IN ORDLINE
  ENDIF
  *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
  STORE 0 TO lnClrLen ,lnClrPos
  mgtclrdt ()
  *--check for Extended Size Scale
  llExtSizSc = oariaenvironment.setups.getSetting('M_USEEXSSC',oariaenvironment.ActiveCompanyID)
  *- Temp cursor used to store carton data while filling
  lcPakCur = '_'+SUBSTR(lcPakLin,2)
  IF USED(lcPakCur)
    USE IN (lcPakCur)
  ENDIF

  SELECT PikTkt
  SET ORDER TO 0
  SET RELATION TO  IIF(EMPTY(STORE),'M'+Account,"S"+Account+STORE) INTO Customer ADDITIVE
  SET RELATION TO 'O' + ORDER INTO ORDLINE ADDITIVE
  SET SKIP TO ORDLINE

  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start]
  *lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
  IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0','.F.'))
  lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
    IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0',IIF(lcRpBrkTyp = 'P','EMPTY(ordline.pack_id)','.F.')))
  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End]


  *MMT
  COUNT FOR &lcExpr AND PikTkt # '******' AND PikTkt = ORDLINE.PikTkt AND STATUS $ 'OP' TO lnDataCnt
  lnCntData = 0
  SELECT PikTkt
  LOCATE
  *MMT

  *-- Don't allow creating packing list on completed or Hold pick ticket
  SCAN FOR &lcExpr AND PikTkt # '******' AND PikTkt = ORDLINE.PikTkt AND STATUS $ 'OP'

    *MMT
    lnCntData = lnCntData + 1
    lnPerCent = lnCntData /lnDataCnt
    IF MOD(lnCntData ,CEILING(lnDataCnt/ 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
      loProgress.DESCRIPTION = "Preparing Data For Piktkt:"+ PikTkt
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      loEnvironment.ClientID = ClientID
      loEnvironment.ConnectionsRefresh()
      *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF
    *MMT


    *WAIT "Preparing Data. Picking ticket# " + PikTkt.PikTkt WINDOW NOWAIT
    IF lcPikTkt # ORDLINE.PikTkt
      STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt
      lcPikTkt = ORDLINE.PikTkt
      STORE 0 TO lnStorFrm ,lnOldCrtNo ,lnPicQty ,lnOldweght
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
      *lcLastSty = ORDLINE.STYLE
      lcLastSty = SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnClrLen-1)
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
      lnWgInCrt = 0
      lnQtyInCrt = 0
      *-- Save BOL on Packing Slip level.
      SELECT (lcPakHdr)
      IF !SEEK(ORDLINE.PikTkt)
        APPEND BLANK
        REPLACE PACK_NO  WITH ORDLINE.PikTkt  ,;
          ORDER    WITH ORDLINE.ORDER   ,;
          STORE    WITH ORDLINE.STORE   ,;
          Account  WITH ORDLINE.Account ,;
          AccName  WITH IIF(Customer.TYPE="M",Customer.btName,Customer.StName)

        *B608710,1 MMT 10/08/2008 Fix bug of Not saving shipvia if no BOL [T20080922.0017][Start]
        IF SEEK(ORDLINE.cOrdType+ORDLINE.ORDER ,'ORDHDR','ORDHDR')
          lcShipVia = OrdHdr.ShipVia
          IF ALLTRIM(lcShipVia)='*'
            lcShipVia = Customer.ShipVia
          ENDIF
          REPLACE ShipVia   WITH lcShipVia
        ENDIF
        *B608710,1 MMT 10/08/2008 Fix bug of Not saving shipvia if no BOL [End]

        =IIF(llEdiSys AND llRpBolAsi,massignbol(ORDLINE.cOrdType + ORDLINE.ORDER, ORDLINE.Account, ORDLINE.STORE),.T.)
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
  SET ORDER TO TAG ORDLINE IN ORDLINE
ENDIF
*T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
STORE 0 TO lnClrLen ,lnClrPos
mgtclrdt ()
*--check for Extended Size Scale
llExtSizSc = oariaenvironment.setups.getSetting('M_USEEXSSC',oariaenvironment.ActiveCompanyID)
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
  SET RELATION TO 'O' + ORDER INTO ORDLINE ADDITIVE
  SET SKIP TO ORDLINE

  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start]
  *lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
  IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0','.F.'))
  lcRejCrit = IIF(lcRpBrkTyp='W','Style.nstyweight > lnRpWght .OR. Style.nStyWeight = 0',;
    IIF(lcRpBrkTyp='S','STYLE.QTY_CTN = 0',IIF(lcRpBrkTyp = 'P','EMPTY(ordline.pack_id)','.F.')))
  *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End]


  COUNT FOR &lcExpr AND PikTkt # '******' AND PikTkt = ORDLINE.PikTkt AND STATUS $ 'OP' TO lnDataCnt
  lnCntData = 0
  SELECT PikTkt
  LOCATE




  *-- Don't allow creating packing list on completed or Hold pick ticket
  SCAN FOR &lcExpr AND PikTkt # '******' AND PikTkt = ORDLINE.PikTkt AND STATUS $ 'OP'

    lnCntData = lnCntData + 1
    lnPerCent = lnCntData /lnDataCnt
    IF MOD(lnCntData ,CEILING(lnDataCnt/ 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
      loProgress.DESCRIPTION = "Preparing Data For Piktkt:"+ PikTkt
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      loEnvironment.ClientID = ClientID
      loEnvironment.ConnectionsRefresh()
      *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF

    *WAIT "Preparing Data. Picking ticket# " + PikTkt.PikTkt WINDOW NOWAIT
    IF lcPikTkt # ORDLINE.PikTkt
      STORE 0 TO lnFrmCrt,lnToCrt,lnNoCrt
      lcPikTkt = ORDLINE.PikTkt
      STORE 0 TO lnStorFrm ,lnOldCrtNo ,lnPicQty ,lnOldweght
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
      *lcLastSty = ORDLINE.STYLE
      lcLastSty = SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnClrLen-1)
      *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
      lnWgInCrt = 0
      lnQtyInCrt = 0
      *-- Save BOL on Packing Slip level.
      SELECT (lcPakHdr)
      IF !SEEK(ORDLINE.PikTkt)
        APPEND BLANK
        REPLACE PACK_NO  WITH ORDLINE.PikTkt  ,;
          ORDER    WITH ORDLINE.ORDER   ,;
          STORE    WITH ORDLINE.STORE   ,;
          Account  WITH ORDLINE.Account ,;
          AccName  WITH IIF(Customer.TYPE="M",Customer.btName,Customer.StName)

        *B608710,1 MMT 10/08/2008 Fix bug of Not saving shipvia if no BOL [T20080922.0017][Start]
        IF SEEK(ORDLINE.cOrdType+ORDLINE.ORDER ,'ORDHDR','ORDHDR')
          lcShipVia = OrdHdr.ShipVia
          IF ALLTRIM(lcShipVia)='*'
            lcShipVia = Customer.ShipVia
          ENDIF
          REPLACE ShipVia   WITH lcShipVia
        ENDIF
        *B608710,1 MMT 10/08/2008 Fix bug of Not saving shipvia if no BOL [End]
        =IIF(llEdiSys AND llRpBolAsi,massignbol(ORDLINE.cOrdType + ORDLINE.ORDER, ORDLINE.Account, ORDLINE.STORE),.T.)
      ENDIF
      SELECT PikTkt
    ENDIF
    =SEEK(ORDLINE.STYLE,'Style')
    *-- The case that all the piktkt is shipped in one carton - this is unusual case
    *-- In this case no check will be performed on style.nstyweight or style.qty_ctn fields
    IF llRp1BoxPk
      SELECT ORDLINE
      SCATTER FIELDS Pik1, Pik2, Pik3, Pik4, Pik5, Pik6, Pik7, Pik8, Totpik TO laQtyArr
      IF laQtyArr[9] > 0
        lfUpdCart(1,1,@laQtyArr)
      ENDIF
      REPLACE  Tot_Cart  WITH 1  ,;
        Tot_Pcs   WITH Tot_Pcs   + ORDLINE.Totpik ,;
        Tot_Wght  WITH Tot_Wght  + (ORDLINE.Totpik * STYLE.nstyweight);
        Pick_Qty  WITH Pick_Qty  + ORDLINE.Totpik;
        Order_Qty WITH Order_Qty + ORDLINE.TotQty IN (lcPakHdr)
      LOOP
    ENDIF
    IF lcRpBrkTyp = 'P'
      *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[Start]
      *=SEEK('P'+ordline.account+ordline.pack_id+ordline.style,'SPCK_LIN','SPCK_LIN')
      IF !&lcRejCrit AND SEEK('P'+ORDLINE.Account+ORDLINE.pack_id+ORDLINE.STYLE,'SPCK_LIN','SPCK_LIN')
        *B607955,1 MMT 01/31/2007 Fix bug of error when Select break by pack[End]

        SELECT SPCK_LIN
        SCATTER FIELDS Qty1, Qty2, Qty3, Qty4, Qty5, Qty6, Qty7, Qty8, TotQty TO laQtyArr
        lnStorFrm = IIF(ORDLINE.pack_id = lcLastSty,lnStorFrm ,lnToCrt + 1)
        lnNoCrt = ORDLINE.Totpik/SPCK_LIN.TotQty
        lnToCrt =  lnStorFrm + lnNoCrt - 1
        lcLastSty = ORDLINE.pack_id
        IF laQtyArr[9] > 0
          lfUpdCart(lnStorFrm,lnToCrt,@laQtyArr)
        ENDIF
        REPLACE  Tot_Cart  WITH lnToCrt  ,;
          Tot_Pcs   WITH Tot_Pcs   + laQtyArr[9] ,;
          Tot_Wght  WITH Tot_Wght  +(laQtyArr[9] * STYLE.nstyweight);
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
        IF ORDLINE.PikTkt <> lcDelPik
          =lfpakbysz()
          =lfupdhdr()
        ENDIF
      ENDIF
      LOOP
    ENDIF

    IF SEEK(ORDLINE.STYLE,'Style') .AND. SEEK('P'+STYLE.SCALE+STYLE.PrePak,'Scale')
      SELECT SCALE
      IF SCALE.PPTot <> 0
        lcBrkTypCr = IIF(lcRpBrkTyp='W','Style.nStyWeight <> 0 .AND. Style.nStyWeight <= lnRpWght',;
          IIF(lcRpBrkTyp='S','Style.QTY_CTN    <> 0 ','.T.'))
        IF &lcBrkTypCr .AND. !SEEK(lcPikTkt,lcRejPkTmp)
          lcPreWght = SCALE.PPTot * STYLE.nstyweight
          IF ORDLINE.PikTkt <> lcDelPik AND ;
              IIF(lcRpBrkTyp = 'W',lnRpWght > STYLE.nstyweight AND lnRpWght >= lcPreWght,;
              IIF(lcRpBrkTyp = 'S',STYLE.QTY_CTN >=SCALE.PPTot,lnRpMnlQty >=SCALE.PPTot))
            =lfpredist()
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly
            *! when choosing by style quantit and by preback =yes  ----  [start]
            =SEEK('P'+STYLE.SCALE+STYLE.PrePak,'Scale')
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly ----  [end]
            lcPreWght = SCALE.PPTot * STYLE.nstyweight
            *--- lnRpWght: Carton Wieght.
            *--- lcPreWght: PrePack Wieght.
            *--- lnCrtNo: TotNo of PrePack per carton.
            STORE 0 TO lnCrtNo,lnNoCrt
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly
            *! when choosing by style quantit and by preback =yes  ----  [start]
            *!*	  IF lcPreWght = 0
            IF lcPreWght = 0 AND !lcRpBrkTyp = 'S'
              llChngPrpk = .T.
              lcPreWght = STYLE.nstyweight
            ENDIF
            *!B607986,1  -t20070108.0002 AYM :Automatic packing list not working correctly ----  [end]

            *-- Check if the totpick weight < Break weight
            *-- calculate carton number depend on the totpick
            *-- else calculate depend on the break weight.
            lnHQty = IIF(lcRpBrkTyp = 'S',STYLE.QTY_CTN,lnRpMnlQty)
            IF lcRpBrkTyp = 'W'
              IF ORDLINE.Totpik * STYLE.nstyweight < lnRpWght
                lnCrtNo = INT(ORDLINE.Totpik * STYLE.nstyweight / lcPreWght)
              ELSE
                lnCrtNo = INT(lnRpWght / lcPreWght)
              ENDIF
            ELSE
              IF !EOF('SCALE')
                IF ORDLINE.Totpik < lnHQty
                  lnCrtNo = INT(ORDLINE.Totpik/SCALE.PPTot)
                ELSE
                  lnCrtNo = INT(lnHQty/SCALE.PPTot)
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
              lnNoCrt = CEILING((ORDLINE.Totpik * STYLE.nstyweight) / lnRealWght)
            ELSE
              *--- lnNoCrt : Tot No of Carton = (Total style qty / qty per carton)
              IF lnCrtNo <> 0 AND !EOF('SCALE')
                lnNoCrt = CEILING( ORDLINE.Totpik / (lnCrtNo*SCALE.PPTot))
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
              IF !SEEK(ORDLINE.PikTkt)
                APPEND BLANK
                REPLACE PACK_NO  WITH ORDLINE.PikTkt  ,;
                  ORDER    WITH ORDLINE.ORDER   ,;
                  STORE    WITH ORDLINE.STORE   ,;
                  Account  WITH ORDLINE.Account ,;
                  AccName  WITH Customer.btName
              ENDIF
              REPLACE  Tot_Cart WITH Tot_Cart + lnNoCrt        ,;
                Tot_Pcs  WITH Tot_Pcs  + ORDLINE.Totpik ,;
                Tot_Wght WITH Tot_Wght + (ORDLINE.Totpik * STYLE.nstyweight);
                Pick_Qty WITH  Pick_Qty + ORDLINE.Totpik;
                Order_Qty WITH Order_Qty + ORDLINE.TotQty

              *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
              IF llExtSizSc .AND. lcRpBrkTyp = 'S'
                REPLACE  Tot_Cart  WITH lnNoCrt
              ENDIF
              *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]


            ELSE
              IF &lcRejCrit
                =lfUpRej()
              ELSE
                =lfpakbysz()
                =lfupdhdr()
              ENDIF
            ENDIF
          ELSE
            *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[Start]
            *  IF &lcRejCrit
            IF &lcRejCrit OR (STYLE.QTY_CTN < SCALE.PPTot AND llRpMltSku AND llRpUsePre)
              *T20060929.0004,1 MMT 11/12/2006 Fix bug in case of extended size scale[End]
              =lfUpRej()
            ELSE
              =lfpakbysz()
              =lfupdhdr()
            ENDIF
          ENDIF
        ELSE
          =lfUpRej()
        ENDIF
      ELSE
        IF &lcRejCrit
          =lfUpRej()
        ELSE
          =lfpakbysz()
          =lfupdhdr()
        ENDIF
      ENDIF
    ELSE
      IF !SEEK('P'+STYLE.SCALE+STYLE.PrePak,'Scale')
        IF SEEK('S'+STYLE.SCALE,'Scale')
          IF &lcRejCrit
            =lfUpRej()
          ELSE
            =lfpakbysz()
            =lfupdhdr()
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  *WAIT CLEAR
  GO TOP IN (lcPakHdr)
  GO TOP IN (lcPakLin)
  SELECT PikTkt
  SET RELATION OFF INTO ORDLINE
  SET RELATION OFF INTO Customer
  RETURN .T.
ENDIF
RETURN .F.

**********************************************************************************************************************************************************

FUNCTION moptionspad
LOCAL lcHostFormName

lcHostFormName = '[' + THISFORMSET.cHostFormName + ']'

DEFINE PAD _Option OF (THISFORMSET.cHostFormName) PROMPT 'Options' KEY ALT+P , ' '
ON PAD _Option OF (THISFORMSET.cHostFormName) ACTIVATE POPUP _OPTIONPOP
DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT "Print Labels Setup"  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. ;
  _SCREEN.ACTIVEFORM.PARENT.ActiveMode <> "A"
ON SELECTION POPUP _OPTIONPOP _SCREEN.ACTIVEFORM.mlblsetup()
ON KEY LABEL ALT+P ACTIVATE POPUP _OPTIONPOP

**********************************************************************************************************************************************************

FUNCTION mRecordChanged
lcPakHdr   = poformclass.lcPakHdr
lcPakLin   = poformclass.lcPakLin
lcRejPkTmp = lcRejPkTmp

WITH THIS.ariapageframe1.ariapage1.grdRejected
  .RECORDSOURCE = lcRejPkTmp
  .column1.CONTROLSOURCE  = lcRejPkTmp + '.PikTkt'
  .column2.CONTROLSOURCE  = lcRejPkTmp + '.PikDate'
  .column3.CONTROLSOURCE  = lcRejPkTmp + '.Order'
  .column4.CONTROLSOURCE  = lcRejPkTmp + '.Account'
  .column5.CONTROLSOURCE  = lcRejPkTmp + '.Store'
  .column6.CONTROLSOURCE  = lcRejPkTmp + '.AccName'
  .column7.CONTROLSOURCE  = lcRejPkTmp + '.cReason'
  .SETALL('READONLY',.T.,'Column')
ENDWITH
WITH THIS.ariapageframe1.ariapage2.grdPckLst
  .RECORDSOURCE = lcPakHdr
  .column1.CONTROLSOURCE  = lcPakHdr+ '.Pack_No'
  .column2.CONTROLSOURCE  = lcPakHdr+ '.Order'
  .column3.CONTROLSOURCE  = lcPakHdr+ '.Account'
  .column4.CONTROLSOURCE  = lcPakHdr+ '.Store'
  .column5.CONTROLSOURCE  = lcPakHdr+ '.AccName'
  .column6.CONTROLSOURCE  = lcPakHdr+ '.Tot_Cart'
  .column7.CONTROLSOURCE  = lcPakHdr+ '.Tot_Pcs'
  .column8.CONTROLSOURCE  = lcPakHdr+ '.Pick_Qty'
  .column9.CONTROLSOURCE  = lcPakHdr+ '.Order_Qty'
  .column10.CONTROLSOURCE = lcPakHdr+ '.Tot_wght'
  .SETALL('READONLY',.T.,'Column')
ENDWITH
WITH THIS.ariapageframe1.ariapage3.grdPckDetail
  .RECORDSOURCE = lcPakLin
  .column1.CONTROLSOURCE  = lcPakLin+ '.From_Crt'
  .column30.CONTROLSOURCE  = lcPakLin+ '.To_Crt'
  .column2.CONTROLSOURCE  = lcPakLin+ '.Style'
  .column2.header1.CAPTION = THIS.ariapageframe1.ariapage3.keyItem.lcimjrheader
  .column3.CONTROLSOURCE  = lcPakLin+ '.Qty1'
  .column4.CONTROLSOURCE  = lcPakLin+ '.Qty2'
  .column5.CONTROLSOURCE  = lcPakLin+ '.Qty3'
  .column6.CONTROLSOURCE  = lcPakLin+ '.Qty4'
  .column7.CONTROLSOURCE  = lcPakLin+ '.Qty5'
  .column8.CONTROLSOURCE  = lcPakLin+ '.Qty6'
  .column9.CONTROLSOURCE  = lcPakLin+ '.Qty7'
  .column10.CONTROLSOURCE  = lcPakLin+ '.Qty8'
  .column11.CONTROLSOURCE  = lcPakLin+ '.TotQty'
  .column12.CONTROLSOURCE  = lcPakLin+ '.OrgOrd1'
  .column13.CONTROLSOURCE  = lcPakLin+ '.OrgOrd2'
  .column14.CONTROLSOURCE  = lcPakLin+ '.OrgOrd3'
  .column15.CONTROLSOURCE  = lcPakLin+ '.OrgOrd4'
  .column16.CONTROLSOURCE  = lcPakLin+ '.OrgOrd5'
  .column17.CONTROLSOURCE  = lcPakLin+ '.OrgOrd6'
  .column18.CONTROLSOURCE  = lcPakLin+ '.OrgOrd7'
  .column19.CONTROLSOURCE  = lcPakLin+ '.OrgOrd8'
  .column20.CONTROLSOURCE  = lcPakLin+ '.TotOrgOrd'
  .column21.CONTROLSOURCE  = lcPakLin+ '.Pik1'
  .column22.CONTROLSOURCE  = lcPakLin+ '.Pik2'
  .column23.CONTROLSOURCE  = lcPakLin+ '.Pik3'
  .column24.CONTROLSOURCE  = lcPakLin+ '.Pik4'
  .column25.CONTROLSOURCE  = lcPakLin+ '.Pik5'
  .column26.CONTROLSOURCE  = lcPakLin+ '.Pik6'
  .column27.CONTROLSOURCE  = lcPakLin+ '.Pik7'
  .column28.CONTROLSOURCE  = lcPakLin+ '.Pik8'
  .column29.CONTROLSOURCE  = lcPakLin+ '.TotPik'
  .SETALL('READONLY',.T.,'Column')
ENDWITH

**********************************************************************************************************************************************************

FUNCTION OPENTABLES
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'OrdHdr',oariaenvironment.DataDir+'OrdHdr','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'OrdLine',oariaenvironment.DataDir+'OrdLinSt','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Style',oariaenvironment.DataDir+'Style','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'StyDye',oariaenvironment.DataDir+'StyDye','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Scale',oariaenvironment.DataDir+'Scale','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Customer',oariaenvironment.DataDir+'Customer','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'PikTkt',oariaenvironment.DataDir+'OrdPik','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Pack_Hdr',oariaenvironment.DataDir+'Pack_Hdr','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Pack_Lin',oariaenvironment.DataDir+'Pack_Lin','SH')
*--Open ASN_SHIP & WAREHOUS files and create temp ASN_SHIP file [Begin]

* MAH
*WLD
*=oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir+'PACKINFO','PACKINFO','SH')
=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'EDIPKINF','EDIPKINF','SH')
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

=oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Asn_Ship','Asn_Ship','SH')
IF ('AS' $ oariaenvironment.CompanySetupModules )
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir + 'EDIACPRT' , oariaenvironment.DataDir + 'ACCFACT' , 'SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir + 'EDIPH' , oariaenvironment.DataDir + 'PARTNER' , 'SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'WareHous','WareHous','SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Spck_Lin','Spcklins','SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Spck_Hdr','SPCK_HDR','SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'StyleUpc','StyleUpc','SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir +'CUSTDEPT',oariaenvironment.DataDir+'CUSTDEPT' ,'SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir + 'EDIPD' , oariaenvironment.DataDir + 'PARTTRANS', 'SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir +'BOL_HDR',oariaenvironment.DataDir+'BOL_HDR' ,'SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir+'Bol_Lin',oariaenvironment.DataDir+'Bol_Lin','SH')
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.DataDir +'EdiSv',oariaenvironment.DataDir+'Key' ,'SH')
ENDIF
* HUSSIEN
* IF gfUserPriv('AL','ALAUPC','PRNPACKING')
IF .F.
  * HUSSIEN
  =oariaenvironment.remotetableaccess.OPENTABLE(oariaenvironment.SysPath+'SYCASNLB','ASNlbl','SH')
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
  =SEEK('O' + PikTkt.ORDER , 'ORDHDR', 'ORDHDR')

  SELECT('ordline')
  SET ORDER TO ORDLINST
  SEEK(OrdHdr.cOrdType + PikTkt.ORDER + PikTkt.STORE)
  *SET FILTER TO CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = ordhdr.cordtype + piktkt.order + piktkt.store
  SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = OrdHdr.cOrdType + PikTkt.ORDER + PikTkt.STORE
    IF ORDLINE.STYLE = 'AWIB82101A18-ASST'
      *set step on
      *set debug on
    ENDIF
    IF EMPTY(ORDLINE.pack_id)
      PackByPackingInfoBySize()
    ELSE
      IF TYPE('laUsedPacks[1]') = 'L'
        packByOrderPackingInfoByPack()
        laUsedPacks[1] = ORDLINE.pack_id
      ELSE
        IF ASCAN(laUsedPacks, ORDLINE.pack_id) = 0
          packByOrderPackingInfoByPack()

          DECLARE laUsedPacks[ALEN(laUsedPacks, 1) + 1]
          laUsedPacks[ALEN(laUsedPacks, 1)] = ORDLINE.pack_id
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
SEEK(ORDLINE.ORDER + STR(ORDLINE.LINENO,6))

SCAN REST WHILE ORDER+STR(LINENO,6)+STYLE+SIZE+pack_id = ORDLINE.ORDER + STR(ORDLINE.LINENO,6)
  RELEASE laQtyArr
  DIMENSION laQtyArr[9]
  STORE 0 TO laQtyArr

  lcSize = ALLTRIM(SIZE)

  LOCAL lnSize
  lnSize = VAL(lcSize)

  LOCAL lnPikQty
  lnPikQty = EVALUATE('ordline.pik'+lcSize)

  LOCAL lnCartonNumber
  lnCartonNumber = FLOOR(lnPikQty / (EDIPKINF.nPackUnits * EDIPKINF.nInnerPack))
  IF lnCartonNumber > 0
    lnFromCarton = lnToCarton + 1
    lnToCarton = lnFromCarton + lnCartonNumber - 1
    laQtyArr[lnSize] = (EDIPKINF.nPackUnits * EDIPKINF.nInnerPack)
    *lcStyle = SPCK_LIN.style
    lcStyle = ORDLINE.STYLE
    packByOrderPackingInfoUpdateCart()
  ENDIF

  LOCAL lnRemaining
  lnRemaining = lnPikQty % (EDIPKINF.nPackUnits * EDIPKINF.nInnerPack)
  IF lnRemaining > 0
    lnFromCarton = lnToCarton + 1
    lnToCarton = lnFromCarton
    laQtyArr[lnSize] = lnRemaining
    *lcStyle = SPCK_LIN.style
    lcStyle = ORDLINE.STYLE
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

SEEK('P' + OrdHdr.Account + ORDLINE.STYLE + ORDLINE.pack_id)
IF EOF()
  SEEK('P*****' + ORDLINE.STYLE + ORDLINE.pack_id)
ENDIF

SELECT('EDIPKINF')

** SAPCE(1) can be modfied to SPACE(2)
*SET FILTER TO ORDER+STR(LINENO,6)+STYLE+SIZE+PACK_ID = ordline.order + STR(ordline.LINENO,6) + SPACE(19) + SPACE(2) + ordline.pack_id
SEEK(ORDLINE.ORDER + STR(ORDLINE.LINENO,6) + SPACE(19) + SPACE(2) + ORDLINE.pack_id)

IF !EOF('SPCK_LIN') .AND. !EOF('EDIPKINF')
  LOCAL lnPackNo, lnSPCKLINTotQty
  lnPackNo = FLOOR(ORDLINE.Totpik / SPCK_LIN.TotQty)
  lnSPCKLINTotQty = SPCK_LIN.TotQty

  RELEASE laQtyArr
  DIMENSION laQtyArr[9]

  LOCAL lnCartonNumber
  *TEST lnCartonNumber = FLOOR(lnPackNo / EDIPKINF.nPackUnits)
  lnCartonNumber = lnPackNo

  IF lnCartonNumber > 0
    lnFromCarton = lnToCarton + 1
    lnToCarton = lnFromCarton + lnCartonNumber - 1

    SELECT('SPCK_LIN')
    SET ORDER TO SPCK_LIN
    *SET FILTER TO (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P' + ordhdr.account + ordline.pack_id) .OR. ;
    (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P*****' + ordline.pack_id)
    =SEEK('P' + OrdHdr.Account + ORDLINE.pack_id) .OR. SEEK('P*****' + ORDLINE.pack_id)

    SCAN REST WHILE (TYPE+Account+pack_id+STYLE+DYELOT = 'P' + OrdHdr.Account + ORDLINE.pack_id) .OR. ;
        (TYPE+Account+pack_id+STYLE+DYELOT = 'P*****' + ORDLINE.pack_id)
      FOR lnSize = 1 TO 8
        LOCAL lcSize
        lcSize = ALLTRIM(STR(lnSize))
        laQtyArr[lnSize] = SPCK_LIN.Qty&lcSize.
      ENDFOR
      *lcStyle = SPCK_LIN.style
      lcStyle = ORDLINE.STYLE
      packByOrderPackingInfoUpdateCart()
    ENDSCAN

  ENDIF

  LOCAL lnRemaining
  *TEST lnRemaining = lnPackNo % EDIPKINF.npackunits
  lnRemaining = ORDLINE.Totpik % lnSPCKLINTotQty
  IF lnRemaining > 0
    lnFromCarton = lnToCarton + 1
    lnToCarton = lnFromCarton

    SELECT('SPCK_LIN')
    SET ORDER TO SPCK_LIN
    *SET FILTER TO (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P' + ordhdr.account + ordline.pack_id) .OR. ;
    (TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT = 'P*****' + ordline.pack_id)
    = SEEK('P' + OrdHdr.Account + ORDLINE.pack_id) .OR. SEEK('P*****' + ORDLINE.pack_id)

    SCAN REST WHILE (TYPE+Account+pack_id+STYLE+DYELOT = 'P' + OrdHdr.Account + ORDLINE.pack_id) .OR. ;
        (TYPE+Account+pack_id+STYLE+DYELOT = 'P*****' + ORDLINE.pack_id)
      FOR lnSize = 1 TO 8
        LOCAL lcSize
        lcSize = ALLTRIM(STR(lnSize))
        laQtyArr[lnSize] = SPCK_LIN.Qty&lcSize.
      ENDFOR

      *lcStyle = SPCK_LIN.style
      lcStyle = ORDLINE.STYLE
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
= SEEK(ORDLINE.ORDER + ORDLINE.STORE, poformclass.lcPakHdr, 'ORDERPCK')
SET ORDER TO (poformclass.lcPakHdr)
LOCAL llNew
llNew = .F.

IF EOF(poformclass.lcPakHdr)
  APPEND BLANK
  REPLACE PACK_NO WITH ORDLINE.PikTkt, ;
    ORDER WITH ORDLINE.ORDER, Account WITH OrdHdr.Account , ;
    STORE WITH ORDLINE.STORE, Tot_Cart WITH lnToCarton - lnFromCarton  + 1, ;
    Tot_Pcs WITH (laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8]) * ORDLINE.nPackNo, ;
    ShipVia WITH OrdHdr.ShipVia, cWareCode WITH ORDLINE.cWareCode , PikTkt WITH ORDLINE.PikTkt
  *loAddUserInfo.Do(poformclass.lcPakHdr,.null.)
  llNew = .T.
ELSE
  REPLACE Tot_Cart WITH Tot_Cart + lnToCarton - lnFromCarton  + 1, ;
    Tot_Pcs WITH Tot_Pcs + (laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + ;
    laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8]) * ORDLINE.nPackNo
ENDIF

SELECT(poformclass.lcPakLin)
GOTO BOTTOM
LOCAL lnLastCartNo, lnLastLineNo
lnLastCartNo = No_Cart
lnLastLineNo = line_no

APPEND BLANK
REPLACE PACK_NO WITH ORDLINE.PikTkt, ;
  line_no WITH IIF(llNew, 1, lnLastLineNo + 1), ;
  From_Crt WITH lnFromCarton, To_Crt WITH lnToCarton, ;
  STYLE WITH lcStyle, ;
  Qty1 WITH laQtyArr[1], ;
  Qty2 WITH laQtyArr[2], ;
  Qty3 WITH laQtyArr[3], ;
  Qty4 WITH laQtyArr[4], ;
  Qty5 WITH laQtyArr[5], ;
  Qty6 WITH laQtyArr[6], ;
  Qty7 WITH laQtyArr[7], ;
  Qty8 WITH laQtyArr[8], ;
  TotQty WITH laQtyArr[1] + laQtyArr[2] + laQtyArr[3] + laQtyArr[4] + laQtyArr[5] + laQtyArr[6] + laQtyArr[7] + laQtyArr[8], ;
  nOrdLineNo WITH ORDLINE.LINENO, No_Cart WITH IIF(llNew, lnToCarton - lnFromCarton + 1, lnToCarton - lnFromCarton + 1 + lnLastCartNo), ;
  cZeroQty WITH "N", ;
  pack_id WITH ORDLINE.pack_id, ;
  nPackNo WITH ORDLINE.nPackNo

=SEEK(ORDLINE.STYLE,'Style', 'Style')
SELECT(poformclass.lcPakHdr)
REPLACE Tot_Wght WITH Tot_Wght + STYLE.nstyweight * ORDLINE.Totpik

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
