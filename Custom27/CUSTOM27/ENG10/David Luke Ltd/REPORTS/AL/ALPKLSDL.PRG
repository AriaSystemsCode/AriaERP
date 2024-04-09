*:***************************************************************************
*: Program file  : ALPKLSDL.PRG
*: Program desc. : CUSTOMIZED PACKING LIST Form FOR DAVID LUKE LTD.
*: Date          : 12/19/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200432
*:
*:***************************************************************************
*: Calls : lfBasToClr , LFDelPhon , lfGrpSetes , lfNonMjDes , lfSpckln
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO ALPKLSDL
*:***************************************************************************
*: Modification:
*****************************************************************************
*:B607007,1 03/30/2003 WAB Fix the bug of printing the same price for all styles
*:C200511,1 ABD 04/15/2003 Add an user definable field to the customer file
*:C200511,1 ABD            And print the piktkt collected based on this custom
*:C200511,1 ABD            Field if yes or No.
*:C200562,1 ABD Add some change at the layout while & after printing.
*:B122855,1 ASH 05/17/2004 Fix bug of not printing 'Remains to follow' in some cases.
*:B128619,1 BWA 07/05/2004 Fix bug of printing styles in wrong index in case group sequence = "Y"
*:****************************************************************************

*--In case the user select YES in "Product Group Sequence".
lcEmail  = gfGetMemVar('M_CEMAIL' , gcAct_Comp)
llMScale = gfGetMemVar('M_USEEXSSC')

*--Initial the variables hold the Delivery NotePad.
STORE SPACE(0) TO lcDelNot1 , lcDelNot2

*-- Get company Phone.
PRIVATE lcAlasPhon
lcAlasPhon = SELECT(0)
SELECT SYCCOMP
SEEK gcAct_Comp
lcPhonComp = cCom_Phon             && Company Phone.
SELECT(lcAlasPhon)

*C200511,1 ABD - Collect data with a new way in case print packing list. [Begin]
IF lcRpSelcBy = "P"                            && Packing list
  lcTmpGroup = ''
  
  *C200562,1 ABD - Define new table to hold the printed packing list. [Begin]
  lcPckPrtUp = gfTempName()
  CREATE CURSOR (lcPckPrtUp) (Pack_No  C(6))
  INDEX ON Pack_No TAG Invoice of (gcWorkDir+lcPckPrtUp)
  *C200562,1 ABD - [End]
  
  *-- This Code to handle the new way of collecting data.
  = lfPrtPack ()
  
  *C200562,1 ABD - update the pack header file with the printing date if empty. [Begin]
  SELECT (lcPckPrtUp)
  SCAN 
    SELECT PACK_Hdr
    IF SEEK(EVAL(lcPckPrtUp+'.Pack_No')) .AND. Empty(dShipdate)
      REPLACE DSHIPDATE  WITH gdSysDate
    ENDIF
  ENDSCAN  
  USE IN (lcPckPrtUp)
  *C200562,1 ABD - [End]
  RETURN
ENDIF
*C200511,1 ABD - [End]

IF llRpPrGrS
  PRIVATE lcAlasDl , lcOrdDl , lcKeyDL , lcOldPktmp
  lcAlasDl = SELECT(0)
  lcOrdDl  = ORDER()
  lcKeyDL  = EVAL(KEY())

  SELECT (lclinfile)
  lcAdStyGrp = gfTempName()
  =lfCreatTmp()

  IF lcRpSelcBy = "I"                              && Invoice
    lnPosInv = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVOICE'),1)
  ENDIF

  IF lcRpSelcBy = "T"                              && Picking Ticket
    lnPosPik = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'PIKTKT.PIKTKT'),1)
  ENDIF

  SELECT (lclinfile)
  SCAN
    IF lcRpSelcBy = "I"                              && Invoice
      IF USED(laOGFxFlt[lnPosInv,6]) AND SEEK(INVOICE , laOGFxFlt[lnPosInv,6])
        =lfGetData()
      ENDIF
    ELSE
      IF lcRpSelcBy = "T"                          && Picking Ticket
        IF USED(laOGFxFlt[lnPosPik,6]) AND SEEK(PIKTKT , laOGFxFlt[lnPosPik,6])
          =lfGetData()
        ENDIF
      ELSE
        =lfGetData()
      ENDIF
    ENDIF
  ENDSCAN
  
  *--Section to the Notepad Template from the SM module.
  IF SEEK("T" + "DELIVERY" , 'NOTEPAD')
    SELECT (lcAdStyGrp)
    PRIVATE lcPack
    LOCATE
    lcPack = IIF( lcRpSelcBy = "I" , INVOICE , IIF( lcRpSelcBy = "P" , PACK_NO , CORDTYPE + ORDER))
    SCAN
      IF lcPack # IIF( lcRpSelcBy = "I" , INVOICE , IIF( lcRpSelcBy = "P" , PACK_NO , CORDTYPE + ORDER))
        lcPack = IIF( lcRpSelcBy = "I" , INVOICE , IIF( lcRpSelcBy = "P" , PACK_NO , CORDTYPE + ORDER))
        SKIP - 1
        REPLACE llSmPad WITH .T.            ,;
                NoteSm  WITH NOTEPAD.MNOTES
        SKIP
      ENDIF
    ENDSCAN
    *--The last pack no must update witht the SM notepad.
    SELECT (lcAdStyGrp)
    LOCATE
    GOTO BOTTOM
    REPLACE llSmPad WITH .T.            ,;
            NoteSm  WITH NOTEPAD.MNOTES
  ENDIF

  *--Section to evaluate the logic field to print the scale.
  SELECT (lcAdStyGrp)
  LOCATE

  PRIVATE lcSclDl , lcHoldMstr
  STORE SPACE(0) TO lcSclDl , lcHoldMstr
  SCAN
    IF lcHoldMstr # IIF(lcRpSelcBy = "I" , INVOICE , IIF(lcRpSelcBy = "P" , PACK_NO , CORDTYPE + ORDER ))
      STORE SPACE(0) TO lcSclDl
    ENDIF
    IF lcSclDl # ScalDL
      REPLACE llPrnScal WITH .T.
    ENDIF
    lcHoldMstr = IIF(lcRpSelcBy = "I" , INVOICE , IIF(lcRpSelcBy = "P" , PACK_NO , CORDTYPE + ORDER))
    lcSclDl = ScalDL
  ENDSCAN

  *--Save the old name to restore it at the end if the custom 
  *--program becasue it is used in the main program.
  lcOldPktmp = lcpaklntmp

  *--Assign the name of the new indexed file to the data files.
  lclinfile = lcAdStyGrp
  IF lcRpSelcBy = "P"                            && Packing list
    lcpaklntmp = lcAdStyGrp
  ENDIF

  *--Section break the relations and Creat the index and the new relations.
  IF lcRpSelcBy = "P"                                && Packing list
    SELECT PACK_HDR
    SET RELATION TO
  ELSE
    IF lcRpSelcBy = "I"                              && Invoice
      SELECT INVHDR
      SET RELATION TO
    ELSE                                             && Pick Ticket
      SELECT ORDHDR
      SET RELATION TO
    ENDIF
  ENDIF

  SELECT (lclinfile)
  IF lcRpSelcBy = "I"                              && Invoice
    INDEX ON INVOICE + STYGROP + STR(LINENO,6) TAG lcGrpSty
  ELSE
    IF lcRpSelcBy = "P"                            && Packing list
      INDEX ON PACK_NO + STR(NO_CART,4) + STYGROP + STYLE TAG lcGrpSty
    ELSE                                           && Pick Ticket
      INDEX ON CORDTYPE + ORDER + STORE + STYGROP + STYLE + STR(LINENO,6) TAG lcGrpSty
    ENDIF
  ENDIF

  IF lcRpSelcBy = "P"                            && Packing list
    SELECT PACK_HDR
    SET RELATION TO PACK_NO INTO (lclinfile)
  ELSE
    IF lcRpSelcBy = "I"                              && Invoice
      SELECT INVHDR
      SET RELATION TO INVOICE INTO (lclinfile)
      SELECT (lclinfile)
      SET RELATION TO STYLE INTO STYLE ADDITIVE
    ELSE                                           && Pick Ticket
      SELECT ORDHDR
      SET RELATION TO "O" + ORDER INTO (lclinfile)
      SELECT (lclinfile)
      SET RELATION TO STYLE INTO STYLE ADDITIVE
    ENDIF
  ENDIF

  *--case the invocie only create new file hold the data in the (lcpaklntmp) with new index for the style group.
  lcSGrpPkln = gfTempName()
  =lfTmpPack()

  SELECT (lcPackTmp)
  LOCATE
  DO gfDispRe WITH EVAL('lcFormName')
  llALPakLst = .F.

  *--Restore the old name.
  lcpaklntmp = lcOldPktmp

  SELECT(lcAlasDl)
  SET ORDER TO TAG &lcOrdDl
  =SEEK(lcKeyDL)
  
  *--Function to clear the Temp. file.
  =lfBasToClr(lcSGrpPkln , 'F')

ELSE
  *--To be able to print the style descreption I use the same name.
  lcAdStyGrp = gfTempName()
  *ash1
  CREATE CURSOR (lcAdStyGrp) (Style C(19) , StylDesc C(60) , ScalDL C(3) , Sz1DL C(5)  ,;
                              Sz2DL C(5)  , Sz3DL C(5)     , Sz4DL C(5)  , Sz5DL C(5)  ,;
                              Sz6DL C(5)  , Sz7DL C(5)     , Sz8DL C(5)  , llPrnScal L , Price N(7,2),;
                              MastrFld C(IIF(lcRpSelcBy = "T" , 7 , 6)))
  INDEX ON Style + MastrFld + StylDesc TAG Desc1 of (gcWorkDir + lcAdStyGrp)

  SELECT (lclinfile)
  PRIVATE lcEvlKyDl , lcHoldMstr
  STORE SPACE(0) TO lcHoldMstr
  lcEvlKyDl = EVAL(KEY())
  LOCATE
  SCAN
    lcHoldMstr = IIF(lcRpSelcBy = "I" , INVOICE , IIF(lcRpSelcBy = "P" , PACK_NO , CORDTYPE + ORDER ))
    IF !SEEK(STYLE + lcHoldMstr , lcAdStyGrp)
      =SEEK(STYLE , 'STYLE')
      SELECT (lcAdStyGrp)
      APPEND BLANK
      REPLACE STYLE    WITH &lclinfile..STYLE ,;
              StylDesc WITH STYLE.DESC1       ,;
              ScalDL   WITH STYLE.SCALE       ,;
              Sz1DL    WITH SCALE.SZ1         ,;
              Sz2DL    WITH SCALE.SZ2         ,;
              Sz3DL    WITH SCALE.SZ3         ,;
              Sz4DL    WITH SCALE.SZ4         ,;
              Sz5DL    WITH SCALE.SZ5         ,;
              Sz6DL    WITH SCALE.SZ6         ,;
              Sz7DL    WITH SCALE.SZ7         ,;
              Sz8DL    WITH SCALE.SZ8         ,;
              MastrFld WITH lcHoldMstr
      *ash1
      
      *B607007,1 WAB (Start) -the realtion is not correct in case of the tempfile is created from 
      *B607007,1 WAB		 -Pack_lin because there is no field in the Pack_lin Named PikTkt and 
      *B607007,1 WAB		 -the relation between the temp file and the piktkt file base on piktkt
      *IF SEEK('O'+Piktkt.Order+PikTkt.store+style,'OrdLine')
      *   REPLACE &lcAdSty..GrpPrice WITH OrdLine.Price              
      *ENDIF
      lcOrder = Piktkt.Order
      IF lcRpSelcBy = "P" 
        lcOrder = ''
        IF SEEK(&lclinfile..Pack_no,'PIKTKT')
          lcOrder = PIKTKT.Order
        ELSE
          lcOrder = Pack_hdr.Order
        ENDIF
      ENDIF
      IF SEEK('O'+lcOrder,'OrdLine')
        SELECT 'OrdLine' 
        *cordtype+order+store+style+STR(lineno,6)
        LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR STYLE = &lcAdStyGrp..Style
        IF FOUND()
         REPLACE &lcAdStyGrp..Price  WITH OrdLine.Price
        ENDIF
      ENDIF
      *B607007,1 WAB (End)

      SELECT (lclinfile)
    ENDIF
  ENDSCAN

  *--Section to evaluate the logic field to print the scale.
  SELECT (lcAdStyGrp)
  LOCATE

  PRIVATE lcSclDl , lcHoldMstr
  STORE SPACE(0) TO lcSclDl , lcHoldMstr
  SCAN
    IF lcHoldMstr # MastrFld
      STORE SPACE(0) TO lcSclDl
    ENDIF
    IF lcSclDl # ScalDL
      REPLACE llPrnScal WITH .T.
    ENDIF
    lcHoldMstr = MastrFld
    lcSclDl = ScalDL
  ENDSCAN

  STORE SPACE(0) TO lcHoldSek
  SELECT (lclinfile)
  SET RELATION TO STYLE + IIF(lcRpSelcBy = "I" , INVOICE , IIF(lcRpSelcBy = "P" , PACK_NO , "O" + ORDER )) INTO (lcAdStyGrp) ADDTIVE
  =SEEK(lcEvlKyDl)

  *--Section of the print.
  SELECT (lcPackTmp)
  LOCATE
  DO gfDispRe WITH EVAL('lcFormName')
  llALPakLst = .F.

ENDIF

*--Function to clear the Temp. file.
=lfBasToClr(lcAdStyGrp , 'F')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/19/2002
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*!*************************************************************
*! Name      : LFDelPhon
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/19/2002
*! Purpose   : Function to delete the phone number.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =LFDelPhon()
*!*************************************************************
FUNCTION LFDelPhon
PARAMETER lcReturn

IF ASCAN(laCompAdd , "Phone# : ") > 0
  lnPos = ASUBSCRIPT(laCompAdd , ASCAN(laCompAdd , "Phone# : ") ,1)
  laCompAdd[lnPos,1] = SPACE(0)
ENDIF

RETURN ""
*--End of LFDelPhon
*!*************************************************************
*! Name      : lfGrpSetes
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/19/2002
*! Purpose   : Function to get the wanted settings for the FRX.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGrpSetes()
*!*************************************************************
FUNCTION lfGrpSetes
PARAMETERS llReturn

*C200511,1 ABD - return in case print packing list.[Begain]
IF lcRpSelcBy = "P" 
  RETURN
ENDIF
*C200511,1 ABD - [End]

PRIVATE lcSkipExpr , lnCurAlias , lcSkipTag , lcCurRec
lnCurAlias = SELECT(0)
lnGrdTotWg = 0

lcSkipExpr = ''
SET SKIP TO

SELECT INVLINE
SET RELATION OFF INTO STYLE

SELECT (lcPakLnTmp)
SET RELATION OFF INTO STYLE

IF llRpPrGrS
  SELECT (lcAdStyGrp)
  SET RELATION OFF INTO STYLE
ENDIF

SELECT (lnCurAlias)
PRIVATE lcLocExpr

DO CASE
  CASE lcRpSelcBy = "I"                              && Invoice
    SELECT INVLINE
    IF llRpPrGrS
      lcLinFile = lcAdStyGrp
    ELSE
      lcLinFile = "INVLINE"
    ENDIF
    lcPackNo  = INVOICE
    lcLocExpr = "INVOICE = '" + lcPackNo + "' AND ORDER = '" + ORDHDR.ORDER + "'"

  CASE lcRpSelcBy = "P"                            && Packing list
    SELECT (lcPakLnTmp)
    lcLinFile = lcPakLnTmp
    lcPackNo  = Pack_no
    lcLocExpr = ""

  CASE lcRpSelcBy = "T"                            && Picking Ticket
    SELECT ORDLINE
    IF llRpPrGrS
      lcLinFile = lcAdStyGrp
      lcLocExpr = "CORDTYPE + ORDER = '" + lcPackNo + "' AND PIKTKT = '" + EVAL(lcAdStyGrp+'.PIKTKT') + "'"
    ELSE
      lcLinFile = "ORDLINE"
      lcLocExpr = "CORDTYPE + ORDER = '" + lcPackNo + "' AND PIKTKT = '" + ORDLINE.PIKTKT + "'"
    ENDIF
    lcPackNo  = "O" + ORDER

ENDCASE
lcSkipTag = ORDER()
lcCurRec = EVALUATE(KEY())
SET ORDER TO (lcSkipTag) DESC

IF SEEK(lcPackNo)
  IF !EMPTY(lcLocExpr)
    LOCATE REST WHILE &lcLocExpr
  ENDIF
  lnLastRec = RECNO()
ENDIF
SET ORDER TO (lcSkipTag) ASCE
=SEEK(lcCurRec)

SELECT (lnCurAlias)
IF llRpPrGrS
  *--bas
  lcSkipExpr = IIF(nRprtTyp=1,lcAdStyGrp,IIF(nRprtTyp=2,lcPakLnTmp,lcAdStyGrp))
  *lcSkipExpr = IIF(nRprtTyp = 1 , lcAdStyGrp , IIF(nRprtTyp = 2 , IIF(lcRpSelcBy = "I" , lclinfile , lcPakLnTmp) , lcAdStyGrp))
ELSE
  lcSkipExpr = IIF(nRprtTyp=1,"INVLINE",IIF(nRprtTyp=2,lcPakLnTmp,"ORDLINE"))
  *lcSkipExpr = IIF(nRprtTyp = 1 , "INVLINE" , IIF(nRprtTyp = 2 , IIF(lcRpSelcBy = "I" , lclinfile , lcPakLnTmp) , "ORDLINE"))
  *--bas
ENDIF

SET SKIP TO &lcSkipExpr
llEndGroup = .F.

RETURN ""
*--End of lfGrpSetes.
*!*************************************************************
*! Name      : lfNonMjDes
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/19/2002
*! Purpose   : Evaluate Non Major Code and Description.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNonMjDes()
*!*************************************************************
FUNCTION lfNonMjDes
PARAMETERS llReturn

PRIVATE lnI , lcTemp , lcStyle , lcNonMjDes,lnAlias
STORE '' TO lcTemp , lcNonMjDes , lnAlias
lnAlias = SELECT()
SELECT(lcPackTmp)

lcStyle = IIF(&lcPackTmp..nRprtTyp=1,&lclinfile..STYLE,IIF(&lcPackTmp..nRprtTyp=2,&lcPakLnTmp..STYLE,&lclinfile..STYLE))
lnI = 0

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  lcTemp = ''

  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSegs[lnI,1] $ "FOTQ"
      IF SEEK(STR(lnI,1)+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),"ICSEGVAL")
        lcTemp = ALLTRIM(ICSEGVAL.cISgValSd)
      ENDIF
    *-- Season, Color, Division, or lcStyle group Segment.
    CASE laMajSegs[lnI,1] $ "ZCDG"
      DO CASE
        CASE laMajSegs[lnI,1] = "Z"
          lcCodeExpr = "SEASON"
        CASE laMajSegs[lnI,1] = "C"
          lcCodeExpr = "COLOR"
        CASE laMajSegs[lnI,1] = "D"
          lcCodeExpr = "CDIVISION"
        OTHERWISE
          lcCodeExpr = "CSTYGROUP"
      ENDCASE

      lcTemp = ALLTRIM(gfCodDes(SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcCodeExpr,.F.))
    *-- Size Seqment case.
    OTHERWISE
      IF SEEK("S"+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),"SCALE")
        lcTemp = ALLTRIM(SCALE.cScl_desc)
      ENDIF

  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
ENDFOR    && end Loop Around Non Major elements.

lcStyle    = IIF(lnExtScPos = 0,lcStyle,LEFT(lcStyle,LEN(lcStyle)-lnExtScLen))
lcStyleExp = lcStyle+' '+lcNonMjDes

*--Seek to get the right recno in the lclinfile.
IF llMScale
  IF llRpPrGrS AND lcRpSelcBy = "I"                              && Invoice
    =SEEK( EVAL(lcPackTmp+'.INVOICE') + EVAL(lcLinFile+'.STYGROP') + STR(EVAL(lcPakLnTmp+'.LINE_NO'),6) , lcLinFile )
  ELSE
    IF lcRpSelcBy = "I"
      =SEEK( EVAL(lcPackTmp+'.INVOICE') + STR(EVAL(lcPakLnTmp+'.LINE_NO'),6) , lcLinFile )
    ENDIF
  ENDIF
ENDIF

SELECT(lnAlias)
RETURN ''

*-- End Of lfNonMjDes.
*!*************************************************************
*! Name      : lfSpckln
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/19/2002
*! Purpose   : Fill The Array With Spck_lin & Scale
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSpckln()
*!*************************************************************
FUNCTION lfSpckln
PARAMETERS llReturn

PRIVATE lcStyls , lnCount , lcCount ,lcAlias
STORE '' TO  lcStyls , lnCount , lcCount
DIMENSION laTemp[8]

STORE "" TO laSpckTemp , laTemp
lcAlias = SELECT (0)

IF lcRpForm = "ALPKLSB" AND llPrnPack
  =lfGetPack(&lcPackTmp..Order, &lcPackTmp..Store, &lcLinFile..Style)
ENDIF

PRIVATE lcAlasPck , lcOrdrLin , lnRcNoOrd , lcOrdrSek , lcLnNoFld
lcAlasPck = SELECT(0)
SELECT ORDLINE
lnRcNoOrd = RECNO()
lcOrdrLin = ORDER()
SET ORDER TO TAG ORDLINE
lcOrdrSek = EVAL(lcPackTmp+'.ORDER')
LOCATE

DO CASE
  CASE lcRpSelcBy $ "TI"
    lcLnNoFld = STR(EVAl(lcLinFile+'.LINENO'),6)
  CASE lcRpSelcBy = "P"
    lcLnNoFld = STR(EVAl(lcLinFile+'.nordlineno'),6)
ENDCASE

IF SEEK("O" + lcOrdrSek + lcLnNoFld) AND !EMPTY(ORDLINE.PACK_ID)
    STORE SPACE(0) TO laSpckTemp[1]
    laSpckTemp[1] = "Pack ID : " + ALLTRIM(ORDLINE.PACK_ID)
ELSE
  IF SEEK( 'S' + &lcPackTmp..ACCOUNT + &lcLinFile..Style, 'Spck_Lin' )
    IF Spck_Lin.TotQty = 0
      laSpckTemp[1] ='SKU #:' + Spck_Lin.Pack_Id
    ELSE
      lnCount = 1
      SELECT Spck_Lin
      lnSavRec = RECNO()
      FOR lnCount = 1 To 8
        GOTO lnSavRec
        lcCount = STR(lnCount, 1 )
        IF  !EMPTY(EVAL(lcLinFile+'.Qty'+lcCount))
          SCAN REST WHILE Type+Account+Style+Pack_id = 'S' + &lcPackTmp..Account+&lcLinFile..Style;
            FOR lnCount <= 8
              IF !EMPTY(Spck_Lin.Qty&lcCount)
                laTemp[lnCount] =IIF(EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)),'',Scale.Sz&lcCount + ':' + Spck_Lin.Pack_Id)
              ENDIF
          ENDSCAN
        ENDIF
      ENDFOR
    ENDIF
  ENDIF

SELECT ORDLINE
IF BETWEEN(lnRcNoOrd,1,RECCOUNT('ORDLINE'))
  GOTO lnRcNoOrd IN ORDLINE
ENDIF
SET ORDER TO TAG &lcOrdrLin
SELECT(lcAlasPck)
ENDIF

lnNtpty = 1
FOR I   = 1 TO 8
  IF !EMPTY(laTemp[I])
    laSpckTemp[lnNtpty] = laTemp[I]
    lnNtpty = lnNtpty + 1
  ENDIF
ENDFOR

SELECT (lcalias)
RETURN ''
*--End Of lfSpckln.
*!*************************************************************
*! Name      : lfGetData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/19/2002
*! Purpose   : Fill the temp. file with the data.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetData()
*!*************************************************************
FUNCTION lfGetData

SCATTER MEMVAR MEMO
SELECT (lcAdStyGrp)
APPEND BLANK
GATHER MEMVAR MEMO
IF SEEK(STYLE , 'STYLE')
  REPLACE &lcAdStyGrp..StyGrop  WITH STYLE.CSTYGROUP ,;
          &lcAdStyGrp..StylDesc WITH STYLE.DESC1     ,;
          &lcAdStyGrp..ScalDL   WITH STYLE.SCALE     ,;
          &lcAdStyGrp..Sz1DL    WITH SCALE.SZ1       ,;
          &lcAdStyGrp..Sz2DL    WITH SCALE.SZ2       ,;
          &lcAdStyGrp..Sz3DL    WITH SCALE.SZ3       ,;
          &lcAdStyGrp..Sz4DL    WITH SCALE.SZ4       ,;
          &lcAdStyGrp..Sz5DL    WITH SCALE.SZ5       ,;
          &lcAdStyGrp..Sz6DL    WITH SCALE.SZ6       ,;
          &lcAdStyGrp..Sz7DL    WITH SCALE.SZ7       ,;
          &lcAdStyGrp..Sz8DL    WITH SCALE.SZ8
  *ash1

  *B607007,1 WAB (Start) -the realtion is not correct in case of the tempfile is created from 
  *B607007,1 WAB		 -Pack_lin because there is no field in the Pack_lin Named PikTkt and 
  *B607007,1 WAB		 -the relation between the temp file and the piktkt file base on piktkt
  *IF lcRpSelcBy = "P" AND SEEK('O'+Piktkt.Order+PikTkt.store+style,'OrdLine')
  *  REPLACE &lcAdStyGrp..Price  WITH OrdLine.Price
  *ENDIF
  IF lcRpSelcBy = "P" 
    lcOrder = ''
    IF SEEK(&lclinfile..Pack_no,'PIKTKT')
      lcOrder = PIKTKT.Order
    ELSE
      lcOrder = Pack_hdr.Order
    ENDIF
    IF SEEK('O'+lcOrder,'OrdLine')
      SELECT 'OrdLine' 
      *cordtype+order+store+style+STR(lineno,6)
      LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR STYLE = &lcAdStyGrp..Style
      IF FOUND()
        REPLACE &lcAdStyGrp..Price  WITH OrdLine.Price
      ENDIF
    ENDIF
  ENDIF
  *B607007,1 WAB (End) 

  
  
ENDIF

*--End of lfGetData.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/19/2002
*! Purpose   : Creat tje Tmp. file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp

SELECT (lclinfile)
=AFIELDS(laTmpStru)
lnTmpStru = ALEN(laTmpStru,1)
*ash1
*DIMENSION laTmpStru[lnTmpStru + 14 ,4]
IF lcRpSelcBy = "P"
  DIMENSION laTmpStru[lnTmpStru + 15 ,4]

  *C200511,1 ABD - New dimi for the array in case print Packing list. [Begin]
  *DIMENSION laTmpStru[lnTmpStru + 15 ,4]
  DIMENSION laTmpStru[lnTmpStru + 19 ,4]
  *C200511,1 ABD - [End]
  
ELSE
  DIMENSION laTmpStru[lnTmpStru + 14 ,4]
ENDIF 

*-- Field hold the style group data.
laTmpStru[lnTmpStru + 1 ,1] = 'StyGrop'
laTmpStru[lnTmpStru + 1 ,2] = 'C'
laTmpStru[lnTmpStru + 1 ,3] = 6
laTmpStru[lnTmpStru + 1 ,4] = 0

*-- Logic field to check if print the notepad or not.
laTmpStru[lnTmpStru + 2 ,1] = 'llSmPad'
laTmpStru[lnTmpStru + 2 ,2] = 'L'
laTmpStru[lnTmpStru + 2 ,3] = 1
laTmpStru[lnTmpStru + 2 ,4] = 0

*-- Field hold the SM Template notepad.
laTmpStru[lnTmpStru + 3 ,1] = 'NoteSm'
laTmpStru[lnTmpStru + 3 ,2] = 'M'
laTmpStru[lnTmpStru + 3 ,3] = 10
laTmpStru[lnTmpStru + 3 ,4] = 0

*-- Field hold the SM Template notepad.
laTmpStru[lnTmpStru + 4 ,1] = 'StylDesc'
laTmpStru[lnTmpStru + 4 ,2] = 'C'
laTmpStru[lnTmpStru + 4 ,3] = 60
laTmpStru[lnTmpStru + 4 ,4] = 0

*--Section for fields scale and sizes.
laTmpStru[lnTmpStru + 5 ,1] = 'ScalDL'
laTmpStru[lnTmpStru + 5 ,2] = 'C'
laTmpStru[lnTmpStru + 5 ,3] = 3
laTmpStru[lnTmpStru + 5 ,4] = 0

laTmpStru[lnTmpStru + 6 ,1] = 'Sz1DL'
laTmpStru[lnTmpStru + 6 ,2] = 'C'
laTmpStru[lnTmpStru + 6 ,3] = 5
laTmpStru[lnTmpStru + 6 ,4] = 0

laTmpStru[lnTmpStru + 7 ,1] = 'Sz2DL'
laTmpStru[lnTmpStru + 7 ,2] = 'C'
laTmpStru[lnTmpStru + 7 ,3] = 5
laTmpStru[lnTmpStru + 7 ,4] = 0

laTmpStru[lnTmpStru + 8 ,1] = 'Sz3DL'
laTmpStru[lnTmpStru + 8 ,2] = 'C'
laTmpStru[lnTmpStru + 8 ,3] = 5
laTmpStru[lnTmpStru + 8 ,4] = 0

laTmpStru[lnTmpStru + 9 ,1] = 'Sz4DL'
laTmpStru[lnTmpStru + 9 ,2] = 'C'
laTmpStru[lnTmpStru + 9 ,3] = 5
laTmpStru[lnTmpStru + 9 ,4] = 0

laTmpStru[lnTmpStru + 10 ,1] = 'Sz5DL'
laTmpStru[lnTmpStru + 10 ,2] = 'C'
laTmpStru[lnTmpStru + 10 ,3] = 5
laTmpStru[lnTmpStru + 10 ,4] = 0

laTmpStru[lnTmpStru + 11 ,1] = 'Sz6DL'
laTmpStru[lnTmpStru + 11 ,2] = 'C'
laTmpStru[lnTmpStru + 11 ,3] = 5
laTmpStru[lnTmpStru + 11 ,4] = 0

laTmpStru[lnTmpStru + 12 ,1] = 'Sz7DL'
laTmpStru[lnTmpStru + 12 ,2] = 'C'
laTmpStru[lnTmpStru + 12 ,3] = 5
laTmpStru[lnTmpStru + 12 ,4] = 0

laTmpStru[lnTmpStru + 13 ,1] = 'Sz8DL'
laTmpStru[lnTmpStru + 13 ,2] = 'C'
laTmpStru[lnTmpStru + 13 ,3] = 5
laTmpStru[lnTmpStru + 13 ,4] = 0

laTmpStru[lnTmpStru + 14 ,1] = 'llPrnScal'
laTmpStru[lnTmpStru + 14 ,2] = 'L'
laTmpStru[lnTmpStru + 14 ,3] = 1
laTmpStru[lnTmpStru + 14 ,4] = 0

*ash1
IF lcRpSelcBy = "P"
  laTmpStru[lnTmpStru + 15 ,1] = 'Price'
  laTmpStru[lnTmpStru + 15 ,2] = 'N'
  laTmpStru[lnTmpStru + 15 ,3] = 7
  laTmpStru[lnTmpStru + 15 ,4] = 2
  
  *C200511,1 ABD - New fields for the array in case print Packing list. [Begin]
  laTmpStru[lnTmpStru + 16 ,1] = 'ACCOUNT'
  laTmpStru[lnTmpStru + 16 ,2] = 'C'
  laTmpStru[lnTmpStru + 16 ,3] = 5
  laTmpStru[lnTmpStru + 16 ,4] = 0
  
  laTmpStru[lnTmpStru + 17 ,1] = 'STORE'
  laTmpStru[lnTmpStru + 17 ,2] = 'C'
  laTmpStru[lnTmpStru + 17 ,3] = 8
  laTmpStru[lnTmpStru + 17 ,4] = 0

  laTmpStru[lnTmpStru + 18 ,1] = 'CDelivery'
  laTmpStru[lnTmpStru + 18 ,2] = 'C'
  laTmpStru[lnTmpStru + 18 ,3] = 1
  laTmpStru[lnTmpStru + 18 ,4] = 0

  laTmpStru[lnTmpStru + 19 ,1] = 'CGroupKey'
  laTmpStru[lnTmpStru + 19 ,2] = 'C'
  laTmpStru[lnTmpStru + 19 ,3] = 6
  laTmpStru[lnTmpStru + 19 ,4] = 0
  *C200511,1 ABD - [End]
  
ENDIF

CREATE TABLE (gcWorkDir + lcAdStyGrp) FROM ARRAY laTmpStru
IF lcRpSelcBy = "I"                              && Invoice
  INDEX ON INVOICE + STYGROP + STR(LINENO,6) TAG lcAdStyGrp
ELSE
  IF lcRpSelcBy = "P"                            && Packing list
    *C200511,1 ABD - new index on account and store in case print packing list. [Begin]
    *INDEX ON PACK_NO + STR(NO_CART,4) + STYGROP + STYLE TAG lcAdStyGrp

	*B128619,1 BWA 07/05/2004 Add new index.[START]
    INDEX ON PACK_NO + STYGROP TAG PackStyGrp ADDITIVE
    *B128619,1 BWA 07/05/2004.[END]

    INDEX ON Account + Store + cGroupkey  + CDelivery + PACK_NO TAG lcAdStyGrp ADDITIVE
    
    lcTmpGroup = gfTempName()
    CREATE TABLE (gcWorkDir+lcTmpGroup) ( CGroupKey C(6) , PACK_NO C(6) , weightdl N(13,2) , noofcarton N(8) ,;
     TOT_PCS N(8) , DPrintDate D(8), consgment C(20) , llRemantTo L(1))
    INDEX ON CGroupKey  Tag (lcTmpGroup)
    *C200511,1 ABD - [End]
  
  ELSE                                           && Pick Ticket
    INDEX ON CORDTYPE + ORDER + STORE + STYGROP + STYLE + STR(LINENO,6) TAG lcAdStyGrp
  ENDIF
ENDIF

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfDelNote
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/19/2002
*! Purpose   : Print the first 2 lines of the Template Notepad.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDelNote()
*!*************************************************************
FUNCTION lfDelNote
PARAMETERS lcReturn

STORE SPACE(0) TO lcDelNot1 , lcDelNot2

PRIVATE lcAlasDelv
lcAlasDelv = SELECT(0)
lnMemoSet = SET('MEMOWIDTH')
SET MEMOWIDTH TO 100

SELECT NOTEPAD
IF SEEK("T" + "DELIVERY" , 'NOTEPAD')
  lcDelNot1 = MLINE(NOTEPAD.MNOTES,1)
  lcDelNot2 = MLINE(NOTEPAD.MNOTES,2)
ENDIF

SELECT(lcAlasDelv)
SET MEMOWIDTH TO lnMemoSet
RETURN ''

*--End of lfDelNote.
*C200511,1 ABD - Call all this function below  in this custom program. [Begin]
*:*************************************************************
*: Name      : lfPrtPack
*: Developer : Abdou Elgendy [ABD]
*: Date      : 04/14/2003
*: Purpose   : Print packing list only.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfPrtPack ()
*:*************************************************************
*:
FUNCTION lfPrtPack
PRIVATE lnOlsAls , lcOldPktmp

lnOlsAls = SELECT (0)
IF llRpPrGrS
  SELECT (lcPackTmp)
  SET RELATION TO 
  
  *--Save the old name to restore it at the end if the custom 
  *--program becasue it is used in the main program.
  lcOldPktmp = lcpaklntmp


  SELECT PACK_HDR
  SET RELATION TO

  SELECT (lclinfile)
  lcAdStyGrp = gftempName()

  *- Create temp file.
  = lfCreatTmp ()

  SELECT (lclinfile)
  SCAN
    SCATTER MEMVAR MEMO
    M.cGroupkey = 'zzzzzz'
    M.llPrnScal = .T.
    = lfInsertRd ()
  ENDSCAN
  

  
  *--Assign the name of the new indexed file to the data files.
  lcpaklntmp = lcAdStyGrp
  *-- Function to update the grouping key fields.
  = lfUpdgroup ()
   
  SELECT (lcPackTmp)
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
                  PACK_NO               INTO PACK_HDR,; 
                  INVOICE               INTO INVHDR  ,; 
                  PikTkt			    INTO PIKTKT  ,;
                  "O" + ORDER           INTO ORDHDR  ,;
                  "O" + ORDER + STORE   INTO ORDLINE

  SELECT (lcpaklntmp)

  *B128619,1 BWA 07/05/2004 Use the new index.[START]
  PRIVATE lcOrder
  IF llRpPrGrS
    lcOrder = ORDER()
    SET ORDER TO TAG PackStyGrp
  ENDIF
  *B128619,1 BWA 07/05/2004.[END]

  SET RELATION TO  PACK_NO INTO (lcPackTmp) ADDITIVE
  SET RELATION TO CGroupKey INTO (lcTmpGroup) ADDITIVE
  SELECT (lcPakLnTmp)
  LOCATE
  DO gfDispRe WITH EVAL('lcFormName')

  *B128619,1 BWA 07/05/2004 Retrieve the original order.[START]
  IF llRpPrGrS
    SET ORDER TO TAG &lcOrder
  ENDIF
  *B128619,1 BWA 07/05/2004.[END]

  llALPakLst = .F.

  *--Restore the old name.
  lcpaklntmp = lcOldPktmp

ELSE
  lcOldPktmp = lcpaklntmp 
  SELECT (lcPackTmp)
  SET RELATION TO 
 
   *--To be able to print the style descreption I use the same name.
  lcAdStyGrp = gfTempName()
   = lfCreatTmp ()
  SELECT Pack_hdr
  SET RELATION TO
  SELECT (lclinfile)
  PRIVATE lcEvlKyDl , lcPack_No
  
  STORE SPACE(0) TO lcPack_No
  lcEvlKyDl = EVAL(KEY())
  LOCATE
  SCAN
    SCATTER MEMVAR MEMO
    = lfInsertRc ()
  ENDSCAN

  *--Section to evaluate the logic field to print the scale.
  SELECT (lcAdStyGrp)
  LOCATE

  *--Assign the name of the new indexed file to the data files.
  lcpaklntmp = lcAdStyGrp
  *-- Function to update the grouping key fields.
  = lfUpdgroup ()
   
  SELECT (lcPackTmp)
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
                  PACK_NO               INTO PACK_HDR,; 
                  INVOICE               INTO INVHDR  ,; 
                  PikTkt			    INTO PIKTKT  ,;
                  "O" + ORDER           INTO ORDHDR  ,;
                  "O" + ORDER + STORE   INTO ORDLINE


  SELECT (lcpaklntmp)
  SET RELATION TO  PACK_NO INTO (lcPackTmp)
  SELECT (lcPakLnTmp)
  LOCATE
  DO gfDispRe WITH EVAL('lcFormName')
  llALPakLst = .F.

  *--Restore the old name.
  lcpaklntmp = lcOldPktmp

ENDIF


SELECT(lnOlsAls)

*-- End OF lfPrtPack.
*:*************************************************************
*: Name      : lfInsertRd
*: Developer : Abdou Elgendy [ABD]
*: Date      : 04/14/2003
*: Purpose   : Insert record at the temp file.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfInsertRd ()
*:*************************************************************
*:
FUNCTION lfInsertRd
PRIVATE lnPrvAls

lnPrvAls = SELECT (0)

SELECT (lcAdStyGrp)
APPEND BLANK
GATHER MEMVAR MEMO

IF SEEK(M.Pack_No,'PACK_HDR')
  = SEEK(IIF(EMPTY(PACK_HDR.STORE),'M','S') + PACK_HDR.Account + PACK_HDR.Store,'CUSTOMER')
  REPLACE &lcAdStyGrp..Account   WITH PACK_HDR.Account ,;
          &lcAdStyGrp..STORE     WITH PACK_HDR.Store   ,;
          &lcAdStyGrp..cDelivery WITH IIF(Customer.llDelivery,'Y','N')
ENDIF

IF SEEK(M.Style , 'STYLE')
  REPLACE &lcAdStyGrp..StyGrop  WITH Style.CSTYGROUP ,;
          &lcAdStyGrp..StylDesc WITH Style.DESC1     ,;
          &lcAdStyGrp..ScalDL   WITH Style.SCALE     ,;
          &lcAdStyGrp..Sz1DL    WITH SCALE.SZ1       ,;
          &lcAdStyGrp..Sz2DL    WITH SCALE.SZ2       ,;
          &lcAdStyGrp..Sz3DL    WITH SCALE.SZ3       ,;
          &lcAdStyGrp..Sz4DL    WITH SCALE.SZ4       ,;
          &lcAdStyGrp..Sz5DL    WITH SCALE.SZ5       ,;
          &lcAdStyGrp..Sz6DL    WITH SCALE.SZ6       ,;
          &lcAdStyGrp..Sz7DL    WITH SCALE.SZ7       ,;
          &lcAdStyGrp..Sz8DL    WITH SCALE.SZ8

  *-- The realtion is not correct in case of the tempfile is created from 
  *-- Pack_lin because there is no field in the Pack_lin Named PikTkt and 
  *-- The relation between the temp file and the piktkt file base on piktkt
  *-- IF lcRpSelcBy = "P" AND SEEK('O'+Piktkt.Order+PikTkt.store+Style,'OrdLine')
  lcOrder = ''
  IF SEEK(&lclinfile..Pack_no,'PIKTKT')
    lcOrder = PIKTKT.Order
  ELSE
    lcOrder = Pack_hdr.Order
  ENDIF
  
  IF SEEK('O'+lcOrder,'OrdLine')
    SELECT 'OrdLine' 
    LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR Style = &lcAdStyGrp..Style
    IF FOUND()
      REPLACE &lcAdStyGrp..Price  WITH OrdLine.Price
    ENDIF
  ENDIF
ENDIF

SELECT (lnPrvAls)

*-- End OF lfInsertRd
*:*************************************************************
*: Name      : lfUpdgroup
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/20/2003
*: Purpose   : Function to update the grouping key fields
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfUpdgroup()
*:*************************************************************
*:
FUNCTION lfUpdgroup
PRIVATE lnPrvAls

lnPrvAls = SELECT (0)

SELECT(lcPaklntmp)
LOCATE

lnGroupKey = 1
STORE Pack_No To lcPack_No , lcOldPiktk
STORE '' TO lcCurtpktk 

lcOldKey   = Account + cDelivery + Store

SCAN
  = SEEK(Pack_No,'Pack_hdr')
  IF (lcOldKey # Account + cDelivery + Store) .OR. (lcCurtpktk # Pack_No .AND. cDelivery = 'N')
    lnGroupKey = lnGroupKey + 1

    *-- Update the group file
    SELECT (lcTmpGroup)
    APPEND BLANK
    REPLACE Pack_No    WITH &lcPaklntmp..Pack_No    ,;
            CGroupKey  WITH ALLTRIM(STR(lnGroupKey)),;
            weightdl   WITH weightdl   + Pack_hdr.weightdl  ,;
            noofcarton WITH noofcarton + Pack_hdr.noofcarton,;
            TOT_PCS    WITH TOT_PCS    + Pack_hdr.TOT_PCS

    *C200562,1 ABD - Update the grouping file with the first packing ship date. [Begin]
    REPLACE DPrintDate WITH Pack_hdr.dShipdate,;
            consgment  WITH Pack_hdr.consgment
    *C200562,1 ABD - [End]
            
    SELECT (lcPaklntmp)
  ENDIF  
  REPLACE CGroupKey WITH ALLTRIM(STR(lnGroupKey))
  *B122855,1 ASH 05/17/2004 (Begin) Fix bug of not printing 'Remains to follow' in some cases.      
  IF lfRemToflw ()
    REPLACE &lcTmpGroup..llRemantTo    WITH .T.
  ENDIF
  *B122855,1 ASH 05/17/2004 (End)
  IF (lcOldPiktk # Pack_No .AND. lnOldGroup = lnGroupKey) .OR. EOF(lcTmpGroup)
    IF EOF(lcTmpGroup)
      SELECT (lcTmpGroup)
      APPEN BLANK
      REPLACE Pack_No    WITH &lcPaklntmp..Pack_No     ,;
              CGroupKey  WITH ALLTRIM(STR(lnGroupKey)) ,;
              weightdl   WITH weightdl   + Pack_hdr.weightdl  ,;
              noofcarton WITH noofcarton + Pack_hdr.noofcarton,;
              TOT_PCS    WITH TOT_PCS    + Pack_hdr.TOT_PCS
  
      *C200562,1 ABD - Update the grouping file with the first packing ship date. [Begin]
      REPLACE DPrintDate WITH Pack_hdr.dShipdate,;
              consgment  WITH Pack_hdr.consgment
      *C200562,1 ABD - [End]
            
      SELECT(lcPaklntmp)
    ELSE
      REPLACE &lcTmpGroup..Pack_No    WITH '******',;
              &lcTmpGroup..weightdl   WITH &lcTmpGroup..weightdl   + Pack_hdr.weightdl  ,;
              &lcTmpGroup..noofcarton WITH &lcTmpGroup..noofcarton + Pack_hdr.noofcarton,;
              &lcTmpGroup..TOT_PCS    WITH &lcTmpGroup..TOT_PCS    + Pack_hdr.TOT_PCS
    ENDIF

    *-- Function to Check if there are outstanding items on sales order print Remains to follow
    IF lfRemToflw ()
      REPLACE &lcTmpGroup..llRemantTo    WITH .T.
    ENDIF
    
  ENDIF
  
  
  lcOldKey   = Account + cDelivery + Store
  
  
  STORE Pack_No To  lcOldPiktk , lcCurtpktk
  lnOldGroup = lnGroupKey

  IF llRpPrnNot .AND. Pack_No # lcPack_No  .AND. SEEK(lcPack_No,'Pack_hdr') .AND.;
    SEEK("B"+Pack_hdr.Order,'NOTEPAD')
    lcPack_No = Pack_No
    SKIP - 1
    REPLACE llSmPad    WITH .T.            ,;
            NoteSm     WITH NOTEPAD.MNOTES
    SKIP
  ENDIF
ENDSCAN


SELECT(lcPaklntmp)
GO BOTTOM
IF llRpPrnNot .AND. SEEK(Pack_No,'Pack_hdr') .AND. SEEK("B"+Pack_hdr.Order,'NOTEPAD')
  REPLACE llSmPad WITH .T.            ,;
          NoteSm  WITH ALLTRIM(NOTEPAD.MNOTES)
ENDIF

SELECT (lnPrvAls)
*-- End OF lfUpdgroup
*:*************************************************************
*: Name      : lfUpdgroup
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/20/2003
*: Purpose   : Function to update the grouping key fields
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfUpdgroup()
*:*************************************************************
*:
FUNCTION lfInsertRc
PRIVATE lnPrvAls

lnPrvAls = SELECT (0)

= SEEK(M.Pack_No,'Pack_hdr')    
lcPack_No = PACK_NO

=SEEK(STYLE,'STYLE')
SELECT (lcAdStyGrp)
APPEND BLANK
GATHER MEMVAR MEMO      

REPLACE STYLE     WITH &lclinfile..STYLE ,;
        StylDesc  WITH STYLE.DESC1       ,;
        ScalDL    WITH STYLE.SCALE       ,;
        Sz1DL     WITH SCALE.SZ1         ,;
        Sz2DL     WITH SCALE.SZ2         ,;
        Sz3DL     WITH SCALE.SZ3         ,;
        Sz4DL     WITH SCALE.SZ4         ,;
        Sz5DL     WITH SCALE.SZ5         ,;
        Sz6DL     WITH SCALE.SZ6         ,;
        Sz7DL     WITH SCALE.SZ7         ,;
        Sz8DL     WITH SCALE.SZ8         ,;
        PACK_NO   WITH lcPack_No         ,;
        llPrnScal WITH .T.               ,;
        cGroupkey WITH 'zzzzzz'
lcOrder = Piktkt.Order
IF lcRpSelcBy = "P" 
  lcOrder = ''
  IF SEEK(&lclinfile..Pack_no,'PIKTKT')
    lcOrder = PIKTKT.Order
  ELSE
    lcOrder = Pack_hdr.Order
  ENDIF
  
ENDIF
IF SEEK('O'+lcOrder,'OrdLine')
  SELECT 'OrdLine' 
  LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR STYLE = &lcAdStyGrp..Style
  IF FOUND()
    REPLACE &lcAdStyGrp..Price  WITH OrdLine.Price
  ENDIF
ENDIF

IF SEEK(M.Pack_No,'PACK_HDR')
  = SEEK(IIF(EMPTY(PACK_HDR.STORE),'M','S') + PACK_HDR.Account + PACK_HDR.Store,'CUSTOMER')
  REPLACE &lcAdStyGrp..Account   WITH PACK_HDR.Account ,;
          &lcAdStyGrp..STORE     WITH PACK_HDR.Store   ,;
          &lcAdStyGrp..cDelivery WITH IIF(Customer.llDelivery,'Y','N')
ENDIF
SELECT (lclinfile)

SELECT (lnPrvAls)
*-- End OF lfInsertRc
*:*************************************************************
*C200511,1 ABD - Call all this function below  in this custom program. [End]
*:*************************************************************
*: Name      : lfUpdgroup
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/20/2003
*: Purpose   : Function to update the grouping key fields
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfUpdgroup()
*:*************************************************************
*:C200562,1
FUNCTION lfUpdatPrt

*If the Device is not Screen
IF gcDevice <> 'SCREEN'
  IF !SEEK(Pack_no,lcPckPrtUp)
    INSERT INTO (lcPckPrtUp) (Pack_No) VALUES (Pack_hdr.Pack_No)
  ENDIF
ENDIF    && End of IF

*-- End OF lfUpdatPrt.
*:*************************************************************
*: Name      : lfRemToflw
*: Developer : Abdou Elgendy [ABD]
*: Date      : 05/20/2003
*: Purpose   : Function to Check if there are outstanding 
*: Purpose   : Items on sales order print Remains to follow
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfRemToflw()
*:*************************************************************
*:C200562,1
FUNCTION lfRemToflw
PRIVATE lnAlias , llRemains

lnAlias = SELECT (0)
llRemains = .F.
*-- cordtype+order+store+style+STR(lineno,6)
IF SEEK('O'+PACK_Hdr.Order,'OrdHdr') 
  IF Ordhdr.Open+Ordhdr.Ship > PACK_Hdr.Tot_pcs
    llRemains = .T.
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN llRemains
*-- End OF lfRemToflw
*!*************************************************************
*! Name      : lfTmpPack
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 07/13/2005
*! Purpose   : Creat Tmp. file for (lcpaklntmp) with new index for the style group.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfTmpPack()
*!*************************************************************
FUNCTION lfTmpPack

PRIVATE lcAlias , lcKeyPk

SELECT STYLE
lcAlias = SELECT(0)
lcKeyPk = EVAL(KEY())

SELECT (lcpaklntmp)
=AFIELDS(laTmpStru)
=lfAddField("laTmpStru", "StyGrop" , "C",6,0)

CREATE TABLE (gcWorkDir + lcSGrpPkln) FROM ARRAY laTmpStru
INDEX ON PACK_NO + STYGROP TAG PackStyGrp ADDITIVE
INDEX ON PACK_NO + STR(NO_CART,4) + STYLE TAG MainIndx ADDITIVE

SELECT (lcpaklntmp)
SCAN
  SCATTER MEMVAR MEMO
  SELECT (lcSGrpPkln)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE StyGrop WITH IIF(SEEK(STYLE, 'STYLE') , STYLE.CSTYGROUP , SPACE(0))
ENDSCAN

SELECT (lcSGrpPkln)
SET ORDER TO TAG PackStyGrp

SELECT PACK_HDR
SET RELATION TO
lcpaklntmp = lcSGrpPkln
SET RELATION TO PACK_NO INTO (lcpaklntmp)

SELECT(lcAlias)
=SEEK(lcKeyPk)

*--End of lfTmpPack.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.