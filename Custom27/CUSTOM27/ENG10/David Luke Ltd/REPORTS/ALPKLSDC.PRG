*:***************************************************************************
*: Program file  : ALPKLSDC.PRG
*: Program desc. : CUSTOMIZED PACKING LIST Form FOR DCC.
*: Date          : 2/06/2005
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: CC123849
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
*: Example : DO ALPKLSDC
*:***************************************************************************
*: Modification:
*: C127511,1 NNA 09/15/2005 Use Ordhdr.cContRef instead of Ordhdr.Custpo or InvHdr.Custpo
*: C127511,1 NNA            that Because Customer need This Field to Be 30 Chr. (Fixed in the FRX)
*: B130949,1 MHM 01/26/2006 In case of at store level it is not working correct and did not get to follow part
*:T20060831.0016 TMI 09/07/2006 Fix a bug that when user selects two packings with same color the report do not show the first 
*                               packing style information
*****************************************************************************

*--Initial the variables used in the program.
STORE SPACE(0) TO lcDelNot1 , lcDelNot2
STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
=lfChkStrct() && Get the length of the style , color and scale.

*--In case the user select YES in "Product Group Sequence".
lcEmail  = gfGetMemVar('M_CEMAIL' , gcAct_Comp)
llMScale = gfGetMemVar('M_USEEXSSC')

                       *-- Open the files used in the Report--*
*--EMPLOYEE file to print the Ucode and name.
IF !USED('EMPLOYEE')
  =gfOpenFile(gcDataDir+'EMPLOYEE',gcDataDir+'EMPLOYEE','SH')
ENDIF

IF !USED('POSHDR')
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF

IF !USED('POSLN')
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLNS','SH')
ELSE
  SELECT POSLN
  SET ORDER TO TAG POSLNS
ENDIF

                       *-- End Open the files used in the Report--*
*-- Get company Phone.
PRIVATE lcAlasPhon , lcPhonUser
lcAlasPhon = SELECT(0)
SELECT SYCCOMP
SEEK gcAct_Comp
lcPhonComp = cCom_Phon             && Company Phone.

*--Get User Phone.
SELECT SYUUSER
SEEK gcUser_ID
lcPhonUser = CUSR_PHON

SELECT(lcAlasPhon)
*--End getting values.

*--Define new table to hold the printed packing list.
CREATE CURSOR (lcPckPrtUp) (Pack_No  C(6))
INDEX ON Pack_No TAG Invoice of (gcWorkDir+lcPckPrtUp)

*-- This Code to handle the new way of collecting data.
=lfPrtPack()

*-- update the pack header file with the printing date if empty.
SELECT (lcPckPrtUp)
SCAN
  SELECT PACK_Hdr
  IF SEEK(EVAL(lcPckPrtUp+'.Pack_No')) .AND. Empty(dShipdate)
    REPLACE DSHIPDATE WITH gdSysDate
  ENDIF
ENDSCAN
USE IN (lcPckPrtUp)

*--Function to clear the Temp. file.
=lfBasToClr(lcAdStyGrp , 'F')

RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*! Purpose   : Function to delete the phone number.
*!*************************************************************
*! Called from :
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*! Purpose   : Function to get the wanted settings for the FRX.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGrpSetes()
*!*************************************************************
FUNCTION lfGrpSetes
PARAMETERS llReturn

*--Return in case print packing list.
IF lcRpSelcBy = "P"
  llEndGroup = .F.
  RETURN
ENDIF

*--End of lfGrpSetes.
*!*************************************************************
*! Name      : lfNonMjDes
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*! Purpose   : Evaluate Non Major Code and Description.
*!*************************************************************
*! Called from :
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

lcStyle = &lcPakLnTmp..STYLE
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
lcStyleExp = lcStyle + ' ' + lcNonMjDes

SELECT(lnAlias)
RETURN ''

*-- End Of lfNonMjDes.
*!*************************************************************
*! Name      : lfSpckln
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*! Purpose   : Fill The Array With Spck_lin & Scale
*!*************************************************************
*! Called from :
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

PRIVATE lcAlasPck , lcOrdrLin , lnRcNoOrd , lcOrdrSek , lcLnNoFld
lcAlasPck = SELECT(0)
SELECT ORDLINE
lnRcNoOrd = RECNO()
lcOrdrLin = ORDER()
SET ORDER TO TAG ORDLINE
lcOrdrSek = EVAL(lcPackTmp+'.ORDER')
LOCATE

lcLnNoFld = STR(EVAl(lcLinFile+'.nordlineno'),6)

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
*! Name      : lfDelNote
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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
*:*************************************************************
*: Name      : lfPrtPack
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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
FUNCTION lfPrtPack

PRIVATE lnOlsAls , lcOldPktmp , lcEvlKyDl , lcPack_No , llToFollow , lcPoChk , lcPoChk2 , lcOpenPO
PRIVATE lcHldScal , lcKey , lcStyClr , lcValStClr , lcScalVal , lcCdm1Val , lcNewPack , ldAvalbl
PRIVATE lcChngSty , lcAccPrnDs
STORE SPACE(0) TO lcHldScal , lcKey , lcStyClr , lcValStClr , lcScalVal , lcCdm1Val
STORE SPACE(0) TO lcPack_No , lcNewPack , lcOpenPO
STORE {} TO ldAvalbl
lnOlsAls = SELECT (0)

lcOldPktmp = lcpaklntmp

SELECT POSLN
IF !('POSLN.PO INTO POSHDR' $ SET('RELATION'))
  SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE
ENDIF

SELECT (lcPackTmp)
SET RELATION TO

*--To be able to print the style descreption I use the same name.
=lfCreatTmp()

*--Section to add the ordlines records that doesn't exsit in the packlines file 
*--to add it in section To Follow in the report.
=lfAdMsdRec()

SELECT PACK_HDR
SET RELATION TO
SET RELATION TO IIF(EMPTY(Pack_hdr.Store) , 'M' + Pack_hdr.Account ,'S' + Pack_hdr.Account + Pack_hdr.Store) INTO CUSTOMER ADDITIVE

SELECT (lclinfile)
LOCATE
SET RELATION TO EVAL(lclinfile+'.pack_no') INTO PACK_HDR ADDITIVE
lcEvlKyDl = EVAL(KEY())

SCAN
  WAIT WINDOW 'Selecting Records For The Report ...' + PACK_NO NOWAIT
  *--mhm130949
  =SEEK('M' + Pack_hdr.Account,'CUSTOMER')
  =SEEK(IIF(EMPTY(Pack_hdr.Store) , 'M' + Pack_hdr.Account ,'S' + Pack_hdr.Account + Pack_hdr.Store),'CUSTOMER')
  *--mhm130949
  llToFollow = CUSTOMER.CTOFOLLOW
  lcValStClr = SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + "-" + SUBSTR(STYLE,lnClrPosGl,lnClrLnGl)
  SCATTER MEMVAR MEMO

  lcKey = SUBSTR(STYLE , lnScaPosGl , 2)
  IF !(lcKey $ lcHldScal)
    =lfGetSizes()
    lcHldScal = lcHldScal + IIF(EMPTY(lcHldScal) , "" , "," ) + lcKey
  ENDIF

  IF lcStyClr # lcValStClr OR lcNewPack # PACK_NO
    =lfInsertRc()
  ELSE
    =lfnsrtfnd()
  ENDIF
  lcStyClr = lcValStClr
ENDSCAN

*--Section to evaluate the logic field to print the scale.
SELECT (lcAdStyGrp)
LOCATE

SELECT PACK_HDR
SET RELATION TO

*--Assign the name of the new indexed file to the data files.
lcpaklntmp = lcAdStyGrp

*-- Function to update the grouping key fields.
=lfUpdgroup()

SELECT (lcPackTmp)
SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
                PACK_NO               INTO PACK_HDR,;
                INVOICE               INTO INVHDR  ,;
                PikTkt			      INTO PIKTKT  ,;
                "O" + ORDER           INTO ORDHDR  ,;
                "O" + ORDER + STORE   INTO ORDLINE

SELECT (lcpaklntmp)

SET RELATION TO  PACK_NO INTO (lcPackTmp)
DELETE ALL FOR TOTQTY = 0
LOCATE
DELETE ALL FOR EMPTY(FolowRec) AND LINE_NO = 0

STORE SPACE(0) TO lcPoChk , lcPoChk2 , lcNewPckNo , lcChngSty , lcAccPrnDs
SCAN
  IF lcPoChk2 # PACK_NO AND !EMPTY(lcPoChk2) AND llRpPrnNot
    SKIP -1
    REPLACE &lcpaklntmp..llPrnNtPd WITH .T.
    SKIP 1
  ENDIF

  *-- This case to be able to print the style description and the line at last record.
  IF (lcChngSty # SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) AND !EMPTY(lcChngSty)) OR ;
	 (lcChngSty == SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) AND !EMPTY(lcChngSty) AND (lcAccPrnDs # ACCOUNT))	 
    SKIP -1
    IF ACCOUNT # "ZZZZZ"
      REPLACE &lcpaklntmp..llPrnSDc WITH .T.
    ENDIF
    REPLACE &lcpaklntmp..llPrnLin WITH .T.
    SKIP 1
  ENDIF
  
  IF lcPoChk # PACK_NO AND ACCOUNT = "ZZZZZ"
    REPLACE &lcpaklntmp..PrnToFol WITH .T.
  ENDIF

  IF ACCOUNT = "ZZZZZ" AND TOTQTY > 0 
    REPLACE &lcpaklntmp..llPrnLin WITH .T.
  ENDIF

  IF llRpPrnNot .AND. Pack_No # lcPoChk2    ;
     AND SEEK(lcPoChk2,'Pack_hdr')          ;
     AND SEEK("B"+Pack_hdr.Order,'NOTEPAD')

    SKIP - 1
    REPLACE llSmPad WITH .T.            ,;
            NoteSm  WITH NOTEPAD.MNOTES
    SKIP
  ENDIF

  lcPoChk = IIF(ACCOUNT = "ZZZZZ" ,  PACK_NO , "")
  lcPoChk2   = PACK_NO
  lcChngSty  = SUBSTR(STYLE,lnStyPosGl,lnStyLnGl)
  lcAccPrnDs = ACCOUNT
ENDSCAN

*-- This case to be able to print the style description and the line at last record.
IF (lcChngSty # SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) AND !EMPTY(lcChngSty)) OR ;
  (lcChngSty == SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) AND !EMPTY(lcChngSty) AND (lcAccPrnDs # ACCOUNT))
  SKIP -1
  IF ACCOUNT # "ZZZZZ"
    REPLACE &lcpaklntmp..llPrnSDc WITH .T.
  ENDIF
  REPLACE &lcpaklntmp..llPrnLin WITH .T.
  SKIP 1
ENDIF

*T20060831.0016 TMI 09/07/2006 [START]
** For each pack allow to print the last line in the pack, this to fix a problem that if two packings with one style then the
** report do not print the first one style information
SELECT (lcpaklntmp)
LOCATE
lcPckno = PACK_NO
DO WHILE !EOF(lcpaklntmp)
  SCAN REST WHILE PACK_NO = lcPckno  
  ENDSCAN   
  lcPckno = PACK_NO   && assign the new pack no
  SKIP -1
  REPLACE llPrnSDc WITH .T. ;
          llPrnLin WITH .T.
  SKIP  
ENDDO
*T20060831.0016 TMI 09/07/2006 [END  ]

*--Section change the value of the llPrnSDc to .T. in case value = .F.
GOTO BOTTOM
IF !&lcpaklntmp..llPrnSDc
  REPLACE &lcpaklntmp..llPrnSDc WITH .T.
ENDIF
  
IF llRpPrnNot
  GOTO BOTTOM
  IF llRpPrnNot .AND. SEEK(Pack_No,'Pack_hdr') .AND. SEEK("B"+Pack_hdr.Order,'NOTEPAD')
    REPLACE llSmPad WITH .T.            ,;
          NoteSm  WITH ALLTRIM(NOTEPAD.MNOTES)
  ENDIF
  REPLACE &lcpaklntmp..llPrnNtPd WITH .T.
ENDIF
LOCATE

DO gfDispRe WITH EVAL('lcFormName')
llALPakLst = .F.
WAIT CLEAR
SET DEVICE TO SCREEN

*--Restore the old name.
lcpaklntmp = lcOldPktmp

SELECT (lclinfile)
SET RELATION TO
DELETE ALL FOR COWNER == "BBBBVVVVDDDDRRRR"

SELECT(lnOlsAls)

*-- End OF lfPrtPack.
*!*************************************************************
*! Name      : lfGetSizes
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*! Purpose   : Function to collect the scale data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfGetSizes()
*!*************************************************************
FUNCTION lfGetSizes
PRIVATE lcAlias

lcAlias = ALIAS()
PRIVATE lnI , lnHdr , lnScalRec , lnContSrt
STORE 0 TO lnI , lnHdr , lnScalRec , lnContSrt

SELECT SCALE
lnScalRec = IIF(EOF('SCALE') , 0 , RECNO('SCALE'))
LOCATE

IF SEEK("S" + SUBSTR(STYLE , lnScaPosGl , 2))
  lnContSrt = 1
  SCAN FOR TYPE + SCALE + PREPAK = "S" + lcKey
    SCATTER MEMVAR MEMO
    SELECT (lcTmpSizes)
    SET ORDER TO TAG (lcTmpSizes)

    IF SEEK(lcKey + cDim1 , lcTmpSizes)
      IF &lcTmpSizes..cDim1 == SCALE.Cdim1
        FOR lnCrtTmp = 1 TO 8
          lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp+8))
          lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
          IF !EMPTY(SCALE.&lcSizFld)
            REPLACE &lcTmpSizes..&lcNumSiz WITH SCALE.&lcSizFld ,;
                    &lcTmpSizes..cDim1     WITH SCALE.Cdim1
          ENDIF
        ENDFOR
      ELSE
        lnContSrt = lnContSrt + 1
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE &lcTmpSizes..ScalFld WITH LEFT(SCALE.SCALE,2) ,;
                &lcTmpSizes..cDim1   WITH SCALE.Cdim1         ,;
                &lcTmpSizes..IndxDm2 WITH lnContSrt
      ENDIF
    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE &lcTmpSizes..ScalFld WITH LEFT(SCALE.SCALE,2) ,;
              &lcTmpSizes..IndxDm2 WITH 1
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcTmpSizes)
SET ORDER TO TAG SortScal
REPLACE &lcTmpSizes..llPrnSDc WITH .T.

SELECT SCALE
IF lnScalRec <> 0
  GOTO lnScalRec IN SCALE
ENDIF

SELECT(lcAlias)

*--End of lfGetSizes.
*:*************************************************************
*: Name      : lfInsertRc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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
*: Example     : =lfInsertRc()
*:*************************************************************
FUNCTION lfInsertRc
PRIVATE lnPrvAls , lcHldDim , lcKeyRec , lcOldOrdr , lcOldOrdr2
STORE SPACE(0) TO lcHldDim , lcKeyRec , lcOldOrdr , lcOldOrdr2

PRIVATE llRtrnVl
STORE .F. TO llRtrnVl

lnPrvAls = SELECT(0)

=SEEK(M.Pack_No,'Pack_hdr')
lcPack_No = PACK_NO

IF lcNewPack # lcPack_No
  lcNewPack = lcPack_No
ENDIF
=SEEK(IIF(EMPTY(Pack_hdr.Store) , 'M' + Pack_hdr.Account ,'S' + Pack_hdr.Account + Pack_hdr.Store),'CUSTOMER')
=SEEK(STYLE,'STYLE')

SELECT ORDLINE
lcOldOrdr = ORDER()
SET ORDER TO TAG ORDLINST

SELECT (lcAdStyGrp)
SET RELATION TO "O" + PACK_HDR.ORDER + STORE + STYLE INTO ORDLINE

lcOldOrdr2 = ORDER()
SET ORDER TO TAG lcStyFond
IF SEEK(EVAL(lclinfile+'.PACK_NO') + SUBSTR(EVAL(lclinfile+'.STYLE'),lnStyPosGl,lnStyLnGl)) AND !EMPTY(SCALE.CDIM1)
  =lfnsrtfnd()
  SELECT (lcAdStyGrp)
  SET ORDER TO TAG &lcOldOrdr2
  RETURN
ENDIF
SET ORDER TO TAG &lcOldOrdr2

APPEND BLANK
REPLACE STYLE     WITH &lclinfile..STYLE ,;
        StylDesc  WITH STYLE.DESC1       ,;
        ScalDL    WITH STYLE.SCALE       ,;
        lcDim1    WITH SCALE.cDim1       ,;
        PACK_NO   WITH lcPack_No         ,;
        llPrnScal WITH .T.               ,;
        cGroupkey WITH 'zzzzzz'

=SEEK( LEFT(STYLE.SCALE,2) , lcTmpSizes )
FOR lnCrtTmp = 1 TO 16
  lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp)) + "dl"
  lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
  IF !EMPTY(&lcTmpSizes..&lcSizFld)
    REPLACE &lcAdStyGrp..&lcNumSiz WITH &lcTmpSizes..&lcSizFld
  ENDIF
ENDFOR

*--Update the fields with the quentities.
GATHER MEMVAR MEMO FIELDS EXCEPT Qty*

FOR lnScl = 1 TO 1
  lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
  FOR lnAll = 1 TO 16
    lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
    IF SCALE.&lcSizScl == &lcAdStyGrp..&lcAdsScl
      FOR lnFill = lnScl TO 8
	    lcSizScl = "Sz" + ALLTRIM(STR(lnFill))
        lcQtyVal = "Qty" + ALLTRIM(STR(lnFill))
        lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
        IF !EMPTY(SCALE.&lcSizScl)
          REPLACE &lcAdStyGrp..&lcAdsScl WITH M.&lcQtyVal
        ENDIF
      ENDFOR
      EXIT
    ENDIF
  ENDFOR
ENDFOR

IF SEEK(M.Pack_No,'PACK_HDR')
  =SEEK(IIF(EMPTY(PACK_HDR.STORE),'M','S') + PACK_HDR.Account + PACK_HDR.Store,'CUSTOMER')
  REPLACE &lcAdStyGrp..Account   WITH PACK_HDR.Account ,;
          &lcAdStyGrp..STORE     WITH PACK_HDR.Store   ,;
          &lcAdStyGrp..cDelivery WITH IIF(Customer.llDelivery,'Y','N')
ENDIF

lcHldDim = &lcAdStyGrp..lcDim1
lcKeyRec = EVAL(KEY())

IF SEEK(SUBSTR(STYLE , lnScaPosGl , 2), lcTmpSizes )
  SELECT (lcTmpSizes)
  SCAN REST WHILE ScalFld + ALLTRIM(STR(IndxDm2)) = SUBSTR(STYLE , lnScaPosGl , 2)
    IF lcHldDim == &lcTmpSizes..cDim1
      =SEEK(lcKeyRec , lcAdStyGrp)
      REPLACE &lcAdStyGrp..IndxDm2  WITH &lcTmpSizes..IndxDm2  ,;
              &lcAdStyGrp..llPrnSDc WITH &lcTmpSizes..llPrnSDc

      IF &lcTmpSizes..IndxDm2 == 1
        REPLACE &lcAdStyGrp..llPrnLin WITH .T.

        *--Update the fields of the Po and the Available date.
        IF lfOpenPo()
          REPLACE &lcAdStyGrp..Ponofolo WITH lcOpenPO ,;
                  &lcAdStyGrp..DatAvlbl WITH ldAvalbl
        ENDIF
      ELSE
        REPLACE &lcAdStyGrp..llPrnLin WITH .F.
      ENDIF

      FOR lnCrtTmp = 9 TO 16
        lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp)) + "dl"
        lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
        IF !EMPTY(&lcTmpSizes..&lcSizFld)
          REPLACE &lcAdStyGrp..&lcNumSiz WITH &lcTmpSizes..&lcSizFld
        ENDIF
      ENDFOR
    ELSE
      SELECT (lcAdStyGrp)
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO FIELDS EXCEPT Qty* , TotQty
      REPLACE &lcAdStyGrp..Account  WITH PACK_HDR.Account      ,;
              &lcAdStyGrp..FolowRec WITH SPACE(0)              ,;
              &lcAdStyGrp..lcDim1   WITH &lcTmpSizes..cDim1    ,;
              &lcAdStyGrp..IndxDm2  WITH &lcTmpSizes..IndxDm2  ,;
              &lcAdStyGrp..llPrnSDc WITH &lcTmpSizes..llPrnSDc

      IF &lcTmpSizes..IndxDm2 == 1
        REPLACE &lcAdStyGrp..llPrnLin WITH .T.

        *--Update the fields of the Po and the Available date.
        IF lfOpenPo()
          REPLACE &lcAdStyGrp..Ponofolo WITH lcOpenPO ,;
                  &lcAdStyGrp..DatAvlbl WITH ldAvalbl
        ENDIF
      ELSE
        REPLACE &lcAdStyGrp..llPrnLin WITH .F.
      ENDIF

      FOR lnCrtTmp = 1 TO 16
        lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp)) + "dl"
        lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
        IF !EMPTY(&lcTmpSizes..&lcSizFld)
          REPLACE &lcAdStyGrp..&lcNumSiz WITH &lcTmpSizes..&lcSizFld
        ENDIF
      ENDFOR
    ENDIF

    *--Section to add the new records in case the customer has to follow.      
    IF llToFollow
      SELECT (lcAdStyGrp)
      SCATTER MEMVAR MEMO
      APPEND BLANK
      GATHER MEMVAR MEMO FIELDS EXCEPT Qty* , TotQty
      REPLACE &lcAdStyGrp..Account  WITH "ZZZZZ" ,;
              &lcAdStyGrp..Cfollow  WITH .T.     ,;
              &lcAdStyGrp..FolowRec WITH "ZZZZ"

      IF lcHldDim == &lcTmpSizes..cDim1
        =SEEK("O" + PACK_HDR.ORDER + STORE + STYLE , 'ORDLINE')
        PRIVATE lcSizScl , lcAdsScl
        FOR lnScl = 1 TO 1
          lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
          FOR lnAll = 1 TO 16
            lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
              IF SCALE.&lcSizScl == &lcAdStyGrp..&lcAdsScl
                FOR lnFill = lnScl TO 8
                 lcSizScl = "Sz" + ALLTRIM(STR(lnFill))
                 lcQtyOrd = "Qty" + ALLTRIM(STR(lnFill))
                 lcPikQty = "Pik" + ALLTRIM(STR(lnFill))
                 lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
                 IF !EMPTY(SCALE.&lcSizScl) AND (ORDLINE.&lcQtyOrd - ORDLINE.&lcPikQty) # 0
                   REPLACE &lcAdStyGrp..&lcAdsScl WITH (ORDLINE.&lcQtyOrd - ORDLINE.&lcPikQty)
                 ENDIF
              ENDFOR
              EXIT
            ENDIF
          ENDFOR
        ENDFOR
        REPLACE &lcAdStyGrp..TOTQTY WITH &lcAdStyGrp..TOTQTY + (ORDLINE.TOTQTY - ORDLINE.TOTPIK)
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

SELECT ORDLINE
SET ORDER TO TAG &lcOldOrdr

lcFolVal = SUBSTR(lcKeyRec , LEN(lckeyrec) - 4 , 4 ) && lcFolVal for the cfolowrec field.

SELECT PACK_HDR
SET RELATION OFF INTO ORDLINE

SELECT (lcAdStyGrp)
lcOrdrTag = ORDER()
SET ORDER TO TAG lcFrstRec
lcKeyRec = LEFT(lcKeyRec , LEN(lckeyrec) - 5 ) + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + lcFolVal + "1"

=SEEK(lcKeyRec , lcAdStyGrp)

lcOrder = Piktkt.Order
lcOrder = ''
IF SEEK(&lclinfile..Pack_no,'PIKTKT')
  lcOrder = PIKTKT.Order
ELSE
  lcOrder = Pack_hdr.Order
ENDIF

IF SEEK('O'+lcOrder,'ORDLINE')
  SELECT ORDLINE
  LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR STYLE = &lcAdStyGrp..Style
  IF FOUND()
    REPLACE &lcAdStyGrp..Price WITH OrdLine.Price
  ENDIF
ENDIF

SELECT (lcAdStyGrp)
SET ORDER TO TAG &lcOrdrTag

SELECT (lclinfile)

SELECT (lnPrvAls)
*-- End OF lfInsertRc
*:*************************************************************
*: Name      : lfUpdgroup
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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
  =SEEK(Pack_No,'Pack_hdr')
  IF ((lcOldKey # Account + cDelivery + Store) .OR. (lcCurtpktk # Pack_No .AND. cDelivery = 'N')) ;
     AND &lcAdStyGrp..Account <> "ZZZZZ"
    lnGroupKey = lnGroupKey + 1

    *-- Update the group file
    SELECT (lcTmpGroup)
    APPEND BLANK
    REPLACE Pack_No    WITH &lcPaklntmp..Pack_No            ,;
            CGroupKey  WITH ALLTRIM(STR(lnGroupKey))        ,;
            weightdl   WITH weightdl   + Pack_hdr.weightdl  ,;
            noofcarton WITH noofcarton + Pack_hdr.noofcarton,;
            TOT_PCS    WITH TOT_PCS    + Pack_hdr.TOT_PCS   ,;
             DPrintDate WITH Pack_hdr.dShipdate             ,;
             consgment  WITH Pack_hdr.consgment

    SELECT (lcPaklntmp)
  ENDIF
  REPLACE CGroupKey WITH ALLTRIM(STR(lnGroupKey))

  IF lfRemToflw()
    REPLACE &lcTmpGroup..llRemantTo WITH .T.
  ENDIF

  IF (lcOldPiktk # Pack_No .AND. lnOldGroup = lnGroupKey) .OR. EOF(lcTmpGroup)
    IF EOF(lcTmpGroup)
      SELECT (lcTmpGroup)
      APPEN BLANK
      REPLACE Pack_No    WITH &lcPaklntmp..Pack_No             ,;
              CGroupKey  WITH ALLTRIM(STR(lnGroupKey))         ,;
              weightdl   WITH weightdl   + Pack_hdr.weightdl   ,;
              noofcarton WITH noofcarton + Pack_hdr.noofcarton ,;
              TOT_PCS    WITH TOT_PCS    + Pack_hdr.TOT_PCS    ,;
              DPrintDate WITH Pack_hdr.dShipdate               ,;
              consgment  WITH Pack_hdr.consgment

      SELECT(lcPaklntmp)
    ELSE
      REPLACE &lcTmpGroup..Pack_No    WITH '******',;
              &lcTmpGroup..weightdl   WITH &lcTmpGroup..weightdl   + Pack_hdr.weightdl  ,;
              &lcTmpGroup..noofcarton WITH &lcTmpGroup..noofcarton + Pack_hdr.noofcarton,;
              &lcTmpGroup..TOT_PCS    WITH &lcTmpGroup..TOT_PCS    + Pack_hdr.TOT_PCS
    ENDIF

    *-- Function to Check if there are outstanding items on sales order print Remains to follow
    IF lfRemToflw()
      REPLACE &lcTmpGroup..llRemantTo WITH .T.
    ENDIF
  ENDIF

  lcOldKey   = Account + cDelivery + Store
  STORE Pack_No To  lcOldPiktk , lcCurtpktk
  lnOldGroup = lnGroupKey

  IF llRpPrnNot .AND. Pack_No # lcPack_No  ;
     AND SEEK(lcPack_No,'Pack_hdr')        ;
     ANDSEEK("B"+Pack_hdr.Order,'NOTEPAD')

    lcPack_No = Pack_No
    SKIP - 1
    REPLACE llSmPad WITH .T.            ,;
            NoteSm  WITH NOTEPAD.MNOTES
    SKIP
  ENDIF
ENDSCAN

SELECT (lnPrvAls)
*-- End OF lfUpdgroup.
*:*************************************************************
*: Name      : lfUpdatPrt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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
*: Example     : =lfUpdatPrt()
*:*************************************************************
FUNCTION lfUpdatPrt

*--If the Device is not Screen
IF gcDevice <> 'SCREEN'
  IF !SEEK(Pack_no,lcPckPrtUp)
    INSERT INTO (lcPckPrtUp) (Pack_No) VALUES (Pack_hdr.Pack_No)
  ENDIF
ENDIF

*-- End OF lfUpdatPrt.
*:*************************************************************
*: Name      : lfRemToflw
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*: Purpose   : Function to Check if there are outstanding 
*:           : Items on sales order print Remains to follow
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
FUNCTION lfRemToflw
PRIVATE lnAlias , llRemains

lnAlias = SELECT (0)
llRemains = .F.

IF SEEK('O'+PACK_Hdr.Order,'OrdHdr')
  IF Ordhdr.Open+Ordhdr.Ship > PACK_Hdr.Tot_pcs
    llRemains = .T.
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN llRemains

*-- End OF lfRemToflw.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*:*************************************************************
*: Name      : lfOpenPo
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*: Purpose   : Function to check for the open PO.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfOpenPo()
*:*************************************************************
FUNCTION lfOpenPo
PRIVATE lcAlasPo , lcEvalKyPo , lcStyCheck

STORE SPACE(0) TO lcOpenPO , lcStyCheck
STORE {} TO ldAvalbl
STORE .F. TO llRtrnVl

lcAlasPo = SELECT(0)
SELECT POSLN
lcEvalKyPo = EVAL(KEY())

=SEEK(&lcAdStyGrp..STYLE)
SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = &lcAdStyGrp..STYLE FOR POSHDR.STATUS = "O"
  lcStyCheck = POSLN.STYLE
  SKIP
  IF lcStyCheck == POSLN.STYLE
    IF TRANCD = ALLTRIM(STR(1))
  	  SKIP - 1
    ENDIF
  ELSE
    SKIP - 1
    lcOpenPO = POSHDR.PO
    ldAvalbl = POSHDR.AVAILABLE + 5
    llRtrnVl = .T.
    EXIT
  ENDIF
ENDSCAN

=SEEK(lcEvalKyPo)
SELECT(lcAlasPo)

RETURN llRtrnVl
*--End of lfOpenPo.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
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

=lfAddField("laTmpStru", "StyGrop"  , "C" ,6  ,0) &&Field hold the style group data.
=lfAddField("laTmpStru", "llSmPad"  , "L" ,1  ,0) &&Logic field to check if print the notepad or not.
=lfAddField("laTmpStru", "NoteSm"   , "M" ,10 ,0) &&Field hold the SM Template notepad.
=lfAddField("laTmpStru", "StylDesc" , "C" ,60 ,0) &&Field hold the SM Template notepad.
=lfAddField("laTmpStru", "ScalDL"   , "C" ,3  ,0) &&Section for fields scale and sizes.

FOR lnCrtTmp = 1 TO 16
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("laTmpStru", "Sz"  + lcNumSiz + "DL", "C" , 5 ,0)
ENDFOR

FOR lnCrtTmp = 1 TO 8
  lcNumQty = ALLTRIM(STR(lnCrtTmp+8))
  =lfAddField("laTmpStru", "Qty"  + lcNumQty , "N" , 6 ,0)
ENDFOR

=lfAddField("laTmpStru", "llPrnScal" , "L" ,1 ,0)
=lfAddField("laTmpStru", "Price"     , "N" ,7 ,2)
=lfAddField("laTmpStru", "ACCOUNT"   , "C" ,5 ,0)
=lfAddField("laTmpStru", "STORE"     , "C" ,8 ,0)
=lfAddField("laTmpStru", "CDelivery" , "C" ,1 ,0)
=lfAddField("laTmpStru", "CGroupKey" , "C" ,6 ,0)
=lfAddField("laTmpStru", "Carrier"   , "C" ,40,0)
=lfAddField("laTmpStru", "llPrnSDc"  , "L" ,1 ,0)      && this flag to use it to print the style description in the last record.
=lfAddField("laTmpStru", "lcDim1"    , "C" ,5 ,0)      && this field used to hold the 2nd dimension.
=lfAddField("laTmpStru", "IndxDm2"   , "N" ,3 ,0)      && this field used to sort the 2nd dimension.
=lfAddField("laTmpStru", "llPrnLin"  , "L" ,1 ,0)      && this flag to use it to non print the data beside the 2nd dim.
=lfAddField("laTmpStru", "Cfollow"   , "L" ,1 ,0)      && this flag to use it in case the customer has to follow.
=lfAddField("laTmpStru", "FolowRec"  , "C" ,4 ,0)      && this field to use it in case print records for customer has to follow.
=lfAddField("laTmpStru", "PrnToFol"  , "L" ,1 ,0)      && this flag to use it in print To Follow Text.
=lfAddField("laTmpStru", "Ponofolo"  , "C" ,6 ,0)      && this field to use it in print the Po Number in the ToFollow section.
=lfAddField("laTmpStru", "DatAvlbl"  , "D" ,8 ,0)      && this field to use it in print the available date in the ToFollow section.
=lfAddField("laTmpStru", "llPrnNtPd" , "L" ,1 ,0)      && this flag to use it to print the notepad at the end of the Pack no.

CREATE TABLE (gcWorkDir + lcAdStyGrp) FROM ARRAY laTmpStru
INDEX ON PACK_NO + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) TAG lcStyFond ADDITIVE
INDEX ON PACK_NO + Account + Store + cGroupkey  + CDelivery + ALLTRIM(STR(Line_No)) + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + FolowRec + ALLTRIM(STR(IndxDm2)) TAG lcFrstRec ADDITIVE
INDEX ON PACK_NO + Account + Store + cGroupkey  + CDelivery + ALLTRIM(STR(Line_No)) + FolowRec + ALLTRIM(STR(IndxDm2)) TAG lcAdStyGrp ADDITIVE

CREATE TABLE (gcWorkDir + lcTmpGroup) ( CGroupKey C(6) , PACK_NO C(6) , weightdl N(13,2) , noofcarton N(8) ,;
                                          TOT_PCS N(8) , DPrintDate D(8), consgment C(20) , llRemantTo L(1))
INDEX ON CGroupKey Tag (lcTmpGroup)

*--File create the scale and sizes.
CREATE TABLE (gcWorkDir + lcTmpSizes) ( ScalFld C(2) , IndxDm2 N(3) , cDim1 C(5) , llPrnSDc L ,;
                                        Sz1 C(5) , Sz2 C(5) , Sz3 C(5) , Sz4 C(5) , Sz5 C(5) , Sz6 C(5) , Sz7 C(5) , Sz8 C(5) ,;
                                        Sz9 C(5) , Sz10 C(5) , Sz11 C(5) , Sz12 C(5) , Sz13 C(5) , Sz14 C(5) , Sz15 C(5) , Sz16 C(5))

INDEX ON ScalFld + ALLTRIM(STR(IndxDm2)) Tag SortScal
INDEX ON ScalFld + cDim1 Tag (lcTmpSizes) ADDITIVE

*--End of lfCreatTmp.
*:*************************************************************
*: Name      : lfnsrtfnd
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*: Purpose   : Function to update record found.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfnsrtfnd()
*:*************************************************************
FUNCTION lfnsrtfnd

SELECT (lclinfile)
lcScalVal = IIF(SEEK("S" + SUBSTR(STYLE , lnScaPosGl , lnScaLnGl) , 'SCALE') , SCALE.SCALE , "")
lcCdm1Val = SCALE.CDIM1

SELECT (lcAdStyGrp)
lcOldOrdr = ORDER()
SET ORDER TO TAG lcStyFond
LOCATE
=SEEK(EVAL(lclinfile+'.PACK_NO') + SUBSTR(EVAL(lclinfile+'.STYLE'),lnStyPosGl,lnStyLnGl))
SET ORDER TO TAG &lcOldOrdr
SET ORDER TO TAG lcAdStyGrp
=SEEK(EVAL(lclinfile+'.PACK_NO') + Account + Store + cGroupkey  + CDelivery)
lcFullIndex = [ PACK_NO + Account + Store + cGroupkey  + CDelivery + ALLTRIM(STR(Line_No)) + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + ALLTRIM(STR(IndxDm2))]

SCAN REST WHILE lcFullIndex = "" FOR PACK_NO == EVAL(lclinfile+'.PACK_NO')
  IF SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnStyPosGl,lnStyLnGl) AND LCDIM1 == lcCdm1Val ;
    AND &lcAdStyGrp..Account <> "ZZZZZ"
    FOR lnScl = 1 TO 1
      lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
      FOR lnAll = 1 TO 16
        lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
        IF SCALE.&lcSizScl == &lcAdStyGrp..&lcAdsScl
          FOR lnFill = lnScl TO 8
            lcSizScl = "Qty" + ALLTRIM(STR(lnFill))
            lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
            IF &lclinfile..&lcSizScl # 0
*              REPLACE &lcAdStyGrp..&lcAdsScl WITH &lclinfile..&lcSizScl
              REPLACE &lcAdStyGrp..&lcAdsScl WITH &lcAdStyGrp..&lcAdsScl + &lclinfile..&lcSizScl
            ENDIF
          ENDFOR
          EXIT
        ENDIF
      ENDFOR
    ENDFOR
    REPLACE &lcAdStyGrp..Totqty    WITH &lcAdStyGrp..Totqty + &lclinfile..Totqty ;
		    &lcAdStyGrp..line_No   WITH &lclinfile..Line_No                      ;
		    &lcAdStyGrp..llPrnScal WITH .T.                                      ;
		    &lcAdStyGrp..llPrnLin  WITH .T.
  ENDIF

  *--Section to add the new records in case the customer has to follow.
  IF llToFollow AND SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnStyPosGl,lnStyLnGl) ;
                AND LCDIM1 == lcCdm1Val                 ;
                AND &lcAdStyGrp..Account == "ZZZZZ"     ;
                AND &lcAdStyGrp..Cfollow                ;
                AND &lcAdStyGrp..FolowRec == "ZZZZ"

    =SEEK("O" + PACK_HDR.ORDER + STORE + &lclinfile..style , 'ORDLINE')
    IF lfOpenPo()
      REPLACE &lcAdStyGrp..Ponofolo WITH lcOpenPO ,;
              &lcAdStyGrp..DatAvlbl WITH ldAvalbl
    ENDIF

    PRIVATE lcSizScl , lcAdsScl
    FOR lnScl = 1 TO 1
      lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
      FOR lnAll = 1 TO 16
        lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
        IF SCALE.&lcSizScl == &lcAdStyGrp..&lcAdsScl
          FOR lnFill = lnScl TO 8
            lcSizScl = "Sz" + ALLTRIM(STR(lnFill))
            lcQtyOrd = "Qty" + ALLTRIM(STR(lnFill))
            lcPikQty = "Pik" + ALLTRIM(STR(lnFill))
            lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
            IF !EMPTY(SCALE.&lcSizScl) AND (ORDLINE.&lcQtyOrd - ORDLINE.&lcPikQty) # 0
*              REPLACE &lcAdStyGrp..&lcAdsScl WITH (ORDLINE.&lcQtyOrd - ORDLINE.&lcPikQty)
              REPLACE &lcAdStyGrp..&lcAdsScl WITH &lcAdStyGrp..&lcAdsScl + (ORDLINE.&lcQtyOrd - ORDLINE.&lcPikQty)
            ENDIF
          ENDFOR
          EXIT
        ENDIF
      ENDFOR
    ENDFOR
    REPLACE &lcAdStyGrp..TOTQTY WITH &lcAdStyGrp..TOTQTY + (ORDLINE.TOTQTY - ORDLINE.TOTPIK)
  ENDIF
ENDSCAN

*--End of lfnsrtfnd.
*:*************************************************************
*: Name      : lfAdMsdRec
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 2/06/2005
*: Purpose   : Function to add the ordlines records.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfAdMsdRec()
*:*************************************************************
FUNCTION lfAdMsdRec
PRIVATE lcAlias , lcPoChk

lcAlias = SELECT(0)
SELECT (lclinfile)

STORE SPACE(0) TO lcPoChk
SCAN
  lcPack_No = &lclinfile..Pack_No
  IF lcPoChk # PACK_NO
    lcKeyMain = EVAL(KEY())
    =SEEK(&lclinfile..PACK_NO , 'PACK_HDR')
    IF SEEK("O" + PACK_HDR.ORDER , 'ORDLINE')
      SELECT ORDLINE
      SCAN REST WHILE CORDTYPE + ORDER + STR(LINENO,6) = "O" + PACK_HDR.ORDER
        IF EMPTY(ORDLINE.PIKTKT)
          SCATTER MEMVAR MEMO
	      SELECT (lclinfile)
	      APPEND BLANK
	      GATHER MEMVAR FIELDS LIKE Qty* , TotQty , Style
	      REPLACE Pack_no WITH lcPack_No ;
	              COWNER  WITH "BBBBVVVVDDDDRRRR"
	      SELECT ORDLINE
        ENDIF
      ENDSCAN
      SELECT (lclinfile)
      =SEEK(lcKeyMain)
    ENDIF
  ENDIF
  lcPoChk = PACK_HDR.PACK_NO
ENDSCAN

SELECT(lcAlias)
*--End of lfAdMsdRec.