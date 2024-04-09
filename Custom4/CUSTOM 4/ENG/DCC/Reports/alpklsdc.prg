*:***************************************************************************
*: Program file  : ALPKLSDC.PRG
*: Program desc. : CUSTOMIZED PACKING LIST Form FOR DCC.
*: Date          : 1/25/2009
*: System        : Aria Advantage Series.4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar (MMT)
*: Tracking Job Number: (C201098) {T20080806.0006}
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
*:C201098,2 MMT 02/10/2009 Fix bugs of not prinitng to Follow[T20080806.0006]
*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [T20080806.0006]
*:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[T20080806.0006]
*:B608866,2 MMT 10/13/2009 Fix bugs of getting to Follow on Store Level [T20080806.0006]
*:C201207,1 HES 12/08/2009 please quote for changes to Pack List [T20091027.0011]
*:C201207,2 HES 12/27/2009 Handle scenario of packlist with shipped Order [T20091027.0011]
*:C201207,3 HES 12/27/2009 Add new note for Delivery1 Notebad template code [T20091027.0011]
*:B609323,1 WAM 06/29/2010 Fix bug to get the sizes for the proper scale dimension [T20100624.0006]
*!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[T20100914.0023]
*:B609835,1 MMT 02/20/2012 Custom Packing list form DC Takes time to preview or print[T20120118.0002]
*!B610326,1 TMI 05/03/2013 fix a problem that qty is duplicated in the packlist 682116 [T20130415.0032] 
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007]
*!B610828,1 MMT 08/31/2014 Fix the bug of wrong line qty if it is exist for the same employee more than once[T20140827.0004]
*!B610936,1 MMT 01/22/2015 Custom Packing list DC does not show allocated line '***"  as to follow[T20150116.0004]
*!B611014,1 MMT 06/09/2015 Custom packing list form Prints incorrect employee name[T20150605.0002] 
*!B611441,1 MMT 10/16/2017 Custom Packing list form does not print all to follow lines[T20171013.0004]
*****************************************************************************
*-SAB ----- [Start]
*SET STEP ON
*!*	IF TYPE('lcXMLFileName') = 'C'
*!*	  PRIVATE loAgent
*!*	  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
*!*	 
*!*	  PRIVATE loProgress
*!*	  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
*!*	  loProgress.Percent = 0
*!*	  loProgress.DESCRIPTION = "Opening Data Files..."
*!*	  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
*!*	 
*!*	  LOCAL loEnvironment
*!*	  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*!*	  loEnvironment.ClientId = ClientId
*!*	 
*!*	  LOCAL lcCurrentProcedure
*!*	  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
*!*	  loEnvironment.ConnectionsRefresh()
*!*	 
*!*	  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
*!*	  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
*!*	  lcClientRoot = loEnvironment.Aria40SharedPath
*!*	  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)

*!*	  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
*!*	  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
*!*	  lcActiveMod = 'AL'
*!*	  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
*!*	  oAriaEnvironment.activeModuleID = 'AL'
*!*	  oAriaEnvironment.RequestID = lcRequestID
*!*	  PUBLIC gcAct_Appl
*!*	  gcAct_Appl = lcActiveMod
*!*	  IF LEFT(gcDevice, 7) = "PRINTER"
*!*	    oAriaEnvironment.gcDevice = "PRINTER"
*!*	  ELSE
*!*	    oAriaEnvironment.gcDevice = "FILE"
*!*	  ENDIF
*!*	  oAriaEnvironment.Report.cCROrientation = 'P' 
*!*	ELSE
*!*	  loOGScroll.cCROrientation = 'P'
*!*	ENDIF
*-SAB ----- [End]
*--Initial the variables used in the program.
*:C201207,3 HES 12/27/2009 Add new note for Delivery1 Notebad template code [Start]
*!*	STORE SPACE(0) TO lcDelNot1 , lcDelNot2
STORE SPACE(0) TO lcDelNot1 , lcDelNot2, lcDelNot3, lcDelNot4
*:C201207,3 HES 12/27/2009 Add new note for Delivery1 Notebad template code [End]

STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
=lfChkStrct() && Get the length of the style , color and scale.

llPrntMsg = .F.

*--In case the user select YES in "Product Group Sequence".
lcEmail  = gfGetMemVar('M_CEMAIL' , oAriaApplication.ActiveCompanyID)
llMScale = gfGetMemVar('M_USEEXSSC')

                       *-- Open the files used in the Report--*
*--EMPLOYEE file to print the Ucode and name.
IF !USED('contact')
  =gfOpenTable('contact','contact','SH')
ENDIF

IF !USED('POSHDR')
  =gfOpenTable('POSHDR','POSHDR','SH')
ENDIF

IF !USED('POSLN')
  =gfOpenTable('POSLN','POSLNS','SH')
ELSE
  SELECT POSLN
  =gfSetOrder('POSLNS')
ENDIF

IF !USED('PIKTKT')
  =gfOpenTable('PIKTKT','PIKTKT','SH')
ENDIF 
IF !USED('INVHDR')
  =gfOpenTable('INVHDR','INVHDR','SH')
ENDIF 

                       *-- End Open the files used in the Report--*
*-- Get company Phone.
PRIVATE lcAlasPhon , lcPhonUser
lcAlasPhon = SELECT(0)
SELECT (lcCompInfo)
lcPhonComp = cCom_Phon             && Company Phone.
*MT
*=gfOPenTable('SYUUSER','CUSER_ID',"SH")
=gfOPenTable('SYUUSER','CUSER_ID',"SH",'SYUUSER_DC')
*MT
*--Get User Phone.
*MT
*SELECT SYUUSER
SELECT SYUUSER_DC
*MT
gfSEEK(oAriaApplication.User_ID)
lcPhonUser = CUSR_PHON

*-SAB ----- [Start]
IF TYPE('lcXMLFileName') = 'C' && David
	oAriaEnvironment.User_Name = cUsr_Name
endif && David
*-SAB ----- [End]

SELECT(lcAlasPhon)
*--End getting values.

DIMENSION laFileStruct[1,4]
laFileStruct[1,1] = 'Pack_No'
laFileStruct[1,2] = 'C'
laFileStruct[1,3] = 6
laFileStruct[1,4] = 0
*--Define new table to hold the printed packing list.
=gfCrtTmp(lcPckPrtUp,@laFileStruct,"Pack_No",'Invoice',.T.)

*:C201098,2 MMT 02/10/2009 Fix bugs of not prinitng to Follow[Start]
lfGetAllOrdl()
*:C201098,2 MMT 02/10/2009 Fix bugs of not prinitng to Follow[End]

*-- This Code to handle the new way of collecting data.
=lfPrtPack()

*-- update the pack header file with the printing date if empty.
SELECT (lcPckPrtUp)
SCAN
  IF loPack_Hdr.SEEK(EVAL(lcPckPrtUp+'.Pack_No')) .AND. Empty(&lcTempPack_Hdr..dShipdate)
    loPack_Hdr.REPLACE("DSHIPDATE WITH oAriaApplication.SystemDate")
  ENDIF
ENDSCAN
loPack_Hdr.TableUpdate()
USE IN (lcPckPrtUp)

*--Function to clear the Temp. file.
=lfBasToClr(lcAdStyGrp , 'F')
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
      IF SEEK("S"+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcScaleFile)
        lcTemp = ALLTRIM(&lcScaleFile..cScl_desc)
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
SELECT (lcOrdLnTmp)
lnRcNoOrd = RECNO()
lcOrdrLin = ORDER()
SET ORDER TO (lcOrdLnTmp)
lcOrdrSek = EVAL(lcPackTmp+'.ORDER')
LOCATE

lcLnNoFld = STR(EVAl(lcLinFile+'.nordlineno'),6)

IF SEEK("O" + lcOrdrSek + lcLnNoFld) AND !EMPTY(&lcOrdLnTmp..PACK_ID)
    STORE SPACE(0) TO laSpckTemp[1]
    laSpckTemp[1] = "Pack ID : " + ALLTRIM(&lcOrdLnTmp..PACK_ID)
ELSE
  IF loSpck_Lin.SEEK( 'S' + &lcPackTmp..ACCOUNT + &lcLinFile..Style)
    IF &lcTempSpck_Lin..TotQty = 0
      laSpckTemp[1] ='SKU #:' + &lcTempSpck_Lin..Pack_Id
    ELSE
      lnCount = 1
      SELECT (lcTempSpck_Lin)
      lnSavRec = RECNO()
      FOR lnCount = 1 To 8
        GOTO lnSavRec
        lcCount = STR(lnCount, 1 )
        IF  !EMPTY(EVAL(lcLinFile+'.Qty'+lcCount))
          SCAN REST WHILE Type+Account+Style+Pack_id = 'S' + &lcPackTmp..Account+&lcLinFile..Style;
            FOR lnCount <= 8
              IF !EMPTY(&lcTempSpck_Lin..Qty&lcCount)
                laTemp[lnCount] =IIF(EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)),'',&lcScaleFile..Sz&lcCount + ':' + &lcTempSpck_Lin..Pack_Id)
              ENDIF
          ENDSCAN
        ENDIF
      ENDFOR
    ENDIF
  ENDIF

SELECT (lcOrdLnTmp)
IF BETWEEN(lnRcNoOrd,1,RECCOUNT(lcOrdLnTmp))
  GOTO lnRcNoOrd IN (lcOrdLnTmp)
ENDIF
SET ORDER TO (lcOrdrLin)
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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

*:C201207,3 HES 12/27/2009 Add new note for Delivery1 Notebad template code [Start]
*!*	STORE SPACE(0) TO lcDelNot1 , lcDelNot2
STORE SPACE(0) TO lcDelNot1 , lcDelNot2, lcDelNot3, lcDelNot4
*:C201207,3 HES 12/27/2009 Add new note for Delivery1 Notebad template code [End]

PRIVATE lcAlasDelv
lcAlasDelv = SELECT(0)
lnMemoSet = SET('MEMOWIDTH')
SET MEMOWIDTH TO 100


IF loNotePad.SEEK("T" + "DELIVERY" )
  lcDelNot1 = MLINE(&lcTempNotePad..MNOTES,1)
  lcDelNot2 = MLINE(&lcTempNotePad..MNOTES,2)
ENDIF

*:C201207,3 HES 12/27/2009 Add new note for Delivery1 Notebad template code [Start]
IF loNotePad.SEEK("T" + "DELIVERY1" )
  lcDelNot3 = MLINE(&lcTempNotePad..MNOTES,1)
  lcDelNot4 = MLINE(&lcTempNotePad..MNOTES,2)
ENDIF
*:C201207,3 HES 12/27/2009 Add new note for Delivery1 Notebad template code [End]

SELECT(lcAlasDelv)
SET MEMOWIDTH TO lnMemoSet
RETURN ''

*--End of lfDelNote.
*:*************************************************************
*: Name      : lfPrtPack
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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

*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
STORE 0 TO lnVldlnNo,lnNewLn
*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]

lcOldPktmp = lcpaklntmp

SELECT POSLN
*!*	IF !('POSLN.PO INTO POSHDR' $ SET('RELATION'))
*!*	  SET RELATION TO "P" + POSLN.PO INTO POSHDR ADDITIVE
*!*	ENDIF

SELECT (lcPackTmp)
SET RELATION TO

*--To be able to print the style descreption I use the same name.
=lfCreatTmp()

*--Section to add the ordlines records that doesn't exsit in the packlines file 
*--to add it in section To Follow in the report.
=lfAdMsdRec()

SELECT (lclinfile)
LOCATE
lcEvlKyDl = EVAL(KEY())

SCAN

  loPack_hdr.Seek(Pack_no)
  
  *:B608866,2 MMT 10/13/2009 Fix bugs of getting to Follow on Store Level [Start]
  =SEEK('M' + &lcTempPack_Hdr..Account,lcCUSTOMER)  
  llToFollow = &lcCUSTOMER..CTOFOLLOW
  *:B608866,2 MMT 10/13/2009 Fix bugs of getting to Follow on Store Level [End]
  
  =SEEK(IIF(EMPTY(&lcTempPack_Hdr..Store) , 'M' + &lcTempPack_Hdr..Account ,'S' + &lcTempPack_Hdr..Account + &lcTempPack_Hdr..Store),lcCUSTOMER)
  *-SAB ----- [Start]
  *WAIT WINDOW 'Selecting Records For The Report ...' + PACK_NO NOWAIT
  *tmi 15/5/2014
  *IF TYPE('lcXMLFileName') <> 'O'
  IF TYPE('lcXMLFileName') <> 'C'
  *tmi 
    WAIT WINDOW 'Selecting Records For The Report ...' + PACK_NO NOWAIT
  ENDIF
  *-SAB ----- [End]
  
  *:B608866,2 MMT 10/13/2009 Fix bugs of getting to Follow on Store Level [Start]
  *llToFollow = &lcCUSTOMER..CTOFOLLOW
  *:B608866,2 MMT 10/13/2009 Fix bugs of getting to Follow on Store Level [End]
  
  lcValStClr = SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + "-" + SUBSTR(STYLE,lnClrPosGl,lnClrLnGl)
  
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
  lnNewLn = nOrdLineno
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
  
  SCATTER MEMVAR MEMO
  
  lcKey = SUBSTR(STYLE , lnScaPosGl , 2)
  IF !(lcKey $ lcHldScal)
    =lfGetSizes()
    lcHldScal = lcHldScal + IIF(EMPTY(lcHldScal) , "" , "," ) + lcKey
  ENDIF


  *TMI [B610326,4 [START] do not include lines with 0 qty
  IF !EMPT(&lclinfile..COWNER) and &lclinfile..TOTQTY=0 
    LOOP
  ENDIF
  *TMI [B610326,4 [END  ]

  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
  *IF lcStyClr # lcValStClr OR lcNewPack # PACK_NO
  IF lcStyClr # lcValStClr OR lcNewPack # PACK_NO OR lnVldlnNo # lnNewLn
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
    
    =lfInsertRc()
    
  ELSE
    =lfnsrtfnd()
  ENDIF
  lcStyClr = lcValStClr
  lnVldlnNo  = lnNewLn
ENDSCAN

*--Section to evaluate the logic field to print the scale.
SELECT (lcAdStyGrp)
LOCATE


*--Assign the name of the new indexed file to the data files.
lcpaklntmp = lcAdStyGrp

*-- Function to update the grouping key fields.
=lfUpdgroup()

*!*	SELECT (lcPackTmp)
*!*	SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO CUSTOMER ,;
*!*	                PACK_NO               INTO PACK_HDR,;
*!*	                INVOICE               INTO INVHDR  ,;
*!*	                PikTkt			      INTO PIKTKT  ,;
*!*	                "O" + ORDER           INTO ORDHDR  ,;
*!*	                "O" + ORDER + STORE   INTO ORDLINE
SELECT (lcPackTmp)
SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO &lcCustomer ,;
                                      "O" + ORDER + STORE     INTO &lcOrdLnTmp,;
                                      "B" + ORDER             INTO &lcNotePad  ,;  
                                      "O" + ORDER             INTO &lcOrdHdr  ,;
                                      invoice                 INTO &lcInvLnTmp
                                      *,;
				 	                  INVOICE               INTO INVHDR  ,;
									  PikTkt			      INTO PIKTKT  			 ADDITIVE   
SELECT (lcpaklntmp)
*                                      PACK_NO                 INTO (lcPakLnTmp)            
SET RELATION TO  PACK_NO INTO (lcPackTmp)
DELETE ALL FOR TOTQTY = 0
LOCATE
DELETE ALL FOR EMPTY(FolowRec) AND LINE_NO = 0

*:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[Start]
llStrtToF = .F.
*:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[End]
STORE SPACE(0) TO lcPoChk , lcPoChk2 , lcNewPckNo , lcChngSty , lcAccPrnDs
SCAN
  
  IF lcPoChk2 # PACK_NO AND !EMPTY(lcPoChk2) AND llRpPrnNot
    SKIP -1
    REPLACE &lcpaklntmp..llPrnNtPd WITH .T.
    SKIP 1
  ENDIF

  *:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[Start]
  IF lcPoChk2 <> PACK_NO
    llStrtToF = .F.
  ENDIF   
  *:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[End]

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
  
  
  *C201098,1 MMT 02/08/2008 Convert Packing List Form of DCC to Aria4[Start]
  
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
*  IF loPack_Hdr.SEEK(&lcpaklntmp..Pack_no) AND SEEK("O" + &lcTempPack_Hdr..ORDER + STORE + STYLE , lcOrdLnTmp)
  
  *:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[Start]
  *IF loPack_Hdr.SEEK(&lcpaklntmp..Pack_no) AND SEEK("O" + &lcTempPack_Hdr..ORDER + STORE + STYLE+STR(nOrdLineNo,6) , lcOrdLnTmp)
  IF loPack_Hdr.SEEK(&lcpaklntmp..Pack_no) AND SEEK("O" + &lcTempPack_Hdr..ORDER + STR(nOrdLineNo,6) , lcOrdLnTmp,lcOrdLnTmp)
  *:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[End]
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
  
    REPLACE cpackcolor WITH &lcOrdLnTmp..Employee
    
    
    
  
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
  *:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[Start]
  *IF &lcAdStyGrp..Cfollow   
  *    REPLACE cpackcolor WITH "ZZZZZZZZZZ" 
  *ENDIF 
   *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[Start]
   *   IF loStyle.Seek(&lcOrdLnTmp..Style)
   IF loStyle.Seek( &lcpaklntmp..Style)
   *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[End]
      REPLACE StylDesc WITH &lcTempStyle..Desc1,;
              llPrnSDc WITH .T.
    ENDIF   

  *:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[End]
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
  
  ENDIF 

  *C201098,1 MMT 02/08/2008 Convert Packing List Form of DCC to Aria4[End]
  
  
  IF lcPoChk # PACK_NO AND ACCOUNT = "ZZZZZ"
    REPLACE &lcpaklntmp..PrnToFol WITH .T.
  ENDIF

  IF ACCOUNT = "ZZZZZ" AND TOTQTY > 0 
    REPLACE &lcpaklntmp..llPrnLin WITH .T.
  ENDIF

  *:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[Start]
  IF &lcpaklntmp..PrnToFol   
    llStrtToF = &lcpaklntmp..PrnToFol  
  ENDIF    
  IF llStrtToF AND &lcAdStyGrp..Cfollow   
    *:C201207,1 HES 12/08/2009 please quote for changes to Pack List [Start] && Just to print a to follow per employee
      *REPLACE cpackcolor WITH "ZZZZZZZZZZ" 
    *:C201207,1 HES 12/08/2009 please quote for changes to Pack List [End]
  ENDIF 
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]

  IF llRpPrnNot .AND. Pack_No # lcPoChk2    ;
     AND loPack_Hdr.SEEK(lcPoChk2)          ;
     AND loNotePad.SEEK("B"+&lcTempPack_Hdr..Order)

    SKIP - 1
    REPLACE llSmPad WITH .T.            ,;
            NoteSm  WITH &lcTempNotePad..MNOTES
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

*--Section change the value of the llPrnSDc to .T. in case value = .F.
GOTO BOTTOM
IF !&lcpaklntmp..llPrnSDc
  REPLACE &lcpaklntmp..llPrnSDc WITH .T.
ENDIF
  
IF llRpPrnNot
  GOTO BOTTOM
  IF llRpPrnNot .AND. loPack_Hdr.SEEK(Pack_No) .AND. loNotePad.SEEK("B"+&lcTempPack_Hdr..Order)
    REPLACE llSmPad WITH .T.            ,;
          NoteSm  WITH ALLTRIM(&lcTempNotePad..MNOTES)
  ENDIF
  REPLACE &lcpaklntmp..llPrnNtPd WITH .T.
ENDIF
LOCATE

loogScroll.cCROrientation = 'P'

*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]

*:C201207,1 HES 12/08/2009 please quote for changes to Pack List [Start] && change the index to print a to follow per employee
*!*	INDEX ON PACK_NO + Account + Store + cpackcolor+ cGroupkey  + CDelivery + ALLTRIM(STR(Line_No))+ SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + FolowRec + ALLTRIM(STR(IndxDm2)) TAG 'EmpIndx' ADDITIVE
INDEX ON PACK_NO + cpackcolor + FolowRec + cGroupkey  + CDelivery + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + ALLTRIM(STR(Line_No))+ ALLTRIM(STR(IndxDm2))  TAG 'EmpIndx' ADDITIVE
*:C201207,1 HES 12/08/2009 please quote for changes to Pack List [End]

SET ORDER TO EmpIndx

lcDltSet = SET("Deleted")

*:C201207,1 HES 12/08/2009 please quote for changes to Pack List [Start]
*!*	SET DELETED OFF
SET DELETED ON 
*:C201207,1 HES 12/08/2009 please quote for changes to Pack List [End]

*:C201207,1 HES 12/08/2009 please quote for changes to Pack List [Start]
*!*	lclcDim1 = SPACE(5)
*!*	lcStyFlw = SPACE(19)
*!*	lcPackNum = SPACE(6)
*!*	lnRecNumber = 0
*!*	*!*	SCAN FOR Cfollow AND TOTQTY > 0 AND cpackcolor = "ZZZZZZZZZZ"  AND !DELETED()
*!*	  IF (Style <> lcStyFlw  AND IIF(!EMPTY(lcDim1),lcDim1 <> lclcDim1 ,.T.)) OR PACK_NO <> lcPackNum  
*!*	    lclcDim1  = lcDim1 
*!*	    lnRecNumber = RECNO()
*!*	    lcStyFlw = Style 
*!*	    lcPackNum = PACK_NO
*!*	    STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12,lnQty13,lnQty14,lnQty15,lnQty16,lnTotQty
*!*	    SCAN FOR Cfollow AND Style = lcStyFlw AND TOTQTY > 0 AND;
*!*	    		 cpackcolor = "ZZZZZZZZZZ"  AND lcDim1 = lclcDim1 AND PACK_NO = lcPackNum AND !DELETED() 		 
*!*	       FOR lnT = 1 TO 16
*!*	         lcT = ALLTRIM(STR(lnT))
*!*	         lnQty&lcT = lnQty&lcT + Qty&lcT.
*!*	         lnTotQty = lnTotQty + Qty&lcT.
*!*	       ENDFOR  
*!*	       IF RECNO() <> lnRecNumber
*!*	         DELETE 
*!*	       ENDIF        
*!*	    ENDSCAN 
*!*	    IF BETWEEN(lnRecNumber,1,RECCOUNT())
*!*	      GO lnRecNumber 
*!*		  FOR lnT = 1 TO 16
*!*	    	lcT = ALLTRIM(STR(lnT))
*!*		    REPLACE Qty&lcT. WITH lnQty&lcT
*!*	  	  ENDFOR    
*!*		  REPLACE TOTQTY  WITH lnTotQty 
*!*		ENDIF   
*!*	  ENDIF 
*!*	ENDSCAN 
*!*	SET DELETED &lcDltSet
 
lclcDim1 = SPACE(5)
lcStyFlw = SPACE(lnScaPosGl-1)
lcPackNum = SPACE(6)
lnRecNumber = 0
&& Combining the Sizes in the related scale into one line (max 16 sizes) to be printed
SCAN FOR Cfollow AND TOTQTY > 0 AND !DELETED()
  IF (Style <> lcStyFlw AND IIF(!EMPTY(lcDim1),lcDim1 <> lclcDim1 ,.T.)) OR PACK_NO <> lcPackNum  
    lclcDim1  = lcDim1 
    lnRecNumber = RECNO()
    lcStyFlw = SUBSTR(STYLE,lnStyPosGl,lnScaPosGl-1) 
    lcPackNum = PACK_NO
    STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12,lnQty13,lnQty14,lnQty15,lnQty16,lnTotQty
    SCAN FOR Cfollow AND Style = lcStyFlw AND TOTQTY > 0 AND lcDim1 = lclcDim1 AND PACK_NO = lcPackNum AND !DELETED()
       FOR lnT = 1 TO 16
         lcT = ALLTRIM(STR(lnT))
         lnQty&lcT = lnQty&lcT + Qty&lcT.
         lnTotQty = lnTotQty + Qty&lcT.
       ENDFOR  
       IF RECNO() <> lnRecNumber
         DELETE 
       ENDIF        
    ENDSCAN 
    IF BETWEEN(lnRecNumber,1,RECCOUNT())
      GO lnRecNumber 
	  FOR lnT = 1 TO 16
    	lcT = ALLTRIM(STR(lnT))
        REPLACE Qty&lcT. WITH lnQty&lcT
  	  ENDFOR    
	  REPLACE TOTQTY  WITH lnTotQty 
	ENDIF   
  ENDIF 
ENDSCAN 
*:C201207,1 HES 12/08/2009 please quote for changes to Pack List [End]

LOCATE 
*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [ENd]

*:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[Start]
llPrntTot = .F.
*:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[End]
***
*C201207,1 HES Changes for DCC Custom Form [Start]
&& Add a total to follow section for all employees per PackList
lcPack = ""
lnCntr = 0
SCAN 
  IF lcPack <> Pack_No
    ***

    lcPack = Pack_No
    Select DISTINCT cPackColor from (lcpaklntmp) WHERE cPackColor <> 'ZZZZZZZZZZ' AND !EMPTY(cPackColor) and  FolowRec = 'ZZZZ' and  Pack_No=lcPack into arra  laEmp
    if _Tally =<1
     loop 
    ENDIF
    
    lcEmploye = cpackcolor
    lnRecNo = RECNO()
    && just to print the "Total pack qty" before the To_Follow section.
    APPEND BLANK 
    REPLACE Pack_No WITH lcPack ;
            Account WITH 'ZZZZZ'    
    IF !EMPTY(lcEmploye)
      REPLACE cpackcolor WITH 'ZZZZZZZZZZ'    
    ELSE 
      REPLACE FolowRec   WITH 'YYYY'
    ENDIF 
    GOTO lnRecNo 
    
    SCAN REST WHILE Pack_No = lcPack FOR FolowRec = 'ZZZZ' AND cPackColor <> 'ZZZZZZZZZZ' AND !EMPTY(cPackColor)
      *!* Collect the to follow line for this PIKTKT to be printed in the total to follow section for all employees
      lnRecNo = RECNO()
      SCATTER MEMVAR MEMO
      IF lnCntr  = 0
        m.prnToFol = .T.
        lnCntr = lnCntr + 1
      ENDIF 
      m.cPackColor = 'ZZZZZZZZZZ'
      APPEND BLANK
      GATHER MEMVAR MEMO
      GOTO lnRecNo     
    ENDSCAN
  ENDIF
ENDSCAN
SET ORDER TO EmpIndx
LOCATE
*C201207,1 HES Changes for DCC Custom Form [End]
*-SAB ----- [Start]
*DO gfDispRe WITH EVAL('lcFormName')
*SET STEP ON
IF TYPE('lcXMLFileName') <> 'C'
  DO gfDispRe WITH EVAL('lcFormName')
ELSE
  oAriaEnvironment.Report.cCROrientation = 'P'
  oAriaEnvironment.REPORT.OGLastForm = EVAL('lcFormName')
  loProgress.DESCRIPTION = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
ENDIF
*-SAB ----- [End]
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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

SELECT (lcTempScale)
lnScalRec = IIF(EOF(lcTempScale) , 0 , RECNO(lcTempScale))
LOCATE

*TMI B610326,2 [start] define local variable to use in counting the # of scales in multidim size scale
LOCAL lnLnCnt
lnLnCnt = 0
*TMI B610326,2 [END  ]
*TMI B610326,3 TMI 14/05/2013 [Start] define current dimension 
lcCurrDim = '*'
*TMI B610326,3 TMI 14/05/2013 [End  ] 
IF loScale.SEEK("S" + SUBSTR(STYLE , lnScaPosGl , 2))
  lnContSrt = 1
  SCAN FOR TYPE + SCALE + PREPAK = "S" + lcKey
    SCATTER MEMVAR MEMO
    SELECT (lcTmpSizes)
    SET ORDER TO TAG (lcTmpSizes)

    *TMI B610326,3 TMI 14/05/2013 [Start] dimension changed
    if lcCurrDim <> &lcTempScale..cDim1
      lcCurrDim = &lcTempScale..cDim1
      lnLnCnt = 0
    endif 
    *TMI B610326,3 TMI 14/05/2013 [End  ] 
    *TMI B610326,2 [start]increment dimension 
    lnLnCnt = lnLnCnt + 1
    *TMI B610326,2 [END  ]

    *B609323,1 WAM 06/29/2010 Fix bug to get the sizes for the proper scale dimension
    *IF SEEK(lcKey + cDim1 , lcTmpSizes)
    IF SEEK(lcKey + &lcTempScale..cDim1 , lcTmpSizes)
    *B609323,1 WAM 06/29/2010 (End)
    
      IF &lcTmpSizes..cDim1 == &lcTempScale..Cdim1
        *TMI B610326,2 [start] keep adding only the first two scale lines
        if lnLnCnt <= 2
        *TMI B610326,2 [end  ]
        FOR lnCrtTmp = 1 TO 8
          lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp+8))
          lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
          IF !EMPTY(&lcTempScale..&lcSizFld)
            REPLACE &lcTmpSizes..&lcNumSiz WITH &lcTempScale..&lcSizFld ,;
                    &lcTmpSizes..cDim1     WITH &lcTempScale..Cdim1
          ENDIF
        ENDFOR
        *TMI B610326,2 [start]
        endif 
        *TMI B610326,2 [end  ]
      ELSE
        lnContSrt = lnContSrt + 1
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE &lcTmpSizes..ScalFld WITH LEFT(&lcTempScale..SCALE,2) ,;
                &lcTmpSizes..cDim1   WITH &lcTempScale..Cdim1         ,;
                &lcTmpSizes..IndxDm2 WITH lnContSrt
                
       *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[Start]
       REPLACE OrgScl WITH  &lcTempScale..SCALE
	   *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[End]
      ENDIF
    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE &lcTmpSizes..ScalFld WITH LEFT(&lcTempScale..SCALE,2) ,;
              &lcTmpSizes..IndxDm2 WITH 1
       *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[Start]
       REPLACE OrgScl WITH  &lcTempScale..SCALE
	   *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[End]
              
              
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcTmpSizes)
SET ORDER TO TAG SortScal
REPLACE &lcTmpSizes..llPrnSDc WITH .T.

SELECT (lcTempScale)
IF lnScalRec <> 0
  GOTO lnScalRec IN (lcTempScale)
ENDIF

SELECT(lcAlias)

*--End of lfGetSizes.
*:*************************************************************
*: Name      : lfInsertRc
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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

=loPack_Hdr.SEEK(M.Pack_No)
lcPack_No = &lcTempPack_Hdr..PACK_NO

IF lcNewPack # lcPack_No
  lcNewPack = lcPack_No
ENDIF
=SEEK(IIF(EMPTY(&lcTempPack_Hdr..Store) , 'M' + &lcTempPack_Hdr..Account ,'S' + &lcTempPack_Hdr..Account + &lcTempPack_Hdr..Store),lcCUSTOMER)
=SEEK(STYLE,lcStyleFile)
=SEEK('S'+&lcStyleFile..ScaLE,lcTempScale)

SELECT (lcOrdLnTmp)
lcOldOrdr = ORDER()
SET ORDER TO (lcOrdLnIndTmp)

SELECT (lcAdStyGrp)

*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
SET RELATION TO "O" + &lcTempPack_Hdr..ORDER + STORE + STYLE INTO (lcOrdLnTmp)
*SET RELATION TO "O" + &lcTempPack_Hdr..ORDER + STORE + STYLE+ STR(nOrdLineno,6) INTO (lcOrdLnTmp)
*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]

lcOldOrdr2 = ORDER()
SET ORDER TO TAG lcStyFond
*!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[Start]
*IF SEEK(EVAL(lclinfile+'.PACK_NO') + SUBSTR(EVAL(lclinfile+'.STYLE'),lnStyPosGl,lnStyLnGl)) AND !EMPTY(&lcTempScale..CDIM1)
=lopack_hdr.SEEK(EVAL(lclinfile+'.Pack_No')) AND SEEK("O" + &lctemppack_hdr..ORDER + STR(&lclinfile..nordlineno,6) , lcordlntmp,lcordlntmp)
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][Start]
lcEmploy = EVAL(lcordlntmp+'.EMPLOYEE')
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][End]
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][Start]
*IF SEEK(EVAL(lclinfile+'.PACK_NO') + SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1)+EVAL(lcordlntmp+'.EMPLOYEE')) AND !EMPTY(&lctempscale..cdim1)
IF SEEK(EVAL(lclinfile+'.PACK_NO') + SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1)+EVAL(lcordlntmp+'.EMPLOYEE')) &&AND !EMPTY(&lctempscale..cdim1)
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][End]
*!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[End]
  =lfnsrtfnd()
  SELECT (lcAdStyGrp)
  SET ORDER TO TAG &lcOldOrdr2
  RETURN
ENDIF
SET ORDER TO TAG &lcOldOrdr2

APPEND BLANK
REPLACE STYLE     WITH &lclinfile..STYLE ,;
        StylDesc  WITH &lcStyleFile..DESC1       ,;
        ScalDL    WITH &lcStyleFile..SCALE       ,;
        lcDim1    WITH &lcTempScale..cDim1       ,;
        PACK_NO   WITH lcPack_No         ,;
        llPrnScal WITH .T.               ,;
        cGroupkey WITH 'zzzzzz'

*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
REPLACE nordlineNo WITH &lclinfile..nOrdLineno
lnOrdLinNo = &lclinfile..nOrdLineno
*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][Start]
REPLACE cPackColor WITH lcEmploy 
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][End]
*B609323,1 WAM 06/29/2010 Fix bug to get the sizes for the proper scale dimension
*=SEEK( LEFT(&lcStyleFile..SCALE,2) , lcTmpSizes )
=SEEK( LEFT(&lcStyleFile..SCALE,2)+&lcAdStyGrp..lcDim1 , lcTmpSizes , lcTmpSizes )
*B609323,1 WAM 06/29/2010 (End)

FOR lnCrtTmp = 1 TO 16
  lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp)) + "dl"
  lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
  IF !EMPTY(&lcTmpSizes..&lcSizFld)
    REPLACE &lcAdStyGrp..&lcNumSiz WITH &lcTmpSizes..&lcSizFld
  ENDIF
ENDFOR

*--Update the fields with the quentities.
*TMI B610326,1 [START] do not get the qty in this stage, comment out this code
*!*	GATHER MEMVAR MEMO FIELDS EXCEPT Qty*

*!*	FOR lnScl = 1 TO 1
*!*	  lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
*!*	  FOR lnAll = 1 TO 16
*!*	    lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
*!*	    IF &lcTempScale..&lcSizScl == &lcAdStyGrp..&lcAdsScl
*!*	      FOR lnFill = lnScl TO 8
*!*		    lcSizScl = "Sz" + ALLTRIM(STR(lnFill))
*!*	        lcQtyVal = "Qty" + ALLTRIM(STR(lnFill))
*!*	        lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
*!*	        IF !EMPTY(&lcTempScale..&lcSizScl)
*!*	          REPLACE &lcAdStyGrp..&lcAdsScl WITH M.&lcQtyVal
*!*	        ENDIF
*!*	      ENDFOR
*!*	      EXIT
*!*	    ENDIF
*!*	  ENDFOR
*!*	ENDFOR

*TMI B610326,1 [ENDI  ]

IF loPack_Hdr.SEEK(M.Pack_No)
  =SEEK(IIF(EMPTY(&lcTempPack_Hdr..STORE),'M','S') + &lcTempPack_Hdr..Account + &lcTempPack_Hdr..Store,lcCUSTOMER)
  REPLACE &lcAdStyGrp..Account   WITH &lcTempPack_Hdr..Account ,;
          &lcAdStyGrp..STORE     WITH &lcTempPack_Hdr..Store   ,;
          &lcAdStyGrp..cDelivery WITH IIF(&lcCustomer..llDelivery,'Y','N')
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
	  
	  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
	  *FOR lnCrtTmp = 9 TO 16
      FOR lnCrtTmp = 1 TO 16
      *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
      
        lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp)) + "dl"
        lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
        IF !EMPTY(&lcTmpSizes..&lcSizFld)
          REPLACE &lcAdStyGrp..&lcNumSiz WITH &lcTmpSizes..&lcSizFld
        ENDIF
      ENDFOR
    ELSE
      SELECT (lcAdStyGrp)
      SCATTER MEMVAR MEMO
      *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[Start]
      m.Style = SUBSTR(m.style,1,lnScaPosGl-1)+&lcTmpSizes..OrgScl
	  *!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[End]
      APPEND BLANK
      GATHER MEMVAR MEMO FIELDS EXCEPT Qty* , TotQty
      *!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[Start]
      Replace cfollow With .f.
      *!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[End]
      REPLACE &lcAdStyGrp..Account  WITH &lcTempPack_Hdr..Account      ,;
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
        
        *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
*        =SEEK("O" + &lcTempPack_Hdr..ORDER + STORE + STYLE , lcOrdLnTmp)        
        =SEEK("O" + &lcTempPack_Hdr..ORDER + STORE + STYLE+STR(nOrdLineno,6) , lcOrdLnTmp)
        *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
        
        *TMI B610326,1 [START] do not get the qty in this stage
        ** PRIVATE lcSizScl , lcAdsScl
        ** FOR lnScl = 1 TO 1
        **   lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
        **   FOR lnAll = 1 TO 16
        **     lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
        **       IF &lcTempScale..&lcSizScl == &lcAdStyGrp..&lcAdsScl
        **         FOR lnFill = lnScl TO 8
        **          lcSizScl = "Sz" + ALLTRIM(STR(lnFill))
        **          lcQtyOrd = "Qty" + ALLTRIM(STR(lnFill))
        **          lcPikQty = "Pik" + ALLTRIM(STR(lnFill))
        **          lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
        **          IF !EMPTY(&lcTempScale..&lcSizScl) AND (&lcOrdLnTmp..&lcQtyOrd - &lcOrdLnTmp..&lcPikQty) # 0
        **            REPLACE &lcAdStyGrp..&lcAdsScl WITH (&lcOrdLnTmp..&lcQtyOrd - &lcOrdLnTmp..&lcPikQty)
        **          ENDIF
        **       ENDFOR
        **       EXIT
        **     ENDIF
        **   ENDFOR
        ** ENDFOR
        ** REPLACE &lcAdStyGrp..TOTQTY WITH &lcAdStyGrp..TOTQTY + (&lcOrdLnTmp..TOTQTY - &lcOrdLnTmp..TOTPIK)
        *TMI B610326,1 [END  ]
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcOrdLnTmp)
SET ORDER TO (lcOldOrdr)

lcFolVal = SUBSTR(lcKeyRec , LEN(lckeyrec) - 4 , 4 ) && lcFolVal for the cfolowrec field.


SELECT (lcAdStyGrp)
lcOrdrTag = ORDER()
SET ORDER TO TAG lcFrstRec
*!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[Start]
*lcKeyRec = LEFT(lcKeyRec , LEN(lckeyrec) - 5 ) + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + lcFolVal + "1"
lckeyrec = LEFT(lckeyrec, LEN(lckeyrec)-5)+SUBSTR(style, lnstyposgl, lnclrposgl+lnclrlngl-1)+lcfolval+"1"
*!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[End]
=SEEK(lcKeyRec , lcAdStyGrp)

lcOrder = Piktkt.Order
lcOrder = ''
IF gfSEEK(&lclinfile..Pack_no,'PIKTKT')
  lcOrder = PIKTKT.Order
ELSE
  lcOrder = &lcTempPack_Hdr..Order
ENDIF

IF SEEK('O'+lcOrder,lcOrdLnTmp)
  SELECT(lcOrdLnTmp)
  
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
*  LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR STYLE = &lcAdStyGrp..Style  
  LOCATE REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrder FOR STYLE = &lcAdStyGrp..Style AND;
  					nOrdLineno = lnOrdLinNo 
  *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
  
  IF FOUND()
    REPLACE &lcAdStyGrp..Price WITH &lcOrdLnTmp..Price
  ENDIF
ENDIF

SELECT (lcAdStyGrp)
SET ORDER TO TAG &lcOrdrTag

SELECT (lclinfile)

SELECT (lnPrvAls)

*TMI B610326,1 [START] call =lfnsrtfnd() to calculate qty
=lfnsrtfnd()
*TMI B610326,1 [END  ]

*-- End OF lfInsertRc
*:*************************************************************
*: Name      : lfUpdgroup
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
  loPack_Hdr.SEEK(Pack_No)
  *=SEEK(Pack_No,'Pack_hdr')
  IF ((lcOldKey # Account + cDelivery + Store) .OR. (lcCurtpktk # Pack_No .AND. cDelivery = 'N')) ;
     AND &lcAdStyGrp..Account <> "ZZZZZ"
    lnGroupKey = lnGroupKey + 1

    *-- Update the group file
    SELECT (lcTmpGroup)
    APPEND BLANK
    REPLACE Pack_No    WITH &lcPaklntmp..Pack_No            ,;
            CGroupKey  WITH ALLTRIM(STR(lnGroupKey))        ,;
            weightdl   WITH weightdl   + &lcTempPack_Hdr..weightdl  ,;
            noofcarton WITH noofcarton + &lcTempPack_Hdr..noofcarton,;
            TOT_PCS    WITH TOT_PCS    + &lcTempPack_Hdr..TOT_PCS   ,;
            DPrintDate WITH &lcTempPack_Hdr..dShipdate             ,;
            consgment  WITH &lcTempPack_Hdr..consgment

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
              weightdl   WITH weightdl   + &lcTempPack_Hdr..weightdl   ,;
              noofcarton WITH noofcarton + &lcTempPack_Hdr..noofcarton ,;
              TOT_PCS    WITH TOT_PCS    + &lcTempPack_Hdr..TOT_PCS    ,;
              DPrintDate WITH &lcTempPack_Hdr..dShipdate               ,;
              consgment  WITH &lcTempPack_Hdr..consgment

      SELECT(lcPaklntmp)
    ELSE
      REPLACE &lcTmpGroup..Pack_No    WITH '******',;
              &lcTmpGroup..weightdl   WITH &lcTmpGroup..weightdl   + &lcTempPack_Hdr..weightdl  ,;
              &lcTmpGroup..noofcarton WITH &lcTmpGroup..noofcarton + &lcTempPack_Hdr..noofcarton,;
              &lcTmpGroup..TOT_PCS    WITH &lcTmpGroup..TOT_PCS    + &lcTempPack_Hdr..TOT_PCS
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
     AND loPack_Hdr.SEEK(lcPack_No,'Pack_hdr')        ;
     AND loNotePad.SEEK("B"+&lcTempPack_Hdr..Order)

    lcPack_No = Pack_No
    SKIP - 1
    REPLACE llSmPad WITH .T.            ,;
            NoteSm  WITH &lcTempNotePad..MNOTES
    SKIP
  ENDIF
ENDSCAN

SELECT (lnPrvAls)
*-- End OF lfUpdgroup.
*:*************************************************************
*: Name      : lfUpdatPrt
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
IF OARIAAPPLICATION.GCDEVICE <> 'SCREEN'
  IF !SEEK(Pack_no,lcPckPrtUp)
    INSERT INTO (lcPckPrtUp) (Pack_No) VALUES (&lcTempPack_Hdr..Pack_No)
  ENDIF
ENDIF

*-- End OF lfUpdatPrt.
*:*************************************************************
*: Name      : lfRemToflw
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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

IF SEEK('O'+&lcTempPack_Hdr..Order,lcOrdHdr)
  IF &lcOrdHdr..Open+&lcOrdHdr..Ship > &lcTempPack_Hdr..Tot_pcs
    llRemains = .T.
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN llRemains

*-- End OF lfRemToflw.
*!*************************************************************
*! Name      : lfAddField
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
DIMENSION &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
*-SAB ----- [Start]
*=gfItemMask(@laItemSeg)
IF TYPE('lcXMLFileName') == 'C'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laItemSeg)
ELSE
  =gfItemMask(@laItemSeg)
ENDIF
*-SAB ----- [End]
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
*-SAB ----- [Start]
*=gfItemMask(@laItemSeg)
IF TYPE('lcXMLFileName') == 'C'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laItemSeg)
ELSE
  =gfItemMask(@laItemSeg)
ENDIF
*-SAB ----- [End]
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
*-SAB ----- [Start]
*=gfItemMask(@laItemSeg)
IF TYPE('lcXMLFileName') == 'C'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laItemSeg)
ELSE
  =gfItemMask(@laItemSeg)
ENDIF
*-SAB ----- [End]
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
*:B609835,1 MMT 02/20/2012 Custom Packing list form DC Takes time to preview or print[T20120118.0002][Start]
*=gfSEEK("0001"+&lcAdStyGrp..STYLE+'PP')
*SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD= "0001"+&lcAdStyGrp..STYLE+'PP';
		 FOR gfSeek('PP'+POSLN.PO,'POSHDR') AND POSHDR.STATUS = "O"
=gfSqlRun("Select POSLN.CINVTYPE,POSLN.STYLE,POSLN.CBUSDOCU,POSLN.CSTYTYPE,POSLN.PO,POSLN.[LINENO],POSLN.TRANCD FROM "+;
		  "	POSLN INNER JOIN POSHDR ON POSHDR.CBUSDOCU=POSLN.CBUSDOCU AND POSHDR.CSTYTYPE = POSLN.CSTYTYPE AND POSHDR.PO = POSLN.PO "+;
		  " WHERE POSLN.CBUSDOCU ='P' AND POSLN.CSTYTYPE='P' AND POSLN.STYLE='"+&lcAdStyGrp..STYLE+"' AND POSLN.CINVTYPE='0001' AND POSHDR.STATUS = 'O'",'POSLN')		 
SELECT POSLN
LOCATE 		  
SCAN		 
*:B609835,1 MMT 02/20/2012 Custom Packing list form DC Takes time to preview or print[T20120118.0002][End]
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
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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


FOR lnI = 1 TO ALEN(laTmpStru,1)
  STORE '' TO laTmpStru[lnI ,7],laTmpStru[lnI ,8],laTmpStru[lnI ,9],;
    laTmpStru[lnI ,10],laTmpStru[lnI ,11],laTmpStru[lnI ,12],;
    laTmpStru[lnI ,13],laTmpStru[lnI ,14],laTmpStru[lnI ,15],;
    laTmpStru[lnI ,16]
  STORE 0 TO  laTmpStru[lnI ,17],laTmpStru[lnI ,18]
ENDFOR  

=gfCrtTmp(lcAdStyGrp,@laTmpStru,"PACK_NO + Account + Store + cGroupkey  + CDelivery + ALLTRIM(STR(Line_No)) + FolowRec + ALLTRIM(STR(IndxDm2))",lcAdStyGrp ,.T.)

SELECT (lcAdStyGrp)
*!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[Start]
*!*	INDEX ON PACK_NO + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) TAG lcStyFond ADDITIVE
*!*	INDEX ON PACK_NO + Account + Store + cGroupkey  + CDelivery + ALLTRIM(STR(Line_No))+ SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + FolowRec + ALLTRIM(STR(IndxDm2)) TAG lcFrstRec ADDITIVE
INDEX ON pack_no+SUBSTR(style, lnstyposgl, lnclrposgl+lnclrlngl-1)+cpackcolor TAG lcstyfond ADDITIVE
INDEX ON pack_no+account+store+cgroupkey+cdelivery+ALLTRIM(STR(line_no))+SUBSTR(style, lnstyposgl,  lnclrposgl+lnclrlngl-1)+folowrec+ALLTRIM(STR(indxdm2)) TAG lcfrstrec ADDITIVE
*!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[End]
DIMENSION laFileStr[8,4]
laFileStr[1,1] = 'CGroupKey'
laFileStr[1,2] = 'C'
laFileStr[1,3] = 6
laFileStr[1,4] = 0

laFileStr[2,1] = 'PACK_NO'
laFileStr[2,2] = 'C'
laFileStr[2,3] = 6
laFileStr[2,4] = 0

laFileStr[3,1] = 'weightdl'
laFileStr[3,2] = 'N'
laFileStr[3,3] = 13
laFileStr[3,4] = 2

laFileStr[4,1] = 'noofcarton'
laFileStr[4,2] = 'N'
laFileStr[4,3] = 8
laFileStr[4,4] = 0

laFileStr[5,1] = 'TOT_PCS'
laFileStr[5,2] = 'N'
laFileStr[5,3] = 8
laFileStr[5,4] = 0

laFileStr[6,1] = 'DPrintDate'
laFileStr[6,2] = 'D'
laFileStr[6,3] = 8
laFileStr[6,4] = 0

laFileStr[7,1] = 'consgment'
laFileStr[7,2] = 'C'
laFileStr[7,3] = 20
laFileStr[7,4] = 0

laFileStr[8,1] = 'llRemantTo'
laFileStr[8,2] = 'L'
laFileStr[8,3] = 1
laFileStr[8,4] = 0

=gfCrtTmp(lcTmpGroup,@laFileStr,"CGroupKey",lcTmpGroup,.T.)
*!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[Start]
*DIMENSION laFileStrucu[20,4]
DIMENSION laFileStrucu[21,4]
*!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[End]
laFileStrucu[1,1] = 'ScalFld'
laFileStrucu[1,2] = 'C'
laFileStrucu[1,3] = 2
laFileStrucu[1,4] = 0

laFileStrucu[2,1] = 'IndxDm2'
laFileStrucu[2,2] = 'N'
laFileStrucu[2,3] = 3
laFileStrucu[2,4] = 0

laFileStrucu[3,1] = 'cDim1'
laFileStrucu[3,2] = 'C'
laFileStrucu[3,3] = 5
laFileStrucu[3,4] = 0

laFileStrucu[4,1] = 'llPrnSDc'
laFileStrucu[4,2] = 'L'
laFileStrucu[4,3] = 1
laFileStrucu[4,4] = 0

laFileStrucu[5,1] = 'Sz1'
laFileStrucu[5,2] = 'C'
laFileStrucu[5,3] = 5
laFileStrucu[5,4] = 0

laFileStrucu[6,1] = 'Sz2'
laFileStrucu[6,2] = 'C'
laFileStrucu[6,3] = 5
laFileStrucu[6,4] = 0

laFileStrucu[7,1] = 'Sz3'
laFileStrucu[7,2] = 'C'
laFileStrucu[7,3] = 5
laFileStrucu[7,4] = 0

laFileStrucu[8,1] = 'Sz4'
laFileStrucu[8,2] = 'C'
laFileStrucu[8,3] = 5
laFileStrucu[8,4] = 0

laFileStrucu[9,1] = 'Sz5'
laFileStrucu[9,2] = 'C'
laFileStrucu[9,3] = 5
laFileStrucu[9,4] = 0

laFileStrucu[10,1] = 'Sz6'
laFileStrucu[10,2] = 'C'
laFileStrucu[10,3] = 5
laFileStrucu[10,4] = 0

laFileStrucu[11,1] = 'Sz7'
laFileStrucu[11,2] = 'C'
laFileStrucu[11,3] = 5
laFileStrucu[11,4] = 0

laFileStrucu[12,1] = 'Sz8'
laFileStrucu[12,2] = 'C'
laFileStrucu[12,3] = 5
laFileStrucu[12,4] = 0

laFileStrucu[13,1] = 'Sz9'
laFileStrucu[13,2] = 'C'
laFileStrucu[13,3] = 5
laFileStrucu[13,4] = 0

laFileStrucu[14,1] = 'Sz10'
laFileStrucu[14,2] = 'C'
laFileStrucu[14,3] = 5
laFileStrucu[14,4] = 0

laFileStrucu[15,1] = 'Sz11'
laFileStrucu[15,2] = 'C'
laFileStrucu[15,3] = 5
laFileStrucu[15,4] = 0

laFileStrucu[16,1] = 'Sz12'
laFileStrucu[16,2] = 'C'
laFileStrucu[16,3] = 5
laFileStrucu[16,4] = 0

laFileStrucu[17,1] = 'Sz13'
laFileStrucu[17,2] = 'C'
laFileStrucu[17,3] = 5
laFileStrucu[17,4] = 0

laFileStrucu[18,1] = 'Sz14'
laFileStrucu[18,2] = 'C'
laFileStrucu[18,3] = 5
laFileStrucu[18,4] = 0

laFileStrucu[19,1] = 'Sz15'
laFileStrucu[19,2] = 'C'
laFileStrucu[19,3] = 5
laFileStrucu[19,4] = 0

laFileStrucu[20,1] = 'Sz16'
laFileStrucu[20,2] = 'C'
laFileStrucu[20,3] = 5
laFileStrucu[20,4] = 0

*!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[Start]
laFileStrucu[21,1] = 'OrgScl'
laFileStrucu[21,2] = 'C'
laFileStrucu[21,3] = 3
laFileStrucu[21,4] = 0
*!B609351,1 MMT 11/04/2010 Pack list form DC printing incorrectly to follow records[End]
=gfCrtTmp(lcTmpSizes,@laFileStrucu,"ScalFld + cDim1",lcTmpSizes,.T.)
*--File create the scale and sizes.
SELECT (lcTmpSizes)
INDEX ON ScalFld + ALLTRIM(STR(IndxDm2)) Tag SortScal


*--End of lfCreatTmp.
*:*************************************************************
*: Name      : lfnsrtfnd
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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
lcScalVal = IIF(SEEK("S" + SUBSTR(STYLE , lnScaPosGl , lnScaLnGl) , lcTempScale) , &lcTempScale..SCALE , "")
lcCdm1Val = &lcTempScale..CDIM1
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][Start]
*=SEEK("O" + &lcTempPack_Hdr..ORDER + STORE + &lclinfile..style+STR(&lclinfile..nOrdLineno,6) , lcOrdLnTmp)
=SEEK("O" + &lcTempPack_Hdr..ORDER + &lcTempPack_Hdr..STORE + &lclinfile..style+STR(&lclinfile..nOrdLineno,6) , lcOrdLnTmp)
lcFileEmployee = &lcOrdLnTmp..Employee
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][End]
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
  *!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[Start]
*!*	  IF SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnStyPosGl,lnStyLnGl) AND LCDIM1 == lcCdm1Val ;
*!*	    AND &lcAdStyGrp..Account <> "ZZZZZ"    
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][Start]
*  IF SUBSTR(STYLE,lnstyposgl,lnclrposgl+lnclrlngl-1) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1) AND;
     lcdim1 == lccdm1val  AND &lcadstygrp..account <> "ZZZZZ"
  IF SUBSTR(STYLE,lnstyposgl,lnclrposgl+lnclrlngl-1) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1) AND;
     lcdim1 == lccdm1val  AND &lcadstygrp..account <> "ZZZZZ" AND CPACKCOLOR = lcFileEmployee 
*!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][End]
  *!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[End]
    FOR lnScl = 1 TO 1
      lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
      FOR lnAll = 1 TO 16
        lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
        IF &lcTempScale..&lcSizScl == &lcAdStyGrp..&lcAdsScl
          FOR lnFill = lnScl TO 8
            lcSizScl = "Qty" + ALLTRIM(STR(lnFill))
            lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
            IF &lclinfile..&lcSizScl # 0
*              REPLACE &lcAdStyGrp..&lcAdsScl WITH &lclinfile..&lcSizScl
              *TMI B610326,1 [START] do not calculate qty packed for lines with owner field not empty
              *REPLACE &lcAdStyGrp..&lcAdsScl WITH &lcAdStyGrp..&lcAdsScl + &lclinfile..&lcSizScl
              *TMI B610326,5 , check the same style
              *if &lclinfile..COWNER <> "BBBBVVVVDDDDRRRR"
              *!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][Start]
              *if &lclinfile..COWNER <> "BBBBVVVVDDDDRRRR" AND &lcAdStyGrp..STYLE = EVAL(lclinfile+'.STYLE')
              if &lclinfile..COWNER <> "BBBBVVVVDDDDRRRR" AND SUBSTR(&lcAdStyGrp..STYLE ,lnstyposgl,lnclrposgl+lnclrlngl-1) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1) 
              *!B610794,1 MMT 08/05/2014 Some Styles sizes is not printed [T20140731.0007][End]
                *TMI B610326,5
                *!B610828,1 MMT 08/31/2014 Fix the bug of wrong line qty if it is exist for the same employee more than once[T20140827.0004][Start]
                *REPLACE &lcAdStyGrp..&lcAdsScl WITH &lclinfile..&lcSizScl                
                REPLACE &lcAdStyGrp..&lcAdsScl WITH &lcAdStyGrp..&lcAdsScl + &lclinfile..&lcSizScl
                *!B610828,1 MMT 08/31/2014 Fix the bug of wrong line qty if it is exist for the same employee more than once[T20140827.0004][End]
              endif 
              *TMI B610326,1 [END  ]
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
    *TMI B610326,1 [START] be sure that the totqty is summed correctly
    REPLACE &lcAdStyGrp..TOTQTY WITH &lcAdStyGrp..QTY1+&lcAdStyGrp..QTY2+&lcAdStyGrp..QTY3+&lcAdStyGrp..QTY4+&lcAdStyGrp..QTY5+&lcAdStyGrp..QTY6+&lcAdStyGrp..QTY7+&lcAdStyGrp..QTY8+;
                                     &lcAdStyGrp..QTY9+&lcAdStyGrp..QTY10+&lcAdStyGrp..QTY11+&lcAdStyGrp..QTY12+&lcAdStyGrp..QTY13+&lcAdStyGrp..QTY14+&lcAdStyGrp..QTY15+&lcAdStyGrp..QTY16
    *TMI B610326,1 [END  ]
    *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
    REPLACE &lcAdStyGrp..nOrdLineno WITH &lclinfile..nOrdLineno 
    *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
		    
  ENDIF

  *--Section to add the new records in case the customer has to follow.
  *!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[Start]
*!*	  IF llToFollow AND SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnStyPosGl,lnStyLnGl) ;
*!*	                AND LCDIM1 == lcCdm1Val                 ;
*!*	                AND &lcAdStyGrp..Account == "ZZZZZ"     ;
*!*	                AND &lcAdStyGrp..Cfollow                ;
*!*	                AND &lcAdStyGrp..FolowRec == "ZZZZ"
  IF lltofollow AND SUBSTR(STYLE,lnstyposgl,lnclrposgl+lnclrlngl-1) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1)   AND;
     lcdim1 == lccdm1val                  AND;
     &lcadstygrp..account == "ZZZZZ"      AND ;
     &lcadstygrp..cfollow                 AND;
     &lcadstygrp..folowrec == "ZZZZ" AND;
     &lcadstygrp..CPACKCOLOR = lcFileEmployee 
  *!B609351,1 MMT 11/03/2010 Pack list form DC printing incorrectly to follow records[End]
    *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
*    =SEEK("O" + &lcTempPack_Hdr..ORDER + STORE + &lclinfile..style , lcOrdLnTmp)    
    =SEEK("O" + &lcTempPack_Hdr..ORDER + STORE + &lclinfile..style+STR(&lclinfile..nOrdLineno,6) , lcOrdLnTmp)
    *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
    
    IF lfOpenPo()
      REPLACE &lcAdStyGrp..Ponofolo WITH lcOpenPO ,;
              &lcAdStyGrp..DatAvlbl WITH ldAvalbl
    ENDIF

    PRIVATE lcSizScl , lcAdsScl
    FOR lnScl = 1 TO 1
      lcSizScl = "Sz" + ALLTRIM(STR(lnScl))
      FOR lnAll = 1 TO 16
        lcAdsScl = "Sz" + ALLTRIM(STR(lnAll)) + "DL"
        IF &lcTempScale..&lcSizScl == &lcAdStyGrp..&lcAdsScl
          FOR lnFill = lnScl TO 8
            lcSizScl = "Sz" + ALLTRIM(STR(lnFill))
            lcQtyOrd = "Qty" + ALLTRIM(STR(lnFill))
            lcPikQty = "Pik" + ALLTRIM(STR(lnFill))
            lcAdsScl = "Qty" + ALLTRIM(STR(lnFill + lnAll - 1))
            *!B610936,1 MMT 01/22/2015 Custom Packing list DC does not show allocated line '***"  as to follow[T20150116.0004][Start]
            *IF !EMPTY(&lcTempScale..&lcSizScl) AND (&lcOrdLnTmp..&lcQtyOrd - &lcOrdLnTmp..&lcPikQty) # 0
            IF !EMPTY(&lcTempScale..&lcSizScl) AND IIF(&lcOrdLnTmp..Piktkt='******',&lcOrdLnTmp..&lcQtyOrd,(&lcOrdLnTmp..&lcQtyOrd - &lcOrdLnTmp..&lcPikQty)) # 0
            *!B610936,1 MMT 01/22/2015 Custom Packing list DC does not show allocated line '***"  as to follow[T20150116.0004][End]
*              REPLACE &lcAdStyGrp..&lcAdsScl WITH (ORDLINE.&lcQtyOrd - ORDLINE.&lcPikQty)
              *TMI B610326,1 [START] do not accomulate the qty field, just replace
              *REPLACE &lcAdStyGrp..&lcAdsScl WITH &lcAdStyGrp..&lcAdsScl + (&lcOrdLnTmp..&lcQtyOrd - &lcOrdLnTmp..&lcPikQty)

              *B610326,5 for the FOLLOW check that cowner field is filled and we check the same style
              IF &lclinfile..COWNER = "BBBBVVVVDDDDRRRR" and &lcAdStyGrp..STYLE = EVAL(lclinfile+'.STYLE')
                *B610326,5
                *!B610936,1 MMT 01/22/2015 Custom Packing list DC does not show allocated line '***"  as to follow[T20150116.0004][Start] 
                *REPLACE &lcAdStyGrp..&lcAdsScl WITH (&lcOrdLnTmp..&lcQtyOrd - &lcOrdLnTmp..&lcPikQty)
                REPLACE &lcAdStyGrp..&lcAdsScl WITH IIF(&lcOrdLnTmp..Piktkt='******',&lcOrdLnTmp..&lcQtyOrd,(&lcOrdLnTmp..&lcQtyOrd - &lcOrdLnTmp..&lcPikQty))
                *!B610936,1 MMT 01/22/2015 Custom Packing list DC does not show allocated line '***"  as to follow[T20150116.0004][End]
                *B610326,5
              ENDIF 
              *B610326,5
              
              *TMI B610326,1 [END  ]
            ENDIF
          ENDFOR
          EXIT
        ENDIF
      ENDFOR
    ENDFOR
    *TMI B610326,1 [START]be sure that the totqty is summed correctly
    *REPLACE &lcAdStyGrp..TOTQTY WITH &lcAdStyGrp..TOTQTY + (&lcOrdLnTmp..TOTQTY - &lcOrdLnTmp..TOTPIK)
    REPLACE &lcAdStyGrp..TOTQTY WITH &lcAdStyGrp..QTY1+&lcAdStyGrp..QTY2+&lcAdStyGrp..QTY3+&lcAdStyGrp..QTY4+&lcAdStyGrp..QTY5+&lcAdStyGrp..QTY6+&lcAdStyGrp..QTY7+&lcAdStyGrp..QTY8+;
                                     &lcAdStyGrp..QTY9+&lcAdStyGrp..QTY10+&lcAdStyGrp..QTY11+&lcAdStyGrp..QTY12+&lcAdStyGrp..QTY13+&lcAdStyGrp..QTY14+&lcAdStyGrp..QTY15+&lcAdStyGrp..QTY16
    *TMI B610326,1 [END  ]
  ENDIF
ENDSCAN

*--End of lfnsrtfnd.
*:*************************************************************
*: Name      : lfAdMsdRec
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
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

*:C201207,2 HES 12/27/2009 Handle scenario of packlist with shipped Order [Start]
llShpd = .F.
*:C201207,2 HES 12/27/2009 Handle scenario of packlist with shipped Order [End]

STORE SPACE(0) TO lcPoChk
SCAN
  lcPack_No = &lclinfile..Pack_No
  IF lcPoChk # PACK_NO
    lcKeyMain = EVAL(KEY())
    =loPack_hdr.SEEK(&lclinfile..PACK_NO)
    
    *:C201207,2 HES 12/27/2009 Handle scenario of packlist with shipped Order [Start]
    llShpd = .F.
    SELECT INVLINE
    lcOrd = ORDER()
    SET ORDER TO INVLINEO
    IF SEEK(&lcTempPack_Hdr..ORDER)
      SET ORDER TO &lcOrd
      llShpd = .T.
    ENDIF  
    *!*	    IF SEEK("O" + &lcTempPack_Hdr..ORDER ,lcOrdLnTmp)
    IF SEEK("O" + &lcTempPack_Hdr..ORDER ,lcOrdLnTmp) &&AND !llShpd
    *:C201207,2 HES 12/27/2009 Handle scenario of packlist with shipped Order [End]
      
      SELECT (lcOrdLnTmp)
      SCAN REST WHILE CORDTYPE + ORDER + STR(LINENO,6) = "O" + &lcTempPack_Hdr..ORDER
        *!B610936,1 MMT 01/22/2015 Custom Packing list DC does not show allocated line '***"  as to follow[T20150116.0004][Start]
        *IF EMPTY(&lcOrdLnTmp..PIKTKT)
        *B611441,1 MMT 10/16/2017 Custom Packing list form does not print all to follow lines[T20171013.0004][Start]
        *IF EMPTY(&lcOrdLnTmp..PIKTKT) or &lcOrdLnTmp..PIKTKT = '******'
        IF EMPTY(&lcOrdLnTmp..PIKTKT) or &lcOrdLnTmp..PIKTKT = '******' OR &lcordlntmp..TotPik <> &lcordlntmp..TotQty
        *B611441,1 MMT 10/16/2017 Custom Packing list form does not print all to follow lines[T20171013.0004][End]
        *!B610936,1 MMT 01/22/2015 Custom Packing list DC does not show allocated line '***"  as to follow[T20150116.0004][End]
          SCATTER MEMVAR MEMO
	      SELECT (lclinfile)
	      APPEND BLANK
	      GATHER MEMVAR FIELDS LIKE Qty* , TotQty , Style
	      REPLACE Pack_no WITH lcPack_No ;
	              COWNER  WITH "BBBBVVVVDDDDRRRR"
	              
	       *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
	       REPLACE nOrdLineno WITH &lcOrdLnTmp..LinenO
	       *:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]
	       
	       *C201098,1 MMT 02/08/2008 Convert Packing List Form of DCC to Aria4[Start]
	       IF !SEEK(&lcOrdLnTmp..Style,lcStyleFile)
	         loStyle.Seek(&lcOrdLnTmp..Style)
	         SELECT(lcTempStyle)
             SCATTER MEMO MEMVAR 
		     INSERT INTO (lcStyleFile) FROM MEMVAR 
	       ENDIF 
	       *C201098,1 MMT 02/08/2008 Convert Packing List Form of DCC to Aria4[End]
	              
	      SELECT (lcOrdLnTmp)
        ENDIF
      ENDSCAN
      SELECT (lclinfile)
      =SEEK(lcKeyMain)
    ENDIF
  ENDIF
  lcPoChk = &lcTempPack_Hdr..PACK_NO
ENDSCAN

SELECT(lcAlias)
*--End of lfAdMsdRec.

*:*************************************************************
*: Name      : lfGetEmpName
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
*: Purpose   : Function to Get Employee Name
*:*************************************************************
FUNCTION lfGetEmpName
lnAlias = ALIAS()
lcEmpName = ''
SELECT Contact
=gfSEEK('C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..Store,8),'contact') 

*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [Start]
*LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..Store,8) FOR CCONTCODE = ALLTRIM(&lcpaklntmp..cpackcolor)
*!B611014,1 MMT 06/09/2015 Custom packing list form Prints incorrect employee name[T20150605.0002] [Start]
*LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..Store,8) FOR CCNTCTCODE = ALLTRIM(&lcpaklntmp..cpackcolor)
LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..Store,8) FOR CCNTCTCODE == substr(&lcpaklntmp..cpackcolor,1,12)
*!B611014,1 MMT 06/09/2015 Custom packing list form Prints incorrect employee name[T20150605.0002] [End]
*:B608866,1 MMT 05/19/2009 Fix bugs of grouping to Follow by Employee [End]

IF FOUND()
  lcEmpName = Contact.contact
ENDIF  
SELECT (lnAlias )
RETURN lcEmpName

*:*************************************************************
*: Name      : lfGetuCode
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
*: Purpose   : Function to Get Uniform Code
*:*************************************************************
FUNCTION lfGetuCode
lnAlias = ALIAS()
lcUCode = ''
SELECT Contact
=gfSEEK('C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..Store,8),'contact') 
LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..Store,8) FOR CCNTCTCODE = ALLTRIM(&lcpaklntmp..cpackcolor)
IF FOUND()
  lcUCode  = Contact.Ucode
ENDIF  
SELECT (lnAlias )
RETURN lcUCode 

*:*************************************************************
*: Name      : lfPrnMsg
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2009
*: Purpose   : Function to decide if message will be printed or not
*:*************************************************************
FUNCTION lfPrnMsg
lnAlias = ALIAS()

lcPackNo =&lcpaklntmp..Pack_no 
loPack_Hdr.SEEK(lcPackNo) 
SELECT (lcpaklntmp)
lnRcNo = RECNO()
LOCATE 
LOCATE FOR PACK_No =lcPackNo AND Account  = "ZZZZZ" AND Cfollow =.T. AND FolowRec = "ZZZZ" AND (EMPTY(Ponofolo) OR EMPTY(DatAvlbl))
IF FOUND()
  llPrntMsg =.T.
ELSE
  llPrntMsg = .F.
ENDIF 
IF BETWEEN(lnRcNo ,1,RECCOUNT())
  GO RECORD lnRcNo 
ENDIF
SELECT(lnAlias)

RETURN ''

*:C201098,2 MMT 02/10/2009 Fix bugs of not prinitng to Follow[Start]
*:*************************************************************
*: Name      : lfGetAllOrdl
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/10/2009
*: Purpose   : Function to get line of ordeline table
*:*************************************************************
FUNCTION lfGetAllOrdl

SELECT(lcOrdLnTmp)
DELETE ALL

SELECT (lcPackTmp)
LOCATE 
SCAN 
  loOrdLine.Seek('O'+ &lcPackTmp..order + &lcPackTmp..Store)
  SELECT(lcTempOrdLine)
  SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ &lcPackTmp..order + &lcPackTmp..Store 
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdLnTmp) FROM MEMVAR
  ENDSCAN 
ENDSCAN 

SELECT(lcOrdLnTmp)
SCAN 
  loStyle.Seek(Style)
  SELECT(lcStyleFile)
  IF !SEEK(&lcTempStyle..Style,lcStyleFile)
     SELECT(lcTempStyle)
     SCATTER MEMO MEMVAR 
    INSERT INTO (lcStyleFile) FROM MEMVAR 
  ENDIF
ENDSCAN 

*:*************************************************************
*: Name      : lfGrpReSt
*! Developer : Mariam Mazhar (MMT)
*! Date      : 02/10/2009
*: Purpose   : Function to Reset eng group variable
*:*************************************************************
FUNCTION lfGrpReSt
llEndGroup = .F.
*:C201098,2 MMT 02/10/2009 Fix bugs of not prinitng to Follow[End]

*:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[Start]
*:*************************************************************
*: Name      : lfChkNext
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/30/2009
*: Purpose   : Function to Check next line
*:*************************************************************
FUNCTION lfChkNext
llPrntTot = .F.
lcPackNum = pack_no 
lnOldRec = RECNO(lcpaklntmp)
IF !EOF() AND (cpackcolor <> "ZZZZZZZZZZ" OR EMPTY(cpackcolor))
  SKIP 1 IN (lcpaklntmp)
  IF !EOF() AND (lcPackNum = pack_no  AND cpackcolor = "ZZZZZZZZZZ")
    llPrntTot = .T.
  ELSE
    IF EOF() OR  (!EOF() AND (lcPackNum <> pack_no))
      llPrntTot = .T.
    ENDIF 
  ENDIF 
ELSE
  IF EOF() AND (cpackcolor <> "ZZZZZZZZZZ"  OR EMPTY(cpackcolor))
    llPrntTot = .T.
  ELSE
   IF cpackcolor = "ZZZZZZZZZZ"  
     llPrntTot = .F.
   ENDIF 
  ENDIF 
ENDIF 
IF BETWEEN(lnOldRec ,1,RECCOUNT(lcpaklntmp))
  GO RECORD lnOldRec IN (lcpaklntmp)
ENDIF 
*:B608953,1 MMT 07/30/2009 Fix bug  of printing tot pack Qty after tofollow[End]