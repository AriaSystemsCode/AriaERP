*!--------------------------------------------------------------------------
*! Program file        : MFWIPAC.PRG
*! Program description : GL-WIP Account Report.
*! For System          : Aria Advantage Series - Version oldAriaSql
*! For Module          : MF,PO and MA
*! Developer Name      : BASSEM RAAFAT ERNEST(BWA)
*! Tracking Job Number : N037652
*! Date                : 06/04/2006
*!--------------------------------------------------------------------------
*! Calls               : MFWIPAD.FRX  && Detail Form.
*!--------------------------------------------------------------------------
*! Called From         : System Menu
*!--------------------------------------------------------------------------
*! Passed Parameters   : None.
*!--------------------------------------------------------------------------
*! Example             : DO MFWIPAC
*!--------------------------------------------------------------------------
*! Modificatoins       :
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[T20071016.0003]
*:B608390,2 MMT 01/22/2008 Add Status Actualized to MF module OPGRID[T20071016.0003]
*:B608420,1 WAM 02/05/2008 Show cost types 6 & 7 [T20080125.0001]
*:B608430,1 WAM 02/12/2008 Get bom lines related to PO line number only
*:B608489,1 WAM 03/23/2008 Fix bug of PO closing entries doesn't show up [T20080214.0009]
*:B608540,1 WAM 05/05/2008 Convert MFG cissue ost to base currency [T20080429.0002]
*!--------------------------------------------------------------------------

llGlLink    = (ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',oAriaApplication.ActiveCompanyID)))='Y')
llApLink    = ('AP' $ oAriaApplication.companyinstalledmodules)
llMFInstall = ('MF' $ oAriaApplication.companyinstalledmodules)

*B608489,1 WAM 03/23/2008 Check if system is set to use multiple currencies
llMulCurr=gfGetMemvar('llMulCurr',oAriaApplication.ActiveCompanyID)
*B608489,1 WAM 03/23/2008 (ENd)

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  STORE .F. TO llCutTktH,llCutTktLn
*!*  IF llMFInstall AND !USED('CUTTKTH.DBF')
*!*    llCutTktH = gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
*!*  ENDIF
*!*  IF llMFInstall AND !USED('CUTTKTL.DBF')
*!*    llCutTktLn = gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
*!*  ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[END]

IF llGlLink  && If the system is linked to GL-Module, Else Return.
  lcGlAcnt   = lcRpGlAct            && Gl Account Code.
  lcStyFlt   = ".T."                && Initiate Style file criteria
  lcSeason   = ".T."
  lcDivision = ".T."
  lcStyFlt   = IIF(lcRpStatus <> "N" , "Style.Status = lcRpStatus", lcStyFlt)
  
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*    IF lcRpMakBuy <> "B"  && Not Both.
*!*      lcStyFlt  = lcStyFlt + " .AND. Style.MAKE = (lcRpMakBuy=[M])"
*!*    ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
  
  STORE {} TO ldLowDat , ldHigDat  && Initial Date Range Variable.
  STORE "" TO lcLowSty , lcHigSty  && Initiate Style Range Variable.
  lcWhilCon = ""
  llDumyRet = lfSetRel()           && Call Function to set relations.
  lcSeltdPo = ""
  lcSeltdCT = ""
  =lfCretFltr()
  lnMajLen = LEN(gfitemMask("PM"))

  *--Function to zap temprory files.
  =lfBasToClr(lcPoshdr , 'C')
  =lfBasToClr(lcPosLn  , 'C')

  *--- PrePare style file for collecting data.
  =lfPreSty()
  
  *-- Check if lcRpTmpFil exsit or not.
  IF (!USED(lcRpTmpFil) OR (RECCOUNT(lcRpTmpFil) > 0)) ;
  OR (!USED(lcTmpCurs)  OR (RECCOUNT(lcTmpCurs)  > 0))
    =lfWorkFile()
  ENDIF

  =lfRtrvRec()   &&-- Retrieving Records...
  =lfColctDat()  &&-- Collecting Data...
  
  SELECT (lcRpTmpFil)
  LOCATE
  IF EOF()
    *--Message 'There are no records to display...!'
    =gfModalGen('TRM00052B00000','DIALOG')
  ELSE
    loogScroll.cCROrientation = 'P'
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  =gfModalGen('TRM38195B00000','ALERT')
ENDIF

*--Close the CutTktH and CutTktL files.

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  IF llMFInstall 
*!*    IF  llCutTktH
*!*      USE IN CutTktH
*!*    ENDIF  
*!*    IF llCutTktLn
*!*      USE IN CutTktL
*!*    ENDIF
*!*  ENDIF  
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfSetRel
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Function to set nessesary relations.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRel()
*!*************************************************************
FUNCTION lfSetRel

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  IF llMFInstall AND llCutTktH
*!*    SELECT CutTktH
*!*    SET RELATION TO Link_Code+'013' INTO Gl_Link
*!*  ENDIF
*!*  LOCATE
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

SELECT (lcPoshdr)
SET RELATION TO Link_Code+'013' INTO Gl_Link
LOCATE

*--End of lfSetRel.
*!*************************************************************
*! Name      : lfCretFltr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCretFltr()
*!*************************************************************
FUNCTION lfCretFltr

FOR lnInd = 1 TO ALEN(loOgScroll.laOgFxFlt,1)
  DO CASE
    *B608430,1 WAM 02/12/2008 Fix filter
    *CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6])) AND !EOF(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
    *B608430,1 WAM 02/12/2008 (End)
        
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      *lcStyFlt = lcStyFlt + " .AND. ALLTRIM(STYLE.FABRIC) $ ["+ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6])+"]"
      lcStyFlt = lcStyFlt + " .AND. SEEK(STYLE.FABRIC,'"+ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6])+"')"
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'STYLE.SEASON' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
    
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      *lcSeason = ".Season $ ["+laOgFxFlt[lnInd,6]+"]"
      lcSeason = "Season $ ["+laOgFxFlt[lnInd,6]+"]"
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'STYLE.CDIVISION' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
    
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      *lcDivision = "POSHDR.CDIVISION $ ["+laOgFxFlt[lnInd,6]+"]"
      lcDivision = "CDIVISION $ ["+laOgFxFlt[lnInd,6]+"]"
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
      lcStyFlt = lcStyFlt + " .AND. ALLTRIM(STYLE.CSTYGROUP) $ ["+ALLTRIM(laOgFxFlt[lnInd,6])+"]"

    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'STYLE.PATTERN' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
    
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      *lcStyFlt = lcStyFlt+" .AND. ALLTRIM(STYLE.PATTERN) == ["+ALLTRIM(laOgFxFlt[lnInd,6])+"]"
      lcStyFlt = lcStyFlt+" .AND. STYLE.PATTERN == ["+laOgFxFlt[lnInd,6]+"]"
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
      
    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = "SUBSTR(STYLE.Style,lnClrPo,lnColorLen)" .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
      lcStyFlt = lcStyFlt+" .AND. SUBSTR(STYLE.Style,lnClrPo,lnColorLen) $ ["+laOgFxFlt[lnInd,6]+"]"
      
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
    *!*      CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUTTKTH.ENTERED' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
    *!*        *--- Dates Range
    *!*        ldLowDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
    *!*        ldHigDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'POSHDR.ENTERED' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
      *--- Dates Range
      ldLowDat = CTOD(ALLTRIM(SUBSTR(loOgScroll.laOgFxFlt[lnInd,6],1,10)))
      ldHigDat = CTOD(ALLTRIM(SUBSTR(loOgScroll.laOgFxFlt[lnInd,6],12,20)))

    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'STYLE.CSTYMAJOR' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))
      *--- Style Pattern
      
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      *!*        lcLowSty = SUBSTR(loOgScroll.laOgFxFlt[lnInd,6],1,AT("|",laOgFxFlt[lnInd,6])-1)
      *!*        lcHigSty = SUBSTR(loOgScroll.laOgFxFlt[lnInd,6],AT("|",laOgFxFlt[lnInd,6])+1)
      lcLowSty = loOgScroll.laOgFxFlt[lnInd,6]
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

    CASE ALLTRIM(loOgScroll.laOgFxFlt[lnInd,1]) = 'POSHDR.PO' .AND. !EMPTY(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6])) .AND. USED(ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6]))

      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      lcSeltdCT = ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6])
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

      lcSeltdPo = ALLTRIM(loOgScroll.laOgFxFlt[lnInd,6])
    
    
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
    *!*      CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUTTKTH.CUTTKT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6])) .AND. USED(ALLTRIM(laOgFxFlt[lnInd,6]))
    *!*        lcSeltdCT = ALLTRIM(laOgFxFlt[lnInd,6])
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

  ENDCASE
ENDFOR

*--End of lfCretFltr.
*!**************************************************************************
*! Name      : lfRtrvRec
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Retriev records from the data files.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfRtrvRec()
*!**************************************************************************
FUNCTION lfRtrvRec

*-- Retriev POSHDR Records.
lnFilePos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'POSHDR.PO'),1)

lcSqlStPosHDr = "Select * from Poshdr(index=Poshdr) "
lcSqlStPosLn  = "Select * from POSLN(index=Posln) "

*-- In case the user preview all the records..
llPoFlag  = USED(loOgScroll.laOGFxFlt[lnFilePos,6]) and RECCOUNT(loOgScroll.laOGFxFlt[lnFilePos,6])> 0  && flag indicate user select Po#
IF !llPoFlag
  lcwherePoshdr = WhereCondtion("POSHDR")
  llSqlErro = loDBFPoshdr.SqlRun(lcSqlStPosHDr + lcwherePoshdr ,"CursorPoshdr",.T.)
  SELECT CursorPoshdr
  SCAN
    SCATTER MEMVAR MEMO
    INSERT INTO (lcPoshdr) FROM MEMVAR
  ENDSCAN
  
  lcwherePosln = WhereCondtion("POSLN")
  llSqlErro = loDBFPosLn.SqlRun(lcSqlStPosLn + lcwherePosln ,"CursorPOSLN",.T.)
  SELECT CursorPOSLN
  SCAN
    SCATTER MEMVAR MEMO
    INSERT INTO (lcPosLn) FROM MEMVAR
  ENDSCAN

ELSE  *-- If there is selected PO's.

  SELECT (loOgScroll.laOGFxFlt[lnFilePos,6])
  SCAN
    lcwherePoshdr = WhereCondtion("POSHDR")
    llSqlErro = loDBFPoshdr.SqlRun(lcSqlStPosHDr + lcwherePoshdr + "AND PO='" + PO + "'" ,"CursorPoshdr",.T.)
    SELECT CursorPoshdr
    SCATTER MEMVAR MEMO
    INSERT INTO (lcPoshdr) FROM MEMVAR

    *--Collect Po Lines
    IF RECCOUNT('CursorPoshdr') > 0
      SELECT CursorPoshdr
      lcwherePosln = WhereCondtion("POSLN")
      llSqlErro = loDBFPosLn.SqlRun(lcSqlStPosLn + lcwherePosln + "AND PO='" + PO + "'" ,"CursorPOSLN",.T.)
      SELECT CursorPOSLN
      SCAN
        SCATTER MEMVAR MEMO
        SELECT (lcPosLn)
        INSERT INTO (lcPosLn) FROM MEMVAR

      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF

*--End of lfRtrvRec.
*!*************************************************************
*! Name      : WhereCondtion
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : function to create the filter in where condtion 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : lcwhere
*!*************************************************************
FUNCTION WhereCondtion
LPARAMETERS DbfName

DO CASE
  CASE DbfName = "POSHDR"
    
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
	  *lcwhere = " Where Poshdr.cbusdocu = 'P' AND Poshdr.cstytype = 'M'" 
    IF oAriaApplication.ActiveModuleID = 'MF'
      lcwhere = " Where Poshdr.cbusdocu = 'P' AND Poshdr.cstytype = 'U'" 
    ELSE
      lcwhere = " Where Poshdr.cbusdocu = 'P' AND Poshdr.cstytype = 'P'" 
    ENDIF 
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
     
  CASE DbfName = "POSLN"
    
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
 	  *lcwhere = " Where POSLN.cbusdocu = 'P' AND POSLN.cstytype = 'M'" 
    IF oAriaApplication.ActiveModuleID = 'MF'
      lcwhere = " Where POSLN.cbusdocu = 'P' AND POSLN.cstytype = 'U'" 
    ELSE
      lcwhere = " Where POSLN.cbusdocu = 'P' AND POSLN.cstytype = 'P'" 
    ENDIF
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[END]
    
  CASE DbfName = "CTKTBOM"
    lcwhere = " Where Ctktbom.cimtyp = '" 
    
  CASE DbfName = "ITEMJRNL"
    lcwhere = " Where Itemjrnl.cinvtype = '0002' AND Style = '"
    
  CASE DbfName = "ITEM"
    lcwhere = " Where Item.cinvtype = '0002' AND Style = '"
    
  CASE DbfName = "STYINVJL"
    lcwhere = " Where Styinvjl.Style = '"
ENDCASE
 
RETURN lcwhere

*--End of WhereCondtion.
*!*************************************************************
*! Name      : lfColctDat
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Function to collect Data.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColctDat()
*!*************************************************************
FUNCTION lfColctDat

PRIVATE llFundTr

llUsePoTm = .F.  && Variable to indicate if the user select specific PO.
llUseCTTm = .F.  && Variable to indicate if the user select specific CT.

*-- Check if we are going to use temp file that holding selected PO from OG.

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
IF oAriaApplication.ActiveModuleID = "PO" AND !EMPTY(lcSeltdPo)
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[END]

IF USED(lcSeltdPo)
  GO TOP IN (lcSeltdPo)
  llUsePoTm = !EOF(lcSeltdPo)
ENDIF

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

*-- Check if we are going to use temp file that holding selected CT from OG.

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
IF oAriaApplication.ActiveModuleID = "MF" AND !EMPTY(lcSeltdCT)
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

  IF USED(lcSeltdCT)
    GO TOP IN (lcSeltdCT)
    llUseCTTm = !EOF(lcSeltdCT)
  ENDIF
  
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

DO CASE
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*    CASE lcRpMakBuy = "B"
*!*      =lfUseTmp(llUseCTTm,"M")
*!*      =lfUseTmp(llUsePOTm,"P")
*!*    CASE lcRpMakBuy = "M"
*!*      =lfUseTmp(llUseCTTm,"M")
*!*    CASE lcRpMakBuy = "P"
*!*      =lfUseTmp(llUsePOTm,"P")
  CASE oAriaApplication.ActiveModuleID = "MF"
    =lfUseTmp(llUseCTTm,"M")
  CASE oAriaApplication.ActiveModuleID = "PO"
    =lfUseTmp(llUsePOTm,"P")
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
ENDCASE

*--End of lfColctDat.
*!*************************************************************
*! Name      : lfUseTmp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Function to colect data from tempfiles or master header 
*!           : file.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUseTmp()
*!*************************************************************
FUNCTION lfUseTmp
PARAMETER llUseTmp,lcCTPO
PRIVATE lnOldAls

lnOldAls = SELECT(0)

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  IF lcCTPO = "M"  && IF calling to select CT.
*!*    IF llUseTmp    && IF we are going to colect data for selected CT in
*!*                   && temp file lcSeltdCT that created from OG.
*!*      SELECT (lcSeltdCT)
*!*      SCAN
*!*        *-- Send the second Parameter to colect data for specific CT.
*!*        =lfColPOMF("CutTktL",CUTTKT)
*!*      ENDSCAN
*!*    ELSE
*!*      *-- Send second parameter empty to colect data for all CT.
*!*      =lfColPOMF("CutTktL","")
*!*    ENDIF
*!*  ELSE  && IF calling to select PO.
*!*    IF llUseTmp  && IF we are going to colect data for selected PO in
*!*                 && temp file lcSeltdPO that created from OG.
*!*      SELECT (lcSeltdPO)
*!*      SCAN
*!*        *-- Send the second Parameter to colect data for specific PO.
*!*        =lfColPOMF(lcPosLn,PO)
*!*      ENDSCAN
*!*    ELSE
*!*      *-- Send second parameter empty to colect data for all PO.
*!*      =lfColPOMF(lcPosLn,"")
*!*    ENDIF
*!*  ENDIF
  IF llUseTmp  && IF we are going to colect data for selected PO in
                && temp file lcSeltdPO that created from OG.
    SELECT (lcSeltdPO)
    SCAN
      *-- Send the second Parameter to colect data for specific PO.
      =lfColPOMF(lcPosLn,PO)
    ENDSCAN
  ELSE
    *-- Send second parameter empty to colect data for all PO.
    =lfColPOMF(lcPosLn,"")
  ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
SELECT(lnOldAls)

*--End of lfUseTmp.
*!*************************************************************
*! Name      : lfColPOMF
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Function to CutTkt/PO Lines.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColPOMF()
*!*************************************************************
FUNCTION  lfColPOMF
PARAMETER lc2ColFrm , lcTrNo

PRIVATE llFundTr , llMf

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  IF lc2ColFrm == "CutTktL"
*!*    USE gcDataDir+(lc2ColFrm) AGAIN ALIAS (lcLinTmNam) IN 0 SHARED
*!*  ELSE
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

  SELECT * FROM (lc2ColFrm) WHERE .T. INTO CURSOR &lcLinTmNam READWRITE 
  SELECT (lcLinTmNam)
  INDEX on cbusdocu + cstytype + po + cinvtype + style + STR(lineno,6) + trancd TAG (lcPosLn)
  
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  ENDIF
*!*  llMf = (lc2ColFrm == "CutTktL")
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

SELECT (lc2ColFrm)
SET ORDER TO &lc2ColFrm
LOCATE

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  lcHdrFl   = IIF(llMf,"CUTTKTH",lcPoshdr)
*!*  llFundTr  = IIF(llMf,SEEK(lcTrNo),SEEK("PM"+lcTrNo))
*!*  lcTrWhCnd = IIF(llMf,"CutTkt","cbusdocu+cstytype+PO")
lcHdrFl   = lcPoshdr
llFundTr  = IIF(oAriaApplication.ActiveModuleID = 'PO',SEEK("PP"+lcTrNo),SEEK("PU"+lcTrNo))
lcTrWhCnd = "cbusdocu+cstytype+PO"
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  IF lcSeason <> ".T."
*!*    lcSeason = lcHdrFl+lcSeason
*!*  ENDIF
*!*  IF lcDivision <> ".T."
*!*    lcDivision = lcHdrFl+lcDivision
*!*  ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

SELECT (lc2ColFrm)
IF llFundTr
  SELECT (lcHdrFl)
  
  *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
  *=IIF(llMf,SEEK(lcTrNo),SEEK("PM"+lcTrNo))
  *  SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,"PM"+lcTrNo);
            FOR IIF(lcRpTRStat='A',.T.,;
            IIF(!llMf .AND. lcRpTRStat="S",&lcHdrFl..Status="S",&lcHdrFl..Status=lcRpTRStat)) .AND. ;
                !EMPTY(&lcHdrFl..Link_Code) .AND. ;
                IIF(!EMPTY(lcRPLnkCod),&lcHdrFl..Link_Code=lcRPLnkCod,.T.)
  *  lcDetTrNo = IIF(llMf,CutTkt,PO)
  =IIF(oAriaApplication.ActiveModuleID = 'PO',SEEK("PP"+lcTrNo),SEEK("PU"+lcTrNo))
  
  *:B608390,2 MMT 01/22/2008 Fix bug of Activity date option is now Displayed in OP Grid[Start]
  *SCAN REST WHILE &lcTrWhCnd = IIF(oAriaApplication.ActiveModuleID = 'PO',"PP"+lcTrNo,"PU"+lcTrNo);
            FOR IIF(lcRpTRStat='A',.T.,;
            IIF((oAriaApplication.ActiveModuleID = 'PO') .AND. lcRpTRStat="S",&lcHdrFl..Status="S",&lcHdrFl..Status=lcRpTRStat)) .AND. ;
                !EMPTY(&lcHdrFl..Link_Code) .AND. ;
                IIF(!EMPTY(lcRPLnkCod),&lcHdrFl..Link_Code=lcRPLnkCod,.T.)
	
	SCAN REST WHILE &lcTrWhCnd = IIF(oAriaApplication.ActiveModuleID = 'PO',"PP"+lcTrNo,"PU"+lcTrNo);
            FOR IIF(lcRpTRStat='A',.T.,;
            IIF((oAriaApplication.ActiveModuleID = 'PO') .AND. lcRpTRStat="S",;
            	&lcHdrFl..Status="S",IIF(lcRpTRStat="U",&lcHdrFl..Status="A",&lcHdrFl..Status=lcRpTRStat))) .AND. ;
                !EMPTY(&lcHdrFl..Link_Code) .AND. ;
                IIF(!EMPTY(lcRPLnkCod),&lcHdrFl..Link_Code=lcRPLnkCod,.T.)
	*:B608390,2 MMT 01/22/2008 Fix bug of Activity date option is now Displayed in OP Grid[End]
	
   lcDetTrNo = PO
  *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
  
    SELECT (lc2ColFrm)
    
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*      =IIF(llMf,SEEK(lcDetTrNo),SEEK("PM"+lcDetTrNo))
*!*      SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcDetTrNo,"PM"+lcDetTrNo);
*!*                FOR lfChkSty(&lc2ColFrm..Style,llMf)
*!*        
*!*        *--- Case detail Form
*!*        lcDetTrNo = IIF(llMf,CutTkt,PO)
    =IIF(oAriaApplication.ActiveModuleID = 'PO',SEEK("PP"+lcDetTrNo),SEEK("PU"+lcDetTrNo))
    SCAN REST WHILE &lcTrWhCnd = IIF(oAriaApplication.ActiveModuleID = 'PO',"PP"+lcDetTrNo,"PU"+lcDetTrNo);
              FOR lfChkSty(&lc2ColFrm..Style,oAriaApplication.ActiveModuleID = 'MF')
           
      *--- Case detail Form
      lcDetTrNo = PO
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
      SELECT &lcRpTmpFil
      WAIT WINDOW "Collecting data for transaction# "+lcDetTrNo  NOWAIT
      
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      *LOCATE FOR Type+CtPoNo = IIF(llMf,'M',"I")+lcDetTrNo
      LOCATE FOR Type+CtPoNo = IIF(oAriaApplication.ActiveModuleID = 'MF','M',"I") + lcDetTrNo
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
      
      IF !FOUND()
        *--- Colect Detail transactions.
        
        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
        *=lfCutDet(IIF(llMf,'M',"I"))
        =lfCutDet(IIF(oAriaApplication.ActiveModuleID = 'MF','M',"I"))
        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
        
      ENDIF
      
      *--- Budget record for Closed Transaction (PO,CT)
      IF &lcHdrFl..Status = 'S' AND &lc2ColFrm..TranCd = '1'
        PRIVATE lnTmInd , lcTmInd , lnCloseQty
        lnCloseQty = 0
        *B608489,1 WAM 03/23/2008 Show cost elements 6 & 7
        *FOR lnTmInd = 1 TO 5
        FOR lnTmInd = 1 TO 7
        *B608489,1 WAM 03/23/2008 (End)
          lcTmInd = STR(lnTmInd,1)
          *B608489,1 WAM 03/23/2008 Get cost in base currency
          *lnCloseQty = lnCloseQty+(&lcHdrFl..nfActCost&lcTmInd - &lcHdrFl..nfLanCost&lcTmInd)
          lnCloseQty = lnCloseQty+(&lcHdrFl..NACT_COST&lcTmInd - &lcHdrFl..NLAN_COST&lcTmInd)
          *B608489,1 WAM 03/23/2008 (End)
        ENDFOR
        SELECT &lcRpTmpFil
        
        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
        *LOCATE FOR CtPoNo = lcDetTrNo AND Type = IIF(llMf,'M',"I") AND TranType = 'S'
        LOCATE FOR CtPoNo = lcDetTrNo AND Type = IIF(oAriaApplication.ActiveModuleID = 'MF','M',"I") AND TranType = 'S'
        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
        
        IF !FOUND()
          SELECT (lcTmpNam)
          SET ORDER TO GlDistNo DESCENDING
          ldClDate = IIF(SEEK(lcDetTrNo+'JC',lcTmpNam),&lcTmpNam..Tran_Date,{})
          
          *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
          *IF BETWEEN(ldClDate,ldLowDat,ldHigDat)
          *B608489,1 WAM 03/23/2008 Show PO closing entries
          *IF !EMPTY(ldLowDat) AND !EMPTY(ldHigDat) AND BETWEEN(ldClDate,ldLowDat,ldHigDat)
          lcDateCond = '.T.'
          DO CASE
            CASE EMPTY(ldLowDat) AND !EMPTY(ldHigDat) 
              lcDateCond = 'ldClDate <= ldHigDat'
            CASE !EMPTY(ldLowDat) AND EMPTY(ldHigDat) 
              lcDateCond = 'ldClDate >= ldLowDat'
            CASE !EMPTY(ldLowDat) AND !EMPTY(ldHigDat) 
              lcDateCond = 'BETWEEN(ldClDate,ldLowDat,ldHigDat)'
          ENDCASE              
          IF &lcDateCond
          *B608489,1 WAM 03/23/2008 (End)

          *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
          
            SELECT &lcRpTmpFil
            APPEND BLANK
            
            *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
            *REPLACE WIPAcc    WITH IIF(EMPTY(lcRPLnkCod),Gl_Link.GlAcnt,lcGlAcnt),;
                    CtPoNo    WITH lcDetTrNo                              ,;
                    Type      WITH IIF(llMf,'M',"I")                      ,;
                    Desc      WITH ' '                                    ,;
                    cType     WITH '9'                              ,;
                    TranDate  WITH ldClDate                     ,;
                    TranType  WITH 'S'                        ,;
                    Qty       WITH 1                        ,;
                    Cost      WITH lnCloseQty * -1                   ,; 
                    cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor)          ,;
                    cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))

            REPLACE WIPAcc    WITH IIF(EMPTY(lcRPLnkCod),Gl_Link.GlAcnt,lcGlAcnt),;
                    CtPoNo    WITH lcDetTrNo                     				 ,;
                    Type      WITH IIF(oAriaApplication.ActiveModuleID = 'MF','M',"I")             				 ,;
                    Desc      WITH ' '                           				 ,;
                    cType     WITH '9'              			 				 ,;
                    TranDate  WITH ldClDate 					 				 ,;
                    TranType  WITH 'S'							 				 ,;
                    Qty       WITH 1							 				 ,;
                    Cost      WITH lnCloseQty * -1 				 				 ,; 
                    cVendCode WITH IIF(oAriaApplication.ActiveModuleID = 'MF','',&lcHdrFl..Vendor) 				 ,;
                    cVendName WITH IIF(oAriaApplication.ActiveModuleID = 'MF','',lfGetVend(cVendCode))
            *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
            
          ENDIF
        ENDIF
      ENDIF
      
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      *IF !(IIF(UPPER(lc2ColFrm) = UPPER(lcPosLn) , BETWEEN(&lc2ColFrm..Date,ldLowDat,ldHigDat),;
          BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat)) .AND. ;
          IIF(UPPER(lc2ColFrm) = UPPER(lcPosLn) ,&lc2ColFrm..TranCd $"24",&lc2ColFrm..TranCd $"23"))
      IF !(IIF(!EMPTY(ldLowDat) AND !EMPTY(ldHigDat),BETWEEN(IIF(oAriaApplication.ActiveModuleID = 'PO',&lc2ColFrm..Date,&lc2ColFrm..dPostDate),ldLowDat,ldHigDat),.T.);
          AND &lc2ColFrm..TranCd $"24")
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
      
        LOOP
      ENDIF
    ENDSCAN
  ENDSCAN
ENDIF

USE IN  (lcLinTmNam)
RETURN

*-- End of lfColctDat.
*!*************************************************************
*! Name      : lfCutDet
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Collecting data for either C/T or P/O (Detail Form).
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcCtPoT='I' (Imported) or 'M' (Manufactured)
*!*************************************************************
*! Example            :  =lfCutDet('I')
*!*************************************************************
FUNCTION lfCutDet
LPARAMETERS lcCtPoT

*--- lc2ColFrm :: Detail File POSLN
*--- lcHdrFl   :: Header File PODHDR
*--- lcDetTrNo ::  PO#
SET ORDER TO TAG (lc2ColFrm) IN (lc2ColFrm)
IF BETWEEN(RECNO(lc2ColFrm),1,RECCOUNT(lc2ColFrm))
  GO RECNO(lc2ColFrm) IN (lc2ColFrm)
ENDIF

lcSqlStCtktBom = "Select * from CtktBom(index=CtktBom) "
lcwhereCtktBom = WhereCondtion("CTKTBOM")
IF loDBFCtktBom.SqlRun(lcSqlStCtktBom + lcwhereCtktBom + lcCtPoT + "'" + " And cuttkt ='" + lcDetTrNo +"'" ,"CursorCTKTBOM",.T.)
  SELECT CursorCTKTBOM
  LOCATE
  SCAN WHILE cimtyp + cuttkt + typ + cinvtype + item + mfgcode + dyelot = lcCtPoT + lcDetTrNo
    lcType    = Typ
    llFabTrim = (cCatgTyp = 'F') OR (cCatgTyp = 'T' AND Trim_Invt) && fabric or trim.
    llStyComp = (cCatgTyp = 'S')                                   && style component
    strItem   = SUBSTR(Item,1,7)+"      " + SUBSTR(Item , ATC("-" , Item) +1 )
    lcDesc    = IIF(llFabTrim OR llStyComp OR cCatgTyp = 'T' , strItem , Desc)
    lcDesc    = IIF(llStyComp , LEFT(Item , ATC("-" , Item)-1) , lcDesc)
    lcItem    = Item
    lcMfgCode = MfgCode

    *-- Issue & Return
    DO CASE
      CASE llFabTrim          &&--Fabric or Trim
		lcSqlStItemjrnl = "Select * from Itemjrnl(index=StyInvJl) "
        lcwhereItemJrnl = WhereCondtion("ITEMJRNL")
        *B608489,1 WAM 03/23/2008 Get material issues from material journal
        *IF loDBFItemjrnl.SqlRun(lcSqlStItemjrnl + lcwhereItemJrnl + lcItem + "'" + " AND cbusdocu = 'P' AND cstytype = 'U' " ,"CursorItemjrnl",.T.)
        IF loDBFItemjrnl.SqlRun(lcSqlStItemjrnl + lcwhereItemJrnl + lcItem + "'" + " AND cbusdocu = 'P' AND cstytype = '"+IIF(lcCtPoT = "I" , "P" , "U")+"' " ,"CursorItemjrnl",.T.)
        *B608489,1 WAM 03/23/2008 (End)
        
          lcSqlStItem = "Select * from Item(index=Style) "
          lcwhereItem = WhereCondtion("ITEM")

		  IF loDBFItem.SqlRun(lcSqlStItem + lcwhereItem + lcItem + "'" ,"CursorITEM",.T.)
            SELECT CursorItemjrnl

            SCAN FOR ctrcode = lcDetTrNo .AND. IIF(ldLowDat = {}, .T. , BETWEEN(TTOD(DTRDATE),ldLowDat,ldHigDat));
                   .AND. (EMPTY(lcRPLnkCod) OR lcGlAcnt = IIF(EMPTY(cAdjAcct),Gl_Link.GlAcnt,cAdjAcct)) ;
                   .AND. ALLTRIM(cstytype) = IIF(lcCtPoT = "I" , "P" , "U")

              *--- BOMCOST
              SELECT CursorBOMCOST
              LOCATE FOR cimtyp+ctktno+actualize+cbomtype+item+mfgcode+;
                     coprcode+clotno+cisession+crsession =  lcCtPoT + lcDetTrNo ;
                     AND CursorBOMCOST.cisession = CursorItemjrnl.cisession

	          IF (FOUND() AND CursorCTKTBOM.typ = CursorBOMCOST.cBomType) OR !FOUND()
                SELECT (lcRpTmpFil)
                APPEND BLANK 
                
                *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
                *REPLACE WIPAcc    WITH IIF(ISNULL(CursorItemjrnl.CADJACCT),Gl_Link.GlAcnt,CursorItemjrnl.CADJACCT)        ,;
    		                Cost      WITH IIF(CursorItemjrnl.NCOST <> 0 ,CursorItemjrnl.NCOST,CursorItemjrnl.nuntcstbuy/1)   ,;
                        CtPoNo    WITH lcDetTrNo																		  ,;
		        		        Type      WITH lcCtPoT																			  ,;
     	                  Desc      WITH lcDesc																			  ,;
				                cType     WITH lcType															                  ,;
        				        TranDate  WITH TTOD(CursorItemjrnl.DTRDATE)												          ,;
        				        TranType  WITH CursorItemjrnl.CIRTYPE															  ,;
  	      			        llFabric  WITH .T.																				  ,;
                        cVendCode WITH CursorBOMCOST.cVendCode															  ,;
                        cVendName WITH lfGetVend(cVendCode) 															  ,;
   		                  Qty       WITH IIF(CursorItemjrnl.ctrtype='9' AND TranType='R',-1*CursorItemjrnl.nReceived,IIF(CursorItemjrnl.CIRTYPE = 'I' , -1 * CursorItemjrnl.ntotstk , 0))
                 REPLACE WIPAcc    WITH IIF(ISNULL(CursorItemjrnl.CADJACCT),Gl_Link.GlAcnt,CursorItemjrnl.CADJACCT)        ,;
                        Cost      WITH IIF(CursorItemjrnl.NCOST <> 0 ,CursorItemjrnl.NCOST,CursorItemjrnl.nuntcstbuy/1)   ,;
                        CtPoNo    WITH lcDetTrNo                                      ,;
                        Type      WITH lcCtPoT                                        ,;
                         Desc      WITH lcDesc                                        ,;
                        cType     WITH lcType                                                ,;
                        TranDate  WITH TTOD(CursorItemjrnl.DTRDATE)                                  ,;
                        TranType  WITH CursorItemjrnl.CIRTYPE                                ,;
                        llFabric  WITH .T.                                          ,;
                        cVendCode WITH CursorBOMCOST.cVendCode                                ,;
                        cVendName WITH lfGetVend(cVendCode)                                 ,;
                        Qty       WITH IIF(CursorItemjrnl.ctrtype='9' AND TranType='R',-1*CursorItemjrnl.ntotstk,IIF(CursorItemjrnl.CIRTYPE = 'I' , -1 * CursorItemjrnl.ntotstk , 0))

                *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
                
                SELECT (lcTmpCurs)
                APPEND BLANK
                REPLACE CtPoNo   WITH lcDetTrNo              			 ,;
                        Item     WITH SUBSTR(lcItem,1,7)     			 ,;
                        Clr      WITH SUBSTR(Item , ATC("-" , Item) +1 ) ,;
                        lcIssRet WITH CursorItemjrnl.CIRTYPE 			 ,;
                        cSession WITH IIF(ISNULL(CursorItemjrnl.cisession) , SPACE(0) , CursorItemjrnl.cisession)
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
        
      ** Style Component
      CASE llStyComp
      

    		lcSqlStyInvJl = "Select * from StyInvJl(index=StyInvJl) "
        lcwhereStyInvJl = WhereCondtion("STYINVJL")
        
        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
        *IF loDBFStyInvJl.SqlRun(lcSqlStyInvJl + lcwhereStyInvJl + lcItem + "'" ,"CursorSTYINVJL",.T.)
        IF loDBFStyInvJl.SEEK(lcItem,'StyInvJl')
        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
        
          SELECT CursorStyInvJl
          *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
          *!*            LOCATE
          *!*            SCAN
          SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = lcItem
         *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
         
            IF Style = lcItem .AND. CTRCode = lcDetTrNo AND IIF(ldLowDat = {},.T.,BETWEEN(TTOD(DTRDATE),ldLowDat,ldHigDat));
              .AND. (EMPTY(lcRPLnkCod) OR lcGlAcnt = IIF(EMPTY(cAdjAcct),Gl_Link.GlAcnt,cAdjAcct))
       
            SELECT (lcRpTmpFil)
            APPEND BLANK 
            
            *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
            *REPLACE WIPAcc    WITH IIF(EMPTY(CursorStyInvJl.cAdjAcct),Gl_Link.GlAcnt,CursorStyInvJl.cAdjAcct),;
                    CtPoNo    WITH lcDetTrNo					 											 ,;
                    Type      WITH lcCtPoT					     											 ,;
                    Desc      WITH lcDesc						 											 ,;
                    cType     WITH lcType						 											 ,;
                    TranDate  WITH TTOD(CursorStyInvJl.DTRDATE)  											 ,;
                    TranType  WITH CursorStyInvJl.CIRTYPE        											 ,;
                    Qty       WITH -1 * CursorStyInvJl.nTotStk   											 ,;
                    Cost      WITH CursorStyInvJl.nCost         											 ,;
	                cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor) 											 ,;
                    cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
                    
            REPLACE WIPAcc    WITH IIF(EMPTY(CursorStyInvJl.cAdjAcct),Gl_Link.GlAcnt,CursorStyInvJl.cAdjAcct),;
                    CtPoNo    WITH lcDetTrNo                                  ,;
                    Type      WITH lcCtPoT                                      ,;
                    Desc      WITH lcDesc                                    ,;
                    cType     WITH lcType                                    ,;
                    TranDate  WITH TTOD(CursorStyInvJl.DTRDATE)                         ,;
                    TranType  WITH CursorStyInvJl.CIRTYPE                               ,;
                    Qty       WITH -1 * CursorStyInvJl.nTotStk                          ,;
                    Cost      WITH CursorStyInvJl.nCost                                ,;
                    cVendCode WITH IIF(oAriaApplication.ActiveModuleID = 'MF','',&lcHdrFl..Vendor)                        ,;
                    cVendName WITH IIF(oAriaApplication.ActiveModuleID = 'MF','',lfGetVend(cVendCode))
            *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
            
            ENDIF
          ENDSCAN
        ENDIF
    ENDCASE   

    ** Receive
    SELECT (lcLinTmNam)
    
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
    *!*      IF lcCtPoT = 'M'
    *!*        SET ORDER TO CUTTKTL
    *!*      ELSE
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
    SET ORDER TO (lcPosLn)
    
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
    *ENDIF
    *=SEEK(IIF(lcCtPoT = 'M',"","PM")+lcDetTrNo))
    *SCAN REST WHILE IIF(lcCtPoT='M',CutTkt,"PM"+PO) = IIF(lcCtPoT='M',"","PM") + lcDetTrNo FOR TranCd = '2'
    =SEEK(IIF(lcCtPoT = 'M',"PU","PP")+lcDetTrNo))
    SCAN REST WHILE IIF(lcCtPoT='M',"PU"+PO,"PP"+PO) = IIF(lcCtPoT='M',"PU","PP") + lcDetTrNo FOR TranCd = '2'
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
      strLinDate = EVALUATE(lcLinTmNam+'.Date')
      
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
      IF !EMPTY(ldLowDat) AND !EMPTY(ldHigDat) AND !(IIF(lcCtPoT='M',  BETWEEN(&lcLinTmNam..dPostDate,ldLowDat,ldHigDat),;
          BETWEEN(&lcLinTmNam..Date,ldLowDat,ldHigDat))) 
      *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
      
        LOOP
      ENDIF
         
      lcRSession = cRSession
      lcSqlStBomLine = "Select * from BomLine(index=BomLine) "
  	  lcwhereBlin = " Where BomLine.cimtyp = '" + lcCtPoT + "'"  + " AND BomLine.ctype = '2' AND BomLine.ctktno = '" + lcDetTrNo + "'"
	    IF loDBFBomLine.SqlRun(lcSqlStBomLine + lcwhereBlin ,"CursorBOMLINE",.T.)
        SELECT CursorBOMLINE
        SCAN 
          *B608430,1 WAM 02/12/2008 Get bom lines related to PO line number
          *IF cBomTyp + Style + Item + MfgCode + cRSession =;
               lcType + &lcLinTmNam..Style + lcItem + lcMfgCode + lcRSession
          IF cBomTyp + Style + Item + MfgCode + cRSession =;
               lcType + &lcLinTmNam..Style + lcItem + lcMfgCode + lcRSession AND LINENO = &lcLinTmNam..LineNo
          *B608430,1 WAM 02/12/2008 (End)

            SELECT (lcRpTmpFil)
            APPEND BLANK
            str2LinDate = EVALUATE(lcLinTmNam+'.Date')
            str2LinDate = TTOD(str2LinDate)
            
            *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
            *REPLACE WIPAcc    WITH IIF(EMPTY(lcRPLnkCod),Gl_Link.GlAcnt,lcGlAcnt),;
                    CtPoNo    WITH lcDetTrNo                                           ,;
                    Type      WITH lcCtPoT                                             ,;
                    Desc      WITH lcDesc                       					   ,;
                    cType     WITH CursorBOMLINE.CBomTyp       						   ,;
                    TranDate  WITH str2LinDate 										   ,;
                    TranType  WITH 'E'                          					   ,;
                    Qty       WITH -1 * CursorBOMLINE.ItemQty   					   ,;
		            cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor) 					   ,;
                    cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))				   ,;
                    Cost     WITH IIF(lcCtPoT = 'M' , CursorBOMLINE.UnitCost ,;
                                  IIF(&lcPoSHdr..Rate <> 0 AND CursorBOMLINE.cCatgTyp = 'P' ,;
                                  CursorBOMLINE.UnitCost/&lcPoSHdr..Rate , CursorBOMLINE.UnitCost))

            REPLACE WIPAcc    WITH IIF(EMPTY(lcRPLnkCod),Gl_Link.GlAcnt,lcGlAcnt),;
                    CtPoNo    WITH lcDetTrNo                                           ,;
                    Type      WITH lcCtPoT                                             ,;
                    Desc      WITH lcDesc                                    ,;
                    cType     WITH CursorBOMLINE.CBomTyp                      ,;
                    TranDate  WITH str2LinDate                        ,;
                    TranType  WITH 'E'                                       ,;
                    Qty       WITH -1 * CursorBOMLINE.ItemQty                ,;
                    cVendCode WITH IIF(oAriaApplication.ActiveModuleID = 'MF' ,'',&lcHdrFl..Vendor)              ,;
                    cVendName WITH IIF(oAriaApplication.ActiveModuleID = 'MF' ,'',lfGetVend(cVendCode))           ,;
                    Cost     WITH IIF(lcCtPoT = 'M' , CursorBOMLINE.UnitCost ,;
                                  IIF(&lcPoSHdr..Rate <> 0 AND CursorBOMLINE.cCatgTyp = 'P' ,;
                                  CursorBOMLINE.UnitCost/&lcPoSHdr..Rate , CursorBOMLINE.UnitCost))
            *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

            *B608489,1 WAM 03/23/2008 Get currency and exchange retes from BOMLINE
*!*	            IF lcCtPoT = 'M'
*!*	              lnEqCose = CursorBOMLINE.UnitCost
*!*	            ELSE
*!*	              IF CursorBOMLINE.cCatgTyp = 'P'
*!*	                lcExSin2 = ''
*!*	                lcExSin1 = gfGetExSin(@lcExSin2,&lcPoSHdr..CPriceCur)
*!*	                 lnOldAlias = SELECT(0)
*!*	                 
*!*	                  *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*	                 *IF SEEK("PM" + &lcPoshdr..cstytype+ &lcPoshdr..po , lcPosLn)
*!*	                 IF SEEK("P" + &lcPoshdr..cstytype+ &lcPoshdr..po , lcPosLn)
*!*	                  *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
*!*	                 
*!*	                   SELECT (lcPosLn)
*!*	                   
*!*	                   *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*	                   *SCAN REST WHILE "PM"+cstytype + po + style + STR(lineno,6) + trancd = ;
*!*	                        "PM"+ &lcPoshdr..cstytype + &lcPoshdr..po                     ;
*!*	                        FOR TRANCD = '2' AND &lcPosLn..cRsession = CursorBOMLINE.cRsession
*!*	                   SCAN REST WHILE "P"+cstytype + po + style + STR(lineno,6) + trancd = ;
*!*	                        "P"+ &lcPoshdr..cstytype + &lcPoshdr..po                     ;
*!*	                        FOR TRANCD = '2' AND &lcPosLn..cRsession = CursorBOMLINE.cRsession
*!*	                   *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
*!*	                   
*!*	                     lnCostRate = &lcPosLn..nLanprrat
*!*	                   ENDSCAN
*!*	                   lnEqCose = CursorBOMLINE.UnitCost &lcExSin1. lnCostRate
*!*	                 ENDIF
*!*	                SELECT(lnOldAlias)
*!*	              ELSE
*!*	                IF CursorBOMLINE.cCatgTyp='D'
*!*	                  lcExSin2 = ''
*!*	                  lcExSin1 = gfGetExSin(@lcExSin2,&lcPoSHdr..CDutyCur)
*!*	                   lnOldAlias = SELECT(0)
*!*	                   
*!*	                   *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*	                   *IF SEEK("PM"+&lcPoshdr..cstytype+ &lcPoshdr..po , lcPosLn)
*!*	                   IF SEEK("P"+&lcPoshdr..cstytype+ &lcPoshdr..po , lcPosLn)
*!*	                   *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
*!*	                   
*!*	                     SELECT (lcPosLn)
*!*	                     
*!*	                     *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*	                     *SCAN REST WHILE "PM"+cstytype+po+style+STR(lineno,6)+trancd = ;
*!*	                          "PM"+&lcPoshdr..cstytype + &lcPoshdr..po                     ;
*!*	                          FOR TRANCD='2' AND &lcPosLn..cRsession = CursorBOMLINE.cRsession
*!*	                      SCAN REST WHILE "P"+cstytype+po+style+STR(lineno,6)+trancd = ;
*!*	                          "P"+&lcPoshdr..cstytype + &lcPoshdr..po                     ;
*!*	                          FOR TRANCD='2' AND &lcPosLn..cRsession = CursorBOMLINE.cRsession
*!*	                     *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
*!*	                     
*!*	                       lnCostRate = &lcPosLn..nLanDuRat
*!*	                     ENDSCAN
*!*	                     lnEqCose = CursorBOMLINE.UnitCost &lcExSin1. lnCostRate
*!*	                   ENDIF
*!*	                  SELECT(lnOldAlias)
*!*	                ELSE
*!*	                  lnEqCose = CursorBOMLINE.UnitCost
*!*	                ENDIF
*!*	              ENDIF
*!*	            ENDIF

            IF ISNULL(CursorBOMLINE.cCurrCode) OR EMPTY(CursorBOMLINE.cCurrCode)
              DO CASE
                CASE CursorBOMLINE.cCatgTyp = 'P'
                  IF llMulCurr AND !EMPTY(&lcPoshdr..cPriceCur)
                    lcCurrCode = &lcPosHdr..cPriceCur
                    lnExRate   = IIF(&lcPosHdr..nPriceRat=0,1, &lcPosHdr..nPriceRat)
                    lnCurrUnit = IIF(&lcPosHdr..nCurrUnit=0,1, &lcPosHdr..nCurrUnit)
                  ELSE
                    lcCurrCode = oAriaApplication.BaseCurrency
                    lnExRate   = 1
                    lnCurrUnit = 1
                  ENDIF  
                CASE INLIST(CursorBOMLINE.cCatgTyp,'D','M')
                  IF llMulCurr AND !EMPTY(&lcPosHdr..cDutyCur)
                    lcCurrCode = &lcPosHdr..cDutyCur
                    lnExRate   = IIF(&lcPosHdr..nDutyRat=0,1, &lcPosHdr..nDutyRat)
                    lnCurrUnit = IIF(&lcPosHdr..nDCurUnit=0,1, &lcPosHdr..nDCurUnit)
                  ELSE
                    lcCurrCode = oAriaApplication.BaseCurrency
                    lnExRate   = 1
                    lnCurrUnit = 1
                  ENDIF  
                OTHERWISE
                  lcCurrCode = oAriaApplication.BaseCurrency
                  lnExRate   = 1
                  lnCurrUnit = 1
              ENDCASE
            ELSE
              lcCurrCode = CursorBOMLINE.cCurrCode
              lnExRate   = IIF(ISNULL(CursorBOMLINE.nExRate) OR CursorBOMLINE.nExRate=0,1,CursorBOMLINE.nExRate)
              lnCurrUnit = IIF(ISNULL(CursorBOMLINE.nCurrUnit) OR CursorBOMLINE.nCurrUnit=0,1,CursorBOMLINE.nCurrUnit)
            ENDIF
            STORE '/' TO lcExSign, lcUntSin
            lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
            lnEqCose = CursorBOMLINE.UnitCost &lcExSign lnExRate &lcUntSin lnCurrUnit
            *B608489,1 WAM 03/23/2008 (End)

            REPLACE Cost WITH lnEqCose
          ENDIF
        ENDSCAN
      ENDIF 
    ENDSCAN
  ENDSCAN

  *--SELECT BomCost
  SELECT CursorBOMCOST
  loDBFBomCost.setorder("Pobomcls")
  =SEEK(lcCtPoT + lcDetTrNo)
  IF BETWEEN(RECNO(lc2ColFrm),1,RECCOUNT(lc2ColFrm))
    GO RECNO(lc2ColFrm) IN (lc2ColFrm)
  ENDIF

  SCAN REST WHILE cimtyp + ctktno + actualize + cbomtype + cinvtype + item + mfgcode + coprcode + clotno + cisession + crsession + cvendcode + capinvno = ;
             lcCtPoT + lcDetTrNo ;
       FOR   IIF(ldLowDat = {} , .T. , BETWEEN(TTOD(dTranDate),ldLowDat,ldHigDat)) .AND. cCostType $ "MPD";
       AND  (EMPTY(lcRPLnkCod) OR lcGlAcnt = IIF(EMPTY(cWipAcnt) , Gl_Link.GlAcnt , cWipAcnt ))
  
    SELECT (lcRpTmpFil)
    APPEND BLANK
    strBomCostDate = CursorBomCost.dTranDate
    REPLACE WIPAcc    WITH IIF(EMPTY(CursorBOMCOST.cWipAcnt),Gl_Link.GlAcnt,CursorBOMCOST.cWipAcnt) ,;
            CtPoNo    WITH lcDetTrNo																	  ,;
            Type      WITH lcCtPoT																		  ,;
            cType     WITH CursorBOMCOST.cbomtype														  ,;
            Desc      WITH ALLTRIM(CursorBomCost.cApInvNo) + '  ' + ALLTRIM(CursorBomCost.cVendCode)	  ,;
            TranDate  WITH TTOD(strBomCostDate) 														  ,;
            Qty       WITH CursorBomCost.nTotQty														  ,;
            Cost      WITH IIF(CursorBomCost.nTotQty <> 0 , CursorBomCost.nTotCst/CursorBomCost.nTotQty,0),;
            TranType  WITH 'V' 																			  ,;
            cVendCode WITH CursorBomCost.cVendCode														  ,;
            cVendName WITH lfGetVend(cVendCode)

    IF lcCtPoT = 'M'
      lnEqCose = IIF(CursorBomCost.nTotQty <> 0 , CursorBomCost.nTotCst/CursorBomCost.nTotQty , 0)
    ELSE
      IF CursorBomCost.cCostType = 'P'
        lcExSin2 = ''
        lcExSin1 = gfGetExSin(@lcExSin2,&lcPoSHdr..CPriceCur)

        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
        * IF SEEK(CursorBomCost.cApInvNo + CursorBomCost.cVendCode ,'APINVHDR')
    		IF loDBFAPINVHDR.SEEK(CursorBomCost.cApInvNo + CursorBomCost.cVendCode )
        *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
          lnEqCose = IIF(CursorBomCost.nTotQty <> 0 ,(CursorBomCost.nTotCst &lcExSin1. APINVHDR.nExrate) / CursorBomCost.nTotQty , 0)
        ENDIF
      ELSE
        IF CursorBomCost.cCostType='D'
          *B608540,1 WAM 05/05/2008 Get duty currency from AP invoice file
*!*	          lcExSin2 = ''
*!*	          lcExSin1 = gfGetExSin(@lcExSin2,&lcPoSHdr..CDutyCur)
*!*	          *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*	          *IF SEEK(CursorBomCost.cApInvNo + CursorBomCost.cVendCode ,'APINVHDR')
*!*	    		  IF loDBFAPINVHDR.SEEK(CursorBomCost.cApInvNo + CursorBomCost.cVendCode )
*!*	          *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
*!*	            lnEqCose = IIF(CursorBomCost.nTotQty <> 0 ,(CursorBomCost.nTotCst &lcExSin1. ApinvHdr.nExrate) / CursorBomCost.nTotQty , 0)
*!*	          ENDIF

          IF loDBFAPINVHDR.SEEK(CursorBomCost.cApInvNo + CursorBomCost.cVendCode )
            lcExSin2 = ''
            lcExSin1 = gfGetExSin(@lcExSin2,ApinvHdr.cCurrCode)
            lnEqCose = IIF(CursorBomCost.nTotQty <> 0 ,(CursorBomCost.nTotCst &lcExSin1. ApinvHdr.nExrate &lcExSin2. ApinvHdr.nCurrUnit) / CursorBomCost.nTotQty , 0)
          ENDIF
          *B608540,1 WAM 05/05/2008 (End)
        ELSE
          lnEqCose = IIF(CursorBomCost.nTotQty <> 0 , CursorBomCost.nTotCst/CursorBomCost.nTotQty , 0 )
          *B608540,1 WAM 05/05/2008 Convert MFG cost to base currency
          IF loDBFAPINVHDR.SEEK(CursorBomCost.cApInvNo + CursorBomCost.cVendCode )
            lcExSin2 = ''
            lcExSin1 = gfGetExSin(@lcExSin2,ApinvHdr.cCurrCode)
            lnEqCose = IIF(CursorBomCost.nTotQty <> 0 ,(CursorBomCost.nTotCst &lcExSin1. ApinvHdr.nExrate &lcExSin2. ApinvHdr.nCurrUnit) / CursorBomCost.nTotQty , 0)
          ENDIF
          *B608540,1 WAM 05/05/2008 (End)
        ENDIF
      ENDIF
    ENDIF
    REPLACE Cost WITH lnEqCose
  ENDSCAN
ENDIF
SELECT (lc2ColFrm)
RETURN

*--End of lfCutDet.
*!*************************************************************
*! Name      : lfStatus
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Get the C/T or P/O status to be printed in the report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStatus()
*!*************************************************************
FUNCTION lfStatus
PARAMETER lcStatus

DO CASE
  CASE lcStatus='A'
    RETURN('Actualize')
  CASE lcStatus='C'
    RETURN('Complete')
  CASE lcStatus='H'
    RETURN('Hold')
  CASE lcStatus='O'
    RETURN('Open')
  CASE lcStatus='S'
    RETURN('Close')
  CASE lcStatus='X'
    RETURN('Cancel')
  OTHER
    RETURN('')
ENDCASE  

*--End of lfStatus.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Validation function for Validating fabric Code
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

lcFabObj = OGSYS18()
lcFab    = EVALUATE(OGSYS18())
IF '?' $ lcFab .OR. !EMPTY(lcFab)
  SELECT FABRIC
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    =FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ENDIF
SET ORDER TO FABRIC IN FABRIC

*--End of lfvFabric.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
   
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Option Grid When function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
IF oAriaApplication.ActiveModuleID = 'PO'
  lcPoCTFlds = "PO :R :H= 'PO#' , STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
               " APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' ,"+;
               " Complete :R :H= 'Complete' ,  Open :R :H= 'Open' :P='999999' ,"+;
               " POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'"
ELSE
  lcPoCTFlds = "PO :R :H='CutTkt', STYLE :R :H='Style', STATUS :R :H='Status',"+;
               " ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
               " CDIVISION :R :H='Division', NSTYORDER :R :H='Budget' :P = '999999',"+;
               "RECEIVE :R :H='Received' :P = '999999', DAMAGE :R :H='Damaged' :P = '999999',"+;
               " OPEN :R :H='Open' :P = '999999'"
ENDIF 
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]


*lcTablename, lcTagName, lcViewAlias, lnDataSession, lcComp_Id, llFastTable

*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))

*--Make Temp. File From Item Location File
IF !llFirstTim
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC " 
  lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"
  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  llFirstTim = .T.
ENDIF 

IF lnResult1 >=1
  lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
  =CURSORSETPROP("Buffering",3,lcFabDye)
  SELECT (lcFabDye)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
ENDIF 

*--Poshdr File
IF TYPE('loDBFPoshdr') <> 'O'
  loDBFPoshdr = CreateObject("RemoteTable","POSHDR",,"CursorPoshdr",SET("DATASESSION"),,.T.)
  SELECT * FROM CursorPoshdr WHERE .F. INTO CURSOR &lcPoshdr READWRITE
  SELECT (lcPoshdr)
  INDEX on cbusdocu + cstytype + po TAG (lcPoshdr)
ENDIF

*--PosLn File
IF TYPE('loDBFPosLn') <> 'O'
  loDBFPosLn = CreateObject("RemoteTable","POSLN",,"CursorPOSLN",SET("DATASESSION"),,.T.)
  SELECT * FROM CursorPOSLN WHERE .F. INTO CURSOR &lcPosLn READWRITE 
  SELECT (lcPosLn)
  INDEX on cbusdocu + cstytype + po + cinvtype + style + STR(lineno,6) + trancd TAG (lcPosLn)
ENDIF

*--CtktBom File
IF TYPE('loDBFCtktBom') <> 'O'
  loDBFCtktBom = CreateObject("RemoteTable","CTKTBOM",,"CursorCTKTBOM",SET("DATASESSION"),,.T.)
  SELECT * FROM CursorCTKTBOM WHERE .F. INTO CURSOR &lcCTKTBOM READWRITE 
  SELECT (lcCTKTBOM)
  INDEX on cimtyp + cuttkt + typ + cinvtype + item + mfgcode + dyelot TAG (lcPosLn)
ENDIF

*--Itemjrnl File
IF TYPE('loDBFItemjrnl') <> 'O'
  loDBFItemjrnl = CreateObject("RemoteTable","Itemjrnl",,"CursorItemjrnl",SET("DATASESSION"),,.T.)
ENDIF

*--BomCost File
IF TYPE('loDBFBomCost') <> 'O'
  loDBFBomCost = CreateObject("RemoteTable","BOMCOST",,"CursorBOMCOST",SET("DATASESSION"),,.T.)
  lcSqlStBomCost = "Select * from Bomcost(index=pobomcls)"
  llSqlErro = loDBFBomCost.SqlRun(lcSqlStBomCost ,"CursorBOMCOST",.T.)
ENDIF

*--BomLine File
IF TYPE('loDBFBomLine') <> 'O'
  loDBFBomLine = CreateObject("RemoteTable","BOMLINE",,"CursorBOMLINE",SET("DATASESSION"),,.T.)
ENDIF


*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
IF TYPE('loDBFStyInvJl') <> 'O'
  loDBFStyInvJl= CreateObject("RemoteTable","StyInvJl",'StyInvJl',"CursorStyInvJl",SET("DATASESSION"),,.T.)
ENDIF

IF TYPE('loDBFAPINVHDR') <> 'O'
  loDBFAPINVHDR = CreateObject("RemoteTable","APINVHDR","INVVEND","APINVHDR",SET("DATASESSION"),,.T.)
ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

*--Fabric File
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
*!*	IF TYPE('loDBFabric') <> 'O'
*!*	  loDBFabric = CreateObject("RemoteTable","FABRIC",,"CursorFABRIC",SET("DATASESSION"),,.T.)
*!*	ENDIF
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
*--Item File
IF TYPE('loDBFItem') <> 'O'
  loDBFItem = CreateObject("RemoteTable","ITEM",,"CursorITEM",SET("DATASESSION"))
ENDIF

lnClrSgPo = ASUBSCRIPT(loOgScroll.laOgFxFlt,ASCAN(loOgScroll.laOgFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]
DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(loOgScroll.laOgFxFlt,1) + lnClrSgPo] = .T.
    =lfOGShowGet('laOgFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')

  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(loOgScroll.laOgFxFlt,1) + lnClrSgPo] = .F.
    =lfOGShowGet('laOgFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ENDCASE

IF .f.
lnDatapos = ASCAN(laOGFxFlt,'CUTTKTH.ENTERED')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate - DAY(gdSysDate)+1)) + '|' + ALLTRIM(DTOC(gdSysDate))
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF

lnDataposP = ASCAN(loOgScroll.laOGFxFlt,'POSHDR.ENTERED')
IF lnDataposP > 0
  lnDataposP = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnDataposP,1)
  lcStr = ALLTRIM(DTOC(gdSysDate - DAY(gdSysDate)+1)) + '|' + ALLTRIM(DTOC(gdSysDate))
  IF EMPTY(loOgScroll.laOGFxFlt[lnDataposP,6])
    loOgScroll.laOGFxFlt[lnDataposP,6] = lcStr
  ENDIF
ENDIF
ENDIF

*B608420,1 WAM 02/05/2008 Show cost types 6 & 7 [T20080125.0001]
*!*	DECLARE laSetups[10,2]
*!*	laSetups[1,1]  = 'M_C'+"M"+'SLBL1'
*!*	laSetups[2,1]  = 'M_C'+"M"+'SLBL2'
*!*	laSetups[3,1]  = 'M_C'+"M"+'SLBL3'
*!*	laSetups[4,1]  = 'M_C'+"M"+'SLBL4'
*!*	laSetups[5,1]  = 'M_C'+"M"+'SLBL5'
*!*	laSetups[6,1]  = 'M_C'+"I"+'SLBL1'
*!*	laSetups[7,1]  = 'M_C'+"I"+'SLBL2'
*!*	laSetups[8,1]  = 'M_C'+"I"+'SLBL3'
*!*	laSetups[9,1]  = 'M_C'+"I"+'SLBL4'
*!*	laSetups[10,1] = 'M_C'+"I"+'SLBL5'

DECLARE laSetups[14,2]
laSetups[1,1]  = 'M_C'+"M"+'SLBL1'
laSetups[2,1]  = 'M_C'+"M"+'SLBL2'
laSetups[3,1]  = 'M_C'+"M"+'SLBL3'
laSetups[4,1]  = 'M_C'+"M"+'SLBL4'
laSetups[5,1]  = 'M_C'+"M"+'SLBL5'
laSetups[6,1]  = 'M_C'+"M"+'SLBL6'
laSetups[7,1]  = 'M_C'+"M"+'SLBL7'

laSetups[8,1]  = 'M_C'+"I"+'SLBL1'
laSetups[9,1]  = 'M_C'+"I"+'SLBL2'
laSetups[10,1] = 'M_C'+"I"+'SLBL3'
laSetups[11,1] = 'M_C'+"I"+'SLBL4'
laSetups[12,1] = 'M_C'+"I"+'SLBL5'
laSetups[13,1] = 'M_C'+"I"+'SLBL6'
laSetups[14,1] = 'M_C'+"I"+'SLBL7'
*B608420,1 WAM 02/05/2008 (End)

=gfGetMemVar(@laSetups,gcAct_Comp)

*B608420,1 WAM 02/05/2008 Show cost types 6 & 7 [T20080125.0001]
*!*	FOR lnInd = 1 TO 5
*!*	  lcInd = STR(lnInd,1)
*!*	  lcMDesc&lcInd = laSetups[lnInd,2]
*!*	  lcIDesc&lcInd = laSetups[lnInd+5,2]
*!*	ENDFOR

FOR lnInd = 1 TO 7
  lcInd = STR(lnInd,1)
  lcMDesc&lcInd = laSetups[lnInd,2]
  lcIDesc&lcInd = laSetups[lnInd+7,2]
ENDFOR
*B608420,1 WAM 02/05/2008 (End)

*--End of lfwOGWhen
*!*************************************************************
*! Name      : lfvLnkCod
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Validate link code
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvLnkCod()
*!*************************************************************
FUNCTION lfvLnkCod

lcCodeVar = OGSYS18()                  && Varible to hold  the name of the memory variable used to create the current GET field
lcCodeVal = EVALUATE(OGSYS18())        && Varible to hold  the value of the current GET field

SET ORDER TO Gl_link1 IN GL_Link
IF !EMPTY(lcCodeVal)
  SELECT GL_Link
  *--- linktype + link_code
  IF !SEEK("05" + ALLTRIM(lcCodeVal))
    llNothing  = lfGLLinkBrw()
  ELSE
    llNothing = .T.
  ENDIF
  lcCodeVar = IIF(llNothing,GL_LINK.Link_Code,'')
  lcRPLnkCod = lcCodeVar
  =LFOGSHOWGET('lcRPLnkCod')

  SELECT GL_LINK
  SET ORDER TO Gl_link IN GL_Link
  IF SEEK(lcCodeVar+'013')
    lcRpGlAct = GL_LINK.GlAcnt
  ELSE
    lcRpGlAct = SPACE(3)
  ENDIF
ENDIF

*--End of lfvLnkCod.
*!*************************************************************
*! Name      : lfGLLinkBrw
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Browse link codes
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGLLinkBrw()
*!*************************************************************
FUNCTION lfGLLinkBrw

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)
lcTag      = ORDER()

lcFields   = "link_code"
lcBrFields = "Link_Code    :H='Link code',"+;
             "Description= GL_LINK.LinkDesc :H='Description'"

lcFile_Ttl  = 'Link codes'
lc2Sek = '05'
SELECT GL_LINK
INDEX ON linktype + link_code TAG fabraca UNIQUE
LOCATE
DECLARE laTemp[1]

llReturn  = gfBrows('lc2Sek', 'LinkType,link_code', 'laTemp')

SELECT(lnCurAlias)
SET ORDER TO (lcTag)

RETURN llReturn

*--End of lfGLLinkBrw.
*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : To set relation on or off when running the in range function 
*!             of selecting PO in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
LPARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
    IF oAriaApplication.ActiveModuleID = 'PO'
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
      SET ORDER TO VENCODE IN APVENDOR
      SELECT POSHDR
      SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
    
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]  
    ENDIF 
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
  CASE lcParm = 'R'
  
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
    IF oAriaApplication.ActiveModuleID = 'PO'
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
      SELECT POSHDR
      SET RELATION TO
      
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
    ENDIF 
    *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
    
ENDCASE

*--End of lfsrvTrans.
*!*************************************************************
*! Name      : lfClrRed
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Function to clear read in the option gris.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClrRed()
*!*************************************************************
FUNCTION lfClrRed

CLEARREAD()

*--End of lfClrRed.
*!*************************************************************
*! Name      : lfGetVend
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Function to return Vendor Name from vendor file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetVend()
*!*************************************************************
FUNCTION lfGetVend
LPARAMETERS lcVendCode

PRIVATE lcOldAlias,lcVendName
lcVendName = ''
lcOldAlias = ALIAS()

IF !USED("APVENDOR")
  =gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VENCODE','SH')
ENDIF

SELECT APVENDOR
SET ORDER TO VENCODE
IF SEEK(lcVendCode)
  lcVendName = APVENDOR.cVenComp
ENDIF

SELECT (lcOldAlias)
RETURN lcVendName

*--End of lfGetVend.
*!**************************************************************************
*! Name      : lfWorkFile
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Create work cursors.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfWorkFile()
*!**************************************************************************
FUNCTION lfWorkFile

*--Close temporary files if it were opened before.
IF USED(lcRpTmpFil)
  USE IN (lcRpTmpFil)
ENDIF

IF USED(lcTmpCurs)
  USE IN (lcTmpCurs)
ENDIF

*--Create temporary file.
CREATE TABLE (gcWorkDir + lcRpTmpFil) (WIPAcc C(24) , CtPoNo C(6)   , Type C(1)  , cType C(1)    ,;
                                       TranDate D   , TranType C(1) , Desc C(20) , Qty N(13,3)   ,;
                                       Cost N(12,3) , llDontShow L  , llFabric L , cVendCode C(8),;
                                       cVendName C(30))
SELECT (lcRpTmpFil)
*-- Must make Zap to save the file on The Hard Disk.
ZAP
INDEX ON CType + CtPoNo TAG cTkTNo
INDEX ON Type  + CtPoNo TAG TkTNo ADDITIVE
INDEX ON WIPAcc + Type + CtPoNo + cType TAG (lcRpTmpFil) ADDITIVE

*--lcTmpCurs
CREATE TABLE (gcWorkDir + lcTmpCurs) (CtPoNo C(6) , Item C(7) , Clr C(6) , cSession C(06),;
                                      lcIssRet C(1))

SELECT (lcTmpCurs)
*-- Must make Zap to save the file on The Hard Disk.
ZAP
INDEX ON CtPoNo + Item + Clr + cSession + lcIssRet TAG (lcTmpCurs)

IF (!USED(lcTmpNam))
  USE gcDataDir+"GLDIST" AGAIN ALIAS (lcTmpNam) IN 0 SHARED
ENDIF

*-- End Of lfWorkFile.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!                   :              : 'C' To clear all the records of the file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(FileName , 'F')     >> Close one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> Close more than one file.
*!             : =lfBasToClr(FileName , 'C')     >> Zap one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "C"
  IF USED(lcFilName)
    SELECT (lcFilName)
    ZAP
  ENDIF
  RETURN
ENDIF

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
*! Name      : lfClearRep
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Close any opened files if user press OG <Close> Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .T.  

*--Function to delet temprory files.
=lfBasToClr(lcPoshdr , 'F')
=lfBasToClr(lcPosLn  , 'F')

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfPreSty
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Prepare Style filter.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPreSty()
*!*************************************************************
FUNCTION lfPreSty
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT Style
DO CASE

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*    CASE !EMPTY(lcLowSty) .AND. !EMPTY(lcHigSty)
*!*      =SEEK(PADR(lcLowSty,lnMajLen))
*!*      lcWhilCon = "PADR(STYLE.STYLE,lnMajLen) <= PADR(lcHigSty,lnMajLen)"

*!*    CASE EMPTY(lcLowSty) .AND. !EMPTY(lcHigSty)
*!*      LOCATE
*!*      lcWhilCon = "PADR(STYLE.STYLE,lnMajLen) <= PADR(lcHigSty,lnMajLen)"
  CASE !EMPTY(lcLowSty) AND USED(lcLowSty)
      lcWhilCon = "Seek(PADR(STYLE.STYLE,lnMajLen),'&lcLowSty')"
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

  OTHERWISE
    LOCATE
    lcWhilCon = ".T."
ENDCASE
SELECT(lnOldAls)

*--End of lfPreSty.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Validation function for Validating Style Code
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
IF EMPTY(EVAL(lcStyle))
  RETURN
ENDIF

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfChkSty
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Check Style validity.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfChkSty()
*!*************************************************************
FUNCTION lfChkSty
LPARAMETERS lcSty2Chk , llSeason

PRIVATE llContinuo , lnOldAls
llContinuo = .F.

*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
*!*  IF lcSeason <> ".T."
*!*    lcSeason = "Style"+SUBSTR(lcSeason,AT('.',lcSeason))
*!*  ENDIF
*:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]

IF SEEK(lcSty2Chk,"Style")
  lnOldAls = SELECT(0)
  SELECT STYLE
  
  *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[Start]
  *LOCATE REST WHILE Style = lcSty2Chk;
              FOR &lcWhilCon .AND. &lcStyFlt .AND. &lcSeason

  LOCATE REST WHILE Style = lcSty2Chk;
              FOR &lcWhilCon .AND. &lcStyFlt .AND. &lcSeason AND &lcDivision
  *:B608390,1 MMT 12/26/2007 Fix bug of Activity date option is now Displayed in OP Grid[End]
      
  llContinuo = FOUND()
  SELECT(lnOldAls)
ENDIF
RETURN(llContinuo)

*--End of lfChkSty.
*!*************************************************************
*! Name      : lfSumFab1
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Sum a specific field for the current fabric 
*!           : in fabric file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab1()
*!*************************************************************
FUNCTION lfSumFab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
LOCAL lnAlias

lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcFabDye)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcFabDye)
  LOCATE 
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
SELECT(lnAlias)
RETURN INT(lnTotcomp)

*--End of lfSumFab1.
*!*************************************************************
*! Name      : lfsrSty
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style   IN  0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    LOCATE 
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE

*--End of lfsrvSty.
*!*************************************************************
*! Name      : lfCloseRpFls
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 06/04/2006
*! Purpose   : This fn called when the User select Close Report.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCloseRpFls()
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfCloseRpFls

=lfBasToClr(lcTmpNam  , 'F')   &&To Delete the Temp name of the GLDIST file.

*--End of lfCloseRpFls.
