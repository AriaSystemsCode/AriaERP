*:***********************dclcRpRptly ************************************************************************
*: Program file  : MFTRGDT.PRG                              
*: Program desc. : Project management trigger date report.(N00341)
*:         Module: Aria Apparel Series.
*:           Date: 01/11/2009
*:      Developer: AHMED MAHER KESHK (AMK)
*:***********************************************************************************************************
*:Modifications:
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[T20080918.0002]
*! B609555,1 MMT 04/07/2011 Report Vertical format display task/line 
*:***********************************************************************************************************

*N000682,1 SAB 03/21/2013 Globalization changes[Start]
#include R:\ARIA4XP\REPORTS\MFTRGDT.H
*N000682,1 SAB 03/21/2013 Globalization changes[End]

R_TITLE   = 'PROJECT MANAGEMENT SCHEDULING REPORT'
lcForCond = '.T.'            && The line filter.
llPrint = .F.
proj_title=''
lcSortBy1=''
lcTitle=''
lcAccVend = ' '
lcVertTmp=''
IF EMPTY(lcRpTemplt) AND lcRpRptLy  = 'V' 
  =gfModalGen('TRM38233B00000','DIALOG')
  lcRpRptLy  = 'H'
  lcRpOprTyp = 'A'
  CLEARREAD()
  RETURN
ENDIF


*** Array to hold all operations data to be printed for the current project.
DIMENSION laOprToPrn[1,7]
=gfOpentable('PMPRJHD','','')
*mt
*=gfOpentable('PmPrjDt','PmPrjDt','')
=gfOpentable('PmPrjDt','PmPrjDts','')
*mt
=gfOpentable('PmCtgHd','PmCtgHd','')
=gfOpentable('Poshdr','Poshdr','')
DO CASE
  CASE lcRpSrtBy $ 'OC' 
    lcSortBy = 'PmPrjHd.cPrj_Id'
    IF lcRpSrtBy='O'
      lcSortBy1='Order'
    ENDIF
    IF lcRpSrtBy='C'
      lcSortBy1='Cut ticket'
    ENDIF
  CASE lcRpSrtBy = 'R'
    lcSortBy = 'PmPrjHd.dReq_Fnsh,PmPrjHd.cPrj_Id'
    lcSortBy1='Required Date'
  CASE lcRpSrtBy $ 'A'
    lcSortBy = 'ORDHDR.Account,PmPrjHd.cPrj_Id'
    lcSortBy1='Account'
  CASE lcRpSrtBy $ 'V'
    lcSortBy = 'POSHDR.VENDOR,PmPrjHd.cPrj_Id'
    lcSortBy1='Vendor'
ENDCASE


IF lfBuldFltr() .AND. lfGetData()
  R_WIDTH = IIF(lcRpRptLy = 'V','XW','W')
  lcCompName = gcCom_Name &&to get company name
  XREPORT = 'PRM910'

  lcTabName = gfTempName()
  lcFName = gfTempName()

  SELECT (lcTmpFile) 

  SET DEVICE TO FILE (oAriaApplication.WorkDir+lcFName)
    lcLastTable = SELECT(0)

    SELECT (lcTmpFile )
    ALTER table (lcTmpFile) add column f1 c(20)
    ALTER table (lcTmpFile) add column quantity c(20)
    
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
    *ALTER table (lcTmpFile) add column cOprt_Ctg1 c(23)
    ALTER table (lcTmpFile) add column cOprt_Ctg1 c(24)
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
    
    ALTER table (lcTmpFile) add column cOprt_Dsc c(40)
    ALTER table (lcTmpFile) add column cOprt_Id c(40)
    ALTER table (lcTmpFile) add column dEst_Strt1 d(8)
    ALTER table (lcTmpFile) add column dEst_Fnsh1 d(8)
    ALTER table (lcTmpFile) add column dClc_Strt1 d(8)
    ALTER table (lcTmpFile) add column dClc_Fnsh1 d(8)
    ALTER table (lcTmpFile) add column dAct_Strt1 d(8)
    ALTER table (lcTmpFile) add column dAct_Fnsh1 d(8)
    ALTER table (lcTmpFile) add column proj_stat c(10)
    
    SELECT (lcLastTable)
    =lfSyPrjHdr() 
    =lfRepFrm&lcRpRptLy() 
    
  SET DEVICE TO SCREEN
  CREATE TABLE (oAriaApplication.WorkDir+lcTabName) (nLine c(250))

  APPEND FROM (oAriaApplication.WorkDir+lcFName) SDF
  IF llPrint
        
  SELECT (lcTmpFile )
  
*!*	  IF lcRpPrjTp  <> 'C'
*!*	      DO CASE
*!*	        CASE lcRpPrjTp  $ 'OT'
*!*	          UPDATE (lcTmpFile) SET f1=Account
*!*	        CASE lcRpPrjTp  $ 'PADNR'
*!*	          UPDATE (lcTmpFile) SET f1=VENDOR
*!*	      ENDCASE
*!*	  ELSE      
*!*	      UPDATE (lcTmpFile) SET f1=Account
*!*	  ENDIF

*AHS  
      DO CASE
        CASE lcRpPrjTp  $ 'OT'
          UPDATE (lcTmpFile) SET f1=Account
        CASE lcRpPrjTp  $ 'PADNR'
          UPDATE (lcTmpFile) SET f1=VENDOR
      ENDCASE
*AHS
    
*!*	  COPY TO oAriaApplication.WorkDir+lcRepFile+'.DBF'
*  COPY TO oAriaApplication.WorkDir+lcVertTmp+'.DBF' 
    IF lcRpRptLy  = 'V'
       SELECT (lcVertTmp)
       *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
       LOCATE 
       IF EOF()
		  =gfModalGen('TRM00052B00000', 'DIALOG')
	      RETURN
	   ENDIF
       *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
    ENDIF

    COPY TO oAriaApplication.WorkDir+lcRepFile+'.DBF'
  
  *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
  *DIMENSION loOGScroll.laCRParams [8,2]
    DIMENSION loOGScroll.laCRParams [9,2]
  *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
  
  loOGScroll.laCRParams [1,1]= 'XREPORT'
  loOGScroll.laCRParams [1,2]= XREPORT
  loOGScroll.laCRParams [2,1] = 'lcField'
  loOGScroll.laCRParams [2,2] = UPPER(lcField)+'#'
  loOGScroll.laCRParams [3,1] = 'lcfieldName'
  loOGScroll.laCRParams [3,2] = lcAccOrVen
  loOGScroll.laCRParams [4,1] = 'lcRpOprTyp'
  loOGScroll.laCRParams [4,2] = lcRpOprTyp
*!*	  loOGScroll.laCRParams [5,1] = 'laTmpltOpr'
*!*	  loOGScroll.laCRParams [5,2] = laTmpltOpr
  loOGScroll.laCRParams [5,1] = 'lcRpRprt'
  loOGScroll.laCRParams [5,2] = lcRpRprt
  loOGScroll.laCRParams [6,1] = 'lcTitle'
  loOGScroll.laCRParams [6,2] = lcTitle
  loOGScroll.laCRParams [7,1] = 'lcSortBy'
  loOGScroll.laCRParams [7,2] = lcSortBy1
  *AHS
  loOGScroll.laCRParams [8,1] = 'lcAccVend'
  loOGScroll.laCRParams [8,2] = lcAccVend
  *AHS
  
  *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
  loOGScroll.laCRParams [9,1] = 'lcRpSrtBy'
  loOGScroll.laCRParams [9,2] = lcRpSrtBy
  *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
  
  PmPrjDt1 = gfTempName()
  SELECT PmPrjDt
  COPY TO oAriaApplication.WorkDir +  PmPrjDt1  + ".DBF" 
  PmCtgHd1 = gfTempName()
  SELECT PmCtgHd
  COPY TO oAriaApplication.WorkDir +  PmCtgHd1  + ".DBF" 
  pmprjhd1 = gfTempName()
  SELECT pmprjhd
  COPY TO oAriaApplication.WorkDir +  pmprjhd1  + ".DBF" 
  IF lcRpRptLy  = 'V'
    lcRpForm = 'MFTRGDTV'
  ELSE
    lcRpForm = 'MFTRGDTA'
  ENDIF
  loOgScroll.lcOGLastForm=lcRpForm
  DIMENSION loOGScroll.laCRTables[1]

  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcRepFile+ '.DBF'

*!*	 loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcRepFile+ '.DBF'
* loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcVertTmp+ '.DBF'
 

  =gfDispRe()
 
 

  ELSE
    =gfModalGen('TRM00052B00000','DIALOG')  
  ENDIF
ELSE
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF


*:************************************************************************
*: Program file  : lfwOGWhen
*: Program desc. : 
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MOUSTAFA (AHS)
*:************************************************************************  
FUNCTION lfwOGWhen
IF llcheck = .T.
   *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*		DO CASE 
*!*		    CASE oAriaApplication.ActiveModuleID = 'SO'
*!*		         lcRpPrjTp = 'O'
*!*		         lcPrjTitle  = 'Only these Orders'
*!*		    CASE oAriaApplication.ActiveModuleID = 'MF'
*!*		         lcRpPrjTp = 'C'
*!*		         lcPrjTitle  = 'Only these C/T #'
*!*		    CASE oAriaApplication.ActiveModuleID = 'PO'
*!*		         lcRpPrjTp = 'P'
*!*		         lcPrjTitle  = 'Only these P/O #'
*!*		ENDCASE  
	DO CASE 
	    CASE oAriaApplication.ActiveModuleID = 'SO'
	         lcRpPrjTp = 'O'
	         lcPrjTitle  = LANG_ONLYORDER
	    CASE oAriaApplication.ActiveModuleID = 'MF'
	         lcRpPrjTp = 'C'
	         lcPrjTitle  = LANG_ONLYCT
	    CASE oAriaApplication.ActiveModuleID = 'PO'
	         lcRpPrjTp = 'P'
	         lcPrjTitle  = LANG_ONLYPO
	ENDCASE  
    *N000682,1 MMT 02/06/2013 Globalization changes[END]
	=gfOpentable('PMPRJHD','PMPRJHD','')
	SELECT PMPRJHD
	gfsetorder('PMPRJHD')
	gfseek('')
    
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
    =gfOpenTable("PMPTHDT","PMPTHDT")
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
    
	*-tmi
	=lfFillSrBy()
 llcheck = .F.
ENDIF
*-tmi



*:************************************************************************
*: Program file  : lfBuldFltr
*: Program desc. : Function to build the filter string according to the
*:                 grid information.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfBuldFltr

DO CASE 
  CASE lcRpPrjTp = 'C'
    *lcRpExp = lcRpExp + '.AND. PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle = lcRpPrjTp+CUTTKTH.CUTTKT'   
     lcRpExp = lcRpExp + '.AND. PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle = lcRpPrjTp+POSHDR.PO'
  CASE lcRpPrjTp $ 'OT'
    lcRpExp = lcRpExp + '.AND. PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle = lcRpPrjTp+ORDHDR.ORDER'
  CASE lcRpPrjTp $ 'PADNR'
    lcRpExp = lcRpExp + '.AND. PmPrjHd.cprj_typ+PmPrjHd.cprj_id+PmPrjHd.cstyle = lcRpPrjTp+POSHDR.PO'
ENDCASE

lcRpExp  = lcRpExp + IIF(llRpShLate,'.AND. PmPrjHd.dAct_Fnsh > PmPrjHd.dEst_Fnsh','')
lcRpExp = lcRpExp + IIF(lcRpPrjStt <> 'A', '.AND. PmPrjHd.cPrj_Stts = lcRpPrjStt ','.AND. PmPrjHd.cPrj_Stts <> "X" ')

*:************************************************************************
*: Program file  : lfGetData
*: Program desc. : Function to collect data required for processing.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfGetData
LOCAL ah
lcSearch = lcRpExp 
lcSelectTemp = gfTempName()
lcSelectSt=''
=gfOpentable('POsHdr','')
=gfOpentable('posln','')


SET EXACT OFF 
*TMI
SELECT PMPRJHD
gfSetOrder('PMPRJHD')
gfSeek(lcRpPrjTp,'PMPRJHD')
SELECT PMPRJDT
gfSeek(lcRpPrjTp,'PMPRJDT')
SELECT POSHDR
gfsetorder('POSHDR')
gfseek(lcRpPrjTp,'POSHDR')
*TMI
IF lcRpPrjTp  <> 'C'

  DO CASE
    CASE lcRpPrjTp  $ 'OT'
    
        lcSelectSt='SELECT PmPrjHd.*,ORDHDR.Account FROM PmPrjHd,ORDHDR'+ ;
                   ' WHERE PmPrjHd.cprj_typ+PmPrjHd.cprj_id = lcRpPrjTp+ORDHDR.Order ORDER BY ' + lcSortBy                   
        lcSelectSt = STRTRAN(lcSelectSt,'lcRpPrjTp',"'" + lcRpPrjTp + "'")
       * lnResult = oAriaApplication.RemoteCompanyData.sqlrun(lcSelectSt,lcSelectTemp,'',oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loOGScroll.Parent.Parent.DataSessionID)
        &lcSelectSt INTO CURSOR &lcSelectTemp
        
        d = lfGenrateSelect() 
        IF EMPTY(d)
         SELECT * FROM &lcSelectTemp INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
        ELSE
         SELECT * FROM &lcSelectTemp WHERE &d INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
        ENDIF
        
        
        
    CASE lcRpPrjTp  $ 'PADNR'
        lcSelectSt="SELECT PmPrjHd.*,POsHdr.VENDOR FROM PmPrjHd,POsHdr"+;
                   " WHERE PmPrjHd.cprj_typ+PmPrjHd.cprj_id = lcRpPrjTp+POSHDR.PO ORDER BY " + lcSortBy 
                   
        lcSelectSt = STRTRAN(lcSelectSt,'lcRpPrjTp',"'" + lcRpPrjTp + "'")
  ***
  *      lnResult = oAriaApplication.RemoteCompanyData.sqlrun(lcSelectSt,lcSelectTemp,'',oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loOGScroll.Parent.Parent.DataSessionID)
         &lcSelectSt INTO CURSOR &lcSelectTemp
         
        d = lfGenrateSelect() 
        IF EMPTY(d)
         SELECT * FROM &lcSelectTemp INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
        ELSE
         SELECT * FROM &lcSelectTemp WHERE &d INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
        ENDIF
  ENDCASE
ELSE  
  IF !EMPTY(lcMStyle1)
 *   lcSelectSt="SELECT PmPrjHd.*,'"+lfGetAccnt('1',PmPrjHd.cPrj_Id)+"' AS Account FROM PmPrjHd,CutTktH"+ ;
                   " WHERE PmPrjHd.cprj_typ+PmPrjHd.cprj_id = lcRpPrjTp+CutTktH.Cuttkt ORDER BY " + lcSortBy
     lcSelectSt="SELECT PmPrjHd.*,'"+lfGetAccnt('1',PmPrjHd.cPrj_Id)+"' AS Account FROM PmPrjHd,POSHDR"+ ;
                   " WHERE PmPrjHd.cprj_typ+PmPrjHd.cprj_id = lcRpPrjTp+POSHDR.PO ORDER BY " + lcSortBy
                                 
     lcSelectSt = STRTRAN(lcSelectSt,'lcRpPrjTp',"'" + lcRpPrjTp + "'")
 ***
 *  lnResult = oAriaApplication.RemoteCompanyData.sqlrun(lcSelectSt,lcSelectTemp,'',;
                   oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loOGScroll.Parent.Parent.DataSessionID)
    *TAREK-CHK TMI 05/28/2009 11:03:12 PM [Start] 
    &lcSelectSt INTO CURSOR &lcSelectTemp
    *TAREK-CHK TMI 05/28/2009 11:03:13 PM [End  ]   
                   
    d = lfGenrateSelect() 
    IF EMPTY(d)
      SELECT * FROM &lcSelectTemp INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
    ELSE
      SELECT * FROM &lcSelectTemp WHERE &d INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
    ENDIF               
  ELSE
  *  lcSelectSt="SELECT PmPrjDt.* FROM PmPrjDt,CutTktH"+ ;
                   " WHERE PmPrjDt.cprj_typ+PmPrjDt.cprj_id+PmPrjDt.Cstyle = lcRpPrjTp+CutTktH.Cuttkt+CutTktH.Style"
     lcSelectSt="SELECT PmPrjDt.* FROM PmPrjDt,POSHDR"+ ;
                   " WHERE PmPrjDt.cprj_typ+PmPrjDt.cprj_id+PmPrjDt.Cstyle = lcRpPrjTp+POSHDR.PO+POSHDR.Style"
      lcSelectSt = STRTRAN(lcSelectSt,'lcRpPrjTp',"'" + lcRpPrjTp + "'")
  ***
  *  lnResult = oAriaApplication.RemoteCompanyData.sqlrun(lcSelectSt,'PmPrjDt','PmPrjDt',;
                   oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loOGScroll.Parent.Parent.DataSessionID)
    
                
 *   =gfSetOrder('PmPrjDt')
 *   lcSelectSt="SELECT PmPrjHd.*,'"+lfGetAccnt('1',PmPrjHd.cPrj_Id)+"' AS Account FROM PmPrjHd,CutTktH"+ ;
                   " WHERE PmPrjHd.cprj_typ+PmPrjHd.cprj_id = lcRpPrjTp+CutTktH.Cuttkt ORDER BY " + lcSortBy
     lcSelectSt="SELECT PmPrjHd.*,'"+lfGetAccnt('1',PmPrjHd.cPrj_Id)+"' AS Account FROM PmPrjHd,POSHDR"+ ;
                   " WHERE PmPrjHd.cprj_typ+PmPrjHd.cprj_id = lcRpPrjTp+POSHDR.PO ORDER BY " + lcSortBy               
    lcSelectSt = STRTRAN(lcSelectSt,'lcRpPrjTp',"'" + lcRpPrjTp + "'")
 
 ***
 *   lnResult = oAriaApplication.RemoteCompanyData.sqlrun(lcSelectSt,'PmPrjHd','',;
                   oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loOGScroll.Parent.Parent.DataSessionID)
    
                   
    d = lfGenrateSelect() 
    IF EMPTY(d)
      SELECT * FROM PmPrjHd INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
    ELSE
      SELECT * FROM PmPrjHd WHERE &d INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
    ENDIF    
  ENDIF
ENDIF

RETURN (_TALLY <> 0)

*:************************************************************************
*: Program file  : lfGetAccnt
*: Program desc. : Function to return the account for the cuttkt.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************

FUNCTION lfGetAccnt
PARAMETERS lcPType,lcPCutTkt
=gfOpentable(oAriaApplication.DataDir+'CutPick','')

lcRetVal = 'STOCK'
SELECT CutPick
LOCATE FOR TranCd + CutPick.cTktNo = lcPType+lcPCutTkt
IF FOUND()
  lcOrdNo = Order
  CONTINUE
  IF FOUND()
    lcRetVal = 'MULTI'
  ELSE
    IF SEEK('O'+lcOrdNo,'OrdHdr')
      lcRetVal = OrdHdr.Account
    ENDIF
  ENDIF
ENDIF
RETURN lcRetVal

*:************************************************************************
*: Program file  : lfvLines
*: Program desc. : Function to validate if the current record notify memo
*:                 has the current notify field entered by the user or to
*:                 validate the line (operation) status for the passed project.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfvLines
PARAMETERS lcSeekCode, lcRpNotfy, lcRpOprStt

lcPrvAlis = SELECT(0)
llReturn = .F.

IF gfSeek(lcSeekCode,'PmPrjDt')
  IF !EMPTY(lcRpNotfy)
    DO CASE
      CASE lcRpOprStt = 'A'
        lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify'
      CASE lcRpOprStt = 'V'
        lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. lVoid'
      CASE lcRpOprStt = 'O'
        lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. !lVoid'
    ENDCASE
  ELSE
    lcForCond = IIF(lcRpOprStt = 'V','lVoid','!lVoid')
  ENDIF

  SELECT PmPrjDt
  LOCATE REST WHILE cPrj_Typ + cPrj_Id + cStyle = lcSeekCode;
            FOR &lcForCond
  llReturn = FOUND()

  SELECT (lcPrvAlis)
ENDIF
RETURN (llReturn)

*:************************************************************************
*: Program file  : lfvPrjID
*: Program desc. : Validation function for the project passed.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfvPrjID

PRIVATE lcPrjTitle, lcBrFields, lnCurTag, lnCurAlias, lnSoftSeek, lnOption

lcPrjID = _screen.ActiveForm.ActiveControl.value
lcPrj  = STRTRAN(lcPrjID ," ","")

IF EMPTY(lcPrj)
  lcPrjID  = ""
ENDIF

IF !EMPTY(_screen.ActiveForm.ActiveControl.oldvalue) AND lcPrjID  = _screen.ActiveForm.ActiveControl.oldvalue
  RETURN
ENDIF
lcFldLocNam = _screen.ActiveForm.ActiveControl.name

lnCurAlias   = SELECT()
SELECT PMPRJHD
lnCurTag     = VAL(SYS(21))
SET ORDER TO TAG PMPRJHD
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	lcBrFields   = "cPrj_ID  : H='Project ID',"+;
*!*	               "cStyle   : H='Style',"+;
*!*	               "cPrj_SDsc: H='Project Description'"   
*!*	DO CASE
*!*	  CASE lcRpPrjTp  = 'C'
*!*	    lcPrjTitle = 'Cutting Tickets Projects'
*!*	  CASE lcRpPrjTp  = 'P'      && PO
*!*	    lcPrjTitle = 'Purchase Orders Projects'
*!*	  CASE lcRpPrjTp  = 'A'      && Adorment Order
*!*	    lcPrjTitle = 'Adorment Order Projects'   
*!*	  CASE lcRpPrjTp  = 'D'      && Dye Order 
*!*	    lcPrjTitle = 'Dye Order Projects'     
*!*	  CASE lcRpPrjTp  = 'N'      && Inter-Location PO
*!*	    lcPrjTitle = 'Inter-Location PO Projects'      
*!*	  CASE lcRpPrjTp  = 'R'      && Return PO
*!*	    lcPrjTitle = 'Return PO Projects'    
*!*	  CASE lcRpPrjTp  = 'O'      && SO
*!*	    lcPrjTitle = 'Sales Orders Projects'    
*!*	  CASE lcRpPrjTp  = 'T'      && EDI Order
*!*	    lcPrjTitle = 'EDI Order Projects'  
*!*	ENDCASE

lcBrFields   = "cPrj_ID  : H='"+LANG_PROJID+"',"+;
               "cStyle   : H='"+LANG_STYLE+"',"+;
               "cPrj_SDsc: H='"+LANG_PROJDESC+"'"   

DO CASE
  CASE lcRpPrjTp  = 'C'
    lcPrjTitle = LANG_CUTTKTPROJ
  CASE lcRpPrjTp  = 'P'      && PO
    lcPrjTitle = LANG_POPROJECTS
  CASE lcRpPrjTp  = 'A'      && Adorment Order
    lcPrjTitle = LANG_ADORPROJECTS
  CASE lcRpPrjTp  = 'D'      && Dye Order 
    lcPrjTitle = LANG_DYEPROJ
  CASE lcRpPrjTp  = 'N'      && Inter-Location PO
    lcPrjTitle = LANG_INTELOCPROJ
  CASE lcRpPrjTp  = 'R'      && Return PO
    lcPrjTitle = LANG_RETPOPROJECTS
  CASE lcRpPrjTp  = 'O'      && SO
    lcPrjTitle = LANG_SOPROJECTS
  CASE lcRpPrjTp  = 'T'      && EDI Order
    lcPrjTitle = LANG_EDIODERPROJ
ENDCASE
*N000682,1 MMT 02/06/2013 Globalization changes[End]                 
llBrowse = IIF(TYPE('llBrowse') = 'U', .F., llBrowse)
DIMENSION latemp[1]
IF llBrowse .OR. '?' $ lcPrjID
  GO TOP
  lcPrjID  = IIF(ARIABROW([lcRpPrjTp],lcPrjTitle, ;
                  gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPRJHD.cPrj_ID','laTemp'),;
                  PMPRJHD.cPrj_ID, SPACE(6))         
                  
ELSE
  IF !EMPTY(lcPrjID) 
    IF !SEEK(lcRpPrjTp + lcPrjID)  
      lnSoftSeek = RECNO(0)
      IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
        GO lnSoftSeek
       ELSE
         GO TOP
       ENDIF
       lcPrjID  = IIF(ARIABROW([lcRpPrjTp],lcPrjTitle, ;
                       gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPRJHD.cPrj_ID','laTemp'),;
                       PMPRJHD.cPrj_ID, SPACE(6))         
    ENDIF  
  ENDIF
ENDIF     

_screen.ActiveForm.ActiveControl.value = lcPrjID

SET ORDER TO (lnCurTag)
SELECT (lnCurAlias)


*:************************************************************************
*: Program file  : lfvUserId
*: Program desc. : Browse function for avilable Users.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfvUserId

PRIVATE lcTitle, lcBrFields, lnCurAlias

lnCurAlias   = SELECT(0)
lcUser_Id = _screen.ActiveForm.ActiveControl.value 
lcUser = STRTRAN(lcUser_Id," ","")
IF EMPTY(lcUser)
  lcUser_Id = ""
ENDIF
IF !EMPTY(lcOldValue) AND lcUser_Id = lcOldValue
  RETURN
ENDIF
lcFldLocNam = _screen.ActiveForm.ActiveControl.name
SELECT SYUUSER
GO TOP
IF EOF()
  SELECT (lnCurAlias)
  lcUser_Id = PADR(ALLTRIM(lcUser_Id),10)
ELSE
  *N000682,1 MMT 02/06/2013 Globalization changes[Start]
  *lcBrFields = "cUser_Id : H='User ID',cUsr_Name : H='User Name'"  
  lcBrFields = "cUser_Id : H='"+LANG_USERID+"',cUsr_Name : H='"+LANG_USERNAME+"'"
  *N000682,1 MMT 02/06/2013 Globalization changes[End]
  lcSetExact = SET('EXACT')
  SET EXACT ON
  *N000682,1 MMT 02/06/2013 Globalization changes[Start]
  *lcTitle    = 'Users'  
  lcTitle    = LANG_USERS
  *N000682,1 MMT 02/06/2013 Globalization changes[Start]
  DIMENSION latemp[1]
  IF !EMPTY(lcUser_Id) AND !SEEK(ALLTRIM(lcUser_Id))
    lcUser_Id = IIF(ARIABROW('',lcTitle,;
                     gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','SYUUSER.cUser_Id','latemp'),;
                     SYUUSER.cUser_Id, SPACE(10))
    &lcFldLocNam = lcUser_Id                   
  ENDIF
  *SET EXACT &lcSetExact
  lcRpNotfy = lcUser_Id 
  _screen.ActiveForm.ActiveControl.value=lcRpNotfy 
ENDIF
SELECT (lnCurAlias)

*:************************************************************************
*: Program file  : lfvPrjTmp
*: Program desc. : Browse function for avilable templates.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfvPrjTmp

PRIVATE lcTitle, lcBrFields, lnCurAlias

lnCurAlias   = SELECT(0)

IF !USED('PMPTHHD')
*AHS
 *=gfOpenFile(gcDataDir+'PMPTHHD',gcDataDir+'PMPTHHD','SH')
  =gfOpenTable('PMPTHHD','PMPTHHD','SH')
*AHS  
ENDIF

SELECT PMPTHHD
*AHS
*SET ORDER TO PMPTHHD
gfsetorder('PMPTHHD')
*AHS

GO TOP
DIMENSION laTemp[1]
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
*lcBrFields = "cPath_ID : H='Template ID',cPath_Dsc  : H='Description'"
lcBrFields = "cPath_ID : H='"+LANG_TEMPID+"',cPath_Dsc  : H='"+LANG_DESC+"'"
*N000682,1 MMT 02/06/2013 Globalization changes[End]
*AHS
*!*	lcTitle    = 'Templates'
*!*	IF !EMPTY(lcRpTemplt) AND !SEEK(ALLTRIM(lcRpTemplt))
*!*	  lcRpTemplt = IIF(ARIABROW('',lcTitle,;
*!*	                   gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','PMPTHHD.cPath_ID','latemp'),;
*!*	                   PMPTHHD.cPath_ID, SPACE(3))
*!*	ENDIF

*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
IF !EMPTY(lcRpTemplt) AND !gfSEEK(ALLTRIM(lcRpTemplt))
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]

    SELECT PMPTHHD
    gfSetorder('PMPTHHD')
    gfseek('')        
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
    *llSel = gfBrows('','Cpath_id,cpath_dsc','laTemp','Templates')           
    llSel = gfBrows('','Cpath_id,cpath_dsc','laTemp',LANG_TEMPLATES)       
    *N000682,1 MMT 02/06/2013 Globalization changes[ENd]
      IF llSel
        lcRpTemplt= laTemp[1]         
      ENDIF
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
ENDIF 
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]

*AHS

IF EMPTY(lcRpTemplt)
  IF lcRpRptLy  = 'V'
    =gfModalGen('TRM38233B00000','DIALOG')
    lcRpRptLy  = 'H'
    lcRpOprTyp   = 'A'
    CLEARREAD()
  ENDIF
ELSE

  *** Array to hold all template operations.
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
*!*	  DIMENSION laTmpltOpr[1]
*!*	  lcTmp_Memo = PmRprTm.mTmp_Oprt
*!*	  =gfStr2Ar(lcTmp_Memo,@laTmpltOpr,'|')
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]

ENDIF
SELECT (lnCurAlias)

*:************************************************************************
*: Program file  : lfRepFrmV
*: Program desc. : Print report format Vertical layout.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MOUSTAFA (AHS)
*:************************************************************************
FUNCTION lfRepFrmV

IF !EMPTY(lcRpNotfy)
  DO CASE
    CASE lcRpOprStt = 'A'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify'
    CASE lcRpOprStt = 'V'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. lVoid'
    CASE lcRpOprStt = 'O'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. !lVoid'
  ENDCASE
ELSE
  lcForCond = IIF(lcRpOprStt = 'V','lVoid','!lVoid')
ENDIF

SELECT (lcTmpFile)
lcTmpDetV = gfTempName()
COPY STRUCTURE TO (oAriaApplication.WorkDir+lcTmpDetV)
USE (oAriaApplication.WorkDir+lcTmpDetV) IN 0
SELECT (lcTmpFile)
LOCATE 

*TAREK-CHK TMI 05/28/2009 10:45:28 PM [Start] 
*SCAN FOR lfvLines(lcRpPrjTp+cPrj_Id+cStyle,lcRpNotfy,lcRpOprStt)
SCAN             
  IF lfvLines(lcRpPrjTp+cPrj_Id+cStyle,lcRpNotfy,lcRpOprStt) 
  *TAREK-CHK TMI 05/28/2009 10:47:37 PM [End  ] 
  llPrint = .T.
  
  *UPDATE (lcTmpFile) SET quantity=STR(lfGetQty())
   SELECT (lcTmpFile)
   REPLACE quantity WITH STR(lfGetQty())
  *UPDATE (lcTmpFile) SET &lcTmpFile..proj_stat = IIF(&lcTmpFile..dAct_Fnsh > &lcTmpFile..dReq_Fnsh OR &lcTmpFile..dClc_Fnsh > &lcTmpFile..dReq_Fnsh, 'LATE', 'ON TIME') 
   REPLACE proj_stat WITH IIF(&lcTmpFile..dAct_Fnsh > &lcTmpFile..dReq_Fnsh OR &lcTmpFile..dClc_Fnsh > &lcTmpFile..dReq_Fnsh, 'LATE', 'ON TIME')
   SCATTER MEMVAR MEMO
  
  
  lcRpPrj_ID = cPrj_Id
                                
*!*	  UPDATE (lcTmpFile) SET &lcTmpFile..cOprt_Dsc=PmPrjDt.cOprt_Id,&lcTmpFile..dEst_Strt1=PmPrjDt.dEst_Strt,;
*!*	  						 &lcTmpFile..dEst_Fnsh1=PmPrjDt.dEst_Fnsh,&lcTmpFile..dClc_Strt1=PmPrjDt.dClc_Strt,;
*!*	  						 &lcTmpFile..dClc_Fnsh1=PmPrjDt.dClc_Fnsh,&lcTmpFile..dAct_Strt1=,;
*!*	  	                     &lcTmpFile..dAct_Fnsh1=PmPrjDt.dAct_Fnsh;
*!*	  from PmPrjDt;
*!*	  WHERE PmPrjDt.CPRJ_TYP+PmPrjDt.CPRJ_ID+PmPrjDt.CSTYLE =;
*!*	                              &lcTmpFile..CPRJ_TYP+&lcTmpFile..CPRJ_ID+&lcTmpFile..CSTYLE
    DO CASE   
      CASE  oAriaApplication.ActiveModuleID = 'MF'
         lcField ='CT'
      CASE  oAriaApplication.ActiveModuleID = 'SO'
         lcField ='Order'
      CASE  oAriaApplication.ActiveModuleID = 'PO'
         lcField ='PO'    
    ENDCASE   
      
      lcField1=lcField
***      
    IF gfSEEK(CPRJ_TYP+CPRJ_ID+CSTYLE,'PmPrjDt')
      SELECT PMPRJDT          
      
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
*!*	      SCAN
      SCAN FOR gfSeek(PADR(lcRpTemplt,4)+PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id,'PMPTHDT','PMPTHDT') AND ;
      		IIF(lcRpOprStt <> 'A',IIF(lcRpOprStt = 'V',lVoid,!lVoid),.T.)
*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]

        IF lcRpRprt = 'C' AND !PMPRJDT.lshw2cust
          LOOP
        ENDIF  
***  
		
   		INSERT INTO (lcTmpDetV) FROM MEMVAR
 		 SELECT (lcTmpDetV) 
        
        *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
        *replace  cOprt_Dsc  WITH PmPrjDt.cOprt_Id+' '+SUBSTR(PmPrjDt.cOprt_Dsc,1,13)
        *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
        
	    REPLACE cOprt_Dsc  WITH PmPrjDt.cOprt_Id+' '+PmPrjDt.cOprt_Dsc;
	           dEst_Strt1 WITH PmPrjDt.dEst_Strt;
	           dEst_Fnsh1 WITH PmPrjDt.dEst_Fnsh;
	           dClc_Strt1 WITH PmPrjDt.dClc_Strt;
	           dClc_Fnsh1 WITH PmPrjDt.dClc_Fnsh;
	           dAct_Strt1 WITH PmPrjDt.dAct_Strt;
	           dAct_Fnsh1 WITH PmPrjDt.dAct_Fnsh
	           lcField=lcField1
       ENDSCAN 
      ENDIF     
                                                         
  *TAREK-CHK TMI 05/28/2009 10:47:42 PM [Start] 
  ELSE
    DELETE
  ENDIF
  *TAREK-CHK TMI 05/28/2009 10:47:43 PM [End  ] 
ENDSCAN

SELECT (lcTmpFile)
DELETE ALL
APPEND FROM (oAriaApplication.WorkDir+lcTmpDetV)
USE IN &lcTmpDetV
ERASE (oAriaApplication.WorkDir+lcTmpDetV+'.*')





lcVertTmp = gfTempName()
*! B609555,1 MMT 04/07/2011 Report Vertical format display task/line[Start] 
*!*	CREATE TABLE (oAriaApplication.WorkDir+lcVertTmp) (prj_id C(6),vendor c(10),style C(12),quantity c(20),req_strt d(8),req_fnsh d(8),status c(10),task_id1 c(13);
*!*	                                                  ,est_strt1 d(8),est_fnsh1 d(8),clc_strt1 d(8),clc_fnsh1 d(8),act_strt1 d(8),act_fnsh1 d(8),task_id2 c(13);
*!*	                                                  ,est_strt2 d(8),est_fnsh2 d(8),clc_strt2 d(8),clc_fnsh2 d(8),act_strt2 d(8),act_fnsh2 d(8),task_id3 c(13);
*!*	                                                  ,est_strt3 d(8),est_fnsh3 d(8),clc_strt3 d(8),clc_fnsh3 d(8),act_strt3 d(8),act_fnsh3 d(8),task_id4 c(13);
*!*	                                                  ,est_strt4 d(8),est_fnsh4 d(8),clc_strt4 d(8),clc_fnsh4 d(8),act_strt4 d(8),act_fnsh4 d(8),task_id5 c(13);
*!*	                                                  ,est_strt5 d(8),est_fnsh5 d(8),clc_strt5 d(8),clc_fnsh5 d(8),act_strt5 d(8),act_fnsh5 d(8),nrec_no n(5))
CREATE TABLE (oAriaApplication.WorkDir+lcVertTmp) (prj_id C(6),vendor c(10),style C(19),quantity c(20),req_strt d(8),req_fnsh d(8),status c(10),task_id1 c(40);
                                                  ,est_strt1 d(8),est_fnsh1 d(8),clc_strt1 d(8),clc_fnsh1 d(8),act_strt1 d(8),act_fnsh1 d(8),task_id2 c(40);
                                                  ,est_strt2 d(8),est_fnsh2 d(8),clc_strt2 d(8),clc_fnsh2 d(8),act_strt2 d(8),act_fnsh2 d(8),task_id3 c(40);
                                                  ,est_strt3 d(8),est_fnsh3 d(8),clc_strt3 d(8),clc_fnsh3 d(8),act_strt3 d(8),act_fnsh3 d(8),task_id4 c(40);
                                                  ,est_strt4 d(8),est_fnsh4 d(8),clc_strt4 d(8),clc_fnsh4 d(8),act_strt4 d(8),act_fnsh4 d(8),task_id5 c(40);
                                                  ,est_strt5 d(8),est_fnsh5 d(8),clc_strt5 d(8),clc_fnsh5 d(8),act_strt5 d(8),act_fnsh5 d(8),nrec_no n(5))
*! B609555,1 MMT 04/07/2011 Report Vertical format display task/line[End] 
INDEX ON prj_id + style  TAG VertTmp                       


                    
 lnCount = 0
 SELECT (lcTmpFile)                     
 lnRec =0
 SCAN 
   *IF !SEEK(cprj_id + cstyle,lcVertTmp)  
   *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
   *IF !SEEK(cprj_id ,lcVertTmp)  
   IF !SEEK(cprj_id + cstyle,lcVertTmp)  
   *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
    IF &lcVertTmp..nrec_no  = 0
       lnCount = 1
    ENDIF
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
*    lcVendor = &lcVertTmp..Vendor     
	LCVENDOR = ""
    IF (lcRpPrjTp $ 'PADNR')
      lcVendor = &lcTmpFile..Vendor 
    ENDIF   
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
    lcQuan = &lcTmpFile..Quantity
    
    SELECT (lcVertTmp)
    APPEND BLANK
    REPLACE &lcVertTmp..prj_id WITH &lcTmpFile..cPrj_Id
    REPLACE &lcVertTmp..vendor WITH lcVendor
    REPLACE &lcVertTmp..style WITH &lcTmpFile..cStyle
    REPLACE &lcVertTmp..quantity WITH &lcTmpFile..Quantity
    *REPLACE &lcVertTmp..quantity WITH lcQuan
    REPLACE &lcVertTmp..status WITH &lcTmpFile..proj_stat
    REPLACE &lcVertTmp..task_id1 WITH &lcTmpFile..cOprt_Dsc
    REPLACE &lcVertTmp..est_strt1 WITH &lcTmpFile..dest_strt1
    REPLACE &lcVertTmp..est_fnsh1 WITH &lcTmpFile..dest_fnsh1
    REPLACE &lcVertTmp..clc_strt1 WITH &lcTmpFile..dclc_strt1
    REPLACE &lcVertTmp..clc_fnsh1 WITH &lcTmpFile..dclc_fnsh1
    REPLACE &lcVertTmp..act_strt1 WITH &lcTmpFile..dact_strt1
    REPLACE &lcVertTmp..act_fnsh1 WITH &lcTmpFile..dact_fnsh1
    REPLACE &lcVertTmp..req_strt WITH &lcTmpFile..dreq_strt
    REPLACE &lcVertTmp..req_fnsh WITH &lcTmpFile..dreq_fnsh       
    REPLACE &lcVertTmp..nrec_no  WITH 1     
    lnRec =1
    lnCount=1
   ELSE
     IF &lcVertTmp..nrec_no <> 0 AND lnCount > 5 
        lnRec = lnRec  +1
	    SELECT (lcVertTmp)
	    APPEND BLANK
	    REPLACE &lcVertTmp..prj_id WITH &lcTmpFile..cPrj_Id
	    REPLACE &lcVertTmp..vendor WITH &lcTmpFile..f1
	    REPLACE &lcVertTmp..style WITH &lcTmpFile..cStyle
	    REPLACE &lcVertTmp..quantity WITH &lcTmpFile..Quantity
        REPLACE &lcVertTmp..status WITH &lcTmpFile..proj_stat
        REPLACE &lcVertTmp..task_id1 WITH &lcTmpFile..cOprt_Dsc
        REPLACE &lcVertTmp..est_strt1 WITH &lcTmpFile..dest_strt1
        REPLACE &lcVertTmp..est_fnsh1 WITH &lcTmpFile..dest_fnsh1
        REPLACE &lcVertTmp..clc_strt1 WITH &lcTmpFile..dclc_strt1
        REPLACE &lcVertTmp..clc_fnsh1 WITH &lcTmpFile..dclc_fnsh1
        REPLACE &lcVertTmp..act_strt1 WITH &lcTmpFile..dact_strt1
        REPLACE &lcVertTmp..act_fnsh1 WITH &lcTmpFile..dact_fnsh1       
        REPLACE &lcVertTmp..nrec_no  WITH lnRec 
	    lnCount=1
     
     ENDIF
     IF lnCount  = 5
       lnCount=1  
	    SELECT (lcVertTmp)
	    APPEND BLANK
	    REPLACE &lcVertTmp..prj_id WITH &lcTmpFile..cPrj_Id
	    REPLACE &lcVertTmp..vendor WITH &lcTmpFile..f1
	    REPLACE &lcVertTmp..style WITH &lcTmpFile..cStyle
	    REPLACE &lcVertTmp..quantity WITH &lcTmpFile..Quantity
	    REPLACE &lcVertTmp..status WITH &lcTmpFile..proj_stat
	    REPLACE &lcVertTmp..task_id1 WITH &lcTmpFile..cOprt_Dsc
	    REPLACE &lcVertTmp..est_strt1 WITH &lcTmpFile..dest_strt1
	    REPLACE &lcVertTmp..est_fnsh1 WITH &lcTmpFile..dest_fnsh1
	    REPLACE &lcVertTmp..clc_strt1 WITH &lcTmpFile..dclc_strt1
	    REPLACE &lcVertTmp..clc_fnsh1 WITH &lcTmpFile..dclc_fnsh1
	    REPLACE &lcVertTmp..act_strt1 WITH &lcTmpFile..dact_strt1
	    REPLACE &lcVertTmp..act_fnsh1 WITH &lcTmpFile..dact_fnsh1       
        lnRec = lnRec +1
	    REPLACE &lcVertTmp..nrec_no  WITH lnRec      

     ELSE
         lnCount=lnCount+1  
	     lcCount = STR(lnCount,1)
	     SELECT (lcVertTmp) 
	     LOCATE FOR prj_id = &lcTmpFile..cprj_id and &lcVertTmp..nrec_no =lnRec 
	     
	     REPLACE &lcVertTmp..task_id&lcCount WITH &lcTmpFile..cOprt_Dsc
	     REPLACE &lcVertTmp..est_strt&lcCount WITH &lcTmpFile..dest_strt1
	     REPLACE &lcVertTmp..est_fnsh&lcCount WITH &lcTmpFile..dest_fnsh1
	     REPLACE &lcVertTmp..clc_strt&lcCount WITH &lcTmpFile..dclc_strt1
	     REPLACE &lcVertTmp..clc_fnsh&lcCount WITH &lcTmpFile..dclc_fnsh1
	     REPLACE &lcVertTmp..act_strt&lcCount WITH &lcTmpFile..dact_strt1
	     REPLACE &lcVertTmp..act_fnsh&lcCount WITH &lcTmpFile..dact_fnsh1 

     ENDIF
     
  ENDIF
  *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
  IF lcRpPrjTp $ 'OT'
    REPLACE &lcVertTmp..Vendor WITH  &lcTmpFile..Account
  ENDIF 
  *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]

 ENDSCAN   




*:************************************************************************
*: Program file  : lfRepFrmH
*: Program desc. : Print report format Horizontal layout.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MOUSTAFA (AHS)
*:************************************************************************                                          
FUNCTION lfRepFrmH

IF !EMPTY(lcRpNotfy)
  DO CASE
    CASE lcRpOprStt = 'A'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify'
    CASE lcRpOprStt = 'V'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. lVoid'
    CASE lcRpOprStt = 'O'
      lcForCond = 'ALLTRIM(lcRpNotfy) $ mNotify .AND. !lVoid'
  ENDCASE
ELSE
  lcForCond = IIF(lcRpOprStt = 'V','lVoid','!lVoid')
ENDIF


SELECT (lcTmpFile)
lcTmpDet = gfTempName()
COPY STRUCTURE TO (oAriaApplication.WorkDir+lcTmpDet)
USE (oAriaApplication.WorkDir+lcTmpDet) IN 0


SELECT (lcTmpFile)
LOCATE 
SCAN  

  IF lfvLines(lcRpPrjTp+cPrj_Id+cStyle,lcRpNotfy,lcRpOprStt)
    llPrint = .T.
    
    SELECT (lcTmpFile)
    REPLACE quantity WITH STR(lfGetQty())
    
    lcOldCatg = SPACE(3)
    SCATTER MEMVAR MEMO
    
      
    DO CASE   
      CASE  oAriaApplication.ActiveModuleID = 'MF'
         lcField ='CT'
      CASE  oAriaApplication.ActiveModuleID = 'SO'
         lcField ='Order'
      CASE  oAriaApplication.ActiveModuleID = 'PO'
         lcField ='PO'    
    ENDCASE   
      
      lcField1=lcField
      
    IF gfSEEK(CPRJ_TYP+CPRJ_ID+CSTYLE,'PmPrjDt')
      SELECT PMPRJDT          
      *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
      *SCAN
      SCAN FOR IIF(lcRpOprTyp <> 'A',gfSeek(PADR(lcRpTemplt,4)+PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id,'PMPTHDT','PMPTHDT') ,.T.) AND ;
            		IIF(lcRpOprStt <> 'A',IIF(lcRpOprStt = 'V',lVoid,!lVoid),.T.)
      *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
        IF lcRpRprt = 'C' AND !PMPRJDT.lshw2cust
          LOOP
        ENDIF  
        IF lcOldCatg <> PmPrjDt.cOprt_Ctg
          lcOldCatg = PmPrjDt.cOprt_Ctg
          m.cOprt_Ctg1 = PmPrjDt.cOprt_Ctg + ' '+IIF(gfSEEK(PmPrjDt.cOprt_Ctg,'PmCtgHd','PmCtgHd'),PmCtgHd.cCtg_Dsc,'')        
        ENDIF
        INSERT INTO (lcTmpDet) FROM MEMVAR
        SELECT (lcTmpDet) 
        *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
        *replace cOprt_Dsc  WITH PmPrjDt.cOprt_Id+' '+SUBSTR(PmPrjDt.cOprt_Dsc,1,13)
        *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
        replace cOprt_Dsc  WITH PmPrjDt.cOprt_Id+' '+PmPrjDt.cOprt_Dsc,;
                dEst_Strt1 WITH PmPrjDt.dEst_Strt;
                dEst_Fnsh1 WITH PmPrjDt.dEst_Fnsh;
                dClc_Strt1 WITH PmPrjDt.dClc_Strt;
                dClc_Fnsh1 WITH PmPrjDt.dClc_Fnsh;
                dAct_Fnsh1 WITH PmPrjDt.dAct_Fnsh;
                dAct_Strt1 WITH PmPrjDt.dAct_Strt;
                proj_stat  WITH IIF(PmPrjDt.dAct_Fnsh > &lcTmpFile..dReq_Fnsh OR PmPrjDt.dClc_Fnsh > &lcTmpFile..dReq_Fnsh, 'LATE', 'ON TIME')
                lcField=lcField1
      ENDSCAN
    ENDIF           
  ELSE
   DELETE
  ENDIF  
ENDSCAN 

SELECT (lcTmpFile)
DELETE ALL
APPEND FROM (oAriaApplication.WorkDir+lcTmpDet)
USE IN &lcTmpDet
ERASE (oAriaApplication.WorkDir+lcTmpDet+'.*')






*!*	*TAREK-CHK TMI 05/28/2009 10:47:58 PM [Start] 
*!*	*SCAN FOR lfvLines(lcRpPrjTp+cPrj_Id+cStyle,lcRpNotfy,lcRpOprStt)
*!*	SCAN
*!*	  IF lfvLines(lcRpPrjTp+cPrj_Id+cStyle,lcRpNotfy,lcRpOprStt)
*!*	*TAREK-CHK TMI 05/28/2009 10:48:13 PM [End]
*!*	  llPrint = .T.

*!*	  UPDATE (lcTmpFile) SET quantity=STR(lfGetQty())

*!*	  lcOldCatg = SPACE(3)
*!*	  
*!*	  IF SEEK(CPRJ_TYP+CPRJ_ID+CSTYLE,'PmPrjDt')
*!*	    SELECT PmPrjDt
*!*	    

*!*	    SCAN REST FOR CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID =;
*!*	                  &lcTmpFile..CPRJ_TYP+&lcTmpFile..CPRJ_ID+&lcTmpFile..CSTYLE ;
*!*	                  .AND. IIF(lcRpOprTyp <> 'A',ASCAN(laTmpltOpr,PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id) <> 0,.t.) .AND. &lcForCond 

*!*	      
*!*	      IF lcRpRprt = 'C' AND !lshw2cust                   
*!*	        LOOP
*!*	      ENDIF                  

*!*	      UPDATE (lcTmpFile) SET quantity=STR(lfGetQty())
*!*	      
*!*	      IF lcOldCatg <> PmPrjDt.cOprt_Ctg
*!*	        lcField1=lcField
*!*	        UPDATE (lcTmpFile) SET &lcTmpFile..cOprt_Ctg1=PmPrjDt.cOprt_Ctg + ' ' +;
*!*	                        IIF(gfSEEK(PmPrjDt.cOprt_Ctg,'PmCtgHd','PmCtgHd'),PmCtgHd.cCtg_Dsc,'');
*!*	                        from PmPrjDt;
*!*	                        WHERE PmPrjDt.CPRJ_TYP+PmPrjDt.CPRJ_ID+PmPrjDt.CSTYLE+PmPrjDt.COPRT_CTG+PmPrjDt.COPRT_ID =;
*!*	                              &lcTmpFile..CPRJ_TYP+&lcTmpFile..CPRJ_ID+&lcTmpFile..CSTYLE ;
*!*	                              .AND. IIF(lcRpOprTyp <> 'A',ASCAN(laTmpltOpr,PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id) <> 0,.t.) .AND. &lcForCond 
*!*	                        
*!*	      ENDIF
*!*	      
*!*	      
*!*	*!*	      UPDATE (lcTmpFile) SET &lcTmpFile..cOprt_Dsc=PmPrjDt.cOprt_Dsc,&lcTmpFile..dEst_Strt1=PmPrjDt.dEst_Strt,;
*!*	*!*	                        &lcTmpFile..dEst_Fnsh1=PmPrjDt.dEst_Fnsh,&lcTmpFile..dClc_Strt1=PmPrjDt.dClc_Strt,;
*!*	*!*	                        &lcTmpFile..dClc_Fnsh1=PmPrjDt.dClc_Fnsh,&lcTmpFile..dAct_Strt1=PmPrjDt.dAct_Strt,;
*!*	*!*	                        &lcTmpFile..dAct_Fnsh1=PmPrjDt.dAct_Fnsh,&lcTmpFile..proj_stat=;
*!*	*!*	                        IIF(PmPrjDt.dAct_Fnsh > &lcTmpFile..dReq_Fnsh OR PmPrjDt.dClc_Fnsh > &lcTmpFile..dReq_Fnsh, 'LATE', 'ON TIME');
*!*	*!*	                        from PmPrjDt;
*!*	*!*	                        WHERE PmPrjDt.CPRJ_TYP+PmPrjDt.CPRJ_ID+PmPrjDt.CSTYLE+PmPrjDt.COPRT_CTG+PmPrjDt.COPRT_ID =;
*!*	*!*	                              &lcTmpFile..CPRJ_TYP+&lcTmpFile..CPRJ_ID+&lcTmpFile..CSTYLE ;
*!*	*!*	                              .AND. IIF(lcRpOprTyp <> 'A',ASCAN(laTmpltOpr,PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id) <> 0,.t.) .AND. &lcForCond

*!*	      UPDATE (lcTmpFile) SET &lcTmpFile..cOprt_Dsc=PmPrjDt.cOprt_Id,&lcTmpFile..dEst_Strt1=PmPrjDt.dEst_Strt,;
*!*	                        &lcTmpFile..dEst_Fnsh1=PmPrjDt.dEst_Fnsh,&lcTmpFile..dClc_Strt1=PmPrjDt.dClc_Strt,;
*!*	                        &lcTmpFile..dClc_Fnsh1=PmPrjDt.dClc_Fnsh,&lcTmpFile..dAct_Strt1=PmPrjDt.dAct_Strt,;
*!*	                        &lcTmpFile..dAct_Fnsh1=PmPrjDt.dAct_Fnsh,&lcTmpFile..proj_stat=;
*!*	                        IIF(PmPrjDt.dAct_Fnsh > &lcTmpFile..dReq_Fnsh OR PmPrjDt.dClc_Fnsh > &lcTmpFile..dReq_Fnsh, 'LATE', 'ON TIME');
*!*	                        from PmPrjDt;
*!*	                        WHERE PmPrjDt.CPRJ_TYP+PmPrjDt.CPRJ_ID+PmPrjDt.CSTYLE+PmPrjDt.COPRT_CTG+PmPrjDt.COPRT_ID =;
*!*	                              &lcTmpFile..CPRJ_TYP+&lcTmpFile..CPRJ_ID+&lcTmpFile..CSTYLE;
*!*	                              .AND. IIF(lcRpOprTyp <> 'A',ASCAN(laTmpltOpr,PmPrjDt.cOprt_Ctg+PmPrjDt.cOprt_Id) <> 0,.t.) .AND. &lcForCond
*!*	  
*!*	      lcField=lcField1
*!*	    ENDSCAN
*!*	  ENDIF
*!*	  
*!*	  *TAREK-CHK TMI 05/28/2009 10:48:54 PM [Start] 
*!*	  ELSE
*!*	    DELETE
*!*	  ENDIF
*!*	  *TAREK-CHK TMI 05/28/2009 10:48:54 PM [End  ] 
*!*	ENDSCAN

*:************************************************************************
*: Program file  : lfSyPrjHdr
*: Program desc. : Function assign project title.
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfSyPrjHdr

*!*	DO CASE 
*!*	    CASE oAriaApplication.ActiveModuleID = 'SO'
*!*	         lcTitle = 'SALES ORDERS'
*!*	    CASE oAriaApplication.ActiveModuleID = 'MF'
*!*	         lcTitle = 'CUTTING TICKETS'
*!*	    CASE oAriaApplication.ActiveModuleID = 'PO'
*!*	         lcTitle = 'PURCHASE ORDERS'
*!*	ENDCASE     
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	DO CASE
*!*	  CASE lcRpPrjTp  = 'C'
*!*	    lcTitle = 'CUTTING TICKETS'
*!*	    lcAccVend = ' '
*!*	  CASE lcRpPrjTp  = 'P'      && PO
*!*	    lcTitle = 'PURCHASE ORDERS'
*!*	    lcAccVend = 'VEND.' 
*!*	  CASE lcRpPrjTp  = 'A'      && Adorment Order
*!*	    lcTitle = 'ADORMENT ORDERS'
*!*	    lcAccVend = 'ACC.'   
*!*	  CASE lcRpPrjTp  = 'D'      && Dye Order 
*!*	    lcTitle = 'DYE ORDERS'
*!*	    lcAccVend = 'ACC.'     
*!*	  CASE lcRpPrjTp  = 'N'      && Inter-Location PO
*!*	    lcTitle = 'INTER-LOCATION PO'
*!*	    lcAccVend = 'VEND.'      
*!*	  CASE lcRpPrjTp  = 'R'      && Return PO
*!*	    lcTitle = 'RETURN PO'
*!*	    lcAccVend = 'VEND.'     
*!*	  CASE lcRpPrjTp  = 'O'      && SO
*!*	    lcTitle = 'SALES ORDERS'
*!*	    lcAccVend = 'ACC.'     
*!*	  CASE lcRpPrjTp  = 'T'      && EDI Order
*!*	    lcTitle = 'EDI ORDERS'
*!*	    lcAccVend = 'ACC.'   
*!*	ENDCASE
DO CASE
  CASE lcRpPrjTp  = 'C'
    lcTitle = LANG_CUTTKTS 
    lcAccVend = ' '
  CASE lcRpPrjTp  = 'P'      && PO
    lcTitle = LANG_POS 
    lcAccVend = LANG_VENDDOT
  CASE lcRpPrjTp  = 'A'      && Adorment Order
    lcTitle = 'ADORMENT ORDERS'
    lcAccVend = LANG_ACCDOT
  CASE lcRpPrjTp  = 'D'      && Dye Order 
    lcTitle = LANG_DYES 
    lcAccVend = LANG_ACCDOT
  CASE lcRpPrjTp  = 'N'      && Inter-Location PO
    lcTitle = LANG_INTPOS 
    lcAccVend = LANG_VENDDOT     
  CASE lcRpPrjTp  = 'R'      && Return PO
    lcTitle = LANG_RETPOS 
    lcAccVend = LANG_VENDDOT    
  CASE lcRpPrjTp  = 'O'      && SO
    lcTitle = LANG_SOS
    lcAccVend = LANG_ACCDOT
  CASE lcRpPrjTp  = 'T'      && EDI Order
    lcTitle = LANG_EDIOS 
    lcAccVend = LANG_ACCDOT
ENDCASE
*N000682,1 MMT 02/06/2013 Globalization changes[End]
proj_title=lcTitle

=lfPreData()


*!*************************************************************
*! Name      : lfGetQty
*! Developer : AHMED MAHER KESHK (AMK)
*! Purpose   :
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfGetQty
IF !USED('POSLN')
  =gfOpentable('POSLN','')
ENDIF

DO CASE
  CASE lcRpPrjTp  $ 'OT'
    DIMENSION laRetVal[1]
    laRetVal = 0
    lcKey    = &lcTmpFile..cPrj_Id+&lcTmpFile..cStyle
    lcKey = PADR(ALLTRIM(lcKey),25,'?')
    SELECT SUM(TotQty) FROM OrdLine ;
      WHERE LIKE(lcKey,OrdLine.Order+OrdLine.Style) ;
      INTO ARRAY laRetVal
      
      IF ISNULL(laRetVal[1])
         laRetVal = 0
      ENDIF 
    
    RETURN laRetVal[1]

  CASE lcRpPrjTp  = 'C'
  *=SEEK(cPrj_Id,'CutTktH')
  =gfseek('PU'+cPrj_Id,'Poshdr')  
   *RETURN CutTktH.Pcs_Opn
    RETURN POSHDR.Open
  CASE lcRpPrjTp  $ 'PADNR'
*!*	    DIMENSION laRetVal1[1]
*!*	    DIMENSION laRetVal2[1]
*!*	    STORE 0 TO laRetVal1,laRetVal2
*!*	    lcKey     = &lcTmpFile..cPrj_Id+&lcTmpFile..cStyle
*!*	    lcPrvAlis = SELECT(0)
*!*	    SELECT POSLN
*!*		
*!*	    *lcKey = PADR(ALLTRIM(lcKey),25,'?')
*!*	    lckey = SUBSTR(lckey,1,6)
*!*	    
*!*	    lctempf=gfTempName()
*!*	*    cdd="select * from posln where LIKE('"+lcKey+"',po+style) .AND. TranCd in('1','2','4','5')"
*!*	*    cdd="select * from posln where po = &lckey .AND. TranCd in('1','2','4','5')"
*!*	     cdd="select * from posln where po = lckey .AND. TranCd in(1,2,4,5)"
*!*	    oAriaApplication.RemoteCompanyData.sqlrun(cdd,lctempf,'',oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loOGScroll.Parent.Parent.DataSessionID)
*!*	    LOCATE 
*!*	   * SCAN FOR LIKE(lcKey,po+style) .AND. TranCd $ '1245'
*!*	   SCAN FOR po = lckey .AND. TranCd $ '1245'
*!*	      IF TranCd = '1'
*!*	        laRetVal1[1] = laRetVal1[1] + PosLn.TotQty
*!*	      ELSE
*!*	        IF TranCd $ '245'
*!*	          laRetVal2[1] = laRetVal2[1] + PosLn.TotQty
*!*	        ENDIF
*!*	      ENDIF
*!*	    ENDSCAN
*!*	    SELECT (lcPrvAlis)

*!*	    RETURN laRetVal1[1] - laRetVal2[1]
    =gfseek('PP'+cPrj_Id,'Poshdr')
    RETURN POSHDR.Open
ENDCASE

*:************************************************************************
*: Program file  : lfFillSrBy
*: Program desc. : Fill Sort by Popup array
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfFillSrBy

*AHS
DIMENSION laSortDesc[IIF(oAriaApplication.ActiveModuleId = "MF" , 2 , 3) ,1]
DIMENSION laSortVal[IIF(oAriaApplication.ActiveModuleId = "MF" , 2 , 3),1]
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	DO CASE
*!*	  CASE oAriaApplication.ActiveModuleId = "MF"
*!*	    laSortDesc[1,1] = 'Cut ticket'
*!*	    laSortVal[1,1]  = 'C'
*!*	    laSortDesc[2,1] = 'Required Date'
*!*	    laSortVal[2,1]  = 'R'
*!*	    lcPrjTitle  = 'Only these C/T #'    
*!*	    lcRpSrtBy   = 'C'

*!*	  CASE oAriaApplication.ActiveModuleId = "SO"
*!*	    laSortDesc[1,1] = 'Order'
*!*	    laSortVal[1,1]  = 'O'
*!*	    laSortDesc[2,1] = 'Account'
*!*	    laSortVal[2,1]  = 'A'
*!*	    laSortDesc[3,1] = 'Required Date'
*!*	    laSortVal[3,1]  = 'R'
*!*	    lcPrjTitle  = 'Only these Orders'    
*!*	    lcRpSrtBy   = 'O'

*!*	  CASE oAriaApplication.ActiveModuleId = "PO"
*!*	    laSortDesc[1,1] = 'Order'
*!*	    laSortVal[1,1]  = 'O'
*!*	    laSortDesc[2,1] = 'Vendor'
*!*	    laSortVal[2,1]  = 'V'
*!*	    laSortDesc[3,1] = 'Required Date'
*!*	    laSortVal[3,1]  = 'R'
*!*	    lcRpSrtBy   = 'O'
*!*	    lcPrjTitle  = 'Only these P/O #' 
*!*	ENDCASE
DO CASE
  CASE oAriaApplication.ActiveModuleId = "MF"
    laSortDesc[1,1] = LANG_S_CUTTKT
    laSortVal[1,1]  = 'C'
    laSortDesc[2,1] = LANG_REQDATE
    laSortVal[2,1]  = 'R'
    lcPrjTitle  = LANG_ONLYCTNO
    lcRpSrtBy   = 'C'

  CASE oAriaApplication.ActiveModuleId = "SO"
    laSortDesc[1,1] = LANG_ORDER
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = LANG_ACCOUNT 
    laSortVal[2,1]  = 'A'
    laSortDesc[3,1] = LANG_REQDATE 
    laSortVal[3,1]  = 'R'
    lcPrjTitle  = LANG_ONLYORDERS    
    lcRpSrtBy   = 'O'

  CASE oAriaApplication.ActiveModuleId = "PO"
    laSortDesc[1,1] = LANG_ORDER 
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = LANG_VENDOR
    laSortVal[2,1]  = 'V'
    laSortDesc[3,1] = LANG_REQDATE 
    laSortVal[3,1]  = 'R'
    lcRpSrtBy   = 'O'
    lcPrjTitle  = LANG_ONLYPOS
ENDCASE
*N000682,1 MMT 02/06/2013 Globalization changes[End]
*AHS

*:************************************************************************
*: Program file  : lfvPrjTyp
*: Program desc. : Project type valid function
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfvPrjTyp

DO CASE
  CASE lcRpPrjTp $ 'OT'
    lcInFile   = 'OrdHdr'
    lcField    = 'Order'
    lcAccCode  = 'account'
    lcAccOrVen = 'account'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	    laSortDesc[1,1] = 'Order'
*!*	    laSortVal[1,1]  = 'O'
*!*	    laSortDesc[2,1] = 'Account'
*!*	    laSortVal[2,1]  = 'A'
*!*	    laSortDesc[3,1] = 'Required Date'
*!*	    laSortVal[3,1]  = 'R'

*!*	    lcPrjTitle  = 'Only these Orders'    
*!*	    lcRpSrtBy   = 'O'
    laSortDesc[1,1] = LANG_ORDER
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = LANG_ACCOUNT
    laSortVal[2,1]  = 'A'
    laSortDesc[3,1] = LANG_REQDATE
    laSortVal[3,1]  = 'R'

    lcPrjTitle  = LANG_ONLYORDERS
    lcRpSrtBy   = 'O'
    *N000682,1 MMT 02/06/2013 Globalization changes[End]
  CASE lcRpPrjTp = 'C'
  * lcInFile   = 'CutTktH'
    lcInFile   = 'POSHDR'
  * lcField    = 'CutTkt'
    lcField    = 'PO' 
    lcAccCode  = 'account'
    lcAccOrVen = ''

    DIMENSION laSortDesc[2,1] 
    DIMENSION laSortVal[2,1]
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	    laSortDesc[1,1] = 'Cut ticket'
*!*	    laSortVal[1,1]  = 'C'
*!*	    laSortDesc[2,1] = 'Required Date'
*!*	    laSortVal[2,1]  = 'R'

*!*	    lcPrjTitle  = 'Only these C/T #'    
*!*	    lcRpSrtBy   = 'C'
    laSortDesc[1,1] = LANG_S_CUTTKT
    laSortVal[1,1]  = 'C'
    laSortDesc[2,1] = LANG_REQDATE
    laSortVal[2,1]  = 'R'

    lcPrjTitle  = LANG_ONLYCTNO
    lcRpSrtBy   = 'C'
    *N000682,1 MMT 02/06/2013 Globalization changes[End]
  CASE lcRpPrjTp $ 'PADNR'
    lcInFile   = 'POsHdr'
    lcField    = 'PO'
    lcAccCode  = 'Vendor'
    lcAccOrVen = 'vendor'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	    laSortDesc[1,1] = 'Order'
*!*	    laSortVal[1,1]  = 'O'
*!*	    laSortDesc[2,1] = 'Vendor'
*!*	    laSortVal[2,1]  = 'V'
*!*	    laSortDesc[3,1] = 'Required Date'
*!*	    laSortVal[3,1]  = 'R'
*!*	    lcRpSrtBy   = 'O'
*!*	    lcPrjTitle  = 'Only these P/O #' 
    laSortDesc[1,1] = LANG_ORDER
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = LANG_VENDOR
    laSortVal[2,1]  = 'V'
    laSortDesc[3,1] = LANG_REQDATE
    laSortVal[3,1]  = 'R'
    lcRpSrtBy   = 'O'
    lcPrjTitle  = LANG_ONLYPOS
    *N000682,1 MMT 02/06/2013 Globalization changes[ENd]    
ENDCASE
CLEARREAD()

*:************************************************************************
*: Program file  : lfPreData
*: Program desc. : Project type valid function
*:         Module: Aria Apparel Series.
*:      Developer: AHMED MAHER KESHK (AMK)
*:************************************************************************
FUNCTION lfPreData

DO CASE
  CASE lcRpPrjTp $ 'OT'
    lcInFile   = 'OrdHdr'
    lcField    = 'Order'
    lcAccCode  = 'account'
    lcAccOrVen = 'account'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	    laSortDesc[1,1] = 'Order'
*!*	    laSortVal[1,1]  = 'O'
*!*	    laSortDesc[2,1] = 'Account'
*!*	    laSortVal[2,1]  = 'A'
*!*	    laSortDesc[3,1] = 'Required Date'
*!*	    laSortVal[3,1]  = 'R'

*!*	    lcPrjTitle  = 'Only these Orders'    
    laSortDesc[1,1] = LANG_ORDER 
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = LANG_ACCOUNT
    laSortVal[2,1]  = 'A'
    laSortDesc[3,1] = LANG_REQDATE 
    laSortVal[3,1]  = 'R'

    lcPrjTitle  = LANG_ONLYORDERS
    *N000682,1 MMT 02/06/2013 Globalization changes[END]
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
   * lcRpSrtBy   = 'O'
   *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]

  CASE lcRpPrjTp = 'C'
  * lcInFile   = 'CutTktH'
    lcInFile   = 'POSHDR'
  * lcField    = 'CutTkt'
    lcField    = 'PO'  
    lcAccCode  = 'account'
    lcAccOrVen = ''

    DIMENSION laSortDesc[2,1] 
    DIMENSION laSortVal[2,1]
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	    laSortDesc[1,1] = 'Cut ticket'
*!*	    laSortVal[1,1]  = 'C'
*!*	    laSortDesc[2,1] = 'Required Date'
*!*	    laSortVal[2,1]  = 'R'

*!*	    lcPrjTitle  = 'Only these C/T #'   
    laSortDesc[1,1] = LANG_S_CUTTKT
    laSortVal[1,1]  = 'C'
    laSortDesc[2,1] = LANG_REQDATE
    laSortVal[2,1]  = 'R'

    lcPrjTitle  = LANG_ONLYCTNO
    *N000682,1 MMT 02/06/2013 Globalization changes[END]
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
*    lcRpSrtBy   = 'C'
	*: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
  CASE lcRpPrjTp $ 'PADNR'
    lcInFile   = 'POsHdr'
    lcField    = 'PO'
    lcAccCode  = 'Vendor'
    lcAccOrVen = 'vendor'

    DIMENSION laSortDesc[3,1] 
    DIMENSION laSortVal[3,1]
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	    laSortDesc[1,1] = 'Order'
*!*	    laSortVal[1,1]  = 'O'
*!*	    laSortDesc[2,1] = 'Vendor'
*!*	    laSortVal[2,1]  = 'V'
*!*	    laSortDesc[3,1] = 'Required Date'
*!*	    laSortVal[3,1]  = 'R'
    laSortDesc[1,1] = LANG_ORDER
    laSortVal[1,1]  = 'O'
    laSortDesc[2,1] = LANG_VENDOR
    laSortVal[2,1]  = 'V'
    laSortDesc[3,1] = LANG_REQDATE
    laSortVal[3,1]  = 'R'
    *N000682,1 MMT 02/06/2013 Globalization changes[End]
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[Start]
*    lcRpSrtBy   = 'O'
    *: E302606,2 MMT 07/08/2009 Fix bugs in report layout and data collecting[End]
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
    *lcPrjTitle  = 'Only these P/O #'     
    lcPrjTitle  = LANG_ONLYPOS
    *N000682,1 MMT 02/06/2013 Globalization changes[End]
ENDCASE

*!*************************************************************
*! Name      : lfvStyMaj
*! Developer : AHMED MAHER KESHK (AMK)
*! Purpose   : Validate style major in range entered in grid.
*!*************************************************************
*! Calls     : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStyMaj()
*!*************************************************************
FUNCTION lfvStyMaj

lcMStyle = _screen.ActiveForm.ActiveControl.value

lcSty = STRTRAN(lcMStyle," ","")

IF EMPTY(lcSty)
  lcMStyle = ""
ENDIF

IF !EMPTY(lcOldValue) AND lcMStyle = lcOldValue
  RETURN
ENDIF

lcFldLocNam = _screen.ActiveForm.ActiveControl.name

IF !EMPTY(lcMStyle) AND !SEEK(lcMStyle,'STYLE')
 *lcMStyle=gfStyBrw('M',"","",.F.)
 lcMStyle=gfStyBrw('M',lcMStyle,"",.F.)
  lcMStyle1=lcMStyle
  _screen.ActiveForm.ActiveControl.value = lcMStyle1
ENDIF
RETURN


*!*************************************************************
*! Name      : lfOldValue
*! Developer : AHMED MAHER KESHK (AMK)
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = _screen.ActiveForm.ActiveControl.oldvalue
RETURN


*!*************************************************************
*! Name      : lfvAccount
*! Developer : AHMED MAHER KESHK (AMK)
*! Date      : 31/12/2001
*! Purpose   : Valid function for account
*!*************************************************************
FUNCTION  lfvAccount

lcAccount = _screen.ActiveForm.ActiveControl.value

lcAcont = STRTRAN(lcAccount ," ","")

IF EMPTY(lcAcont)
  lcAccount = ""
ENDIF

IF !EMPTY(lcOldValue) AND lcAccount = lcOldValue
  RETURN
ENDIF

lcFldLocNam = _screen.ActiveForm.ActiveControl.name
IF !EMPTY(lcAccount) AND !SEEK('M'+lcAccount,Customer)
  _screen.ActiveForm.ActiveControl.value = lcAccount
ENDIF

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Hend Ghanem
*! Purpose   : AHMED MAHER KESHK (AMK)
*!*************************************************************
FUNCTION  lfvVendor

lcVendor = _screen.ActiveForm.ActiveControl.value 

lcVend = STRTRAN(lcVendor," ","")

IF EMPTY(lcVend)
  lcVendor = ""
ENDIF

IF !EMPTY(lcOldValue) AND lcVendor = lcOldValue
  RETURN
ENDIF

lcFldLocNam = _screen.ActiveForm.ActiveControl.name

IF !EMPTY(lcVendor) AND !SEEK(lcVendor ,'ApVendor')
  =gfApVnBrow(@lcVendor) 
  _screen.ActiveForm.ActiveControl.value = lcVendor
ENDIF

*!*************************************************************
*! Name      : lfvRpLay
*! Developer : Hend Ghanem
*! Date      : AHMED MAHER KESHK (AMK)
*! Purpose   : Valid function for Report layout
*!*************************************************************
FUNCTION lfvRpLay

IF lcRpRptLy = 'V'
  lcRpOprtyp = 'S'
ELSE
  lcRpOprtyp = 'A'  
EnDIF  
CLEARREAD()

*!*************************************************************
*! Name      : lfvSelOpr
*! Developer : AHMED MAHER KESHK (AMK)
*! Purpose   : Valid function for Selected operation
*!*************************************************************
FUNCTION lfvSelOpr

IF lcRpOprtyp = 'S'
  lcRpRptLy = 'V'
ELSE
  lcRpRptLy = 'H'
EnDIF  
CLEARREAD()


*:******************************************************************************************************
*: Program file  : lfGenrateSelect
*: Program desc. : Function to return filter depend on lcRcExp where this value add to select statement.
*:         Module: Aria Apparel Series.
*:      Developer: AMHED MAHER KESHK (AMH)
*:******************************************************************************************************
FUNCTION lfGenrateSelect
ev = ''

IF EMPTY(ev) 
  ev  = IIF(llRpShLate,'dAct_Fnsh > dEst_Fnsh','')
ELSE
  ev  = ev + IIF(llRpShLate,' .AND. dAct_Fnsh > dEst_Fnsh','')
ENDIF
IF EMPTY(ev) 
  ev = IIF(lcRpPrjStt <> 'A', "cPrj_Stts = '" + lcRpPrjStt + "'",'cPrj_Stts <> "X" ')
ELSE
  ev = ev + IIF(lcRpPrjStt <> 'A', " .AND. cPrj_Stts = '" + lcRpPrjStt + "'",'.AND. cPrj_Stts <> "X" ')
ENDIF
IF 'PMPRJHD.CPRJ_ID' $ lcRpExp
  locations= loogscroll.laogfxflt(1,6)
  IF !EMPTY(locations) AND USED(locations)
    IF EMPTY(ev) 
      ev ='CPRJ_ID in (select CPRJ_ID from ' + locations + ')' 
    ELSE
      ev = ev + ' .AND. CPRJ_ID in (select CPRJ_ID from ' + locations + ')'
    ENDIF
  ENDIF
ENDIF
*!*	IF 'PMPRJHD.DREQ_FNSH' $ lcRpExp
*!*	  locations= loogscroll.laogfxflt(2,6)
*!*	  IF !EMPTY(locations) AND USED(locations)
*!*	    IF EMPTY(ev) 
*!*	      ev ='PMPRJHD.DREQ_FNSH in (select DREQ_FNSH from ' + locations + ')'
*!*	    ELSE
*!*	      ev = ev + ' .AND. PMPRJHD.DREQ_FNSH in (select DREQ_FNSH from ' + locations + ')'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
IF 'PMPRJHD.DREQ_FNSH' $ lcRpExp
  locations= loogscroll.laogfxflt(2,6)
  IF !EMPTY(locations) 
    IF EMPTY(ev) 
    lnfirst = AT('|',locations)
      ev = "BETWEEN(DREQ_FNSH ,CTOD(LEFT('" + locations + "',10)), CTOD(RIGHT('" + locations + "',10)))"
    ELSE
      ev = ev + " .AND. BETWEEN(DREQ_FNSH ,CTOD(LEFT('" + locations + "',10)), CTOD(RIGHT('" + locations + "',10)))"
    ENDIF
  ENDIF
ENDIF

  *locations= loogscroll.laogfxflt(3,6)
  IF !EMPTY(lcMStyle1)
    IF EMPTY(ev) 
      ev = "CSTYLE = '"+ lcMStyle1 +"'"
    ELSE
      ev = ev + " .AND. CSTYLE = '"+ lcMStyle1 +"'"
    ENDIF
  ENDIF

IF 'CUSTOMER.ACCOUNT' $ lcRpExp
  locations= loogscroll.laogfxflt(3,6)
  IF !EMPTY(locations) AND USED(locations)
    IF EMPTY(ev) 
      ev = 'ACCOUNT in (select ACCOUNT from ' + locations+ ')'
    ELSE
      ev = ev + ' .AND. ACCOUNT in (select ACCOUNT from ' + locations+ ')'
    ENDIF
  ENDIF
ENDIF
IF 'APVENDOR.CVENDCODE' $ lcRpExp
  locations= loogscroll.laogfxflt(4,6)
  IF !EMPTY(locations) AND USED(locations)
    IF EMPTY(ev) 
      ev = 'Vendor in (select CVENDCODE from ' + locations+ ')'
    ELSE
      ev = ev + ' .AND. Vendor in (select CVENDCODE from ' + locations+ ')'
    ENDIF
  ENDIF
ENDIF
RETURN ev
*-- end of lfGenrateSelect.



*!*************************************************************
*! Name      : gfStr2Ar
*! Developer : RENEE - Renee Ezzat
*! Purpose   : Cuts string containing elements separated by
*!             delimeter (passed as a parameter) into a one
*!             or two dimensional array.
*!             Adapted from ARIA ADVANTAGE gfSubStr() function
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  :  - lcString   : String to be cut into array 
*!                                      elements
*!                       - lnAryOrPos : Pointer to an array
*!                       - lcSepta    : Separator pattern(s).
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  - One dim. array : 
*!                          =gfStr2Ar(m.mNotify, @laArrName, '|')
*! Example            :  - Two dim. array : 
*!                          =gfStr2Ar('P|S~Purchase|Sales', 
*!                                    @laArrName, '|~')
*!*************************************************************
FUNCTION gfStr2Ar
PARAMETERS lcString,lnAryOrPos,lcSepta
** lcSepta : separator.

lcSubstr  =' '
lnAryDim  = 1
lnAryRows = 1
lnAryCols = 1
lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',') 

IF LEN(ALLTRIM(lcSepta))>1
  lcColSep  = SUBSTR(lcSepta,2,1)
  lcSepta   = LEFT(lcSepta,1)
  lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
              OCCURS(lcSepta,lcString)+;
              IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
              lnAryDim)
  lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
              OCCURS(lcColSep,lcString)+;
              IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
              lnAryDim)
  lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
  lnAryDim  = lnAryDim +(lnAryCols-1)     
  lcString  = STRTRAN(lcString,lcColSep,lcSepta)
ELSE
  lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
             OCCURS(lcSepta,lcString)+;
             IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
             lnAryDim)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
  CASE TYPE ('lnAryOrPos')='U'
    lnAryOrPos = 1

  *** If array strich it to hold all string parts
  CASE TYPE ('lnAryOrPos') $ 'C,L'    
    IF lnAryCols > 1
      DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
    ELSE
      IF ALEN(lnAryOrPos,2) > 0
        DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
      ELSE
        DIMENSION lnAryOrPos[lnAryDim]
      ENDIF  

    ENDIF
    lnAryOrPos  = ' '

ENDCASE

FOR lnArElem  = 1 TO lnAryDim
  IF TYPE ('lnAryOrPos')='N'
    lnArElem = lnAryOrPos
  ENDIF  

  DO CASE
    *** In case of firest string part
    CASE lnArElem = 1
      lcSubstr = SUBSTR(lcString,1,;
      IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

    *** In case of last string part
    CASE lnArElem = lnAryDim
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
      lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
                 SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
    *** In case of any string part from the meddel
    CASE lnArElem > 1
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
                 AT(lcSepta,lcString,lnArElem)-;
                 AT(lcSepta,lcString,lnArElem-1)-1)
  ENDCASE

  IF TYPE ('lnAryOrPos')='N'
    RETURN lcSubstr
  ENDIF  
  
  IF lnAryCols > 1
    lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
  ELSE
    lnAryOrPos[lnArElem] = lcSubstr
  ENDIF
ENDFOR

