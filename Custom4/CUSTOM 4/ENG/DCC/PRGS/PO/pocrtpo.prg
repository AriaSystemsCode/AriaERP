*:***************************************************************************
*: Program file  : POCRTPO.PRG
*: Program desc. : Custom Program to create POs from CSV file
*: System        : Aria Advantage Series.
*: Module        : PO
*: Developer     : Mariam Mazhar(MMT)  C201695(T20150317.0018)
*:***************************************************************************
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018]
*C201732,2 MMT 11/29/2015 Issue#8- Vendor column cannot be browsed from browse button[T20150317.0018]
*C201774,1 AEG 02/01/2016 Issue#1- create screen for creating style po from CSV file for any user[P20160119.0001]
*C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2]
*C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3]
*C201783,1 MMT 02/15/2016 Point#4- Add Cost sheet description and button to open cost Sheet in Cost sheet browser[P20160119.0001-Point#4]
*C201784,1 MMT 02/15/2016 Point#5 - Enable sort function in create PO program grid[P20160119.0001-Point#5]    
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6]
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1]
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001]
*B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001]
*B611142,1 MMT 04/21/2016 Issue#4 Changing vendor does not update style from PO Auto create[P20160119.0001]
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001]
*C201825,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#5 Add Selection buttons[P20160510.0001]
*C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001]
*C201828,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001]
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001]
*C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12]
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1]
*:***************************************************************************
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
IF !USED('NotePad_Re')
  =gfOpenTable('NotePad','NotePad','SH','NotePad_Re')
ENDIF
IF gfSeek('P'+'XXXXXX','NotePad_Re','NOTEPAD')
  SELECT NotePad_Re
  =gfDelete()
  =gfTableUpdate()
  =gfCloseTable('NotePad_Re')
ENDIF
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
*C201774,1 AEG 02/01/2016 Issue#1- Calling screen pocrtpos for veiwing mutiple user csv files [P20160119.0001][Start]
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
PUBLIC llAllUser 
llAllUser = .F.
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
IF  gfModalgen("TRM00000B00006","ALERT",.F.,.F.,'Do you want to see all users files?') =1
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
  llAllUser = .T.
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
  *TRY
     DO FORM (oAriaApplication.ScreenHome+"\po\POCRTPOC.SCX")
  *CATCH
  *ENDTRY
ELSE
*C201774,1 AEG 02/01/2016 Issue#1- Calling  screen pocrtpos for veiwing mutiple user csv files [P20160119.0001][END]
*TRY
  DO FORM (oAriaApplication.ScreenHome+"\po\POCRTPOS.SCX") 
*!*  CATCH
*!*  ENDTRY

*C201774,1 AEG 02/01/2016 Issue#1- Functions related to screen pocrtpos for veiwing mutiple user csv files [P20160119.0001][Start]
ENDIF


*!*************************************************************
*! Name      : lfValidArr
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/21/2016
*! Purpose   : Valid Array of CSV Files name
*!*************************************************************
FUNCTION lfValidArr AS ARRAY
LPARAMETERS laTmpArr , lnTmpCnt ,laNewArr
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
IF !USED('syuuser')
  llT  =gfOpenTable("syuuser",'CUSER_ID','SH')
ENDIF
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
SELECT 'syuuser'
lnArCnt=0
lnCnt = 1
lcTmpNam =''

DO WHILE lnCnt <= lnTmpCnt
lcTmpNam=STRTRAN(UPPER(laTmpArr[lnCnt,1]) ,'PO.CSV', '')

IF !EMPTY(ALLTRIM(lcTmpNam)) .AND. GFSEEK(ALLTRIM(lcTmpNam))
lnArCnt = lnArCnt+1
DIMENSION laNewArr[lnArCnt,2]
laNewArr[lnArCnt,1] = ALLTRIM(cuser_id)
laNewArr[lnArCnt,2] = ALLTRIM(UPPER(laTmpArr[lnCnt,1]))

ENDIF

lnCnt = lnCnt +1
ENDDO
IF ALEN(laNewArr,1)=1 AND TYPE('laNewArr[1,1]')='L'
 RETURN .F.
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : lfInitPoForm
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/21/2016
*! Purpose   : Init Method of the Form 
*!*************************************************************
FUNCTION lfInitPoForm
LPARAMETERS loFormSet
*B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
*lnFilesFound = ADIR(laArrOfFiles, ADDBS(oAriaApplication.RESOURCEHOME)+"*PO.csv" )
lnFilesFound = ADIR(laArrOfFiles, ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+"*PO.csv" )
*B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
IF lnFilesFound > 0

IF !USED('syuuser')
  llT  =gfOpenTable("syuuser",'CUSER_ID','SH')
ENDIF

*!* Validate  File content
  DIMENSION laNewArr[1,2]
IF !lfValidArr(@laArrOfFiles , lnFilesFound ,@laNewArr)
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'There are no files to be processed - please create one from the Style Summary report and then try again')     
  RETURN .F.
ENDIF
*!* Create temp Tables
loFormSet.lcHeadItemTmp = gfTempName()
loFormSet.lcDetItemTmp = gfTempName()
loFormSet.lctmpvend = gfTempName()
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*CREATE CURSOR (loFormSet.lcDetItemTmp) (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14), lcFileName C(20))
CREATE CURSOR (loFormSet.lcDetItemTmp) (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14), lcFileName C(20),Pattern C(10))
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
SELECT (loFormSet.lcDetItemTmp) 
INDEX On lcFileName+Style+Vendor TAG (loFormSet.lcDetItemTmp) 

CREATE CURSOR (loFormSet.lcHeadItemTmp) (lcUser C(15),lcFile C(20) ,lSelected L)
SELECT (loFormSet.lcHeadItemTmp) 
INDEX On lSelected TAG 'selctd' ADDITIVE 
INDEX On lcUser TAG (loFormSet.lcHeadItemTmp) ADDITIVE 
SET DELETED ON
*!* Fill Temp Data
lnRecno = 1
lnI=1

APPEND FROM ARRAY laNewArr 
GO TOP
SELECT (loFormSet.lcDetItemTmp)
SET DELETED ON
DO WHILE lnI<=ALEN(laNewArr,1)
 *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start] 
 *Append from oAriaApplication.RESOURCEHOME+laNewArr[lnI,2] TYPE delimited
 Append from ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+laNewArr[lnI,2] TYPE delimited
 *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
 DELETE FOR EMPTY(ALLTRIM(STYLE))
 *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
 IF BETWEEN(lnRecno,1,RECCOUNT())
 *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
   GOto lnRecno
 *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
 ENDIF
 *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
 Replace lcFileName WITH ALLTRIM(laNewArr[lnI,2]) FOR EMPTY(ALLTRIM(lcFileName))
 lnRecno = RECNO()+1
 lnI=lnI+1
ENDDO
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
IF !USED('STYLE_Pattern')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_Pattern')
ENDIF
SELECT (loFormSet.lcDetItemTmp)
LOCATE
SCAN 
  =gfSeek(EVALUATE(loFormSet.lcDetItemTmp+'.Style'),'STYLE_Pattern','STYLE')
  REPLACE Pattern WITH STYLE_Pattern.Pattern
ENDSCAN
LOCATE
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
lcExpr ="lcFileName ='"+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile'))+"' AND !EMPTY(STYLE)"
SET FILTER TO &lcExpr. 
GO TOP

ELSE
=gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'There are no files to be processed - please create one from the Style Summary report and then try again')     
  RETURN .F.
ENDIF

RETURN .T.


*!*************************************************************
*! Name      : lfChangeMod
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/24/2016
*! Purpose   : Change  mode
*!*************************************************************
FUNCTION lfChangeMod
LPARAMETERS loFormSet 

WITH loFormSet.AriaForm1
.cmdSelect.enabled = .T.
.cmdSelAll.enabled = .T.
.cmdSelNon.enabled = .T.
.cmdInvert.enabled = .T.
.cmdDelSel.enabled = .T.
ENDWITH

WITH loFormSet.AriaForm1.grdUserHdr
  .ReadOnly = .F.
  .Column1.ReadOnly = .F.
  .Column2.ReadOnly = .T.
  .Column3.ReadOnly = .T.
  .Refresh()
ENDWITH

WITH  loFormSet.AriaForm1.grdUserDt
 .Column1.ReadOnly = .T.
 .Column2.ReadOnly = .T.
 .Column3.ReadOnly = .T.
 .Column4.ReadOnly = .T.
 .Column5.ReadOnly = .T.
 .Column6.ReadOnly = .T.
 .Column7.ReadOnly = .T.
 .Column8.ReadOnly = .T.
 .Column9.ReadOnly = .T.
 .Column10.ReadOnly = .T.
 .Column11.ReadOnly = .T.
  
ENDWITH

*!*************************************************************
*! Name      : lfAddGrdSource
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/24/2016
*! Purpose   : Add grid control source
*!*************************************************************
FUNCTION lfAddGrdSource
LPARAMETERS loFormSet 
WITH  loFormSet.AriaForm1.grdUserHdr
  .RecordSource =''
  .RecordSource = loFormSet.lcHeadItemTmp
  .Column1.ControlSource = loFormSet.lcHeadItemTmp+".lSelected"
  .Column1.CurrentControl= "AriaCheckBox1"
  .Column1.Sparse = .F.
  .Column1.ReadOnly = .F.
  .Column2.ControlSource = loFormSet.lcHeadItemTmp+".lcUser"
  .Column2.ReadOnly  = .F.   
  .Column3.ControlSource = loFormSet.lcHeadItemTmp+".lcFile"
  .Column3.ReadOnly  = .F.
  .ReadOnly = .F.
  .RefreSh()
  ENDWITH
  
  WITH  loFormSet.AriaForm1.grdUserDt
  .RecordSource =''
  .RecordSource = loFormSet.lcDetItemTmp
  .Column1.ControlSource = loFormSet.lcDetItemTmp+".Style"
  .Column2.ControlSource = loFormSet.lcDetItemTmp+".Vendor"
  .Column3.ControlSource = loFormSet.lcDetItemTmp+".OTS1"
  .Column4.ControlSource = loFormSet.lcDetItemTmp+".OTS2"
  .Column5.ControlSource = loFormSet.lcDetItemTmp+".OTS3"
  .Column6.ControlSource = loFormSet.lcDetItemTmp+".OTS4"
  .Column7.ControlSource = loFormSet.lcDetItemTmp+".OTS5"
  .Column8.ControlSource = loFormSet.lcDetItemTmp+".OTS6"
  .Column9.ControlSource = loFormSet.lcDetItemTmp+".OTS7"
  .Column10.ControlSource = loFormSet.lcDetItemTmp+".OTS8"
  .Column11.ControlSource = loFormSet.lcDetItemTmp+".TOTOTS"
  .ReadOnly = .F.
  .RefreSh()
  
  
ENDWITH




*!*************************************************************
*! Name      : lfSelectRec
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/26/2016
*! Purpose   : select record from header grid
*!*************************************************************
FUNCTION lfSelectRec
LPARAMETERS loFormSet 
lcAlias = ALIAS()
SELECT (loFormSet.lcHeadItemTmp)
REplace lSelected WITH !lSelected
loFormSet.AriaForm1.grdUserHdr.refresh()
loFormSet.ariaform1.grdUserHdr.AfterRowColChange()
loFormSet.AriaForm1.grdUserDt.refresh()
SELECT (lcAlias)



*!*************************************************************
*! Name      : lfSelectAll
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/26/2016
*! Purpose   : select All record from header grid
*!*************************************************************
FUNCTION lfSelectAll
LPARAMETERS loFormSet 
lcAlias = ALIAS()
SELECT (loFormSet.lcHeadItemTmp)
lnRcNo=RECNO()
REplace lSelected WITH .T. FOR .T.
IF lnRcNo<= RECCOUNT()
GOTO lnRcNo
ENDIF
loFormSet.AriaForm1.grdUserHdr.refresh()
loFormSet.ariaform1.grdUserHdr.AfterRowColChange()
loFormSet.AriaForm1.grdUserDt.refresh()
SELECT (lcAlias)



*!*************************************************************
*! Name      : lfSelectNon
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/26/2016
*! Purpose   : un select All record from header grid
*!*************************************************************
FUNCTION lfSelectNon
LPARAMETERS loFormSet 
lcAlias = ALIAS()
SELECT (loFormSet.lcHeadItemTmp)
lnRcNo=RECNO()
REplace lSelected WITH .F. FOR .T.
IF lnRcNo<= RECCOUNT()
GOTO lnRcNo
ENDIF
loFormSet.AriaForm1.grdUserHdr.refresh()
loFormSet.ariaform1.grdUserHdr.AfterRowColChange()
loFormSet.AriaForm1.grdUserDt.refresh()
SELECT (lcAlias)


*!*************************************************************
*! Name      : lfInvert
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/26/2016
*! Purpose   : Invert All record from header grid
*!*************************************************************
FUNCTION lfInvert
LPARAMETERS loFormSet 
lcAlias = ALIAS()
SELECT (loFormSet.lcHeadItemTmp)
lnRcNo=RECNO()
REplace lSelected WITH !lSelected FOR .T.
IF lnRcNo<= RECCOUNT()
GOTO lnRcNo
ENDIF
loFormSet.AriaForm1.grdUserHdr.refresh()
loFormSet.ariaform1.grdUserHdr.AfterRowColChange()
loFormSet.AriaForm1.grdUserDt.refresh()
SELECT (lcAlias)



*!*************************************************************
*! Name      : lfDeleteSelec
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/26/2016
*! Purpose   : delete all selected csv PO
*!*************************************************************
FUNCTION lfDeleteSelec
LPARAMETERS loFormSet 
lcAlias = ALIAS()


SELECT (loFormSet.lcHeadItemTmp)
SET ORDER TO selctd
IF SEEK(.T.)
IF  gfModalgen("TRM00000B00006","ALERT",.F.,.F.,'Are you sure you want to delete selected files?') =1
SET ORDER TO (loFormSet.lcHeadItemTmp)
SET DELETED ON
SCAN FOR lSelected
   *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
   *DELETE FILE ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(lcFile)
   DELETE FILE ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(lcFile)
   *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
   loFormSet.lnDelCnt = loFormSet.lnDelCnt + 1
   SELECT (loFormSet.lcDetItemTmp)
   lcExpr ="lcFileName ='"+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile'))+"' AND !EMPTY(STYLE) AND !DELETED()"
   SET FILTER TO &lcExpr.
   SET DELETED ON
   DELETE ALL FOR lcFilename = ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile'))
ENDSCAN
DELETE ALL FOR lSelected
GO TOP
SELECT (loFormSet.lcDetItemTmp)
loFormSet.ariaform1.grdUserHdr.AfterRowColChange()
loFormSet.AriaForm1.grdUserHdr.refresh()
loFormSet.AriaForm1.grdUserDt.refresh()
ENDIF
ELSE 
gfModalgen("TRM00000B34000","ALERT",.F.,.F.,'There are no selected files')
ENDIF

SELECT (lcAlias)






*!*************************************************************
*! Name      : lfValidBtn
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/28/2016
*! Purpose   : Validate Screen Buttons
*!*************************************************************
FUNCTION lfValidBtn
LPARAMETERS loFormSet 
lcAlias = ALIAS()
SELECT (loFormSet.lcHeadItemTmp)
SET ORDER TO selctd
*!* check if GRID EMPTY
IF (RECCOUNT()- loFormSet.lnDelCnt) > 0

*!* CHECK IF the curent recored selected 
IF lSelected
 loFormSet.ariaform1.cmdSelect.caption="Un Select"
ELSE
 loFormSet.ariaform1.cmdSelect.caption="Select"
ENDIF


IF !EOF()
lnRecno= RECNO()
*!* Check if there is a selected record in grid
IF SEEK(lSelected)
  loFormSet.ariaform1.cmdSelNon.ENABLED=.T.
ELSE
  loFormSet.ariaform1.cmdSelNon.ENABLED=.F.
ENDIF


*!* Check if there is a selected record in grid
IF SEEK(.T.)
  loFormSet.ariaform1.cmdSelNon.ENABLED=.T.
  loFormSet.ariaform1.cmdDelSel.ENABLED=.T.
ELSE
  loFormSet.ariaform1.cmdSelNon.ENABLED=.F.
  loFormSet.ariaform1.cmdDelSel.ENABLED=.F.
ENDIF


*!* Check if there is no selected record in grid
IF SEEK(.F.)
     loFormSet.ariaform1.cmdSelAll.ENABLED=.T.
ELSE
     loFormSet.ariaform1.cmdSelAll.ENABLED=.F.  
ENDIF



GOTO lnRecno
ENDIF


ELSE
   *!* EMPTY GRID
   loFormSet.ariaform1.cmdSelect.ENABLED=.F.
   loFormSet.ariaform1.cmdSelAll.ENABLED=.F.
   loFormSet.ariaform1.cmdSelNon.ENABLED=.F.
   loFormSet.ariaform1.cmdInvert.ENABLED=.F.   
   loFormSet.ariaform1.cmdDelSel.ENABLED=.F. 
ENDIF
SET ORDER TO (loFormSet.lcHeadItemTmp)
loFormSet.REFRESH()
SELECT (lcAlias)






*!*************************************************************
*! Name      : lfFrmDest
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 01/26/2016
*! Purpose   : Forrm Destroy event 
*!*************************************************************
FUNCTION lfFrmDest
LPARAMETERS loFormSet 


IF !USED('apvendor')
llT  =gfOpenTable("apvendor",'VENCODE','SH')
ENDIF
*C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][Start]
*CREATE CURSOR (loFormSet.lctmpvend) (lcUser C(15), CVENDCODE C(8),cvencomp C(30) )
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
*CREATE CURSOR (loFormSet.lctmpvend) (lcUser C(15), CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2))
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*CREATE CURSOR (loFormSet.lctmpvend) (lcUser C(15), CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2),nStyCount N(5))
CREATE CURSOR (loFormSet.lctmpvend) (lcUser C(15), CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2),nStyCount N(5),Pattern C(10))
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
*C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][End]
SELECT (loFormSet.lctmpvend) 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*INDEX On lcUser TAG (loFormSet.lctmpvend) 
INDEX On lcUser+CVENDCODE +Pattern  TAG (loFormSet.lctmpvend) 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]

SELECT (loFormSet.lcHeadItemTmp)
GO TOP 
SCAN FOR !EMPTY(lcUser)

  SELECT (loFormSet.lcDetItemTmp)
  SET FILTER TO
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *SELECT distinct Vendor FROM (loFormSet.lcDetItemTmp) WHERE lcFileName =ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')) INTO ARRAY laVend NOFILTER 
  SELECT distinct Vendor,Pattern FROM (loFormSet.lcDetItemTmp) WHERE lcFileName =ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')) INTO ARRAY laVend NOFILTER 
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
  IF _tally = 0
    LOOP
  ENDIF
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
  lnCnt=1
  SELECT(loFormSet.lctmpvend)
  DO WHILE  lnCnt <= ALEN(laVend,1)

    IF SEEK(ALLTRIM(laVend[lnCnt,1]),'apvendor')
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
      IF !SEEK(PADR(EVALUATE(loFormSet.lcHeadItemTmp+'.lcUser'),15)+PADR(laVend[lnCnt,1],8)+laVend[lnCnt,2],loFormSet.lctmpvend,loFormSet.lctmpvend)
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
        APPEND BLANK
        Replace lcUser    WITH ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcUser')) ,;
                CVENDCODE WITH ALLTRIM(laVend[lnCnt,1]),;
                cvencomp  WITH apvendor.cvencomp  
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
        REPLACE Pattern WITH laVend[lnCnt,2]
      ENDIF
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]       
      *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][Start]
      *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
      *lfGetVendorQtyVal(ALLTRIM(laVend[lnCnt,1]),loFormSet.lctmpvend,oAriaApplication.RESOURCEHOME+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')))      
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
      *lfGetVendorQtyVal(ALLTRIM(laVend[lnCnt,1]),loFormSet.lctmpvend,ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')))
      lfGetVendorQtyVal(ALLTRIM(laVend[lnCnt,1]),laVend[lnCnt,2],loFormSet.lctmpvend,ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')))
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
      *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
      *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][End]
    ENDIF
    lnCnt= lnCnt+1
  ENDDO
ENDSCAN 

*!* Dont forget to fill the vendor name in the dbf
SELECT (loFormSet.lctmpvend) 
*C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][Start]
*lcBrowFlds = [lcUser:H = 'User ID'  :15,CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Vendor Name'  :35 ]
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
*lcBrowFlds = [lcUser:H = 'User ID'  :15,CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Vendor Name'  :35 ,POQty:H = 'Purchase Qty'  :10,POValue:H = 'Purchase Value'  :15,SOValue:H = 'Sales Value'  :15]
lcBrowFlds = [lcUser:H = 'User ID'  :15,CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Vendor Name'  :35 ,POQty:H = 'Purchase Qty'  :10,]+;
             [POValue:H = 'Purchase Value'  :15,nStyCount:H='Styles to Purchase':30,SOValue:H = 'Sales Value'  :15]
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
lcBrowFlds = lcBrowFlds + [,Pattern:H='Pattern' :20] 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
*C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][End]
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
*llContinue = gfBrowse(lcBrowFlds,"Vendors",.F.,'',.F.,.F.,.T.,.F.,.F.,.F.,;
    .F.,"CVENDCODE",.F.,.F.,.F.,.F.,.F.,.F.,"")
  SET DELETED ON    
  lcTempVend = gfTempName()   && Name of file that hold temporary Account data.    
  llContinue = gfBrowse(lcBrowFlds,"Vendors",loFormSet.lctmpvend  ,'',.F.,.F.,.T.,.F.,'lfvSelUser()',.F.,;
      lcTempVend ,"lcUser+CVENDCODE+Pattern",.F.,.F.,.F.,.F.,.F.,.F.,loFormSet.lctmpvend)      
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]

IF llContinue
*!*oppen mariam screen
 *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
 *DO FORM (oAriaApplication.ScreenHome+"\po\POCRTPOS.SCX") WITH ALLTRIM(lcUser) , ALLTRIM(CVENDCODE)
 *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
 *DO FORM (oAriaApplication.ScreenHome+"\po\POCRTPOS.SCX") WITH ALLTRIM(lcUser) , ALLTRIM(CVENDCODE),Pattern
 lcTempVend2 = gfTempName()   && Name of file that hold temporary Account data.    
 SELECT (lcTempVend)
 COPY TO (oAriaApplication.workDir+lcTempVend2+".DBF") WITH cdx
 IF USED(lcTempVend2)
   USE IN (lcTempVend2)
 ENDIF
 DO FORM (oAriaApplication.ScreenHome+"\po\POCRTPOS.SCX") WITH lcTempVend2
 *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End] 
 *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
ELSE
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
*!*  IF USED('syuuser')
*!*    USE IN syuuser
*!*  ENDIF 
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
IF USED(loFormSet.lcHeadItemTmp)
  USE IN loFormSet.lcHeadItemTmp
ENDIF 

IF USED(loFormSet.lcDetItemTmp)
  USE IN loFormSet.lcDetItemTmp
ENDIF


IF USED(loFormSet.lctmpvend)
  USE IN loFormSet.lctmpvend
ENDIF 


ENDIF 



*C201774,1 AEG 02/01/2016 Issue#1- Functions related to screen pocrtpos for veiwing mutiple user csv files [P20160119.0001][End]

*!*************************************************************
*! Name      : lfInitForm
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Init Method of the Form 
*!*************************************************************
FUNCTION lfInitForm
*C201774,1 AEG 02/01/2016 Issue#1- if vendor and user parameter if users chose difrent user csv file [P20160119.0001][Start]
*!* LPARAMETERS loFormSet 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*LPARAMETERS loFormSet ,lcUserId , lcVend
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
*LPARAMETERS loFormSet ,lcUserId , lcVend,lcPattern
LPARAMETERS loFormSet ,lcSelFile
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]


*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
*IF TYPE('lcUserId') = 'C' .AND. TYPE('lcVend') = 'C' .AND. !EMPTY(lcUserId) .AND. !EMPTY(lcVend)
IF TYPE('lcSelFile') = 'C' .AND. !EMPTY(lcSelFile) .AND. FILE(oAriaApplication.workDir+lcSelFile+".DBF")
  USE (oAriaApplication.workDir+lcSelFile+".DBF") IN 0 ORDER 1
  SELECT (lcSelFile)
  LOCATE
  lcUserId = SUBSTR(&lcSelFile..KeyExp,1,15)
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
  
  loFormSet.lcUserId = lcUserId 
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
  *loFormSet.lcVend = lcVend
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  IF USED('TmpSty')
    USE IN 'TmpSty'
  ENDIF
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
  IF !USED('TmpSty')
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
    *CREATE CURSOR 'TmpSty' (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14))
    CREATE CURSOR 'TmpSty' (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14),Pattern C(10))
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
    *APPEND  FROM  (ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(lcUserId)+"PO.csv") TYPE CSV  
    APPEND  FROM  (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(lcUserId)+"PO.csv") TYPE CSV  
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
    IF !USED('STYLE_Pattern')
      =gfOpenTable('STYLE','STYLE','SH','STYLE_Pattern')
    ENDIF
    SELECT  'TmpSty'
    LOCATE
    SCAN 
      =gfSeek(TmpSty.Style,'STYLE_Pattern','STYLE')
      REPLACE Pattern WITH STYLE_Pattern.Pattern
    ENDSCAN
    *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
    SELECT 'TmpSty'
    BLANK  FOR !SEEK(PADR(loFormSet.lcUserId,15)+TmpSty.Vendor +TmpSty.Pattern,lcSelFile)
    DELETE FOR EMPTY(Style)
    *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
    LOCATE
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  ENDIF

  loFormSet.lcTempData = gfTempName()
  loFormSet.lcSalesTemp =  gfTempName()

  CREATE CURSOR (loFormSet.lcSalesTemp) (lSelected L,Style C(19),Order C(6),;
   Account C(6),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),Complete D(8),;
   Alo1 N(12), Alo2 N(12), Alo3 N(12), Alo4 N(12), Alo5 N(12), Alo6 N(12), Alo7 N(12), Alo8 N(12), TotAlo N(14),cOrdline C(6))

  IF !USED('apvendor_V')
    =gfOpenTable('apvendor','VENCODE','SH','apvendor_V')
  ENDIF
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
*!*	  lcSelected = ALLTRIM(lcVend)
*!*	  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*!*	  lcSelectPattern = lcPattern
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
ELSE
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
  loFormSet.lcUserId = ''
  loFormSet.lcVend = ''
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
  *C201774,1 AEG 02/01/2016 Issue#1- if vendor and user parameter if users chose difrent user csv file [P20160119.0001][Start]
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
  *IF !FILE(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
  IF !FILE(ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,ALLTRIM(OAriaApplication.User_ID)+' has not created any file to process - please create one from the Style Summary report and then try again')     
    RETURN .F.
  ENDIF
  IF !USED('TmpSty')
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
    *CREATE CURSOR 'TmpSty' (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14))
    CREATE CURSOR 'TmpSty' (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14),Pattern C(10))
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
    *APPEND  FROM  (ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv") TYPE CSV  
    APPEND  FROM  (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv") TYPE CSV  
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
    IF !USED('STYLE_Pattern')
      =gfOpenTable('STYLE','STYLE','SH','STYLE_Pattern')
    ENDIF
    SELECT  'TmpSty'
    LOCATE
    SCAN 
      =gfSeek(TmpSty.Style,'STYLE_Pattern','STYLE')
      REPLACE Pattern WITH STYLE_Pattern.Pattern
    ENDSCAN
    LOCATE
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  ENDIF

  loFormSet.lcTempData = gfTempName()
  loFormSet.lcSalesTemp =  gfTempName()

  CREATE CURSOR (loFormSet.lcSalesTemp) (lSelected L,Style C(19),Order C(6),;
  Account C(6),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),Complete D(8),;
  Alo1 N(12), Alo2 N(12), Alo3 N(12), Alo4 N(12), Alo5 N(12), Alo6 N(12), Alo7 N(12), Alo8 N(12), TotAlo N(14),cOrdline C(6))
 
  SELECT (loFormSet.lcSalesTemp) 
  INDEX On Style+Order TAG (loFormSet.lcSalesTemp) 
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *SELECT Distinct vendor as CVENDCODE FROM 'TmpSty'  WHERE !EMPTY(vendor) AND !DELETED() INTO CURSOR 'VendorList'
  SELECT Distinct vendor as CVENDCODE,Pattern FROM 'TmpSty'  WHERE !EMPTY(vendor) AND !DELETED() INTO CURSOR 'VendorList'
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][Start]
  *CREATE CURSOR 'Vendors' (CVENDCODE C(8),cvencomp C(30))
  *C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
  *CREATE CURSOR 'Vendors' (CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2))
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *CREATE CURSOR 'Vendors' (CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2),nStyCount N(5))
  CREATE CURSOR 'Vendors' (CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2),nStyCount N(5),Pattern C(10))
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  *C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
  *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][End]
  SELECT 'Vendors' 
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *INDEX on cVendCode TAG 'Vendors' 
  INDEX on cVendCode+Pattern  TAG 'Vendors' 
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  IF !USED('apvendor_V')
    =gfOpenTable('apvendor','VENCODE','SH','apvendor_V')
  ENDIF
  SELECT 'VendorList'
  LOCATE
  SCAN 
    =GFSEEK(VendorList.CVENDCODE,'apvendor_V','VENCODE')
    *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][Start]
    *INSERT INTO 'Vendors' VAlues(VendorList.CVENDCODE,apvendor_V.cvencomp) 
    SELECT 'Vendors' 
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
    IF !SEEK(VendorList.CVENDCODE+VendorList.Pattern,'Vendors','Vendors')
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
      APPEND BLANK
      REPLACE CVENDCODE  WITH VendorList.CVENDCODE,;
	          cvencomp  WITH apvendor_V.cvencomp
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
      REPLACE Pattern WITH VendorList.Pattern
    ENDIF      
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]      
    *lfGetVendorQtyVal(VendorList.CVENDCODE,'Vendors',ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
    *lfGetVendorQtyVal(VendorList.CVENDCODE,'Vendors',ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
    lfGetVendorQtyVal(VendorList.CVENDCODE,VendorList.Pattern,'Vendors',ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
    *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
    SELECT 'VendorList'
    *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][End]
  ENDSCAN  
  SELECT 'Vendors' 
  LOCATE  
  *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][Start]
  *lcBrowFlds = [CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Name'  :35 ]  
  *C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
  *lcBrowFlds = [CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Name'  :35 ,POQty:H = 'Purchase Qty'  :10,POValue:H = 'Purchase Value'  :15,SOValue:H = 'Sales Value'  :15]
  lcBrowFlds = [CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Name'  :35 ,POQty:H = 'Purchase Qty'  :10,POValue:H = 'Purchase Value'  :15]+;
  [,nStyCount:H='Styles to Purchase':30,SOValue:H = 'Sales Value'  :15]
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  lcBrowFlds = lcBrowFlds + [,Pattern:H = 'Pattern'  :20] 
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  *C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
  *C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][End]
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
  *llContinue = gfBrowse(lcBrowFlds,"Vendors",.F.,'',.F.,.F.,.T.,.F.,.F.,.F.,;
      .F.,"CVENDCODE",.F.,.F.,.F.,.F.,.F.,.F.,"")
  SET DELETED ON    
  lcTempVend = gfTempName()   && Name of file that hold temporary Account data.    
  llContinue = gfBrowse(lcBrowFlds,"Vendors",'Vendors' ,'',.F.,.F.,.T.,.F.,.F.,.F.,;
      lcTempVend ,"CVENDCODE+Pattern",.F.,.F.,.F.,.F.,.F.,.F.,'Vendors')      
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  IF !llContinue
    RETURN .F.
  ENDIF     
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
  *lcSelected = Vendors.CVENDCODE
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  SELECT 'TmpSty'
  BLANK FOR !SEEK(TmpSty.Vendor +TmpSty.Pattern,lcTempVend)
  DELETE FOR EMPTY(STYLE)
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  *C201774,1 AEG 02/01/2016 Issue#1- if vendor and user parameter if users chose difrent user csv file [P20160119.0001][Start]
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
  *lcSelectPattern =  Vendors.Pattern
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
ENDIF
*C201774,1 AEG 02/01/2016 Issue#1- if vendor and user parameter if users chose difrent user csv file [P20160119.0001][END]

*CREATE CURSOR (loFormSet.lcTempData) (lSelected L,Style C(19),;
               Vendor C(8),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),cdim1 C(5),;
               CCSTSHT_ID C(6),Note_Mem M(10),cnt N(1),CDIVISION C(6),CPURCODE C(6),OrgVendr C(8),Scale C(3),pattern C(10))


*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
*CREATE CURSOR (loFormSet.lcTempData) (lSelected L,Style C(19),;
               Vendor C(8),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),cdim1 C(5),;
               CCSTSHT_ID C(6),Note_Mem M(10),cnt N(1),CDIVISION C(6),CPURCODE C(6),OrgVendr C(8),Scale C(3),pattern C(10))
*C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][Start]               
*!*  CREATE CURSOR (loFormSet.lcTempData) (lSelected L,Style C(19),;
*!*                 Vendor C(8),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),cdim1 C(5),;
*!*                 CCSTSHT_ID C(6),Note_Mem M(10),cnt N(1),CDIVISION C(6),CPURCODE C(6),OrgVendr C(8),Scale C(3),pattern C(10),cphoneno C(16),cStyComp C(19),cCmpVend C(8),cCmpVenPh C(16))
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
*!*  CREATE CURSOR (loFormSet.lcTempData) (lSelected L,Style C(19),;
*!*                 Vendor C(8),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),cdim1 C(5),;
*!*                 CCSTSHT_ID C(6),Note_Mem M(10),cnt N(1),CDIVISION C(6),CPURCODE C(6),OrgVendr C(8),Scale C(3),pattern C(10),cphoneno C(16),cStyComp C(19),;
*!*                 cCmpVend C(8),cCmpVenPh C(16),CStyleDesc C(30),CColorDesc C(30),CCompDesc C(30),CComClrDesc C(30))
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
*CREATE CURSOR (loFormSet.lcTempData) (lSelected L,Style C(19),;
               Vendor C(8),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),cdim1 C(5),;
               CCSTSHT_ID C(6),Note_Mem M(10),cnt N(1),CDIVISION C(6),CPURCODE C(6),OrgVendr C(8),Scale C(3),pattern C(10),cphoneno C(16),cStyComp C(19),;
               cCmpVend C(8),cCmpVenPh C(16),CStyleDesc C(30),CColorDesc C(30),CCompDesc C(30),CComClrDesc C(30),CComCstSht C(6),CcComCstDesc C(30))
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]               
*!*  CREATE CURSOR (loFormSet.lcTempData) (lSelected L,Style C(19),;
*!*                 Vendor C(8),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),cdim1 C(5),;
*!*                 CCSTSHT_ID C(6),Note_Mem M(10),cnt N(1),CDIVISION C(6),CPURCODE C(6),OrgVendr C(8),Scale C(3),pattern C(10),cphoneno C(16),cStyComp C(19),;
*!*                 cVendCode C(8),cCmpVenPh C(16),CStyleDesc C(60),CColorDesc C(30),CCompDesc C(60),CComClrDesc C(30),CComCstSht C(6),CcComCstDesc C(30))
CREATE CURSOR (loFormSet.lcTempData) (lSelected L,Style C(19),;
               Vendor C(8),QTY1 N(12), QTY2 N(12), QTY3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  QTY7 N(12), QTY8 N(12), TOTQTY N(14),cdim1 C(5),;
               CCSTSHT_ID C(6),Note_Mem M(10),cnt N(1),CDIVISION C(6),CPURCODE C(6),OrgVendr C(8),Scale C(3),pattern C(10),cphoneno C(16),cStyComp C(19),;
               cVendCode C(8),cCmpVenPh C(16),CStyleDesc C(60),CColorDesc C(30),CCompDesc C(60),CComClrDesc C(30),CComCstSht C(6),CcComCstDesc C(30),;
               CVENSTY C(19),CCOMPVEN C(19))
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]               
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
*C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][End]               
loFormSet.lcStyNotes =  gfTempName()
lcPhonPict = gfPhoneTem()          
CREATE CURSOR (loFormSet.lcStyNotes) (Style C(19),Note_Mem M(10))
INDEX on Style TAG (loFormSet.lcStyNotes)
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]
SELECT (loFormSet.lcTempData) 

*C201784,1 MMT 02/15/2016 Point#5 - Enable sort function in create PO program grid[P20160119.0001-Point#5][Start]  
*!*	INDEX on Vendor TAG 'VendorInd' 
*!*	INDEX on Style  TAG (loFormSet.lcTempData) ADDITIVE 
INDEX on pattern TAG "IndPatt" ADDITIVE 
INDEX on Style  TAG (loFormSet.lcTempData) ADDITIVE 
INDEX on Vendor TAG 'VendorInd' 
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
INDEX on cVendCode TAG 'BaseVend' 
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]
*C201784,1 MMT 02/15/2016 Point#5 - Enable sort function in create PO program grid[P20160119.0001-Point#5][End]

IF !USED('Scale_V')
  =gfOpenTable('Scale','Scale','SH','Scale_V')
ENDIF
IF !USED('Style_V')
  =gfOpenTable('Style','Style','SH','Style_V')
ENDIF

IF !USED('CutPick_V')
  =gfOpenTable('CutPick','CUTORD','SH','CutPick_V')
ENDIF

IF !USED('ORDLINE_V')
  =gfOpenTable('Ordline','ORDLINES','SH','ORDLINE_V')
ENDIF

IF !USED('ORDHDR_V')
  =gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR_V')
ENDIF

IF !USED('BomHeadr_V')
  =gfOpenTable('BomHeadr','BOMHEADR','SH','BomHeadr_V')
ENDIF
loFormSet.lnMajorlen=LEN(gfItemMask("PM"))
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
IF !USED('Bom_V')
  =gfOpenTable('Bom','MULTIBOM','SH','Bom_V')
ENDIF

IF !USED('NotePad_V')
  =gfOpenTable('NotePad','NotePad','SH','NotePad_V')
ENDIF
STORE  0 TO lnClrLen ,lnClrPos
DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
lnClrLen = 0
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR
lnSclLen=19-(loFormSet.lnMajorlen+lnClrLen+1)  
lnMajorlen =loFormSet.lnMajorlen
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]
SELECT 'TmpSty' 
LOCATE 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*SCAN FOR  Vendor = lcSelected AND !DELETED()
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
*SCAN FOR Vendor = lcSelected AND PATTERN = lcSelectPattern AND !DELETED()
SCAN FOR !DELETED()
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  =gfSeek(TmpSty.Style,'Style_V')
  =gfSeek('S'+Style_V.Scale,'Scale_V')
  SELECT BOMHEADR_V
  lcCostSheet = ''
  IF gfSeek('0001'+PADR(SUBSTR(TmpSty.Style,1,loFormSet.lnMajorlen),19)+"I")
   LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(SUBSTR(TmpSty.Style,1,loFormSet.lnMajorlen),19)+"I" FOR LDEFCSTSHT                    
   IF FOUND()
     lcCostSheet = BOMHEADR_V.CCSTSHT_ID 
   ENDIF
  ENDIF
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
  m.CVENSTY =''
  m.CCOMPVEN =''
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
  *C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
  *INSERT INTO (loFormSet.lcTempData) VALUES (.T.,TmpSty.Style,TmpSty.Vendor ,TmpSty.OTS1*-1,;
  TmpSty.OTS2*-1,TmpSty.OTS3*-1,TmpSty.OTS4*-1,TmpSty.OTS5*-1,TmpSty.OTS6*-1,TmpSty.OTS7*-1,TmpSty.OTS8*-1,TmpSty.TOTOTS*-1,;
  Scale_V.cDim1,lcCostSheet,"",Scale_V.cnt,Style_V.CDIVISION,Style_V.CPURCODE,TmpSty.Vendor,Scale_V.Scale,Style_V.pattern)
  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][Start]               
  m.CStyleDesc =''
  m.CColorDesc =''
  m.CCompDesc =''
  m.CComClrDesc =''
  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][End]               
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  m.CComCstSht  = ''
  m.CcComCstDesc =''
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  *** Get Style Componenet from BOM[Start]
  m.cStyComp = ''
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
  *m.cCmpVend = ''  
  m.cVendCode = ''
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][End]  
  m.cCmpVenPh = ''
  lcSClrPart = SUBSTR(TmpSty.Style,loFormSet.lnMajorlen+2,lnClrLen)
  lcSSclPart = SUBSTR(TmpSty.Style,loFormSet.lnMajorlen+lnClrLen+2,lnSclLen)  
  IF gfSeek('0001'+PADR(SUBSTR(TmpSty.Style,1,loFormSet.lnMajorlen),19)+'I'+lcCostSheet+'2','Bom_V','MULTIBOM')
    SELECT Bom_V
    LOCATE 
    lcPriItm =''
    IF !EOF()
      LOCATE FOR ccatgtyp ='S' AND LIKE(STRTRAN(cItmMask,'*','?'),TmpSty.Style) 
      *IF !FOUND()
********        LOCATE FOR ccatgtyp ='S' AND citmmask = 
      *ENDIF
      IF FOUND()
        =ALINES(laLines,BOM_V.MSZCROSREF)
        FOR lnI = 1 TO ALEN(laLines,1)
          laLines[lnI] = LEFT(laLines[lnI],3)
        ENDFOR      
	      IF ASCAN(laLines,lcSSclPart) = 0
	        LOOP
	      ENDIF
	      DIMENSION laSzCrsRef[8]
	      laSzCrsRef = 0
	      lcPItm = Item
	      lcMClrPart = SUBSTR(citmmask,lnMajorlen+2,lnClrLen)
	      lcMSclPart = SUBSTR(citmmask,lnMajorlen+lnClrLen+2,lnSclLen)            
	      llSAlStrs  = '*' $ lcMSclPart AND '*' $ lcMClrPart      
	      lcIClrPart = SUBSTR(lcPItm,lnMajorlen+2,lnClrLen)
	      lcISclPart = SUBSTR(lcPItm,lnMajorlen+lnClrLen+2,lnSclLen) 
	      lcMajItm   = SUBSTR(lcPItm,1,lnMajorlen)
	      llIAlStrs  = '*' $ lcISclPart AND '*' $ lcIClrPart
	      lcItemScale = ''
	      IF !EMPTY(mszcrosref)
	        lnScalePos = ATC(lcSSclPart,mszcrosref)
	        IF lnScalePos > 0 
	          lcScalStr = SUBSTR(mszcrosref,lnScalePos +LEN(lcSSclPart)-1)
	          lnItemScalePOs = ATC('~',lcScalStr )
	          IF lnItemScalePOs> 0 
	            lcScalStr = SUBSTR(lcScalStr ,lnItemScalePOs+1)
	            lnItemScaleEnd = ATC(',',lcScalStr )
	            lcItemScale = SUBSTR(lcScalStr ,1,lnItemScaleEnd-1)
	          ENDIF
	        ENDIF
	      ENDIF
	      IF !EMPTY(lcItemScale) AND gfSEEK('S'+lcItemScale ,'Scale_V','Scale')
	        lcScaleStyle =lcSSclPart 
	        lcSSclPart =lcItemScale 
	        lcCrosRefFld =mszcrosref
	        DIMENSION laSizesArr[1]
	        laSizesArr = ''
	        =gfSubStr(lcCrosRefFld ,@laSizesArr ,CHR(13))
	        FOR lnS = 1 TO 8
	          LNPOSINARR = 0
	          for lnW = 1 to alen(laSizesArr,1)
	            if ALLTRIM(lcItemScale)+','+STR(lnS,1) $ laSizesArr[lnW]
	              LNPOSINARR  = lnW
	              exit
	            ENDif
	          ENDFOR
	          IF LNPOSINARR >0
	            lnChlScl = ATC('~',laSizesArr[LNPOSINARR])
	            IF lnChlScl > 0
	              lcSclPosStr = SUBSTR(laSizesArr[LNPOSINARR],1,lnChlScl -1)
	              lnCommPos = ATC(',',lcSclPosStr )
	              laSzCrsRef [lnS] = VAL(SUBSTR(lcSclPosStr ,lnCommPos +1))
	            ENDIF  
	          ENDIF
	        ENDFOR
	      ENDIF
	      IF llIAlStrs AND llSAlStrs
	        lcPriItm = lcMajItm + "-" + lcSClrPart + lcSSclPart
	      ELSE 
	        IF llSAlStrs AND '*' $ lcIClrPart AND !'*' $ lcISclPart 
	          lcPriItm = lcMajItm + "-" + lcSSclPart + lcISclPart
	        ELSE 
	          IF llSAlStrs AND !'*' $ lcIClrPart AND '*' $ lcISclPart 
	            lcPriItm = lcMajItm + "-" + lcIClrPart + lcSSclPart
	          ELSE 
	            lcPriItm = lcPItm
	          ENDIF 
	        ENDIF  
	      ENDIF 
	      IF '*' $ lcPriItm 
	         lcTempClrPart = SUBSTR(lcPriItm ,lnMajorlen+2,lnClrLen)
	         lcTempSclPart = SUBSTR(lcPriItm ,lnMajorlen+lnClrLen+2,lnSclLen) 
	         IF  '*' $ lcTempClrPart      
	           lcPriItm = lcMajItm + "-" + lcSClrPart + lcTempSclPart 
	         ELSE 
	           lcPriItm = lcMajItm + "-" + lcTempClrPart + lcSSclPart
	         ENDIF
	      ENDIF
	    ENDIF   
    ENDIF
    m.cStyComp = lcPriItm
    *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
    SELECT BOMHEADR_V
    lcCompCostSheet = ''
    IF gfSeek('0001'+PADR(SUBSTR(lcPriItm,1,loFormSet.lnMajorlen),19)+"I")
      LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(SUBSTR(lcPriItm,1,loFormSet.lnMajorlen),19)+"I" FOR LDEFCSTSHT                    
      IF FOUND()
         m.CComCstSht = BOMHEADR_V.CCSTSHT_ID 
         m.CcComCstDesc = BOMHEADR_V.CCSTSHTDSC                    
      ENDIF
    ENDIF
    *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
    
    =gfSeek(lcPriItm,'Style_V','Style')
    *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
    m.CCOMPVEN = Style_V.cVenSty
    *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
    *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][Start]               
    *B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
    *m.CCompDesc = Style_V.Desc
    m.CCompDesc = Style_V.Desc1
    *B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]
    m.CComClrDesc =gfCodDes(SUBSTR(lcPriItm,lnMajorlen+2,lnClrLen),'COLOR     ') 
    *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][End] 
    *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
    *m.cCmpVend = Style_V.vendor    
    m.cVendCode = Style_V.vendor
    *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][END]  
    =GFSEEK(Style_V.vendor,'apvendor_V','VENCODE')
    m.cCmpVenPh = apvendor_V.cphoneno
  ENDIF
  =gfSeek(TmpSty.Style,'Style_V')
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
  m.cVenSty = Style_V.cVenSty
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]

  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][Start]               
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
  *m.CStyleDesc = Style_V.Desc
  m.CStyleDesc = Style_V.Desc1
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]
  m.CColorDesc = gfCodDes(SUBSTR(TmpSty.Style,lnMajorlen+2,lnClrLen),'COLOR     ') 
  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][End]               
  =gfSeek('S'+Style_V.Scale,'Scale_V')
  *CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)                              
  *** Get Style Componenet from BOM[End]
   =GFSEEK(TmpSty.Vendor,'apvendor_V','VENCODE')
  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][Start]               
*!*    INSERT INTO (loFormSet.lcTempData) VALUES (.T.,TmpSty.Style,TmpSty.Vendor ,TmpSty.OTS1*-1,;
*!*    TmpSty.OTS2*-1,TmpSty.OTS3*-1,TmpSty.OTS4*-1,TmpSty.OTS5*-1,TmpSty.OTS6*-1,TmpSty.OTS7*-1,TmpSty.OTS8*-1,TmpSty.TOTOTS*-1,;
*!*    Scale_V.cDim1,lcCostSheet,"",Scale_V.cnt,Style_V.CDIVISION,Style_V.CPURCODE,TmpSty.Vendor,Scale_V.Scale,Style_V.pattern,;
*!*    TRANSFORM(apvendor_V.cphoneno, '@R ' + lcPhonPict),m.cStyComp,m.cCmpVend ,TRANSFORM(m.cCmpVenPh, '@R ' + lcPhonPict))
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
*!*    INSERT INTO (loFormSet.lcTempData) VALUES (.T.,TmpSty.Style,TmpSty.Vendor ,TmpSty.OTS1*-1,;
*!*    TmpSty.OTS2*-1,TmpSty.OTS3*-1,TmpSty.OTS4*-1,TmpSty.OTS5*-1,TmpSty.OTS6*-1,TmpSty.OTS7*-1,TmpSty.OTS8*-1,TmpSty.TOTOTS*-1,;
*!*    Scale_V.cDim1,lcCostSheet,"",Scale_V.cnt,Style_V.CDIVISION,Style_V.CPURCODE,TmpSty.Vendor,Scale_V.Scale,Style_V.pattern,;
*!*    TRANSFORM(apvendor_V.cphoneno, '@R ' + lcPhonPict),m.cStyComp,m.cCmpVend ,TRANSFORM(m.cCmpVenPh, '@R ' + lcPhonPict),;
*!*    m.CStyleDesc,m.CColorDesc,m.CCompDesc,m.CComClrDesc)
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
*!*	  INSERT INTO (loFormSet.lcTempData) VALUES (.T.,TmpSty.Style,TmpSty.Vendor ,TmpSty.OTS1*-1,;
*!*	  TmpSty.OTS2*-1,TmpSty.OTS3*-1,TmpSty.OTS4*-1,TmpSty.OTS5*-1,TmpSty.OTS6*-1,TmpSty.OTS7*-1,TmpSty.OTS8*-1,TmpSty.TOTOTS*-1,;
*!*	  Scale_V.cDim1,lcCostSheet,"",Scale_V.cnt,Style_V.CDIVISION,Style_V.CPURCODE,TmpSty.Vendor,Scale_V.Scale,Style_V.pattern,;
*!*	  TRANSFORM(apvendor_V.cphoneno, '@R ' + lcPhonPict),m.cStyComp,m.cCmpVend ,TRANSFORM(m.cCmpVenPh, '@R ' + lcPhonPict),;
*!*	  m.CStyleDesc,m.CColorDesc,m.CCompDesc,m.CComClrDesc,m.CComCstSht,m.CcComCstDesc)
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
*!*    INSERT INTO (loFormSet.lcTempData) VALUES (.T.,TmpSty.Style,TmpSty.Vendor ,TmpSty.OTS1*-1,;
*!*    TmpSty.OTS2*-1,TmpSty.OTS3*-1,TmpSty.OTS4*-1,TmpSty.OTS5*-1,TmpSty.OTS6*-1,TmpSty.OTS7*-1,TmpSty.OTS8*-1,TmpSty.TOTOTS*-1,;
*!*    Scale_V.cDim1,lcCostSheet,"",Scale_V.cnt,Style_V.CDIVISION,Style_V.CPURCODE,TmpSty.Vendor,Scale_V.Scale,Style_V.pattern,;
*!*    TRANSFORM(apvendor_V.cphoneno, '@R ' + lcPhonPict),m.cStyComp,m.cVendCode ,TRANSFORM(m.cCmpVenPh, '@R ' + lcPhonPict),;
*!*    m.CStyleDesc,m.CColorDesc,m.CCompDesc,m.CComClrDesc,m.CComCstSht,m.CcComCstDesc)
  INSERT INTO (loFormSet.lcTempData) VALUES (.T.,TmpSty.Style,TmpSty.Vendor ,TmpSty.OTS1*-1,;
  TmpSty.OTS2*-1,TmpSty.OTS3*-1,TmpSty.OTS4*-1,TmpSty.OTS5*-1,TmpSty.OTS6*-1,TmpSty.OTS7*-1,TmpSty.OTS8*-1,TmpSty.TOTOTS*-1,;
  Scale_V.cDim1,lcCostSheet,"",Scale_V.cnt,Style_V.CDIVISION,Style_V.CPURCODE,TmpSty.Vendor,Scale_V.Scale,Style_V.pattern,;
  TRANSFORM(apvendor_V.cphoneno, '@R ' + lcPhonPict),m.cStyComp,m.cVendCode ,TRANSFORM(m.cCmpVenPh, '@R ' + lcPhonPict),;
  m.CStyleDesc,m.CColorDesc,m.CCompDesc,m.CComClrDesc,m.CComCstSht,m.CcComCstDesc,m.CVENSTY,m.CCOMPVEN)
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]  
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][End]    
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]

  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][End]                 
  m.Note_Mem = ""
  IF gfSeek('F'+PADR(SUBSTR(TmpSty.Style,1,loFormSet.lnMajorlen),19),'NotePad_V','NotePad')
    m.Note_Mem = NotePad_V.mnotes
  ENDIF
  IF !SEEK(PADR(SUBSTR(TmpSty.Style,1,loFormSet.lnMajorlen),19),loFormSet.lcStyNotes)
    INSERT INTO (loFormSet.lcStyNotes) VALUES (SUBSTR(TmpSty.Style,1,loFormSet.lnMajorlen),m.Note_Mem)
  ENDIF
  *C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]
  ***************** Fill Sales orders Temp
  SELECT Ordline_V
  IF gfSeek(TmpSty.Style,'ORDLINE_V','Ordlines')
    SCAN REST WHILE  STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)= TmpSty.Style FOR CORDTYPE ='O' AND TotQty > 0
      =gfSeek('O'+Ordline_V.Order,'ORDHDR_V','ORDHDR')
      IF ORDHDR_V.Status <> 'O'
        LOOP
      ENDIF
      IF !gfSeek('2'+Ordline_V.Order+STR(ORDLINE_V.LineNo,6),'CutPick_V','CUTORD')
        INSERT INTO  (loFormSet.lcSalesTemp) values(.T.,TmpSty.Style,ORDHDR_V.Order,ORDHDR_V.Account,;
                      Ordline_V.Qty1,Ordline_V.Qty2,Ordline_V.Qty3,Ordline_V.Qty4,Ordline_V.Qty5,Ordline_V.Qty6,;
                      Ordline_V.Qty7,Ordline_V.Qty8,Ordline_V.TotQty,ORDHDR_V.Complete,;
                      Ordline_V.Qty1,Ordline_V.Qty2,Ordline_V.Qty3,Ordline_V.Qty4,Ordline_V.Qty5,Ordline_V.Qty6,;
                      Ordline_V.Qty7,Ordline_V.Qty8,Ordline_V.TotQty,STR(ORDLINE_V.LineNo,6))
      ELSE
        SELECT CutPick_V
        SUM Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty ;
        REST WHILE TRANCD+ORDER+CORDLINE ='2'+Ordline_V.Order+STR(ORDLINE_V.LineNo,6)
        
        IF (Ordline_V.Qty1 - lnQty1)> 0 OR (Ordline_V.Qty2 - lnQty2)> 0 OR ;
           (Ordline_V.Qty3 - lnQty3)> 0 OR (Ordline_V.Qty4 - lnQty4)> 0 OR ;
           (Ordline_V.Qty5 - lnQty5)> 0 OR (Ordline_V.Qty6 - lnQty6)> 0 OR ;
           (Ordline_V.Qty7 - lnQty7)> 0 OR (Ordline_V.Qty8 - lnQty8)> 0 
          INSERT INTO  (loFormSet.lcSalesTemp) values(.T.,TmpSty.Style,ORDHDR_V.Order,ORDHDR_V.Account,;
                      Ordline_V.Qty1 - lnQty1,Ordline_V.Qty2 - lnQty2,Ordline_V.Qty3 - lnQty3,;
                      Ordline_V.Qty4 - lnQty4,Ordline_V.Qty5 - lnQty5,Ordline_V.Qty6 - lnQty6,;
                      Ordline_V.Qty7 - lnQty7,Ordline_V.Qty8 - lnQty8,Ordline_V.TotQty-lnTotQty ,ORDHDR_V.Complete,;
                      Ordline_V.Qty1 - lnQty1,Ordline_V.Qty2 - lnQty2,Ordline_V.Qty3 - lnQty3,;
                      Ordline_V.Qty4 - lnQty4,Ordline_V.Qty5 - lnQty5,Ordline_V.Qty6 - lnQty6,;
                      Ordline_V.Qty7 - lnQty7,Ordline_V.Qty8 - lnQty8,Ordline_V.TotQty-lnTotQty,STR(ORDLINE_V.LineNo,6))

        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  
  lnQty1 = TmpSty.OTS1*-1
  lnQty2 = TmpSty.OTS2*-1
  lnQty3 = TmpSty.OTS3*-1
  lnQty4 = TmpSty.OTS4*-1
  lnQty5 = TmpSty.OTS5*-1
  lnQty6 = TmpSty.OTS6*-1
  lnQty7 = TmpSty.OTS7*-1
  lnQty8 = TmpSty.OTS8*-1

  SELECT ( loFormSet.lcSalesTemp)
  SCAN FOR style = TmpSty.Style
    REPLACE alo1 WITH MIN(lnQty1 ,Alo1),;
            alo2 WITH MIN(lnQty2 ,Alo2),;
            alo3 WITH MIN(lnQty3 ,Alo3),;
            alo4 WITH MIN(lnQty4 ,Alo4),;
            alo5 WITH MIN(lnQty5 ,Alo5),;
            alo6 WITH MIN(lnQty6 ,Alo6),;
            alo7 WITH MIN(lnQty7 ,Alo7),;
            alo8 WITH MIN(lnQty8 ,Alo8),;
            TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
    REPLACE lSelected WITH IIF(TotAlo = 0 ,.F.,.T.)        
    lnQty1 = MAX(lnQty1 - Alo1 ,0)
    lnQty2 = MAX(lnQty2 - Alo2 ,0)
    lnQty3 = MAX(lnQty3 - Alo3 ,0)
    lnQty4 = MAX(lnQty4 - Alo4 ,0)
    lnQty5 = MAX(lnQty5 - Alo5 ,0)
    lnQty6 = MAX(lnQty6 - Alo6 ,0)
    lnQty7 = MAX(lnQty7 - Alo7 ,0)
    lnQty8 = MAX(lnQty8 - Alo8 ,0)    
  ENDSCAN 
  *****
  SELECT 'TmpSty' 
ENDSCAN 
SELECT (loFormSet.lcTempData)
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
lcRelExpr = "PADR(SUBSTR(Style,1,"+STR(loFormSet.lnMajorlen,2)+"),19)"
SET RELATION TO &lcRelExpr. INTO (loFormSet.lcStyNotes) ADDITIVE 
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]
LOCATE

*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
DECLARE loFormSet.laPanelObj[1,6] 
STORE '' TO loFormSet.lapanelobj
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
*!*  loFormSet.laPanelObj[1,1] = 'pbStyNote'
*!*  loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"notes2.bmp"
*!*  loFormSet.laPanelObj[1,3] = 'lfvStyNote'
*!*  loFormSet.laPanelObj[1,4] = "Style Notes"
*!*  loFormSet.laPanelObj[1,5] = "Style Notes"
loFormSet.laPanelObj[1,1] = 'pbPONote'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"notes2.bmp"
loFormSet.laPanelObj[1,3] = 'lfvPONote'
loFormSet.laPanelObj[1,4] = "PO Notes"
loFormSet.laPanelObj[1,5] = "PO Notes"
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
loFormSet.laPanelObj[1,6] = 'E'
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]

*!*************************************************************
*! Name      : lfAddGridSource
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Add grid control source
*!*************************************************************
FUNCTION lfAddGridSource
LPARAMETERS loFormSet 
WITH  loFormSet.AriaForm1.grdPOLines
  .RecordSource =''
  .RecordSource = loFormSet.lcTempData
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .ReadOnly = .T.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  .Column1.ControlSource = loFormSet.lcTempData+".lSelected"
  .Column1.CurrentControl= "AriaCheckBox1"
  .Column1.Sparse = .F.
  .Column1.ReadOnly = .F.
  .Column2.ControlSource = loFormSet.lcTempData+".Vendor"
  .Column2.AriaKeyField1.KeyTextBox.ControlSource = loFormSet.lcTempData+".Vendor"
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .Column2.ReadOnly = .F.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  .Column3.ControlSource = loFormSet.lcTempData+".Style"
  .Column3.ReadOnly  = .T.
  .Column4.ControlSource = loFormSet.lcTempData+".cdim1"
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .Column4.ReadOnly = .T.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  .Column5.ControlSource = loFormSet.lcTempData+".Qty1"
  .Column6.ControlSource = loFormSet.lcTempData+".Qty2"
  .Column7.ControlSource = loFormSet.lcTempData+".Qty3"
  .Column8.ControlSource = loFormSet.lcTempData+".Qty4"
  .Column9.ControlSource = loFormSet.lcTempData+".Qty5"
  .Column10.ControlSource = loFormSet.lcTempData+".Qty6"
  .Column11.ControlSource = loFormSet.lcTempData+".Qty7"
  .Column12.ControlSource = loFormSet.lcTempData+".Qty8"
  .Column13.ControlSource = loFormSet.lcTempData+".TotQty"
  .Column14.ControlSource = loFormSet.lcTempData+".CCSTSHT_ID"
  .Column17.ControlSource = loFormSet.lcTempData+".PATTERN"
  
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .Column5.ReadOnly = .F.
  .Column6.ReadOnly = .F.
  .Column7.ReadOnly = .F.
  .Column8.ReadOnly = .F.
  .Column9.ReadOnly = .F.
  .Column10.ReadOnly = .F.
  .Column11.ReadOnly = .F.
  .Column12.ReadOnly = .F.
  .Column14.ReadOnly = .F.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  
  
  
  
  *C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
  .Column18.ControlSource = loFormSet.lcTempData+".cphoneno"
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .Column17.ReadOnly = .F.
  .Column18.ReadOnly = .F.
  .Column18.Text1.InputMask =gfPhoNeTem() 
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  
  .Column19.ControlSource = loFormSet.lcTempData+".cStyComp" 
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
  *.Column20.ControlSource = loFormSet.lcTempData+".cCmpVend"  
  .Column20.ControlSource = loFormSet.lcTempData+".cVendCode"
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][End]  
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .Column19.ReadOnly = .T.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
  *.Column20.AriaKeyField1.KeyTextBox.ControlSource = loFormSet.lcTempData+".cCmpVend"  
  .Column20.AriaKeyField1.KeyTextBox.ControlSource = loFormSet.lcTempData+".cVendCode"
  *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][End] 
  .Column26.ControlSource = loFormSet.lcTempData+".CComCstSht"
  .Column26.AriaKeyField1.KeyTextBox.ControlSource = loFormSet.lcTempData+".CComCstSht"
  .Column27.ControlSource = loFormSet.lcTempData+".CcComCstDesc"
  
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .Column21.ControlSource = loFormSet.lcTempData+".cCmpVenPh"
  .Column21.ReadOnly = .T.
  .Column26.ReadOnly = .F.
  .Column20.ReadOnly = .F.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][end]

  *C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]
  
  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][Start]
  .Column22.ControlSource = loFormSet.lcTempData+".cStyleDesc"
  .Column23.ControlSource = loFormSet.lcTempData+".CColorDesc"
  .Column24.ControlSource = loFormSet.lcTempData+".CCompDesc"
  .Column25.ControlSource = loFormSet.lcTempData+".CComClrDesc"
  *C201781,1 MMT 02/11/2016 Point#3- Add style desc and color desc to custom create PO[P20160119.0001-Point#3][End]
  
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
  .Column28.ControlSource = loFormSet.lcTempData+".CVENSTY"
  .Column29.ControlSource = loFormSet.lcTempData+".CCOMPVEN"
  .Column28.Readonly = .F.
  .Column29.Readonly = .F.
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]

  .Column14.AriaKeyField1.KeyTextBox.ControlSource = loFormSet.lcTempData+".CCSTSHT_ID"
  .Column14.Sparse = .F.
  .Column15.Sparse = .F.
  .Column16.Sparse = .F.
 
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  .Column22.Readonly = .T.
  .Column23.Readonly = .T.
  .Column24.Readonly = .T.
  .Column25.Readonly = .T.      
  .Column27.Readonly = .T.      
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
  .RefreSh()
ENDWITH
*!*************************************************************
*! Name      : lfChangeMode
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Change  mode
*!*************************************************************
FUNCTION lfChangeMode
LPARAMETERS loFormSet 
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
TRY
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]
WITH loFormSet.AriaForm1.grdPOLines
  .ReadOnly = .F.
  .Column1.ReadOnly = .F.
  .Column2.ReadOnly = .F.
  .Column3.ReadOnly = .T.
  .Column4.ReadOnly = .T.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
  *.Column17.ReadOnly = .T.
  *C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
ENDWITH
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
CATCH
ENDTRY
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]
*C201825,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#5 Add Selection buttons[P20160510.0001][Start]
loFormSet.AriaForm1.cmdSelAll.Enabled = .F.
*C201825,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#5 Add Selection buttons[P20160510.0001][End]
*!*************************************************************
*! Name      : lfVVendor
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Validate Vendor
*!*************************************************************
FUNCTION lfVVendor
*C201732,2 MMT 11/29/2015 Issue#8- Vendor column cannot be browsed from browse button[T20150317.0018][Start]
*LPARAMETERS loFormSet ,lcValue
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
*LPARAMETERS loFormSet ,lcValue,lFromBrowse
LPARAMETERS loFormSet ,lcValue,lFromBrowse,llCompVend
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
*C201732,2 MMT 11/29/2015 Issue#8- Vendor column cannot be browsed from browse button[T20150317.0018][End]
IF !USED('apvendor')
  =gfOpenTable('apvendor','VENCODE','SH')
ENDIF
lcVendor = lcValue
SELECT APVENDOR
gfSETORDER('VenCode')
*C201732,2 MMT 11/29/2015 Issue#8- Vendor column cannot be browsed from browse button[T20150317.0018][Start]
*IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !gfSEEK(lcVendor , 'APVENDOR'))
IF (!EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !gfSEEK(lcVendor , 'APVENDOR'))) OR lFromBrowse
*C201732,2 MMT 11/29/2015 Issue#8- Vendor column cannot be browsed from browse button[T20150317.0018][End]
  =gfApVnBrow(@lcVendor,.F.,'S')
ENDIF
IF EMPTY(lcVendor)
  lcVendor = lcValue  
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]  
ELSE
  IF llCompVend
    REPLACE cCmpVenPh WITH APVENDOR.cphoneno IN (loFormSet.lcTempData)
  ELSE
    REPLACE cphoneno WITH APVENDOR.cphoneno IN (loFormSet.lcTempData)
  ENDIF
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
ENDIF
RETURN (lcVendor)

*!*************************************************************
*! Name      : lfvCostSheet
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Validate Cost Sheet
*!*************************************************************
FUNCTION lfvCostSheet
LPARAMETERS loFormSet ,lcStyle,lcCostSheet,llFromBrowse
IF (!EMPTY(lcStyle) AND !EMPTY(lcCostSheet) .AND. ('?' $ lcCostSheet .OR. !gfSEEK('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorlen),19)+"I"+lcCostSheet, 'BOMHEADR_V'))) OR  llFromBrowse
  SELECT BOMHEADR_V
  IF gfSeek('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorlen),19)+"I")
    *C201783,1 MMT 02/15/2016 Point#4- Add Cost sheet description and button to open cost Sheet in Cost sheet browser[P20160119.0001-Point#4][Start]
    *lcBrowFlds = [CCSTSHT_ID :H = 'Cost Sheet ID'  :10]
    loParentFormSet = loFormSet
    lcBrowFlds = [CCSTSHT_ID :H = 'Cost Sheet ID'  :10,CCSTSHTDSC:H = 'Description'  :30]
    lcBrowFlds = lcBrowFlds +  [,lnCstsht=lfAddButton():10:H ='']
    *C201783,1 MMT 02/15/2016 Point#4- Add Cost sheet description and button to open cost Sheet in Cost sheet browser[P20160119.0001-Point#4][End]
   llContinue = gfBrowse(lcBrowFlds,"BOMHEADR_V",.F.,'',.F.,.F.,.T.,.F.,.F.,.F.,;
    .F.,"CCSTSHT_ID",.F.,.F.,.F.,.F.,.F.,.F.,"")
   IF llContinue  
      lcCostSheet = BOMHEADR_V.CCSTSHT_ID 
   ENDIF      
  ELSE
    =gfModalGen('TRM00052B40011','ALERT') 
    lcCostSheet =""
  ENDIF
ENDIF
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
*!*  IF !EMPTY(lcCostSheet)
*!*    STORE  0 TO lnClrLen ,lnClrPos
*!*    DECLARE laItemSeg[1]
*!*    PRIVATE lnCount 
*!*    =gfItemMask(@laItemSeg)
*!*    lnClrLen = 0
*!*    FOR lnCount = 1 TO ALEN(laItemSeg,1)
*!*      IF laItemSeg[lnCount,1]='C'
*!*        lnClrLen = LEN(laItemSeg[lnCount,3])
*!*        EXIT
*!*      ENDIF
*!*    ENDFOR
*!*    lnSclLen=19-(loFormSet.lnMajorlen+lnClrLen+1)  
*!*    lnMajorlen =loFormSet.lnMajorlen
*!*    m.cStyComp = ''
*!*    m.cCmpVend = ''
*!*    m.cCmpVenPh = ''
*!*    lcSClrPart = SUBSTR(EVALUATE(loFormSet.lcTempData+'.Style'),loFormSet.lnMajorlen+2,lnClrLen)
*!*    lcSSclPart = SUBSTR(EVALUATE(loFormSet.lcTempData+'.Style'),loFormSet.lnMajorlen+lnClrLen+2,lnSclLen)  
*!*    IF gfSeek('0001'+PADR(SUBSTR(EVALUATE(loFormSet.lcTempData+'.Style'),1,loFormSet.lnMajorlen),19)+'I'+SUBSTR(lcCostSheet,1,6)+'2','Bom_V','MULTIBOM')
*!*      SELECT Bom_V
*!*      LOCATE 
*!*      lcPriItm =''
*!*      IF !EOF()
*!*        LOCATE FOR ccatgtyp ='S' AND LIKE(STRTRAN(cItmMask,'*','?'),EVALUATE(loFormSet.lcTempData+'.Style')) 
*!*        IF FOUND()
*!*          =ALINES(laLines,BOM_V.MSZCROSREF)
*!*          FOR lnI = 1 TO ALEN(laLines,1)
*!*            laLines[lnI] = LEFT(laLines[lnI],3)
*!*          ENDFOR      
*!*          IF ASCAN(laLines,lcSSclPart) = 0
*!*            LOOP
*!*          ENDIF
*!*          DIMENSION laSzCrsRef[8]
*!*          laSzCrsRef = 0
*!*          lcPItm = Item
*!*          lcMClrPart = SUBSTR(citmmask,lnMajorlen+2,lnClrLen)
*!*          lcMSclPart = SUBSTR(citmmask,lnMajorlen+lnClrLen+2,lnSclLen)            
*!*          llSAlStrs  = '*' $ lcMSclPart AND '*' $ lcMClrPart      
*!*          lcIClrPart = SUBSTR(lcPItm,lnMajorlen+2,lnClrLen)
*!*          lcISclPart = SUBSTR(lcPItm,lnMajorlen+lnClrLen+2,lnSclLen) 
*!*          lcMajItm   = SUBSTR(lcPItm,1,lnMajorlen)
*!*          llIAlStrs  = '*' $ lcISclPart AND '*' $ lcIClrPart
*!*          lcItemScale = ''
*!*          IF !EMPTY(mszcrosref)
*!*            lnScalePos = ATC(lcSSclPart,mszcrosref)
*!*            IF lnScalePos > 0 
*!*              lcScalStr = SUBSTR(mszcrosref,lnScalePos +LEN(lcSSclPart)-1)
*!*              lnItemScalePOs = ATC('~',lcScalStr )
*!*              IF lnItemScalePOs> 0 
*!*                lcScalStr = SUBSTR(lcScalStr ,lnItemScalePOs+1)
*!*                lnItemScaleEnd = ATC(',',lcScalStr )
*!*                lcItemScale = SUBSTR(lcScalStr ,1,lnItemScaleEnd-1)
*!*              ENDIF
*!*            ENDIF
*!*          ENDIF
*!*          IF !EMPTY(lcItemScale) AND gfSEEK('S'+lcItemScale ,'Scale_V','Scale')
*!*            lcScaleStyle =lcSSclPart 
*!*            lcSSclPart =lcItemScale 
*!*            lcCrosRefFld =mszcrosref
*!*            DIMENSION laSizesArr[1]
*!*            laSizesArr = ''
*!*            =gfSubStr(lcCrosRefFld ,@laSizesArr ,CHR(13))
*!*            FOR lnS = 1 TO 8
*!*              LNPOSINARR = 0
*!*              for lnW = 1 to alen(laSizesArr,1)
*!*                if ALLTRIM(lcItemScale)+','+STR(lnS,1) $ laSizesArr[lnW]
*!*                  LNPOSINARR  = lnW
*!*                  exit
*!*                ENDif
*!*              ENDFOR
*!*              IF LNPOSINARR >0
*!*                lnChlScl = ATC('~',laSizesArr[LNPOSINARR])
*!*                IF lnChlScl > 0
*!*                  lcSclPosStr = SUBSTR(laSizesArr[LNPOSINARR],1,lnChlScl -1)
*!*                  lnCommPos = ATC(',',lcSclPosStr )
*!*                  laSzCrsRef [lnS] = VAL(SUBSTR(lcSclPosStr ,lnCommPos +1))
*!*                ENDIF  
*!*              ENDIF
*!*            ENDFOR
*!*          ENDIF
*!*          IF llIAlStrs AND llSAlStrs
*!*            lcPriItm = lcMajItm + "-" + lcSClrPart + lcSSclPart
*!*          ELSE 
*!*            IF llSAlStrs AND '*' $ lcIClrPart AND !'*' $ lcISclPart 
*!*              lcPriItm = lcMajItm + "-" + lcSSclPart + lcISclPart
*!*            ELSE 
*!*              IF llSAlStrs AND !'*' $ lcIClrPart AND '*' $ lcISclPart 
*!*                lcPriItm = lcMajItm + "-" + lcIClrPart + lcSSclPart
*!*              ELSE 
*!*                lcPriItm = lcPItm
*!*              ENDIF 
*!*            ENDIF  
*!*          ENDIF 
*!*          IF '*' $ lcPriItm 
*!*             lcTempClrPart = SUBSTR(lcPriItm ,lnMajorlen+2,lnClrLen)
*!*             lcTempSclPart = SUBSTR(lcPriItm ,lnMajorlen+lnClrLen+2,lnSclLen) 
*!*             IF  '*' $ lcTempClrPart      
*!*               lcPriItm = lcMajItm + "-" + lcSClrPart + lcTempSclPart 
*!*             ELSE 
*!*               lcPriItm = lcMajItm + "-" + lcTempClrPart + lcSSclPart
*!*             ENDIF
*!*          ENDIF
*!*        ENDIF   
*!*      ENDIF
*!*      m.cStyComp = lcPriItm
*!*      =gfSeek(lcPriItm,'Style_V','Style')
*!*      m.cCmpVend = Style_V.vendor
*!*      =GFSEEK(Style_V.vendor,'apvendor_V','VENCODE')
*!*      m.cCmpVenPh = apvendor_V.cphoneno
*!*    ENDIF
*!*    =gfSeek(EVALUATE(loFormSet.lcTempData+'.Style'),'Style_V')
*!*    =gfSeek('S'+Style_V.Scale,'Scale_V')
*!*    REPLACE cCmpVenPh WITH m.cCmpVenPh,;
*!*            cCmpVend  With m.cCmpVend ,;
*!*            cStyComp  WITH m.cStyComp  IN (loFormSet.lcTempData)
IF !EMPTY(lcCostSheet)
  lfGetCompInfo(loFormSet,lcStyle,lcCostSheet)
ENDIF
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]


RETURN (lcCostSheet)

*!*************************************************************
*! Name      : lfvNewPONotes
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Add PO notes
*!*************************************************************
FUNCTION lfvNewPONotes
LPARAMETERS loFormSet 
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
*DO FORM (oAriaApplication.ScreenHome + 'Arlnotes') WITH loFormSet.lcTempData,.T.
DO FORM (oAriaApplication.ScreenHome +'SY\NOTEPAD.SCX') WITH 'P', 'XXXXXX', .F., EVALUATE(loFormSet.lcTempData+'.Note_Mem'),loFormset
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
*!*************************************************************
*! Name      : lfvQtyPO
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Validate PO Qty
*!*************************************************************
FUNCTION lfvQtyPO
LPARAMETERS loFormSet ,lnValue
IF lnValue < 0
  =gfModalGen('TRM34051B34000','DIALOG')
  RETURN .F.
ENDIF 
TRY   
  REPLACE ToTQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 IN (loFormSet.lcTempData)
CATCH
ENDTRY 
*!*************************************************************
*! Name      : lfvSalesOrders
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Validate PO Linked SO
*!*************************************************************
FUNCTION lfvSalesOrders
LPARAMETERS loFormSet,lcStyle
DO FORM (oAriaApplication.ScreenHome+"\PO\POCRTSO.SCX") WITH loFormSet,lcStyle
SELECT (loFormSet.lcSalesTemp)  
SET FILTER TO

*!*************************************************************
*! Name      : lfInitSalesform
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : The Init of the Sales order form
*!*************************************************************
FUNCTION lfInitSalesform
LPARAMETERS loChildFormSet

lcTempFile = loChildFormSet.loparentformset.lcTempData
lcExpr = "style = '"+loChildFormSet.lcStyle +"'"
SELECT (loChildFormSet.loparentformset.lcSalesTemp)
SET FILTER TO &lcExpr. 
LOCATE   
lfAddSalesGridControlSource(loChildFormSet)

*!*************************************************************
*! Name      : lfAddSalesGridControlSource
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : add Grid control source
*!*************************************************************
FUNCTION lfAddSalesGridControlSource
LPARAMETERS loChildFormSet

=gfSeek(loChildFormSet.lcStyle  ,'Style_V','Style')
=gfSeek('S'+Style_V.Scale,'Scale_V','Scale')
loChildFormSet.AriaForm1.cntPOQTy.Scale = Style_V.Scale
lcTempFile = loChildFormSet.loparentformset.lcTempData
loChildFormSet.AriaForm1.cntPOQTy.TxtQty1.Value = &lcTempFile..Qty1
loChildFormSet.AriaForm1.cntPOQTy.TxtQty2.Value = &lcTempFile..Qty2
loChildFormSet.AriaForm1.cntPOQTy.TxtQty3.Value = &lcTempFile..Qty3
loChildFormSet.AriaForm1.cntPOQTy.TxtQty4.Value = &lcTempFile..Qty4
loChildFormSet.AriaForm1.cntPOQTy.TxtQty5.Value = &lcTempFile..Qty5
loChildFormSet.AriaForm1.cntPOQTy.TxtQty6.Value = &lcTempFile..Qty6
loChildFormSet.AriaForm1.cntPOQTy.TxtQty7.Value = &lcTempFile..Qty7
loChildFormSet.AriaForm1.cntPOQTy.TxtQty8.Value = &lcTempFile..Qty8
loChildFormSet.AriaForm1.cntPOQTy.TxtTotQty.Value = &lcTempFile..TotQty

WITH loChildFormSet.AriaForm1.grdSales
  .RecordSource =''
  .RecordSource = loChildFormSet.loparentformset.lcSalesTemp
  .Column1.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".lSelected"
  .Column1.CurrentControl= "AriaCheckBox1"
  .Column1.Sparse = .F.

  .Column2.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Order"
  .Column2.readonly = .T.

  .Column3.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Account"
  .Column3.readonly = .T.

  .Column4.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Complete"
  .Column4.Readonly = .T.

  .Column5.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo1"
  .Column5.Header1.Caption = Scale_V.Sz1
  .Column6.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo2"
  .Column6.Header1.Caption = Scale_V.Sz2

  .Column7.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo3"
  .Column7.Header1.Caption = Scale_V.Sz3

  .Column8.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo4"
  .Column8.Header1.Caption = Scale_V.Sz4

  .Column9.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo5"
  .Column9.Header1.Caption = Scale_V.Sz5

  .Column10.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo6"
  .Column10.Header1.Caption = Scale_V.Sz6

  .Column11.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo7"
  .Column11.Header1.Caption = Scale_V.Sz7

  .Column12.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Alo8"
  .Column12.Header1.Caption = Scale_V.Sz8

  IF Scale_V.Cnt < 8
    FOR lnC = Scale_V.Cnt+1 TO 8
      .Columns(lnC+4).Visible = .F.
    ENDFOR   
  ENDIF

  .Column13.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".TotALo"
  .Column13.readonly = .T.
  
  .Column14.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty1"
  .Column15.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty2"
  .Column16.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty3"
  .Column17.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty4"
  .Column18.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty5"
  .Column19.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty6"
  .Column20.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty7"
  .Column21.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".Qty8"
  
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
  .Column14.Header1.Caption =  ALLTRIM(Scale_V.Sz1)+'-'+"Ord1"
  .Column15.Header1.Caption=   ALLTRIM(Scale_V.Sz2)+'-'+"Ord2"
  .Column16.Header1.Caption =  ALLTRIM(Scale_V.Sz3)+'-'+"Ord3"
  .Column17.Header1.Caption =  ALLTRIM(Scale_V.Sz4)+'-'+"Ord4"
  .Column18.Header1.Caption =  ALLTRIM(Scale_V.Sz5)+'-'+"Ord5"
  .Column19.Header1.Caption =  ALLTRIM(Scale_V.Sz6)+'-'+"Ord6"
  .Column20.Header1.Caption =  ALLTRIM(Scale_V.Sz7)+'-'+"Ord7"
  .Column21.Header1.Caption =  ALLTRIM(Scale_V.Sz8)+'-'+"Ord8"
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
  
  .Column21.readonly = .T.
  .Column20.readonly = .T.
  .Column19.readonly = .T.
  .Column18.readonly = .T.
  .Column17.readonly = .T.
  .Column16.readonly = .T.
  .Column15.readonly = .T.
  .Column14.readonly = .T.
  
  IF Scale_V.Cnt < 8
    FOR lnC = Scale_V.Cnt+1 TO 8
      .Columns(lnC+13).Visible = .F.
    ENDFOR   
  ENDIF

  .Column22.ControlSource = loChildFormSet.loparentformset.lcSalesTemp+".TotQty"
  .Column22.readonly = .T.
  .Refresh ()
ENDWITH 

*!*************************************************************
*! Name      : lfvSoQty
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Validate SO QTY 
*!*************************************************************
FUNCTION lfvSoQty
LPARAMETERS loFormSet ,lnValue
IF lnValue < 0
  =gfModalGen('TRM34051B34000','DIALOG')
  RETURN .F.
ENDIF 
iF Alo1 > Qty1 OR Alo2 > Qty2 OR Alo3 > Qty3 OR Alo4 > Qty4 OR;
   Alo5 > Qty5 OR Alo6 > Qty6 OR Alo7 > Qty7 OR Alo8 > Qty8
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Alloctaed quantity cannot be greater than ordered quantity')        
  RETURN .F.
ENDIF
REPLACE ToTAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8 IN (loFormSet.loparentformset.lcSalesTemp)

*!*************************************************************
*! Name      : lfQtyPOVSSO
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Validate SO QTY 
*!*************************************************************
FUNCTION lfQtyPOVSSO
LPARAMETERS loFormSet 
lcTempFile = loFormSet.loparentformset.lcTempData
SELECT (loFormSet.loparentformset.lcSalesTemp)
SUM Alo1,Alo2,Alo3,Alo4,alo5,alo6,Alo7,alo8 TO lnAlo1,lnAlo2,lnAlo3,lnAlo4,lnAlo5,lnAlo6,lnAlo7,lnAlo8 FOR Style =loFormSet .lcStyle AND lSelected

IF &lcTempFile..Qty1 < lnAlo1 OR &lcTempFile..Qty2 < lnAlo2 OR &lcTempFile..Qty3 < lnAlo3;
   OR &lcTempFile..Qty4 < lnAlo4 OR &lcTempFile..Qty5 < lnAlo5 OR &lcTempFile..Qty6 < lnAlo6 OR;
   &lcTempFile..Qty7 < lnAlo7 OR &lcTempFile..Qty8 < lnAlo8
   =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'The Total Sales order allocated qty is greater than the PO Qty, cannot proceed.')     
   RETURN .F.
ENDIF
*!*************************************************************
*! Name      : lfCheckPOLines
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Saving
*!*************************************************************
FUNCTION lfCheckPOLines
LPARAMETERS loFormSet 

*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
ldCompleteDateVal =loFormSet.AriaForm1.DtPickerComplete.value
ldAvailableDateVal = loFormSet.AriaForm1.DtPickerAvailable.value
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]
 
lcTempFile = loFormSet.lcTempData
lcSalesTemp = loFormSet.lcSalesTemp

*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
SELECT (lcSalesTemp)
TRY 
  SET ORDER TO (lcSalesTemp)
CATCH
  INDEX On Style+Order TAG (loFormSet.lcSalesTemp) 
  SET ORDER TO (lcSalesTemp)
ENDTRY   
*B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]

SELECT(lcTempFile)
LOCATE FOR lSelected AND !DELETED()
IF !FOUND()
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'No Styles are selected to create PO.')     
  RETURN .F.
ENDIF 
SELECT(lcTempFile)
LOCATE FOR lSelected AND totQty = 0 AND !DELETED()
IF FOUND()
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot create PO with Zero Quantity.')     
  RETURN .F.
ENDIF 

SELECT(lcTempFile)
LOCATE FOR lSelected AND !DELETED() AND !gfSeek(&lcTempFile..Vendor ,'apvendor_V','VENCODE')
IF FOUND()
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Invalid Vendor code, cannot save')     
  RETURN .F.
ENDIF


*!*  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
lcMemoNotes = ''
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
*!*  SELECT(lcTempFile)
*!*  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
*!*  *LOCATE FOR !EMPTY(Note_mem)
*!*  LOCATE
*!*  LOCATE FOR !EMPTY(ALLTRIM(Note_mem))
*!*  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
*!*  IF FOUND()
*!*    lcMemoNotes = &lcTempFile..Note_mem
*!*  ENDIF
*!*  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]

llExitWithError = .F.
lcErrorStyle = ""
SELECT(lcTempFile)
SCAN FOR lSelected AND !DELETED()
  SELECT (lcSalesTemp)
  SUM Alo1,Alo2,Alo3,Alo4,alo5,alo6,Alo7,alo8 TO lnAlo1,lnAlo2,lnAlo3,lnAlo4,lnAlo5,lnAlo6,lnAlo7,lnAlo8 FOR Style =&lcTempFile..Style AND lSelected
  IF &lcTempFile..Qty1 < lnAlo1 OR &lcTempFile..Qty2 < lnAlo2 OR &lcTempFile..Qty3 < lnAlo3;
     OR &lcTempFile..Qty4 < lnAlo4 OR &lcTempFile..Qty5 < lnAlo5 OR &lcTempFile..Qty6 < lnAlo6 OR;
     &lcTempFile..Qty7 < lnAlo7 OR &lcTempFile..Qty8 < lnAlo8
   llExitWithError = .T.
   lcErrorStyle  = &lcTempFile..Style 
   EXIT 
  ENDIF   
ENDSCAN

IF llExitWithError 
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Style: '+lcErrorStyle +', The Total Sales order allocated qty is greater than the PO Qty, cannot proceed.')     
  RETURN .F.
ENDIF  


MPODEFDAYS  = 90&&gfGetMemVar('M_PODEFDAY',oAriaApplication.ActiveCompanyID)
IF TYPE('loPOForm') <> "O"
  DO FORM (oAriaApplication.ScreenHome+"PO\POSTY.SCX") NOSHOW NAME loPOForm LINKED 
  loPOForm.Hide()
  loPOForm.hasnotes = .F.
  

  loPOForm.lBranchScreen=.F.
  loPOForm.NAME = "AARPOSTY"
  loPOForm.minittriggers ()
  lcDatse = SET("Datasession")
  SET DATASESSION TO loPOForm.DATASESSIONID
  IF  ASCAN(loPOForm.laEvntTrig,PADR('MFCSPUPREF',10)) <> 0 
     PUBLIC oPostyRef
     oPostyRef = loPOForm
  ENDIF
  SET DATASESSION TO lcDatse
  loPOForm.dDefaultCompD  = oAriaApplication.SystemDate + MPODEFDAYS
ENDIF
=gfOpenTable(oAriaApplication.DataDir+'NOTEPAD',oAriaApplication.DataDir+'NOTEPAD','SH')
DIMENSION laPOsCreated[1]
laPOsCreated[1] = ""
CREATE CURSOR 'StylePO' (Style C(19))
lcOldProcd = SET("Procedure")
lcOldProcd  = (oAriaApplication.ApplicationHome + 'PO\POCRTPOM.Fxp') +IIF(!EMPTY(lcOldProcd),",",'')+lcOldProcd
SET PROCEDURE TO &lcOldProcd.
lnChoice = 1
SELECT(lcTempFile)
SET ORDER TO  'VendorInd'
lcVendorCode = SPACE(8)
llCreateNewPO = .T.



SCAN FOR lSelected AND !DELETED()
  IF lcVendorCode <> &lcTempFile..Vendor 
    *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
    lcComponentPONum = ''
    *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
    IF loPOForm.BeforeSave()
      loPOForm.SaveFiles(.F.)
      loPOForm.ChangeMode('S')
      llCreateNewPO = .T.
      *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
      IF !EMPTY(ALLTRIM(lcComponentPONum))
         loPOForm.lanewmpos[1] =PADL(ALLTRIM(STR(VAL(loPOForm.lanewmpos[1])-1)),6,'0')
      ENDIF
      *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]

      IF !EMPTY(loPOForm.lanewmpos[1])
        *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
        *INSERT INTO 'NotePad' (key,Type,CDESC,mnotes )Values(loPOForm.lanewmpos[1],'P','Notes For Style P/O Number : ' +loPOForm.lanewmpos[1],&lcTempFile..Note_mem)        
        *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
        IF gfSeek('P'+'XXXXXX','NOTEPAD','NOTEPAD')
          lcMemoNotes = NotePad.mnotes 
          SELECT 'NotePad'
          REPLACE KEY WITH loPOForm.lanewmpos[1],;
                  CDESC WITH 'Notes For Style P/O Number : ' +loPOForm.lanewmpos[1]
           *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
           IF !EMPTY(ALLTRIM(lcComponentPONum))
             REPLACE mnotes WITH  mnotes + CHR(13)+ CHR(10)+ "This is linked to component PO "+lcComponentPONum IN 'NotePad'
           ENDIF
           *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
          =gfReplace('')        
        ELSE
          *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
          *IF !EMPTY(ALLTRIM(lcMemoNotes))
          IF !EMPTY(ALLTRIM(lcMemoNotes)) OR !EMPTY(ALLTRIM(lcComponentPONum))
          *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
        *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
            INSERT INTO 'NotePad' (key,Type,CDESC,mnotes)Values(loPOForm.lanewmpos[1],'P','Notes For Style P/O Number : ' +loPOForm.lanewmpos[1],lcMemoNotes)
        *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
            *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
            IF !EMPTY(ALLTRIM(lcComponentPONum))
              SELECT  'NotePad' 
              REPLACE mnotes WITH  mnotes+ CHR(13)+ CHR(10)+ "This is linked to component PO "+lcComponentPONum
            ENDIF
            *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
          ENDIF
        ENDIF
        *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
        *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
        *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
        IF !EMPTY(ALLTRIM(lcComponentPONum))
          IF gfSeek('P'+lcComponentPONum,'NOTEPAD','NOTEPAD')
            SELECT 'NotePad'
            REPLACE mnotes WITH  mnotes + CHR(13)+ CHR(10)+ "This is linked to embellished PO "+loPOForm.lanewmpos[1]
            =gfReplace('')  
          ELSE
            INSERT INTO 'NotePad' (key,Type,CDESC,mnotes)Values(lcComponentPONum,'P','Notes For Style P/O Number : ' +lcComponentPONum,"This is linked to embellished PO "+loPOForm.lanewmpos[1])  
          ENDIF
        ENDIF
        *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]        
         SELECT 'StylePO'
         LOCATE
         SCAN 
           SELECT(lcSalesTemp)
           =SEEK(StylePO.Style)
           SCAN REST WHILE Style = StylePO.Style FOR lSelected AND Totalo > 0
             INSERT INTO 'CutPick_V' (trancd,cOrdline,cTktLineNo,CtktNo,Order,Qty1,Qty2,Qty3,Qty4,Qty5,qty6,qty7,qty8,totqty,Style) VALUES ;
                              ('2',&lcSalesTemp..cOrdline,STR(1,6),loPOForm.lanewmpos[1],&lcSalesTemp..Order,&lcSalesTemp..Alo1,&lcSalesTemp..Alo2,&lcSalesTemp..Alo3,;
                              &lcSalesTemp..Alo4,&lcSalesTemp..Alo5,&lcSalesTemp..Alo6,&lcSalesTemp..Alo7,&lcSalesTemp..Alo8,&lcSalesTemp..TotAlo, StylePO.Style)  
             SELECT CutPick_V
             =gfReplace('')
             SELECT(lcSalesTemp)                              
           ENDSCAN
         ENDSCAN   
         IF EMPTY(laPOsCreated[1])
           laPOsCreated[1] = loPOForm.lanewmpos[1]
         ELSE
           DIMENSION laPOsCreated[ALEN(laPOsCreated,1)+1]
           laPOsCreated[ALEN(laPOsCreated,1)] = loPOForm.lanewmpos[1]
         ENDIF
         loPOForm.lanewmpos = ""
       ELSE
         REPLACE lselected WITH .F. IN (lcTempFile) 
      ENDIF
    ENDIF
    llCreateNewPO = .T.
    USE IN 'StylePO'
    CREATE CURSOR 'StylePO' (Style C(19))
  ELSE
    llCreateNewPO = .F.
  ENDIF
  lcVendorCode = &lcTempFile..Vendor 

  IF llCreateNewPO 
    loPOForm.ChangeMode ('A')
    loPOForm.AddNew()
    
    *C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
    loPOForm.ariaForm1.pgfPOStyle.page1.cntheaderFolder.dtPickerAvailable.value  = ldAvailableDateVal 
    loPOForm.ariaForm1.pgfPOStyle.page1.cntheaderFolder.DtPickerComplete.value  = ldCompleteDateVal 
    *C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]
    loPOForm.ariaform1.cntPOHeader.kbvendor.keytextbox.Value = &lcTempFile..Vendor 
    loPOForm.ariaform1.cntPOHeader.kbvendor.keytextbox.Valid()
    loPOForm.ariaform1.cntPOHeader.kbwareHouse.KeyTextBox.Value ="DCC"
    loPOForm.ariaform1.cntPOHeader.kbwareHouse.KeyTextBox.Valid()
    loPOForm.ariaform1.pgfPOStyle.page1.cntheaderFolder.cboDivision.Value = &lcTempFile..CDIVISION
    loPOForm.ariaform1.pgfPOStyle.page1.cntheaderFolder.cboPurGroup.Value = &lcTempFile..CPURCODE
   
  ENDIF
  INSERT INTO 'StylePO' VALUES (&lcTempFile..Style)
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.cmdNew.Click()
  llodlext = loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbStyle.llExtendedScale
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbStyle.llExtendedScale = .F.
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbStyle.txtItem.Value = &lcTempFile..Style
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbStyle.txtItem.Valid()
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbStyle.llExtendedScale=llodlext 
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbCostSheet.keytextbox.Value = &lcTempFile..CCSTSHT_ID
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbCostSheet.Tag='N'
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.kbCostSheet.keytextbox.Valid()
  *loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.cntSizesQty.GotFocus()
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.txtTotalQty.Value = &lcTempFile..TotQty 
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.txtTotalQty.Valid()
  
  FOR lnI =1 TO 8
    lcI = STR(lnI,1)
    loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.cntSizesQty.txtQty&lcI..ControlSource = ''
  ENDFOR 
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.txttotalQty.ControlSource = ''
  
  FOR lnI =1 TO 8
    lcI = STR(lnI,1)
    loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.cntSizesQty.txtQty&lcI..Value = 0
  ENDFOR
  
  FOR lnI =1 TO &lcTempFile..cnt
    lcI = STR(lnI,1)
    loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.cntSizesQty.txtQty&lcI..Value = &lcTempFile..QTY&lcI. 
    loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.cntSizesQty.txtQty&lcI..Valid  
  ENDFOR
  
  loPOForm.ariaForm1.pgfPOStyle.page2.cntDetailFolder.editregion1.cntSizesQty.lostFocus()
  loPOForm.cAssociatedReport =''
  lcReportData = ''
  SELECT 'TmpSty'
  LOCATE FOR Vendor = &lcTempFile..OrgVendr AND style = &lcTempFile..Style
  DELETE 

*!*    IF loPOForm.BeforeSave()
*!*      loPOForm.SaveFiles(.F.)
*!*      loPOForm.ChangeMode('S')
*!*    ENDIF
  
ENDSCAN 

***
*C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
lcComponentPONum = ''
*C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
IF  loPOForm.BeforeSave()
      loPOForm.SaveFiles(.F.)
      loPOForm.ChangeMode('S')
      llCreateNewPO = .T.
      IF !EMPTY(loPOForm.lanewmpos[1])
         *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
         *INSERT INTO 'NotePad' (key,Type,CDESC,mnotes )Values(loPOForm.lanewmpos[1],'P','Notes For Style P/O Number : ' +loPOForm.lanewmpos[1],&lcTempFile..Note_mem)
         *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
         *INSERT INTO 'NotePad' (key,Type,CDESC,mnotes )Values(loPOForm.lanewmpos[1],'P','Notes For Style P/O Number : ' +loPOForm.lanewmpos[1],lcMemoNotes)
         *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
         IF !EMPTY(ALLTRIM(lcComponentPONum))
           loPOForm.lanewmpos[1] =PADL(ALLTRIM(STR(VAL(loPOForm.lanewmpos[1])-1)),6,'0')
         ENDIF
         *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
         IF gfSeek('P'+'XXXXXX','NOTEPAD','NOTEPAD')
           lcMemoNotes = NotePad.mnotes 
           SELECT 'NotePad'
           REPLACE KEY WITH loPOForm.lanewmpos[1],;
                   CDESC WITH 'Notes For Style P/O Number : ' +loPOForm.lanewmpos[1]
           *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
           IF !EMPTY(ALLTRIM(lcComponentPONum))
             REPLACE mnotes WITH  mnotes+ CHR(13)+ CHR(10)+ "This is linked to component PO "+lcComponentPONum IN 'NotePad' 
           ENDIF
           *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
           =gfReplace('')        
    
         ELSE
           *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
           *IF !EMPTY(ALLTRIM(lcMemoNotes))
           IF !EMPTY(ALLTRIM(lcMemoNotes)) OR !EMPTY(ALLTRIM(lcComponentPONum))
           *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
             INSERT INTO 'NotePad' (key,Type,CDESC,mnotes)Values(loPOForm.lanewmpos[1],'P','Notes For Style P/O Number : ' +loPOForm.lanewmpos[1],lcMemoNotes)
             *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
             IF !EMPTY(ALLTRIM(lcComponentPONum))
               SELECT 'NotePad'  
               REPLACE mnotes WITH  mnotes+ CHR(13)+ CHR(10)+ "This is linked to component PO "+lcComponentPONum
             ENDIF
             *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
           ENDIF
         ENDIF
         *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
         IF !EMPTY(ALLTRIM(lcComponentPONum))
           IF gfSeek('P'+lcComponentPONum,'NOTEPAD','NOTEPAD')
             SELECT 'NotePad'
             REPLACE mnotes WITH  mnotes + CHR(13)+ CHR(10)+ "This is linked to embellished PO "+loPOForm.lanewmpos[1]
             =gfReplace('')  
           ELSE
             SELECT 'NotePad' 
             INSERT INTO 'NotePad' (key,Type,CDESC,mnotes)Values(lcComponentPONum,'P','Notes For Style P/O Number : ' +lcComponentPONum,"This is linked to embellished PO "+loPOForm.lanewmpos[1])  
           ENDIF
         ENDIF
         *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]
         *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
         *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
         SELECT 'StylePO'
         LOCATE
         SCAN 
           SELECT(lcSalesTemp)
           =SEEK(StylePO.Style)
           SCAN REST WHILE Style = StylePO.Style FOR lSelected AND Totalo > 0
             INSERT INTO 'CutPick_V' (trancd,cOrdline,cTktLineNo,CtktNo,Order,Qty1,Qty2,Qty3,Qty4,Qty5,qty6,qty7,qty8,totqty,Style) VALUES ;
                              ('2',&lcSalesTemp..cOrdline,STR(1,6),loPOForm.lanewmpos[1],&lcSalesTemp..Order,&lcSalesTemp..Alo1,&lcSalesTemp..Alo2,&lcSalesTemp..Alo3,;
                              &lcSalesTemp..Alo4,&lcSalesTemp..Alo5,&lcSalesTemp..Alo6,&lcSalesTemp..Alo7,&lcSalesTemp..Alo8,&lcSalesTemp..TotAlo, StylePO.Style)  
             SELECT CutPick_V
             =gfReplace('')
             SELECT(lcSalesTemp)                              
           ENDSCAN
         ENDSCAN   
         IF EMPTY(laPOsCreated[1])
           laPOsCreated[1] = loPOForm.lanewmpos[1]
         ELSE
           DIMENSION laPOsCreated[ALEN(laPOsCreated,1)+1]
           laPOsCreated[ALEN(laPOsCreated,1)] = loPOForm.lanewmpos[1]
         ENDIF
         loPOForm.lanewmpos = ""
         SELECT 'TmpSty'
         LOCATE FOR Vendor = &lcTempFile..OrgVendr AND style = &lcTempFile..Style
         DELETE 
       ELSE
         REPLACE lselected WITH .F. IN (lcTempFile) 
      ENDIF
      USE IN 'StylePO'
    ENDIF
***
SELECT 'NotePad'
=gfTableUpdate()
SELECT CutPick_V
=gfTableUpdate()
loPOForm.Release()
RELEASE PROCEDURE (oAriaApplication.ApplicationHome+'PO\POCRTPOM.Fxp')

*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
*  =STRTOFILE(CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",0)
IF !EMPTY(loFormSet.lcUserId) AND !EMPTY(loFormSet.lcVend)
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
  *=STRTOFILE(CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(loFormSet.lcUserId)+"PO.csv",0)
  =STRTOFILE(CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(loFormSet.lcUserId)+"PO.csv",0)
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End] 
ELSE
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
  *=STRTOFILE(CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",0)
  =STRTOFILE(CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",0)
  *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
ENDIF
llOneFound = .F.
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]

SELECT 'TmpSty' 
LOCATE 
SCAN FOR !DELETED() AND (OTS1 <> 0 OR OTS2 <> 0 OR OTS3 <> 0 OR OTS4 <> 0 OR OTS5 <> 0 OR OTS6 <> 0 OR OTS7 <> 0 OR OTS8 <> 0)
  lcString = ""+Style+","+ vendor+","+ALLTRIM(STR(OTS1))+","+ALLTRIM(STR(OTS2))+","+;
            ALLTRIM(STR(OTS3))+","+ALLTRIM(STR(OTS4))+","+ALLTRIM(STR(OTS5))+","+ALLTRIM(STR(OTS6))+","+;
            ALLTRIM(STR(OTS7))+","+ALLTRIM(STR(OTS8))+","+ALLTRIM(STR(TOTOTS))
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
  IF !EMPTY(loFormSet.lcUserId) AND !EMPTY(loFormSet.lcVend)
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
    *=STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(loFormSet.lcUserId)+"PO.csv",1)
    =STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(loFormSet.lcUserId)+"PO.csv",1)
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
  ELSE
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
    *=STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",1)
    =STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",1)
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
  ENDIF
  llOneFound = .T.
  *C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
ENDSCAN

*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
IF !llOneFound 
  IF !EMPTY(loFormSet.lcUserId) AND !EMPTY(loFormSet.lcVend)
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
    *ERASE (ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(loFormSet.lcUserId)+"PO.csv")
    ERASE (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(loFormSet.lcUserId)+"PO.csv")
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
  ELSE
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
    *ERASE (ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
    ERASE (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
    *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
  ENDIF 
ENDIF
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]


*C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][Start]
IF FILE(ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv")
  lcDelSetOld = SET("Deleted") 
  SET DELETED OFF
  CREATE CURSOR 'TmpForc'(Style C(19),Month N(2),YEAR N(4), cMonth C(15),cYear C(4),Qty1 N(12), Qty2 N(12), Qty3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  Qty7 N(12), Qty8 N(12), TOTQty N(14))
  SELECT 'TmpForc'
  INDEX On Style  + STR(Year,4) + STR(Month,2) TAG 'TmpForc'
  APPEND FROM (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv") TYPE CSV
  SELECT 'TmpSty' 
  LOCATE 
  SCAN FOR DELETED()
    IF SEEK(TmpSty.Style,'TmpForc')
      SELECT 'TmpForc'
      DELETE REST WHILE Style  + STR(Year,4) + STR(Month,2) = TmpSty.Style
    ENDIF
    SELECT 'TmpSty' 
  ENDSCAN 
  SET DELETED &lcDelSetOld.
  SELECT 'TmpForc'
  LOCATE 
  =STRTOFILE(CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv",0)  
  SCAN FOR !EMPTY(ALLTRIM(Style)) AND Month <> 0 AND !DELETED()
    lcString = ""+Style+","+STR(Month ,2)+","+STR(YEAR,4) +","+cMonth+","+cYear+","+ALLTRIM(STR(QTY1))+","+ALLTRIM(STR(Qty2))+","+;
                  ALLTRIM(STR(Qty3))+","+ALLTRIM(STR(Qty4))+","+ALLTRIM(STR(Qty5))+","+ALLTRIM(STR(Qty6))+","+;
                  ALLTRIM(STR(Qty7))+","+ALLTRIM(STR(Qty8))+","+ALLTRIM(STR(TOTQTY))
    =STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv",1)   
  ENDSCAN
ENDIF
*C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][End]
IF ALEN(laPOsCreated,1)>1 
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"PO numbers "+laPOsCreated[1]+" to "+laPOsCreated[ALEN(laPOsCreated,1)]+" have been created")
ELSE
  IF !EMPTY(laPOsCreated[1])
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,"PO number "+laPOsCreated[1]+" has been created")
  ENDIF  
ENDIF
SELECT(lcTempFile)
SET ORDER TO (lcTempFile)
DELETE ALL FOR lSelected
LOCATE FOR !DELETED()
IF FOUND() 
  RETURN .f.
ELSE
  RETURN .T.
ENDIF  
*!*************************************************************
*! Name      : lfChangeScale
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/07/2015
*! Purpose   : Change grid headers with scale values
*!*************************************************************
FUNCTION lfChangeScale
LPARAMETERS loFormSet 
lcTempFile = loFormSet.lcTempData

=gfSeek("S"+&lcTempFile..Scale,'Scale_V','Scale')
WITH  loFormSet.AriaForm1.grdPOLines
  .Column5.Header1.Caption= Scale_V.sz1
  .Column6.Header1.Caption= Scale_V.sz2
  .Column7.Header1.Caption = Scale_V.sz3
  .Column8.Header1.Caption= Scale_V.sz4
  .Column9.Header1.Caption= Scale_V.sz5
  .Column10.Header1.Caption= Scale_V.sz6
  .Column11.Header1.Caption= Scale_V.sz7
  .Column12.Header1.Caption= Scale_V.sz8
ENDWITH 
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][Start]
*!*************************************************************
*! Name      : lfvStyNote
*! Developer : Mariam Mazhar(MMT)
*! Date      : 11/12/2015
*! Purpose   : Style notes screen
*!*************************************************************
FUNCTION lfvStyNote
PARAMETERS  loFormSet 
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
*DO FORM (oAriaApplication.ScreenHome + 'Arlnotes') WITH loFormSet.lcStyNotes,loFormSet.Activemode <> 'V'
DO FORM (oAriaApplication.ScreenHome + 'Arlnotes') WITH loFormSet.lcStyNotes,.F.
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]
*C201732,1 MMT 11/12/2015 Add new columns to custom create PO program for DCC[T20150317.0018][End]

*C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][Start]
*!*************************************************************
*! Name      : lfGetVendorQtyVal
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/10/2016
*! Purpose   : Get Vendor Qty and value for PO and Sales
*!*************************************************************
FUNCTION lfGetVendorQtyVal
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*LPARAMETERS lcVendor,lcTempName,lcCSVFile
LPARAMETERS lcVendor,lcStyPattern,lcTempName,lcCSVFile
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
lnSelectF = SELECT()
IF USED('TmpImport')
  USE IN 'TmpImport'
ENDIF

CREATE CURSOR 'TmpImport' (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14),Pattern C(10))
APPEND  FROM  (lcCSVFile) TYPE CSV FOR Vendor = lcVendor
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
IF !USED('STYLE_Pattern')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_Pattern')
ENDIF
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
IF !USED('BomH')
  =gfOpenTable('BOMHEADR','BOMHEADR','SH','BomH')
ENDIF
IF !USED('BomD')
  =gfOpenTable('BOM','MULTIBOM','SH','BomD')
ENDIF

IF !USED('ORDH')
  =gfOpenTable('ORDHDR','ORDHDR','SH','ORDH')
ENDIF

IF !USED('ORDL')
  =gfOpenTable('ORDLINE','ORDLINES','SH','ORDL')
ENDIF

IF !USED('CutP')
  =gfOpenTable('CutPick','CUTORD','SH','CutP')
ENDIF

lnMajorlen=LEN(gfItemMask("PM"))
SELECT 'TmpImport'
LOCATE
lnSOQTy = 0
lnPOQty = 0
lnPOValue = 0
SCAN 
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  =gfSeek(TmpImport.Style,'STYLE_Pattern','STYLE')
  REPLACE Pattern WITH STYLE_Pattern.Pattern
  IF STYLE_Pattern.Pattern <> lcStyPattern
    LOOP
  ENDIF
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  *C201784,1 MMT 02/15/2016 Point#5 - Enable sort function in create PO program grid[P20160119.0001-Point#5][Start]
  WAIT WINDOW "Calculating Sales Qty for Style: " + TmpImport.Style NOWAIT 
  *C201784,1 MMT 02/15/2016 Point#5 - Enable sort function in create PO program grid[P20160119.0001-Point#5][End]
  lcStyle = TmpImport.Style
  lnStyQtyPO = ABS(OTS1+OTS2+OTS3+OTS4+OTS5+OTS6+OTS7+OTS8)
  
  SELECT BomH
  lcCostSheet = ''
  lnPOPrice = 0
  IF gfSeek('0001'+PADR(SUBSTR(lcStyle ,1,lnMajorlen),19)+"I")
    LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(SUBSTR(lcStyle ,1,lnMajorlen),19)+"I" FOR LDEFCSTSHT                    
    IF FOUND()
      lcCostSheet = BOMH.CCSTSHT_ID 
    ENDIF
  ENDIF
  *CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)                              
  IF !EMPTY(lcCostSheet) AND gfSeek('0001'+PADR(SUBSTR(lcStyle ,1,lnMajorlen),19)+'I'+lcCostSheet,'BomD','MULTIBOM')
    SELECT BomD
    lnPrice = 0
    SUM totCost TO lnPrice  REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)=;
                  '0001'+PADR(SUBSTR(lcStyle ,1,lnMajorlen),19)+'I'+lcCostSheet FOR ccatgtyp ='P' AND ccurrcode = oAriaApplication.BaseCurrency
    lnPriceEx = 0              
    =Seek('0001'+PADR(SUBSTR(lcStyle ,1,lnMajorlen),19)+'I'+lcCostSheet,'BomD')
    SCAN REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)=;
                  '0001'+PADR(SUBSTR(lcStyle ,1,lnMajorlen),19)+'I'+lcCostSheet FOR ccatgtyp ='P' AND ccurrcode <> oAriaApplication.BaseCurrency
      lnPriceEx = lnPriceEx +  gfAmntDisp(totCost ,'O')            
    ENDSCAN              
    lnPOPrice  = lnPrice +  lnPriceEx
  ENDIF 
  lnPOValue   = lnPOValue + lnPOPrice  *lnStyQtyPO 
  lnPOQty = lnPOQty + lnStyQtyPO 
  
  **** 
  lnStyleSoQty  = 0
  SELECT OrdL
  IF gfSeek(lcStyle ,'OrdL')
    SCAN REST WHILE  STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)= lcStyle FOR CORDTYPE ='O' AND TotQty > 0
      =gfSeek('O'+OrdL.Order,'ORDH','ORDHDR')
      IF ORDH.Status <> 'O'
        LOOP
      ENDIF
      IF !gfSeek('2'+OrdL.Order+STR(OrdL.LineNo,6),'CutP','CUTORD')
        SELECT ORDH
        lnStyleSoQty  = lnStyleSoQty + (Ordl.TotQty * IIF(ORDH.CCURRCODE = oAriaApplication.BaseCurrency,OrdL.PRICE, gfAmntDisp(OrdL.Price ,'O')))
        SELECT ORDL
      ELSE
        SELECT CutP
        SUM Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,totqty TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty ;
        REST WHILE TRANCD+ORDER+CORDLINE ='2'+Ordl.Order+STR(ORDL.LineNo,6)
        
        IF (Ordl.Qty1 - lnQty1)> 0 OR (Ordl.Qty2 - lnQty2)> 0 OR ;
           (Ordl.Qty3 - lnQty3)> 0 OR (Ordl.Qty4 - lnQty4)> 0 OR ;
           (Ordl.Qty5 - lnQty5)> 0 OR (Ordl.Qty6 - lnQty6)> 0 OR ;
           (Ordl.Qty7 - lnQty7)> 0 OR (Ordl.Qty8 - lnQty8)> 0 
          SELECT ORDH
          lnStyleSoQty  = lnStyleSoQty + ((OrdL.TotQty-lnTotQty)* IIF(ORDH.CCURRCODE = oAriaApplication.BaseCurrency,OrdL.PRICE, gfAmntDisp(OrdL.Price ,'O')))
          SELECT ORDL
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  lnSOQTy = lnSOQTy + lnStyleSoQty 
  *****
  
ENDSCAN 
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
TRY 
  SELECT DISTINCT STYLE FROM 'TmpImport' WHERE Pattern = lcStyPattern INTO ARRAY laStyCnt
  lnStyCnt = ALEN(laStyCnt,1)
CATCH
  lnStyCnt = 0
ENDTRY
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
SELECT (lcTempName)
REPLACE POQty WITH lnPOQty ,;
        POValue WITH lnPOValue   ,;
        SOValue WITH lnSOQTy  
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
REPLACE nStyCount WITH lnStyCnt 
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
SELECT (lnSelectF)        
*C201779,1 MMT 02/10/2016 Point#2 -Add PO Qty,PO value, and SO value to create PO vendor browse[P20160119.0001-Point#2][End]
*C201783,1 MMT 02/15/2016 Point#4- Add Cost sheet description and button to open cost Sheet in Cost sheet browser[P20160119.0001-Point#4][Start]
*!*************************************************************
*! Name      : lfAddButton
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/15/2016
*! Purpose   : Add button to open Style cost sheet screen
*!*************************************************************
FUNCTION lfAddButton
 lcExpRet = '<VFP:CommandButton Delegate = "lfOpnCostSheet" SourceControl = "CCSTSHT_ID" HandlerObject= "loParentFormSet"  Caption="..."'    
 RETURN lcExpRet
*!*************************************************************
*! Name      : lfOpenCostSheetScreen
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/15/2016
*! Purpose   : Open Style cost sheet screen
*!*************************************************************
FUNCTION lfOpenCostSheetScreen
LPARAMETERS loFormSet 
  DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH 'I','0001','S',.T.,.T. NOSHOW NAME loUpScreen
  loUpScreen.ChangeMode('S')
  loUpScreen.ariaForm1.cntCostSheet.kbItem.keytextbox.Value = BOMHEADR_V.citmmajor
  loUpScreen.ariaForm1.cntCostSheet.kbItem.keytextbox.Valid()
  loUpScreen.ariaForm1.cntCostSheet.kbCstSht_ID.keytextbox.Value = BOMHEADR_V.CCSTSHT_ID
  loUpScreen.ariaForm1.cntCostSheet.kbCstSht_ID.keytextbox.Valid()
  loUpScreen.Show(1)
*C201783,1 MMT 02/15/2016 Point#4- Add Cost sheet description and button to open cost Sheet in Cost sheet browser[P20160119.0001-Point#4][End]

*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][Start]
*!*************************************************************
*! Name      : lfUpdateStyVend
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Update Style vendor
*!*************************************************************
FUNCTION  lfUpdateStyVend
LPARAMETERS loFormSet,lcVendor,lcStyleCode
lnSelectAls = SELECT() 
lcStyMaj = SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen)
IF !USED('Style_Ven')
  =gfOpenTable('STYLE','STYLE','SH','Style_Ven')
ENDIF
SELECT 'Style_Ven'
=gfSeek(lcStyMaj)
SCAN REST WHILE Style =lcStyMaj
  gfReplace("VENDOR with '"+lcVendor+"'")
ENDSCAN
SELECT 'Style_Ven'
=gfTableUpdate()

SELECT(lnSelectAls)
*!*************************************************************
*! Name      : lfUpdVendPhone
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Update vendor phone
*!*************************************************************
FUNCTION lfUpdVendPhone
LPARAMETERS loFormSet
lcTempFile = loFormSet.lcTempData
llUpdate = .F.
IF  gfModalgen("QRM00000B00006","ALERT",.F.,.F.,'Do you want to update the Phone number of vendor '+ALLTRIM(&lcTempFile..Vendor)+;
      ' to be '+ALLTRIM(&lcTempFile..cphoneno) +' ?') =1
  IF !USED('APVENDOR_VEN')
    =gfOpenTable('APVENDOR','VENCODE','SH','APVENDOR_VEN')
  ENDIF
  lnRecNoT = RECNO(lcTempFile)
  lcVenPhone = &lcTempFile..cphoneno
  lcVendorId = &lcTempFile..Vendor
  SELECT 'APVENDOR_VEN'
  =gfSeek(&lcTempFile..Vendor)
  =gfReplace("CPHONENO WITH '"+&lcTempFile..cphoneno+"'")
  =gfTableUpdate()     
  REPLACE cphoneno WITH lcVenPhone ALL FOR Vendor = lcVendorId IN (lcTempFile)
  IF BETWEEN(lnRecNoT,1,RECCOUNT(lcTempFile))
    GO RECORD lnRecNoT IN  (lcTempFile)
  ENDIF
  llUpdate = .T.
ENDIF
RETURN llUpdate
*!*************************************************************
*! Name      : lfUpdStylePattern
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Update Style Pattern
*!*************************************************************
FUNCTION lfUpdStylePattern
LPARAMETERS loFormSet
lnSelectAls = SELECT() 
lcTempFile = loFormSet.lcTempData
llUpdate = .F.
lcStyMaj = SUBSTR(&lcTempFile..Style,1,loFormSet.lnMajorlen)
IF  gfModalgen("QRM00000B00006","ALERT",.F.,.F.,'Do you want to update the pattern of Style '+ALLTRIM(lcStyMaj)+;
      ' to be '+ALLTRIM(&lcTempFile..Pattern) +' ?') =1
  lnRecNoT = RECNO(lcTempFile)
  lcPattern = &lcTempFile..Pattern
  IF !USED('Style_Patt')
    =gfOpenTable('STYLE','STYLE','SH','Style_Patt')
  ENDIF
  lnRecNoT = RECNO(lcTempFile)
  SELECT 'Style_Patt'
  =gfSeek(lcStyMaj)
  SCAN rest WHILE Style = lcStyMaj
    =gfReplace("Pattern WITH '"+lcPattern +"'")
  ENDSCAN  
  =gfTableUpdate()     
  REPLACE Pattern WITH lcPattern ALL FOR Style  = lcStyMaj IN (lcTempFile)
  IF BETWEEN(lnRecNoT,1,RECCOUNT(lcTempFile))
    GO RECORD lnRecNoT IN  (lcTempFile)
  ENDIF     
  llUpdate = .T.
ENDIF
SELECT(lnSelectAls)
RETURN llUpdate
*!*************************************************************
*! Name      : lfUpdateDefSty
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Udpate Style Default Cost sheet 
*!*************************************************************
FUNCTION lfUpdateDefSty
LPARAMETERS loFormSet,lcCostSheet,lcStyleCode
lcCostSheet = SUBSTR(lcCostSheet,1,6)
lnSelectAls = SELECT() 
lcTempFile = loFormSet.lcTempData
lnRecNoT = RECNO(lcTempFile)
IF !USED('BOMHEADR_UP')
  =gfOpenTable('BomHeadr','BOMHEADR','SH','BOMHEADR_UP')
ENDIF  
 
SELECT BOMHEADR_UP
lcDefCostSheet = ''
IF gfSeek('0001'+PADR(SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen),19)+"I")
  LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen),19)+"I" FOR LDEFCSTSHT                    
  IF FOUND()
    lcDefCostSheet = BOMHEADR_UP.CCSTSHT_ID 
    =gfReplace("LDEFCSTSHT with .F.")
  ENDIF
  IF SEEK('0001'+PADR(SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen),19)+"I"+lcCostSheet)
    =gfReplace("LDEFCSTSHT with .T.")
  ENDIF
  =gfTableUpdate()
  SELECT (lcTempFile)
  LOCATE 
  SCAN FOR SUBSTR(Style,1,loFormSet.lnMajorlen) = SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen)
    REPLACE CCSTSHT_ID WITH lcCostSheet &&ALL FOR SUBSTR(Style,1,loFormSet.lnMajorlen) = SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen) IN (lcTempFile)
    lfGetCompInfo(loFormSet,&lcTempFile..Style,lcCostSheet)
  ENDSCAN 
  IF BETWEEN(lnRecNoT,1,RECCOUNT(lcTempFile))
    GO RECORD lnRecNoT IN  (lcTempFile)
  ENDIF     
ENDIF  
SELECT(lnSelectAls)
*!*************************************************************
*! Name      : lfvCompCostSheet
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Validate Component Cost sheet 
*!*************************************************************
FUNCTION lfvCompCostSheet
LPARAMETERS loFormSet ,lcStyle,lcCostSheet,llFromBrowse
IF (!EMPTY(lcStyle) AND !EMPTY(lcCostSheet) .AND. ('?' $ lcCostSheet .OR. !gfSEEK('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorlen),19)+"I"+lcCostSheet, 'BOMHEADR_V'))) OR  llFromBrowse
  SELECT BOMHEADR_V
  IF gfSeek('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorlen),19)+"I")
    loParentFormSet = loFormSet
    lcBrowFlds = [CCSTSHT_ID :H = 'Cost Sheet ID'  :10,CCSTSHTDSC:H = 'Description'  :30]
    lcBrowFlds = lcBrowFlds +  [,lnCstsht=lfAddButton():10:H ='']
   llContinue = gfBrowse(lcBrowFlds,"BOMHEADR_V",.F.,'',.F.,.F.,.T.,.F.,.F.,.F.,;
    .F.,"CCSTSHT_ID",.F.,.F.,.F.,.F.,.F.,.F.,"")
   IF llContinue  
      lcCostSheet = BOMHEADR_V.CCSTSHT_ID 
      
   ENDIF      
  ELSE
    =gfModalGen('TRM00052B40011','ALERT') 
    lcCostSheet =""
  ENDIF
ENDIF
IF !EMPTY(lcCostSheet)
  REPLACE CcComCstDesc WITH  BOMHEADR_V.CCSTSHTDSC IN (loFormSet.lcTempData)
ENDIF   
RETURN (lcCostSheet)
*!*************************************************************
*! Name      : lfUpdateCompSty
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Update Component Default Cost sheet 
*!*************************************************************
FUNCTION lfUpdateCompSty
LPARAMETERS loFormSet,lcCostSheet,lcStyleCode
lcCostSheet = SUBSTR(lcCostSheet,1,6)
lnSelectAls = SELECT() 
lcTempFile = loFormSet.lcTempData
lnRecNoT = RECNO(lcTempFile)
IF !USED('BOMHEADR_UP')
  =gfOpenTable('BomHeadr','BOMHEADR','SH','BOMHEADR_UP')
ENDIF  
 
SELECT BOMHEADR_UP
lcDefCostSheet = ''
IF gfSeek('0001'+PADR(SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen),19)+"I")
  LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen),19)+"I" FOR LDEFCSTSHT                    
  IF FOUND()
    lcDefCostSheet = BOMHEADR_UP.CCSTSHT_ID 
    =gfReplace("LDEFCSTSHT with .F.")
  ENDIF
  lcDesc = ''
  IF SEEK('0001'+PADR(SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen),19)+"I"+lcCostSheet)
    =gfReplace("LDEFCSTSHT with .T.")
    lcDesc = BOMHEADR_UP.CCSTSHTDSC
  ENDIF
  SELECT BOMHEADR_UP
  =gfTableUpdate()
  REPLACE CComCstSht WITH lcCostSheet,;
          CcComCstDesc WITH  lcDesc  FOR SUBSTR(cStyComp,1,loFormSet.lnMajorlen) = SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen) IN (lcTempFile)
          
  REPLACE CCSTSHT_ID WITH lcCostSheet FOR SUBSTR(Style,1,loFormSet.lnMajorlen)  = SUBSTR(lcStyleCode,1,loFormSet.lnMajorlen) IN (lcTempFile)        
  IF BETWEEN(lnRecNoT,1,RECCOUNT(lcTempFile))
    GO RECORD lnRecNoT IN  (lcTempFile)
  ENDIF     
  
ENDIF  
SELECT(lnSelectAls)
*!*************************************************************
*! Name      : lfGridRefresh
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Grid refresh
*!*************************************************************
FUNCTION lfGridRefresh
LPARAMETERS loFormSet 
*B611142,1 MMT 04/21/2016 Issue#4 Changing vendor does not update style from PO Auto create[P20160119.0001][Start]
IF TYPE('loFormSet.AriaForm1') <> 'O'
  RETURN
ENDIF
*B611142,1 MMT 04/21/2016 Issue#4 Changing vendor does not update style from PO Auto create[P20160119.0001][End]
WITH  loFormSet.AriaForm1.grdPOLines
  .ReadOnly = .T.
  .Column1.ReadOnly = .F.
  .Column2.ReadOnly = .F.
  .Column3.ReadOnly  = .T.
  .Column4.ReadOnly = .T.
  .Column5.ReadOnly = .F.
  .Column6.ReadOnly = .F.
  .Column7.ReadOnly = .F.
  .Column8.ReadOnly = .F.
  .Column9.ReadOnly = .F.
  .Column10.ReadOnly = .F.
  .Column11.ReadOnly = .F.
  .Column12.ReadOnly = .F.
  .Column14.ReadOnly = .F.
  .Column17.ReadOnly = .F.
  .Column18.ReadOnly = .F.
  .Column19.ReadOnly = .T.
  .Column21.ReadOnly = .T.
  .Column26.ReadOnly = .F.
  .Column20.ReadOnly = .F.
  .Column22.Readonly = .T.
  .Column23.Readonly = .T.
  .Column24.Readonly = .T.
  .Column25.Readonly = .T.      
  .Column27.Readonly = .T.
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
  .Column28.ReadOnly = .F.
  .Column29.ReadOnly = .F.
  *C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
ENDWITH
*!*************************************************************
*! Name      : lfGetCompInfo
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Get Component info.
*!*************************************************************
FUNCTION lfGetCompInfo
LPARAMETERS loFormSet,lcStyle,lcCostSheetID
lcCostSheetID = PADR(lcCostSheetID  ,6)
STORE  0 TO lnClrLen ,lnClrPos
DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
lnClrLen = 0
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR
lnSclLen=19-(loFormSet.lnMajorlen+lnClrLen+1)  
lnMajorlen =loFormSet.lnMajorlen
m.CStyleDesc =''
m.CColorDesc =''
m.CCompDesc =''
m.CComClrDesc =''
m.CComCstSht  = ''
m.CcComCstDesc =''
m.cStyComp = ''
*B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
*m.cCmpVend = ''
m.cVendCode = ''
*B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][End]  
m.cCmpVenPh = ''
lcSClrPart = SUBSTR(lcStyle,loFormSet.lnMajorlen+2,lnClrLen)
lcSSclPart = SUBSTR(lcStyle,loFormSet.lnMajorlen+lnClrLen+2,lnSclLen)  
IF gfSeek('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorlen),19)+'I'+lcCostSheetID+'2','Bom_V','MULTIBOM')
    SELECT Bom_V
    LOCATE 
    lcPriItm =''
    IF !EOF()
      LOCATE FOR ccatgtyp ='S' AND LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) 
      IF FOUND()
        =ALINES(laLines,BOM_V.MSZCROSREF)
        FOR lnI = 1 TO ALEN(laLines,1)
          laLines[lnI] = LEFT(laLines[lnI],3)
        ENDFOR      
        IF ASCAN(laLines,lcSSclPart) = 0
          LOOP
        ENDIF
        DIMENSION laSzCrsRef[8]
        laSzCrsRef = 0
        lcPItm = Item
        lcMClrPart = SUBSTR(citmmask,lnMajorlen+2,lnClrLen)
        lcMSclPart = SUBSTR(citmmask,lnMajorlen+lnClrLen+2,lnSclLen)            
        llSAlStrs  = '*' $ lcMSclPart AND '*' $ lcMClrPart      
        lcIClrPart = SUBSTR(lcPItm,lnMajorlen+2,lnClrLen)
        lcISclPart = SUBSTR(lcPItm,lnMajorlen+lnClrLen+2,lnSclLen) 
        lcMajItm   = SUBSTR(lcPItm,1,lnMajorlen)
        llIAlStrs  = '*' $ lcISclPart AND '*' $ lcIClrPart
        lcItemScale = ''
        IF !EMPTY(mszcrosref)
          lnScalePos = ATC(lcSSclPart,mszcrosref)
          IF lnScalePos > 0 
            lcScalStr = SUBSTR(mszcrosref,lnScalePos +LEN(lcSSclPart)-1)
            lnItemScalePOs = ATC('~',lcScalStr )
            IF lnItemScalePOs> 0 
              lcScalStr = SUBSTR(lcScalStr ,lnItemScalePOs+1)
              lnItemScaleEnd = ATC(',',lcScalStr )
              lcItemScale = SUBSTR(lcScalStr ,1,lnItemScaleEnd-1)
            ENDIF
          ENDIF
        ENDIF
        IF !EMPTY(lcItemScale) AND gfSEEK('S'+lcItemScale ,'Scale_V','Scale')
          lcScaleStyle =lcSSclPart 
          lcSSclPart =lcItemScale 
          lcCrosRefFld =mszcrosref
          DIMENSION laSizesArr[1]
          laSizesArr = ''
          =gfSubStr(lcCrosRefFld ,@laSizesArr ,CHR(13))
          FOR lnS = 1 TO 8
            LNPOSINARR = 0
            for lnW = 1 to alen(laSizesArr,1)
              if ALLTRIM(lcItemScale)+','+STR(lnS,1) $ laSizesArr[lnW]
                LNPOSINARR  = lnW
                exit
              ENDif
            ENDFOR
            IF LNPOSINARR >0
              lnChlScl = ATC('~',laSizesArr[LNPOSINARR])
              IF lnChlScl > 0
                lcSclPosStr = SUBSTR(laSizesArr[LNPOSINARR],1,lnChlScl -1)
                lnCommPos = ATC(',',lcSclPosStr )
                laSzCrsRef [lnS] = VAL(SUBSTR(lcSclPosStr ,lnCommPos +1))
              ENDIF  
            ENDIF
          ENDFOR
        ENDIF
        IF llIAlStrs AND llSAlStrs
          lcPriItm = lcMajItm + "-" + lcSClrPart + lcSSclPart
        ELSE 
          IF llSAlStrs AND '*' $ lcIClrPart AND !'*' $ lcISclPart 
            lcPriItm = lcMajItm + "-" + lcSSclPart + lcISclPart
          ELSE 
            IF llSAlStrs AND !'*' $ lcIClrPart AND '*' $ lcISclPart 
              lcPriItm = lcMajItm + "-" + lcIClrPart + lcSSclPart
            ELSE 
              lcPriItm = lcPItm
            ENDIF 
          ENDIF  
        ENDIF 
        IF '*' $ lcPriItm 
           lcTempClrPart = SUBSTR(lcPriItm ,lnMajorlen+2,lnClrLen)
           lcTempSclPart = SUBSTR(lcPriItm ,lnMajorlen+lnClrLen+2,lnSclLen) 
           IF  '*' $ lcTempClrPart      
             lcPriItm = lcMajItm + "-" + lcSClrPart + lcTempSclPart 
           ELSE 
             lcPriItm = lcMajItm + "-" + lcTempClrPart + lcSSclPart
           ENDIF
        ENDIF
      ENDIF   
    ENDIF
    IF !EMPTY(lcPriItm)
      m.cStyComp = lcPriItm
    
      SELECT BOMHEADR_V
      lcCompCostSheet = ''
      IF gfSeek('0001'+PADR(SUBSTR(lcPriItm,1,loFormSet.lnMajorlen),19)+"I")
        LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID = '0001'+PADR(SUBSTR(lcPriItm,1,loFormSet.lnMajorlen),19)+"I" FOR LDEFCSTSHT                    
        IF FOUND()
           m.CComCstSht = BOMHEADR_V.CCSTSHT_ID 
           m.CcComCstDesc = BOMHEADR_V.CCSTSHTDSC                    
        ENDIF
      ENDIF
    
      =gfSeek(lcPriItm,'Style_V','Style')
      *B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][Start]
      *m.CCompDesc = Style_V.Desc
      m.CCompDesc = Style_V.Desc1
      *B611125,1 MMT 03/14/2016 Issue#1 - Error while closing screen[P20160119.0001-Issue#1][End]
      m.CComClrDesc =gfCodDes(SUBSTR(lcPriItm,lnMajorlen+2,lnClrLen),'COLOR     ') 
      *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
      *m.cCmpVend = Style_V.vendor      
      m.cVendCode = Style_V.vendor
      *B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][End]  
      =GFSEEK(Style_V.vendor,'apvendor_V','VENCODE')
      m.cCmpVenPh = apvendor_V.cphoneno
   ENDIF    
ENDIF     
*B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][Start]  
*!*	REPLACE CComCstSht  WITH   m.CComCstSht  ,;
*!*	        CcComCstDesc WITH m.CcComCstDesc ,;
*!*	        cStyComp  WITH m.cStyComp ,;
*!*	        cCmpVend  WITH m.cCmpVend ,;
*!*	        cCmpVenPh  WITH m.cCmpVenPh,;        
*!*	        CCompDesc with m.CCompDesc ,;
*!*	        CComClrDesc   with m.CComClrDesc  IN (loFormSet.lcTempData)
REPLACE CComCstSht  WITH   m.CComCstSht  ,;
        CcComCstDesc WITH m.CcComCstDesc ,;
        cStyComp  WITH m.cStyComp ,;
        cVendCode WITH m.cVendCode ,;
        cCmpVenPh  WITH m.cCmpVenPh,;        
        CCompDesc with m.CCompDesc ,;
        CComClrDesc   with m.CComClrDesc  IN (loFormSet.lcTempData)
*B611125,1 MMT 03/14/2016 Issue#1 - Error while validating style cost sheet[P20160119.0001-Issue#1][End]          
*C201789,1 MMT 03/07/2016 Point#6 - Allow user to edit Style Pattern and cost sheet and update style Master[P20160119.0001-Point#6][End]

*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][Start]
*!*************************************************************
*! Name      : lfGetVendorsData
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/07/2016
*! Purpose   : Get Vendor info.
*!*************************************************************
FUNCTION lfGetVendorsData
PARAMETERS loFormSet
*B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start]
*lnFilesFound = ADIR(laArrOfFiles, ADDBS(oAriaApplication.RESOURCEHOME)+"*PO.csv" )
lnFilesFound = ADIR(laArrOfFiles, ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+"*PO.csv" )
*B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
IF lnFilesFound > 0

  IF !USED('syuuser')
  llT  =gfOpenTable("syuuser",'CUSER_ID','SH')
  ENDIF

  *!* Validate  File content
  DIMENSION laNewArr[1,2]
  IF !lfValidArr(@laArrOfFiles , lnFilesFound ,@laNewArr)
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'There are no files to be processed - please create one from the Style Summary report and then try again')     
    RETURN .F.
  ENDIF
  *!* Create temp Tables
  IF TYPE('loFormSet.lcHeadItemTmp') = 'U'
    loFormSet.Addproperty('lcHeadItemTmp','')
  ENDIF
  IF TYPE('loFormSet.lctmpvend') = 'U'
    loFormSet.Addproperty('lctmpvend','')
  ENDIF
  IF TYPE('loFormSet.lcDetItemTmp') = 'U'
    loFormSet.Addproperty('lcDetItemTmp','')
  ENDIF

  loFormSet.lcHeadItemTmp = gfTempName()
  loFormSet.lcDetItemTmp = gfTempName()
  loFormSet.lctmpvend = gfTempName()
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *CREATE CURSOR (loFormSet.lcDetItemTmp) (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14), lcFileName C(20))  
  CREATE CURSOR (loFormSet.lcDetItemTmp) (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14), lcFileName C(20),Pattern C(10))
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  SELECT (loFormSet.lcDetItemTmp) 
  INDEX On lcFileName+Style+Vendor TAG (loFormSet.lcDetItemTmp) 

  CREATE CURSOR (loFormSet.lcHeadItemTmp) (lcUser C(15),lcFile C(20) ,lSelected L)
  SELECT (loFormSet.lcHeadItemTmp) 
  INDEX On lSelected TAG 'selctd' ADDITIVE 
  INDEX On lcUser TAG (loFormSet.lcHeadItemTmp) ADDITIVE 
  SET DELETED ON
  *!* Fill Temp Data
  lnRecno = 1
  lnI=1

  APPEND FROM ARRAY laNewArr 
  GO TOP
  SELECT (loFormSet.lcDetItemTmp)
  SET DELETED ON
  DO WHILE lnI<=ALEN(laNewArr,1)
   *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start] 
   *Append from oAriaApplication.RESOURCEHOME+laNewArr[lnI,2] TYPE delimited
   Append from ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID)) +laNewArr[lnI,2] TYPE delimited
   *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
   DELETE FOR EMPTY(ALLTRIM(STYLE))
   IF BETWEEN(lnRecno,1,RECCOUNT())
     GOto lnRecno
   ENDIF
   Replace lcFileName WITH ALLTRIM(laNewArr[lnI,2]) FOR EMPTY(ALLTRIM(lcFileName))
   lnRecno = RECNO()+1
   lnI=lnI+1
  ENDDO
  
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  IF !USED('STYLE_Pattern')
    =gfOpenTable('STYLE','STYLE','SH','STYLE_Pattern')
  ENDIF
  SELECT (loFormSet.lcDetItemTmp)
  LOCATE
  SCAN 
    =gfSeek(EVALUATE(loFormSet.lcDetItemTmp+'.Style'),'STYLE_Pattern','STYLE')
    REPLACE Pattern WITH STYLE_Pattern.Pattern
  ENDSCAN
  LOCATE
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  
  lcExpr ="lcFileName ='"+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile'))+"' AND !EMPTY(STYLE)"
  SET FILTER TO &lcExpr. 
  GO TOP
  
ELSE
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'There are no files to be processed - please create one from the Style Summary report and then try again')     
  RETURN .F.
ENDIF
IF !USED('apvendor')
llT  =gfOpenTable("apvendor",'VENCODE','SH')
ENDIF
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
*CREATE CURSOR (loFormSet.lctmpvend) (lcUser C(15), CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2))
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*CREATE CURSOR (loFormSet.lctmpvend) (lcUser C(15), CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2),nStyCount N(5))
CREATE CURSOR (loFormSet.lctmpvend) (lcUser C(15), CVENDCODE C(8),cvencomp C(30),POQty N(12),POValue N(14,2),SOValue N(14,2),nStyCount N(5),Pattern C(10))
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
SELECT (loFormSet.lctmpvend) 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
*INDEX On lcUser TAG (loFormSet.lctmpvend) 
INDEX On lcUser+CVENDCODE+Pattern   TAG (loFormSet.lctmpvend) 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]



SELECT (loFormSet.lcHeadItemTmp)
GO TOP 
SCAN FOR !EMPTY(lcUser)
  SELECT (loFormSet.lcDetItemTmp)
  SET FILTER TO
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *SELECT distinct Vendor FROM (loFormSet.lcDetItemTmp) WHERE lcFileName =ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')) INTO ARRAY laVend NOFILTER 
  SELECT distinct Vendor,pattern FROM (loFormSet.lcDetItemTmp) WHERE lcFileName =ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')) INTO ARRAY laVend NOFILTER 
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  IF _tally = 0
    LOOP
  ENDIF
  
  lnCnt=1
  SELECT(loFormSet.lctmpvend)
  DO WHILE  lnCnt <= ALEN(laVend,1)
    IF SEEK(ALLTRIM(laVend[lnCnt,1]),'apvendor')
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
      IF !SEEK(PADR(ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcUser')),15)+PADR(ALLTRIM(laVend[lnCnt,1]),8)+laVend[lnCnt,2],loFormSet.lctmpvend,loFormSet.lctmpvend)
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
        APPEND BLANK
        Replace lcUser WITH ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcUser')) ,;
                CVENDCODE WITH ALLTRIM(laVend[lnCnt,1]),;
                 cvencomp WITH apvendor.cvencomp  
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
        REPLACE pattern WITH laVend[lnCnt,2]
      ENDIF
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]    
      *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][Start] 
      *lfGetVendorQtyVal(ALLTRIM(laVend[lnCnt,1]),loFormSet.lctmpvend,oAriaApplication.RESOURCEHOME+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')))
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
      *lfGetVendorQtyVal(ALLTRIM(laVend[lnCnt,1]),loFormSet.lctmpvend,ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')))
      lfGetVendorQtyVal(ALLTRIM(laVend[lnCnt,1]),laVend[lnCnt,2],loFormSet.lctmpvend,ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(EVALUATE(loFormSet.lcHeadItemTmp+'.lcFile')))
      *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
      *B611138,1 MMT 04/11/2016 Issue#4:Fix issues in Custom Auto create PO program for DCC[P20160119.0001][End]
    ENDIF
    lnCnt= lnCnt+1
  ENDDO
ENDSCAN 

*!* Dont forget to fill the vendor name in the dbf
SELECT (loFormSet.lctmpvend) 
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][Start]
*lcBrowFlds = [lcUser:H = 'User ID'  :15,CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Vendor Name'  :35 ,POQty:H = 'Purchase Qty'  :10,POValue:H = 'Purchase Value'  :15,SOValue:H = 'Sales Value'  :15]
lcBrowFlds = [lcUser:H = 'User ID'  :15,CVENDCODE:H = 'Vendor'  :10, cvencomp  :H = 'Vendor Name'  :35 ,POQty:H = 'Purchase Qty'  :10,]+;
              [POValue:H = 'Purchase Value'  :15,nStyCount:H='Styles to Purchase':15,SOValue:H = 'Sales Value'  :15]
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
lcBrowFlds = lcBrowFlds + [,Pattern:H = 'Pattern'  :20] 
*C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]              
*C201828,1 MMT 05/31/2016 P20160510.0001 - P20160510.0001 - Issue#4 - Point#10 - display style count[P20160510.0001][End]
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
*llContinue = gfBrowse(lcBrowFlds,"Vendors",.F.,'',.F.,.F.,.T.,.F.,.F.,.F.,;
    .F.,"CVENDCODE",.F.,.F.,.F.,.F.,.F.,.F.,"")
  SET DELETED ON    
  lcTempVend = gfTempName()   && Name of file that hold temporary Account data.    
  llContinue = gfBrowse(lcBrowFlds,"Vendors",loFormSet.lctmpvend ,'',.F.,.F.,.T.,.F.,'lfvSelUser()',.F.,;
      lcTempVend ,"lcUser+CVENDCODE+Pattern",.F.,.F.,.F.,.F.,.F.,.F.,loFormSet.lctmpvend)      
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
IF llContinue
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][Start]
  *lfInitForm(loFormSet,ALLTRIM(lcUser) , ALLTRIM(CVENDCODE))
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
  *lfInitForm(loFormSet,ALLTRIM(lcUser) , ALLTRIM(CVENDCODE),Pattern)
  lcTempVend2 = gfTempName()   
  SELECT (lcTempVend)
  COPY TO (oAriaApplication.workDir+lcTempVend2+".DBF") WITH cdx
  IF USED(lcTempVend2)
    USE IN (lcTempVend2)
  ENDIF
  lfInitForm(loFormSet,lcTempVend2)
  *C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
  *C201830,1 MMT 05/31/2016 P20160510.0001 - Issue#4 - Point#11 - Show Styles pattern[P20160510.0001][End]
  lfAddGridSource(loFormSet)
  loFormSet.changemode ('E')
ELSE

  IF USED(loFormSet.lcHeadItemTmp)
    USE IN loFormSet.lcHeadItemTmp
  ENDIF 

  IF USED(loFormSet.lcDetItemTmp)
    USE IN loFormSet.lcDetItemTmp
  ENDIF


  IF USED(loFormSet.lctmpvend)
    USE IN loFormSet.lctmpvend
  ENDIF 
ENDIF 
*C201792,1 MMT 03/14/2016 Entity#8 - Exchange notes buttons in Custom create PO screen for DCC[P20160119.0001][End]

*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][Start]
*!*************************************************************
*! Name      : lfUpdCompVendor
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/30/2016 
*! Purpose   : Update Supplier Item of the Component
*!*************************************************************
FUNCTION lfUpdCompVendor
LPARAMETERS loFormSet
lnSelectAls = SELECT() 
lcTempFile = loFormSet.lcTempData
llUpdate = .F.
lcStyMaj = SUBSTR(&lcTempFile..cStyComp ,1,loFormSet.lnMajorlen)
IF  gfModalgen("QRM00000B00006","ALERT",.F.,.F.,'Do you want to update the Vendor item of Style '+ALLTRIM(lcStyMaj)+;
      ' to be '+ALLTRIM(&lcTempFile..CCOMPVEN) +' ?') =1
  lnRecNoT = RECNO(lcTempFile)
  lcCCompVen = &lcTempFile..CCOMPVEN
  IF !USED('Style_VNIT')
    =gfOpenTable('STYLE','STYLE','SH','Style_VNIT')
  ENDIF
  lnRecNoT = RECNO(lcTempFile)
  SELECT 'Style_VNIT'
  =gfSeek(lcStyMaj)
  SCAN rest WHILE Style = lcStyMaj
    =gfReplace("CVENSTY WITH '"+lcCCompVen +"'")
  ENDSCAN  
  =gfTableUpdate()     
  REPLACE CCOMPVEN WITH lcCCompVen ALL FOR cStyComp = lcStyMaj IN (lcTempFile)
  IF BETWEEN(lnRecNoT,1,RECCOUNT(lcTempFile))
    GO RECORD lnRecNoT IN  (lcTempFile)
  ENDIF     
  llUpdate = .T.
ENDIF
SELECT(lnSelectAls)
RETURN llUpdate
*!*************************************************************
*! Name      : lfUpdStyleVendor
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/30/2016 
*! Purpose   : Update Supplier Item of the style
*!*************************************************************
FUNCTION lfUpdStyleVendor
LPARAMETERS loFormSet
lnSelectAls = SELECT() 
lcTempFile = loFormSet.lcTempData
llUpdate = .F.
lcStyMaj = SUBSTR(&lcTempFile..STYLE ,1,loFormSet.lnMajorlen)
IF  gfModalgen("QRM00000B00006","ALERT",.F.,.F.,'Do you want to update the Vendor item of Style '+ALLTRIM(lcStyMaj)+;
      ' to be '+ALLTRIM(&lcTempFile..CVENSTY) +' ?') =1
  lnRecNoT = RECNO(lcTempFile)
  lcCCompVen = &lcTempFile..CVENSTY 
  IF !USED('Style_VNIT')
    =gfOpenTable('STYLE','STYLE','SH','Style_VNIT')
  ENDIF
  lnRecNoT = RECNO(lcTempFile)
  SELECT 'Style_VNIT'
  =gfSeek(lcStyMaj)
  SCAN REST WHILE Style = lcStyMaj
    =gfReplace("CVENSTY WITH '"+lcCCompVen +"'")
  ENDSCAN  
  =gfTableUpdate()     
  REPLACE CVENSTY WITH lcCCompVen ALL FOR STYLE = lcStyMaj IN (lcTempFile)
  IF BETWEEN(lnRecNoT,1,RECCOUNT(lcTempFile))
    GO RECORD lnRecNoT IN  (lcTempFile)
  ENDIF     
  llUpdate = .T.
ENDIF
SELECT(lnSelectAls)
RETURN llUpdate
*C201824,1 MMT 05/30/2016 P20160510.0001 - Issue#5 changes in Auto create PO program[P20160510.0001][End]
*C201825,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#5 Add Selection buttons[P20160510.0001][Start]
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/30/2016 
*! Purpose   : Select Button Click
*!*************************************************************
FUNCTION lfvSelect
LPARAMETERS loFormSet
lcTempFile = loFormSet.lcTempData
lnRecSel = RECNO(lcTempFile)
REPLACE lSelected  WITH !lSelected IN (lcTempFile)   
SELECT (lcTempFile)   
LOCATE FOR lSelected  
IF FOUND()
  loFormSet.AriaForm1.cmdSelNone.Enabled = .T.
ELSE
  loFormSet.AriaForm1.cmdSelNone.Enabled = .F.
ENDIF
LOCATE FOR !lSelected  
IF FOUND()
  loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
ELSE
  loFormSet.AriaForm1.cmdSelAll.Enabled = .F.
ENDIF

IF BETWEEN(lnRecSel,1,RECCOUNT(lcTempFile))
  GO RECORD lnRecSel IN (lcTempFile)   
ENDIF
lfvpbSel(loFormSet)
*!*************************************************************
*! Name      : lfvSelectAll
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/30/2016 
*! Purpose   : Select All Button Click
*!*************************************************************
FUNCTION lfvSelectAll
LPARAMETERS loFormSet
lcTempFile = loFormSet.lcTempData
lnRecSel = RECNO(lcTempFile)
REPLACE ALL lSelected  WITH .T. IN (lcTempFile)   
IF BETWEEN(lnRecSel,1,RECCOUNT(lcTempFile))
  GO RECORD lnRecSel IN (lcTempFile)   
ENDIF
loFormSet.AriaForm1.cmdSelNone.Enabled = .T.
loFormSet.AriaForm1.cmdSelAll.Enabled = .F.

lfvpbSel(loFormSet)
*!*************************************************************
*! Name      : lfvSelectNone
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/30/2016 
*! Purpose   : Select None Button Click
*!*************************************************************
FUNCTION lfvSelectNone
LPARAMETERS loFormSet
lcTempFile = loFormSet.lcTempData
lnRecSel = RECNO(lcTempFile)
REPLACE ALL lSelected  WITH .F. IN (lcTempFile)   
IF BETWEEN(lnRecSel,1,RECCOUNT(lcTempFile))
  GO RECORD lnRecSel IN (lcTempFile)   
ENDIF
loFormSet.AriaForm1.cmdSelNone.Enabled = .F.
loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
lfvpbSel(loFormSet)
*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/30/2016 
*! Purpose   : Invert Button Click
*!*************************************************************
FUNCTION lfvInvert
LPARAMETERS loFormSet
lcTempFile = loFormSet.lcTempData
lnRecSel = RECNO(lcTempFile)
REPLACE ALL lSelected  WITH !lSelected IN (lcTempFile)   
LOCATE FOR lSelected  
IF FOUND()
  loFormSet.AriaForm1.cmdSelNone.Enabled = .T.
ELSE
  loFormSet.AriaForm1.cmdSelNone.Enabled = .F.
ENDIF
LOCATE FOR !lSelected  
IF FOUND()
  loFormSet.AriaForm1.cmdSelAll.Enabled = .T.
ELSE
  loFormSet.AriaForm1.cmdSelAll.Enabled = .F.
ENDIF
IF BETWEEN(lnRecSel,1,RECCOUNT(lcTempFile))
  GO RECORD lnRecSel IN (lcTempFile)   
ENDIF
lfvpbSel(loFormSet)
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/30/2016 
*! Purpose   : Refresh Select Button Caption
*!*************************************************************
FUNCTION lfvpbSel
LPARAMETERS loFormSet
lcTempFile = loFormSet.lcTempData
IF &lcTempFile..lSelected  
  loFormSet.AriaForm1.cmdSelect.Caption = 'UnSelect'
ELSE
  loFormSet.AriaForm1.cmdSelect.Caption = 'Select'
ENDIF
*C201825,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#5 Add Selection buttons[P20160510.0001][End]
*C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][Start]
*!*************************************************************
*! Name      : lfForecasting
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/16/2016 
*! Purpose   : Call Forecasting screen
*!*************************************************************
FUNCTION lfForecasting
LPARAMETERS loFormSet
lcTempFile = loFormSet.lcTempData
IF USED('TmpForc')
  USE IN 'TmpForc'
ENDIF
IF !FILE(ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv")
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'There is Forecasting information available for this style')     
  RETURN .F.
ENDIF
CREATE CURSOR 'TmpForc'(Style C(19),Month N(2),YEAR N(4), cMonth C(15),cYear C(4),Qty1 N(12), Qty2 N(12), Qty3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  Qty7 N(12), Qty8 N(12), TOTQty N(14))
SELECT 'TmpForc'
INDEX On Style  + STR(Year,4) + STR(Month,2) TAG 'TmpForc'
APPEND FROM (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv") TYPE CSV
IF !SEEK(&lcTempFile..Style,'TmpForc')
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'There is Forecasting information available for this style')     
  RETURN .F.
ENDIF
lcSelectedSty = &lcTempFile..Style
SELECT 'TmpForc'
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
=SEEK(lcSelectedSty)
SUM Qty1,Qty2,QTy3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,QT REST WHILE Style  + STR(Year,4) + STR(Month,2) =lcSelectedSty
APPEND BLANK 
REPLACE Style WITH lcSelectedSty,;
        Month WITH 99,;
        Year WITH 9999,;
        cMonth WITH 'Total',;
        cYear With '',;
        Qty1 With Q1,;
        Qty2 With Q2,;
        Qty3 With Q3,;
        Qty4 With Q4,;
        Qty5 With Q5,;
        Qty6 With Q6,;
        Qty7 With Q7,;
        Qty8 With Q8,;
        TotQty With QT
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]
SET KEY TO (lcSelectedSty)
LOCATE
DO FORM (oAriaApplication.ScreenHome+"\po\POcrtFrs.SCX")
*!*************************************************************
*! Name      : lfInitForecst
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/16/2016 
*! Purpose   : Init of Forecasting screen
*!*************************************************************
FUNCTION lfInitForecst
LPARAMETERS loBranchFormSet
IF !USED("Style_Forc")
  =gfOpenTable('Style','Style','SH',"Style_Forc")
ENDIF

IF !USED("Scale_Forc")
  =gfOpenTable('Scale','Scale','SH',"Scale_Forc")
ENDIF
=gfSeek(lcSelectedSty,"Style_Forc",'STYLE')
=gfSeek('S'+Style_Forc.Scale,"Scale_Forc",'SCALE')

loBranchFormSet.AriaForm1.Caption = ALLTRIM(lcSelectedSty) + '  Forecasting'
WITH loBranchFormSet.AriaForm1.grdForeCast
  .RecordSource =''
  .RecordSource =  'TmpForc'
  .Column1.ControlSource =  "TmpForc.cMonth+' - '+TmpForc.cYear"
  .Column2.ControlSource =  'TmpForc.Qty1'
  .Column2.Header1.Caption = Scale_Forc.Sz1
  .Column3.ControlSource =  'TmpForc.Qty2'
  .Column3.Header1.Caption = Scale_Forc.Sz2
  .Column4.ControlSource =  'TmpForc.Qty3'
  .Column4.Header1.Caption = Scale_Forc.Sz3
  .Column5.ControlSource =  'TmpForc.Qty4'
  .Column5.Header1.Caption = Scale_Forc.Sz4
  .Column6.ControlSource =  'TmpForc.Qty5'
  .Column6.Header1.Caption = Scale_Forc.Sz5
  .Column7.ControlSource =  'TmpForc.Qty6'
  .Column7.Header1.Caption = Scale_Forc.Sz6
  .Column8.ControlSource =  'TmpForc.Qty7'
  .Column8.Header1.Caption = Scale_Forc.Sz7
  .Column9.ControlSource =  'TmpForc.Qty8'
  .Column9.Header1.Caption = Scale_Forc.Sz8
  .Column10.ControlSource = 'TmpForc.TOTQty'
  .Readonly = .T.
ENDWITh  
*C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][End]
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][Start]
*!*************************************************************
*! Name      : lfvSelUser
*! Developer : Mariam Mazhar(MMT)
*! Date      : 06/20/2016 
*! Purpose   : Validated selected record user
*!*************************************************************
FUNCTION lfvSelUser
lcCurrAls = ALIAS()
lcCurUser = &lcCurrAls..lcUser
SELECT (GLOBALBROWSEWINDOW.lcMultiSelectFile)
lnCurrSelRec = RECNO()
LOCATE 
IF !EOF()
  LOCATE FOR SUBSTR(KeyExp,1,15) <> lcCurUser 
  IF FOUND()
    =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'You cannot select more than one user')      
    SELECT (GLOBALBROWSEWINDOW.lcMultiSelectFile)
    GO RECORD lnCurrSelRec 
    GLOBALBROWSEWINDOW.AddSelect()
    SELECT(lcCurrAls)
    RETURN .F.
  ENDIF
ENDIF  
SELECT(lcCurrAls)
*C201835,1 MMT 06/20/2016 Allow user to select multiple lines from Auto create PO browser[P20160510.0001 - Issue#1][End]