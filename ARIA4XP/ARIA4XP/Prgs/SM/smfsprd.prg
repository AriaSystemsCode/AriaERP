PARAMETERS loFormSet


lcScx = lfGetScx('SM\SMFSPRD.scx')
DO FORM (lcScx) WITH loFormSet



************************************************************
*! Name      : lfSMFSPRDInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/14/2013
*! Purpose   : SMFSPRD init 
************************************************************
FUNCTION lfSMFSPRDInit
PARAMETERS loBranFormSet
loFormSet = loBranFormSet.loFormSet 

llCloseYear = IIF(TYPE('llCloseYear')='U',.F.,llCloseYear)
loBranFormSet.AddProperty('llCloseYear',llCloseYear)

*loBranFormSet.AriaForm1.txtName.Value = IIF(llCloseYear,SUBSTR(lcCompany,4,LEN(lcCompany)),lcComp)
lcCompany = loFormSet.Ariaform1.puComp.Value
WITH loBranFormSet.AriaForm1
  .txtName.Value = SUBSTR(lcCompany,4,LEN(lcCompany))
  .txtFiscalyear.Value = IIF(llCloseYear,lcFiscal,loFormSet.laData[2])
  .cmdOk.Enabled = loFormSet.ActiveMode $ 'AE'
  .Caption = 'Periods'
ENDWITH   

*- set the grid
=lfSetGridDataSource(loBranFormSet)

RETURN 
*- End of lfSMFSPRDInit.


************************************************************
*! Name      : lfSetGridDataSource
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 22/05/2012
*! Purpose   : Set the Grid Data Source
************************************************************
FUNCTION lfSetGridDataSource
PARAMETERS loBRANFormSet  
loFormSet = loBRANFormSet.loFormSet
oGrd = loBRANFormSet.Ariaform1.grdSMFSPRD
WITH oGrd
  .RecordSource = ''
  .RecordSource = loFormSet.lc_TempPR
ENDWITH   

SELECT (loFormSet.lc_TempPR)
CURSORSETPROP("Buffering",5)
LOCATE 

lc_TempPR = loFormSet.lc_TempPR
lfSetColumnsProp('1',"&lc_TempPR..CFSPPRDID",'Prd'          ,30 ,oGrd)
lfSetColumnsProp('2',"&lc_TempPR..CFSPPDESC"  ,"Description"  ,150,oGrd)
lfSetColumnsProp('3',"&lc_TempPR..DFSPPBGDT","Begin Date"   ,90 ,oGrd) 
lfSetColumnsProp('4',"&lc_TempPR..DFSPPENDT","End Date"     ,90 ,oGrd) 
lfSetColumnsProp('5',"&lc_TempPR..DFSPPENDT - &lc_TempPR..DFSPPBGDT + 1",'Days'         ,40 ,oGrd)
lfSetColumnsProp('6',"&lc_TempPR..NFSPPARTN"  ,"Part"         ,50 ,oGrd)
lfSetColumnsProp('7',"&lc_TempPR..LFSPLOCKS","Locked"       ,50 ,oGrd) 
lfSetColumnsProp('8',"&lc_TempPR..LFSPCLSDS","Closed"       ,50 ,oGrd) 
*- End of lfSetGridDataSource.

************************************************************
*! Name      : lfSetColumnsProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 05/23/2012
*! Purpose   : Set Columns Properties
************************************************************
FUNCTION lfSetColumnsProp
PARAMETERS lcCol,lcSrc,lcHeader,lnWidth,loGrd
lnWidth = IIF(EMPTY(lnWidth),50,lnWidth)
WITH loGrd
  .Column&lcCol..Header1.Caption = lcHeader
  .Column&lcCol..ControlSource   = lcSrc
  .Column&lcCol..Width           = lnWidth
ENDWITH 
*- End of lfSetColumnsProp.
************************************************************
*! Name      : lfUpcStatus
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 02/24/2013
*! Purpose   : update the cStatus field
************************************************************
FUNCTION lfUpcStatus
PARAMETERS loFld
IF loFld.Value <> loFld.OldValue
  REPLACE cStatus WITH 'M'
ENDIF 

*- End of lfUpcStatus.

*!**************************************************************************
*!
*!      Function: lfvDateRng
*!
*!**************************************************************************
* Function to validate all end date fields
*
FUNCTION lfvDateRng
PARAMETERS loBranFormSet,loFld
loFormSet = loBranFormSet.loFormSet

lc_TempPR = loFormSet.lc_TempPR
lnPeriod = VAL(&lc_TempPR..CFSPPRDID)

  DO CASE 
    *** User is not permited to skip end date without adding
    CASE EMPTY(loFld.Value)
      *** Periods End Date must be enterd sequentialy..!
      =gfModalGen("TRM00078B00000","DIALOG",STR(lnPeriod))
      loFld.Value = loFld.OldValue
      RETURN .F.

    *** Periods days not less than one day
    CASE loFld.Value < loFld.parent.parent.Column3.Text1.Value
      *** End date of period ð can not ***
      *** be less than begin date ..!
      *** <  Ok  > ***
      =gfModalGen("TRM00072B00000","DIALOG",ALLTRIM(STR(lnPeriod)))
      loFld.Value = loFld.OldValue
      RETURN .F.

    *** End Date has to be within the fiscal year. ***
    CASE loFld.Value >= IIF(loBranFormSet.llCloseYear,ldFisEnd,loFormSet.laData[7])
      *** End date of period ð can not be grater ***
      *** than end date of the year ..!
      *** <  Ok  > ***
      =gfModalGen("TRM00073B00000","DIALOG",STR(lnPeriod))
      loFld.Value = loFld.OldValue
      RETURN .F.

    *** Begin Date of next period is one day affter the end date
    OTHERWISE
      IF loBranFormSet.llCloseYear
        laPriod[lnPeriod+1,5] = MIN(EVALUATE(SYS(18))+1,ldFisEnd)
      ELSE
        *laPriod[lnPeriod+1,5] = MIN(EVALUATE(SYS(18))+1,laData[7])
        ldDate = loFld.Value
        IF lnPeriod<12
          SKIP
          replace DFSPPBGDT WITH ldDate + 1
          SKIP -1
        ENDIF 
      ENDIF
  ENDCASE


IF loFld.Value <> loFld.OldValue
  REPLACE cStatus WITH 'M'
ENDIF   


*!**************************************************************************
*!
*!      Function: lfvPartRng
*!
*!**************************************************************************
* This function is called from all the part fields
* Parts has to be from 1 to 6
FUNCTION lfvPartRng
PARAMETERS loBranFormSet,loFld
loFormSet = loBranFormSet.loFormSet

IF loFld.Value < 1 .OR. loFld.Value > 6
  =gfModalGen("TRM00071B00000","DIALOG")
  RETURN .F.
ENDIF

IF loFld.Value <> loFld.OldValue 
  REPLACE cStatus WITH 'M'
ENDIF 

