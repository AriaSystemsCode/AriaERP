*!**************************************************************************
*! Name      : SHIMAIN.PRG
*! Developer : Ahmed Moustafa (AHS)
*! Date      : 07/28/2009
*! Purpose   : Custom triggers for Shield Overalls Ltd   
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Modifications:
*B608996,1 TMI 9/06/2009 Do not allow to apply the discount come from orders imorted from web unless it is already <> 0 [T20090624.0001 ]
*B609093,1 WAM 11/19/2009 Seek ORDHDR file with the proper index [T20091109.0004]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*:**************************************************************************
*:* Name        : lfMerDisc
*:* Developer   : AHS - AHMED MOUSTAFA
*:* Date        : 07/28/2009
*:***************************************************************************
*:* Called from : ARIINV.SCX lfGetOrder method
*:***************************************************************************
FUNCTION lfMerDisc
LOCAL lnSlct,lcFile
lnSlct = SELECT(0)

lcFile = loFormSet.lcinvhdr
SELECT (lcFile)

LOCATE 
SCAN
  *B609093,1 WAM 11/19/2009 Seek ORDHDR file with the proper index
  *=SEEK('O'+&lcFile..ORDER,'ORDHDR')
  =SEEK('O'+&lcFile..ORDER,'ORDHDR','ORDHDR')
  *B609093,1 WAM 11/19/2009 (End)

  *B608996,1 TMI [start] check that the merch amount <> 0
  IF ORDHDR.NMERAMNT<> 0
    *B608996,1 TMI [ end ] check that the merch amount <> 0
    REPLACE &lcFile..DISCOUNT WITH -ABS(ORDHDR.NMERAMNT)
    REPLACE &lcFile..DISCPCNT WITH IIF(&lcFile..SHIPAMT>0,ABS((&lcFile..DISCOUNT/&lcFile..SHIPAMT)*100),0)
    REPLACE totalchg WITH SHIPAMT+DISCOUNT + ROUND( (SHIPAMT+DISCOUNT)*TAX_RATE/100 , 2 )
    *B608996,1 TMI [start] check that the merch amount <> 0
  ENDIF
  *B608996,1 TMI [ end ] check that the merch amount <> 0
ENDSCAN 
SELECT (lnSlct)


*:**************************************************************************
*:* Name        : lfINVMRDSC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/07/2009
*:* Purpose     : Recalculate totalchg and tax_amt for SHIMAIN        
*:***************************************************************************
*:* Called from : mSumAllCharges method in InvoiceSummary class Utility.vcx
*:***************************************************************************
FUNCTION lfINVMRDSC
LOCAL lnSlct
lnSlct = SELECT()

SELECT (loFormSet.lcInvHdr)
*B608996,1 TMI [start] check that the merch amount <> 0
IF ORDHDR.NMERAMNT<> 0
  *B608996,1 TMI [ end ] check that the merch amount <> 0
  REPLACE Tax_Amt  WITH ROUND( (SHIPAMT+DISCOUNT)*TAX_RATE/100 ,2) ;
          totalchg WITH SHIPAMT+DISCOUNT + ROUND( (SHIPAMT+DISCOUNT)*TAX_RATE/100 ,2)
  loFormSet.AriaForm1.Ariapageframe1.Page4.cntInvoicesummary.spnDscPcnt.Enabled = .F.
  *B608996,1 TMI [start] check that the merch amount <> 0
ENDIF
*B608996,1 TMI [ end ] check that the merch amount <> 0
        
SELECT (lnSlct )
*-- end of lfINVMRDSC.
        
        
        
*:**************************************************************************
*:* Name        : lfAdPrWbCl  && 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/24/2009
*:* Purpose     : Add the follwoing columns
*                          CWBPRODREF : Web Product Ref
*                          CPERSON  : Personlise
*:***************************************************************************
*:* Called from : SOORD.SCX
*:***************************************************************************
FUNCTION lfAdPrWbCl  && 
LOCAL lnLast
IF loFormset.Activemode <> 'S' AND loFormSet.llDetails
  SELECT (lcOrderFile)
  WITH loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1
    lnLast = .ColumnCount 
    .ColumnCount = .ColumnCount + 2
    lcOrderFile = .Recordsource

    lnLast = lnLast+1 
    .Columns(lnLast).Controlsource   = lcOrderFile+'.CPERSON'
    .Columns(lnLast).header1.Caption = 'Personlise' 

    lnLast = lnLast+1 
    .Columns(lnLast).Controlsource   = lcOrderFile+'.CWBPRODREF'
    .Columns(lnLast).header1.Caption = 'Web Product Ref' 
    .Columns(lnLast).Width = 110

    .SETALL('ReadOnly',.T.,'COLUMN')
  ENDWITH 
ENDIF 
        