*:***************************************************************************
*: Program file  : ARINVTK.PRG
*: Program desc. : Custom Program to import invoice tracking number in invoice note
*: System        : Aria Advantage Series.
*: Module        : AR
*: Developer     : Abdelrahman Esam(AEG)  C201786(T20150424.0003)
*:***************************************************************************
*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[T20150424.0003]
*B611164,1 MMT 07/12/2016 Convert Import Invoice tracking# program for BBC to R13[P20160601.0001]
*!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable
*:***************************************************************************

*!* TRY
*B611164,1 MMT 07/12/2016 Convert Import Invoice tracking# program for BBC to R13[P20160601.0001][Start]
*DO FORM (oAriaApplication.ScreenHome+"AR\arinvtk.scx")
DO FORM (oAriaApplication.ClientScreenHome+"AR\arinvtk.scx")
*B611164,1 MMT 07/12/2016 Convert Import Invoice tracking# program for BBC to R13[P20160601.0001][End]
*!* CATCH
*!* ENDTRY



*!*************************************************************
*! Name      : lfInitPoForm
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/09/2016
*! Purpose   : Init Method of the Form
*!*************************************************************
FUNCTION lfInitPoForm
LPARAMETERS loFormSet
SET STEP ON

SET MULTILOCKS ON
IF !USED('lcNotPd')
  llT  =gfOpenTable('NOTEPAD','NOTEPAD','SH','lcNotPd')
ENDIF

*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[Begin]
*!*IF !USED('lcTmpInvH')
*!*llT  =gfOpenTable('INVHDR','INVHDR','SH','lcTmpInvH')
IF !USED('lcTmpInv')
  llT  =gfOpenTable('INVHDR','INVHDR','SH','lcTmpInv')
  *B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[End]
ENDIF
*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[Begin]
*!*SELECT lcTmpInvH
*!*SELECT * FROM lcTmpInvH INTO Table (oariaapplication.workdir+'lcTmpInv')
*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[End]
SELECT lcTmpInv
*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[Begin]
*!*INDEX ON Order TAG 'INVORD' ADDITIVE
SET ORDER TO INVORD
*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[End]
IF !USED('lcTmpPack')
  llT  =gfOpenTable('PACK_HDR','PACK_HDR','SH','lcTmpPack')
ENDIF

IF !USED('lcOrdhd')
  llT  =gfOpenTable('ORDHDR','ORDHDR','SH','lcOrdhd')
ENDIF

WITH loFormSet.AriaForm1
  .keyBrowse.ENABLED = .T.
  .keyBrowse.keyCmd.ENABLED = .T.
  .keyBrowse.KeytextBox.ENABLED = .T.
  .keyBrowse.KeytextBox.READONLY = .T.
  loFormSet.AriaForm1.cmdProc.ENABLED = .F.
  loFormSet.AriaForm1.cmdCancel.ENABLED = .T.
ENDWITH

loFormSet.lcTmpInvT = gfTempName()
CREATE CURSOR (loFormSet.lcTmpInvT) (ShpmntInfoCollDate C(50), ShpFrmUPSAcntNum C(50),ShpToCompName C(50), ShpToAttention C(50), ShpToAddr1 C(50),;
  ShpToAddr2 C(50), ShpToCntryTer C(50), ShpToPosCode C(50),  ShpToCty C(50), ShpToState C(5), ShpntInfSerTyp C(50), PckgRef1 C(50),PckgRef2 C(50),;
  PckagRefChrg N(12),PckgWght N(12),PckgTrckNum C(100))
SELECT (loFormSet.lcTmpInvT)
INDEX ON PckgRef1+PckgRef2+PckgTrckNum TAG (loFormSet.lcTmpInvT)

*!*************************************************************
*! Name      : lfChangeMod
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/09/2016
*! Purpose   : Change  mode
*!*************************************************************
FUNCTION lfChangeMod
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1
  .keyBrowse.ENABLED = .T.
  .keyBrowse.keyCmd.ENABLED = .T.
  .keyBrowse.KeytextBox.ENABLED = .T.
  .keyBrowse.KeytextBox.READONLY = .T.
  loFormSet.AriaForm1.cmdProc.ENABLED = .F.
  loFormSet.AriaForm1.cmdCancel.ENABLED = .T.
ENDWITH


*!*************************************************************
*! Name      : lfShrdVal
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/09/2016
*! Purpose   : Change  mode
*!*************************************************************
FUNCTION lfShrdVal
LPARAMETERS loFormSet
SET STEP ON

IF loFormSet.AriaForm1.keyBrowse.Selectedfrombrowse
  loFormSet.lcTmpCsv = GETFILE('xls')
  loFormSet.AriaForm1.keyBrowse.Selectedfrombrowse=.F.
ELSE
  loFormSet.lcTmpCsv = loFormSet.AriaForm1.keyBrowse.KeytextBox.VALUE
ENDIF

IF EMPTY(loFormSet.lcTmpCsv) .OR. !FILE(loFormSet.lcTmpCsv)
  loFormSet.AriaForm1.cmdProc.ENABLED = .F.
  loFormSet.REFRESH()
  RETURN
ENDIF


loFormSet.AriaForm1.keyBrowse.KeytextBox.VALUE = loFormSet.lcTmpCsv
loFormSet.AriaForm1.cmdProc.ENABLED = .T.
loFormSet.REFRESH()






*!*************************************************************
*! Name      : lfProceed
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/11/2016
*! Purpose   : Start Import invoice tracking numebr
*!*************************************************************
Function lfProceed
  Lparameters loFormSet

  Set Step On

  lcFilHandl = Fcreat(oAriaApplication.WorkDir + 'InvNtFRnd.txt')
  lcCustPo=""
  lcPackList=""
  If lcFilHandl < 0
    Return
  Endif
  lcErrCnt = 0

  =Fputs(lcFilHandl,"*======================================================*")
  =Fputs(lcFilHandl,"Couldn't Identify the following  :")
  =Fputs(lcFilHandl,"*======================================================*"+Chr(10))

  Fflush(lcFilHandl)

  Select(loFormSet.lcTmpInvT)
  Append From (loFormSet.lcTmpCsv) Type Xls
  Delete For PckgRef1 = "PackageReference1" .And. PckgRef2 = "PackageReference2"
  Go Top
  lncount=0
  Scan
    lcPackList = Alltrim(Strtran(Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef1')), 'BBC', ''))
    lcCustPo = Alltrim(Strtran(Strtran(Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef2')), 'PO#', ''), 'PO', ''))
    *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [Begin]
    *IF SEEK(lcPackList,'lcTmpPack','PACK_HDR') .AND. SEEK('O'+ALLTRIM(lcTmpPack.ORDER),'lcOrdhd','ORDHDR') .AND. lcOrdhd.custpo = lcCustPo .AND. SEEK(lcOrdhd.ORDER,'lcTmpInv','INVORD')
    If Seek(lcPackList,'lcTmpPack','PACK_HDR')
      If Seek('O'+Alltrim(lcTmpPack.Order),'lcOrdhd','ORDHDR')
        If lcOrdhd.custpo = lcCustPo
          If Seek(lcOrdhd.Order,'lcTmpInv','INVORD')
            *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [End]


            If Seek('C'+Alltrim(lcTmpInv.invoice),'lcNotPd','NOTEPAD')
              Select lcNotPd

              ** check
              *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [Begin]

              If (" TRCK# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum')) $ Mnotes)
                *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [End]

                lcErrCnt=lcErrCnt+1
                =Fputs(lcFilHandl,Alltrim(Str(lcErrCnt))+"- Packing: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef1'))+"    - Order: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef2')))

                =Fputs(lcFilHandl,"Pack No:"+ lcPackList  +" Customer PO:" +lcCustPo + " Tracking#: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum')+" has been imported before " +  Chr(13)))

                =Fputs(lcFilHandl,"   - Tracking# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum'))+Chr(13))
                Fflush(lcFilHandl)
              Else

                Replace Mnotes With Mnotes+ " TRCK# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum'))+Chr(13)
              Endif

            Else
              Select lcNotPd
              Append Blank
               *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [Begin]
              Replace Type With 'C', Key With Alltrim(lcTmpInv.invoice)
             
              Replace Mnotes With  " UserName: "+ oAriaApplication.User_name+ Chr(13) + " Date Time:"+ oAriaApplication.SystemDate+ Chr(13) +"TRCK# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum'))+Chr(13)
              *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [End]
              GFADD_INFO('lcNotPd')
            Endif
            lncount=lncount+1

            *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [Begin]
          Else
            lcErrCnt=lcErrCnt+1
            =Fputs(lcFilHandl,Alltrim(Str(lcErrCnt))+"- Packing: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef1'))+"    - Order: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef2')))

            =Fputs(lcFilHandl,"Pack No:"+ lcPackList  +" Customer PO:" +lcCustPo +" Order No:"+lcOrdhd.Order+" Is not Found in the invoice "+Chr(13))

            =Fputs(lcFilHandl,"   - Tracking# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum'))+Chr(13))
            Fflush(lcFilHandl)

          Endif
        Else

          lcErrCnt=lcErrCnt+1
          =Fputs(lcFilHandl,Alltrim(Str(lcErrCnt))+"- Packing: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef1'))+"    - Order: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef2')))

          =Fputs(lcFilHandl," Pack No:"+ lcPackList  +" Customer PO:" +lcCustPo +" lcCustPo doesn't equal custpo into Ordhd "+Chr(13))

          =Fputs(lcFilHandl,"   - Tracking# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum'))+Chr(13))
          Fflush(lcFilHandl)
        Endif
      Else
        lcErrCnt=lcErrCnt+1
        =Fputs(lcFilHandl,Alltrim(Str(lcErrCnt))+"- Packing: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef1'))+"    - Order: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef2')))

        =Fputs(lcFilHandl," Pack No:"+ lcPackList  +" Customer PO:" +lcCustPo +Chr(13))

        =Fputs(lcFilHandl,"   - Tracking# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum'))+Chr(13))
        Fflush(lcFilHandl)
 
      Endif
      *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [End]
    Else
      lcErrCnt=lcErrCnt+1
      =Fputs(lcFilHandl,Alltrim(Str(lcErrCnt))+"- Packing: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef1'))+"    - Order: "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgRef2')))
      *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [Begin]
      =Fputs(lcFilHandl," Pack No:"+ lcPackList  +" Customer PO:" +lcCustPo +Chr(13))
      *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [End]
      =Fputs(lcFilHandl,"   - Tracking# "+Alltrim(Evaluate(loFormSet.lcTmpInvT+'.PckgTrckNum'))+Chr(13))
      Fflush(lcFilHandl)
    Endif
  Endscan
  *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [Begin]
  =Fputs(lcFilHandl,"- Accepted  "+alltrim(str(lncount))+Chr(13))
  =Fputs(lcFilHandl,"- Rejected  "+ alltrim(str(Reccount(loFormSet.lcTmpInvT)-lncount))+Chr(13))
  *!* B611306,1 AHH 03/05/2017 Import Invoice Tracking Number screen unstable [End]

  =Tableupdate(1 , .F. , 'lcNotPd')





  Fflush(lcFilHandl)
  Fclose(lcFilHandl)
  Create Cursor TMPSTR (mStrRep M(10))
  Append Blank
  Append Memo mStrRep From (oAriaApplication.WorkDir + 'InvNtFRnd.txt') Overwrite
  *B611164,1 MMT 07/12/2016 Convert Import Invoice tracking# program for BBC to R13[P20160601.0001][Start]
  *DO FORM (oAriaApplication.ScreenHome + 'AR\arimprep') with"Error Log Report"
  Do Form (oAriaApplication.ClientScreenHome + 'AR\arimprep') With"Error Log Report"
  *B611164,1 MMT 07/12/2016 Convert Import Invoice tracking# program for BBC to R13[P20160601.0001][End]
  *B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[Begin]
  =gfModalgen("TRM00000B00000","DIALOG",.F.,.F.,'Finished Import Invoice Tracking Numbers')
  *B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[End]


  Select(loFormSet.lcTmpInvT)
  Delete All

  If Used('TMPSTR')
    Use In TMPSTR
  Endif





*!*************************************************************
*! Name      : lfCancel
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/14/2016
*! Purpose   : close Import invoice tracking screen
*!*************************************************************
FUNCTION lfCancel
LPARAMETERS loFormSet
loFormSet.CLOSE()



*!*************************************************************
*! Name      : lfDestroy
*! Developer : Abdelrahman Essam(AEG)
*! Date      : 02/14/2016
*! Purpose   : destroy import tracking screen
*!*************************************************************
FUNCTION lfDestroy
LPARAMETERS loFormSet

IF USED('lcNotPd')
  USE IN lcNotPd
ENDIF
*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[Begin]
*!*	IF USED('lcTmpInvH')
*!*	  USE IN lcTmpInvH
*!*	ENDIF
*B611147,1 AEG 05/16/2016 Issue#2-  Error when trying to import invoice tracking number[End]

IF USED('loFormSet.lcTmpInvT')
  USE IN loFormSet.lcTmpInvT
ENDIF

IF USED('TMPSTR')
  USE IN TMPSTR
ENDIF


IF USED('lcTmpPack')
  USE IN lcTmpPack
ENDIF

IF USED('lcOrdhd')
  USE IN lcOrdhd
ENDIF


IF USED('lcTmpInv')
  USE IN lcTmpInv
ENDIF


DELETE FILE (oAriaApplication.WorkDir + 'InvNtFRnd.txt')
DELETE FILE (oAriaApplication.WorkDir + 'lcTmpInv.dbf')
DELETE FILE (oAriaApplication.WorkDir + 'lcTmpInv.cdx')
