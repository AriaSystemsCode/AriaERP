*!**************************************************************************
*! Name      : VITMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 11/07/2001
*! Purpose   : VITAL'S Main program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! C102478,1 ..
*!**************************************************************************
*! Modifications:
*! B606274,1 ADEL 08/04/02 Generate a Picking Ticket for selected style(s) only
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!******************************************************************
*! Name      : lfUPDPICK
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/03/2001
*! Purpose   : Update piktkt file.
*!******************************************************************
FUNCTION lfUPDPICK

*--Generate P/T  from P. List?
lcGenPk = gfGetMemVar('M_GENPIK',gcAct_Comp)
IF lcGenPk = 'N'
  RETURN
ENDIF
*--When editing
IF SEEK(laData[2] + laData[1], 'PIKTKT')
  RETURN
ENDIF
SELECT(lcPckLin)
lcOldSkip = SET('SKIP')
SET SKIP TO
*B606274,1 (Begin) Generate a Picking Ticket for selected style(s) only
*SCAN 
SCAN FOR Selected = 1 OR;
  ((PQty1<> 0 OR PQty2<>0 OR PQty3<>0 OR PQty4<>0 OR PQty5<>0 OR PQty6<>0 OR PQty7<>0 OR PQty8<>0);
   AND (SEEK('O'+laData[2]+laData[5]+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine') AND !ORDLINE.PICKED))
*B606274,1 (End)
  SCATTER MEMVAR MEMO
  = SEEK('O'+laData[2]+laData[5]+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine')
  *-- Check it stock is still available for allocation
  SET ORDER TO ORDPIK IN PIKTKT
  IF !SEEK(laData[2] + laData[1], 'PIKTKT')
    INSERT INTO PIKTKT;
               (Piktkt, Account, Store, Order, Date, cWareCode, CustPo, Status);
           VALUES;
               (laData[1], laData[4], laData[5], laData[2], gdSysDate, lcWareCode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')
  ENDIF             
  REPLACE Pack_Hdr.PIKTKT WITH laData[1]
  =gfAdd_Info('PIKTKT')
  *-- Reseek
  = SEEK('O'+laData[2]+laData[5]+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine')
  SELECT OrdLine 
  REPLACE Pik1    WITH nPck1,;
          Pik2    WITH nPck2,;
          Pik3    WITH nPck3,;
          Pik4    WITH nPck4,;
          Pik5    WITH nPck5,;
	      Pik6    WITH nPck6,;
	      Pik7    WITH nPck7,;
          Pik8    WITH nPck8,;
          TotPik  WITH Pik1 + Pik2 + Pik3 + Pik4 +Pik5 + Pik6 + Pik7 + Pik8,;
          PIKTKT  WITH laData[1],;
          PICKED  WITH .T.
  *B606274,1 (Begin) Update PikDate
  REPLACE PIKDATE WITH gdSysDate
  *B606274,1 (End)
          
  *-- Update allocated quantities in STYDYE 
  SELECT STYDYE
  IF SEEK(m.Style + ORDLINE.cWareCode + SPACE(10)) .AND. gfObj_Lock(.T.)
    REPLACE Alo1   WITH Alo1 + OrdLine.Pik1,;
            Alo2   WITH Alo2 + OrdLine.Pik2,;
            Alo3   WITH Alo3 + OrdLine.Pik3,;
            Alo4   WITH Alo4 + OrdLine.Pik4,;
            Alo5   WITH Alo5 + OrdLine.Pik5,;
            Alo6   WITH Alo6 + OrdLine.Pik6,;
            Alo7   WITH Alo7 + OrdLine.Pik7,;
            Alo8   WITH Alo8 + OrdLine.Pik8,;
            TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
    =gfObj_Lock(.F.)
  ENDIF
  *-- Update allocated quantities in STYDYE if dyelots
  =SEEK(m.Style,'STYLE')
  IF llDyelot .AND. Style.cDye_flg = 'Y' .AND.;
     SEEK(m.Style + ORDLINE.cWareCode + ORDLINE.Dyelot) .AND. gfObj_Lock(.T.)
    REPLACE Alo1   WITH Alo1 + OrdLine.Pik1,;
            Alo2   WITH Alo2 + OrdLine.Pik2,;
            Alo3   WITH Alo3 + OrdLine.Pik3,;
            Alo4   WITH Alo4 + OrdLine.Pik4,;
            Alo5   WITH Alo5 + OrdLine.Pik5,;
            Alo6   WITH Alo6 + OrdLine.Pik6,;
            Alo7   WITH Alo7 + OrdLine.Pik7,;
            Alo8   WITH Alo8 + OrdLine.Pik8,;
            TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
    =gfObj_Lock(.F.)
  ENDIF  
  *-- Update the style  record in STYLE file.
  SELECT STYLE
  IF SEEK(m.Style) .AND. gfObj_Lock(.T.)
    REPLACE Alo1   WITH Alo1 + OrdLine.Pik1,;
            Alo2   WITH Alo2 + OrdLine.Pik2,;
            Alo3   WITH Alo3 + OrdLine.Pik3,;
            Alo4   WITH Alo4 + OrdLine.Pik4,;
            Alo5   WITH Alo5 + OrdLine.Pik5,;
            Alo6   WITH Alo6 + OrdLine.Pik6,;
            Alo7   WITH Alo7 + OrdLine.Pik7,;
            Alo8   WITH Alo8 + OrdLine.Pik8,;
            TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
    =gfObj_Lock(.F.)
  ENDIF
ENDSCAN
SET SKIP TO &lcOldSkip

*!****************************************************************************************
*! Name      : lfSrtByLoc
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/24/2002
*! Purpose   : Add new option - Location - to Sort by filter in Order Confirmation form
*!****************************************************************************************

FUNCTION lfSrtByLoc

DIMENSION laSortDesc[3,1] , laSortVal[3,1]
laSortDesc[1] = 'Style'
laSortDesc[2] = 'Line Number'
laSortDesc[3] = 'Location'
    
laSortVal[1] = 'S'
laSortVal[2] = 'L'
laSortVal[3] = 'C'    

*--End of lfSrtByLoc.