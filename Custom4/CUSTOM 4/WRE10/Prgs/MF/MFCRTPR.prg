*:*********************************************************************************
*: Program file  : MFCRTPR.prg
*: Program desc. : Create Project for cutting tickets
*:        System : Aria4 XP.
*:        Module : (MF).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C201396.122,C201397.exe[T20110909.0009]
*:*********************************************************************************
*: Modifications :
*:********************************************************************************
IF !USED('POSHDR_A')
  =gfOpenTable('POSHDR','POSHDR','SH','POSHDR_A')
ENDIF
IF !USED('MFPRJHD')
  =gfOpenTable('PMPRJHD','PMPRJHD','SH','PMPRJHD_A')
ENDIF
lnmajorlen =LEN(gfItemMask("PM"))
SELECT 'POSHDR_A'
=gfSqlRun("Select POSHDR.* From POSHDR Where cStytype ='U' and cBusDocu ='P' and STATUS IN ('O','H') and PO NOT IN (Select cPRJ_ID from PMPRJHD WHERE cprj_typ ='C')",'POSHDR_A')

lcTempPO = gfTempName()   && Name of file that hold temporary Account data.
SELECT 'POSHDR_A'
LOCATE 
lcBrowFlds = [PO   :H = 'CutTkt#' :10,  ;
              Status    :H = 'Status' :10 , ;
              Style    :H = 'Style' :35,  ;
              cDivision :H = 'Division' :30,  ;
              cWareCode    :H = 'Location',;
              Entered     :H = 'Entered',;
              Complete      :H = 'Complete'] 
SET FILTER TO !DELETED()
llContinue = gfBrowse(lcBrowFlds,"Cutting Tickets","POSHDR_A",['PU'],.F.,.F.,.T.,.F.,.F.,.F.,;
                        lcTempPO,"PO",.F.,.F.,.F.,.F.,.F.,.F.,"POSHDR_A")

IF llContinue 
  lcdeftemp = gfGetMemVar('M_MFDEFTMP')
  SELECT (lcTempPO)
  SCAN
    IF gfSeek('C'+&lcTempPO..PO,'PMPRJHD_A') 
      LOOP 
    ENDIF
    WAIT WINDOW 'Creating Project for Cutting ticket# : '+&lcTempPO..PO NOWAIT 
    lnCurrent_Record = RECNO(lcTempPO)
    DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH 'C',&lcTempPO..PO,REPLICATE('*',lnmajorlen),0,lcdeftemp,.F.,.F.
    IF BETWEEN(lnCurrent_Record ,1,RECCOUNT(lcTempPO))
      GO RECORD lnCurrent_Record  IN (lcTempPO)
    ENDIF
  ENDSCAN 
  =gfCloseTable('POSHDR_A')  
  =gfCloseTable('PMPRJHD')
  WAIT CLEAR 
  RETURN 
ELSE
  RETURN 
ENDIF  
                       