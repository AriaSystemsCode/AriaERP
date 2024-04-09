PARAMETER lcDataID

IF TYPE('lcDataID')<> 'C'
  lcDataID = ''
ENDIF

CLOSE DATA
SET EXCL OFF
SET EXACT OFF
SET SAFE OFF

IF !FILE('sysfiles\syccomp.dbf')
  lcSysPath = GETDIR('','Select System Files Directory')
ELSE
  lcSysPath = 'SysFiles\'
ENDIF
IF EMPTY(lcSysPath)
  RETURN
ENDIF


lcDataID = ALLTRIM(lcDataID)

USE (lcSysPath+'SYCCOMP.DBF') IN 0 SHARED
SELECT SYCCOMP
SCAN FOR cComp_ID = lcDataID

  IF FILE(ALLTRIM(cCom_dDir)+'glerrors.txt')
	  errfile = FOPEN(ALLTRIM(cCom_dDir)+'glerrors.txt',12)
  ELSE
	  errfile = FCREATE(ALLTRIM(cCom_dDir)+'glerrors.txt')
  ENDIF
  IF errfile < 0
	  WAIT 'Cannot open or create output file' WINDOW NOWAIT
  	RETURN
  ELSE
	  = FPUTS(errfile, PADR('Transaction',15) + PADR('Tran#',10) + 'Taken Action')
  ENDIF


  = lfOpnFiles(cCom_dDir)
  = lfChkDist()
  = lfClsDbf()
ENDSCAN

CLOSE DATA

FUNCTION lfOpnFiles
PARAMETER lcDataDir
lcDataDir = ALLTRIM(lcDataDir)
*-- Open glDistFile
USE (lcDataDir+'GLDIST')     IN 0 SHARED ORDER TAG Gldistno

*-- Open InvHdr
USE (lcDataDir+'InvHdr.dbf') IN 0 SHARED ORDER TAG InvHdr

*-- Open arHist
USE (lcDataDir+'arHist.dbf') IN 0 SHARED ORDER TAG Arhistt

*-- Open DEBIT
USE (lcDataDir+'DEBIT.dbf') IN 0 SHARED ORDER TAG Debit


***********************************************************************************************
FUNCTION lfChkDist

  SELECT tran_type , tran_no , SUM(neqvamnt) as Var ;
    FROM gldist                                     ;
GROUP BY tran_type,tran_no HAVING Var <> 0          ;
ORDER BY tran_type DESC                             ;
    INTO CURSOR DistVar

*-- Establish relations between temp cursor and gldist,invhdr
SELECT DistVar
SET RELATION TO tran_no+tran_type INTO Gldist ADDITIVE

SELECT DistVar
SET RELATION TO Distvar.tran_no INTO Invhdr ADDITIVE

SELECT DistVar

lcInvToDel = ''
SCAN 
  DO CASE 
     CASE TRAN_TYPE = 'VI'
       * 1  if 'VI' replace Posted with X
       *    also Its 'IN' posted with X
       *    in glDist File
       SELECT GLDIST
       REPLACE POSTED WITH 'X'
       lcInvToDel = lcInvToDel + " " +TRAN_NO
       =lfWriteErr('VI',.F.)

     CASE TRAN_TYPE =  "IN" AND  TRAN_NO $ lcInvToDel 
       SELECT GLDIST
       REPLACE POSTED WITH 'X'
       lcInvToDel = STRTRAN(lcInvToDel,TRAN_NO)

     CASE TRAN_TYPE =  "IN"
       =lfHandInv()
       
     CASE TRAN_TYPE =  "KO"  
       IF SEEK(TRAN_NO+TRAN_TYPE,"GLDIST")
         SELECT GLDIST
         DELETE REST WHILE TRAN_NO+TRAN_TYPE=DistVar.TRAN_NO+DistVar.TRAN_TYPE
       ENDIF  
    OTHERWISE
       =lfWriteErr("N/A")
  ENDCASE
ENDSCAN

***********************************************************************************************
FUNCTION lfHandInv
*  llHDrRep = .F.
*-- Adjust glDist File by Diffrence
=SEEK(TRAN_NO+TRAN_TYPE,"GLDIST")
STORE 0.00 TO lnSales,lnFreight,lnDiscount,lnTax
SELECT glDist
SCAN WHILE TRAN_NO+TRAN_TYPE=DistVar.TRAN_NO+DistVar.TRAN_TYPE
  DO CASE
    CASE CATG_KEY = '003' 
      lnSales     = lnSales    + neqvamnt
    CASE CATG_KEY = '004'
      lnFreight   = lnFreight  + neqvamnt
    CASE CATG_KEY = '005'
      lnDiscount  = lnDiscount + neqvamnt
    CASE CATG_KEY = '014'
      lnTax       = lnTax      + neqvamnt
  ENDCASE
ENDSCAN

IF SEEK(INVHDR.ACCOUNT + DistVar.TRAN_NO , 'ARHIST') && Keyd Off Invoice
  =lfWriteErr(PADR(DistVar.Tran_No,15) + 'Fixed glDist File' +CHR(10) + CHR(13))
  *-- Adjust Sales revenue amount in gldist file
  IF (INVHDR.SHIPAMT + lnSales) <> 0 AND SEEK(DistVar.TRAN_NO+DistVar.TRAN_TYPE)
    lnSlsDiff = INVHDR.SHIPAMT + lnSales
    LOCATE REST ;
     WHILE TRAN_NO+TRAN_TYPE=DistVar.TRAN_NO+DistVar.TRAN_TYPE;
       FOR CATG_KEY = '003'
    SCATTER MEMVAR MEMO
    APPEND BLANK
    M.nEqvAmnt  = -lnSlsDiff
    M.nGlAmount = -lnSlsDiff
    GATHER MEMVAR MEMO
  ENDIF

  *-- Adjust discount amount in gldist file
  IF (INVHDR.DISCOUNT + lnDiscount) <> 0 AND SEEK(DistVar.TRAN_NO+DistVar.TRAN_TYPE)
    =lfWriteErr('VI',.T.)
    lnDisDiff = INVHDR.DISCOUNT + lnDiscount
    LOCATE REST ;
     WHILE TRAN_NO+TRAN_TYPE=DistVar.TRAN_NO+DistVar.TRAN_TYPE;
       FOR CATG_KEY = '005'
    SCATTER MEMVAR MEMO
    APPEND BLANK
    M.nEqvAmnt  = -lnDisDiff
    M.nGlAmount = -lnDisDiff
    GATHER MEMVAR MEMO
  ENDIF

  *-- Adjust frieght amount in gldist file
  IF (INVHDR.freight+INVHDR.INSUR + lnFreight) <> 0 AND SEEK(DistVar.TRAN_NO+DistVar.TRAN_TYPE)
    lnFrDiff = (INVHDR.freight+INVHDR.INSUR) + lnFreight
    LOCATE REST ;
     WHILE TRAN_NO+TRAN_TYPE=DistVar.TRAN_NO+DistVar.TRAN_TYPE;
       FOR CATG_KEY = '004'
    SCATTER MEMVAR MEMO
    APPEND BLANK
    M.nEqvAmnt  = -lnFrDiff
    M.nGlAmount = -lnFrDiff
    GATHER MEMVAR MEMO
  ENDIF


  *-- Adjust TAX amount in gldist file
  IF (INVHDR.TAX_AMOUNT + lnTax) <> 0 AND SEEK(DistVar.TRAN_NO+DistVar.TRAN_TYPE)
    lnTaxDiff = (INVHDR.TAX_AMOUNT) + lnTax
    LOCATE REST ;
     WHILE TRAN_NO+TRAN_TYPE=DistVar.TRAN_NO+DistVar.TRAN_TYPE;
       FOR CATG_KEY = '014'
    SCATTER MEMVAR MEMO
    APPEND BLANK
    M.nEqvAmnt  = -lnTaxDiff
    M.nGlAmount = -lnTaxDiff
    GATHER MEMVAR MEMO
  ENDIF

ELSE && Invoice not keyd off
  =lfWriteErr('VI',.F.)
      
  *-- Adjust Sales revenue amount in invhdr file
  IF (INVHDR.SHIPAMT + lnSales) <> 0
    lnSlsDiff = INVHDR.SHIPAMT + lnSales
    REPLACE INVHDR.SHIPAMT WITH INVHDR.SHIPAMT - lnSlsDiff
  ENDIF

  *-- Adjust discount amount in gldist file
  IF (INVHDR.DISCOUNT + lnDiscount) <> 0
    lnDisDiff = INVHDR.DISCOUNT + lnDiscount
    REPLACE INVHDR.DISCOUNT WITH INVHDR.DISCOUNT - lnDisDiff
  ENDIF

  *-- Adjust freight amount in gldist file
  IF (INVHDR.freight + INVHDR.INSUR + lnFreight) <> 0
    lnFrDiff = (INVHDR.freight+INVHDR.INSUR) + lnFreight
    REPLACE INVHDR.freight WITH INVHDR.freight - lnFrDiff
  ENDIF

  *-- Adjust TAX amount in gldist file
  IF (INVHDR.TAX_AMT + lnTax) <> 0
    lnTaxDiff = INVHDR.TAX_AMT + lnTax
    REPLACE INVHDR.TAX_AMT WITH INVHDR.TAX_AMT - lnTaxDiff
  ENDIF
  *-- Adjust total Amount field in invhdr file
  REPLACE INVHDR.TOTALCHG WITH (INVHDR.SHIPAMT-INVHDR.DISCOUNT+INVHDR.freight + INVHDR.INSUR+INVHDR.TAX_AMT)


  *-- Adjust AR Key in gldist File
  SELECT GLDIST
  IF SEEK(DistVar.TRAN_NO+DistVar.TRAN_TYPE)
    LOCATE REST ;
     WHILE TRAN_NO+TRAN_TYPE=DistVar.TRAN_NO+DistVar.TRAN_TYPE;
       FOR CATG_KEY = '001'
    REPLACE nEqvAmnt  WITH INVHDR.TOTALCHG,;
            nGlAmount WITH INVHDR.TOTALCHG
 
  ENDIF

  
  SELECT DEBIT
  IF SEEK(INVHDR.ACCOUNT+INVHDR.INVOICE)
    REPLACE Amount  WITH INVHDR.TotalChg,;
            Dsc_Amt WITH (INVHDR.ShipAmt+INVHDR.Discount) * INVHDR.Trde_Disc/100 
  ENDIF
ENDIF

***********************************************************************************************

*--Function to close opened data files
FUNCTION lfClsDbf

USE IN GLDIST
USE IN INVHDR
USE IN DistVar
USE IN Debit
=FCLOSE(errfile)
***********************************************************************************************
FUNCTION lfWriteErr
PARAMETER lcTranTyp , llKo
lcAcion=''
DO CASE
  CASE lcTranTyp = 'IN'
       lcTranTyp = 'INVOICE'

       IF llKo
         lcAcion   = 'Fixed glDist File'
       ELSE
         lcAcion   = "Fixed INVHDR FILE AND 'AR' RECORD IN glDist File"
       ENDIF
  
  CASE lcTranTyp = 'VI'
       lcTranTyp ='VOID INVOICE'
       lcAcion   = "Marked as Posted"
       
  CASE lcTranTyp = 'KO'
       lcTranTyp ='KEY OFF'
       lcAcion   = "Deleted from glDist File"
       
  CASE lcTranTyp = 'N/A'
       lcTranTyp = DistVar.TRAN_NO
       lcAcion   = "NO ACTION"
ENDCASE  

lcTranNo  = DistVar.TRAN_NO
WAIT WINDOW "Fix " + PADR(lcTranTyp,15) + PADR(lcTranNo,10) nowait
=FPUTS(errfile, PADR(lcTranTyp,15) + PADR(lcTranNo,10) + lcAcion)
