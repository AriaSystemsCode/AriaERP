*!**********************************************************************
*!PROGRAM   : ICDAN200
*!AUTH      : Nader Anis
*!DATE      : 05/23/2000
*! DESC     : Print Bill of Lading LABELS with its Packing Slips .
*! REF.     : C101776
*! NOTES    : There is a hidden field on the details part on the .frx, this 
*!            hidden box you can see if you increased the size of the details 
*!            part. this part calls the function lfSkip() that handel 
*!            the printing of wanted copies.
*!**********************************************************************
IF lcRpExp='.T.'
  *--Range cannot be empty
  =gfModalGen('TRM003680000')
  RETURN
ENDIF
lnCtr    = 1
lnCopies =  _PCOPIES
_PCOPIES=1
llMULWare = IIF(gfGetMemVar('M_WareHouse') = 'Y', .T., .F. )
IF !lfSelBOL()
  *--no records selected                       
  =gfModalGen('TRM42153B0000')
  RETURN
ENDIF

SELECT PACK_HDR.account, &LcTmpbol..store,PACK_HDR.Order, PACK_HDR.PIKTKT,;
       PACK_HDR.SP_INST1,PACK_HDR.SP_INST2,PACK_HDR.store AS ordstore,;
       &lcTmpbol..bol_no ,&lcTmpbol..W_Code,BOL_Lin.PACK_NO;
     FROM PACK_HDR,;
          BOL_LIN,;
          (lcTmpbol) ;
     WHERE BOL_LIN.BOL_NO + BOL_LIN.Order + BOL_LIN.Pack_No = &lcTmpbol..BOL_NO ;
     AND   BOL_Lin.PACK_NO    = Pack_hdr.PACK_NO   ;
     INTO DBF (gcWorkDir+lcTmpLin)




INDEX ON BOL_NO+PACK_NO TAG &lcTmpLin
SET RELATION TO 'O'+ &lcTmpLin..order INTO ORDHDR ADDITIVE

* Add this record to Accept the SKIP -1 in first record to print the wanted 
* number of copies.
IF lnCopies > 1
  APPEND BLANK
  GO TOP
  *Go to the first valid record of the report
  SKIP 1
ENDIF

IF SEEK(W_CODE,'WareHous')
  lcWrName  = WareHous.cDesc 
  lcWrAddr1 = WareHous.cAddress1
  lcWrAddr2 = WareHous.cAddress2
  lcWrAddr3  = LEFT(ALLTRIM(WareHous.cAddress3),10) + ',' + LEFT(ALLTRIM(WareHous.cAddress4),3) + ',' + LEFT(ALLTRIM(WareHous.cAddress5),7)
ENDIF

lcBol_No  = SPACE(6)
lcPack_NO = PACK_NO


lcRepExpr = "!EMPTY(ORDER)"
DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRepExpr
IF USED(lcTmpBol) 
  USE IN (lcTmpBol) 
ENDIF
ERASE (gcWorkDir+lcTmpBol+'.DBF')
ERASE (gcWorkDir+lcTmpBol+'.CDX')
ERASE (gcWorkDir+lcTmpBol+'.FPT')
IF USED(lcTmpLin) 
  USE IN (lcTmpLin) 
ENDIF
ERASE (gcWorkDir+lcTmpLin+'.DBF')
ERASE (gcWorkDir+lcTmpLin+'.CDX')
ERASE (gcWorkDir+lcTmpLin+'.FPT')
*!**********************************************************************
*! FUNCTION : lfSelBOL
*! AUTH     : Nader Anis
*! DATE     : 05/23/2000
*! DESC     : Fill the temp file
*! REF.     : C101776
************************************************************************
FUNCTION lfSelBOL

lcFilter = "EMPTY(Status)"
SELECT BOL_HDR
SET FILTER TO &lcFilter

GO TOP
IF EOF()
  RETURN(.F.)
ENDIF



COPY STRUCTURE TO gcWorkDir+lcTmpBol+".DBF"
=gfOpenFile(gcWorkDir+lcTmpBol,'','EX')
SELECT (lcTmpBol)
INDEX ON BOL_NO TO cWorkDir+lcTmpBol
SET INDEX TO cWorkDir+lcTmpBol

SELECT BOL_HDR
SCAN FOR &lcRpExp
  lcLowBol = BOL_NO
  SELECT (lcTmpBol)
  IF SEEK(lcLowBol)
    REPLACE RECFLAG WITH IIF(RECFLAG='Y','N','Y')
  ELSE  
    SELECT BOL_HDR
    SCATTER MEMVAR
    SELECT (lctmpBol)
    APPEND BLANK
    GATHER MEMVAR
    REPLACE RECFLAG WITH 'Y'
  ENDIF
ENDSCAN
 

SELECT (lcTmpBol)
GOTO TOP
DELETE ALL FOR RECFLAG='N'
GOTO TOP
IF EOF()
  RETURN(.F.)
ENDIF
RETURN(.T.)

*!**********************************************************************
*! FUNCTION : lfGetAdd
*! AUTH     : Nader Anis
*! DATE     : 05/23/2000
*! DESC     : Get the stores and wareHouse addresses.
*! REF.     : C101776
*! NOTES    : Called from the .frx
************************************************************************

FUNCTION lfGetAdd
lnAlias = SELECT(0)
SELECT (lctmpLin)

IF SEEK(W_CODE,'WareHous')
  lcWrName  = WareHous.cDesc 
  lcWrAddr1 = WareHous.cAddress1
  lcWrAddr2 = WareHous.cAddress2
  lcWrAddr3  = LEFT(ALLTRIM(WareHous.cAddress3),10) + ',' + LEFT(ALLTRIM(WareHous.cAddress4),3)  + ',' + LEFT(ALLTRIM(WareHous.cAddress5),7)
ENDIF
IF OrdHdr.Alt_Shpto
  lcOname  = OrdHdr.StName
  lcOAddr1 = OrdHdr.cAddress1
  lcOAddr2 = OrdHdr.cAddress2 
  lcOAddr3 = LEFT(ALLTRIM(OrdHdr.cAddress3),10)+','+OrdHdr.cAddress4+','+LEFT(ALLTRIM(OrdHdr.cAddress5),7)
ELSE
  IF SEEK('S'+ACCOUNT+ORDSTORE,'CUSTOMER')
    lcOname  = Customer.StName
    lcOAddr1 = Customer.cAddress1
    lcOAddr2 = Customer.cAddress2 
    lcOAddr3 = LEFT(ALLTRIM(Customer.cAddress3),10)+','+Customer.cAddress4+','+LEFT(ALLTRIM(Customer.cAddress5),7)
   ENDIF
ENDIF

IF SEEK('S'+ACCOUNT+STORE,'CUSTOMER')
  lcBname  = Customer.StName
  lcBAddr1 = Customer.cAddress1
  lcBAddr2 = Customer.cAddress2
  lcBAddr3 = LEFT(ALLTRIM(Customer.cAddress3),10)+','+Customer.cAddress4+','+LEFT(ALLTRIM(Customer.cAddress5),7)
ENDIF

SELECT (lnAlias)

*!**********************************************************************
*! FUNCTION : lfSkip
*! AUTH     : Nader Anis
*! DATE     : 05/23/2000
*! DESC     : Decrement the record pointer to print the wanted number of copies.
*! REF.     : C101776
*! NOTES    : Called from the .frx
*! MODI.    :
************************************************************************
FUNCTION lfSkip

* Return to the same record, number of times equal number of copies wanted.
IF lnPageNo - 1 < lnCopies
  IF !BOF()
    SKIP -1
    * Increment this variable because we have a group on the .frx with it.
    lnCtr = lnCtr + 1
  ENDIF
ELSE
  lnCtr = 1
ENDIF
RETURN ''
*!**************************************************************************
*! Name      : lfBOL
*! Developer : Nader Anis (NAD)
*! Date      : 05/23/2000
*! Purpose   : Go top in the BOL file when browse.
*! Ref       :  C101776
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfAccount()
*!**************************************************************************
*
FUNCTION lfBol
PARAMETERS OpGrdParm
lcAlias=ALIAS()
SELECT BOL_HDR
GO TOP
SELECT(lcAlias)
*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
*B802264,1
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.
