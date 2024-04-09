*:****************************************************************************
*: Program file        : MFSTYDYE
*: Program desc.       : Print Dye Orders
*: For Report          : MFSTYDYE.FRX
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Manufucturing (MF)
*: Developer           : RAMY MABROUK (RAM)
*: Tracking Job Number : C200080,4
*: Date                : July/99
*:****************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfApVnBrow()
*:                 gfCodDes()
*:****************************************************************************
*: Called From         : System Menu (MF --> Output --> Transaction -->Dye Order Report)
*:****************************************************************************
*: Passed Parameters  : None
*:****************************************************************************
*: Example : DO MFSTYDYE
*:****************************************************************************
*: Modification        :
*C200098 RAMY 10/21/1999 Print the Rate field dirctly from the poshdr file
*:****************************************************************************

*-- lcDivDesc       Hold the division Describtion
STORE SPACE(0) TO lcDivDesc

*-- Creating temp. file that will hold the collected data
SELECT POSLN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru , 1)
*C200098 RAMY Change these lines because of now we print the 'nRate' filed directly
*C200098 RAMY from the POSHDR file [start]

*DIMENSION laFileStru[lnFileStru+2,4]
*laFileStru[lnFileStru+1 , 1] = 'nRate'
*laFileStru[lnFileStru+1 , 2] = 'N'
*laFileStru[lnFileStru+1 , 3] = 6
*laFileStru[lnFileStru+1 , 4] = 0
*laFileStru[lnFileStru+2 , 1] = 'Complete'
*laFileStru[lnFileStru+2 , 2] = 'D'
*laFileStru[lnFileStru+2 , 3] = 8
*laFileStru[lnFileStru+2 , 4] = 0


DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1 , 1] = 'Complete'
laFileStru[lnFileStru+1 , 2] = 'D'
laFileStru[lnFileStru+1 , 3] = 8
laFileStru[lnFileStru+1 , 4] = 0

CREATE TABLE (gcWorkDir+lcRpTmp) FROM ARRAY laFileStru

*-- Choose the index [Begin]

*-- If choosen (Sort on Dye Order)
IF lcRpSortBy = 'D'
  INDEX ON PO TAG (lcRpTmp)
ELSE
  *-- If choosen (Sort on Coplete Date +Dye Order)
  INDEX ON DTOS(COMPLETE) + cStyType + PO TAG (lcRpTmp)
ENDIF
*-- Choose the index [End]


*--Collecting the data depends on the report format
SELECT POSHDR
IF lcRpSumDet='D'   && When report format is 'DETAIL'

  *-- Loop for inserting data in the temp file
  SCAN FOR cStyType = 'D' .AND. &lcRpExp
    SELECT POSLN
    IF SEEK(POSHDR.cStyType + POSHDR.PO)
      SCAN WHILE PO = POSHDR.PO  && Looking for the PO# in POSLN file
        SCATTER MEMVAR MEMO
        m.Complete = POSHDR.Complete

        * Condition statement that looks for the PO# in CTKTBOM file
        IF SEEK('I' + POSLN.PO , 'CTKTBOM')
          SELECT CTKTBOM
          LOCATE WHILE CIMTYP+CUTTKT = 'I'+ M.PO  ;
            FOR (CCATGTYP = 'M' AND MFGCODE = PADR(lcOprCod , 6))
           
          IF FOUND()
            *C200098 RAMY Comment this line because of now we print this filed directly 
            *C200098 RAMY from the POSHDR file [start]
            *M.nRate = CTKTBOM.UntCost
            *C200098 RAMY [end]
            SELECT BOMLINE
            IF SEEK('I' + m.PO)   && Locate for the same record in BOMLINE file
              LOCATE WHILE cImTyp + ctktno + cOprCode = 'I' + M.PO + SPACE(06);
                FOR (CCATGTYP='M' AND  CBOMTYP = CTKTBOM.Typ AND MfgCode=PADR(lcOprCod,6))
                    
              m.Gros_Price = BOMLINE.UnitCost
            ENDIF && IF SEEK('I'+.....)
          ENDIF && IF FOUND()
        ENDIF && IF SEEK('I'+.....)
        INSERT INTO (lcRpTmp) FROM MEMVAR
        SELECT POSLN
      ENDSCAN  && SCAN FOR POSHDR.PO = POSLN.PO
    ENDIF   && IF FOUND()
  ENDSCAN  && SCAN FOR CSTYTYPE='D' .AND. POSLN.PO .AND. &lcRpExp

ELSE    && Else [When report format is 'SUMMARY']

  SCAN FOR cStyType = 'D' .AND. &lcRpExp  && Loop for inserting data in the temp file
    SELECT POSLN
    IF SEEK (POSHDR.cStyType + POSHDR.PO)
      SCAN WHILE POSHDR.PO = POSLN.PO  && Looking for the PO# in POSLN file
         *C200098,1 
         INSERT INTO   (lcRpTmp) (cStyType , PO , Vendor ,  COMPLETE) ;
            VALUES (POSLN.Cstytype , POSLN.Po , POSLN.Vendor ,  POSHDR.Complete)

        * Condition statement that looks for the PO# in CTKTBOM file
        *IF SEEK('I'+POSLN.PO,'CTKTBOM')
        *  SELECT CTKTBOM
        *  LOCATE WHILE CIMTYP+CUTTKT = 'I'+ POSLN.PO  ;
        *    FOR (CCATGTYP = 'M' AND MFGCODE = PADR(lcOprCod , 6))

        *  IF FOUND()
        *    INSERT INTO   (lcRpTmp) (cStyType , PO , Vendor , nRate , COMPLETE) ;
              VALUES (POSLN.Cstytype , POSLN.Po , POSLN.Vendor , CTKTBOM.Untcost , POSHDR.Complete)

        *  ENDIF  &&  IF FOUND()
        *ENDIF  &&IF SEEK('I'+POSLN.PO,'CTKTBOM')

        SELECT POSLN
      ENDSCAN && SCAN FOR POSHDR.PO = POSLN.PO
    ENDIF   && IF SEEK(PO , 'POSLN')
  ENDSCAN  && SCAN FOR CSTYTYPE='D' .AND. POSLN.PO .AND. &lcRpExp
ENDIF    && End of IF lcRpSumDet='D'

*--- Call the FRX.
SELECT (lcRpTmp)
SET RELATION TO cStyType + PO INTO POSHDR
SET RELATION TO 'S' + Scale INTO SCALE
DO gfDispRe WITH EVAL('lcRpForm')

*-- Close tmp file
SELECT (lcRpTmp)
SET RELATION TO
USE
IF FILE(gcWorkDir+lcRpTmp)
  ERASE (gcWorkDir + lcRpTmp + '.DBF')
  ERASE (gcWorkDir + lcRpTmp + '.CDX')
  ERASE (gcWorkDir + lcRpTmp + '.FPT')
ENDIF

*:****************************************************************************
*! Name      : lfCallCod
*! Developer : RAMY MABROUK (RAM)
*! Date      : 07/25/1999
*! Purpose   : To get the Division Description
*:****************************************************************************
*! Calls       : gfCodDes().
*!****************************************************************************
*! Passed Parameters : None.
*:****************************************************************************
*! Return      : None
*:****************************************************************************
*! Example     : = lfCallCod()
*:****************************************************************************

FUNCTION lfCallCod

  * To get the Division Description
  lcDivDesc   = gfCodDes(POSHDR.CDIVISION, 'CDIVISION')

  RETURN ''

  *!****************************************************************************
  *! Name      : lfwOldVal
  *! Developer : RAMY MABROUK (RAM)
  *! Date      : 07/25/1999
  *! Purpose   : To get the old value of the field
  *!****************************************************************************
  *! Calls       : None.
  *!****************************************************************************
  *! Passed Parameters : None.
  *!****************************************************************************
  *! Return      : None
  *!****************************************************************************
  *! Example     : = lfwOldVal()
  *!****************************************************************************

FUNCTION lfwOldVal

  *-- Save the Old Value
  laOldVal = EVALUATE(SYS(18))

  *!****************************************************************************
  *! Name      : lfvVend
  *! Developer : RAMY MABROUK (RAM)
  *! Date      : 07/25/1999
  *! Purpose   : Valid function of the Vendor field.
  *!****************************************************************************
  *! Calls       : gfApVnBrow().
  *!****************************************************************************
  *! Passed Parameters : None.
  *!****************************************************************************
  *! Return      : None
  *!****************************************************************************
  *! Example     : = lfvVend()
  *!****************************************************************************

FUNCTION lfvVend

  PRIVATE lcVar, lcObj
  lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
  lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
  SELECT APVENDOR
  SET ORDER TO TAG VenCode

  *-- IF Statment to check if we are going to Browse
  IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))

    * Browse the vendors file
    =gfApVnBrow(@lcObj)

    * Check if we well update the field or not
    IF !EMPTY(lcObj)
      &lcVar = lcObj      && Update the field
    ELSE    && Else of IF !EMPTY(lcObj)
      &lcVar = laOldVal
    ENDIF    && End of IF !EMPTY(lcObj)
  ENDIF    && End of IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj...
  SELECT APVENDOR
  SET ORDER TO