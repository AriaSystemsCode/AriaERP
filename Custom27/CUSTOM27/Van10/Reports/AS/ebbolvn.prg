*C102834,1 MME Changes in Bill of Lading, [Begin]
LPARAMETERS oObj,cModule
X = ALIAS()
lcWorkDir = oObj.WORKDIR + oObj.lcRepDir
USE (lcWorkDir+'\BolHdr.DBF') IN 0 SHARED
SELECT BOLHDR
**Select BOLHDR to get the required data for each PO
SCAN
  i = 1
  IF SEEK(BOL_NO,'BOL_LIN','BOL_LIN')
    SELECT bol_lin
    
    **Scan rest of the BOL_LIN file to get the required shipper information
    ** While the BOL_NO is the same in both BOL_LIN and BOLHDR
    SCAN REST WHILE BOL_NO + ORDER + pack_no = BOLHDR.BOL_NO FOR i<=5
      IF SEEK('O' + bol_lin.ORDER,'ORDHDR','ORDHDR')
    	**Collect the data
        cShipperInfo = IIF(EMPTY(STNAME), ' ' , ALLTRIM(STNAME) + ', ' ) + ;
        ' O ' + ', ' + ;
        IIF(EMPTY(ORDHDR.DEPT), ' ', ALLTRIM(ORDHDR.DEPT) + ', ') + ;
        IIF(EMPTY(DTOC(ORDHDR.COMPLETE)) , ' ' , DTOC(ORDHDR.COMPLETE))

        lCORD = 'BOLHDR.ORD' + ALLTRIM(STR(i))
        IF &lCORD. = ORDHDR.CUSTPO
          lCshpInfo = 'BOLHDR.CshpInfo'+ALLTRIM(STR(i))
          REPLACE &lCshpInfo. WITH cShipperInfo
        ELSE
          i=i+1
          IF i > 5
            EXIT
          ENDIF
          lCshpInfo = 'BOLHDR.CshpInfo'+ALLTRIM(STR(i))
          REPLACE &lCshpInfo. WITH cShipperInfo
        ENDIF
      ENDIF
    ENDSCAN
    SELECT BOLHDR
  ENDIF
  ** Get the entered code for the commodity description from the codes file
  ** Note that this code on the order header level
  IF SEEK('N' + PADR('CCOMDESC',10) + ORDHDR.ccomdesc, 'CODES', 'CCODE_NO')
    REPLACE BOLHDR.ccomm_desc WITH CODES.CDISCREP
  ENDIF
  ** Collect data for the special instructions
  IF SEEK('S' + ACCOUNT + STORE, 'CUSTOMER', 'CUSTOMER')
    mSpc_Inst2 = IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM1)), '' ,ALLTRIM(CUSTOMER.CCOMM1) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM2)), '' ,ALLTRIM(CUSTOMER.CCOMM2) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM3)), '' ,ALLTRIM(CUSTOMER.CCOMM3) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM4)), '' ,ALLTRIM(CUSTOMER.CCOMM4) + CHR(10) );
    + IIF(EMPTY(ALLTRIM(CUSTOMER.CCOMM5)), '' ,ALLTRIM(CUSTOMER.CCOMM5))
    REPLACE mspc_inst WITH mSpc_Inst2
  ENDIF

ENDSCAN
USE IN 'BolHdr'
IF!EMPTY(X)
  SELECT (X)
ENDIF
*C102834,1 MME Changes in Bill of Lading, [End]
