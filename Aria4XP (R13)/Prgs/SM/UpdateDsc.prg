*B609875,1 MMT 03/26/2012 Zero discount in store records in customer table[T20120305.0029]
*Create Fix prg to be called by Media to update stores records disc field from Main account record [T20120305.0029]
LPARAMETERS lcA27Sys
USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0
SELECT SYCCOMP_A
SCAN FOR lRunfroma4 AND !EMPTY(ALLTRIM(SYCCOMP_A.ccom_ddir))
  WAIT WINDOW 'Updating Customer table of company:'+SYCCOMP_A.ccomp_id NOWAIT
  lcDataDir =ADDBS(ALLTRIM(SYCCOMP_A.ccom_ddir))
  IF !DIRECTORY(lcDataDir) OR !FILE(lcDataDir+'Customer.DBF')
    LOOP
  ENDIF
  = updatedisc(lcDataDir)
  SELECT SYCCOMP_A
ENDSCAN
USE IN 'SYCCOMP_A'
WAIT CLEAR
FUNCTION UpdateDisc
 LPARAMETERS lccomppath
 USE SHARED (lccomppath+"Customer.DBF") IN 0 ORDER CUSTOMER
 SELECT type, account, store, disc FROM Customer WHERE TYPE+ACCOUNT+STORE ='M' INTO CURSOR 'MainAcc'
 INDEX ON TYPE+ACCOUNT+STORE TAG 'Customer'
 SELECT mainacc
 SET ORDER TO CUSTOMER
 SCAN
    SCATTER MEMO MEMVAR
    SELECT CUSTOMER
    =SEEK('S'+m.account)
    REPLACE disc WITH m.disc REST WHILE TYPE+ACCOUNT+STORE ='S'+m.account FOR disc=0
    SELECT mainacc
 ENDSCAN
 USE IN CUSTOMER
 USE IN mainacc
ENDPROC
