*!****************************************************************
*! Program file  : FXSPLTPO.PRG
*! Program desc. : Fix program to update the lost data from the field poshdr.cconpo
*! System        : Aria Apparel System - Version 2.7.
*! Module        : Style Purchase Order (PO)
*! Developer     : TMI - TAREK MOHAMMED IBRAHIM
*! Date          : 01/03/2007
*! T20060915.0004, B607918
*!****************************************************************

*NOTE :  The CCONPO field is always entered onto the original PO and never changed - so needs to be updated to all P Orders split from that original PO


lcFolder = GETDIR('','Select company folder')
IF EMPTY(lcFolder)
  WAIT WINDOW NOWAIT 'No folder selected.'
  RETURN
ENDIF

IF !FILE(lcFolder+'POSHDR.DBF')
  WAIT WINDOW NOWAIT 'Wrong folder selected.'
  RETURN
ENDIF

CLOSE ALL

USE (lcFolder+'POSHDR') IN 0 ORDER POSHDR ALIAS POSHDR
USE (lcFolder+'POSHDR') IN 0 AGAIN        ALIAS POSHDR2

WAIT WINDOW NOWAIT 'Creating a new index to get the cconpo value using the "PARENTPO" '
SELECT POSHDR2
INDEX ON PARENTPO TO (lcFolder+'PARENTPO.IDX') FOR !EMPTY(PARENTPO) AND !EMPTY(CCONPO)

GO TOP
SCAN
  =SEEK(CSTYTYPE+PARENTPO,'POSHDR')
  IF EMPTY(POSHDR.CCONPO)
    SELECT POSHDR
    REPLACE CCONPO WITH POSHDR2.CCONPO
    WAIT WINDOW NOWAIT 'UPDATING THE PO# ' + PO
  ENDIF
ENDSCAN

WAIT CLEAR
CLOSE ALL

ERASE (lcFolder+'PARENTPO.IDX')