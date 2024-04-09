lcBaseWind = gfTempName()
DECLARE laWareCode[1]
STORE '' TO laWareCode
STORE '' TO lcBOffDir,lcBakUpDir
STORE '' TO lcBackPath

DO (gcScrDir + 'sm\smtmn10.SPX')




FUNCTION lfvBOff

lcBOffDir = GETDIR('','Select Back office data Dir.')
=lfRefresh(lcBaseWind)

*----------------------------------------------------------
FUNCTION lfvOff


DIMENSION laWareCode[1]
lcBakUpDir = GETDIR('','Select Backup Dir.')
IF !EMPTY(lcBakUpDir) AND !EMPTY(lcBOffDir)
  =gfOpenFile(lcBOffDir+'WAREHOUS')
  SELECT CWARECODE + '-' + ALLTRIM(CDESC) + '-' + CSITEID , CWARECODE , CSITEID FROM WAREHOUS INTO ARRAY laWareCode
  SHOW GET pbProc ENABLED
ELSE
  WAIT WINDOW 'Error'
  RETURN
ENDIF

SHOW GET lnWareCD ENABLED
=lfRefresh(lcBaseWind)
*------------------------------------------------------------------------------------------------
FUNCTION lfvWareH

*------------------------------------------------------------------------------------------------
FUNCTION lfvClose


*------------------------------------------------------------------------------------------------






FUNCTION lfReadAct
RETURN



*------------------------------------------------------------------------------------------------
FUNCTION lfvOK
PRIVATE lcTmpDir, llS1Used , llS2Used

*-- create backup directory

SET DEFAULT TO &lcBakUpDir
lcTmpDir = ALLTRIM(laWareCode[lnWareCD,2])
! /7 CD &lcBakUpDir
! /7 MD &lcTmpDir

lcBackPath =  lcBakUpDir + ALLTRIM(laWareCode[lnWareCD,2])+'\'

*--Copy InvHDR Files
lcTmpSFile = lcBOffDir + 'invHDR.*'
lcTmpDFile = lcBackPath + 'invHDR.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*###############################################################################################
* Category 1 [Start]
*###############################################################################################
*-- use invHDR and zap
USE lcBackPath+'INVHDR.DBF' ALIAS INVHDR_F EXCLUSIVE IN 0
SELECT INVHDR_F
ZAP

*--Copy InvLine Files
lcTmpSFile = lcBOffDir + 'invLine.*'
lcTmpDFile = lcBackPath + 'invLine.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- use invline and zap
USE lcBackPath+'INVLINE.DBF' ALIAS INVLINE_F EXCLUSIVE IN 0
SELECT INVLINE_F
ZAP


llS1Used = gfOpenfile(lcBOffDir + 'invLine','INVLINE','SH','InvLine_A')
llS2Used = gfOpenfile(lcBOffDir + 'invHDR','','EX','invHDR_A')

*--ESTABLISH A RELATION BETWEEN invLine And invHdr
SELECT INVHDR_A

*--Add New Temp Index on INVHDR file
lcIndexF = lcBackPath+'FXINDEX.CDX'

INDEX ON CWARECODE TO &lcIndexF ADDITIVE
REINDEX

SET RELATION TO INVOICE INTO INVLINE_A ADDI
SELECT INVHDR_A

*--COLLECT DATA FOR THE SELECTED W/H It should be very slow
IF SEEK(laWareCode[lnWareCD,2])
SCAN WHILE CWARECODE = laWareCode[lnWareCD,2]
    WAIT WINDOW 'Processing Inv#' + INVHDR_A.invoice nowait
    SCATTER MEMVAR MEMO
    SELECT INVHDR_F
    APPEND BLANK
    GATHER MEMVAR MEMO
  SELECT INVLINE_A
  SCAN WHILE INVOICE = INVHDR_A.INVOICE
    WAIT WINDOW 'Processing Inv#' + INVLINE_A.invoice nowait
    SCATTER MEMVAR MEMO
    SELECT INVLINE_F
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDSCAN
ENDIF
*-- close invoice files
SELECT INVHDR_F
REINDEX

SELECT INVLINE_F
REINDEX

USE IN InvLine_A
SELECT InvHDR_A
SET ORDER TO
USE IN InvHDR_A

USE IN INVHDR_F
USE IN INVLINE_F

*--Copy POSTRAN Files
lcTmpSFile = lcBOffDir + 'POSTRAN.*'
lcTmpDFile = lcBackPath + 'POSTRAN.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing POSTRAN File
WAIT WINDOW 'Processing POSTRAN...' nowait
USE lcBackPath+'POSTRAN.DBF' ALIAS POSTRAN_F EXCLUSIVE IN 0
SELECT POSTRAN_F
WAIT WINDOW 'Processing POSTRAN...' nowait
ZAP
WAIT WINDOW 'Processing POSTRAN...' nowait
USE (lcBOffDir + 'POSTRAN') AGAIN ALIAS POSTRAN_A IN 0 EXCLUSIVE

*--Add New Temp Index on POSTRAN file
SELECT POSTRAN_A
lcIndexF = lcBackPath+'FXINDEX.CDX'

INDEX ON CWARECODE TO &lcIndexF ADDITIVE
REINDEX

SELECT POSTRAN_A
IF SEEK(laWareCode[lnWareCD,2])
SCAN WHILE CWARECODE = laWareCode[lnWareCD,2]
  WAIT WINDOW 'Processing POSTRAN...' + CPOSSEQ nowait
  SCATTER MEMVAR MEMO
  SELECT POSTRAN_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SET INDEX TO
ENDIF

*--Add New Temp Index on POSTRAN file
SELECT POSTRAN_A
lcIndexF = lcBackPath+'FXINDEX.CDX'

INDEX ON COWNER TO &lcIndexF ADDITIVE
REINDEX
IF SEEK(laWareCode[lnWareCD,3])
SCAN WHILE cOwner = laWareCode[lnWareCD,3]
  WAIT WINDOW 'Processing POSTRAN...' + CPOSSEQ nowait
  SCATTER MEMVAR MEMO
  SELECT POSTRAN_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SET INDEX TO
ENDIF
SELECT POSTRAN_F
REINDEX
USE IN POSTRAN_F
USE IN POSTRAN_A

*--Processing RETLINE File

*--Copy RETHDR Files
lcTmpSFile = lcBOffDir + 'RETHDR.*'
lcTmpDFile = lcBackPath + 'RETHDR.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*--Copy RETLine Files
lcTmpSFile = lcBOffDir + 'RETLINE.*'
lcTmpDFile = lcBackPath + 'RETLINE.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- use invHDR and zap
USE lcBackPath+'RETHDR.DBF' ALIAS RETHDR_F EXCLUSIVE IN 0
SELECT RETHDR_F
ZAP


*-- use RETLINE and zap
USE lcBackPath+'RETLINE.DBF' ALIAS RETLINE_F EXCLUSIVE IN 0
SELECT RETLINE_F
ZAP


llS1Used = gfOpenfile(lcBOffDir + 'RETLINE','RETLINE','EX','RETLINE_A')
llS2Used = gfOpenfile(lcBOffDir + 'RETHDR','','EX','RETHDR_A')

*--ESTABLISH A RELATION BETWEEN invLine And invHdr
SELECT RETHDR_A

*--Add New Temp Index on RETHDR file
lcIndexF = lcBackPath+'FXINDEX.CDX'

INDEX ON CWARECODE TO &lcIndexF ADDITIVE
REINDEX

SET RELATION TO CRMEMO INTO RETLINE_A ADDI
SELECT RETHDR_A

*--COLLECT DATA FOR THE SELECTED W/H It should be very slow
IF SEEK(laWareCode[lnWareCD,2])
SCAN WHILE CWARECODE = laWareCode[lnWareCD,2]
    WAIT WINDOW 'Processing Return#' + RETHDR_A.invoice nowait
    SCATTER MEMVAR MEMO
    SELECT RETHDR_F
    APPEND BLANK
    GATHER MEMVAR MEMO
  SELECT RETLINE_A
  SCAN WHILE CRMEMO = RETHDR_A.CRMEMO
    WAIT WINDOW 'Processing Return#' + RETLINE_A.CRMEMO nowait
    SCATTER MEMVAR MEMO
    SELECT RETLINE_F
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDSCAN
ENDIF
*-- close invoice files
SELECT RETHDR_F
REINDEX

SELECT RETLINE_F
REINDEX

USE IN RETLine_A
USE IN RETHDR_A

USE IN RETHDR_F
USE IN RETLINE_F



*--Copy STYDYE Files
lcTmpSFile = lcBOffDir + 'STYDYE.*'
lcTmpDFile = lcBackPath + 'STYDYE.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing STYDYE File
WAIT WINDOW 'Processing STYDYE...' nowait
USE lcBackPath+'STYDYE.DBF' ALIAS STYDYE_F EXCLUSIVE IN 0
SELECT STYDYE_F
WAIT WINDOW 'Processing STYDYE...' nowait
ZAP
WAIT WINDOW 'Processing STYDYE...' nowait
USE (lcBOffDir + 'STYDYE') AGAIN ALIAS STYDYE_A IN 0 EXCL
SELECT STYDYE_A
*--Add New Temp Index on RETHDR file
lcIndexF = lcBackPath+'FXINDEX.CDX'

INDEX ON CWARECODE TO &lcIndexF ADDITIVE
REINDEX
IF SEEK(laWareCode[lnWareCD,2])
SCAN WHILE CWARECODE = laWareCode[lnWareCD,2]
  WAIT WINDOW 'Processing STYDYE...' nowait
  SCATTER MEMVAR MEMO
  SELECT STYDYE_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
ENDIF
SELECT STYDYE_F
REINDEX
USE IN STYDYE_F
USE IN STYDYE_A


*--Copy STYINVJL Files
lcTmpSFile = lcBOffDir + 'STYINVJL.*'
lcTmpDFile = lcBackPath + 'STYINVJL.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing STYINVJL File
WAIT WINDOW 'Processing STYINVJL...' nowait
USE lcBackPath+'STYINVJL.DBF' ALIAS STYINVJL_F EXCLUSIVE IN 0
SELECT STYINVJL_F
WAIT WINDOW 'Processing STYINVJL...' nowait
ZAP
WAIT WINDOW 'Processing STYINVJL...' nowait
USE (lcBOffDir + 'STYINVJL') AGAIN ALIAS STYINVJL_A IN 0 EXCL
SELECT STYINVJL_A
*--Add New Temp Index on STYINVJL file
lcIndexF = lcBackPath+'FXINDEX.CDX'
SELECT STYINVJL_A
INDEX ON CWARECODE TO &lcIndexF ADDITIVE
REINDEX
IF SEEK(laWareCode[lnWareCD,2])
SCAN WHILE CWARECODE = laWareCode[lnWareCD,2]
  WAIT WINDOW 'Processing STYINVJL...'+ ALLTRIM(STYLE) nowait
  SCATTER MEMVAR MEMO
  SELECT STYINVJL_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
ENDIF
SELECT STYINVJL_F
REINDEX
USE IN STYINVJL_F
USE IN STYINVJL_A


*--Processing PURCHASE ORDER Files

*--Copy POSHDR Files
lcTmpSFile = lcBOffDir + 'POSHDR.*'
lcTmpDFile = lcBackPath + 'POSHDR.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*--Copy InvLine Files
lcTmpSFile = lcBOffDir + 'POSLN.*'
lcTmpDFile = lcBackPath + 'POSLN.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

USE lcBackPath+'POSHDR.DBF' ALIAS POSHDR_F EXCLUSIVE IN 0
SELECT POSHDR_F
ZAP

*-- use POSLN and zap
USE lcBackPath+'POSLN.DBF' ALIAS POSLN_F EXCLUSIVE IN 0
SELECT POSLN_F
ZAP


llS1Used = gfOpenfile(lcBOffDir + 'POSLN','','EX','POSLN_A')
llS2Used = gfOpenfile(lcBOffDir + 'POSHDR','POSHDR','SH','POSHDR_A')

*--ESTABLISH A RELATION BETWEEN POSHDR And POSLN
SELECT POSLN_A
SET RELATION TO cstytype + po INTO POSHDR_A ADDI
SELECT POSLN_A

*--Add New Temp Index on POSLN file
lcIndexF = lcBackPath+'FXINDEX.CDX'

INDEX ON CWARECODE+po TO &lcIndexF ADDITIVE
REINDEX
*--COLLECT DATA FOR THE SELECTED W/H It should be very slow
GO TOP
lcPO = ''
IF SEEK(laWareCode[lnWareCD,2])
SCAN WHILE CWARECODE + PO = laWareCode[lnWareCD,2]
    IF lcPo <> POSLN_A.PO
      SELECT POSHDR_A
      SCATTER MEMVAR MEMO
      SELECT POSHDR_F
      APPEND BLANK
      GATHER MEMVAR MEMO
      *--Zero Out all costing fields tobe re-calculated
      REPLACE NICOST1    WITH 0,;
              NICOST2    WITH 0,;
              NICOST3    WITH 0,;
              NICOST4    WITH 0,;
              NICOST5    WITH 0,;
              POTOTAL    WITH 0,;
              NSTYORDER  WITH 0,;
              RECEIVE    WITH 0,;
              OPEN       WITH 0,;
              NLAN_COST1 WITH 0,;
              NLAN_COST2 WITH 0,;
              NLAN_COST3 WITH 0,;
              NLAN_COST4 WITH 0,;
              NLAN_COST5 WITH 0,;
              NTOT_COST WITH 0,;
              NFCOST1    WITH 0,;
              NFCOST2    WITH 0,;
              NFCOST3    WITH 0,;
              NFCOST4    WITH 0,;
              NFCOST5    WITH 0,;
              NFLANCOST1 WITH 0,;
              NFLANCOST2 WITH 0,;
              NFLANCOST3 WITH 0,;
              NFLANCOST4 WITH 0,;
              NFLANCOST5 WITH 0
      SELECT POSLN_A
    ENDIF
    lcPO = POSLN_A.PO
    WAIT WINDOW 'Processing PO#' + POSHDR_A.PO nowait
    SCATTER MEMVAR MEMO
    SELECT POSLN_F
    APPEND BLANK
    GATHER MEMVAR MEMO
    *--Rebuild The POSHDR File
    DO CASE
      CASE POSLN_A.TRANCD = '1'
        WAIT WINDOW 'Re-building PO#' + POSLN_A.PO + 'Header...' NOWAIT
        SELECT POSHDR_F
        =RLOCK()
        REPLACE NICOST1    WITH NICOST1 + (POSLN_A.NCOST1 * POSLN_A.TOTQTY) ,;
                NICOST2    WITH NICOST2 + (POSLN_A.NCOST2 * POSLN_A.TOTQTY) ,;
                NICOST3    WITH NICOST3 + (POSLN_A.NCOST3 * POSLN_A.TOTQTY) ,;
                NICOST4    WITH NICOST4 + (POSLN_A.NCOST4 * POSLN_A.TOTQTY) ,;
                NICOST5    WITH NICOST5 + (POSLN_A.NCOST5 * POSLN_A.TOTQTY) ,;
                POTOTAL    WITH POTOTAL + NICOST1 +NICOST2 +NICOST3 +NICOST4 +NICOST5,;
                NFCOST1    WITH NICOST1,;
                NFCOST2    WITH NICOST2,;
                NFCOST3    WITH NICOST3,;
                NFCOST4    WITH NICOST4,;
                NFCOST5    WITH NICOST5,;
                NSTYORDER  WITH NSTYORDER + POSLN_A.TOTQTY,;
                OPEN WITH NSTYORDER - RECEIVE
         UNLOCK  
      CASE POSLN_A.TRANCD = '2'
        SELECT POSHDR_F
        WAIT WINDOW 'Re-building PO#' + POSLN_A.PO + 'Receiving...' NOWAIT
        =RLOCK()
        REPLACE NLAN_COST1 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST2 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST3 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST4 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST5 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NTOT_COST WITH NTOT_COST + NLAN_COST1 + NLAN_COST2 + NLAN_COST3 + NLAN_COST4 + NLAN_COST5,;
                NFLANCOST1 WITH NLAN_COST1,;
                NFLANCOST2 WITH NLAN_COST2,;
                NFLANCOST3 WITH NLAN_COST3,;
                NFLANCOST4 WITH NLAN_COST4,;
                NFLANCOST5 WITH NLAN_COST5,;
                RECEIVE    WITH RECEIVE + POSLN_A.TOTQTY,;
                OPEN WITH NSTYORDER - RECEIVE
        UNLOCK
    ENDCASE
ENDSCAN
ENDIF


*--VENDOR
SELECT POSLN_A
INDEX ON VENDOR + PO TO &lcIndexF ADDITIVE
REINDEX
*--COLLECT DATA FOR THE SELECTED W/H It should be very slow
GO TOP
IF SEEK(laWareCode[lnWareCD,2])
lcPO = ''
SCAN WHILE  VENDOR+PO = laWareCode[lnWareCD,2]
    IF lcPo <> POSLN_A.PO
      SELECT POSHDR_A
      SCATTER MEMVAR MEMO
      SELECT POSHDR_F
      APPEND BLANK
      GATHER MEMVAR MEMO
      *--Zero Out all costing fields tobe re-calculated
      REPLACE NICOST1    WITH 0,;
              NICOST2    WITH 0,;
              NICOST3    WITH 0,;
              NICOST4    WITH 0,;
              NICOST5    WITH 0,;
              POTOTAL    WITH 0,;
              NSTYORDER  WITH 0,;
              RECEIVE    WITH 0,;
              OPEN       WITH 0,;
              NLAN_COST1 WITH 0,;
              NLAN_COST2 WITH 0,;
              NLAN_COST3 WITH 0,;
              NLAN_COST4 WITH 0,;
              NLAN_COST5 WITH 0,;
              NTOT_COST WITH 0,;
              NFCOST1    WITH 0,;
              NFCOST2    WITH 0,;
              NFCOST3    WITH 0,;
              NFCOST4    WITH 0,;
              NFCOST5    WITH 0,;
              NFLANCOST1 WITH 0,;
              NFLANCOST2 WITH 0,;
              NFLANCOST3 WITH 0,;
              NFLANCOST4 WITH 0,;
              NFLANCOST5 WITH 0
      SELECT POSLN_A
    ENDIF
    lcPO = POSLN_A.PO
    WAIT WINDOW 'Processing PO#' + POSHDR_A.PO nowait
    SCATTER MEMVAR MEMO
    SELECT POSLN_F
    APPEND BLANK
    GATHER MEMVAR MEMO
    *--Rebuild The POSHDR File
    DO CASE
      CASE POSLN_A.TRANCD = '1'
        WAIT WINDOW 'Re-building PO#' + POSLN_A.PO + 'Header...' NOWAIT
        SELECT POSHDR_F
        REPLACE NICOST1    WITH NICOST1 + (POSLN_A.NCOST1 * POSLN_A.TOTQTY) ,;
                NICOST2    WITH NICOST2 + (POSLN_A.NCOST2 * POSLN_A.TOTQTY) ,;
                NICOST3    WITH NICOST3 + (POSLN_A.NCOST3 * POSLN_A.TOTQTY) ,;
                NICOST4    WITH NICOST4 + (POSLN_A.NCOST4 * POSLN_A.TOTQTY) ,;
                NICOST5    WITH NICOST5 + (POSLN_A.NCOST5 * POSLN_A.TOTQTY)
        
        
        REPLACE POTOTAL    WITH POTOTAL + NICOST1 +NICOST2 +NICOST3 +NICOST4 +NICOST5
        WAIT WINDOW 'Re-building PO#' + POSLN_A.PO + 'Header...' NOWAIT
        
        REPLACE NFCOST1    WITH NICOST1,;
                NFCOST2    WITH NICOST2,;
                NFCOST3    WITH NICOST3,;
                NFCOST4    WITH NICOST4,;
                NFCOST5    WITH NICOST5,;
                NSTYORDER  WITH NSTYORDER + POSLN_A.TOTQTY
        
        REPLACE OPEN WITH NSTYORDER - RECEIVE

      CASE POSLN_A.TRANCD = '2'
        SELECT POSHDR_F
        WAIT WINDOW 'Re-building PO#' + POSLN_A.PO + 'Receiving...' NOWAIT
        REPLACE NLAN_COST1 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST2 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST3 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST4 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY),;
                NLAN_COST5 WITH (POSLN_A.NLAN_CST1 * POSLN_A.TOTQTY)
        
        REPLACE NTOT_COST WITH NTOT_COST + NLAN_COST1 + NLAN_COST2 + NLAN_COST3 + + NLAN_COST4 + + NLAN_COST5
        
        REPLACE NFLANCOST1 WITH NLAN_COST1,;
                NFLANCOST2 WITH NLAN_COST2,;
                NFLANCOST3 WITH NLAN_COST3,;
                NFLANCOST4 WITH NLAN_COST4,;
                NFLANCOST5 WITH NLAN_COST5,;
                RECEIVE    WITH RECEIVE + POSLN_A.TOTQTY
        
        REPLACE OPEN WITH NSTYORDER - RECEIVE
        
    ENDCASE
ENDSCAN
ENDIF


*-- close PO files
SELECT POSHDR_F
REINDEX

SELECT POSLN_F
REINDEX

USE IN POSLN_A
USE IN POSHDR_A

USE IN POSHDR_F
USE IN POSLN_F


*--Copy ORDHDR Files
lcTmpSFile = lcBOffDir + 'ORDHDR.*'
lcTmpDFile = lcBackPath + 'ORDHDR.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile


*-- use invHDR and zap
USE lcBackPath+'ORDHDR.DBF' ALIAS ORDHDR_F EXCLUSIVE IN 0
SELECT ORDHDR_F
ZAP

*--Copy InvLine Files
lcTmpSFile = lcBOffDir + 'ORDLine.*'
lcTmpDFile = lcBackPath + 'ORDLine.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- use invline and zap
USE lcBackPath+'ORDLINE.DBF' ALIAS ORDLINE_F EXCLUSIVE IN 0
SELECT ORDLINE_F
ZAP


llS1Used = gfOpenfile(lcBOffDir + 'ORDLine','ORDLINE','SH','ORDLine_A')
llS2Used = gfOpenfile(lcBOffDir + 'ORDHDR','','EX','ORDHDR_A')

*--ESTABLISH A RELATION BETWEEN invLine And invHdr
SELECT ORDHDR_A
*--Add New Temp Index on RETHDR file
lcIndexF = lcBackPath+'FXINDEX.CDX'

INDEX ON CWARECODE TO &lcIndexF ADDITIVE
REINDEX

SET RELATION TO CORDTYPE+ORDER INTO ORDLINE_A ADDI
SELECT ORDHDR_A


*--COLLECT DATA FOR THE SELECTED W/H It should be very slow
IF SEEK(laWareCode[lnWareCD,2])
SCAN WHILE CWARECODE = laWareCode[lnWareCD,2]
    WAIT WINDOW 'Processing Order#' + ORDHDR_A.Order nowait
    SCATTER MEMVAR MEMO
    SELECT ORDHDR_F
    APPEND BLANK
    GATHER MEMVAR MEMO
  SELECT ORDLINE_A
  SCAN WHILE CORDTYPE+order = ORDHDR_A.CORDTYPE+ORDHDR_A.ORDER
    WAIT WINDOW 'Processing ORDER #' + ORDLINE_A.order nowait
    SCATTER MEMVAR MEMO
    SELECT ORDLINE_F
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDSCAN
ENDIF
*-- close invoice files
SELECT ORDHDR_F
REINDEX

SELECT ORDLINE_F
REINDEX

USE IN ORDLine_A
USE IN ORDHDR_A

USE IN ORDHDR_F
USE IN ORDLINE_F

*###############################################################################################
* Category 1 [End]
*###############################################################################################
*-----------------------------------------------------------------------------------------------
*###############################################################################################
* Category 2 [Start]
*###############################################################################################

*--Copy APPAYMNT Files
lcTmpSFile = lcBOffDir + 'APPAYMNT.*'
lcTmpDFile = lcBackPath + 'APPAYMNT.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing APPAYMNT File
WAIT WINDOW 'Processing APPAYMNT...' nowait
USE lcBackPath+'APPAYMNT.DBF' ALIAS APPAYMNT_F EXCLUSIVE IN 0
SELECT APPAYMNT_F
WAIT WINDOW 'Processing APPAYMNT...' nowait
ZAP
WAIT WINDOW 'Processing APPAYMNT...' nowait
USE (lcBOffDir + 'APPAYMNT') AGAIN ALIAS APPAYMNT_A IN 0 SHARED
SELECT APPAYMNT_A
SCAN FOR COWNER = laWareCode[lnWareCD,3] OR EMPTY(COWNER)
  WAIT WINDOW 'Processing APPAYMNT...' NOWAIT
  SCATTER MEMVAR MEMO
  SELECT APPAYMNT_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT APPAYMNT_F
REINDEX
USE IN APPAYMNT_F
USE IN APPAYMNT_A


*--Copy ARCUSHST Files
lcTmpSFile = lcBOffDir + 'ARCUSHST.*'
lcTmpDFile = lcBackPath + 'ARCUSHST.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing ARCUSHST File
WAIT WINDOW 'Processing ARCUSHST...' nowait
USE lcBackPath+'ARCUSHST.DBF' ALIAS ARCUSHST_F EXCLUSIVE IN 0
SELECT ARCUSHST_F
WAIT WINDOW 'Processing ARCUSHST...' nowait
ZAP
WAIT WINDOW 'Processing ARCUSHST...' nowait
USE (lcBOffDir + 'ARCUSHST') AGAIN ALIAS ARCUSHST_A IN 0 SHARED
SELECT ARCUSHST_A

SCAN FOR COWNER = laWareCode[lnWareCD,3] OR EMPTY(COWNER)
  WAIT WINDOW 'Processing ARCUSHST...' nowait
  SCATTER MEMVAR MEMO
  SELECT ARCUSHST_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT ARCUSHST_F
REINDEX
USE IN ARCUSHST_F
USE IN ARCUSHST_A


*--Copy ARHST Files
lcTmpSFile = lcBOffDir + 'ARHIST.*'
lcTmpDFile = lcBackPath + 'ARHIST.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing ARHST File
WAIT WINDOW 'Processing ARHST...' nowait
USE lcBackPath+'ARHIST.DBF' ALIAS ARHST_F EXCLUSIVE IN 0
SELECT ARHST_F
WAIT WINDOW 'Processing ARHST...' nowait
ZAP
WAIT WINDOW 'Processing ARHST...' nowait
USE (lcBOffDir + 'ARHIST') AGAIN ALIAS ARHST_A IN 0 SHARED
SELECT ARHST_A

SCAN FOR COWNER = laWareCode[lnWareCD,3] OR EMPTY(COWNER)
  WAIT WINDOW 'Processing ARHST...' nowait
  SCATTER MEMVAR MEMO
  SELECT ARHST_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT ARHST_F
REINDEX
USE IN ARHST_F
USE IN ARHST_A

*--Copy CREDIT Files
lcTmpSFile = lcBOffDir + 'CREDIT.*'
lcTmpDFile = lcBackPath + 'CREDIT.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing CREDIT File
WAIT WINDOW 'Processing CREDIT...' nowait
USE lcBackPath+'CREDIT.DBF' ALIAS CREDIT_F EXCLUSIVE IN 0
SELECT CREDIT_F
WAIT WINDOW 'Processing CREDIT...' nowait
ZAP
WAIT WINDOW 'Processing CREDIT...' nowait
USE (lcBOffDir + 'CREDIT') AGAIN ALIAS CREDIT_A IN 0 SHARED
SELECT CREDIT_A

SCAN FOR COWNER = laWareCode[lnWareCD,3] OR EMPTY(COWNER)
  WAIT WINDOW 'Processing CREDIT...' + ACCOUNT nowait
  SCATTER MEMVAR MEMO
  SELECT CREDIT_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT CREDIT_F
REINDEX
USE IN CREDIT_F
USE IN CREDIT_A


*--Copy DEBIT Files
lcTmpSFile = lcBOffDir + 'DEBIT.*'
lcTmpDFile = lcBackPath + 'DEBIT.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing DEBIT File
WAIT WINDOW 'Processing DEBIT...' nowait
USE lcBackPath+'DEBIT.DBF' ALIAS DEBIT_F EXCLUSIVE IN 0
SELECT DEBIT_F
WAIT WINDOW 'Processing DEBIT...' nowait
ZAP
WAIT WINDOW 'Processing DEBIT...' nowait
USE (lcBOffDir + 'DEBIT') AGAIN ALIAS DEBIT_A IN 0 SHARED
SELECT DEBIT_A

SCAN FOR COWNER = laWareCode[lnWareCD,3] OR EMPTY(COWNER)
  WAIT WINDOW 'Processing DEBIT...' + ACCOUNT nowait
  SCATTER MEMVAR MEMO
  SELECT DEBIT_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT DEBIT_F
REINDEX
USE IN DEBIT_F
USE IN DEBIT_A

*--Copy DEBIT Files
lcTmpSFile = lcBOffDir + 'CUSTOMER.*'
lcTmpDFile = lcBackPath + 'CUSTOMER.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile

*-- Processing CUSTOMER File
WAIT WINDOW 'Processing CUSTOMER...' nowait
USE lcBackPath+'CUSTOMER.DBF' ALIAS CUSTOMER_F EXCLUSIVE IN 0
SELECT CUSTOMER_F
WAIT WINDOW 'Processing CUSTOMER...' nowait
ZAP
WAIT WINDOW 'Processing CUSTOMER...' nowait
USE (lcBOffDir + 'CUSTOMER') AGAIN ALIAS CUSTOMER_A IN 0 SHARED
SELECT CUSTOMER_A

SCAN FOR COWNER = laWareCode[lnWareCD,3] OR EMPTY(COWNER) OR ACCOUNT = 'MISC1'
  WAIT WINDOW 'Processing CUSTOMER...' + ACCOUNT nowait
  SCATTER MEMVAR MEMO
  SELECT CUSTOMER_F
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT CUSTOMER_F
REINDEX
USE IN CUSTOMER_F
USE IN CUSTOMER_A

*###############################################################################################
* Category 2 [End]
*###############################################################################################
*-----------------------------------------------------------------------------------------------
*###############################################################################################
* Category 3 [Start]
*###############################################################################################
*--Copy BOMLINE Files
WAIT WINDOW 'Processing BOMLINE...' nowait
lcTmpSFile = lcBOffDir + 'BOMLINE.*'
lcTmpDFile = lcBackPath + 'BOMLINE.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'BOMLINE.DBF' ALIAS BOMLINE_F EXCLUSIVE IN 0
SELECT BOMLINE_F
REINDEX
USE IN BOMLINE_F

*--Copy CODES Files
WAIT WINDOW 'Processing CODES...' nowait
lcTmpSFile = lcBOffDir + 'CODES.*'
lcTmpDFile = lcBackPath + 'CODES.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'CODES.DBF' ALIAS CODES_F EXCLUSIVE IN 0
SELECT CODES_F
REINDEX
USE IN CODES_F


*--Copy FABRIC Files
WAIT WINDOW 'Processing FABRIC...' nowait
lcTmpSFile = lcBOffDir + 'FABRIC.*'
lcTmpDFile = lcBackPath + 'FABRIC.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'FABRIC.DBF' ALIAS FABRIC_F EXCLUSIVE IN 0
SELECT FABRIC_F
REINDEX
USE IN FABRIC_F

*--Copy GLDIST Files
WAIT WINDOW 'Processing GLDIST...' nowait
lcTmpSFile = lcBOffDir + 'GLDIST.*'
lcTmpDFile = lcBackPath + 'GLDIST.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'GLDIST.DBF' ALIAS GLDIST_F EXCLUSIVE IN 0
SELECT GLDIST_F
REINDEX
USE IN GLDIST_F

*--Copy ICISTRU Files
WAIT WINDOW 'Processing ICISTRU...' nowait
lcTmpSFile = lcBOffDir + 'ICISTRU.*'
lcTmpDFile = lcBackPath + 'ICISTRU.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'ICISTRU.DBF' ALIAS ICISTRU_F EXCLUSIVE IN 0
SELECT ICISTRU_F
REINDEX
USE IN ICISTRU_F

*--Copy ICSEGVAL Files
WAIT WINDOW 'Processing ICSEGVAL...' nowait
lcTmpSFile = lcBOffDir + 'ICSEGVAL.*'
lcTmpDFile = lcBackPath + 'ICSEGVAL.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'ICSEGVAL.DBF' ALIAS ICSEGVAL_F EXCLUSIVE IN 0
SELECT ICSEGVAL_F
REINDEX
USE IN ICSEGVAL_F


*--Copy ICSTYHST Files
WAIT WINDOW 'Processing ICSTYHST...' nowait
lcTmpSFile = lcBOffDir + 'ICSTYHST.*'
lcTmpDFile = lcBackPath + 'ICSTYHST.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'ICSTYHST.DBF' ALIAS ICSTYHST_F EXCLUSIVE IN 0
SELECT ICSTYHST_F
REINDEX
USE IN ICSTYHST_F

*--Copy STYLE Files
WAIT WINDOW 'Processing STYLE...' nowait
lcTmpSFile = lcBOffDir + 'STYLE.*'
lcTmpDFile = lcBackPath + 'STYLE.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'STYLE.DBF' ALIAS STYLE_F EXCLUSIVE IN 0
SELECT STYLE_F
REINDEX
USE IN STYLE_F

*--Copy WAREHOUS Files
WAIT WINDOW 'Processing WAREHOUS...' nowait
lcTmpSFile = lcBOffDir + 'WAREHOUS.*'
lcTmpDFile = lcBackPath + 'WAREHOUS.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'WAREHOUS.DBF' ALIAS WAREHOUS_F EXCLUSIVE IN 0
SELECT WAREHOUS_F
REINDEX
USE IN WAREHOUS_F

*--Copy WHSLOC Files
WAIT WINDOW 'Processing WHSLOC...' nowait
lcTmpSFile = lcBOffDir + 'WHSLOC.*'
lcTmpDFile = lcBackPath + 'WHSLOC.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'WHSLOC.DBF' ALIAS WHSLOC_F EXCLUSIVE IN 0
SELECT WHSLOC_F
REINDEX
USE IN WHSLOC_F

*--Copy SCALE Files
WAIT WINDOW 'Processing SCALE...' nowait
lcTmpSFile = lcBOffDir + 'SCALE.*'
lcTmpDFile = lcBackPath + 'SCALE.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'SCALE.DBF' ALIAS SCALE_F EXCLUSIVE IN 0
SELECT SCALE_F
REINDEX
USE IN SCALE_F

*--Copy SCALEHD Files
WAIT WINDOW 'Processing SCALEHD...' nowait
lcTmpSFile = lcBOffDir + 'SCALEHD.*'
lcTmpDFile = lcBackPath + 'SCALEHD.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'SCALEHD.DBF' ALIAS SCALEHD_F EXCLUSIVE IN 0
SELECT SCALEHD_F
REINDEX
USE IN SCALEHD_F

*--Copy SETUPS Files
WAIT WINDOW 'Processing SETUPS...' nowait
lcTmpSFile = lcBOffDir + 'SETUPS.*'
lcTmpDFile = lcBackPath + 'SETUPS.*'
! /7 XCOPY &lcTmpSFile &lcTmpDFile
USE lcBackPath + 'SETUPS.DBF' ALIAS SETUPS_F EXCLUSIVE IN 0
SELECT SETUPS_F
REINDEX
SET ORDER TO TAG VARNAME

*-- Make System type = point of sale
WAIT WINDOW 'Updating Point of sale record' nowait
IF SEEK('M_SYSTYPE ','SETUPS_F')
  REPLACE MDATA_DEF WITH 'P'
ENDIF
USE IN SETUPS_F

**Styinvjl
*lcBOffDir
WAIT CLEAR











*------------------------------------------------------------------------------------------------
