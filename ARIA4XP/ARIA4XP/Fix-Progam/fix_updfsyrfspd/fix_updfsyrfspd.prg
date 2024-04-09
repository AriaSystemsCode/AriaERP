*******************************************************************************************
*  this is a fix program related to the fix E303001,[T20110818.0027]
* Fix description change the value stored in APINVHDR.CFISFYEAR, CFSPPRDID to be the post date instead of invoice date [ ] 
* Developer : TMI Tarek Mohamed Ibrahim
* Application : aria4xp, aria27
* this fix should be copied to the shared folder to loop over all the clients in CLIENTS table
*******************************************************************************************
CLOSE ALL
SET DELETED ON
IF FILE('CLIENTS.DBF')
  USE CLIENTS
  SCAN
    lcAppPath = ADDBS(ALLTRIM(clients.cDataPath))
    SELECT 0
    =lfUpdateCompany(lcAppPath)
  ENDSCAN
  
ELSE  && not a SaaS structure 
  lcAppPath = GETDIR('','Get the ARIA27 root folder')
  IF EMPTY(lcAppPath)
    MESSAGEBOX('No updates performed..',0,'Aria Systems')
    RETURN
  ENDIF
  =lfUpdateCompany(lcAppPath)
ENDIF


********************************************************************************************************************
*   FUNCTION lfUpdateCompany
********************************************************************************************************************
FUNCTION lfUpdateCompany
PARAMETERS lcAppPath
LOCAL lnSlct
lnSlct = SELECT(0)
SELECT 0
USE (lcAppPath+'sysfiles\SYCCOMP')
SCAN 
  lcDbf = lcAppPath+'dbfs\'+SYCCOMP.cComp_id+'\'
  =lfUpdateAPInvHdr(lcDbf)
ENDSCAN
USE IN SYCCOMP
SELECT (lnSlct)
*- End of function lfUpdateCompany

********************************************************************************************************************
*   FUNCTION lfUpdateAPInvHdr
********************************************************************************************************************
FUNCTION lfUpdateAPInvHdr
PARAMETERS lcDbf
LOCAL lnSlct
lnSlct = SELECT(0)
USE (lcDbf+'APINVHDR') IN 0
USE (lcDbf+'FSPRD') IN 0
SELECT 'APINVHDR'
SCAN FOR !EMPTY(DPOSTDATE )
  SELECT FSPRD
  WAIT WINDOW NOWAIT lcDbf + CHR(13) + apinvhdr.cvendcode+' '+apinvhdr.cinvno
  LOCATE FOR BETWEEN(APINVHDR.DPOSTDATE,FSPRD.DFSPPBGDT,FSPRD.DFSPPENDT)
  IF FOUND() AND (APINVHDR.CFISFYEAR <> FSPRD.CFISFYEAR OR APINVHDR.CFSPPRDID <> FSPRD.CFSPPRDID )
    SELECT APINVHDR
    REPLACE CFISFYEAR WITH FSPRD.CFISFYEAR ;
            CFSPPRDID WITH FSPRD.CFSPPRDID 
  ENDIF
ENDSCAN

USE IN 'APINVHDR'
USE IN 'FSPRD'

SELECT (lnSlct)
*- End of lfUpdateAPInvHdr.
