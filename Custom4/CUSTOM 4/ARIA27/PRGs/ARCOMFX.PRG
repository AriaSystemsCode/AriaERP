*:**************************************************************************
*: Program file  : ARCommFx
*: Program desc. : Fix program to fix wrong comm in RepComm.
*: System        : Aria Advantage Series.
*: Module        : System Manager (SM)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 09/17/2002
*: Reference     : B606165,1
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*
PARAMETERS lcDataDir

PRIVATE lcDataDir
lcDataDir = ""
lcDataDir = GETDIR('','Select Data Directory')

IF EMPTY(lcDataDir)
  RETURN
ENDIF

PRIVATE llFilsExst
llFilsExst = .F.
llFilsExst = FILE(lcDataDir+'INVHDR.DBF') AND FILE(lcDataDir+'REPCOMM.DBF') AND FILE(lcDataDir+'SALESREP.DBF')

IF !SUBSTR (lcDataDir,LEN(lcDataDir))='\'
  lcDataDir = lcDataDir+'\'
ENDIF
  
IF llFilsExst
  *-- Update RepComm file.
  WAIT WINDOW 'Start updating....' NOWAIT

  *-- Create Difference table ,This table will record all the invoice that has 2nd sales rep
  *-- commission amount difference (Commission Amount updated with 1st Rep one)
  PRIVATE lnHandle
  lnHandle = FCREATE(lcDataDir+'COMM2DIF.TXT')
  IF lnHandle < 0  
    WAIT WINDOW 'Cannot create Commission 2 differences text file, Program will terminate.'
    =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot create Commission 2 differences text file, Program will terminate.')
    RETURN
  ENDIF
    
  *-- Open InvHdr.
  IF USED('InvHdr')
    USE IN InvHdr
  ENDIF  
  USE (lcDataDir+'InvHdr') IN 0 ORDER TAG InvHdr
  
  *-- Open SalesRep.
  IF USED('SalesRep')
    USE IN SalesRep
  ENDIF  
  USE (lcDataDir+'SalesRep') IN 0 ORDER TAG SalesRep

  *-- Open RepComm.
  IF USED('RepComm')
    USE IN RepComm
  ENDIF  
  USE (lcDataDir+'RepComm') IN 0 ORDER TAG RepComm
  
  *-- Start fixing here in this scan.
  PRIVATE lcUntSin , lcExRSin , lnCommAmt2 , lnOldAmt2 , llChanged
  llChanged = .F.
  SELECT InvHdr
  SCAN 
  
   IF !EMPTY(Rep2) 
   	=lfChkComm(2)
   ENDIF 
   SELECT InvHdr
   IF !EMPTY(Rep1) 
   	=lfChkComm(1)
   ENDIF 
   SELECT InvHdr
  ENDSCAN 
  *-- If text 
  IF llChanged
    =gfModalGen("TRM000000B00000","DIALOG",'','','Finished updating.')
    =gfModalGen("TRM000000B00000","DIALOG",'','','All changes are saved as a text file in ' + gcDataDir + 'COMM2DIF.TXT')

    =FFLUSH(lnHandle)
    CREATE CURSOR TMPSTR (mStrRep M(10))
    APPEND BLANK
    APPEND MEMO mStrRep FROM (gcDataDir+"COMM2DIF.txt") OVERWRITE
    *DO (gcScrDir + gcWinAppl + '\SMSTRREP.SPR')
    DO (gcScrDir + 'ARCOMFX.SPR')
    USE IN TMPSTR
  ENDIF

  *-- Close the text file 
  =FCLOSE(lnHandle)
  
  *--Close files
  IF USED('InvHdr')
    USE IN InvHdr
  ENDIF

  IF USED('SalesRep')
    USE IN SalesRep
  ENDIF

  IF USED('RepComm')
    USE IN RepComm
  ENDIF
  WAIT CLEAR  
ELSE
  WAIT WINDOW 'Wrong data directory..' TIMEOUT 3
ENDIF
*--End of ARCommFx.

*:**************************************************************************
*! Name      : lfPrnt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/22/2002
*! Purpose   : Print the 2nd Rep commission difference report.
*:**************************************************************************
*! Example   : = lfPrnt()
*:**************************************************************************
*
FUNCTION lfvPrnt
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF
*-- End of lfvPrnt.





*:**************************************************************************
* FUNCTION lfvChkComm 
*:**************************************************************************
FUNCTION lfChkComm 
PARAM lcsalesrep
    lcUntSin = ''      
    lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)      && Exchange Rate sign "/" Or "*"
    lnCommAmt2 = ROUND(IIF(lcsalesrep=2,InvHdr.CommAmt2,InvHdr.CommAmt1) &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit,2)

    IF lnCommAmt2 = 0 
      RETURN 
    ENDIF 
    
    IF SEEK(IIF(lcsalesrep=2,InvHdr.Rep2,InvHdr.Rep1)+DTOS(InvHdr.InvDate)+InvHdr.Invoice+'1','RepComm') AND int(RepComm.Amount) <> int(lnCommAmt2)
    
      llChanged = .T.
      SELECT RepComm 
      lnOldAmt2 = Amount
      =RLOCK()
      REPLACE Amount WITH lnCommAmt2
      UNLOCK
      
      IF SEEK(RepCode,'SalesRep')
        SELECT SalesRep
        =RLOCK()
        REPLACE Current WITH Current + (lnOldAmt2 - lnCommAmt2) , ;
                Balance WITH Balance + (lnOldAmt2 - lnCommAmt2)
        UNLOCK
      ENDIF
      IF lcsalesrep=2
      =FPUTS(lnHandle,"2nd Rep. Code : " + InvHdr.Rep2 + " in Invoice # " + InvHdr.Invoice + " has old commission amount = " ;
             + ALLTRIM(STR(lnOldAmt2,14,2)) + " , the new updated commission amount = " ;
             + ALLTRIM(STR(lnCommAmt2,14,2)) + " with difference = " ;
             + ALLTRIM(STR(lnOldAmt2 - lnCommAmt2,14,2)) )
      =FPUTS(lnHandle,REPLICATE('-',68))
      
      ELSE
            =FPUTS(lnHandle,"1st Rep. Code : " + InvHdr.Rep1 + " in Invoice # " + InvHdr.Invoice + " has old commission amount = " ;
             + ALLTRIM(STR(lnOldAmt2,14,2)) + " , the new updated commission amount = " ;
             + ALLTRIM(STR(lnCommAmt2,14,2)) + " with difference = " ;
             + ALLTRIM(STR(lnOldAmt2 - lnCommAmt2,14,2)) )
             =FPUTS(lnHandle,REPLICATE('-',68))
      ENDIF  
    ELSE
     *Amin
     IF !SEEK(IIF(lcsalesrep=2,InvHdr.Rep2,InvHdr.Rep1)+DTOS(InvHdr.InvDate)+InvHdr.Invoice+'1','RepComm')  .AND.  lnCommAmt2 <> 0 
        llChanged = .T.
     	SELECT REPCOMM
        APPEND BLANK
        =RLOCK()
        REPLACE REPCODE   WITH IIF(lcsalesrep=2,InvHdr.Rep2,InvHdr.Rep1),;
          TRANTYPE  WITH '1'         ,;
          STATUS    WITH 'O'         ,;
          TRAN      WITH InvHdr.Invoice   ,;
          Account   WITH InvHdr.Account   ,;
          ORDER     WITH InvHdr.Order     ,;
          CustPO    WITH InvHdr.CustPO    ,;
          STORE     WITH InvHdr.Store     ,;
          DATE      WITH InvHdr.InvDate   ,;
          DESC      WITH 'INVOICE'        ,;
          AMOUNT    WITH lnCommAmt2       ,;
          COMMPCNT  WITH IIF(lcsalesrep=2,InvHdr.Comm2,InvHdr.Comm1)     ,;
          nForAmnt  WITH IIF(lcsalesrep=2,InvHdr.COMMAMT2,InvHdr.COMMAMT1)    ,;
          cCurrCode WITH InvHdr.cCurrCode ,;
          nCurrUnit WITH InvHdr.nCurrUnit ,;
          nExRate   WITH InvHdr.nExRate   ,;
          dAdd_Date WITH gdSysDate   ,;
          cAdd_Time WITH TIME()      ,;
          cAdd_User WITH gcUser_id
          UNLOCK
		
		lnOldAmt2 = REPCOMM.Amount          
          
      IF SEEK(RepCode,'SalesRep')
        SELECT SalesRep
        =RLOCK()
        REPLACE Current WITH Current +lnCommAmt2 ,;
                Balance WITH Balance +lnCommAmt2
        UNLOCK
      ENDIF
      IF lcsalesrep=2
      =FPUTS(lnHandle,"2nd Rep. Code : " + InvHdr.Rep2 + " in Invoice # " + InvHdr.Invoice + " has been added to commission file with amount = " ;
             + ALLTRIM(STR(lnOldAmt2,14,2)))
      =FPUTS(lnHandle,REPLICATE('-',68))
      
      ELSE
      
            =FPUTS(lnHandle,"1st Rep. Code : " + InvHdr.Rep1 + " in Invoice # " + InvHdr.Invoice + " has been added to commission file with amount = " ;
             + ALLTRIM(STR(lnOldAmt2,14,2)))
      =FPUTS(lnHandle,REPLICATE('-',68))

            *=FPUTS(lnHandle,"new 1st Rep. Code : " + InvHdr.Rep1 + " in Invoice # " + InvHdr.Invoice + " has old commission amount = " ;
            * + ALLTRIM(STR(lnOldAmt2,14,2)) + " , the new updated commission amount = " ;
            * + ALLTRIM(STR(lnCommAmt2,14,2)) + " with difference = " ;
            * + ALLTRIM(STR(lnOldAmt2 - lnCommAmt2,14,2)) )
			*=FPUTS(lnHandle,REPLICATE('-',68))             
      ENDIF  
	  
     ENDIF 
     *Amin  
    ENDIF

    
