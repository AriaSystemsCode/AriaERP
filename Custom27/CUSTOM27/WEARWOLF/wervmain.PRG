PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

FUNCTION lfCHKORDINV 

LOCAL lcRetAlias ,llOrdInvoiced 
lcRetAlias =Alias()
lcOrdNotInv = oGetTempName.DO()
lcOrdNotInvFox2x = oGetTempName.DO()
CREATE TABLE (oAriaApplication.WorkDir+lcOrdnotinv) (ORDER C(6),Account c(6),Stname c(50),Cstatus c(1),custpo c(30),dept c(5),store c(8),Start d,Complete d)
Copy To (oAriaApplication.WorkDir+lcOrdNotInvFox2x) Fox2x
Use (oAriaApplication.WorkDir+lcOrdNotInvFox2x)
ERASE (oAriaApplication.WorkDir+lcOrdnotinv+".DBF")
IF !USED("INVHDR")
   SELECT 0
   Use INVHDR
   SET ORDER TO INVHDRA
ENDIF
IF !USED("BOL_HDR")
   SELECT 0
   USE BOL_HDR
ENDIF 
=SEEK(allt(lcBol_no),"BOL_HDR","BOL_HDR")
=SEEK('A'+BOL_HDR.ACCOUNT,"EDIACPRT","ACCFACT")

IF EDIACPRT.Cpartcode # 'FMGBON'
   RETURN
ENDIF
IF !USED("BOL_LIN")
   SELECT 0
   USE BOL_LIN
ENDIF

IF !SEEK(allt(lcBol_no),"BOL_LIN","BOL_LIN")
   =MESSAGEBOX('Empty BOL lines not allowed')
   RETURN(.F.)
ENDIF
Select bol_lin
SCAN REST WHILE Bol_no = allt(lcBol_no)
   llOrdInvoiced = .F.
   SELECT INVHDR
   SEEK(BOL_HDR.Account)
   SCAN REST WHILE Account = BOL_HDR.Account
      IF ORDER = BOL_LIN.ORDER
         llOrdInvoiced = .T.
         EXIT
      ENDIF
   ENDSCAN
   IF !llOrdInvoiced
      =SEEK("O"+BOL_LIN.ORDER,"ORDHDR","ORDHDR")
      =SEEK("M"+BOL_HDR.ACCOUNT,"Customer","Customer")
      INSERT INTO (lcOrdNotInvFox2x) (Order,Account,Stname,Cstatus,custpo,dept,store,Start,Complete);
      				     VALUES (BOL_LIN.ORDER,customer.account,customer.stname,ordhdr.status,ordhdr.custpo,ordhdr.dept,;
      				             Ordhdr.store,ordhdr.start,ordhdr.complete)
   ENDIF 
ENDSCAN
SELECT (lcOrdNotInvFox2x)
lcReportData = lcOrdNotInvFox2x+'.Dbf'
GO TOP
IF !EOF()
   llordinvoice = .F.
ENDIF

*:**************************************************************************
*:* Name        : lfAutOutArcv
*:* Developer   : AAH - ABDELAZIZ AWADALLAH
*:* Date        : 11/22/2005
*:* Purpose     : ADD NEW CONTROL INTO NETWORK SCREEN TO AUTOMATIC ARCHIVING OF OUTGOING FILES
*:***************************************************************************
*:* Called from : EBNTSET.SCX,SENDTRANSACTIONS,GETOUTGOING
*:***************************************************************************
*C130147

FUNCTION lfAutOutArcv
RETURN .T.

*:***************************************************************************