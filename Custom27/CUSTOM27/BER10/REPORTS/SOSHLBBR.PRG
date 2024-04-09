*:************************************************************************
*: Program file  : SOSHLBBR
*: Program desc. : Customized Shipping Labels Report for Berkley Shirts
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO,AR,AL
*! Developer     : Sameh Saiid Ezzat (SSE)
*! Date          : 05/24/2000
*: Reference     : C101790
*:************************************************************************
*C101790,1 SSE 05/24/2000 Custom Shipping labels for Berkley Shirts.
*:************************************************************************
*

PRIVATE lcAlias
lcAlias = ALIAS()

SELECT CUSTOMER
SET RELATION TO 'S'+Account+Dist_Ctr INTO (lcCust_A) ADDITIVE

*-- If Program is running form Account receivable module
IF gcAct_Appl = "AR"

  SELECT (lcLblTbl) && Temp File for Label Information used in Label Screen
  SCAN
    FOR lnI = nFrom TO nTo
      SCATTER MEMVAR MEMO
      m.nLblsNo = lnI
      INSERT INTO (lcLblInfo) FROM MEMVAR
    ENDFOR
  ENDSCAN

  SELECT (lcMainF)
  SET RELATION TO Invoice + STR(nLblsNo,4) INTO (lcLblInfo) ADDITIVE
ENDIF
SELECT (lcAlias)
*-- End of Optional Program.

*!**************************************************************************
*! Name      : lfgetAdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : get Customer Address and Label no
*!**************************************************************************
*! Called from : SOSHLBBR.LBX
*!**************************************************************************
*
FUNCTION lfgetAdr
PARAMETERS lcDummy

IF gcAct_Appl <> 'AL'

  *-- Redimension laDivLName array which is defined in the FormSetting field
  DIMENSION laDivLName[1,2]
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'
  = gfRltFld(&lcMainF..CDIVISION , @laDivLName , 'CDIVISION')
ENDIF  

lcDivLName = IIF(!EMPTY(lcDivLName),lcDivLName,SUBSTR(gcCompName,1,29))
lcCustAdd2 = IIF(!EOF(lcCust_A),IIF(!EMPTY(&lcCust_A..cAddress2),;
             &lcCust_A..cAddress2,&lcCust_A..cAddress3+' '+;
             &lcCust_A..cAddress4+' '+&lcCust_A..cAddress5),;
             IIF(!EMPT(Customer.cAddress2),Customer.cAddress2,;
             Customer.cAddress3+' '+Customer.cAddress4+' '+;
             Customer.cAddress5))
lcCustAdd3 = IIF(!EOF(lcCust_A),IIF(!EMPT(&lcCust_A..cAddress2),;
             &lcCust_A..cAddress3+' '+&lcCust_A..cAddress4+' '+;
             &lcCust_A..cAddress5,''),IIF(!EMPT(Customer.cAddress2),;
             Customer.cAddress3+' '+Customer.cAddress4+' '+;
             Customer.cAddress5,''))

RETURN lcDummy
*-- End of lfgetAdr.
