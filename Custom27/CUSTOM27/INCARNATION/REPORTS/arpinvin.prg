llTax     = (gfGetMemVar('M_TAX') = 'Y')
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
lcTaxMeth = gfGetMemVar('M_TAX_METH')

=SEEK (gcAct_Comp,'SYCCOMP')
lcCompFax  = SYCCOMP.cCom_Fax      && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcDuns     = gfGetMemVar('XDUNS')
