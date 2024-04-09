***********************************************************************
*:  Program file : SOSHLBGM.PRG
*:  Program desc.: Custom Shipping Label form GM
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 02/05/2014
*:      Reference: C201602.Exe[T20131219.0020 - Upgrade to R13 at GMA]
**************************************************************************
* Modifications:
*:************************************************************************
*!*************************************************************
*! Name      : lfbringstr
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 02/05/2014
*! Purpose   : Fill String lcStr
*!*************************************************************
FUNCTION lfbringstr
PARAMETER lcdummy
PRIVATE lcshipstr, lcpostr
lcstr = ''
IF lcxtype = 'I'
     = SEEK(invoice, 'INVHDR')
     lcshipstr = gfcoddes(invhdr.shipvia,'SHIPVIA',.T.)
     lcstr = invhdr.dept + '     ' + invhdr.custpo + SPACE(5) + store + ' '
ELSE
     IF '*' $ ordhdr.shipvia
          lcshipstr = customer.shipvia
     ELSE
          lcshipstr = ordhdr.shipvia
     ENDIF
     lcshipstr = gfcoddes(lcshipstr,'SHIPVIA',.T.)
     IF ordhdr.multipo
          lcpostr = '*Multi_PO*     '
     ELSE
          lcpostr = ordhdr.custpo
     ENDIF
     lcstr = ordhdr.dept + '     ' + lcpostr + SPACE(6)
ENDIF
lcstr = lcstr + lcshipstr
RETURN ''
