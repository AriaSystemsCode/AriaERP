
set step on 
set safe off
close all
cd (justpath(sys(16)))
set excl off
use syustatc in 0
use clients in 0
erase err.txt
lcCr = chr(13)+chr(10)

select clients
scan
  lcStatcFl = addbs(allt(cdatapath))+'sysfiles\syustatc'
  llErr = .F.
  on error llErr = .T.
  use (lcStatcFl) in 0 alias static
  on error
  if !llErr 
    use in static
  else
    llErr = .F.
    on error llErr = .T.
    select SYUSTATC
    *COPY FILE SYUSTATC.* TO (+'.*')
    *copy stru to (lcStatcFl) with cdx fox2x
    copy to (lcstatcfl) with cdx fox2x
    if llErr
      strtofile('The file '+lcStatcFl+' can not be fixed'+lcCr,'err.txt',1)
    endif 
  endif     
endscan
if file('err.txt')
  modi file err.txt
else  
  MESSAGEBOX('static files fixed successfully')
endif   
on error