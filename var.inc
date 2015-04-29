cataloc     =   '/home/ast3/disk1/nju/catalog/'
masterfile  =   '/home/ast3/disk1/nju/catalog/lc/master.cat'
lcbinfile   =   '/home/ast3/disk1/nju/catalog/lc/lcbin.cat' ;defocused star spot
lcloc       =   '/home/ast3/disk1/nju/catalog/lc/'  
dateloc     =   [24,29,32,35,38,41] ; [YY, MM, DD, HH, MM,SS] ;2013-09-18
tfloc		=	'/home/ast3/disk1/nju/'


master_def  =   0  ; 1 -> master file pre-exist and will be used.
lcbin_def   =   0  ; 1 -> lcbin file already created and will be loaded.
sexcol		=	[4,5,6,7,8,9,20]  ; [ra, dec, ravar, decvar, mag, magerr, flags] column in catalog file
dr 			=	10D0   ; Matching radius careful in crowded fields
mfreq		=	1D0    ; How common should each star appear in all frames

;dateloc=dateloc+STRLEN(cataloc)-1LL

;# obs. start time(UT) : 2015-04-07T16:37:42.3712 | exposure time(s) : 40.00 | mag_zp: 24.51  | Av:   1.7
;#HATP37_130918_V+N_121910

