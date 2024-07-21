       01  BAT0012I.                                                            
           02  FILLER PIC X(12).                                                
           02  E3USERL    COMP  PIC  S9(4).                                     
           02  E3USERF    PICTURE X.                                            
           02  FILLER REDEFINES E3USERF.                                        
             03 E3USERA    PICTURE X.                                           
           02  E3USERI  PIC X(7).                                               
           02  A3NAMEL    COMP  PIC  S9(4).                                     
           02  A3NAMEF    PICTURE X.                                            
           02  FILLER REDEFINES A3NAMEF.                                        
             03 A3NAMEA    PICTURE X.                                           
           02  A3NAMEI  PIC X(20).                                              
           02  E3ABTL    COMP  PIC  S9(4).                                      
           02  E3ABTF    PICTURE X.                                             
           02  FILLER REDEFINES E3ABTF.                                         
             03 E3ABTA    PICTURE X.                                            
           02  E3ABTI  PIC X(5).                                                
           02  E3VBNRL    COMP  PIC  S9(4).                                     
           02  E3VBNRF    PICTURE X.                                            
           02  FILLER REDEFINES E3VBNRF.                                        
             03 E3VBNRA    PICTURE X.                                           
           02  E3VBNRI  PIC X(7).                                               
           02  E3STATL    COMP  PIC  S9(4).                                     
           02  E3STATF    PICTURE X.                                            
           02  FILLER REDEFINES E3STATF.                                        
             03 E3STATA    PICTURE X.                                           
           02  E3STATI  PIC X(1).                                               
           02  E3ANWL    COMP  PIC  S9(4).                                      
           02  E3ANWF    PICTURE X.                                             
           02  FILLER REDEFINES E3ANWF.                                         
             03 E3ANWA    PICTURE X.                                            
           02  E3ANWI  PIC X(4).                                                
           02  A3TEXTD OCCURS 13 TIMES.                                         
             03  A3TEXTL    COMP  PIC  S9(4).                                   
             03  A3TEXTF    PICTURE X.                                          
             03  A3TEXTI  PIC X(79).                                            
       01  BAT0012O REDEFINES BAT0012I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  E3USERO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  A3NAMEO  PIC X(20).                                              
           02  FILLER PICTURE X(3).                                             
           02  E3ABTO  PIC X(5).                                                
           02  FILLER PICTURE X(3).                                             
           02  E3VBNRO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  E3STATO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  E3ANWO  PIC X(4).                                                
           02  DFHMS1 OCCURS 13 TIMES.                                          
             03  FILLER PICTURE X(2).                                           
             03  A3TEXTA    PICTURE X.                                          
             03  A3TEXTO  PIC X(79).                                            
