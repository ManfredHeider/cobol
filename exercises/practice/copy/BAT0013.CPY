       01  BAT0013I.                                                            
           02  FILLER PIC X(12).                                                
           02  E4USERL    COMP  PIC  S9(4).                                     
           02  E4USERF    PICTURE X.                                            
           02  FILLER REDEFINES E4USERF.                                        
             03 E4USERA    PICTURE X.                                           
           02  E4USERI  PIC X(7).                                               
           02  A4NAMEL    COMP  PIC  S9(4).                                     
           02  A4NAMEF    PICTURE X.                                            
           02  FILLER REDEFINES A4NAMEF.                                        
             03 A4NAMEA    PICTURE X.                                           
           02  A4NAMEI  PIC X(20).                                              
           02  E4ABTL    COMP  PIC  S9(4).                                      
           02  E4ABTF    PICTURE X.                                             
           02  FILLER REDEFINES E4ABTF.                                         
             03 E4ABTA    PICTURE X.                                            
           02  E4ABTI  PIC X(5).                                                
           02  E4VBNRL    COMP  PIC  S9(4).                                     
           02  E4VBNRF    PICTURE X.                                            
           02  FILLER REDEFINES E4VBNRF.                                        
             03 E4VBNRA    PICTURE X.                                           
           02  E4VBNRI  PIC X(7).                                               
           02  E4STATL    COMP  PIC  S9(4).                                     
           02  E4STATF    PICTURE X.                                            
           02  FILLER REDEFINES E4STATF.                                        
             03 E4STATA    PICTURE X.                                           
           02  E4STATI  PIC X(1).                                               
           02  E4ANWL    COMP  PIC  S9(4).                                      
           02  E4ANWF    PICTURE X.                                             
           02  FILLER REDEFINES E4ANWF.                                         
             03 E4ANWA    PICTURE X.                                            
           02  E4ANWI  PIC X(4).                                                
           02  E4VP1L    COMP  PIC  S9(4).                                      
           02  E4VP1F    PICTURE X.                                             
           02  FILLER REDEFINES E4VP1F.                                         
             03 E4VP1A    PICTURE X.                                            
           02  E4VP1I  PIC X(3).                                                
           02  E4VP2L    COMP  PIC  S9(4).                                      
           02  E4VP2F    PICTURE X.                                             
           02  FILLER REDEFINES E4VP2F.                                         
             03 E4VP2A    PICTURE X.                                            
           02  E4VP2I  PIC X(26).                                               
           02  E4DIRD OCCURS 200 TIMES.                                         
             03  E4DIRL    COMP  PIC  S9(4).                                    
             03  E4DIRF    PICTURE X.                                           
             03  E4DIRI  PIC X(3).                                              
       01  BAT0013O REDEFINES BAT0013I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  E4USERO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  A4NAMEO  PIC X(20).                                              
           02  FILLER PICTURE X(3).                                             
           02  E4ABTO  PIC X(5).                                                
           02  FILLER PICTURE X(3).                                             
           02  E4VBNRO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  E4STATO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  E4ANWO  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  E4VP1O  PIC X(3).                                                
           02  FILLER PICTURE X(3).                                             
           02  E4VP2O  PIC X(26).                                               
           02  DFHMS1 OCCURS 200 TIMES.                                         
             03  FILLER PICTURE X(2).                                           
             03  E4DIRA    PICTURE X.                                           
             03  E4DIRO  PIC X(3).                                              
