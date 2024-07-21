       01  BAT0010I.                                                            
           02  FILLER PIC X(12).                                                
           02  A1USERL    COMP  PIC  S9(4).                                     
           02  A1USERF    PICTURE X.                                            
           02  FILLER REDEFINES A1USERF.                                        
             03 A1USERA    PICTURE X.                                           
           02  A1USERI  PIC X(7).                                               
           02  E1USERL    COMP  PIC  S9(4).                                     
           02  E1USERF    PICTURE X.                                            
           02  FILLER REDEFINES E1USERF.                                        
             03 E1USERA    PICTURE X.                                           
           02  E1USERI  PIC X(7).                                               
           02  A1FUNKL    COMP  PIC  S9(4).                                     
           02  A1FUNKF    PICTURE X.                                            
           02  FILLER REDEFINES A1FUNKF.                                        
             03 A1FUNKA    PICTURE X.                                           
           02  A1FUNKI  PIC X(9).                                               
           02  E1FUNKL    COMP  PIC  S9(4).                                     
           02  E1FUNKF    PICTURE X.                                            
           02  FILLER REDEFINES E1FUNKF.                                        
             03 E1FUNKA    PICTURE X.                                           
           02  E1FUNKI  PIC X(2).                                               
           02  A1PROFL    COMP  PIC  S9(4).                                     
           02  A1PROFF    PICTURE X.                                            
           02  FILLER REDEFINES A1PROFF.                                        
             03 A1PROFA    PICTURE X.                                           
           02  A1PROFI  PIC X(7).                                               
           02  E1PROFL    COMP  PIC  S9(4).                                     
           02  E1PROFF    PICTURE X.                                            
           02  FILLER REDEFINES E1PROFF.                                        
             03 E1PROFA    PICTURE X.                                           
           02  E1PROFI  PIC X(3).                                               
           02  A1PROFTL    COMP  PIC  S9(4).                                    
           02  A1PROFTF    PICTURE X.                                           
           02  FILLER REDEFINES A1PROFTF.                                       
             03 A1PROFTA    PICTURE X.                                          
           02  A1PROFTI  PIC X(35).                                             
           02  A1NAMEL    COMP  PIC  S9(4).                                     
           02  A1NAMEF    PICTURE X.                                            
           02  FILLER REDEFINES A1NAMEF.                                        
             03 A1NAMEA    PICTURE X.                                           
           02  A1NAMEI  PIC X(20).                                              
       01  BAT0010O REDEFINES BAT0010I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  A1USERO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  E1USERO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  A1FUNKO  PIC X(9).                                               
           02  FILLER PICTURE X(3).                                             
           02  E1FUNKO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  A1PROFO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  E1PROFO  PIC X(3).                                               
           02  FILLER PICTURE X(3).                                             
           02  A1PROFTO  PIC X(35).                                             
           02  FILLER PICTURE X(3).                                             
           02  A1NAMEO  PIC X(20).                                              
