       01  UU906I.                                                              
           02  FILLER PIC X(12).                                                
           02  PNRL    COMP  PIC  S9(4).                                        
           02  PNRF    PICTURE X.                                               
           02  FILLER REDEFINES PNRF.                                           
             03 PNRA    PICTURE X.                                              
           02  PNRI  PIC X(7).                                                  
           02  DATUML    COMP  PIC  S9(4).                                      
           02  DATUMF    PICTURE X.                                             
           02  FILLER REDEFINES DATUMF.                                         
             03 DATUMA    PICTURE X.                                            
           02  DATUMI  PIC X(8).                                                
           02  UHRL    COMP  PIC  S9(4).                                        
           02  UHRF    PICTURE X.                                               
           02  FILLER REDEFINES UHRF.                                           
             03 UHRA    PICTURE X.                                              
           02  UHRI  PIC X(5).                                                  
           02  TTEXTL    COMP  PIC  S9(4).                                      
           02  TTEXTF    PICTURE X.                                             
           02  FILLER REDEFINES TTEXTF.                                         
             03 TTEXTA    PICTURE X.                                            
           02  TTEXTI  PIC X(50).                                               
           02  TVBNRL    COMP  PIC  S9(4).                                      
           02  TVBNRF    PICTURE X.                                             
           02  FILLER REDEFINES TVBNRF.                                         
             03 TVBNRA    PICTURE X.                                            
           02  TVBNRI  PIC X(28).                                               
           02  VBNRL    COMP  PIC  S9(4).                                       
           02  VBNRF    PICTURE X.                                              
           02  FILLER REDEFINES VBNRF.                                          
             03 VBNRA    PICTURE X.                                             
           02  VBNRI  PIC X(7).                                                 
           02  TNAMEL    COMP  PIC  S9(4).                                      
           02  TNAMEF    PICTURE X.                                             
           02  FILLER REDEFINES TNAMEF.                                         
             03 TNAMEA    PICTURE X.                                            
           02  TNAMEI  PIC X(28).                                               
           02  NAMEL    COMP  PIC  S9(4).                                       
           02  NAMEF    PICTURE X.                                              
           02  FILLER REDEFINES NAMEF.                                          
             03 NAMEA    PICTURE X.                                             
           02  NAMEI  PIC X(20).                                                
           02  TRANSL    COMP  PIC  S9(4).                                      
           02  TRANSF    PICTURE X.                                             
           02  FILLER REDEFINES TRANSF.                                         
             03 TRANSA    PICTURE X.                                            
           02  TRANSI  PIC X(4).                                                
           02  FEHLERL    COMP  PIC  S9(4).                                     
           02  FEHLERF    PICTURE X.                                            
           02  FILLER REDEFINES FEHLERF.                                        
             03 FEHLERA    PICTURE X.                                           
           02  FEHLERI  PIC X(79).                                              
       01  UU906O REDEFINES UU906I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNRO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  DATUMO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHRO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  TTEXTO  PIC X(50).                                               
           02  FILLER PICTURE X(3).                                             
           02  TVBNRO  PIC X(28).                                               
           02  FILLER PICTURE X(3).                                             
           02  VBNRO  PIC X(7).                                                 
           02  FILLER PICTURE X(3).                                             
           02  TNAMEO  PIC X(28).                                               
           02  FILLER PICTURE X(3).                                             
           02  NAMEO  PIC X(20).                                                
           02  FILLER PICTURE X(3).                                             
           02  TRANSO  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  FEHLERO  PIC X(79).                                              
