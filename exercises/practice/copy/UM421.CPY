       01  UM421I.                                                              
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
           02  JOBNEUL    COMP  PIC  S9(4).                                     
           02  JOBNEUF    PICTURE X.                                            
           02  FILLER REDEFINES JOBNEUF.                                        
             03 JOBNEUA    PICTURE X.                                           
           02  JOBNEUI  PIC X(6).                                               
           02  ANWNEUL    COMP  PIC  S9(4).                                     
           02  ANWNEUF    PICTURE X.                                            
           02  FILLER REDEFINES ANWNEUF.                                        
             03 ANWNEUA    PICTURE X.                                           
           02  ANWNEUI  PIC X(4).                                               
           02  FEHLERL    COMP  PIC  S9(4).                                     
           02  FEHLERF    PICTURE X.                                            
           02  FILLER REDEFINES FEHLERF.                                        
             03 FEHLERA    PICTURE X.                                           
           02  FEHLERI  PIC X(79).                                              
       01  UM421O REDEFINES UM421I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNRO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  DATUMO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHRO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  JOBNEUO  PIC X(6).                                               
           02  FILLER PICTURE X(3).                                             
           02  ANWNEUO  PIC X(4).                                               
           02  FILLER PICTURE X(3).                                             
           02  FEHLERO  PIC X(79).                                              
