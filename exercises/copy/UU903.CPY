       01  UU903I.                                                              
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
           02  TTEXT1L    COMP  PIC  S9(4).                                     
           02  TTEXT1F    PICTURE X.                                            
           02  FILLER REDEFINES TTEXT1F.                                        
             03 TTEXT1A    PICTURE X.                                           
           02  TTEXT1I  PIC X(50).                                              
           02  TEXT1L    COMP  PIC  S9(4).                                      
           02  TEXT1F    PICTURE X.                                             
           02  FILLER REDEFINES TEXT1F.                                         
             03 TEXT1A    PICTURE X.                                            
           02  TEXT1I  PIC X(28).                                               
           02  VBNUM1L    COMP  PIC  S9(4).                                     
           02  VBNUM1F    PICTURE X.                                            
           02  FILLER REDEFINES VBNUM1F.                                        
             03 VBNUM1A    PICTURE X.                                           
           02  VBNUM1I  PIC X(7).                                               
           02  TEXT2L    COMP  PIC  S9(4).                                      
           02  TEXT2F    PICTURE X.                                             
           02  FILLER REDEFINES TEXT2F.                                         
             03 TEXT2A    PICTURE X.                                            
           02  TEXT2I  PIC X(28).                                               
           02  DATUM2L    COMP  PIC  S9(4).                                     
           02  DATUM2F    PICTURE X.                                            
           02  FILLER REDEFINES DATUM2F.                                        
             03 DATUM2A    PICTURE X.                                           
           02  DATUM2I  PIC X(6).                                               
           02  TEXT3L    COMP  PIC  S9(4).                                      
           02  TEXT3F    PICTURE X.                                             
           02  FILLER REDEFINES TEXT3F.                                         
             03 TEXT3A    PICTURE X.                                            
           02  TEXT3I  PIC X(28).                                               
           02  SCHL3L    COMP  PIC  S9(4).                                      
           02  SCHL3F    PICTURE X.                                             
           02  FILLER REDEFINES SCHL3F.                                         
             03 SCHL3A    PICTURE X.                                            
           02  SCHL3I  PIC X(2).                                                
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
       01  UU903O REDEFINES UU903I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNRO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  DATUMO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHRO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  TTEXT1O  PIC X(50).                                              
           02  FILLER PICTURE X(3).                                             
           02  TEXT1O  PIC X(28).                                               
           02  FILLER PICTURE X(3).                                             
           02  VBNUM1O  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  TEXT2O  PIC X(28).                                               
           02  FILLER PICTURE X(3).                                             
           02  DATUM2O  PIC X(6).                                               
           02  FILLER PICTURE X(3).                                             
           02  TEXT3O  PIC X(28).                                               
           02  FILLER PICTURE X(3).                                             
           02  SCHL3O  PIC X(2).                                                
           02  FILLER PICTURE X(3).                                             
           02  TRANSO  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  FEHLERO  PIC X(79).                                              
