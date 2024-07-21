       01  UU904I.                                                              
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
           02  TEXTL    COMP  PIC  S9(4).                                       
           02  TEXTF    PICTURE X.                                              
           02  FILLER REDEFINES TEXTF.                                          
             03 TEXTA    PICTURE X.                                             
           02  TEXTI  PIC X(40).                                                
           02  TF1L    COMP  PIC  S9(4).                                        
           02  TF1F    PICTURE X.                                               
           02  FILLER REDEFINES TF1F.                                           
             03 TF1A    PICTURE X.                                              
           02  TF1I  PIC X(13).                                                 
           02  F1L    COMP  PIC  S9(4).                                         
           02  F1F    PICTURE X.                                                
           02  FILLER REDEFINES F1F.                                            
             03 F1A    PICTURE X.                                               
           02  F1I  PIC X(2).                                                   
           02  TF2L    COMP  PIC  S9(4).                                        
           02  TF2F    PICTURE X.                                               
           02  FILLER REDEFINES TF2F.                                           
             03 TF2A    PICTURE X.                                              
           02  TF2I  PIC X(17).                                                 
           02  F2L    COMP  PIC  S9(4).                                         
           02  F2F    PICTURE X.                                                
           02  FILLER REDEFINES F2F.                                            
             03 F2A    PICTURE X.                                               
           02  F2I  PIC X(15).                                                  
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
       01  UU904O REDEFINES UU904I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNRO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  DATUMO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHRO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  TEXTO  PIC X(40).                                                
           02  FILLER PICTURE X(3).                                             
           02  TF1O  PIC X(13).                                                 
           02  FILLER PICTURE X(3).                                             
           02  F1O  PIC X(2).                                                   
           02  FILLER PICTURE X(3).                                             
           02  TF2O  PIC X(17).                                                 
           02  FILLER PICTURE X(3).                                             
           02  F2O  PIC X(15).                                                  
           02  FILLER PICTURE X(3).                                             
           02  TRANSO  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  FEHLERO  PIC X(79).                                              
