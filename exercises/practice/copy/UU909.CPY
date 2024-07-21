       01  UU909I.                                                              
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
           02  TGESL    COMP  PIC  S9(4).                                       
           02  TGESF    PICTURE X.                                              
           02  FILLER REDEFINES TGESF.                                          
             03 TGESA    PICTURE X.                                             
           02  TGESI  PIC X(22).                                                
           02  GESL    COMP  PIC  S9(4).                                        
           02  GESF    PICTURE X.                                               
           02  FILLER REDEFINES GESF.                                           
             03 GESA    PICTURE X.                                              
           02  GESI  PIC X(2).                                                  
           02  TVSNRL    COMP  PIC  S9(4).                                      
           02  TVSNRF    PICTURE X.                                             
           02  FILLER REDEFINES TVSNRF.                                         
             03 TVSNRA    PICTURE X.                                            
           02  TVSNRI  PIC X(22).                                               
           02  VSNRL    COMP  PIC  S9(4).                                       
           02  VSNRF    PICTURE X.                                              
           02  FILLER REDEFINES VSNRF.                                          
             03 VSNRA    PICTURE X.                                             
           02  VSNRI  PIC X(15).                                                
           02  TFAELLL    COMP  PIC  S9(4).                                     
           02  TFAELLF    PICTURE X.                                            
           02  FILLER REDEFINES TFAELLF.                                        
             03 TFAELLA    PICTURE X.                                           
           02  TFAELLI  PIC X(22).                                              
           02  FAELTTL    COMP  PIC  S9(4).                                     
           02  FAELTTF    PICTURE X.                                            
           02  FILLER REDEFINES FAELTTF.                                        
             03 FAELTTA    PICTURE X.                                           
           02  FAELTTI  PIC X(2).                                               
           02  FAELMML    COMP  PIC  S9(4).                                     
           02  FAELMMF    PICTURE X.                                            
           02  FILLER REDEFINES FAELMMF.                                        
             03 FAELMMA    PICTURE X.                                           
           02  FAELMMI  PIC X(2).                                               
           02  FAELJJL    COMP  PIC  S9(4).                                     
           02  FAELJJF    PICTURE X.                                            
           02  FILLER REDEFINES FAELJJF.                                        
             03 FAELJJA    PICTURE X.                                           
           02  FAELJJI  PIC X(4).                                               
           02  TPRODSL    COMP  PIC  S9(4).                                     
           02  TPRODSF    PICTURE X.                                            
           02  FILLER REDEFINES TPRODSF.                                        
             03 TPRODSA    PICTURE X.                                           
           02  TPRODSI  PIC X(22).                                              
           02  PRODSL    COMP  PIC  S9(4).                                      
           02  PRODSF    PICTURE X.                                             
           02  FILLER REDEFINES PRODSF.                                         
             03 PRODSA    PICTURE X.                                            
           02  PRODSI  PIC X(2).                                                
           02  TLFDNRL    COMP  PIC  S9(4).                                     
           02  TLFDNRF    PICTURE X.                                            
           02  FILLER REDEFINES TLFDNRF.                                        
             03 TLFDNRA    PICTURE X.                                           
           02  TLFDNRI  PIC X(22).                                              
           02  LFDNRL    COMP  PIC  S9(4).                                      
           02  LFDNRF    PICTURE X.                                             
           02  FILLER REDEFINES LFDNRF.                                         
             03 LFDNRA    PICTURE X.                                            
           02  LFDNRI  PIC X(2).                                                
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
       01  UU909O REDEFINES UU909I.                                             
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
           02  TGESO  PIC X(22).                                                
           02  FILLER PICTURE X(3).                                             
           02  GESO  PIC X(2).                                                  
           02  FILLER PICTURE X(3).                                             
           02  TVSNRO  PIC X(22).                                               
           02  FILLER PICTURE X(3).                                             
           02  VSNRO  PIC X(15).                                                
           02  FILLER PICTURE X(3).                                             
           02  TFAELLO  PIC X(22).                                              
           02  FILLER PICTURE X(3).                                             
           02  FAELTTO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  FAELMMO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  FAELJJO  PIC X(4).                                               
           02  FILLER PICTURE X(3).                                             
           02  TPRODSO  PIC X(22).                                              
           02  FILLER PICTURE X(3).                                             
           02  PRODSO  PIC X(2).                                                
           02  FILLER PICTURE X(3).                                             
           02  TLFDNRO  PIC X(22).                                              
           02  FILLER PICTURE X(3).                                             
           02  LFDNRO  PIC X(2).                                                
           02  FILLER PICTURE X(3).                                             
           02  TRANSO  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  FEHLERO  PIC X(79).                                              
