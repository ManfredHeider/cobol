       01  UU947I.                                                              
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
           02  TTEXTI  PIC X(46).                                               
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
           02  FAELLL    COMP  PIC  S9(4).                                      
           02  FAELLF    PICTURE X.                                             
           02  FILLER REDEFINES FAELLF.                                         
             03 FAELLA    PICTURE X.                                            
           02  FAELLI  PIC X(6).                                                
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
           02  TPRODAL    COMP  PIC  S9(4).                                     
           02  TPRODAF    PICTURE X.                                            
           02  FILLER REDEFINES TPRODAF.                                        
             03 TPRODAA    PICTURE X.                                           
           02  TPRODAI  PIC X(22).                                              
           02  PRODAL    COMP  PIC  S9(4).                                      
           02  PRODAF    PICTURE X.                                             
           02  FILLER REDEFINES PRODAF.                                         
             03 PRODAA    PICTURE X.                                            
           02  PRODAI  PIC X(1).                                                
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
       01  UU947O REDEFINES UU947I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNRO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  DATUMO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHRO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  TTEXTO  PIC X(46).                                               
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
           02  FAELLO  PIC X(6).                                                
           02  FILLER PICTURE X(3).                                             
           02  TPRODSO  PIC X(22).                                              
           02  FILLER PICTURE X(3).                                             
           02  PRODSO  PIC X(2).                                                
           02  FILLER PICTURE X(3).                                             
           02  TPRODAO  PIC X(22).                                              
           02  FILLER PICTURE X(3).                                             
           02  PRODAO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  TRANSO  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  FEHLERO  PIC X(79).                                              
