       01  BVB0050I.                                                            
           02  FILLER PIC X(12).                                                
           02  ESTNRL    COMP  PIC  S9(4).                                      
           02  ESTNRF    PICTURE X.                                             
           02  FILLER REDEFINES ESTNRF.                                         
             03 ESTNRA    PICTURE X.                                            
           02  ESTNRI  PIC X(7).                                                
           02  ESTPRZL    COMP  PIC  S9(4).                                     
           02  ESTPRZF    PICTURE X.                                            
           02  FILLER REDEFINES ESTPRZF.                                        
             03 ESTPRZA    PICTURE X.                                           
           02  ESTPRZI  PIC X(1).                                               
           02  ANAME1L    COMP  PIC  S9(4).                                     
           02  ANAME1F    PICTURE X.                                            
           02  FILLER REDEFINES ANAME1F.                                        
             03 ANAME1A    PICTURE X.                                           
           02  ANAME1I  PIC X(30).                                              
           02  ASTR1L    COMP  PIC  S9(4).                                      
           02  ASTR1F    PICTURE X.                                             
           02  FILLER REDEFINES ASTR1F.                                         
             03 ASTR1A    PICTURE X.                                            
           02  ASTR1I  PIC X(30).                                               
           02  AORT1L    COMP  PIC  S9(4).                                      
           02  AORT1F    PICTURE X.                                             
           02  FILLER REDEFINES AORT1F.                                         
             03 AORT1A    PICTURE X.                                            
           02  AORT1I  PIC X(30).                                               
           02  ADIR1L    COMP  PIC  S9(4).                                      
           02  ADIR1F    PICTURE X.                                             
           02  FILLER REDEFINES ADIR1F.                                         
             03 ADIR1A    PICTURE X.                                            
           02  ADIR1I  PIC X(3).                                                
           02  ABTRNAML    COMP  PIC  S9(4).                                    
           02  ABTRNAMF    PICTURE X.                                           
           02  FILLER REDEFINES ABTRNAMF.                                       
             03 ABTRNAMA    PICTURE X.                                          
           02  ABTRNAMI  PIC X(45).                                             
           02  EAUSL    COMP  PIC  S9(4).                                       
           02  EAUSF    PICTURE X.                                              
           02  FILLER REDEFINES EAUSF.                                          
             03 EAUSA    PICTURE X.                                             
           02  EAUSI  PIC X(1).                                                 
           02  ETAETL    COMP  PIC  S9(4).                                      
           02  ETAETF    PICTURE X.                                             
           02  FILLER REDEFINES ETAETF.                                         
             03 ETAETA    PICTURE X.                                            
           02  ETAETI  PIC X(1).                                                
           02  EDIRL    COMP  PIC  S9(4).                                       
           02  EDIRF    PICTURE X.                                              
           02  FILLER REDEFINES EDIRF.                                          
             03 EDIRA    PICTURE X.                                             
           02  EDIRI  PIC X(3).                                                 
           02  AABDATL    COMP  PIC  S9(4).                                     
           02  AABDATF    PICTURE X.                                            
           02  FILLER REDEFINES AABDATF.                                        
             03 AABDATA    PICTURE X.                                           
           02  AABDATI  PIC X(10).                                              
           02  ADIRALTL    COMP  PIC  S9(4).                                    
           02  ADIRALTF    PICTURE X.                                           
           02  FILLER REDEFINES ADIRALTF.                                       
             03 ADIRALTA    PICTURE X.                                          
           02  ADIRALTI  PIC X(3).                                              
           02  EDIRFRL    COMP  PIC  S9(4).                                     
           02  EDIRFRF    PICTURE X.                                            
           02  FILLER REDEFINES EDIRFRF.                                        
             03 EDIRFRA    PICTURE X.                                           
           02  EDIRFRI  PIC X(1).                                               
       01  BVB0050O REDEFINES BVB0050I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTPRZO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  ANAME1O  PIC X(30).                                              
           02  FILLER PICTURE X(3).                                             
           02  ASTR1O  PIC X(30).                                               
           02  FILLER PICTURE X(3).                                             
           02  AORT1O  PIC X(30).                                               
           02  FILLER PICTURE X(3).                                             
           02  ADIR1O  PIC X(3).                                                
           02  FILLER PICTURE X(3).                                             
           02  ABTRNAMO  PIC X(45).                                             
           02  FILLER PICTURE X(3).                                             
           02  EAUSO  PIC X(1).                                                 
           02  FILLER PICTURE X(3).                                             
           02  ETAETO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  EDIRO  PIC X(3).                                                 
           02  FILLER PICTURE X(3).                                             
           02  AABDATO  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  ADIRALTO  PIC X(3).                                              
           02  FILLER PICTURE X(3).                                             
           02  EDIRFRO  PIC X(1).                                               
