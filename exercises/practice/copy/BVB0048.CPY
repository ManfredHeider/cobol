       01  BVB0048I.                                                            
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
           02  ADIR1L    COMP  PIC  S9(4).                                      
           02  ADIR1F    PICTURE X.                                             
           02  FILLER REDEFINES ADIR1F.                                         
             03 ADIR1A    PICTURE X.                                            
           02  ADIR1I  PIC X(3).                                                
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
           02  ABTRNAML    COMP  PIC  S9(4).                                    
           02  ABTRNAMF    PICTURE X.                                           
           02  FILLER REDEFINES ABTRNAMF.                                       
             03 ABTRNAMA    PICTURE X.                                          
           02  ABTRNAMI  PIC X(40).                                             
           02  ASTFL    COMP  PIC  S9(4).                                       
           02  ASTFF    PICTURE X.                                              
           02  FILLER REDEFINES ASTFF.                                          
             03 ASTFA    PICTURE X.                                             
           02  ASTFI  PIC X(2).                                                 
           02  ESTNR2L    COMP  PIC  S9(4).                                     
           02  ESTNR2F    PICTURE X.                                            
           02  FILLER REDEFINES ESTNR2F.                                        
             03 ESTNR2A    PICTURE X.                                           
           02  ESTNR2I  PIC X(7).                                               
           02  ESTPRZ2L    COMP  PIC  S9(4).                                    
           02  ESTPRZ2F    PICTURE X.                                           
           02  FILLER REDEFINES ESTPRZ2F.                                       
             03 ESTPRZ2A    PICTURE X.                                          
           02  ESTPRZ2I  PIC X(1).                                              
           02  AABDATL    COMP  PIC  S9(4).                                     
           02  AABDATF    PICTURE X.                                            
           02  FILLER REDEFINES AABDATF.                                        
             03 AABDATA    PICTURE X.                                           
           02  AABDATI  PIC X(10).                                              
           02  EAUSL    COMP  PIC  S9(4).                                       
           02  EAUSF    PICTURE X.                                              
           02  FILLER REDEFINES EAUSF.                                          
             03 EAUSA    PICTURE X.                                             
           02  EAUSI  PIC X(1).                                                 
           02  EDIRALTL    COMP  PIC  S9(4).                                    
           02  EDIRALTF    PICTURE X.                                           
           02  FILLER REDEFINES EDIRALTF.                                       
             03 EDIRALTA    PICTURE X.                                          
           02  EDIRALTI  PIC X(1).                                              
           02  ADIR2L    COMP  PIC  S9(4).                                      
           02  ADIR2F    PICTURE X.                                             
           02  FILLER REDEFINES ADIR2F.                                         
             03 ADIR2A    PICTURE X.                                            
           02  ADIR2I  PIC X(3).                                                
           02  ANAME2L    COMP  PIC  S9(4).                                     
           02  ANAME2F    PICTURE X.                                            
           02  FILLER REDEFINES ANAME2F.                                        
             03 ANAME2A    PICTURE X.                                           
           02  ANAME2I  PIC X(30).                                              
       01  BVB0048O REDEFINES BVB0048I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTPRZO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  ADIR1O  PIC X(3).                                                
           02  FILLER PICTURE X(3).                                             
           02  ANAME1O  PIC X(30).                                              
           02  FILLER PICTURE X(3).                                             
           02  ASTR1O  PIC X(30).                                               
           02  FILLER PICTURE X(3).                                             
           02  AORT1O  PIC X(30).                                               
           02  FILLER PICTURE X(3).                                             
           02  ABTRNAMO  PIC X(40).                                             
           02  FILLER PICTURE X(3).                                             
           02  ASTFO  PIC X(2).                                                 
           02  FILLER PICTURE X(3).                                             
           02  ESTNR2O  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  ESTPRZ2O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  AABDATO  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  EAUSO  PIC X(1).                                                 
           02  FILLER PICTURE X(3).                                             
           02  EDIRALTO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  ADIR2O  PIC X(3).                                                
           02  FILLER PICTURE X(3).                                             
           02  ANAME2O  PIC X(30).                                              
