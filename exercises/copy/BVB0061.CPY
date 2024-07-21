       01  BVB0061I.                                                            
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
           02  AINFOZLL    COMP  PIC  S9(4).                                    
           02  AINFOZLF    PICTURE X.                                           
           02  FILLER REDEFINES AINFOZLF.                                       
             03 AINFOZLA    PICTURE X.                                          
           02  AINFOZLI  PIC X(65).                                             
           02  ANAMEL    COMP  PIC  S9(4).                                      
           02  ANAMEF    PICTURE X.                                             
           02  FILLER REDEFINES ANAMEF.                                         
             03 ANAMEA    PICTURE X.                                            
           02  ANAMEI  PIC X(32).                                               
           02  AVNAMEL    COMP  PIC  S9(4).                                     
           02  AVNAMEF    PICTURE X.                                            
           02  FILLER REDEFINES AVNAMEF.                                        
             03 AVNAMEA    PICTURE X.                                           
           02  AVNAMEI  PIC X(30).                                              
           02  ASTEUNRL    COMP  PIC  S9(4).                                    
           02  ASTEUNRF    PICTURE X.                                           
           02  FILLER REDEFINES ASTEUNRF.                                       
             03 ASTEUNRA    PICTURE X.                                          
           02  ASTEUNRI  PIC X(1).                                              
           02  ESTEUNRL    COMP  PIC  S9(4).                                    
           02  ESTEUNRF    PICTURE X.                                           
           02  FILLER REDEFINES ESTEUNRF.                                       
             03 ESTEUNRA    PICTURE X.                                          
           02  ESTEUNRI  PIC X(13).                                             
           02  ASTJAHRL    COMP  PIC  S9(4).                                    
           02  ASTJAHRF    PICTURE X.                                           
           02  FILLER REDEFINES ASTJAHRF.                                       
             03 ASTJAHRA    PICTURE X.                                          
           02  ASTJAHRI  PIC X(1).                                              
           02  ESTJAHRL    COMP  PIC  S9(4).                                    
           02  ESTJAHRF    PICTURE X.                                           
           02  FILLER REDEFINES ESTJAHRF.                                       
             03 ESTJAHRA    PICTURE X.                                          
           02  ESTJAHRI  PIC X(4).                                              
           02  ASTSTATL    COMP  PIC  S9(4).                                    
           02  ASTSTATF    PICTURE X.                                           
           02  FILLER REDEFINES ASTSTATF.                                       
             03 ASTSTATA    PICTURE X.                                          
           02  ASTSTATI  PIC X(1).                                              
           02  ESTSTATL    COMP  PIC  S9(4).                                    
           02  ESTSTATF    PICTURE X.                                           
           02  FILLER REDEFINES ESTSTATF.                                       
             03 ESTSTATA    PICTURE X.                                          
           02  ESTSTATI  PIC X(1).                                              
           02  AUEBERZL    COMP  PIC  S9(4).                                    
           02  AUEBERZF    PICTURE X.                                           
           02  FILLER REDEFINES AUEBERZF.                                       
             03 AUEBERZA    PICTURE X.                                          
           02  AUEBERZI  PIC X(79).                                             
           02  AZEILED OCCURS 8 TIMES.                                          
             03  AZEILEL    COMP  PIC  S9(4).                                   
             03  AZEILEF    PICTURE X.                                          
             03  AZEILEI  PIC X(79).                                            
       01  BVB0061O REDEFINES BVB0061I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTPRZO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  AINFOZLO  PIC X(65).                                             
           02  FILLER PICTURE X(3).                                             
           02  ANAMEO  PIC X(32).                                               
           02  FILLER PICTURE X(3).                                             
           02  AVNAMEO  PIC X(30).                                              
           02  FILLER PICTURE X(3).                                             
           02  ASTEUNRO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  ESTEUNRO  PIC X(13).                                             
           02  FILLER PICTURE X(3).                                             
           02  ASTJAHRO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  ESTJAHRO  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  ASTSTATO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  ESTSTATO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  AUEBERZO  PIC X(79).                                             
           02  DFHMS1 OCCURS 8 TIMES.                                           
             03  FILLER PICTURE X(2).                                           
             03  AZEILEA    PICTURE X.                                          
             03  AZEILEO  PIC X(79).                                            
