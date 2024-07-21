       01  BVB0056I.                                                            
           02  FILLER PIC X(12).                                                
           02  EAUSWL    COMP  PIC  S9(4).                                      
           02  EAUSWF    PICTURE X.                                             
           02  FILLER REDEFINES EAUSWF.                                         
             03 EAUSWA    PICTURE X.                                            
           02  EAUSWI  PIC X(1).                                                
           02  EKLTYPL    COMP  PIC  S9(4).                                     
           02  EKLTYPF    PICTURE X.                                            
           02  FILLER REDEFINES EKLTYPF.                                        
             03 EKLTYPA    PICTURE X.                                           
           02  EKLTYPI  PIC X(1).                                               
           02  ASTNRL    COMP  PIC  S9(4).                                      
           02  ASTNRF    PICTURE X.                                             
           02  FILLER REDEFINES ASTNRF.                                         
             03 ASTNRA    PICTURE X.                                            
           02  ASTNRI  PIC X(7).                                                
           02  ASTPRZL    COMP  PIC  S9(4).                                     
           02  ASTPRZF    PICTURE X.                                            
           02  FILLER REDEFINES ASTPRZF.                                        
             03 ASTPRZA    PICTURE X.                                           
           02  ASTPRZI  PIC X(1).                                               
           02  EFUERVBL    COMP  PIC  S9(4).                                    
           02  EFUERVBF    PICTURE X.                                           
           02  FILLER REDEFINES EFUERVBF.                                       
             03 EFUERVBA    PICTURE X.                                          
           02  EFUERVBI  PIC X(7).                                              
           02  EFVBPRZL    COMP  PIC  S9(4).                                    
           02  EFVBPRZF    PICTURE X.                                           
           02  FILLER REDEFINES EFVBPRZF.                                       
             03 EFVBPRZA    PICTURE X.                                          
           02  EFVBPRZI  PIC X(1).                                              
           02  EANREDEL    COMP  PIC  S9(4).                                    
           02  EANREDEF    PICTURE X.                                           
           02  FILLER REDEFINES EANREDEF.                                       
             03 EANREDEA    PICTURE X.                                          
           02  EANREDEI  PIC X(1).                                              
           02  EVORNAML    COMP  PIC  S9(4).                                    
           02  EVORNAMF    PICTURE X.                                           
           02  FILLER REDEFINES EVORNAMF.                                       
             03 EVORNAMA    PICTURE X.                                          
           02  EVORNAMI  PIC X(20).                                             
           02  ENAMEL    COMP  PIC  S9(4).                                      
           02  ENAMEF    PICTURE X.                                             
           02  FILLER REDEFINES ENAMEF.                                         
             03 ENAMEA    PICTURE X.                                            
           02  ENAMEI  PIC X(40).                                               
           02  EZUSTLL    COMP  PIC  S9(4).                                     
           02  EZUSTLF    PICTURE X.                                            
           02  FILLER REDEFINES EZUSTLF.                                        
             03 EZUSTLA    PICTURE X.                                           
           02  EZUSTLI  PIC X(2).                                               
           02  ESTRASSL    COMP  PIC  S9(4).                                    
           02  ESTRASSF    PICTURE X.                                           
           02  FILLER REDEFINES ESTRASSF.                                       
             03 ESTRASSA    PICTURE X.                                          
           02  ESTRASSI  PIC X(40).                                             
           02  EPLZL    COMP  PIC  S9(4).                                       
           02  EPLZF    PICTURE X.                                              
           02  FILLER REDEFINES EPLZF.                                          
             03 EPLZA    PICTURE X.                                             
           02  EPLZI  PIC X(5).                                                 
           02  EORTL    COMP  PIC  S9(4).                                       
           02  EORTF    PICTURE X.                                              
           02  FILLER REDEFINES EORTF.                                          
             03 EORTA    PICTURE X.                                             
           02  EORTI  PIC X(40).                                                
           02  EAUSLL    COMP  PIC  S9(4).                                      
           02  EAUSLF    PICTURE X.                                             
           02  FILLER REDEFINES EAUSLF.                                         
             03 EAUSLA    PICTURE X.                                            
           02  EAUSLI  PIC X(3).                                                
           02  ETELFONL    COMP  PIC  S9(4).                                    
           02  ETELFONF    PICTURE X.                                           
           02  FILLER REDEFINES ETELFONF.                                       
             03 ETELFONA    PICTURE X.                                          
           02  ETELFONI  PIC X(26).                                             
           02  ETELFAXL    COMP  PIC  S9(4).                                    
           02  ETELFAXF    PICTURE X.                                           
           02  FILLER REDEFINES ETELFAXF.                                       
             03 ETELFAXA    PICTURE X.                                          
           02  ETELFAXI  PIC X(26).                                             
           02  EEINDTL    COMP  PIC  S9(4).                                     
           02  EEINDTF    PICTURE X.                                            
           02  FILLER REDEFINES EEINDTF.                                        
             03 EEINDTA    PICTURE X.                                           
           02  EEINDTI  PIC X(2).                                               
           02  EEINDML    COMP  PIC  S9(4).                                     
           02  EEINDMF    PICTURE X.                                            
           02  FILLER REDEFINES EEINDMF.                                        
             03 EEINDMA    PICTURE X.                                           
           02  EEINDMI  PIC X(2).                                               
           02  EEINDJL    COMP  PIC  S9(4).                                     
           02  EEINDJF    PICTURE X.                                            
           02  FILLER REDEFINES EEINDJF.                                        
             03 EEINDJA    PICTURE X.                                           
           02  EEINDJI  PIC X(4).                                               
       01  BVB0056O REDEFINES BVB0056I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  EAUSWO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  EKLTYPO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  ASTNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  ASTPRZO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  EFUERVBO  PIC X(7).                                              
           02  FILLER PICTURE X(3).                                             
           02  EFVBPRZO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  EANREDEO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  EVORNAMO  PIC X(20).                                             
           02  FILLER PICTURE X(3).                                             
           02  ENAMEO  PIC X(40).                                               
           02  FILLER PICTURE X(3).                                             
           02  EZUSTLO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  ESTRASSO  PIC X(40).                                             
           02  FILLER PICTURE X(3).                                             
           02  EPLZO  PIC X(5).                                                 
           02  FILLER PICTURE X(3).                                             
           02  EORTO  PIC X(40).                                                
           02  FILLER PICTURE X(3).                                             
           02  EAUSLO  PIC X(3).                                                
           02  FILLER PICTURE X(3).                                             
           02  ETELFONO  PIC X(26).                                             
           02  FILLER PICTURE X(3).                                             
           02  ETELFAXO  PIC X(26).                                             
           02  FILLER PICTURE X(3).                                             
           02  EEINDTO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  EEINDMO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  EEINDJO  PIC X(4).                                               
