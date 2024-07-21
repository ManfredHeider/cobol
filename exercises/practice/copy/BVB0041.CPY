       01  BVB0041I.                                                            
           02  FILLER PIC X(12).                                                
           02  ESTL    COMP  PIC  S9(4).                                        
           02  ESTF    PICTURE X.                                               
           02  FILLER REDEFINES ESTF.                                           
             03 ESTA    PICTURE X.                                              
           02  FILLER   PICTURE X(1).                                           
           02  ESTI  PIC X(7).                                                  
           02  ESTPL    COMP  PIC  S9(4).                                       
           02  ESTPF    PICTURE X.                                              
           02  FILLER REDEFINES ESTPF.                                          
             03 ESTPA    PICTURE X.                                             
           02  FILLER   PICTURE X(1).                                           
           02  ESTPI  PIC X(1).                                                 
           02  AINFOZLL    COMP  PIC  S9(4).                                    
           02  AINFOZLF    PICTURE X.                                           
           02  FILLER REDEFINES AINFOZLF.                                       
             03 AINFOZLA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  AINFOZLI  PIC X(65).                                             
           02  ANAMEL    COMP  PIC  S9(4).                                      
           02  ANAMEF    PICTURE X.                                             
           02  FILLER REDEFINES ANAMEF.                                         
             03 ANAMEA    PICTURE X.                                            
           02  FILLER   PICTURE X(1).                                           
           02  ANAMEI  PIC X(20).                                               
           02  AVORNAML    COMP  PIC  S9(4).                                    
           02  AVORNAMF    PICTURE X.                                           
           02  FILLER REDEFINES AVORNAMF.                                       
             03 AVORNAMA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  AVORNAMI  PIC X(15).                                             
           02  ABTRNAML    COMP  PIC  S9(4).                                    
           02  ABTRNAMF    PICTURE X.                                           
           02  FILLER REDEFINES ABTRNAMF.                                       
             03 ABTRNAMA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  ABTRNAMI  PIC X(16).                                             
           02  EMAILL    COMP  PIC  S9(4).                                      
           02  EMAILF    PICTURE X.                                             
           02  FILLER REDEFINES EMAILF.                                         
             03 EMAILA    PICTURE X.                                            
           02  FILLER   PICTURE X(1).                                           
           02  EMAILI  PIC X(70).                                               
           02  URLTXTL    COMP  PIC  S9(4).                                     
           02  URLTXTF    PICTURE X.                                            
           02  FILLER REDEFINES URLTXTF.                                        
             03 URLTXTA    PICTURE X.                                           
           02  FILLER   PICTURE X(1).                                           
           02  URLTXTI  PIC X(6).                                               
           02  URLL    COMP  PIC  S9(4).                                        
           02  URLF    PICTURE X.                                               
           02  FILLER REDEFINES URLF.                                           
             03 URLA    PICTURE X.                                              
           02  FILLER   PICTURE X(1).                                           
           02  URLI  PIC X(70).                                                 
           02  APINL    COMP  PIC  S9(4).                                       
           02  APINF    PICTURE X.                                              
           02  FILLER REDEFINES APINF.                                          
             03 APINA    PICTURE X.                                             
           02  FILLER   PICTURE X(1).                                           
           02  APINI  PIC X(10).                                                
           02  ABDATL    COMP  PIC  S9(4).                                      
           02  ABDATF    PICTURE X.                                             
           02  FILLER REDEFINES ABDATF.                                         
             03 ABDATA    PICTURE X.                                            
           02  FILLER   PICTURE X(1).                                           
           02  ABDATI  PIC X(10).                                               
           02  APINH1L    COMP  PIC  S9(4).                                     
           02  APINH1F    PICTURE X.                                            
           02  FILLER REDEFINES APINH1F.                                        
             03 APINH1A    PICTURE X.                                           
           02  FILLER   PICTURE X(1).                                           
           02  APINH1I  PIC X(10).                                              
           02  ABDATH1L    COMP  PIC  S9(4).                                    
           02  ABDATH1F    PICTURE X.                                           
           02  FILLER REDEFINES ABDATH1F.                                       
             03 ABDATH1A    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  ABDATH1I  PIC X(10).                                             
           02  APINH2L    COMP  PIC  S9(4).                                     
           02  APINH2F    PICTURE X.                                            
           02  FILLER REDEFINES APINH2F.                                        
             03 APINH2A    PICTURE X.                                           
           02  FILLER   PICTURE X(1).                                           
           02  APINH2I  PIC X(10).                                              
           02  ABDATH2L    COMP  PIC  S9(4).                                    
           02  ABDATH2F    PICTURE X.                                           
           02  FILLER REDEFINES ABDATH2F.                                       
             03 ABDATH2A    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  ABDATH2I  PIC X(10).                                             
           02  APINH3L    COMP  PIC  S9(4).                                     
           02  APINH3F    PICTURE X.                                            
           02  FILLER REDEFINES APINH3F.                                        
             03 APINH3A    PICTURE X.                                           
           02  FILLER   PICTURE X(1).                                           
           02  APINH3I  PIC X(10).                                              
           02  ABDATH3L    COMP  PIC  S9(4).                                    
           02  ABDATH3F    PICTURE X.                                           
           02  FILLER REDEFINES ABDATH3F.                                       
             03 ABDATH3A    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  ABDATH3I  PIC X(10).                                             
           02  EAUSWL    COMP  PIC  S9(4).                                      
           02  EAUSWF    PICTURE X.                                             
           02  FILLER REDEFINES EAUSWF.                                         
             03 EAUSWA    PICTURE X.                                            
           02  FILLER   PICTURE X(1).                                           
           02  EAUSWI  PIC X(1).                                                
       01  BVB0041O REDEFINES BVB0041I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTH    PICTURE X.                                               
           02  ESTO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  ESTPH    PICTURE X.                                              
           02  ESTPO  PIC X(1).                                                 
           02  FILLER PICTURE X(3).                                             
           02  AINFOZLH    PICTURE X.                                           
           02  AINFOZLO  PIC X(65).                                             
           02  FILLER PICTURE X(3).                                             
           02  ANAMEH    PICTURE X.                                             
           02  ANAMEO  PIC X(20).                                               
           02  FILLER PICTURE X(3).                                             
           02  AVORNAMH    PICTURE X.                                           
           02  AVORNAMO  PIC X(15).                                             
           02  FILLER PICTURE X(3).                                             
           02  ABTRNAMH    PICTURE X.                                           
           02  ABTRNAMO  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  EMAILH    PICTURE X.                                             
           02  EMAILO  PIC X(70).                                               
           02  FILLER PICTURE X(3).                                             
           02  URLTXTH    PICTURE X.                                            
           02  URLTXTO  PIC X(6).                                               
           02  FILLER PICTURE X(3).                                             
           02  URLH    PICTURE X.                                               
           02  URLO  PIC X(70).                                                 
           02  FILLER PICTURE X(3).                                             
           02  APINH    PICTURE X.                                              
           02  APINO  PIC X(10).                                                
           02  FILLER PICTURE X(3).                                             
           02  ABDATH    PICTURE X.                                             
           02  ABDATO  PIC X(10).                                               
           02  FILLER PICTURE X(3).                                             
           02  APINH1H    PICTURE X.                                            
           02  APINH1O  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  ABDATH1H    PICTURE X.                                           
           02  ABDATH1O  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  APINH2H    PICTURE X.                                            
           02  APINH2O  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  ABDATH2H    PICTURE X.                                           
           02  ABDATH2O  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  APINH3H    PICTURE X.                                            
           02  APINH3O  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  ABDATH3H    PICTURE X.                                           
           02  ABDATH3O  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  EAUSWH    PICTURE X.                                             
           02  EAUSWO  PIC X(1).                                                
