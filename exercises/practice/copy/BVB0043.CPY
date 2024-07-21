       01  BVB0043I.                                                            
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
           02  ANAMEI  PIC X(20).                                               
           02  AVORNAML    COMP  PIC  S9(4).                                    
           02  AVORNAMF    PICTURE X.                                           
           02  FILLER REDEFINES AVORNAMF.                                       
             03 AVORNAMA    PICTURE X.                                          
           02  AVORNAMI  PIC X(15).                                             
           02  ABTRNAML    COMP  PIC  S9(4).                                    
           02  ABTRNAMF    PICTURE X.                                           
           02  FILLER REDEFINES ABTRNAMF.                                       
             03 ABTRNAMA    PICTURE X.                                          
           02  ABTRNAMI  PIC X(16).                                             
           02  EAUSWL    COMP  PIC  S9(4).                                      
           02  EAUSWF    PICTURE X.                                             
           02  FILLER REDEFINES EAUSWF.                                         
             03 EAUSWA    PICTURE X.                                            
           02  EAUSWI  PIC X(1).                                                
           02  EBVABTL    COMP  PIC  S9(4).                                     
           02  EBVABTF    PICTURE X.                                            
           02  FILLER REDEFINES EBVABTF.                                        
             03 EBVABTA    PICTURE X.                                           
           02  EBVABTI  PIC X(2).                                               
           02  EBVABML    COMP  PIC  S9(4).                                     
           02  EBVABMF    PICTURE X.                                            
           02  FILLER REDEFINES EBVABMF.                                        
             03 EBVABMA    PICTURE X.                                           
           02  EBVABMI  PIC X(2).                                               
           02  EBVABJL    COMP  PIC  S9(4).                                     
           02  EBVABJF    PICTURE X.                                            
           02  FILLER REDEFINES EBVABJF.                                        
             03 EBVABJA    PICTURE X.                                           
           02  EBVABJI  PIC X(4).                                               
           02  ASTATL    COMP  PIC  S9(4).                                      
           02  ASTATF    PICTURE X.                                             
           02  FILLER REDEFINES ASTATF.                                         
             03 ASTATA    PICTURE X.                                            
           02  ASTATI  PIC X(20).                                               
           02  AIBANL    COMP  PIC  S9(4).                                      
           02  AIBANF    PICTURE X.                                             
           02  FILLER REDEFINES AIBANF.                                         
             03 AIBANA    PICTURE X.                                            
           02  AIBANI  PIC X(1).                                                
           02  EIBANL    COMP  PIC  S9(4).                                      
           02  EIBANF    PICTURE X.                                             
           02  FILLER REDEFINES EIBANF.                                         
             03 EIBANA    PICTURE X.                                            
           02  EIBANI  PIC X(34).                                               
           02  AKTONRL    COMP  PIC  S9(4).                                     
           02  AKTONRF    PICTURE X.                                            
           02  FILLER REDEFINES AKTONRF.                                        
             03 AKTONRA    PICTURE X.                                           
           02  AKTONRI  PIC X(1).                                               
           02  EKTONRL    COMP  PIC  S9(4).                                     
           02  EKTONRF    PICTURE X.                                            
           02  FILLER REDEFINES EKTONRF.                                        
             03 EKTONRA    PICTURE X.                                           
           02  EKTONRI  PIC X(20).                                              
           02  ABLZL    COMP  PIC  S9(4).                                       
           02  ABLZF    PICTURE X.                                              
           02  FILLER REDEFINES ABLZF.                                          
             03 ABLZA    PICTURE X.                                             
           02  ABLZI  PIC X(1).                                                 
           02  EBLZL    COMP  PIC  S9(4).                                       
           02  EBLZF    PICTURE X.                                              
           02  FILLER REDEFINES EBLZF.                                          
             03 EBLZA    PICTURE X.                                             
           02  EBLZI  PIC X(8).                                                 
           02  ABANKL    COMP  PIC  S9(4).                                      
           02  ABANKF    PICTURE X.                                             
           02  FILLER REDEFINES ABANKF.                                         
             03 ABANKA    PICTURE X.                                            
           02  ABANKI  PIC X(1).                                                
           02  EBANKL    COMP  PIC  S9(4).                                      
           02  EBANKF    PICTURE X.                                             
           02  FILLER REDEFINES EBANKF.                                         
             03 EBANKA    PICTURE X.                                            
           02  EBANKI  PIC X(40).                                               
           02  ABVINHL    COMP  PIC  S9(4).                                     
           02  ABVINHF    PICTURE X.                                            
           02  FILLER REDEFINES ABVINHF.                                        
             03 ABVINHA    PICTURE X.                                           
           02  ABVINHI  PIC X(1).                                               
           02  EBVINHL    COMP  PIC  S9(4).                                     
           02  EBVINHF    PICTURE X.                                            
           02  FILLER REDEFINES EBVINHF.                                        
             03 EBVINHA    PICTURE X.                                           
           02  EBVINHI  PIC X(40).                                              
       01  BVB0043O REDEFINES BVB0043I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTPRZO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  AINFOZLO  PIC X(65).                                             
           02  FILLER PICTURE X(3).                                             
           02  ANAMEO  PIC X(20).                                               
           02  FILLER PICTURE X(3).                                             
           02  AVORNAMO  PIC X(15).                                             
           02  FILLER PICTURE X(3).                                             
           02  ABTRNAMO  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  EAUSWO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  EBVABTO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  EBVABMO  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  EBVABJO  PIC X(4).                                               
           02  FILLER PICTURE X(3).                                             
           02  ASTATO  PIC X(20).                                               
           02  FILLER PICTURE X(3).                                             
           02  AIBANO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  EIBANO  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  AKTONRO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  EKTONRO  PIC X(20).                                              
           02  FILLER PICTURE X(3).                                             
           02  ABLZO  PIC X(1).                                                 
           02  FILLER PICTURE X(3).                                             
           02  EBLZO  PIC X(8).                                                 
           02  FILLER PICTURE X(3).                                             
           02  ABANKO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  EBANKO  PIC X(40).                                               
           02  FILLER PICTURE X(3).                                             
           02  ABVINHO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  EBVINHO  PIC X(40).                                              
