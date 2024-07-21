       01  BVB0035I.                                                            
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
           02  ADIRL    COMP  PIC  S9(4).                                       
           02  ADIRF    PICTURE X.                                              
           02  FILLER REDEFINES ADIRF.                                          
             03 ADIRA    PICTURE X.                                             
           02  FILLER   PICTURE X(1).                                           
           02  ADIRI  PIC X(3).                                                 
           02  EAUSWL    COMP  PIC  S9(4).                                      
           02  EAUSWF    PICTURE X.                                             
           02  FILLER REDEFINES EAUSWF.                                         
             03 EAUSWA    PICTURE X.                                            
           02  FILLER   PICTURE X(1).                                           
           02  EAUSWI  PIC X(1).                                                
           02  TUMSDATL    COMP  PIC  S9(4).                                    
           02  TUMSDATF    PICTURE X.                                           
           02  FILLER REDEFINES TUMSDATF.                                       
             03 TUMSDATA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  TUMSDATI  PIC X(17).                                             
           02  EUMSDTTL    COMP  PIC  S9(4).                                    
           02  EUMSDTTF    PICTURE X.                                           
           02  FILLER REDEFINES EUMSDTTF.                                       
             03 EUMSDTTA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  EUMSDTTI  PIC X(2).                                              
           02  EUMSDMML    COMP  PIC  S9(4).                                    
           02  EUMSDMMF    PICTURE X.                                           
           02  FILLER REDEFINES EUMSDMMF.                                       
             03 EUMSDMMA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  EUMSDMMI  PIC X(2).                                              
           02  EUMSDJJL    COMP  PIC  S9(4).                                    
           02  EUMSDJJF    PICTURE X.                                           
           02  FILLER REDEFINES EUMSDJJF.                                       
             03 EUMSDJJA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  EUMSDJJI  PIC X(4).                                              
           02  TUMSDA2L    COMP  PIC  S9(4).                                    
           02  TUMSDA2F    PICTURE X.                                           
           02  FILLER REDEFINES TUMSDA2F.                                       
             03 TUMSDA2A    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  TUMSDA2I  PIC X(15).                                             
           02  ANLL    COMP  PIC  S9(4).                                        
           02  ANLF    PICTURE X.                                               
           02  FILLER REDEFINES ANLF.                                           
             03 ANLA    PICTURE X.                                              
           02  FILLER   PICTURE X(1).                                           
           02  ANLI  PIC X(40).                                                 
           02  AVNL    COMP  PIC  S9(4).                                        
           02  AVNF    PICTURE X.                                               
           02  FILLER REDEFINES AVNF.                                           
             03 AVNA    PICTURE X.                                              
           02  FILLER   PICTURE X(1).                                           
           02  AVNI  PIC X(20).                                                 
           02  ASTRSTFL    COMP  PIC  S9(4).                                    
           02  ASTRSTFF    PICTURE X.                                           
           02  FILLER REDEFINES ASTRSTFF.                                       
             03 ASTRSTFA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  ASTRSTFI  PIC X(4).                                              
           02  APRXSTFL    COMP  PIC  S9(4).                                    
           02  APRXSTFF    PICTURE X.                                           
           02  FILLER REDEFINES APRXSTFF.                                       
             03 APRXSTFA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  APRXSTFI  PIC X(1).                                              
           02  ADATANGL    COMP  PIC  S9(4).                                    
           02  ADATANGF    PICTURE X.                                           
           02  FILLER REDEFINES ADATANGF.                                       
             03 ADATANGA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  ADATANGI  PIC X(10).                                             
           02  ADATUMGL    COMP  PIC  S9(4).                                    
           02  ADATUMGF    PICTURE X.                                           
           02  FILLER REDEFINES ADATUMGF.                                       
             03 ADATUMGA    PICTURE X.                                          
           02  FILLER   PICTURE X(1).                                           
           02  ADATUMGI  PIC X(10).                                             
       01  BVB0035O REDEFINES BVB0035I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ESTH    PICTURE X.                                               
           02  ESTO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  ESTPH    PICTURE X.                                              
           02  ESTPO  PIC X(1).                                                 
           02  FILLER PICTURE X(3).                                             
           02  ADIRH    PICTURE X.                                              
           02  ADIRO  PIC X(3).                                                 
           02  FILLER PICTURE X(3).                                             
           02  EAUSWH    PICTURE X.                                             
           02  EAUSWO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  TUMSDATH    PICTURE X.                                           
           02  TUMSDATO  PIC X(17).                                             
           02  FILLER PICTURE X(3).                                             
           02  EUMSDTTH    PICTURE X.                                           
           02  EUMSDTTO  PIC X(2).                                              
           02  FILLER PICTURE X(3).                                             
           02  EUMSDMMH    PICTURE X.                                           
           02  EUMSDMMO  PIC X(2).                                              
           02  FILLER PICTURE X(3).                                             
           02  EUMSDJJH    PICTURE X.                                           
           02  EUMSDJJO  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  TUMSDA2H    PICTURE X.                                           
           02  TUMSDA2O  PIC X(15).                                             
           02  FILLER PICTURE X(3).                                             
           02  ANLH    PICTURE X.                                               
           02  ANLO  PIC X(40).                                                 
           02  FILLER PICTURE X(3).                                             
           02  AVNH    PICTURE X.                                               
           02  AVNO  PIC X(20).                                                 
           02  FILLER PICTURE X(3).                                             
           02  ASTRSTFH    PICTURE X.                                           
           02  ASTRSTFO  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  APRXSTFH    PICTURE X.                                           
           02  APRXSTFO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  ADATANGH    PICTURE X.                                           
           02  ADATANGO  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  ADATUMGH    PICTURE X.                                           
           02  ADATUMGO  PIC X(10).                                             
