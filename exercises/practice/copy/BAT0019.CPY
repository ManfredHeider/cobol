       01  SIGNMAPI.                                                            
           02  FILLER PIC X(12).                                                
           02  DATIDL    COMP  PIC  S9(4).                                      
           02  DATIDF    PICTURE X.                                             
           02  FILLER REDEFINES DATIDF.                                         
             03 DATIDA    PICTURE X.                                            
           02  DATIDI  PIC X(8).                                                
           02  NEWS1L    COMP  PIC  S9(4).                                      
           02  NEWS1F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS1F.                                         
             03 NEWS1A    PICTURE X.                                            
           02  NEWS1I  PIC X(34).                                               
           02  NEWS2L    COMP  PIC  S9(4).                                      
           02  NEWS2F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS2F.                                         
             03 NEWS2A    PICTURE X.                                            
           02  NEWS2I  PIC X(34).                                               
           02  NEWS3L    COMP  PIC  S9(4).                                      
           02  NEWS3F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS3F.                                         
             03 NEWS3A    PICTURE X.                                            
           02  NEWS3I  PIC X(34).                                               
           02  NEWS4L    COMP  PIC  S9(4).                                      
           02  NEWS4F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS4F.                                         
             03 NEWS4A    PICTURE X.                                            
           02  NEWS4I  PIC X(34).                                               
           02  NEWS5L    COMP  PIC  S9(4).                                      
           02  NEWS5F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS5F.                                         
             03 NEWS5A    PICTURE X.                                            
           02  NEWS5I  PIC X(34).                                               
           02  NEWS6L    COMP  PIC  S9(4).                                      
           02  NEWS6F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS6F.                                         
             03 NEWS6A    PICTURE X.                                            
           02  NEWS6I  PIC X(34).                                               
           02  NEWS7L    COMP  PIC  S9(4).                                      
           02  NEWS7F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS7F.                                         
             03 NEWS7A    PICTURE X.                                            
           02  NEWS7I  PIC X(34).                                               
           02  NEWS8L    COMP  PIC  S9(4).                                      
           02  NEWS8F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS8F.                                         
             03 NEWS8A    PICTURE X.                                            
           02  NEWS8I  PIC X(34).                                               
           02  NEWS9L    COMP  PIC  S9(4).                                      
           02  NEWS9F    PICTURE X.                                             
           02  FILLER REDEFINES NEWS9F.                                         
             03 NEWS9A    PICTURE X.                                            
           02  NEWS9I  PIC X(34).                                               
           02  NEWS10L    COMP  PIC  S9(4).                                     
           02  NEWS10F    PICTURE X.                                            
           02  FILLER REDEFINES NEWS10F.                                        
             03 NEWS10A    PICTURE X.                                           
           02  NEWS10I  PIC X(34).                                              
           02  NEWS11L    COMP  PIC  S9(4).                                     
           02  NEWS11F    PICTURE X.                                            
           02  FILLER REDEFINES NEWS11F.                                        
             03 NEWS11A    PICTURE X.                                           
           02  NEWS11I  PIC X(34).                                              
           02  NEWS12L    COMP  PIC  S9(4).                                     
           02  NEWS12F    PICTURE X.                                            
           02  FILLER REDEFINES NEWS12F.                                        
             03 NEWS12A    PICTURE X.                                           
           02  NEWS12I  PIC X(34).                                              
           02  USERIDL    COMP  PIC  S9(4).                                     
           02  USERIDF    PICTURE X.                                            
           02  FILLER REDEFINES USERIDF.                                        
             03 USERIDA    PICTURE X.                                           
           02  USERIDI  PIC X(7).                                               
           02  NEWS13L    COMP  PIC  S9(4).                                     
           02  NEWS13F    PICTURE X.                                            
           02  FILLER REDEFINES NEWS13F.                                        
             03 NEWS13A    PICTURE X.                                           
           02  NEWS13I  PIC X(34).                                              
           02  NEWS14L    COMP  PIC  S9(4).                                     
           02  NEWS14F    PICTURE X.                                            
           02  FILLER REDEFINES NEWS14F.                                        
             03 NEWS14A    PICTURE X.                                           
           02  NEWS14I  PIC X(34).                                              
           02  PWORDL    COMP  PIC  S9(4).                                      
           02  PWORDF    PICTURE X.                                             
           02  FILLER REDEFINES PWORDF.                                         
             03 PWORDA    PICTURE X.                                            
           02  PWORDI  PIC X(127).                                              
           02  NPWORD1L    COMP  PIC  S9(4).                                    
           02  NPWORD1F    PICTURE X.                                           
           02  FILLER REDEFINES NPWORD1F.                                       
             03 NPWORD1A    PICTURE X.                                          
           02  NPWORD1I  PIC X(127).                                            
           02  NPWORD2L    COMP  PIC  S9(4).                                    
           02  NPWORD2F    PICTURE X.                                           
           02  FILLER REDEFINES NPWORD2F.                                       
             03 NPWORD2A    PICTURE X.                                          
           02  NPWORD2I  PIC X(127).                                            
           02  ANWENDL    COMP  PIC  S9(4).                                     
           02  ANWENDF    PICTURE X.                                            
           02  FILLER REDEFINES ANWENDF.                                        
             03 ANWENDA    PICTURE X.                                           
           02  ANWENDI  PIC X(4).                                               
           02  OBL    COMP  PIC  S9(4).                                         
           02  OBF    PICTURE X.                                                
           02  FILLER REDEFINES OBF.                                            
             03 OBA    PICTURE X.                                               
           02  OBI  PIC X(32).                                                  
           02  MESSL    COMP  PIC  S9(4).                                       
           02  MESSF    PICTURE X.                                              
           02  FILLER REDEFINES MESSF.                                          
             03 MESSA    PICTURE X.                                             
           02  MESSI  PIC X(79).                                                
       01  SIGNMAPO REDEFINES SIGNMAPI.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  DATIDO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  NEWS1O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS2O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS3O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS4O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS5O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS6O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS7O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS8O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS9O  PIC X(34).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS10O  PIC X(34).                                              
           02  FILLER PICTURE X(3).                                             
           02  NEWS11O  PIC X(34).                                              
           02  FILLER PICTURE X(3).                                             
           02  NEWS12O  PIC X(34).                                              
           02  FILLER PICTURE X(3).                                             
           02  USERIDO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  NEWS13O  PIC X(34).                                              
           02  FILLER PICTURE X(3).                                             
           02  NEWS14O  PIC X(34).                                              
           02  FILLER PICTURE X(3).                                             
           02  PWORDO  PIC X(127).                                              
           02  FILLER PICTURE X(3).                                             
           02  NPWORD1O  PIC X(127).                                            
           02  FILLER PICTURE X(3).                                             
           02  NPWORD2O  PIC X(127).                                            
           02  FILLER PICTURE X(3).                                             
           02  ANWENDO  PIC X(4).                                               
           02  FILLER PICTURE X(3).                                             
           02  OBO  PIC X(32).                                                  
           02  FILLER PICTURE X(3).                                             
           02  MESSO  PIC X(79).                                                
