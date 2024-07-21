       01  UA916I.                                                              
           02  FILLER PIC X(12).                                                
           02  VBPNRL    COMP  PIC  S9(4).                                      
           02  VBPNRF    PICTURE X.                                             
           02  FILLER REDEFINES VBPNRF.                                         
             03 VBPNRA    PICTURE X.                                            
           02  VBPNRI  PIC X(7).                                                
           02  VBTITL    COMP  PIC  S9(4).                                      
           02  VBTITF    PICTURE X.                                             
           02  FILLER REDEFINES VBTITF.                                         
             03 VBTITA    PICTURE X.                                            
           02  VBTITI  PIC X(33).                                               
           02  VBDATL    COMP  PIC  S9(4).                                      
           02  VBDATF    PICTURE X.                                             
           02  FILLER REDEFINES VBDATF.                                         
             03 VBDATA    PICTURE X.                                            
           02  VBDATI  PIC X(8).                                                
           02  VBUHRL    COMP  PIC  S9(4).                                      
           02  VBUHRF    PICTURE X.                                             
           02  FILLER REDEFINES VBUHRF.                                         
             03 VBUHRA    PICTURE X.                                            
           02  VBUHRI  PIC X(8).                                                
           02  VBAUSL    COMP  PIC  S9(4).                                      
           02  VBAUSF    PICTURE X.                                             
           02  FILLER REDEFINES VBAUSF.                                         
             03 VBAUSA    PICTURE X.                                            
           02  VBAUSI  PIC X(1).                                                
           02  VBVBNL    COMP  PIC  S9(4).                                      
           02  VBVBNF    PICTURE X.                                             
           02  FILLER REDEFINES VBVBNF.                                         
             03 VBVBNA    PICTURE X.                                            
           02  VBVBNI  PIC X(7).                                                
           02  VBVBPL    COMP  PIC  S9(4).                                      
           02  VBVBPF    PICTURE X.                                             
           02  FILLER REDEFINES VBVBPF.                                         
             03 VBVBPA    PICTURE X.                                            
           02  VBVBPI  PIC X(1).                                                
           02  VBMSGL    COMP  PIC  S9(4).                                      
           02  VBMSGF    PICTURE X.                                             
           02  FILLER REDEFINES VBMSGF.                                         
             03 VBMSGA    PICTURE X.                                            
           02  VBMSGI  PIC X(56).                                               
       01  UA916O REDEFINES UA916I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBPNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBTITO  PIC X(33).                                               
           02  FILLER PICTURE X(3).                                             
           02  VBDATO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBUHRO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBAUSO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBVBNO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBVBPO  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBMSGO  PIC X(56).                                               
