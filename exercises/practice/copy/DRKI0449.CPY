      *****************************************************************         
      *    COPY-MEMBER: DRKI0449                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0449                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         25.05.2022 / 13:31:30 / T13AC20            
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI0449                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0449.                                                         
           05  PI-DRKI0449-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0449-KFZVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0449-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0449-N-KFZFHRZGTYPD      PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZGEBDAT-VN     PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZGESGEW        PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZKW            PIC S9(4) COMP.                 
             15 PI-DRKI0449-N-KFZLSTNGSRTD      PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZNUTZ          PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZPAKET-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZPAKET-KTY     PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZPLATZ         PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZSCOREWERT     PIC S9(4) COMP.                 
             15 PI-DRKI0449-NI-KFZTELEMATIKID   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0449-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0449-KFZ-E-FZG-KZ   PIC  X(0001).                        
             10 PI-DRKI0449-KFZABWFAHRLSTJH   PIC S9(7)V COMP-3.                
             10 PI-DRKI0449-KFZAGBRANCHE   PIC  X(0060).                        
             10 PI-DRKI0449-KFZAKZART-KNR   PIC S9(15)V COMP-3.                 
             10 PI-DRKI0449-KFZAKZART-KTY   PIC S9(4) COMP.                     
             10 PI-DRKI0449-KFZALTERSGRP-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0449-KFZALTERSGRP-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0449-KFZAMTLICHESKENNZ   PIC  X(0012).                   
             10 PI-DRKI0449-KFZAUFBAUART-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0449-KFZAUFBAUART-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0449-KFZBEGFAHRERSCHL   PIC S9(4) COMP.                  
             10 PI-DRKI0449-KFZBEGFAHRERTEXT   PIC  X(0010).                    
             10 PI-DRKI0449-KFZBEITRANPASSAM   PIC  X(010).                     
             10 PI-DRKI0449-KFZBERECHNETAM   PIC  X(010).                       
             10 PI-DRKI0449-KFZDATKMSTANDAKT   PIC  X(010).                     
             10 PI-DRKI0449-KFZERSTZULDATUMVN   PIC  X(010).                    
             10 PI-DRKI0449-KFZFAHRER   PIC  X(0010).                           
             10 PI-DRKI0449-KFZFAHRGEBIET   PIC  X(0010).                       
             10 PI-DRKI0449-KFZFAHRGESTELLNR   PIC  X(0020).                    
             10 PI-DRKI0449-KFZFAHRZEUGTYPID   PIC S9(4) COMP.                  
             10 PI-DRKI0449-KFZFALTERMAX-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0449-KFZFALTERMAX-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0449-KFZFIRMENNR   PIC  X(0006).                         
             10 PI-DRKI0449-KFZFSERWERB   PIC  X(0010).                         
             10 PI-DRKI0449-KFZFZGABSTELLPLATZ   PIC  X(0010).                  
             10 PI-DRKI0449-KFZFZGART-KNR   PIC S9(15)V COMP-3.                 
             10 PI-DRKI0449-KFZFZGART-KTY   PIC S9(4) COMP.                     
             10 PI-DRKI0449-KFZFZGNUTZUNG   PIC  X(0010).                       
             10 PI-DRKI0449-KFZGEBDAT-VN   PIC  X(010).                         
             10 PI-DRKI0449-KFZGEBDATFAHRER   PIC  X(010).                      
             10 PI-DRKI0449-KFZGEBDATFAHRERIN   PIC  X(010).                    
             10 PI-DRKI0449-KFZGESAMTNEUWERTFZ   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI0449-KFZGESGEW   PIC S9(7)V COMP-3.                      
             10 PI-DRKI0449-KFZGESNEUWERTWKZ   PIC  X(0001).                    
             10 PI-DRKI0449-KFZHAFTPFLTYPKL   PIC S9(4) COMP.                   
             10 PI-DRKI0449-KFZHALTERNNAME   PIC  X(0040).                      
             10 PI-DRKI0449-KFZHALTERSCHL   PIC S9(4) COMP.                     
             10 PI-DRKI0449-KFZHALTERTEXT   PIC  X(0010).                       
             10 PI-DRKI0449-KFZHALTERVNAME   PIC  X(0040).                      
             10 PI-DRKI0449-KFZHERSTELLERSCHL   PIC  X(0004).                   
             10 PI-DRKI0449-KFZHERSTELLTEXT   PIC  X(0025).                     
             10 PI-DRKI0449-KFZHERSTMODTEXT   PIC  X(0020).                     
             10 PI-DRKI0449-KFZJAEHRLFAHRL   PIC S9(9) COMP.                    
             10 PI-DRKI0449-KFZKMSTAND   PIC S9(9) COMP.                        
             10 PI-DRKI0449-KFZKMSTANDAKT   PIC S9(7)V COMP-3.                  
             10 PI-DRKI0449-KFZKW   PIC S9(4)V COMP-3.                          
             10 PI-DRKI0449-KFZLEASING   PIC S9(4) COMP.                        
             10 PI-DRKI0449-KFZLEISTUNG   PIC S9(5)V9(2) COMP-3.                
             10 PI-DRKI0449-KFZLEISTUNGSARTID   PIC S9(4) COMP.                 
             10 PI-DRKI0449-KFZLEISTUNGTEXT   PIC  X(0012).                     
             10 PI-DRKI0449-KFZLSVSCOR   PIC  X(0001).                          
             10 PI-DRKI0449-KFZNAEHSPEZ-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0449-KFZNAEHSPEZ-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0449-KFZNUTZ   PIC S9(7)V COMP-3.                        
             10 PI-DRKI0449-KFZPAKET-KNR   PIC S9(15)V COMP-3.                  
             10 PI-DRKI0449-KFZPAKET-KTY   PIC S9(4) COMP.                      
             10 PI-DRKI0449-KFZPLATZ   PIC S9(3)V COMP-3.                       
             10 PI-DRKI0449-KFZPSFANZKH   PIC S9(4) COMP.                       
             10 PI-DRKI0449-KFZPSFANZVK   PIC S9(4) COMP.                       
             10 PI-DRKI0449-KFZSAISONBIS   PIC S9(4) COMP.                      
             10 PI-DRKI0449-KFZSAISONKZ   PIC  X(0001).                         
             10 PI-DRKI0449-KFZSAISONVON   PIC S9(4) COMP.                      
             10 PI-DRKI0449-KFZSCHLUESSELTYP   PIC  X(0003).                    
             10 PI-DRKI0449-KFZSCHUTZBRIEF   PIC  X(0010).                      
             10 PI-DRKI0449-KFZSCOREDATUM   PIC  X(010).                        
             10 PI-DRKI0449-KFZSCOREWERT   PIC S9(3)V COMP-3.                   
             10 PI-DRKI0449-KFZSFANZKH   PIC S9(4) COMP.                        
             10 PI-DRKI0449-KFZSFANZVK   PIC S9(4) COMP.                        
             10 PI-DRKI0449-KFZSFRFREIKH   PIC  X(0001).                        
             10 PI-DRKI0449-KFZSFRFREIVK   PIC  X(0001).                        
             10 PI-DRKI0449-KFZTARIFAUSWAHL   PIC  X(0001).                     
             10 PI-DRKI0449-KFZTELEMATIKID   PIC S9(14)V COMP-3.                
             10 PI-DRKI0449-KFZVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0449-KFZVERWEND-KNR   PIC S9(15)V COMP-3.                
             10 PI-DRKI0449-KFZVERWEND-KTY   PIC S9(4) COMP.                    
             10 PI-DRKI0449-KFZVOLLKASKOTYPKL   PIC S9(4) COMP.                 
             10 PI-DRKI0449-KFZWERKSTBINDKZ   PIC  X(0001).                     
             10 PI-DRKI0449-KFZWOHNEIGT   PIC  X(0010).                         
             10 PI-DRKI0449-KFZZULASSUNGDATUM   PIC  X(010).                    
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0449-NI1.                                                     
           05 PI-DRKI0449-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 11.                                
       01  PI-DRKI0449-NI7.                                                     
           05 PI-DRKI0449-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 11.                                
                                                                                
      *                                                                         
       01  PO-DRKI0449.                                                         
           05  PO-DRKI0449-RC                     PIC  X(003).                  
               88 PO-DRKI0449-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI0449-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
