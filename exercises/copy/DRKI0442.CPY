      *****************************************************************         
      *    COPY-MEMBER: DRKI0442                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0442                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         25.05.2022 / 13:31:27 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI0442                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0442.                                                         
           05  PI-DRKI0442-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0442-KFZVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0442-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0442-N-KFZFHRZGTYPD      PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZGEBDAT-VN     PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZGESGEW        PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZKW            PIC S9(4) COMP.                 
             15 PI-DRKI0442-N-KFZLSTNGSRTD      PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZNUTZ          PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZPAKET-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZPAKET-KTY     PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZPLATZ         PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZSCOREWERT     PIC S9(4) COMP.                 
             15 PI-DRKI0442-NI-KFZTELEMATIKID   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0442-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0442-KFZ-E-FZG-KZ   PIC  X(0001).                        
             10 PI-DRKI0442-KFZABWFAHRLSTJH   PIC S9(7)V COMP-3.                
             10 PI-DRKI0442-KFZAGBRANCHE   PIC  X(0060).                        
             10 PI-DRKI0442-KFZAKZART-KNR   PIC S9(15)V COMP-3.                 
             10 PI-DRKI0442-KFZAKZART-KTY   PIC S9(4) COMP.                     
             10 PI-DRKI0442-KFZALTERSGRP-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0442-KFZALTERSGRP-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0442-KFZAMTLICHESKENNZ   PIC  X(0012).                   
             10 PI-DRKI0442-KFZAUFBAUART-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0442-KFZAUFBAUART-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0442-KFZBEGFAHRERSCHL   PIC S9(4) COMP.                  
             10 PI-DRKI0442-KFZBEGFAHRERTEXT   PIC  X(0010).                    
             10 PI-DRKI0442-KFZBEITRANPASSAM   PIC  X(010).                     
             10 PI-DRKI0442-KFZBEITRGESAMALT   PIC S9(5)V9(2) COMP-3.           
             10 PI-DRKI0442-KFZBEITRGESAMNEU   PIC S9(5)V9(2) COMP-3.           
             10 PI-DRKI0442-KFZBERECHNETAM   PIC  X(010).                       
             10 PI-DRKI0442-KFZDATKMSTANDAKT   PIC  X(010).                     
             10 PI-DRKI0442-KFZERSTZULDATUMVN   PIC  X(010).                    
             10 PI-DRKI0442-KFZFAHRER   PIC  X(0010).                           
             10 PI-DRKI0442-KFZFAHRGEBIET   PIC  X(0010).                       
             10 PI-DRKI0442-KFZFAHRGESTELLNR   PIC  X(0020).                    
             10 PI-DRKI0442-KFZFAHRZEUGTYPID   PIC S9(4) COMP.                  
             10 PI-DRKI0442-KFZFALTERMAX-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI0442-KFZFALTERMAX-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0442-KFZFIRMENNR   PIC  X(0006).                         
             10 PI-DRKI0442-KFZFSERWERB   PIC  X(0010).                         
             10 PI-DRKI0442-KFZFZGABSTELLPLATZ   PIC  X(0010).                  
             10 PI-DRKI0442-KFZFZGART-KNR   PIC S9(15)V COMP-3.                 
             10 PI-DRKI0442-KFZFZGART-KTY   PIC S9(4) COMP.                     
             10 PI-DRKI0442-KFZFZGNUTZUNG   PIC  X(0010).                       
             10 PI-DRKI0442-KFZGEBDAT-VN   PIC  X(010).                         
             10 PI-DRKI0442-KFZGEBDATFAHRER   PIC  X(010).                      
             10 PI-DRKI0442-KFZGEBDATFAHRERIN   PIC  X(010).                    
             10 PI-DRKI0442-KFZGESAMTNEUWERTFZ   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI0442-KFZGESGEW   PIC S9(7)V COMP-3.                      
             10 PI-DRKI0442-KFZGESNEUWERTWKZ   PIC  X(0001).                    
             10 PI-DRKI0442-KFZHAFTPFLTYPKL   PIC S9(4) COMP.                   
             10 PI-DRKI0442-KFZHALTERNNAME   PIC  X(0040).                      
             10 PI-DRKI0442-KFZHALTERSCHL   PIC S9(4) COMP.                     
             10 PI-DRKI0442-KFZHALTERTEXT   PIC  X(0010).                       
             10 PI-DRKI0442-KFZHALTERVNAME   PIC  X(0040).                      
             10 PI-DRKI0442-KFZHERSTELLERSCHL   PIC  X(0004).                   
             10 PI-DRKI0442-KFZHERSTELLTEXT   PIC  X(0025).                     
             10 PI-DRKI0442-KFZHERSTMODTEXT   PIC  X(0020).                     
             10 PI-DRKI0442-KFZJAEHRLFAHRL   PIC S9(9) COMP.                    
             10 PI-DRKI0442-KFZKMSTAND   PIC S9(9) COMP.                        
             10 PI-DRKI0442-KFZKMSTANDAKT   PIC S9(7)V COMP-3.                  
             10 PI-DRKI0442-KFZKW   PIC S9(4)V COMP-3.                          
             10 PI-DRKI0442-KFZLEASING   PIC S9(4) COMP.                        
             10 PI-DRKI0442-KFZLEISTUNG   PIC S9(5)V9(2) COMP-3.                
             10 PI-DRKI0442-KFZLEISTUNGSARTID   PIC S9(4) COMP.                 
             10 PI-DRKI0442-KFZLEISTUNGTEXT   PIC  X(0012).                     
             10 PI-DRKI0442-KFZLSVSCOR   PIC  X(0001).                          
             10 PI-DRKI0442-KFZNAEHSPEZ-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0442-KFZNAEHSPEZ-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0442-KFZNUTZ   PIC S9(7)V COMP-3.                        
             10 PI-DRKI0442-KFZPAKET-KNR   PIC S9(15)V COMP-3.                  
             10 PI-DRKI0442-KFZPAKET-KTY   PIC S9(4) COMP.                      
             10 PI-DRKI0442-KFZPLATZ   PIC S9(3)V COMP-3.                       
             10 PI-DRKI0442-KFZPSFANZKH   PIC S9(4) COMP.                       
             10 PI-DRKI0442-KFZPSFANZVK   PIC S9(4) COMP.                       
             10 PI-DRKI0442-KFZSAISONBIS   PIC S9(4) COMP.                      
             10 PI-DRKI0442-KFZSAISONKZ   PIC  X(0001).                         
             10 PI-DRKI0442-KFZSAISONVON   PIC S9(4) COMP.                      
             10 PI-DRKI0442-KFZSCHLUESSELTYP   PIC  X(0003).                    
             10 PI-DRKI0442-KFZSCHUTZBRIEF   PIC  X(0010).                      
             10 PI-DRKI0442-KFZSCOREDATUM   PIC  X(010).                        
             10 PI-DRKI0442-KFZSCOREWERT   PIC S9(3)V COMP-3.                   
             10 PI-DRKI0442-KFZSFANZKH   PIC S9(4) COMP.                        
             10 PI-DRKI0442-KFZSFANZVK   PIC S9(4) COMP.                        
             10 PI-DRKI0442-KFZSFRFREIKH   PIC  X(0001).                        
             10 PI-DRKI0442-KFZSFRFREIVK   PIC  X(0001).                        
             10 PI-DRKI0442-KFZTARIFAUSWAHL   PIC  X(0001).                     
             10 PI-DRKI0442-KFZTELEMATIKID   PIC S9(14)V COMP-3.                
             10 PI-DRKI0442-KFZVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0442-KFZVERWEND-KNR   PIC S9(15)V COMP-3.                
             10 PI-DRKI0442-KFZVERWEND-KTY   PIC S9(4) COMP.                    
             10 PI-DRKI0442-KFZVOLLKASKOTYPKL   PIC S9(4) COMP.                 
             10 PI-DRKI0442-KFZWERKSTBINDKZ   PIC  X(0001).                     
             10 PI-DRKI0442-KFZWOHNEIGT   PIC  X(0010).                         
             10 PI-DRKI0442-KFZZULASSUNGDATUM   PIC  X(010).                    
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0442-NI1.                                                     
           05 PI-DRKI0442-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 11.                                    
       01  PI-DRKI0442-NI7.                                                     
           05 PI-DRKI0442-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 11.                                    
      *                                                                         
       01  PO-DRKI0442.                                                         
           05  PO-DRKI0442-RC                     PIC  X(003).                  
               88 PO-DRKI0442-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0442-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
