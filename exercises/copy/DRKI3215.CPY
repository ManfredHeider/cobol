      *****************************************************************         
      *    COPY-MEMBER: DRKI3215                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3215                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         02.08.2022 / 13:19:32 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3215                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3215.                                                         
           05  PI-DRKI3215-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3215-LPVVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3215-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3215-NI-LPVBAV-KZN       PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVGSMTTAR-BEZ   PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVPRODKLAS-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVPRODUKTID     PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVRNTBEZUG-KZN  PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVSTVD0067-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVSTVD0067-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVVZUST-D-DAT   PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVVZUST-D-KNR   PIC S9(4) COMP.                 
             15 PI-DRKI3215-NI-LPVWERBEBEZ-KNR  PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3215-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3215-LPVBAV-KZN   PIC  X(0001).                          
             10 PI-DRKI3215-LPVGSMTTAR-BEZ   PIC  X(0025).                      
             10 PI-DRKI3215-LPVPRODKLAS-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3215-LPVPRODUKTID   PIC S9(4) COMP.                      
             10 PI-DRKI3215-LPVRNTBEZUG-KZN   PIC  X(0001).                     
             10 PI-DRKI3215-LPVSTVD0067-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3215-LPVSTVD0067-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI3215-LPVVZUST-D-DAT   PIC  X(010).                       
             10 PI-DRKI3215-LPVVZUST-D-KNR   PIC S9(4) COMP.                    
             10 PI-DRKI3215-LPVWERBEBEZ-KNR   PIC S9(15)V COMP-3.               
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3215-NI1.                                                     
           05 PI-DRKI3215-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 10.                                    
       01  PI-DRKI3215-NI7.                                                     
           05 PI-DRKI3215-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 10.                                    
      *                                                                         
       01  PO-DRKI3215.                                                         
           05  PO-DRKI3215-RC                     PIC  X(003).                  
               88 PO-DRKI3215-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3215-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
