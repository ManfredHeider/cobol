      *****************************************************************         
      *    COPY-MEMBER: DRKI3214                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3214                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         02.08.2022 / 13:19:30 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3214                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3214.                                                         
           05  PI-DRKI3214-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3214-LPVVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3214-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3214-NI-LPVRNTBEZUG-KZN  PIC S9(4) COMP.                 
             15 PI-DRKI3214-NI-LPVSTVD0067-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI3214-NI-LPVSTVD0067-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI3214-NI-LPVVZUST-D-DAT   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3214-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3214-LPVBAV-KZN   PIC  X(0001).                          
             10 PI-DRKI3214-LPVGSMTTAR-BEZ   PIC  X(0025).                      
             10 PI-DRKI3214-LPVPRODKLAS-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3214-LPVPRODUKTID   PIC S9(4) COMP.                      
             10 PI-DRKI3214-LPVRNTBEZUG-KZN   PIC  X(0001).                     
             10 PI-DRKI3214-LPVSTVD0067-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI3214-LPVSTVD0067-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI3214-LPVVZUST-D-DAT   PIC  X(010).                       
             10 PI-DRKI3214-LPVVZUST-D-KNR   PIC S9(4) COMP.                    
             10 PI-DRKI3214-LPVWERBEBEZ-KNR   PIC S9(15)V COMP-3.               
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3214-NI1.                                                     
           05 PI-DRKI3214-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 4.                                 
       01  PI-DRKI3214-NI7.                                                     
           05 PI-DRKI3214-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 4.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3214.                                                         
           05  PO-DRKI3214-RC                     PIC  X(003).                  
               88 PO-DRKI3214-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3214-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
