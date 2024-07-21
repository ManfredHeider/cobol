      *****************************************************************         
      *    COPY-MEMBER: DRKI3213                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3213                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         02.08.2022 / 13:19:29 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3213                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3213.                                                         
           05  PI-DRKI3213-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3213-LPVVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3213.                                                         
           05  PO-DRKI3213-RC                     PIC  X(003).                  
               88 PO-DRKI3213-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3213-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3213-NI.                                                  
               10 NI-DRKI3213-TAB.                                              
                  15 DRKI3213-ELE                                               
                     PIC S9(04) COMP OCCURS 4 TIMES.                            
               10 NI-DRKI3213 REDEFINES NI-DRKI3213-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI3213-NI-LPVRNTBEZUG-KZN   PIC S9(4) COMP.                
             15 PO-DRKI3213-NI-LPVSTVD0067-KNR   PIC S9(4) COMP.                
             15 PO-DRKI3213-NI-LPVSTVD0067-KTY   PIC S9(4) COMP.                
             15 PO-DRKI3213-NI-LPVVZUST-D-DAT   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PO-DRKI3213-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3213-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3213-LPVBAV-KZN   PIC  X(0001).                          
             10 PO-DRKI3213-LPVGSMTTAR-BEZ   PIC  X(0025).                      
             10 PO-DRKI3213-LPVPRODKLAS-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI3213-LPVPRODKLAS-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI3213-LPVPRODUKTID   PIC S9(4) COMP.                      
             10 PO-DRKI3213-LPVRNTBEZUG-KZN   PIC  X(0001).                     
             10 PO-DRKI3213-LPVSTVD0067-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI3213-LPVSTVD0067-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI3213-LPVVZUST-D-DAT   PIC  X(010).                       
             10 PO-DRKI3213-LPVVZUST-D-KNR   PIC S9(4) COMP.                    
             10 PO-DRKI3213-LPVVZUST-D-KTY   PIC S9(4) COMP.                    
             10 PO-DRKI3213-LPVWERBEBEZ-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI3213-LPVWERBEBEZ-KTY   PIC S9(4) COMP.                   
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
