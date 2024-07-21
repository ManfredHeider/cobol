      *****************************************************************         
      *    COPY-MEMBER: DRKI3874                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3874                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.10.2022 / 10:42:35 / U130684            
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
      *    FÜR DIE FUNKTION DRKI3874                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3874.                                                         
           05  PI-DRKI3874-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3874-BDKKARTENNR   PIC  X(0013).                         
             10 PI-DRKI3874-BDKLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3874-BDKVERTRAGSID   PIC  X(0013).                       
             10 PI-DRKI3874-BDKWAEHRUNG   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
           05  PI-DRKI3874-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3874-NI-BDKGUELTIGBIS    PIC S9(4) COMP.                 
             15 PI-DRKI3874-NI-BDKINHABERNAME1  PIC S9(4) COMP.                 
             15 PI-DRKI3874-NI-BDKINHABERNAME2  PIC S9(4) COMP.                 
             15 PI-DRKI3874-NI-BDKKARTENART     PIC S9(4) COMP.                 
             15 PI-DRKI3874-NI-BDKNFCKZN        PIC S9(4) COMP.                 
             15 PI-DRKI3874-NI-BDKNFCSTATUS     PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3874-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3874-BDKGUELTIGBIS   PIC  X(0004).                       
             10 PI-DRKI3874-BDKINHABERNAME1   PIC  X(0030).                     
             10 PI-DRKI3874-BDKINHABERNAME2   PIC  X(0030).                     
             10 PI-DRKI3874-BDKKARTENART   PIC  X(0003).                        
             10 PI-DRKI3874-BDKNFCKZN   PIC  X(0001).                           
             10 PI-DRKI3874-BDKNFCSTATUS   PIC  X(0001).                        
             10 PI-DRKI3874-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3874-NI1.                                                     
           05 PI-DRKI3874-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 6.                                     
       01  PI-DRKI3874-NI7.                                                     
           05 PI-DRKI3874-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 6.                                     
      *                                                                         
       01  PO-DRKI3874.                                                         
           05  PO-DRKI3874-RC                     PIC  X(003).                  
               88 PO-DRKI3874-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3874-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
