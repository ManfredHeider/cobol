      *****************************************************************         
      *    COPY-MEMBER: DRKI2755                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2755                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         15.11.2021 / 15:47:26 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI2755                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2755.                                                         
           05  PI-DRKI2755-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2755-VZALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2755-VZAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI2755.                                                         
           05  PO-DRKI2755-RC                     PIC  X(003).                  
               88 PO-DRKI2755-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI2755-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI2755-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI2755-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI2755-VZAANZMONVERARB   PIC S9(4) COMP.                   
             10 PO-DRKI2755-VZABEGINN   PIC  X(010).                            
             10 PO-DRKI2755-VZADAUER   PIC S9(4) COMP.                          
             10 PO-DRKI2755-VZAERSTELLTDAT   PIC  X(010).                       
             10 PO-DRKI2755-VZAKLBERHAUSSCHL   PIC  X(0001).                    
             10 PO-DRKI2755-VZALFDMONRECHPGM   PIC S9(4) COMP.                  
             10 PO-DRKI2755-VZATSCHICHTNR   PIC S9(4) COMP.                     
             10 PO-DRKI2755-VZAZAHLBETRAG   PIC S9(11)V9(2) COMP-3.             
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
