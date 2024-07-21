      *****************************************************************         
      *    COPY-MEMBER: DRKI3877                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3877                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.10.2022 / 09:55:41 / U130684            
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
      *    FÜR DIE FUNKTION DRKI3877                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3877.                                                         
           05  PI-DRKI3877-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3877-G48-DKKARTENNR   PIC  X(0013).                      
             10 PI-DRKI3877-G48-DKKONTOUNR   PIC  X(0002).                      
             10 PI-DRKI3877-G48-DKVERTRAGSNR   PIC  X(0015).                    
             10 PI-DRKI3877-G48-DKWAEHRUNG   PIC  X(0003).                      
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3877.                                                         
           05  PO-DRKI3877-RC                     PIC  X(003).                  
               88 PO-DRKI3877-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3877-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3877-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3877-G48-DKDELKZ   PIC  X(0001).                         
             10 PO-DRKI3877-G48-DKGUELTIGBIS   PIC  X(0004).                    
             10 PO-DRKI3877-G48-DKINHNAME1   PIC  X(0027).                      
             10 PO-DRKI3877-G48-DKINHNAME2   PIC  X(0027).                      
             10 PO-DRKI3877-G48-DKKARTENART   PIC  X(0003).                     
             10 PO-DRKI3877-G48-DKNFCFAEHIG   PIC  X(0001).                     
             10 PO-DRKI3877-G48-DKNFCSTATUS   PIC  X(0001).                     
             10 PO-DRKI3877-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3877-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
