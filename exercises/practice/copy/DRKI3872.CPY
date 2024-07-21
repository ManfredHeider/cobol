      *****************************************************************         
      *    COPY-MEMBER: DRKI3872                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3872                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.10.2022 / 10:42:29 / U130684            
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
      *    FÜR DIE FUNKTION DRKI3872                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3872.                                                         
           05  PI-DRKI3872-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3872-BDKKARTENNR   PIC  X(0013).                         
             10 PI-DRKI3872-BDKLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3872-BDKVERTRAGSID   PIC  X(0013).                       
             10 PI-DRKI3872-BDKWAEHRUNG   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3872.                                                         
           05  PO-DRKI3872-RC                     PIC  X(003).                  
               88 PO-DRKI3872-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3872-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3872-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3872-BDKGUELTIGBIS   PIC  X(0004).                       
             10 PO-DRKI3872-BDKINHABERNAME1   PIC  X(0030).                     
             10 PO-DRKI3872-BDKINHABERNAME2   PIC  X(0030).                     
             10 PO-DRKI3872-BDKKARTENART   PIC  X(0003).                        
             10 PO-DRKI3872-BDKNFCKZN   PIC  X(0001).                           
             10 PO-DRKI3872-BDKNFCSTATUS   PIC  X(0001).                        
             10 PO-DRKI3872-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3872-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
