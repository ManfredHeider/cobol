      *****************************************************************         
      *    COPY-MEMBER: DRKI3528                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3528                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         24.08.2022 / 10:44:22 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3528                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3528.                                                         
           05  PI-DRKI3528-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3528-G21VVVNR   PIC  X(0015).                            
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3528.                                                         
           05  PO-DRKI3528-RC                     PIC  X(003).                  
               88 PO-DRKI3528-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3528-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3528-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3528-G21VVAENDKZN   PIC  X(0001).                        
             10 PO-DRKI3528-G21VVBERECH-DAT   PIC  X(0010).                     
             10 PO-DRKI3528-G21VVERLFALL-SUM   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI3528-G21VVFONDSGUTH   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI3528-G21VVKAPABFIND   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI3528-G21VVLEISTUNG-KZN   PIC  X(0001).                   
             10 PO-DRKI3528-G21VVMANDANT   PIC S9(9) COMP.                      
             10 PO-DRKI3528-G21VVNJBEITRAG   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI3528-G21VVPRODUKT   PIC  X(0050).                        
             10 PO-DRKI3528-G21VVRENTEGARANT   PIC S9(7)V COMP-3.               
             10 PO-DRKI3528-G21VVRENTENBEZUG   PIC  X(0001).                    
             10 PO-DRKI3528-G21VVRENTEVORAUS   PIC S9(7)V COMP-3.               
             10 PO-DRKI3528-G21VVRNTFAKTOR   PIC S9(5)V9(2) COMP-3.             
             10 PO-DRKI3528-G21VVRNTZAHLW   PIC S9(4) COMP.                     
             10 PO-DRKI3528-G21VVSPARTE   PIC  X(0003).                         
             10 PO-DRKI3528-G21VVTARIF   PIC  X(0050).                          
             10 PO-DRKI3528-G21VVTODFALL-SUM   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI3528-G21VVVBNR   PIC S9(9) COMP.                         
             10 PO-DRKI3528-G21VVVERARB-KZN   PIC  X(0001).                     
             10 PO-DRKI3528-G21VVVERTRABLAUF   PIC  X(0010).                    
             10 PO-DRKI3528-G21VVVERTRBEGINN   PIC  X(0010).                    
             10 PO-DRKI3528-G21VVVERTRZUSTAND   PIC S9(4) COMP.                 
             10 PO-DRKI3528-G21VVZAHLBEITRAG   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI3528-G21VVZAHLWEISE   PIC S9(4) COMP.                    
             10 PO-DRKI3528-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3528-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
