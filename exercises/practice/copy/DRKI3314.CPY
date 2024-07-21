      *****************************************************************         
      *    COPY-MEMBER: DRKI3314                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3314                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         13.09.2022 / 13:23:33 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3314                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3314.                                                         
           05  PI-DRKI3314-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3314-GVDLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3314-GVDVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3314.                                                         
           05  PO-DRKI3314-RC                     PIC  X(003).                  
               88 PO-DRKI3314-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3314-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3314-NI.                                                  
               10 NI-DRKI3314-TAB.                                              
                  15 DRKI3314-ELE                                               
                     PIC S9(04) COMP OCCURS 29 TIMES.                           
               10 NI-DRKI3314 REDEFINES NI-DRKI3314-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI3314-NI-GVDABLAUF   PIC S9(4) COMP.                      
             15 PO-DRKI3314-NI-GVDAKTVWERT   PIC S9(4) COMP.                    
             15 PO-DRKI3314-NI-GVDAMTKZ   PIC S9(4) COMP.                       
             15 PO-DRKI3314-NI-GVDBEGINN   PIC S9(4) COMP.                      
             15 PO-DRKI3314-NI-GVDBERECHDAT   PIC S9(4) COMP.                   
             15 PO-DRKI3314-NI-GVDERLFALL-SUM   PIC S9(4) COMP.                 
             15 PO-DRKI3314-NI-GVDFLAECHE   PIC S9(4) COMP.                     
             15 PO-DRKI3314-NI-GVDFONDSGUTH   PIC S9(4) COMP.                   
             15 PO-DRKI3314-NI-GVDGESCHAEFTSZW   PIC S9(4) COMP.                
             15 PO-DRKI3314-NI-GVDGUTHABEN   PIC S9(4) COMP.                    
             15 PO-DRKI3314-NI-GVDKAPABFIND   PIC S9(4) COMP.                   
             15 PO-DRKI3314-NI-GVDLEISTUNG-KZN   PIC S9(4) COMP.                
             15 PO-DRKI3314-NI-GVDLVART   PIC S9(4) COMP.                       
             15 PO-DRKI3314-NI-GVDMIG-KZN   PIC S9(4) COMP.                     
             15 PO-DRKI3314-NI-GVDNJBEITRAG   PIC S9(4) COMP.                   
             15 PO-DRKI3314-NI-GVDPRODKOMBI   PIC S9(4) COMP.                   
             15 PO-DRKI3314-NI-GVDPRODUKT   PIC S9(4) COMP.                     
             15 PO-DRKI3314-NI-GVDRENTEGARANT   PIC S9(4) COMP.                 
             15 PO-DRKI3314-NI-GVDRENTENBEZUG   PIC S9(4) COMP.                 
             15 PO-DRKI3314-NI-GVDRENTEVORAUS   PIC S9(4) COMP.                 
             15 PO-DRKI3314-NI-GVDRNTFAKTOR   PIC S9(4) COMP.                   
             15 PO-DRKI3314-NI-GVDRNTZAHLW-KNR   PIC S9(4) COMP.                
             15 PO-DRKI3314-NI-GVDRNTZAHLW-KTY   PIC S9(4) COMP.                
             15 PO-DRKI3314-NI-GVDSUMME   PIC S9(4) COMP.                       
             15 PO-DRKI3314-NI-GVDTARIF   PIC S9(4) COMP.                       
             15 PO-DRKI3314-NI-GVDTARPROD-KNR   PIC S9(4) COMP.                 
             15 PO-DRKI3314-NI-GVDTARPROD-KTY   PIC S9(4) COMP.                 
             15 PO-DRKI3314-NI-GVDTODFALL-SUM   PIC S9(4) COMP.                 
             15 PO-DRKI3314-NI-GVDWAGNISKZ   PIC S9(4) COMP.                    
END   *                                                                         
                                                                                
           05  PO-DRKI3314-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3314-GVDABLAUF   PIC  X(010).                            
             10 PO-DRKI3314-GVDAKTVWERT   PIC S9(8)V9(2) COMP-3.                
             10 PO-DRKI3314-GVDAMTKZ   PIC  X(0012).                            
             10 PO-DRKI3314-GVDBEGINN   PIC  X(010).                            
             10 PO-DRKI3314-GVDBEITRAG   PIC S9(8)V9(2) COMP-3.                 
             10 PO-DRKI3314-GVDBERECHDAT   PIC  X(010).                         
             10 PO-DRKI3314-GVDERLFALL-SUM   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI3314-GVDFLAECHE   PIC S9(9) COMP.                        
             10 PO-DRKI3314-GVDFONDSGUTH   PIC S9(9)V9(2) COMP-3.               
             10 PO-DRKI3314-GVDGESCHAEFTSZW   PIC  X(0002).                     
             10 PO-DRKI3314-GVDGUTHABEN   PIC S9(8)V9(2) COMP-3.                
             10 PO-DRKI3314-GVDKAPABFIND   PIC S9(9)V9(2) COMP-3.               
             10 PO-DRKI3314-GVDLEISTUNG-KZN   PIC  X(0001).                     
             10 PO-DRKI3314-GVDLVART   PIC S9(4) COMP.                          
             10 PO-DRKI3314-GVDMIG-KZN   PIC  X(0001).                          
             10 PO-DRKI3314-GVDNJBEITRAG   PIC S9(8)V9(2) COMP-3.               
             10 PO-DRKI3314-GVDPRODKOMBI   PIC S9(9) COMP.                      
             10 PO-DRKI3314-GVDPRODUKT   PIC  X(0050).                          
             10 PO-DRKI3314-GVDPRODUKTID   PIC S9(4) COMP.                      
             10 PO-DRKI3314-GVDRENTEGARANT   PIC S9(8)V9(2) COMP-3.             
             10 PO-DRKI3314-GVDRENTENBEZUG   PIC  X(0001).                      
             10 PO-DRKI3314-GVDRENTEVORAUS   PIC S9(8)V9(2) COMP-3.             
             10 PO-DRKI3314-GVDRNTFAKTOR   PIC S9(5)V9(2) COMP-3.               
             10 PO-DRKI3314-GVDRNTZAHLW-KNR   PIC S9(4) COMP.                   
             10 PO-DRKI3314-GVDRNTZAHLW-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI3314-GVDSPARTE   PIC  X(0003).                           
             10 PO-DRKI3314-GVDSUMME   PIC S9(8)V9(2) COMP-3.                   
             10 PO-DRKI3314-GVDTARIF   PIC  X(0050).                            
             10 PO-DRKI3314-GVDTARPROD-KNR   PIC S9(15)V COMP-3.                
             10 PO-DRKI3314-GVDTARPROD-KTY   PIC S9(4) COMP.                    
             10 PO-DRKI3314-GVDTODFALL-SUM   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI3314-GVDWAGNISKZ   PIC S9(4) COMP.                       
             10 PO-DRKI3314-LETZTEAENDERUNG   PIC  X(0026).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
