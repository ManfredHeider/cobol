      *****************************************************************         
      *    COPY-MEMBER: DRKI3316                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3316                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         14.09.2022 / 12:30:25 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3316                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3316.                                                         
           05  PI-DRKI3316-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3316-GVDLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3316-GVDVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3316-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3316-NI-GVDABLAUF        PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDAKTVWERT      PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDAMTKZ         PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDBEGINN        PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDBEITRAG       PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDBERECHDAT     PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDERLFALL-SUM   PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDFLAECHE       PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDFONDSGUTH     PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDGESCHAEFTSZW  PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDGUTHABEN      PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDKAPABFIND     PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDLEISTUNG-KZN  PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDLVART         PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDMIG-KZN       PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDNJBEITRAG     PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDPRODKOMBI     PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDPRODUKT       PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDPRODUKTID     PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDRENTEGARANT   PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDRENTENBEZUG   PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDRENTEVORAUS   PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDRNTFAKTOR     PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDRNTZAHLW-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDRNTZAHLW-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDSPARTE        PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDSUMME         PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDTARIF         PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDTARPROD-KNR   PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDTARPROD-KTY   PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDTODFALL-SUM   PIC S9(4) COMP.                 
             15 PI-DRKI3316-NI-GVDWAGNISKZ      PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3316-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3316-GVDABLAUF   PIC  X(010).                            
             10 PI-DRKI3316-GVDAKTVWERT   PIC S9(8)V9(2) COMP-3.                
             10 PI-DRKI3316-GVDAMTKZ   PIC  X(0012).                            
             10 PI-DRKI3316-GVDBEGINN   PIC  X(010).                            
             10 PI-DRKI3316-GVDBEITRAG   PIC S9(8)V9(2) COMP-3.                 
             10 PI-DRKI3316-GVDBERECHDAT   PIC  X(010).                         
             10 PI-DRKI3316-GVDERLFALL-SUM   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3316-GVDFLAECHE   PIC S9(9) COMP.                        
             10 PI-DRKI3316-GVDFONDSGUTH   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3316-GVDGESCHAEFTSZW   PIC  X(0002).                     
             10 PI-DRKI3316-GVDGUTHABEN   PIC S9(8)V9(2) COMP-3.                
             10 PI-DRKI3316-GVDKAPABFIND   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3316-GVDLEISTUNG-KZN   PIC  X(0001).                     
             10 PI-DRKI3316-GVDLVART   PIC S9(4) COMP.                          
             10 PI-DRKI3316-GVDMIG-KZN   PIC  X(0001).                          
             10 PI-DRKI3316-GVDNJBEITRAG   PIC S9(8)V9(2) COMP-3.               
             10 PI-DRKI3316-GVDPRODKOMBI   PIC S9(9) COMP.                      
             10 PI-DRKI3316-GVDPRODUKT   PIC  X(0050).                          
             10 PI-DRKI3316-GVDPRODUKTID   PIC S9(4) COMP.                      
             10 PI-DRKI3316-GVDRENTEGARANT   PIC S9(8)V9(2) COMP-3.             
             10 PI-DRKI3316-GVDRENTENBEZUG   PIC  X(0001).                      
             10 PI-DRKI3316-GVDRENTEVORAUS   PIC S9(8)V9(2) COMP-3.             
             10 PI-DRKI3316-GVDRNTFAKTOR   PIC S9(5)V9(2) COMP-3.               
             10 PI-DRKI3316-GVDRNTZAHLW-KNR   PIC S9(4) COMP.                   
             10 PI-DRKI3316-GVDRNTZAHLW-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI3316-GVDSPARTE   PIC  X(0003).                           
             10 PI-DRKI3316-GVDSUMME   PIC S9(8)V9(2) COMP-3.                   
             10 PI-DRKI3316-GVDTARIF   PIC  X(0050).                            
             10 PI-DRKI3316-GVDTARPROD-KNR   PIC S9(15)V COMP-3.                
             10 PI-DRKI3316-GVDTARPROD-KTY   PIC S9(4) COMP.                    
             10 PI-DRKI3316-GVDTODFALL-SUM   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3316-GVDWAGNISKZ   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3316-NI1.                                                     
           05 PI-DRKI3316-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 32.                                    
       01  PI-DRKI3316-NI7.                                                     
           05 PI-DRKI3316-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 32.                                    
      *                                                                         
       01  PO-DRKI3316.                                                         
           05  PO-DRKI3316-RC                     PIC  X(003).                  
               88 PO-DRKI3316-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3316-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
