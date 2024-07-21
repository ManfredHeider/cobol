      *****************************************************************         
      *    COPY-MEMBER: DRKI3315                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3315                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         14.09.2022 / 12:30:22 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3315                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3315.                                                         
           05  PI-DRKI3315-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3315-GVDLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3315-GVDVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3315-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3315-NI-GVDABLAUF        PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDAKTVWERT      PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDAMTKZ         PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDBEGINN        PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDBERECHDAT     PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDERLFALL-SUM   PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDFLAECHE       PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDFONDSGUTH     PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDGESCHAEFTSZW  PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDGUTHABEN      PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDKAPABFIND     PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDLEISTUNG-KZN  PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDLVART         PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDMIG-KZN       PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDNJBEITRAG     PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDPRODKOMBI     PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDPRODUKT       PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDRENTEGARANT   PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDRENTENBEZUG   PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDRENTEVORAUS   PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDRNTFAKTOR     PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDRNTZAHLW-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDRNTZAHLW-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDSUMME         PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDTARIF         PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDTARPROD-KNR   PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDTARPROD-KTY   PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDTODFALL-SUM   PIC S9(4) COMP.                 
             15 PI-DRKI3315-NI-GVDWAGNISKZ      PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3315-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3315-GVDABLAUF   PIC  X(010).                            
             10 PI-DRKI3315-GVDAKTVWERT   PIC S9(8)V9(2) COMP-3.                
             10 PI-DRKI3315-GVDAMTKZ   PIC  X(0012).                            
             10 PI-DRKI3315-GVDBEGINN   PIC  X(010).                            
             10 PI-DRKI3315-GVDBEITRAG   PIC S9(8)V9(2) COMP-3.                 
             10 PI-DRKI3315-GVDBERECHDAT   PIC  X(010).                         
             10 PI-DRKI3315-GVDERLFALL-SUM   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3315-GVDFLAECHE   PIC S9(9) COMP.                        
             10 PI-DRKI3315-GVDFONDSGUTH   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3315-GVDGESCHAEFTSZW   PIC  X(0002).                     
             10 PI-DRKI3315-GVDGUTHABEN   PIC S9(8)V9(2) COMP-3.                
             10 PI-DRKI3315-GVDKAPABFIND   PIC S9(9)V9(2) COMP-3.               
             10 PI-DRKI3315-GVDLEISTUNG-KZN   PIC  X(0001).                     
             10 PI-DRKI3315-GVDLVART   PIC S9(4) COMP.                          
             10 PI-DRKI3315-GVDMIG-KZN   PIC  X(0001).                          
             10 PI-DRKI3315-GVDNJBEITRAG   PIC S9(8)V9(2) COMP-3.               
             10 PI-DRKI3315-GVDPRODKOMBI   PIC S9(9) COMP.                      
             10 PI-DRKI3315-GVDPRODUKT   PIC  X(0050).                          
             10 PI-DRKI3315-GVDPRODUKTID   PIC S9(4) COMP.                      
             10 PI-DRKI3315-GVDRENTEGARANT   PIC S9(8)V9(2) COMP-3.             
             10 PI-DRKI3315-GVDRENTENBEZUG   PIC  X(0001).                      
             10 PI-DRKI3315-GVDRENTEVORAUS   PIC S9(8)V9(2) COMP-3.             
             10 PI-DRKI3315-GVDRNTFAKTOR   PIC S9(5)V9(2) COMP-3.               
             10 PI-DRKI3315-GVDRNTZAHLW-KNR   PIC S9(4) COMP.                   
             10 PI-DRKI3315-GVDRNTZAHLW-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI3315-GVDSPARTE   PIC  X(0003).                           
             10 PI-DRKI3315-GVDSUMME   PIC S9(8)V9(2) COMP-3.                   
             10 PI-DRKI3315-GVDTARIF   PIC  X(0050).                            
             10 PI-DRKI3315-GVDTARPROD-KNR   PIC S9(15)V COMP-3.                
             10 PI-DRKI3315-GVDTARPROD-KTY   PIC S9(4) COMP.                    
             10 PI-DRKI3315-GVDTODFALL-SUM   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3315-GVDWAGNISKZ   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3315-NI1.                                                     
           05 PI-DRKI3315-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 29.                                
       01  PI-DRKI3315-NI7.                                                     
           05 PI-DRKI3315-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 29.                                
                                                                                
      *                                                                         
       01  PO-DRKI3315.                                                         
           05  PO-DRKI3315-RC                     PIC  X(003).                  
               88 PO-DRKI3315-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3315-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
