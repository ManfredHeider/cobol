      *****************************************************************         
      *    COPY-MEMBER: DRKI2238                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2238                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:58:01 / U131774            
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
      *    FÜR DIE FUNKTION DRKI2238                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2238.                                                         
           05  PI-DRKI2238-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2238-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2238-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2238-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2238-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2238-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI2238-NI-GHAANLEGERIDOIF  PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAANTEILEFREI   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAANTEILEVORJ   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAANTEILGEKOIF  PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHNTLGSPF         PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAANTEILVEFOIF  PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHNTRWVRJ13       PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAANTGESPERRT1  PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAANTGESPERRT2  PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAANTGESPERRT3  PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHNSTNDSKRS       PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHFLGNSTKRS       PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAGUTHABEN      PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAGUTHABENANT   PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHHSSZHLNG        PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHHSNZHLNG        PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAKAPITALAKT    PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAKAPITALVORJ   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAKUMAUSZHALBJ  PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAKUMAUTOWA     PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAKUMBARAUSBR   PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHKMBRSBRVJ       PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHKMBRTRGS        PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHKMBRTRVR        PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHKMLSZHLNG       PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHKMLNZHLNG       PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHALIEFERDATUM   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHANRGWINKLBBA   PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHNRGWNKLBBVH     PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHANRGWOHNEBBA   PIC S9(4) COMP.                 
             15 PI-DRKI2238-N-GHNRGWHNBBVH      PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAREALGEWVERL   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHARUECKBRUGES   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHARUECKBRUVORJ  PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHARUECKNAHMEPR  PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAVLSPERRDAT1   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAVLSPERRDAT2   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAVLSPERRDAT3   PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAWPBEZ-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI2238-NI-GHAWPBEZ-KTY     PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2238-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2238-GHAANLEGERIDOIF   PIC  X(0014).                     
             10 PI-DRKI2238-GHAANTEILEFREI   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2238-GHAANTEILEVORJ   PIC S9(6)V9(4) COMP-3.             
             10 PI-DRKI2238-GHAANTEILGEKOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2238-GHAANTEILGESPOIF   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2238-GHAANTEILVEFOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2238-GHAANTERWVORJU13   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2238-GHAANTGESPERRT1   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2238-GHAANTGESPERRT2   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2238-GHAANTGESPERRT3   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2238-GHAEINSTANDSKURS   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2238-GHAFLAGEINSTKURS   PIC  X(0001).                    
             10 PI-DRKI2238-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI2238-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2238-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI2238-GHAHISAUSZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2238-GHAHISEINZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2238-GHAKAPITALAKT   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI2238-GHAKAPITALVORJ   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2238-GHAKUMAUSZHALBJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2238-GHAKUMAUTOWA   PIC S9(11)V9(2) COMP-3.              
             10 PI-DRKI2238-GHAKUMBARAUSBR   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2238-GHAKUMBARAUSBRVJ   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2238-GHAKUMUEBERTRGES   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2238-GHAKUMUEBERTRVOR   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2238-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2238-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2238-GHALIEFERDATUM   PIC  X(010).                       
             10 PI-DRKI2238-GHANRGWINKLBBA   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI2238-GHANRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.           
             10 PI-DRKI2238-GHANRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI2238-GHANRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.           
             10 PI-DRKI2238-GHAREALGEWVERL   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2238-GHARUECKBRUGES   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2238-GHARUECKBRUVORJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2238-GHARUECKNAHMEPR   PIC S9(5)V9(4) COMP-3.            
             10 PI-DRKI2238-GHAVLSPERRDAT1   PIC  X(010).                       
             10 PI-DRKI2238-GHAVLSPERRDAT2   PIC  X(010).                       
             10 PI-DRKI2238-GHAVLSPERRDAT3   PIC  X(010).                       
             10 PI-DRKI2238-GHAWPBEZ-KNR   PIC S9(15)V COMP-3.                  
             10 PI-DRKI2238-GHAWPBEZ-KTY   PIC S9(4) COMP.                      
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2238-NI1.                                                     
           05 PI-DRKI2238-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 40.                                    
       01  PI-DRKI2238-NI7.                                                     
           05 PI-DRKI2238-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 40.                                    
      *                                                                         
       01  PO-DRKI2238.                                                         
           05  PO-DRKI2238-RC                     PIC  X(003).                  
               88 PO-DRKI2238-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI2238-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
