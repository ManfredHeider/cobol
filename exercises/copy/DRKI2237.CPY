      *****************************************************************         
      *    COPY-MEMBER: DRKI2237                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2237                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:57:59 / U131774            
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
      *    FÜR DIE FUNKTION DRKI2237                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2237.                                                         
           05  PI-DRKI2237-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2237-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2237-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2237-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2237-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2237-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI2237-NI-GHAANLEGERIDOIF  PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAANTEILEFREI   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAANTEILEVORJ   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAANTEILGEKOIF  PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHNTLGSPF         PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAANTEILVEFOIF  PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHNTRWVRJ13       PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAANTGESPERRT1  PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAANTGESPERRT2  PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAANTGESPERRT3  PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHNSTNDSKRS       PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHFLGNSTKRS       PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAGUTHABEN      PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAGUTHABENANT   PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHHSSZHLNG        PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHHSNZHLNG        PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAKAPITALAKT    PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAKAPITALVORJ   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAKUMAUSZHALBJ  PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAKUMAUTOWA     PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAKUMBARAUSBR   PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHKMBRSBRVJ       PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHKMBRTRGS        PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHKMBRTRVR        PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHKMLSZHLNG       PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHKMLNZHLNG       PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHALIEFERDATUM   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHANRGWINKLBBA   PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHNRGWNKLBBVH     PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHANRGWOHNEBBA   PIC S9(4) COMP.                 
             15 PI-DRKI2237-N-GHNRGWHNBBVH      PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAREALGEWVERL   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHARUECKBRUGES   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHARUECKBRUVORJ  PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHARUECKNAHMEPR  PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAVLSPERRDAT1   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAVLSPERRDAT2   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAVLSPERRDAT3   PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAWPBEZ-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI2237-NI-GHAWPBEZ-KTY     PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2237-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2237-GHAANLEGERIDOIF   PIC  X(0014).                     
             10 PI-DRKI2237-GHAANTEILEFREI   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2237-GHAANTEILEVORJ   PIC S9(6)V9(4) COMP-3.             
             10 PI-DRKI2237-GHAANTEILGEKOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2237-GHAANTEILGESPOIF   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2237-GHAANTEILVEFOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2237-GHAANTERWVORJU13   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2237-GHAANTGESPERRT1   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2237-GHAANTGESPERRT2   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2237-GHAANTGESPERRT3   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2237-GHAEINSTANDSKURS   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2237-GHAFLAGEINSTKURS   PIC  X(0001).                    
             10 PI-DRKI2237-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI2237-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2237-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI2237-GHAHISAUSZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2237-GHAHISEINZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2237-GHAKAPITALAKT   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI2237-GHAKAPITALVORJ   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2237-GHAKUMAUSZHALBJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2237-GHAKUMAUTOWA   PIC S9(11)V9(2) COMP-3.              
             10 PI-DRKI2237-GHAKUMBARAUSBR   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2237-GHAKUMBARAUSBRVJ   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2237-GHAKUMUEBERTRGES   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2237-GHAKUMUEBERTRVOR   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2237-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2237-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2237-GHALIEFERDATUM   PIC  X(010).                       
             10 PI-DRKI2237-GHANRGWINKLBBA   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI2237-GHANRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.           
             10 PI-DRKI2237-GHANRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI2237-GHANRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.           
             10 PI-DRKI2237-GHAREALGEWVERL   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2237-GHARUECKBRUGES   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2237-GHARUECKBRUVORJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2237-GHARUECKNAHMEPR   PIC S9(5)V9(4) COMP-3.            
             10 PI-DRKI2237-GHAVLSPERRDAT1   PIC  X(010).                       
             10 PI-DRKI2237-GHAVLSPERRDAT2   PIC  X(010).                       
             10 PI-DRKI2237-GHAVLSPERRDAT3   PIC  X(010).                       
             10 PI-DRKI2237-GHAWPBEZ-KNR   PIC S9(15)V COMP-3.                  
             10 PI-DRKI2237-GHAWPBEZ-KTY   PIC S9(4) COMP.                      
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2237-NI1.                                                     
           05 PI-DRKI2237-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 40.                                
       01  PI-DRKI2237-NI7.                                                     
           05 PI-DRKI2237-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 40.                                
                                                                                
      *                                                                         
       01  PO-DRKI2237.                                                         
           05  PO-DRKI2237-RC                     PIC  X(003).                  
               88 PO-DRKI2237-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI2237-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
