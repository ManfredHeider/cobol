      *****************************************************************
      *    COPY-MEMBER: DRKI2217                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2217                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   erzeugt mit genOZP - (c) PMSmicado  22.09.2004/15:13:25       
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00639!23.11.07!      NEUERSTELLUNG FÜR ZWKI0314!SCHLEICHER*  
      *         !        !      WEGEN 3 NEUER FELDER      !          *
      *DBO-10227!11.08.22!Längenänderung                  !u131774   *
      *         !        !GHBGUTHABENANT von 10 auf 11 B  !          * 
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER            
      *    FÜR DIE FUNKTION DRKI2217                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  INSERT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2217.                                                 
           05  PI-DRKI2217-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI2217-GHBBUCHUNGLFDNR      PIC S9(4) COMP.        
             10 PI-DRKI2217-GHBFONDID            PIC S9(4) COMP.        
             10 PI-DRKI2217-GHBGHALFDNR          PIC S9(4) COMP.        
             10 PI-DRKI2217-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PI-DRKI2217-GHBVERTRAGSID        PIC  X(13).            
                                                                        
                                                                        
           05  PI-DRKI2217-DATEN.                                       
                                                                        
                                                                        
             10 PI-DRKI2217-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2217-GHBBETRLBUWKZ        PIC  X(1).             
             10 PI-DRKI2217-GHBBUCHUNGSDATUM     PIC  X(10).            
             10 PI-DRKI2217-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3. 
             10 PI-DRKI2217-GHBKZFINHLETZTBUID   PIC S9(4) COMP.        
             10 PI-DRKI2217-GHBNETTOBETRAG       PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2217-GHBNETTOBETWKZ       PIC  X(1).             
             10 PI-DRKI2217-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2217-GHBSTLETZTBUWKZ      PIC  X(1).             
             10 PI-DRKI2217-GHBGEBBETRAG         PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2217-GHBGEBSCHLUESSEL     PIC S9(4) COMP.        
             10 PI-DRKI2217-GHBGEBTEXT           PIC  X(20).            
                                                                        
                                                                        
      *                                                                 
       01  PO-DRKI2217.                                                 
           05  PO-DRKI2217-RC                    PIC  X(003).           
               88 PO-DRKI2217-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI2217-RC-DUPLICATE-VALUE           VALUE 'DVA'. 
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
      *---------------------------------------------------------------* 
