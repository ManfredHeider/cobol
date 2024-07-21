      *****************************************************************
      *    COPY-MEMBER: DRKI2218                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2218                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   erzeugt mit genOZP - (c) PMSmicado  22.09.2004/15:13:25       
      ***************************************************************** 
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00639!23.11.07!2.0 ! NEUERSTELLUNG FÜR ZWKI0314!SCHLEICHER*  
      *         !        !    ! WEGEN 3 NEUER FELDER      !          * 
      *DBO-10227!11.08.22!Längenänderung                  !u131774   *
      *         !        !GHBGUTHABENANT von 10 auf 11 B  !          * 
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER            
      *    FÜR DIE FUNKTION DRKI2218                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  UPDATE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2218.                                                 
           05  PI-DRKI2218-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI2218-GHBBUCHUNGLFDNR      PIC S9(4) COMP.        
             10 PI-DRKI2218-GHBFONDID            PIC S9(4) COMP.        
             10 PI-DRKI2218-GHBGHALFDNR          PIC S9(4) COMP.        
             10 PI-DRKI2218-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PI-DRKI2218-GHBVERTRAGSID        PIC X(13).             
                                                                        
                                                                        
           05  PI-DRKI2218-DATEN.                                       
                                                                        
                                                                        
             10 PI-DRKI2218-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2218-GHBBETRLBUWKZ        PIC X(1).              
             10 PI-DRKI2218-GHBBUCHUNGSDATUM     PIC X(10).             
             10 PI-DRKI2218-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3.
             10 PI-DRKI2218-GHBKZFINHLETZTBUID   PIC S9(4) COMP.        
             10 PI-DRKI2218-GHBNETTOBETRAG       PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2218-GHBNETTOBETWKZ       PIC X(1).              
             10 PI-DRKI2218-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2218-GHBSTLETZTBUWKZ      PIC X(1).              
             10 PI-DRKI2218-GHBGEBBETRAG         PIC S9(9)V9(2) COMP-3. 
             10 PI-DRKI2218-GHBGEBSCHLUESSEL     PIC S9(4) COMP.        
             10 PI-DRKI2218-GHBGEBTEXT           PIC  X(20).            
                                                                        
                                                                        
                                                                        
      *                                                                 
       01  PO-DRKI2218.                                                 
           05  PO-DRKI2218-RC                    PIC  X(003).           
               88 PO-DRKI2218-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI2218-RC-NOT-FOUND                 VALUE 'NFD'. 
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
      *---------------------------------------------------------------* 
