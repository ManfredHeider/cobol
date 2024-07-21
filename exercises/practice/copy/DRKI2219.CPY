      *****************************************************************
      *    COPY-MEMBER: DRKI2219                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2219                    * 
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
      *    FÜR DIE FUNKTION DRKI2219                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2219.                                                 
           05  PI-DRKI2219-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI2219-GHBBUCHUNGLFDNR      PIC S9(4) COMP.        
             10 PI-DRKI2219-GHBFONDID            PIC S9(4) COMP.        
             10 PI-DRKI2219-GHBGHALFDNR          PIC S9(4) COMP.        
             10 PI-DRKI2219-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PI-DRKI2219-GHBVERTRAGSID        PIC X(13).             
                                                                        
      *                                                                 
       01  PO-DRKI2219.                                                 
           05  PO-DRKI2219-RC                    PIC  X(003).           
               88 PO-DRKI2219-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI2219-RC-NOT-FOUND                 VALUE 'NFD'. 
      *                                                                 
           05  PO-DRKI2219-DATEN.                                       
                                                                        
                                                                        
             10 PO-DRKI2219-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3. 
             10 PO-DRKI2219-GHBBETRLBUWKZ        PIC X(1).              
             10 PO-DRKI2219-GHBBUCHUNGSDATUM     PIC X(10).             
             10 PO-DRKI2219-GHBGUTHABENANT       PIC S9(7)V9(4) COMP-3. 
             10 PO-DRKI2219-GHBKZFINHLETZTBUID   PIC S9(4) COMP.        
             10 PO-DRKI2219-GHBNETTOBETRAG       PIC S9(9)V9(2) COMP-3. 
             10 PO-DRKI2219-GHBNETTOBETWKZ       PIC X(1).              
             10 PO-DRKI2219-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3. 
             10 PO-DRKI2219-GHBSTLETZTBUWKZ      PIC X(1).              
             10 PO-DRKI2219-GHBGEBBETRAG         PIC S9(9)V9(2) COMP-3. 
             10 PO-DRKI2219-GHBGEBSCHLUESSEL     PIC S9(4) COMP.        
             10 PO-DRKI2219-GHBGEBTEXT           PIC  X(20).            
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
      *---------------------------------------------------------------* 
