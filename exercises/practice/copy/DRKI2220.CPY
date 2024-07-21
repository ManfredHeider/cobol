      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  23.11.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2220.CPY  $                                 00010000

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2220.CPv  $                                
      *
      *   Rev 2.1   Dec 19 2007 13:56:32   t13ex50
      *AKI00639: PremiumDepot: Datenspiegel und Programmanpassungen
      *                                                                 
      *   Rev 2.0   Nov 23 2007 09:45:20   t13ex50                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2220                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2220                    * 
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
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER            
      *    FÜR DIE FUNKTION DRKI2220                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  MIN / MAX <<<                                   
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2220.                                                 
           05  PI-DRKI2220-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI2220-GHBFONDID            PIC S9(4) COMP.        
             10 PI-DRKI2220-GHBGHALFDNR          PIC S9(4) COMP.        
             10 PI-DRKI2220-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.        
             10 PI-DRKI2220-GHBVERTRAGSID        PIC X(13).             
                                                                        
      *                                                                 
       01  PO-DRKI2220.                                                 
           05  PO-DRKI2220-RC                    PIC  X(003).           
               88 PO-DRKI2220-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI2220-RC-NOT-FOUND                 VALUE 'NFD'. 
      *                                                                 
           05  PO-DRKI2220-DATEN.                                       
                                                                        
                                                                        
             10 PO-DRKI2220-GHBBUCHUNGLFDNR      PIC S9(4) COMP.        
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
      *---------------------------------------------------------------* 
