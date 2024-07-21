      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  23.11.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2216.CPY  $                                 00010000

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2216.CPv  $                                
      *
      *   Rev 2.1   Dec 19 2007 13:56:12   t13ex50
      *AKI00639: PremiumDepot: Datenspiegel und Programmanpassungen
      *                                                                 
      *   Rev 2.0   Nov 23 2007 09:44:50   t13ex50                      
      *Archiverstellung                                                 
      *                                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2216                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2216                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   erzeugt mit genOZP - (c) PMSmicado  28.07.98/14.48.05         
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
      *    FÜR DIE FUNKTION DRKI2216                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  DELETE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2216.                                                 
           05  PI-DRKI2216-KEY.                                         
                                                                        
                                                                        
             10 PI-DRKI2216-GHBBUCHUNGLFDNR      PIC S9(04) COMP.       
             10 PI-DRKI2216-GHBFONDID            PIC S9(04) COMP.       
             10 PI-DRKI2216-GHBGHALFDNR          PIC S9(04) COMP.       
             10 PI-DRKI2216-GHBGUTHABPRODLFDNR   PIC S9(04) COMP.       
             10 PI-DRKI2216-GHBVERTRAGSID        PIC  X(13).            
                                                                        
      *                                                                 
       01  PO-DRKI2216.                                                 
           05  PO-DRKI2216-RC                    PIC  X(003).           
               88 PO-DRKI2216-RC-OK                        VALUE 'OK '. 
               88 PO-DRKI2216-RC-NOT-FOUND                 VALUE 'NFD'. 
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
      *---------------------------------------------------------------* 
