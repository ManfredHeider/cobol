      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  23.05.2006                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1907.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1907.CPv  $                            
      *
      *   Rev 2.2   Nov 12 2013 12:01:22   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *                                                                 
      *   Rev 2.1   Jul 28 2006 07:30:04   t13ex14                      
      *AKI00553: OZPs                                                   
      *                                                                 
      *   Rev 2.0   May 23 2006 10:55:42   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI1907                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1907                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 16:19:19 / T13EX52    
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER            
      *    FšR DIE FUNKTION DRKI1907                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  UPDATE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI1907.                                                 
           05  PI-DRKI1907-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI1907-KFZBVBLFDNR   PIC S9(4) COMP.               
             10 PI-DRKI1907-KFZBVBVERTRAGSID   PIC  X(0013).            
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRKI1907-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRKI1907-KFZBVBBESVEREINB   PIC  X(0050).            
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
       01  PO-DRKI1907.                                                 
           05  PO-DRKI1907-RC                     PIC  X(003).          
               88 PO-DRKI1907-RC-OK               VALUE 'OK '.          
               88 PO-DRKI1907-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
