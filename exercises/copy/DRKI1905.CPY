      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  23.05.2006                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1905.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1905.CPv  $                            
      *
      *   Rev 2.2   Nov 12 2013 12:01:04   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *                                                                 
      *   Rev 2.1   Jul 28 2006 07:29:48   t13ex14                      
      *AKI00553: OZPs                                                   
      *                                                                 
      *   Rev 2.0   May 23 2006 10:55:18   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI1905                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1905                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 16:19:12 / T13EX52    
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
      *    FšR DIE FUNKTION DRKI1905                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI1905.                                                 
           05  PI-DRKI1905-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI1905-KFZBVBLFDNR   PIC S9(4) COMP.               
             10 PI-DRKI1905-KFZBVBVERTRAGSID   PIC  X(0013).            
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI1905.                                                 
           05  PO-DRKI1905-RC                     PIC  X(003).          
               88 PO-DRKI1905-RC-OK               VALUE 'OK '.          
               88 PO-DRKI1905-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
           05  PO-DRKI1905-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PO_DATEN%                                             
             10 PO-DRKI1905-KFZBVBBESVEREINB   PIC  X(0050).            
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
