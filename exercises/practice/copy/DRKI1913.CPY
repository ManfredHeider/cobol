      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  23.05.2006                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI1913.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI1913.CPv  $                            
      *
      *   Rev 2.2   Nov 12 2013 12:02:12   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *                                                                 
      *   Rev 2.1   Jul 28 2006 07:30:46   t13ex14                      
      *AKI00553: OZPs                                                   
      *                                                                 
      *   Rev 2.0   May 23 2006 11:47:20   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI1913                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1913                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         28.10.2013 / 16:19:22 / T13EX52    
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
      *    FšR DIE FUNKTION DRKI1913                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  DELETE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI1913.                                                 
           05  PI-DRKI1913-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI1913-KFZBVBLFDNR   PIC S9(4) COMP.               
             10 PI-DRKI1913-KFZBVBVERTRAGSID   PIC  X(0013).            
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI1913.                                                 
           05  PO-DRKI1913-RC                     PIC  X(003).          
               88 PO-DRKI1913-RC-OK               VALUE 'OK '.          
               88 PO-DRKI1913-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
