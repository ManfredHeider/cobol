      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  28.09.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2151.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2151.CPv  $                            
      *
      *   Rev 2.2   Sep 15 2011 15:16:10   t13ex14
      *AKI01037: SUH - FK: Aktualisierung SGWBESVEREINB
      *                                                                 
      *   Rev 2.1   17 Oct 2007 13:12:34   t13ex27                      
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs              
      *                                                                 
      *   Rev 2.0   28 Sep 2007 16:56:42   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2151                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2151                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         17.08.2011 / 11:13:36 / T13EX27    
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
      *    FšR DIE FUNKTION DRKI2151                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  UPDATE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2151.                                                 
           05  PI-DRKI2151-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2151-SGWPSGBLFDNR   PIC S9(4) COMP.              
             10 PI-DRKI2151-SGWRGID   PIC S9(4) COMP.                   
             10 PI-DRKI2151-SGWVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2151-SGWVETLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRKI2151-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRKI2151-SGWBEDKLAUSELNR   PIC  X(0020).             
             10 PI-DRKI2151-SGWEINZVTGHOSTID   PIC  X(0015).            
             10 PI-DRKI2151-SGWVERTBTEILTXT   PIC  X(0080).             
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
       01  PO-DRKI2151.                                                 
           05  PO-DRKI2151-RC                     PIC  X(003).          
               88 PO-DRKI2151-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2151-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
