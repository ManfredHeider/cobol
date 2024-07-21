      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  28.09.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2150.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2150.CPv  $                            
      *
      *   Rev 2.2   Sep 15 2011 15:16:00   t13ex14
      *AKI01037: SUH - FK: Aktualisierung SGWBESVEREINB
      *                                                                 
      *   Rev 2.1   17 Oct 2007 13:12:22   t13ex27                      
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs              
      *                                                                 
      *   Rev 2.0   28 Sep 2007 16:53:46   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2150                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2150                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         17.08.2011 / 11:13:31 / T13EX27    
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
      *    FšR DIE FUNKTION DRKI2150                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  INSERT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2150.                                                 
           05  PI-DRKI2150-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2150-SGWPSGBLFDNR   PIC S9(4) COMP.              
             10 PI-DRKI2150-SGWRGID   PIC S9(4) COMP.                   
             10 PI-DRKI2150-SGWVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2150-SGWVETLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRKI2150-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRKI2150-SGWBEDKLAUSELNR   PIC  X(0020).             
             10 PI-DRKI2150-SGWEINZVTGHOSTID   PIC  X(0015).            
             10 PI-DRKI2150-SGWVERTBTEILTXT   PIC  X(0080).             
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2150.                                                 
           05  PO-DRKI2150-RC                     PIC  X(003).          
               88 PO-DRKI2150-RC-OK                  VALUE 'OK '.       
               88 PO-DRKI2150-RC-DUPLICATE-VALUE VALUE 'DVA'.           
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
