      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  28.09.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2153.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2153.CPv  $                            
      *
      *   Rev 2.2   Sep 15 2011 15:16:26   t13ex14
      *AKI01037: SUH - FK: Aktualisierung SGWBESVEREINB
      *                                                                 
      *   Rev 2.1   17 Oct 2007 13:12:58   t13ex27                      
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs              
      *                                                                 
      *   Rev 2.0   28 Sep 2007 17:02:40   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2153                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2153                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         17.08.2011 / 11:13:45 / T13EX27    
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
      *    FšR DIE FUNKTION DRKI2153                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2153.                                                 
           05  PI-DRKI2153-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2153-SGWPSGBLFDNR   PIC S9(4) COMP.              
             10 PI-DRKI2153-SGWRGID   PIC S9(4) COMP.                   
             10 PI-DRKI2153-SGWVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2153-SGWVETLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2153.                                                 
           05  PO-DRKI2153-RC                     PIC  X(003).          
               88 PO-DRKI2153-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2153-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
           05  PO-DRKI2153-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PO_DATEN%                                             
             10 PO-DRKI2153-LETZTEAENDERUNG   PIC  X(026).              
             10 PO-DRKI2153-SGWBEDKLAUSELNR   PIC  X(0020).             
             10 PO-DRKI2153-SGWEINZVTGHOSTID   PIC  X(0015).            
             10 PO-DRKI2153-SGWVERTBTEILTXT   PIC  X(0080).             
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
