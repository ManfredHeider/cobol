      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  28.09.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2152.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2152.CPv  $                            
      *
      *   Rev 2.2   Sep 15 2011 15:16:18   t13ex14
      *AKI01037: SUH - FK: Aktualisierung SGWBESVEREINB
      *                                                                 
      *   Rev 2.1   17 Oct 2007 13:12:46   t13ex27                      
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs              
      *                                                                 
      *   Rev 2.0   28 Sep 2007 16:59:42   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2152                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2152                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         17.08.2011 / 11:13:40 / T13EX27    
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
      *    FšR DIE FUNKTION DRKI2152                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  DELETE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2152.                                                 
           05  PI-DRKI2152-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2152-SGWPSGBLFDNR   PIC S9(4) COMP.              
             10 PI-DRKI2152-SGWRGID   PIC S9(4) COMP.                   
             10 PI-DRKI2152-SGWVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2152-SGWVETLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2152.                                                 
           05  PO-DRKI2152-RC                     PIC  X(003).          
               88 PO-DRKI2152-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2152-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
