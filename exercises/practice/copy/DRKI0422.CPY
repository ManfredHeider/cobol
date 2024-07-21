      *---------------------------------------------------------------*         
      *    Standard f•r alle Objekte unter PVCS                                 
      *            Datum   :  November 1995                                     
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI0422.CPY  $                                         
      *    $Revision:   2.7  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI0422.CPv  $                                    
      *                                                                         
      *   Rev 2.7   Dec 22 2015 08:37:50   t13ex15                              
      *AKI01406: VBI SST VEP auf SUMKD Source                                   
      *                                                                         
      *   Rev 2.6   Jul 01 2014 13:00:22   t13ex27                              
      *AKI01303: "VEP"-Wertung für AS-Club und KLZ-OZP Rechenwerte              
      *                                                                         
      *   Rev 2.5   19 Oct 2006 11:51:14   t13ex39                              
      *AKI00615: Einführung Pflegerentenversicherung                            
      *                                                                         
      *   Rev 2.4   27 Sep 2006 10:39:48   t13ex39                              
      *AKI00619: neuer Unfall Plus Tarif "50+"                                  
      *                                                                         
      *   Rev 2.3   26 Sep 2006 12:48:52   t13ex39                              
      *AKI00619: neuer Unfall Plus Tarif "50+"                                  
      *                                                                         
      *   Rev 1.0   28 Jul 1998 09:52:40   P13SP99                              
      *Initial revision.                                                        
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI0422                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0422                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         14.06.2021 / 16:14:45 / T13EX78            
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
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI0422                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0422.                                                         
           05  PI-DRKI0422-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0422-LRWVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0422-LRWVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0422.                                                         
           05  PO-DRKI0422-RC                     PIC  X(003).                  
               88 PO-DRKI0422-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0422-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI0422-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI0422-LRWRKWERTGES   PIC S9(9)V9(2) COMP-3.               
             10 PO-DRKI0422-LRWSUMDK   PIC S9(11)V9(2) COMP-3.                  
             10 PO-DRKI0422-LRWSUMDKBKZ   PIC  X(0001).                         
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
