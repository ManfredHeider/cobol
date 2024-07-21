      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0089.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0089.CPv  $
      *
      *   Rev 2.1   Mar 26 2009 10:42:52   t13ex21
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   Rev 2.0   Feb 27 1999 11:57:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:51:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: ALLGEMEINE ZWISCHENFELDER (DRAT0089)          * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: GIESE                                            * 
      *    DATUM:    05.01.1995                                       * 
      ***************************************************************** 
      *    VERSION   1    10.01.95 16:27:08
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AFVB0163!10.01.95! 01 ! NEU                       ! GIESE    *  
      * APA00820!15.10.08 2.01! FC-CODE HINZUGEFÜGT       ! THIEL    *
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      * Es ist zu prüfen, ob diese allgemeinen Zwischenfelder nicht     
      * ausschließlich für den Dialogtyp: Änderungsliste gebraucht      
      * werden. d.h. für andere Dialogtypen werden andere copies        
      * gebaut.                                                         
                                                                        
       01  Z-ALLG-BEDINGUNGSNAME-BED.                                   
         05 Z-FC-EVAL                    PIC  X(01).                    
           88  Z-FC-INIT            VALUE SPACE.                        
           88  Z-FC-AUSWAHL         VALUE 'X', 'x'.                     
           88  Z-FC-UPD             VALUE 'U', 'u'.                     
           88  Z-FC-INS             VALUE 'I', 'i'.                     
           88  Z-FC-DEL             VALUE 'D', 'd'.                     
           88  Z-FC-AEND            VALUE 'A', 'a'.                     
           88  Z-FC-LOE             VALUE 'L', 'l'.                     
           88  Z-FC-EINF            VALUE 'E', 'e'.
151008     88  Z-FC-RUECK            VALUE 'R', 'r'.
                                                                        
           88  Z-FC-ANZ-HIST        VALUE 'H', 'h'.                     
         05  Z-OK-EVAL                   PIC  X(01).
           88 Z-OK-JA               VALUE 'J', 'j'.
           88 Z-OK-NEIN             VALUE 'N', 'n'.

      *    * Schalter, werden mit SET TO TRUE gesetzt (in V5-)          
       01  Z-ALLG-SCHALTER.                                             
         05  S-FC-BELEGUNG               PIC  9(01).
           88  S-FC-INIT            VALUE 0.
           88  S-FC-GUELTIG         VALUE 1.
           88  S-FC-UNGUELTIG       VALUE 2.
           88  S-FC-EINTRAG         VALUE 3.
         05  S-AEND                      PIC  9(01).                    
           88  S-AEND-N             VALUE 1.                            
           88  S-AEND-J             VALUE 2.                            
         05  S-LOE                       PIC  9(01).                    
           88  S-LOE-N              VALUE 1.                            
           88  S-LOE-J              VALUE 2.                            
                                                                        
         05  S-EINF                      PIC  9(01).                    
           88  S-EINF-N             VALUE 1.                            
           88  S-EINF-J             VALUE 2.                            
         05  S-AHI                       PIC  9(01).                    
           88  S-AHI-N              VALUE 1.                            
           88  S-AHI-J              VALUE 2.
151008   05  S-RUECK                       PIC  9(01).
151008     88  S-RUECK-N              VALUE 1.
151008     88  S-RUECK-J              VALUE 2.
                                                                        
      *                                                                 
      ******** ENDE COPY-MEMBER ALLGEMEINE ZWISCHENFELDER (DRAT0089)*** 
