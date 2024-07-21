      *---------------------------------------------------------------* 
      *    Standard f?r alle Objekte unter PVCS                         
      *            Datum   :  November 1995                             
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT                    
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright 1995 ATLAS Dienstleistungs GmbH                    
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI0697.CPY  $                                         

      *
      *    $Revision:   2.17  $
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI0697.CPv  $                            
      *
      *   Rev 2.17   Dec 20 2012 15:48:16   T13ex52
      *AKI01188: VB-Tag 2013: Einführung Badeniatarife "Via Badenia
      * 500"
      *
      *   Rev 2.16   Aug 02 2010 12:32:58   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.15   Aug 02 2010 12:18:04   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.14   Aug 02 2010 12:09:28   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.13   Aug 02 2010 11:30:44   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.12   Aug 02 2010 10:59:18   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.11   Jul 14 2010 18:02:30   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.10   Jul 14 2010 17:22:24   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.9   Jul 13 2010 19:38:02   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.8   Oct 26 2009 07:36:46   t13ex14
      *AKI00846: Erw. der Bestands-DTAs Badenia: Programme u. Auftr
      *äge
      *
      *   Rev 2.7   Dec 10 2008 10:02:10   t13ex14
      *AKI00737: Bündel-KZ f. Zwischenfinanzierungen im Vertrag
      *                                                                 
      *   Rev 2.5   Apr 05 2005 17:44:06   t13ex27                      
      *AKI00475: Anpassungen f?r Badenia                                
      *                                                                 
      *   Rev 2.4   Jan 24 2003 07:51:12   t13ae31                      
      *FKI00695 Beitrag Minus zulassen                                  
      *                                                                 
      *   Rev 2.3   Jun 30 1999 09:47:50   T13EX51                      
      *AKI00030 Einf?gen WKZ                                            
      *                                                                 
      *   Rev 2.2   Apr 08 1999 15:48:08   T13EX51                      
      *AKI00021                                                         
      *                                                                 
      *   Rev 2.1   Mar 22 1999 19:14:14   T13EX51                      
      *AKI00021, Vorgzeichen bei ZAHLW                                  
      *                                                                 
      *   Rev 2.0   Mar 01 1999 15:07:34   T13EX51                      
      *Neue Revision AKI000001                                          
      *                                                                 
      *   Rev 1.0   25 Aug 1998 09:31:36   P13SP99                      
      *Initial revision.                                                
      *                                                                 
      *---------------------------------------------------------------* 
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 6 9 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER f?r Badenia-Daten, die in die Vertragstabelle     *
      *              eingestellt werden (aus dem DTA Vertragsdaten)    *
      *                                                                *
      *    SATZLŽNGE   94                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ŽNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *30.09.98!NEUERSTELLUNG                          !Dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *11.01.99!Vertragszustand-Datum eingef?gt        !Dobslaff ! 02  *
      *22.03.99!Zahlweise mit Vorzeichen definiert     !Dobslaff ! 03  *
      *        !                                       !         !     *
      *AKI00030!Einf?gen W„hrungskennzeichen           !Dobslaff ! 04  *
      *        !                                       !         !     *
      *FKI00695! Vorzeichen Beitrag                    !P. H?bner      *
      *AKI00475! neues Feld EHELEUTEKZ                 !I.Frenzel!     *
      *        !                                       !         !     *
041208*AKI00737! neues Feld VBS-BUENDELKZ              !W.Misgeld!     *
      *        !                                       !         !     *
110809*AKI00846! DTA-Erweiterung INKASSOART            !W.Misgeld!     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
170610*17.06.10! DTA-Erweiterung um 7 Datenfelder      !H.Schlenther!  *
      *AKI00967! gesamt 36 Byte. Aktualisierungs-KNZ   !         !     *
      *   UE002! auf die Stelle 1 setzen.              !         !     *
      *        ! Datensatz verlängert, der Fillerber-  !         !     *
      *        ! in der Laenge 22 Byte nicht mehr aus- !         !     *
      *        ! reichend war.                         !         !     *
      ******************************************************************
271112*27.11.12! Die unter AKI00967.UE002 gemachten    !H.Schlenther!  *
      *AKI01188! Aenderung wieder rueckgaengig machen, !         !     *
      *   UE001! da hier nicht noetig. Die hinzugefueg-!         !     *
      *        ! ten Felder werden nicht im VERTRAG be-!         !     *
      *        ! noetigt. (Rueckgaengig machen gilt    !         !     *
      *        ! nur fuer die hinzugefuegten Attribute)!         !     *
      *        ! Gilt nicht fuer die Positionsaenderung!         !     *
      *        ! des Attributs "VBS-AKTUAL-KZ".        !         !     *
      *--------!---------------------------------------!---------!-----*
      *
      ******************************************************************
      *
       01  DRKI0697.                                                    
      *
170610     05  VBS-AKTUAL-KZ               PIC  X(001).
    >>*        I = INSERT
      *        U = UPDATE
    <<*        D = DELETE
170610*
           05  VBS-GES                     PIC  9(002).                 
           05  VBS-VNR                     PIC  9(015).                 
      *                                                                 
           05  VBS-ZAHLW                   PIC  S9(002).                
           05  VBS-AEND-ZAHLW-KZ           PIC  X(001).                 
      *                                                                 
           05  VBS-VERTRAGSZUSTAND         PIC  9(002).                 
           05  VBS-AEND-VERTRAGSZUSTAND-KZ PIC  X(001).                 
      *        J WENN VERTRAGSZUSTAND GEAENDERT                         
      *                                                                 
           05  VBS-VERTRAGSZUSTAND-DAT     PIC  X(010).                 
      *                                                                 
           05  VBS-BEITRAG-GESAMT          PIC  S9(011)V99.
           05  VBS-AEND-BEITRAG-GESAMT-KZ  PIC  X(001).                 
      *        J WENN BEITRAG GESAMT GEAENDERT                          
      *                                                                 
           05  VBS-INT-VNR                 PIC  9(007).                 
      *                                                                 
           05  VBS-INT-VNR-PRUEF           PIC  9(002).                 
      *                                                                 
           05  VBS-VERTRAGSABLAUF          PIC  X(010).                 
           05  VBS-AEND-VERTRAGSABLAUF-KZ  PIC  X(001).                 
      *        J WENN VERTRAGSABLAUF GEAENDERT                          
      *                                                                 
           05  VBS-VERTRAGSBEGINN          PIC  X(010).                 
           05  VBS-AEND-VERTRAGSBEGINN-KZ  PIC  X(001).                 
      *        J WENN VERTRAGSBEGINN GEAENDERT                          
      *                                                                 
           05  VBS-VBNR                    PIC  9(007).                 
           05  VBS-AEND-VBNR-KZ            PIC  X(001).                 
      *                                                                 
           05  VBS-PRODUKT-ID              PIC  9(002).                 
           05  VBS-VL-KZ                   PIC  X(001).                 
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG                
                                                                        
           05  VBS-WKZ                     PIC  X(001).                 
           05  VBS-AEND-WKZ-KZ             PIC  X(001).                 
                                                                        
           05  VBS-EHELEUTEKZ              PIC  X(001).
           05  VBS-AEND-EHELEUTEKZ-KZ      PIC  X(001).

           05  VBS-BUENDELKZ               PIC  X(001).
           05  VBS-AEND-BUENDELKZ-KZ       PIC  X(001).
      *
           05  VBS-INKASSOART              PIC  X(001).
           05  VBS-AEND-INKASSOART-KZ      PIC  X(001).
      *
           05  FILLER                      PIC  X(102).
      *
170610*    05  VBS-AKTUAL-KZ               PIC  X(001).
    >>*        I = INSERT
      *        U = UPDATE
    <<*        D = DELETE
170610*
      *
      ******************************************************************
