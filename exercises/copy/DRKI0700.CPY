      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0700.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI0700.CPv  $
      *
      *   Rev 2.12   Dec 23 2010 13:09:22   t13ex52
      *FKI01320: Fehlerbehebg. Selektion LänderKZ und  Ausgabesatz
      *
      *   Rev 2.11   Aug 02 2010 12:33:06   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.10   Aug 02 2010 12:18:12   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.9   Aug 02 2010 12:09:38   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.8   Aug 02 2010 11:30:52   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.7   Aug 02 2010 10:59:26   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.6   Jul 14 2010 18:02:58   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.5   Jul 14 2010 17:22:54   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.4   Jul 13 2010 19:38:34   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.3   Sep 11 2008 15:48:40   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.2   Jun 30 1999 09:52:46   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.1   Apr 08 1999 15:49:02   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:40   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   06 Oct 1998 15:30:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 7 0 0    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM DER GES BADENIA,       *
      *    Daten, die in die UMSATZ-tabelle gestellt werden sollen     *
      *                                                                *
      *    SATZLÄNGE   157                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *13.10.98!NEUERSTELLUNG                          !dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *AKI00030!Einfügen Währungskennzeichen           !Dobslaff ! 02  *
      *        !                                       !         !     *
120808*AKI00737! neues Feld UMS-LFDNR für ZWK/BVD      !W.Misgeld!
      *        !                                       !         !
      *        !                                       !         !     *
      *--------+---------------------------------------+---------+-----*
290610*AKI00967! Integration 1 zusaetzliches Feld      ! HSL     !     *
      *   UE002! (VL-UMSATZ) in die Tabelle UMS-TABELLE!         !     *
      *--------+---------------------------------------!---------+-----*
      *        !                                       !         !     *
221210*FKI01320! Objekt wurde mit uebergeben, obwohl   ! HSL     !     *
      *   UE002! keine Aenderungen an der Copystrecke  !         !     *
      *        ! vorgenommen wurden. Ohne diese Copy-  !         !     *
      *        ! strecke war keine Uebergabe der Pro-  !         !     *
      *        ! gramme in der Stueckliste moeglich.   !         !     *
      *--------+---------------------------------------!---------+-----*
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0700.
      *
           05  UMS-GES                       PIC  9(002).
120808*    05  UMS-VNR                       PIC  9(015).
120808     05  UMS-VNR-LFDNR.
120808          10 UMS-VNR                   PIC  9(015).
120808          10 UMS-LFDNR                 PIC  9(001).
120808*         1 = Bausparvertrag
120808*         2 = ZWK (Zwischenkredit)/BVD (Bauvorausdarlehen)
      *
           05  UMS-ZAHLW                     PIC  S9(002).
           05  UMS-AEND-ZAHLW-KZ             PIC  X(001).
      *        J WENN Zahlungsweise geändert
      *
           05  UMS-ARBSPARZU-KUMUL           PIC S9(009)V99.
           05  UMS-AEND-ARBSPARZU-KUMUL-KZ   PIC X(001).
      *
           05  UMS-ENDE-ST-LAUFZEIT          PIC X(010).
           05  UMS-AEND-ENDE-ST-LAUFZEIT-KZ  PIC X(001).
      *
           05  UMS-TABELLE OCCURS 5.
               10  UMS-BETRAG                PIC S9(007)V99.
               10  UMS-SCHLUESSEL            PIC S9(003).
               10  UMS-DATUM                 PIC  X(010).
290610*** VL-Umsatz Mit 'J' Ja, 'N' Nein oder Blank kennzeichnen.
290610         10  UMS-VL-UMSATZ             PIC  X.
      *
           05  UMS-AEND-TAB-KZ               PIC  X(001).
      *
           05  UMS-WKZ                       PIC  X(001).
           05  UMS-AEND-WKZ-KZ               PIC  X(001).
      *
      *
           05  UMS-AKTUAL-KZ                 PIC  X(001).
      *
120808***  05  FILLER                        PIC  X(022).
290610     05  FILLER                        PIC  X(017).
      ******************************************************************
