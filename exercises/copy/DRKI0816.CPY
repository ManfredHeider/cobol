      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0816.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI0816.CPv  $
      *
      *   Rev 2.12   Jan 24 2018 17:18:30   t13ex27
      *AKI01540: Bereinigung Compile-Fehler
      *
      *   Rev 2.11   Aug 02 2010 12:33:12   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.10   Aug 02 2010 12:18:18   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.9   Aug 02 2010 12:09:42   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.8   Aug 02 2010 11:30:58   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.7   Aug 02 2010 10:59:32   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.6   Jul 14 2010 18:03:16   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.5   Jul 14 2010 17:23:14   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.4   Jul 13 2010 19:38:56   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.3   Sep 11 2008 15:48:44   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.2   Jun 30 1999 09:55:00   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.1   Apr 08 1999 16:41:18   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Feb 27 1999 12:58:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:48:36   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 8 1 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM DER GES BADENIA,       *
      *    Daten, die in die UMSATZ-tabelle gestellt werden sollen     *
      *    zus. mit Vertrags-Id                                        *
      *                                                                *
      *    SATZLÄNGE   170                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      * 4.03.98!NEUERSTELLUNG                          !WHO      ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *AKI00030!Einfügen Währungskennzeichen           !Dobslaff ! 02  *
      *        !                                       !         !     *
090908*AKI00737! neues Feld UMS-LFDNR für ZWK/BVD      !W.Misgeld!
      *        !                                       !         !     *
      *--------+---------------------------------------+---------+-----*
290610*AKI00967! Integration 1 zusaetzliches Feld      !H.Schlen-!     *
      *   UE002! (VL-UMSATZ) in die Tabelle            !     ther!     *
      *        ! UMS-TABELLE1                          !         !     *
      *--------+---------------------------------------!---------+-----*
      *AKI01540! Korrektur für EnterpriseDeveloper     !IFR      !     *
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

100118*01  DRKI0719.
100118 01  DRKI0816.
      *
           05  UMS-VERTID                       PIC  X(013).

           05  UMS-GESAMT.
               15  UMS-GES1                      PIC  9(002).
               15  UMS-VNR1                      PIC  9(015).
090908         15  UMS-LFDNR                     PIC  9(001).
      *
               15  UMS-ZAHLW1                    PIC  S9(002).
               15  UMS-AEND-ZAHLW-KZ1            PIC  X(001).
      *            J WENN Zahlungsweise geändert
      *
               15  UMS-ARBSPARZU-KUMUL1          PIC S9(009)V99.
               15  UMS-AEND-ARBSPARZU-KUMUL-KZ1  PIC X(001).
      *
               15  UMS-ENDE-ST-LAUFZEIT1         PIC X(010).
               15  UMS-AEND-ENDE-ST-LAUFZEIT-KZ1 PIC X(001).
      *
               15  UMS-TABELLE1 OCCURS 5.
                   20  UMS-BETRAG1               PIC S9(007)V99.
                   20  UMS-SCHLUESSEL1           PIC S9(003).
                   20  UMS-DATUM1                PIC  X(010).
290610*** VL-Umsatz Mit 'J' Ja, 'N' Nein oder Blank kennzeichnen.
290610             20  UMS-VL-UMSATZ1            PIC  X.
      *
               15  UMS-AEND-TAB-KZ1              PIC  X(001).
      *
      *
               15  UMS-WKZ1                      PIC  X(001).
               15  UMS-AEND-WKZ-KZ1              PIC  X(001).
      *
               15  UMS-AKTUAL-KZ1                PIC  X(001).
      *

