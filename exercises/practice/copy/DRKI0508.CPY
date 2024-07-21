      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0508.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI0508.CPv  $
      *
      *   Rev 2.13   Aug 02 2010 12:32:54   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.12   Aug 02 2010 12:17:58   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.11   Aug 02 2010 12:09:24   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.10   Aug 02 2010 11:30:38   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.9   Aug 02 2010 10:59:12   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.8   Jul 14 2010 18:03:50   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.7   Jul 14 2010 17:23:48   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.6   Jul 13 2010 19:39:32   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.5   Sep 17 2008 09:48:10   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.4   Sep 11 2008 15:48:36   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.3   Dec 19 2002 11:40:28   t13ex49
      *AKI00206 - neues Fehlerverfahren 
      *(und DM in WERT umbenannt)
      *
      *   Rev 2.2   Jun 30 1999 09:47:14   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.1   Apr 08 1999 15:42:42   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:06:34   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   07 Aug 1998 14:43:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 5 0 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BADENIA Umsatzdaten Änderungsbestand         *
      *                                                                *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   176 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *21.08.98!NEUERSTELLUNG                          !Dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *AKI00030!Einfügen Währungskennzeichen und       !Dobslaff ! 02  *
      *        !AE-STAND-DARLEHEN mit 2Nachkommastellen!         !     *
      *        !                                       !         !     *
111202*AKI00206!Umbenennen D-Ums-DM in D-Ums-Wert      !P. Pohl  !     *
      *        !                                       !         !     *
120808*AKI00737! neues Feld AE-LFDNR für  ZWK/BVD      !W.Misgeld!
      *        !                                       !         !
      *---------+--------+----+---------------------------+------------*
250610* AKI00967!25.06.10!    !Integration 1 zusaetzliches! HSL        *
      *    UE002!        !    !Feld (VL-UMSATZ) in die Ta-!            *
      *         !        !    !belle UMSATZ-TAB           !            *
      *---------!--------!----!---------------------------!------------*
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      ******************************************************************
       01  DRKI0508.
      *
           05 AE-AEND-KZ               PIC  X(001).
120808*    05 AE-VNR                   PIC  9(015).
120808     05 AE-VNR-LFDNR.
120808        10 AE-VNR                PIC  9(015).
120808        10 AE-LFDNR              PIC  9(001).
120808*       1 = Bausparvertrag
120808*       2 = ZWK (Zwischenkredit)/BVD (Bauvorausdarlehen)
           05 AE-PRODSCHL              PIC  9(002).
           05 AE-PRODSCHL-AEKZ         PIC  X(001).
           05 AE-FREIBETRAG            PIC  S9(005)V99.
           05 AE-FREIBETRAG-AEKZ       PIC  X(001).
           05 AE-BAUSPAR-GUTHABEN      PIC  S9(007)V99.
           05 AE-BAUSPAR-GUTHABEN-AEKZ PIC  X(001).
           05 AE-STAND-DARLEHEN        PIC  S9(007)V99.
           05 AE-STAND-DARLEHEN-AEKZ   PIC  X(001).
           05 AE-WOBAU-KUMUL           PIC  S9(009)V99.
           05 AE-WOBAU-KUMUL-AEKZ      PIC  X(001).
           05 AE-ARBSPARZU-KUMUL       PIC  S9(009)V99.
           05 AE-ARBSPARZU-KUMUL-AEKZ  PIC  X(001).
           05 UMSATZ-TAB.
              10 UMSAETZE OCCURS 5.
111202*          15 AE-UMS-DM          PIC  S9(007)V99.
111202           15 AE-UMS-WERT        PIC  S9(007)V99.
                 15 AE-UMS-TEXTSCHL    PIC  9(003).
                 15 AE-UMS-DAT         PIC  9(008).
250610*** VL-Umsatz Mit 'J' Ja, 'N' Nein oder Blank kennzeichnen.
250610           15 AE-UMS-VL-UMSATZ   PIC  X.
      ***
           05 AE-UMSATZ-TAB-AEKZ       PIC  X(001).
           05 AE-UMSATZ-WKZ            PIC  X(003).
           05 AE-UMSWKZ-AEKZ           PIC  X(001).
120808**   05 FILLER                   PIC  X(023).
250610     05 FILLER                   PIC  X(018).
      *
      *
      *
      *************************************************************
