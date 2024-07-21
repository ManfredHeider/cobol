      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0507.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI0507.CPv  $
      *
      *   Rev 2.13   Aug 02 2010 12:32:48   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.12   Aug 02 2010 12:17:54   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.11   Aug 02 2010 12:09:20   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.10   Aug 02 2010 11:30:34   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.9   Aug 02 2010 10:59:06   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.8   Jul 14 2010 18:03:44   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.7   Jul 14 2010 17:23:44   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.6   Jul 13 2010 19:39:28   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.5   Jan 06 2009 14:45:32   t13ex14
      *AKI00737: Satzlängenanpassung f. Freistellungsaufträge
      *
      *   Rev 2.4   Sep 11 2008 15:48:32   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.3   Dec 19 2002 11:40:28   t13ex49
      *AKI00206 - neues Fehlerverfahren 
      *(und DM in WERT umbenannt)
      *
      *   Rev 2.2   Jun 30 1999 09:45:28   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.1   Apr 08 1999 15:07:34   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:06:32   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   07 Aug 1998 14:43:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 5 0 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BADENIA Umsatzdaten Komplettbestand          *
      *                                                                *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   177 Bytes                                       *
090908*    SATZLÄNGE   200 Bytes                                       *
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
      *        !K-STAND-DARLEHEN  mit Nachkommastellen !         !     *
      *        !                                       !         !     *
111202*AKI00206!Umbenennen D-Ums-DM in D-Ums-Wert      !P. Pohl  !     *
      *        !                                       !         !     *
090908*AKI00737! neues Feld K-LFDNR für  ZWK/BVD       !W.Misgeld!
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
        01  DRKI0507.
      *
           05 K-AEND-KZ             PIC  X(002).
           05 K-UMSATZ-KEY.
              10 K-VNR              PIC  9(015).
090908        10 K-LFDNR            PIC  9(001).
090908*       1 = Bausparvertrag
090908*       2 = ZWK (Zwischenkredit)/BVD (Bauvorausdarlehen)
           05 K-PRODSCHL            PIC  9(002).
           05 K-FREIBETRAG          PIC  S9(005)V99.
           05 K-BAUSPAR-GUTHABEN    PIC  S9(007)V99.
           05 K-STAND-DARLEHEN      PIC  S9(007)V99.
           05 K-WOBAU-KUMUL         PIC  S9(009)V99.
           05 K-ARBSPARZU-KUMUL     PIC  S9(009)V99.
           05 UMSATZ-TAB.
              10 UMSAETZE OCCURS 5.
111202*          15 K-UMS-DM     PIC  S9(007)V99.
111202           15 K-UMS-WERT      PIC  S9(007)V99.
                 15 K-UMS-TEXTSCHL  PIC  9(003).
                 15 K-UMS-DAT       PIC  9(008).
250610*** VL-Umsatz Mit 'J' Ja, 'N' Nein oder Blank kennzeichnen.
250610           15 K-UMS-VL-UMSATZ PIC  X.
      *
           05 K-UMS-WKZ             PIC  X(003).
           05 K-LETZTE-AEND         PIC  9(008).
090908**   05 FILLER                PIC  X(022).
250610     05 FILLER                PIC  X(017).
      *
      *
      *************************************************************
