      * T01JM5X   Stand: 02/10/98    10:40:36
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0477.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRKI0477.CPv  $
      *
      *   Rev 2.14   Aug 02 2010 12:32:34   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.13   Aug 02 2010 12:17:40   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.12   Aug 02 2010 12:09:06   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.11   Aug 02 2010 11:30:20   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.10   Aug 02 2010 10:58:52   T13ae49
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.9   Jul 14 2010 18:02:40   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.8   Jul 14 2010 17:22:36   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.7   Jul 13 2010 19:38:12   t13ex52
      *AKI00967: Felderweiterg. elektr. Vtrg.Datenänderung Ermittlu
      *ng, Aufbereitung, Aktualisierung
      *
      *   Rev 2.6   Sep 17 2008 10:05:28   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.5   Sep 17 2008 09:48:04   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.4   Sep 11 2008 15:48:26   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Umsatzdaten
      *
      *   Rev 2.3   Dec 19 2002 11:39:58   t13ex49
      *AKI00206 - neues Fehlerverfahren 
      *(und DM in WERT umbenannt)
      *
      *   Rev 2.2   Jun 30 1999 09:37:02   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.1   Apr 08 1999 15:42:22   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:06:16   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   28 Jul 1998 09:57:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 4 7 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BADENIA-DTA Umsatzdaten                      *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   169 Bytes                                       *
180808*    SATZLÄNGE   400 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *03.08.98!NEUERSTELLUNG                          !Dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *AKI00030!Einfügen Währungskennzeichen           !Dobslaff ! 02  *
      *        !und Änderung D-STAND-DARLEHEN von      !         !     *
      *        !S9(9) zu S9(7)V99.                     !         !     *
      *        !                                       !         !     *
111202*AKI00206!Umbenennen D-Ums-DM in D-Ums-Wert      !P. Pohl  !     *
      *        !                                       !         !     *
180808*AKI00737! neues Feld D-LFDNR für  ZWK/BVD       !W.Misgeld!     *
      *        !                                       !         !     *
      *---------+--------+----+---------------------------+------------*
240610* AKI00967!24.06.10!    !Integration 1 zusaetzliches! HSL        *
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
       01  DRKI0477.
      *
           05  VORSATZ.
               10                       PIC  X(002).
               10 D-DAT                 PIC  9(008).
               10 D-TEXT                PIC  X(010).
               10 D-BESTAND             PIC  X(017).
      *        10                       PIC  X(132).
180808         10                       PIC  X(363).
      *
           05  UMSATZ-DATEN REDEFINES VORSATZ.
      *
               10 D-SA                  PIC  X(002).
180808*******  10 D-VNR                 PIC  9(015).
180808         10 D-VNR-LFDNR.
180808            15 D-VNR              PIC  9(015).
180808            15 D-LFDNR            PIC  X(001).
180808*           1 = Bausparvertrag
180808*           2 = ZWK (Zwischenkredit)/BVD (Bauvorausdarlehen)
               10 D-PRODSCHL            PIC  9(002).
               10 D-FREIBETRAG          PIC  S9(005)V99.
               10 D-BAUSPAR-GUTHABEN    PIC  S9(007)V99.
               10 D-STAND-DARLEHEN      PIC  S9(007)V99.
               10 D-WOBAU-KUMUL         PIC  S9(009)V99.
               10 D-ARBSPARZU-KUMUL     PIC  S9(009)V99.
               10 UMSATZ-TAB.
                  15 UMSAETZE OCCURS 5.
111202*              20 D-UMS-DM        PIC  S9(007)V99.
111202               20 D-UMS-WERT      PIC  S9(007)V99.
                     20 D-UMS-TEXTSCHL  PIC  9(003).
                     20 D-UMS-DAT       PIC  9(008).
240610*** VL-Umsatz Mit 'J' Ja, 'N' Nein oder Blank kennzeichnen.
240610               20 D-UMS-VL-UMSATZ PIC  X.
               10 D-UMSATZ-WKZ          PIC  X(003).
180808*        10                       PIC  X(230).
240610         10 FILLER                PIC  X(225).
      *
           05  NACHSATZ REDEFINES VORSATZ.
               10                       PIC  X(002).
               10 D-ANZAHL              PIC  9(009).
180808*        10                       PIC  X(158).
180808         10                       PIC  X(389).
      *
      ******************************************************************
