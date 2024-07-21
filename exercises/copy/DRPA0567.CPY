      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0567.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0567.CPv  $
      *
      *   Rev 2.1   Apr 07 2016 10:12:02   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.0   May 27 1999 12:28:26   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.2   May 07 1999 15:02:04   T13EX25
      *Neu: Aktueller GSMT-PROV-Satz
      *
      *   Rev 1.1   May 04 1999 14:52:40   T13EX25
      *Neue Attribute für. Kond. Vorbewegung
      *
      *   Rev 1.0   22 Oct 1997 11:56:00   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0567                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<RÜCKBEL. DURCHFÜHREN>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    05.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.11.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!19.05.98! 01 ! RC-MFE HINZU              ! PARENT   *
      * AFPA0012!27.01.99! 02 ! HINZU: UFKT, PVPT         !          *
      *         !        !    ! WEG: PMPR-ALT, KNR-OBJEKT-!          *
      *         !        !    !      HKF-TAB, NTZ-PTR-ID  !          *
      * APA00012!04.05.99! 03 ! NEU: GSMT-PROV-SATZ VORB. ! JÄGER    *
      * APA00012!07.05.99! 04 ! NEU: GSMT-PROV-SATZ BEL.  ! JÄGER    *
      * APA02220!10.03.16!2.1 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0567                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
          01 PI-DRPA0567.
           05 PI-DRPA0567-DATEN.

            10 PI-DRPA0567-UFKT        PIC  9(001).
               88 PI-DRPA0567-UFKT-VOLLRUECK  VALUE  1.
               88 PI-DRPA0567-UFKT-RATRUECK   VALUE  2.
               88 PI-DRPA0567-UFKT-VOLLMIND   VALUE  3.
               88 PI-DRPA0567-UFKT-RATMIND    VALUE  4.
               88 PI-DRPA0567-UFKT-GUELTIG    VALUES 1 THRU 4.

            10 PI-DRPA0567-PMBE.
               15 PMBE-ID            PIC S9(015)  PACKED-DECIMAL.
               15 PMBE-PGFAELLIG-VON PIC  X(010).
               15 PMBE-PGFAELLIG-BIS PIC  X(010).

            10 PI-DRPA0567-PMPR.
               15 PMPR-ID            PIC S9(015)       PACKED-DECIMAL.
               15 PMPR-VERSION       PIC S9(004)       BINARY.
               15 PMPR-PGV           PIC S9(004)       BINARY.
               15 PMPR-PG-GSMT-PROV  PIC S9(009)V9(02) PACKED-DECIMAL.
100316*        15 PMPR-PG-PROV-STZ   PIC S9(003)V9(03) PACKED-DECIMAL.
100316         15 PMPR-PG-PROV-STZ   PIC S9(004)V9(04) PACKED-DECIMAL.

            10 PI-DRPA0567-PMPT-ALT.
               COPY DRPA0421.

            10 PI-DRPA0567-PVPT.
               15 MAX-USED-PVPT          PIC S9(004)   BINARY.
               15 FILLER       OCCURS 40.
                  20 PVPT-ID             PIC S9(015) PACKED-DECIMAL.
                  20 PVPT-VT-PRIO        PIC S9(004) BINARY.

            10 PI-DRPA0567-PMPR-KOND.
               15 KOND-ELEMENT OCCURS 10.
                  20 PMPR-ID             PIC S9(015)  PACKED-DECIMAL.
100316*           20 GSMT-PROV-STZ       PIC S999V999 PACKED-DECIMAL.
100316            20 GSMT-PROV-STZ       PIC S9(4)V9(4) PACKED-DECIMAL.
      *
          01 PO-DRPA0567.
           05 PO-DRPA0567-RC             PIC  X(003).
              88 PO-DRPA0567-RC-OK              VALUE 'OK '.
              88 PO-DRPA0567-RC-VSTAT-MIT-FEHL  VALUE 'VMF'.
              88 PO-DRPA0567-RC-VSTAT-OHNE-FEHL VALUE 'VOF'.
              88 PO-DRPA0567-RC-MELDUNGSFEHLER  VALUE 'MFE'.
           05 PO-DRPA0567-DATEN.
              10 PO-DRPA0567-PMPT.
                 COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************

