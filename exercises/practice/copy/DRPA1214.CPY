      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1214.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/MF/CPY/DRPA1214.CPv  $
      *
      *   Rev 2.5   Oct 18 2002 14:01:14   p13sp16
      *Korrektur der Längen der einzelnen Zeilen
      *wg. Speicherung auf RESTART-Tabelle
      *-- Korrektur des Archives durch P13SP16 wg. falscher Rev-Nr.
      *
      *   Rev 2.4   Jan 28 2000 11:28:24   T13EX44
      * 
      *
      *   Rev 2.3   Apr 21 1999 17:34:38   T13EX22
      *ERGAENZUNGEN FUER PROTOKOLL
      *
      *   Rev 2.2   Apr 19 1999 10:12:40   T13EX21
      *APA00012 - frei (kann benutzt werden)
      *
      *   Rev 2.1   Apr 15 1999 17:44:24   T13EX21
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:04   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1214                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0023                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    20.04.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.04.99! 01 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      * APA00066!22.11.99!3.0 ! KOMPLETTE UMSTRUKTURIERUNG! SCHEIDEL *
      * APA00066!07.02.00!3.1 ! KORREKTUR ZEILENLAENGEN   ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN FÜR PROTOKOLLAUSGABE UND TSO-RESTART (PMPA0023)    *
      *                                                              *
      ****************************************************************

       01  PROT-INFO-OP.

           05 PROT-ID.
              10 FILLER                             PIC  X(080)
                                               VALUE 'RESTART-INFO '.
           05 PROT-DATEN.
              10 PROT-ZEILE-01.
                 15 FILLER                 PIC  X(080)
                                           VALUE 'PMPA0023'.

              10 PROT-ZEILE-02.
                 15 FILLER                 PIC  X(040)
                    VALUE 'PROVISIONSABRECHNUNG FÜR GESELLSCHAFT '.
                 15 PROT-PGEB-KURZ         PIC  X(002).
                 15 FILLER                 PIC  X(005)    VALUE SPACES.
                 15 PROT-PGEB-LANG         PIC  X(010).
                 15 FILLER                 PIC  X(023).

              10 PROT-ZEILE-03.
                 15 FILLER                 PIC  X(009)
                    VALUE 'LAUF VOM '.
                 15 PROT-DATE              PIC  X(010).
                 15 FILLER                 PIC  X(001)    VALUE SPACE.
                 15 PROT-TIME              PIC  X(008).
                 15 FILLER                 PIC  X(004)    VALUE ' UHR'.
                 15 FILLER                 PIC  X(048).

              10 PROT-ZEILE-04.
                 15 FILLER                 PIC  X(080) VALUE ALL '-'.

              10 PROT-ZEILE-05.
                 15 FILLER                 PIC  X(039)
                    VALUE 'PMPE-             ANZAHL       PROZ   '.
                 15 FILLER                 PIC  X(036)
                    VALUE 'PMPP-             ANZAHL       PROZ'.
                 15 FILLER                 PIC  X(005).

      *    PMPE GESAMT
              10 PROT-ZEILE-06.
                 15 FILLER                 PIC  X(018)
                    VALUE 'GESAMT           :'.
                 15 PROT-PMPE-GES          PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPE-GES      PIC  9(003)    VALUE 100.
                 15 FILLER                 PIC  X(004).
      *    PMPP GESAMT
                 15 FILLER                 PIC  X(018)
                    VALUE 'GESAMT           :'.
                 15 PROT-PMPP-GES          PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPP-GES      PIC  9(003)    VALUE 100.
                 15 FILLER                 PIC  X(006)    VALUE ' '.

      *    PMPE VERARBEITET
              10 PROT-ZEILE-07.
                 15 FILLER                 PIC  X(018)
                    VALUE 'VERARBEITET      :'.
                 15 PROT-PMPE-V            PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPE-V        PIC  9(003).
                 15 FILLER                 PIC  X(004).
      *    PMPP VERARBEITET
                 15 FILLER                 PIC  X(018)
                    VALUE 'VERARBEITET      :'.
                 15 PROT-PMPP-V            PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPP-V        PIC  9(003).
                 15 FILLER                 PIC  X(006)    VALUE ' '.

      *    PMPE NEUTRAL
              10 PROT-ZEILE-08.
                 15 FILLER                 PIC  X(018)
                    VALUE 'NEUTRAL          :'.
                 15 PROT-PMPE-N            PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPE-N        PIC  9(003).
                 15 FILLER                 PIC  X(004).
      *    PMPP NEUTRAL
                 15 FILLER                 PIC  X(018)
                    VALUE 'NEUTRAL          :'.
                 15 PROT-PMPP-N            PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPP-N        PIC  9(003).
                 15 FILLER                 PIC  X(006)    VALUE ' '.

      *    PMPE NICHT VERARBEITET
              10 PROT-ZEILE-09.
                 15 FILLER                 PIC  X(018)
                    VALUE 'NICHT VERARBEITET:'.
                 15 PROT-PMPE-N-V          PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPE-N-V      PIC  9(003).
                 15 FILLER                 PIC  X(004).
      *    PMPP NICHT VERARBEITET
                 15 FILLER                 PIC  X(018)
                    VALUE 'NICHT VERARBEITET:'.
                 15 PROT-PMPP-N-V          PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPP-N-V      PIC  9(003).
                 15 FILLER                 PIC  X(006)    VALUE ' '.

      *    ABBRUCH
              10 PROT-ZEILE-10.
                 15 FILLER                 PIC  X(018)
                    VALUE 'MIT ABBRUCH S145 :'.
                 15 PROT-PMPE-ERROR        PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPE-ERROR    PIC  9(003).
                 15 FILLER                 PIC  X(004).
      *    PMPP MIT ABBRUCH S145
                 15 FILLER                 PIC  X(018)
                    VALUE 'MIT ABBRUCH S145 :'.
                 15 PROT-PMPP-ERROR        PIC  9(012).
                 15 FILLER                 PIC  X(002).
                 15 PROT-PRZ-PMPP-ERROR    PIC  9(003).
                 15 FILLER                 PIC  X(006)  VALUE ' '.

              10 PROT-ZEILE-11.
                 15 FILLER                 PIC  X(080) VALUE ALL '-'.

      *    PVER
              10 PROT-ZEILE-12.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PVER GESCHRIEBEN :'.
                 15 PROT-PVER-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PVPT
                 15 FILLER                 PIC  X(018)
                    VALUE 'PVPT-GESCHRIEBEN :'.
                 15 PROT-PVPT-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(011).

      *    PVER
              10 PROT-ZEILE-13.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PVER-UPDATE      :'.
                 15 PROT-PVER-UPDATE       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PVPT
                 15 FILLER                 PIC  X(018)
                    VALUE 'PVPT-UPDATE      :'.
                 15 PROT-PVPT-UPDATE       PIC  9(012).
                 15 FILLER                 PIC  X(011).

      *    PVER
              10 PROT-ZEILE-14.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PVOR GESCHRIEBEN :'.
                 15 PROT-PVOR-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PVPT
                 15 FILLER                 PIC  X(018)
                    VALUE 'PVER VORV. MARK. :'.
                 15 PROT-PVER-MARKIERT     PIC  9(012).
                 15 FILLER                 PIC  X(011).

              10 PROT-ZEILE-15.
                 15 FILLER                 PIC  X(080) VALUE ALL '-'.

      *    PMBE OK
              10 PROT-ZEILE-16.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMBE GESCHRIEBEN :'.
                 15 PROT-PMBE-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PMBE PNAP
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMBE IN PNAP     :'.
                 15 PROT-PMBE-PNAP         PIC  9(012).
                 15 FILLER                 PIC  X(011).

      *    PMPR OK
              10 PROT-ZEILE-17.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMPR GESCHRIEBEN :'.
                 15 PROT-PMPR-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PMPR PNAP
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMPR IN PNAP     :'.
                 15 PROT-PMPR-PNAP         PIC  9(012).
                 15 FILLER                 PIC  X(011).

      *    PMPT OK
              10 PROT-ZEILE-18.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMPT GESCHRIEBEN :'.
                 15 PROT-PMPT-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PMPT PNAP
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMPT IN PNAP     :'.
                 15 PROT-PMPT-PNAP         PIC  9(012).
                 15 FILLER                 PIC  X(011).

      *    PMDS OK
              10 PROT-ZEILE-19.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMDS GESCHRIEBEN :'.
                 15 PROT-PMDS-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(050).

              10 PROT-ZEILE-20.
                 15 FILLER                 PIC  X(080) VALUE ALL '-'.

      *    PDPT OK
              10 PROT-ZEILE-21.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PDPT GESCHRIEBEN :'.
                 15 PROT-PDPT-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PAMS
                 15 FILLER                 PIC  X(018)
                    VALUE 'PAMS GESCHRIEBEN :'.
                 15 PROT-PAMS-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(011).

      *    PEST OK
              10 PROT-ZEILE-22.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PEST GESCHRIEBEN :'.
                 15 PROT-PEST-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    PAEV
                 15 FILLER                 PIC  X(018)
                    VALUE 'PAEV GESCHRIEBEN :'.
                 15 PROT-PAEV-INSERT       PIC  9(012).
                 15 FILLER                 PIC  X(011).

      *    DGV USED
              10 PROT-ZEILE-23.
                 15 FILLER                 PIC  X(018)
                    VALUE 'AKT-DGV USED     :'.
                 15 PROT-DGV-USED          PIC  9(012).
                 15 FILLER                 PIC  X(009).
      *    DGV UNUSED
                 15 FILLER                 PIC  X(018)
                    VALUE 'AKT-DGV-UNUSED   :'.
                 15 PROT-DGV-UNUSED        PIC  9(012).
                 15 FILLER                 PIC  X(011).

              10 PROT-ZEILE-24.
                 15 FILLER                 PIC  X(080) VALUE ALL '-'.

      *    PMPE GESICHERT
              10 PROT-ZEILE-25.
                 15 FILLER                 PIC  X(018)
                    VALUE 'PMPE MARKIERT    :'.
                 15 PROT-PMPE-SAVED        PIC  9(012).
                 15 FILLER                 PIC  X(050).

