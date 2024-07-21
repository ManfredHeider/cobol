      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0295.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0295.CPv  $
      *
      *   Rev 2.1   Jul 24 1999 11:13:30   T13EX25
      *APA00046, Neuerstellung
      *
      *   Rev 2.0   Mar 16 1999 09:57:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0295
      *    --------------------------------------------------------
      *    RESTARTINFORMATIONEN ZU PMPA0047
      *    --------------------------------------------------------
      *    ERSTELLT: Jäger-Sokoluk
      *    DATUM:    10.06.1998
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!10.06.99! 01 ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0047 (PVW-BATCH nach PNAP-Selektion)        *
      *    Da für diese Verarbeitung kein Wiederaufsetzen mit dem    *
      *    Schlüssel notwendig ist, werden keine Schlüssel gemerkt   *
      ****************************************************************
      *
       01  DRAT0295-RESTART.
           05 DRAT0295-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRAT0295'.

           05 DRAT0295-DATEN.
              10 DRAT0295-SELEKTIONSART             PIC S9(004) BINARY.
              10 DRAT0295-VSTAT                     PIC  X(004).
              10 DRAT0295-ANZ-PNAP-READ             PIC S9(009) BINARY.
              10 DRAT0295-ANZ-PNAP-V                PIC S9(009) BINARY.
              10 DRAT0295-ANZ-PNAP-NV               PIC S9(009) BINARY.
              10 DRAT0295-WERT-PNAP-V               PIC S9(011)V99
                                                        PACKED-DECIMAL.
              10 DRAT0295-WERT-PNAP-NV              PIC S9(011)V99
                                                        PACKED-DECIMAL.
