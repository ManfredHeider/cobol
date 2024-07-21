      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0060.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAT0060.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 14:57:08   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
           MOVE FUNCTION CURRENT-DATE (1:8)TO ZW-RUNDATE-B
           MOVE FUNCTION CURRENT-DATE (9:) TO ZW-RUNTIME-B
           MOVE   WHEN-COMPILED         TO  ZW-GENDATE-B
           IF ZW-GENDATE-JHR-B > 88
              MOVE 19 TO ZW-GENDATE-JRH-A
           ELSE
              MOVE 20 TO ZW-GENDATE-JRH-A
           END-IF
           MOVE ZW-RUNDATE-TAG-B  TO ZW-RUNDATE-TAG-A
           MOVE ZW-RUNDATE-MON-B  TO ZW-RUNDATE-MON-A
           MOVE ZW-RUNDATE-JHR-B  TO ZW-RUNDATE-JHR-A
           MOVE ZW-RUNDATE-JRH-B  TO ZW-RUNDATE-JRH-A
           MOVE ZW-RUNTIME-STD-B  TO ZW-RUNTIME-STD-A
           MOVE ZW-RUNTIME-MIN-B  TO ZW-RUNTIME-MIN-A
           MOVE ZW-RUNTIME-SEK-B  TO ZW-RUNTIME-SEK-A
           MOVE ZW-RUNTIME-HSEK-B TO ZW-RUNTIME-HSEK-A
           MOVE ZW-GENDATE-TAG-B  TO ZW-GENDATE-TAG-A
           MOVE ZW-GENDATE-MON-B  TO ZW-GENDATE-MON-A
           MOVE ZW-GENDATE-JHR-B  TO ZW-GENDATE-JHR-A
           MOVE ZW-GENDATE-STD-B  TO ZW-GENDATE-STD-A
           MOVE ZW-GENDATE-MIN-B  TO ZW-GENDATE-MIN-A
           MOVE ZW-GENDATE-SEK-B  TO ZW-GENDATE-SEK-A
