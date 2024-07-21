      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0542.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0542.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 15:05:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  ZW-RUN-TIME-DATE.
           05 ZW-RUNDATE-A.
              10 ZW-RUNDATE-TAG-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE '.'.
              10 ZW-RUNDATE-MON-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE '.'.
              10 ZW-RUNDATE-JRH-A  PIC 99      VALUE ZERO.
              10 ZW-RUNDATE-JHR-A  PIC 99      VALUE ZERO.
           05 ZW-RUNDATE-B         PIC 9(8)    VALUE ZERO.
           05            REDEFINES ZW-RUNDATE-B.
              10 ZW-RUNDATE-JRH-B  PIC 99.
              10 ZW-RUNDATE-JHR-B  PIC 99.
              10 ZW-RUNDATE-MON-B  PIC 99.
              10 ZW-RUNDATE-TAG-B  PIC 99.
           05 ZW-RUNTIME-A.
              10 ZW-RUNTIME-STD-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE ':'.
              10 ZW-RUNTIME-MIN-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE ':'.
              10 ZW-RUNTIME-SEK-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE ':'.
              10 ZW-RUNTIME-HSEK-A PIC 99      VALUE ZERO.
           05 ZW-RUNTIME-B.
              10 ZW-RUNTIME-STD-B  PIC 99      VALUE ZERO.
              10 ZW-RUNTIME-MIN-B  PIC 99      VALUE ZERO.
              10 ZW-RUNTIME-SEK-B  PIC 99      VALUE ZERO.
              10 ZW-RUNTIME-HSEK-B PIC 99      VALUE ZERO.
           05 ZW-GENDATE-A.
              10 ZW-GENDATE-TAG-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE '.'.
              10 ZW-GENDATE-MON-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE '.'.
              10 ZW-GENDATE-JRH-A  PIC 99      VALUE ZERO.
              10 ZW-GENDATE-JHR-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE '-'.
              10 ZW-GENDATE-STD-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE ':'.
              10 ZW-GENDATE-MIN-A  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE ':'.
              10 ZW-GENDATE-SEK-A  PIC 99      VALUE ZERO.
           05 ZW-GENDATE-B.
              10 ZW-GENDATE-MON-B  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE SPACE.
              10 ZW-GENDATE-TAG-B  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE SPACE.
              10 ZW-GENDATE-JHR-B  PIC 99      VALUE ZERO.
              10 ZW-GENDATE-STD-B  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE SPACE.
              10 ZW-GENDATE-MIN-B  PIC 99      VALUE ZERO.
              10                   PIC X       VALUE SPACE.
              10 ZW-GENDATE-SEK-B  PIC 99      VALUE ZERO.
