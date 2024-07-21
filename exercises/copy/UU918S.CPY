      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU918S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU918S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:07:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    30.06.93 16:45:41
      ******************************************************
      *                                                    *
      *         COPY-MEMBER PROVISION-EIGENGESCHAEFT       *
      *                                                    *
      *         ERSTELLT:             HUEBNER/GEDO         *
      *                               29. JULI 1981        *
      *         ERWEITERT UM 140 %:   PRIGGEN/DV-D         *
      *                               JANUAR   1985        *
      *                                                    *
      *         VBP7 --> 136 %                             *
      *         VBP9 --> 140 %        HECKER  30.06.93     *
      *                                                    *
      ******************************************************
       01  UU918S.
           05  EPROV.
               10  FILLER          PIC X               VALUE '1'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1000.
               10  FILLER          PIC X               VALUE '2'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1100.
               10  FILLER          PIC X               VALUE '3'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1200.
               10  FILLER          PIC X               VALUE '4'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1260.
               10  FILLER          PIC X               VALUE '5'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1320.
               10  FILLER          PIC X               VALUE '6'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1360.
               10  FILLER          PIC X               VALUE '8'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1400.
               10  FILLER          PIC X               VALUE SPACE.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 9999.
           05  EPROV1      REDEFINES   EPROV   OCCURS 8
                                               INDEXED BY WS-IND.
               10  EPROVNR         PIC X.
               10  EPROVS          PIC 999V9   COMP-3.

