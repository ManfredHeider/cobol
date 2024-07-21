      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0446.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0446.CPv  $
      *
      *   Rev 2.1   Sep 28 2010 11:20:30   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-DB-Änderung und OZP
      *
      *   Rev 2.0   Aug 23 2010 10:43:28   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE ABGLSTEUERSATZ TABLE
CCC004*    ( ASTTABELLE                     CHAR(30) NOT NULL,
CCC004*      ASTATTRIBUT                    CHAR(30) NOT NULL,
CCC004*      ASTPRODUKTID                   SMALLINT NOT NULL,
CCC004*      ASTPARTGESELLSCH               SMALLINT NOT NULL,
CCC004*      ASTGESCHAEFTSZW                CHAR(2) NOT NULL,
CCC004*      ASTVEZSPARTE                   SMALLINT NOT NULL,
CCC004*      ASTOKISPARTE                   CHAR(2),
CCC004*      ASTSTEUERSATZTYP               CHAR(3),
CCC004*      ASTALLEPRODUKTID               CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ABGLSTEUERSATZ                     *
      ******************************************************************
       01  ABGLSTEUERSATZ-GESAMT.
        05 NI-ABGLSTEUERSATZ-TAB.
           10 NI-ABGLSTEUERSATZ-ELE PIC S9(04) COMP OCCURS 9 TIMES.
        05 NI-ABGLSTEUERSATZ REDEFINES NI-ABGLSTEUERSATZ-TAB.
           10 NI-ASTTABELLE         PIC S9(4) COMP.
           10 NI-ASTATTRIBUT        PIC S9(4) COMP.
           10 NI-ASTPRODUKTID       PIC S9(4) COMP.
           10 NI-ASTPARTGESELLSCH   PIC S9(4) COMP.
           10 NI-ASTGESCHAEFTSZW    PIC S9(4) COMP.
           10 NI-ASTVEZSPARTE       PIC S9(4) COMP.
           10 NI-ASTOKISPARTE       PIC S9(4) COMP.
           10 NI-ASTSTEUERSATZTYP   PIC S9(4) COMP.
           10 NI-ASTALLEPRODUKTID   PIC S9(4) COMP.
        05 DCL-ABGLSTEUERSATZ.
           10 ASTTABELLE           PIC X(30).
           10 ASTATTRIBUT          PIC X(30).
           10 ASTPRODUKTID         PIC S9(4) USAGE COMP.
           10 ASTPARTGESELLSCH     PIC S9(4) USAGE COMP.
           10 ASTGESCHAEFTSZW      PIC X(2).
           10 ASTVEZSPARTE         PIC S9(4) USAGE COMP.
           10 ASTOKISPARTE         PIC X(2).
           10 ASTSTEUERSATZTYP     PIC X(3).
           10 ASTALLEPRODUKTID     PIC X(1).
      ************************** ENDE-DCLGEN *************************
