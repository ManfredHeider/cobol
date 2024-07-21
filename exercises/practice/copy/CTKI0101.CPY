      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0101.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0101.CPv  $
      *
      *   Rev 2.1   Feb 11 2004 12:50:28   t13ex27
      *AKI00403 - DB-Änderung Fix 57
      *
      *   Rev 2.0   Feb 10 2004 08:45:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE ADRESSFEHLER
      ******************************************************************
CCC005*    EXEC SQL DECLARE ADRESSFEHLER TABLE
CCC004*    ( ADRFKEY                        SMALLINT NOT NULL,
CCC004*      ADRFTEXT                       CHAR(60) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  ADRESSFEHLER
      ******************************************************************
       01  ADRESSFEHLER-GESAMT.
         05 NI-ADRESSFEHLER-TAB.
           10 NI-ADRESSFEHLER-ELE PIC S9(4) COMP OCCURS 2 TIMES.
         05 NI-ADRESSFEHLER REDEFINES NI-ADRESSFEHLER-TAB.
           10 NI-ADRFKEY                   PIC S9(4) COMP.
           10 NI-ADRFTEXT                  PIC S9(4) COMP.
         05 DCL-ADRESSFEHLER.
           10 ADRFKEY                      PIC S9(4) COMP.
           10 ADRFTEXT                     PIC X(60).
      ******************************************************************