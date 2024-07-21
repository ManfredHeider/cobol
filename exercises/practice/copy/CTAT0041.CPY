      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04/12/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0041.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAT0041.CPv  $
      *
      *   Rev 2.2   Jan 07 2014 12:39:26   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.1   Jan 07 2014 12:30:52   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.0   Dec 4 2013 15:05:00   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE TBAT0041 TABLE
CCC004*    ( ABTJOBNAME                     CHAR(8) NOT NULL,
CCC004*      ABTTEXT                        VARCHAR(100),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0041                    *
      ******************************************************************
       01  TBAT0041-GESAMT.
        05 NI-TBAT0041-TAB.
           10 NI-TBAT0041-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05 NI-TBAT0041 REDEFINES NI-TBAT0041-TAB.
           10 NI-ABTJOBNAME         PIC S9(4) COMP.
           10 NI-ABTTEXT            PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-TBAT0041.
           10 ABTJOBNAME           PIC X(8).
           10 ABTTEXT.
              49 ABTTEXT-LEN       PIC S9(4) USAGE COMP.
              49 ABTTEXT-TEXT      PIC X(100).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
