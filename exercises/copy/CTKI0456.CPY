      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19/11/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0456.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0456.CPv  $
      *
      *   Rev 2.1   Nov 22 2017 09:45:48   t13ae31
      *AKI01549: CKV Tarifdetails Copies
      *
      *   Rev 2.0   Nov 19 2012 10:30:24   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE CKVTARIFDETAILS TABLE
CCC004*    ( CKVTARIFKEY                    SMALLINT NOT NULL,
CCC004*      CKVTARIFBEZ                    CHAR(8) NOT NULL,
CCC004*      CKVTARIFNAME                   VARCHAR(30) NOT NULL,
CCC004*      CKVTARIFVERSION                CHAR(7) NOT NULL,
CCC004*      CKVTARIFART                    CHAR(1) NOT NULL,
CCC004*      CKVTARIFLFDNR                  SMALLINT NOT NULL,
CCC004*      CKVTARIFTEXT                   VARCHAR(500) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CKVTARIFDETAILS             *
      ******************************************************************
       01  CKVTARIFDETAILS-GESAMT.
        05 NI-CKVTARIFDETAILS-TAB.
           10 NI-CKVTARIFDETAILS-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-CKVTARIFDETAILS REDEFINES NI-CKVTARIFDETAILS-TAB.
           10 NI-CKVTARIFKEY        PIC S9(4) COMP.
           10 NI-CKVTARIFBEZ        PIC S9(4) COMP.
           10 NI-CKVTARIFNAME       PIC S9(4) COMP.
           10 NI-CKVTARIFVERSION    PIC S9(4) COMP.
           10 NI-CKVTARIFART        PIC S9(4) COMP.
           10 NI-CKVTARIFLFDNR      PIC S9(4) COMP.
           10 NI-CKVTARIFTEXT       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-CKVTARIFDETAILS.
           10 CKVTARIFKEY          PIC S9(4) USAGE COMP.
           10 CKVTARIFBEZ          PIC X(8).
           10 CKVTARIFNAME.
              49 CKVTARIFNAME-LEN
                 PIC S9(4) USAGE COMP.
              49 CKVTARIFNAME-TEXT
                 PIC X(30).
           10 CKVTARIFVERSION      PIC X(7).
           10 CKVTARIFART          PIC X(1).
           10 CKVTARIFLFDNR        PIC S9(4) USAGE COMP.
           10 CKVTARIFTEXT.
              49 CKVTARIFTEXT-LEN
                 PIC S9(4) USAGE COMP.
              49 CKVTARIFTEXT-TEXT
                 PIC X(500).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

