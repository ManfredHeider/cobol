      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16/04/19
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0533.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0533.CPv  $
      *
      *   Rev 2.2   Jun 25 2019 09:15:56   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.1   Jun 04 2019 14:46:48   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.0   Apr 16 2019 15:20:41   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE DBFILIALE TABLE
CCC004*    ( DBFVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      DBFFILART_KTY                  SMALLINT,
CCC004*      DBFFILART_KNR                  DECIMAL(15, 0),
CCC004*      DBFFILNAME_KTY                 SMALLINT,
CCC004*      DBFFILNAME_KNR                 DECIMAL(15, 0),
CCC004*      DBFFILSTR_KTY                  SMALLINT,
CCC004*      DBFFILSTR_KNR                  DECIMAL(15, 0),
CCC004*      DBFFILORT_KTY                  SMALLINT,
CCC004*      DBFFILORT_KNR                  DECIMAL(15, 0),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DBFILIALE                   *
      ******************************************************************
       01  DBFILIALE-GESAMT.
        05 NI-DBFILIALE-TAB.
           10 NI-DBFILIALE-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05 NI-DBFILIALE REDEFINES NI-DBFILIALE-TAB.
           10 NI-DBFVERTRAGSID      PIC S9(4) COMP.
           10 NI-DBFFILART-KTY      PIC S9(4) COMP.
           10 NI-DBFFILART-KNR      PIC S9(4) COMP.
           10 NI-DBFFILNAME-KTY     PIC S9(4) COMP.
           10 NI-DBFFILNAME-KNR     PIC S9(4) COMP.
           10 NI-DBFFILSTR-KTY      PIC S9(4) COMP.
           10 NI-DBFFILSTR-KNR      PIC S9(4) COMP.
           10 NI-DBFFILORT-KTY      PIC S9(4) COMP.
           10 NI-DBFFILORT-KNR      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-DBFILIALE.
           10 DBFVERTRAGSID        PIC X(13).
           10 DBFFILART-KTY        PIC S9(4) USAGE COMP.
           10 DBFFILART-KNR        PIC S9(15)V USAGE COMP-3.
           10 DBFFILNAME-KTY       PIC S9(4) USAGE COMP.
           10 DBFFILNAME-KNR       PIC S9(15)V USAGE COMP-3.
           10 DBFFILSTR-KTY        PIC S9(4) USAGE COMP.
           10 DBFFILSTR-KNR        PIC S9(15)V USAGE COMP-3.
           10 DBFFILORT-KTY        PIC S9(4) USAGE COMP.
           10 DBFFILORT-KNR        PIC S9(15)V USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


