      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0440.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0440.CPv  $
      *
      *   Rev 2.1   Sep 11 2009 07:56:18   t13ex14
      *AKI00843: DB-Änderungen für Rel. 4.60 - Badenia
      *
      *   Rev 2.0   Aug 14 2009 10:43:44   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BAUSPARJAHRWERTE TABLE
CCC004*    ( BAJVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BAJGUTHPRODLFDNR               SMALLINT NOT NULL,
CCC004*      BAJJAHR                        SMALLINT NOT NULL,
CCC004*      BAJVLSUMJAHR                   DECIMAL(9, 2),
CCC004*      BAJWOPAVORJHR                  SMALLINT,
CCC004*      BAJWOPAVORVORJHR               SMALLINT,
CCC004*      BAJSUMFOEREINZHL               DECIMAL(9, 2),
CCC004*      BAJSALDWHNFOEKTO               DECIMAL(9, 2),
CCC004*      BAJSUMAVWLEINZHL               DECIMAL(9, 2),
CCC004*      BAJSUMFOERVORJHR               DECIMAL(9, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BAUSPARJAHRWERTE            *
      ******************************************************************
       01  BAUSPARJAHRWERTE-GESAMT.
        05  NI-BAUSPARJAHRWERTE-TAB.
           10 NI-BAUSPARJAHRWERTE-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05  NI-BAUSPARJAHRWERTE  REDEFINES  NI-BAUSPARJAHRWERTE-TAB.
           10 NI-BAJVERTRAGSID           PIC S9(4) COMP.
           10 NI-BAJGUTHPRODLFDNR        PIC S9(4) COMP.
           10 NI-BAJJAHR                 PIC S9(4) COMP.
           10 NI-BAJVLSUMJAHR            PIC S9(4) COMP.
           10 NI-BAJWOPAVORJHR           PIC S9(4) COMP.
           10 NI-BAJWOPAVORVORJHR        PIC S9(4) COMP.
           10 NI-BAJSUMFOEREINZHL        PIC S9(4) COMP.
           10 NI-BAJSALDWHNFOEKTO        PIC S9(4) COMP.
           10 NI-BAJSUMAVWLEINZHL        PIC S9(4) COMP.
           10 NI-BAJSUMFOERVORJHR        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG         PIC S9(4) COMP.
        05  DCL-BAUSPARJAHRWERTE.
           10 BAJVERTRAGSID        PIC X(13).
           10 BAJGUTHPRODLFDNR     PIC S9(4) USAGE COMP.
           10 BAJJAHR              PIC S9(4) USAGE COMP.
           10 BAJVLSUMJAHR         PIC S9(7)V9(2) USAGE COMP-3.
           10 BAJWOPAVORJHR        PIC S9(4) USAGE COMP.
           10 BAJWOPAVORVORJHR     PIC S9(4) USAGE COMP.
           10 BAJSUMFOEREINZHL     PIC S9(7)V9(2) USAGE COMP-3.
           10 BAJSALDWHNFOEKTO     PIC S9(7)V9(2) USAGE COMP-3.
           10 BAJSUMAVWLEINZHL     PIC S9(7)V9(2) USAGE COMP-3.
           10 BAJSUMFOERVORJHR     PIC S9(7)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
