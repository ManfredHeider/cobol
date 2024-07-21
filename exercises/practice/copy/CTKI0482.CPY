      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04/08/17
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0482.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0482.CPv  $
      *
      *   Rev 2.3   Aug 28 2017 09:58:54   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.2   Aug 23 2017 05:55:20   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.1   Aug 08 2017 14:25:22   t13ex39
      *AKI01533: Vertriebshinweise Privatkredit in KI & 3 neue Sele
      *ktionen
      *
      *   Rev 2.0   Aug 4 2017 09:24:17   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BANKVERTRIEBSHINW TABLE
CCC004*    ( BVHVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BVHLAUFZEIT                    SMALLINT,
CCC004*      BVHTILGGRAD                    SMALLINT,
CCC004*      BVHVERSPAK_KTY                 SMALLINT,
CCC004*      BVHVERSPAK_KNR                 DECIMAL(15, 0),
CCC004*      BVHSALDO                       DECIMAL(14, 2),
CCC004*      BVHEFFEKTIVZINS                DECIMAL(6, 4),
CCC004*      BVHNOMINALZINS                 DECIMAL(6, 4),
CCC004*      BVHAUFSTOCKUNGP                CHAR(1),
CCC004*      BVHHOCHKREDITAF                CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BANKVERTRIEBSHINW           *
      ******************************************************************
       01  BANKVERTRIEBSHINW-GESAMT.
        05 NI-BANKVERTRIEBSHINW-TAB.
           10 NI-BANKVERTRIEBSHINW-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-BANKVERTRIEBSHINW REDEFINES NI-BANKVERTRIEBSHINW-TAB.
           10 NI-BVHVERTRAGSID      PIC S9(4) COMP.
           10 NI-BVHLAUFZEIT        PIC S9(4) COMP.
           10 NI-BVHTILGGRAD        PIC S9(4) COMP.
           10 NI-BVHVERSPAK-KTY     PIC S9(4) COMP.
           10 NI-BVHVERSPAK-KNR     PIC S9(4) COMP.
           10 NI-BVHSALDO           PIC S9(4) COMP.
           10 NI-BVHEFFEKTIVZINS    PIC S9(4) COMP.
           10 NI-BVHNOMINALZINS     PIC S9(4) COMP.
           10 NI-BVHAUFSTOCKUNGP    PIC S9(4) COMP.
           10 NI-BVHHOCHKREDITAF    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-BANKVERTRIEBSHINW.
           10 BVHVERTRAGSID        PIC X(13).
           10 BVHLAUFZEIT          PIC S9(4) USAGE COMP.
           10 BVHTILGGRAD          PIC S9(4) USAGE COMP.
           10 BVHVERSPAK-KTY       PIC S9(4) USAGE COMP.
           10 BVHVERSPAK-KNR       PIC S9(15)V USAGE COMP-3.
           10 BVHSALDO             PIC S9(12)V9(2) USAGE COMP-3.
           10 BVHEFFEKTIVZINS      PIC S9(2)V9(4) USAGE COMP-3.
           10 BVHNOMINALZINS       PIC S9(2)V9(4) USAGE COMP-3.
           10 BVHAUFSTOCKUNGP      PIC X(1).
           10 BVHHOCHKREDITAF      PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************



