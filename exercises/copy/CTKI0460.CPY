      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/03/13
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0460.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0460.CPv  $
      *
      *   Rev 2.1   Apr 03 2013 11:24:18   t13ex73
      *AKI01211: Anpassung der Produkt- und Tarifnamen für Badenia-
      *Produkte
      *
      *   Rev 2.0   Mar 6 2013 15:37:30   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BADPRODTARIFBEZ TABLE
CCC004*    ( PTBPRODSCHL                    CHAR(2) NOT NULL,
CCC004*      PTBTARIFVARIANTE               CHAR(1) NOT NULL,
CCC004*      PTBWOHNPSPARKEY                SMALLINT NOT NULL,
CCC004*      PTBPRODUKTTEXT                 CHAR(50),
CCC004*      PTBTARIFTEXT                   CHAR(50),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BADPRODTARIFBEZ             *
      ******************************************************************
       01  BADPRODTARIFBEZ-GESAMT.
        05 NI-BADPRODTARIFBEZ-TAB.
           10 NI-BADPRODTARIFBEZ-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05 NI-BADPRODTARIFBEZ REDEFINES NI-BADPRODTARIFBEZ-TAB.
           10 NI-PTBPRODSCHL        PIC S9(4) COMP.
           10 NI-PTBTARIFVARIANTE   PIC S9(4) COMP.
           10 NI-PTBWOHNPSPARKEY    PIC S9(4) COMP.
           10 NI-PTBPRODUKTTEXT     PIC S9(4) COMP.
           10 NI-PTBTARIFTEXT       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-BADPRODTARIFBEZ.
           10 PTBPRODSCHL          PIC X(2).
           10 PTBTARIFVARIANTE     PIC X(1).
           10 PTBWOHNPSPARKEY      PIC S9(4) USAGE COMP.
           10 PTBPRODUKTTEXT       PIC X(50).
           10 PTBTARIFTEXT         PIC X(50).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
