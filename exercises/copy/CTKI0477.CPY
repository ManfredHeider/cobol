      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22/06/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0477.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0477.CPv  $
      *
      *   Rev 2.2   Jul 09 2015 10:16:18   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.1   Jul 09 2015 09:50:56   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.0   Jun 22 2015 17:18:33   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE LEBENKAPENTNAHME TABLE
CCC004*    ( LKEVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LKEKAPENTNNR                   SMALLINT NOT NULL,
CCC004*      LKEKAPENTNBETRAG               DECIMAL(13, 2) NOT NULL,
CCC004*      LKEKAPENTNDAT                  DATE NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENKAPENTNAHME            *
      ******************************************************************
       01  LEBENKAPENTNAHME-GESAMT.
        05 NI-LEBENKAPENTNAHME-TAB.
           10 NI-LEBENKAPENTNAHME-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05 NI-LEBENKAPENTNAHME REDEFINES NI-LEBENKAPENTNAHME-TAB.
           10 NI-LKEVERTRAGSID      PIC S9(4) COMP.
           10 NI-LKEKAPENTNNR       PIC S9(4) COMP.
           10 NI-LKEKAPENTNBETRAG   PIC S9(4) COMP.
           10 NI-LKEKAPENTNDAT      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-LEBENKAPENTNAHME.
           10 LKEVERTRAGSID        PIC X(13).
           10 LKEKAPENTNNR         PIC S9(4) USAGE COMP.
           10 LKEKAPENTNBETRAG     PIC S9(11)V9(2) USAGE COMP-3.
           10 LKEKAPENTNDAT        PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


