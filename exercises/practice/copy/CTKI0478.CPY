      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01/07/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0478.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0478.CPv  $
      *
      *   Rev 2.2   Jul 09 2015 10:16:26   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.1   Jul 09 2015 09:51:04   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.0   Jul 1 2015 10:45:51   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE LEBENGELRENTE TABLE
CCC004*    ( LGRVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LGRRNTBEGINN                   DATE NOT NULL,
CCC004*      LGRRNTART                      SMALLINT NOT NULL,
CCC004*      LGRRNTABLAUF                   DATE,
CCC004*      LGRRNTZAHLW_KTY                SMALLINT NOT NULL,
CCC004*      LGRRNTZAHLW_KNR                SMALLINT NOT NULL,
CCC004*      LGRGSMTHOEHE                   DECIMAL(13, 2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENGELRENTE               *
      ******************************************************************
       01  LEBENGELRENTE-GESAMT.
        05 NI-LEBENGELRENTE-TAB.
           10 NI-LEBENGELRENTE-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-LEBENGELRENTE REDEFINES NI-LEBENGELRENTE-TAB.
           10 NI-LGRVERTRAGSID      PIC S9(4) COMP.
           10 NI-LGRRNTBEGINN       PIC S9(4) COMP.
           10 NI-LGRRNTART          PIC S9(4) COMP.
           10 NI-LGRRNTABLAUF       PIC S9(4) COMP.
           10 NI-LGRRNTZAHLW-KTY    PIC S9(4) COMP.
           10 NI-LGRRNTZAHLW-KNR    PIC S9(4) COMP.
           10 NI-LGRGSMTHOEHE       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-LEBENGELRENTE.
           10 LGRVERTRAGSID        PIC X(13).
           10 LGRRNTBEGINN         PIC X(10).
           10 LGRRNTART            PIC S9(4) USAGE COMP.
           10 LGRRNTABLAUF         PIC X(10).
           10 LGRRNTZAHLW-KTY      PIC S9(4) USAGE COMP.
           10 LGRRNTZAHLW-KNR      PIC S9(4) USAGE COMP.
           10 LGRGSMTHOEHE         PIC S9(11)V9(2) USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


