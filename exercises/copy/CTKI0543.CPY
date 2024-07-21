      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0543.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0543.CPv  $
      *
      *   Rev 2.2   Jul 31 2020 14:00:56   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.1   Jul 01 2020 09:43:48   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:02:32   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GDKAKTVERSPERS TABLE
CCC004*    ( AKPANGEBOTSNAME                CHAR(10) NOT NULL,
CCC004*      AKPVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      AKPPERSONLFDNR                 SMALLINT NOT NULL,
CCC004*      AKPUMST_TERMIN                 DATE NOT NULL,
CCC004*      AKPNACHNAME_VP                 CHAR(40) NOT NULL,
CCC004*      AKPVORNAME_VP                  CHAR(30) NOT NULL,
CCC004*      AKPGEBDAT_VP                   DATE NOT NULL,
CCC004*      AKPANZ_ANGEBOTE                SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GDKAKTVERSPERS              *
      ******************************************************************
       01  GDKAKTVERSPERS-GESAMT.
        05 NI-GDKAKTVERSPERS-TAB.
           10 NI-GDKAKTVERSPERS-ELE PIC S9(04) COMP OCCURS 9 TIMES.
        05 NI-GDKAKTVERSPERS REDEFINES NI-GDKAKTVERSPERS-TAB.
           10 NI-AKPANGEBOTSNAME    PIC S9(4) COMP.
           10 NI-AKPVERTRAGSID      PIC S9(4) COMP.
           10 NI-AKPPERSONLFDNR     PIC S9(4) COMP.
           10 NI-AKPUMST-TERMIN     PIC S9(4) COMP.
           10 NI-AKPNACHNAME-VP     PIC S9(4) COMP.
           10 NI-AKPVORNAME-VP      PIC S9(4) COMP.
           10 NI-AKPGEBDAT-VP       PIC S9(4) COMP.
           10 NI-AKPANZ-ANGEBOTE    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-GDKAKTVERSPERS.
           10 AKPANGEBOTSNAME      PIC X(10).
           10 AKPVERTRAGSID        PIC X(13).
           10 AKPPERSONLFDNR       PIC S9(4) USAGE COMP.
           10 AKPUMST-TERMIN       PIC X(10).
           10 AKPNACHNAME-VP       PIC X(40).
           10 AKPVORNAME-VP        PIC X(30).
           10 AKPGEBDAT-VP         PIC X(10).
           10 AKPANZ-ANGEBOTE      PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

