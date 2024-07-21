      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24/06/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0542.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0542.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:43:10   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 14:44:56   T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GDKAKTVERTRAG TABLE
CCC004*    ( AKVANGEBOTSNAME                CHAR(10) NOT NULL,
CCC004*      AKVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      AKVVERTRAGSNUMMER              CHAR(15) NOT NULL,
CCC004*      AKVUMST_TERMIN                 DATE NOT NULL,
CCC004*      AKVNACHNAME_VN                 CHAR(40) NOT NULL,
CCC004*      AKVVORNAME_VN                  CHAR(30) NOT NULL,
CCC004*      AKVANZ_VP                      SMALLINT NOT NULL,
CCC004*      AKVANNAHME_BIS                 DATE NOT NULL,
CCC004*      AKVAUFBEWAHR_BIS               DATE NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GDKAKTVERTRAG               *
      ******************************************************************
       01  GDKAKTVERTRAG-GESAMT.
        05 NI-GDKAKTVERTRAG-TAB.
           10 NI-GDKAKTVERTRAG-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05 NI-GDKAKTVERTRAG REDEFINES NI-GDKAKTVERTRAG-TAB.
           10 NI-AKVANGEBOTSNAME    PIC S9(4) COMP.
           10 NI-AKVVERTRAGSID      PIC S9(4) COMP.
           10 NI-AKVVERTRAGSNUMMER  PIC S9(4) COMP.
           10 NI-AKVUMST-TERMIN     PIC S9(4) COMP.
           10 NI-AKVNACHNAME-VN     PIC S9(4) COMP.
           10 NI-AKVVORNAME-VN      PIC S9(4) COMP.
           10 NI-AKVANZ-VP          PIC S9(4) COMP.
           10 NI-AKVANNAHME-BIS     PIC S9(4) COMP.
           10 NI-AKVAUFBEWAHR-BIS   PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-GDKAKTVERTRAG.
           10 AKVANGEBOTSNAME      PIC X(10).
           10 AKVVERTRAGSID        PIC X(13).
           10 AKVVERTRAGSNUMMER    PIC X(15).
           10 AKVUMST-TERMIN       PIC X(10).
           10 AKVNACHNAME-VN       PIC X(40).
           10 AKVVORNAME-VN        PIC X(30).
           10 AKVANZ-VP            PIC S9(4) USAGE COMP.
           10 AKVANNAHME-BIS       PIC X(10).
           10 AKVAUFBEWAHR-BIS     PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

