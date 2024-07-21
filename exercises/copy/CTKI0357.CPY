      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27/05/10
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0357.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/CTKI0357.CPv  $
      *
      *   Rev 2.12   Jun 28 2010 10:02:38   t13ex48
      *AKI00970: DB-Änderungen für Rel. 5.12: VERTRAG und VERTRAGUR
      *LBEZ
      *
      *   Rev 2.0   May 27 2010 14:55:45   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE VERTRAG TABLE
CCC004*    ( VERVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VERFRAPERSONENID               CHAR(13),
CCC004*      VERFRALFDNR                    SMALLINT,
CCC004*      VERPARTGESELLSCHID             SMALLINT NOT NULL,
CCC004*      VERFREMDVERTRAGKZ              CHAR(1) NOT NULL,
CCC004*      VERVERTRAGSNUMMER              CHAR(15),
CCC004*      VERZAHLUNGSARTID               SMALLINT,
CCC004*      VERZAHLUNGSWEISEID             SMALLINT,
CCC004*      VERVERTRZUSTANDID              SMALLINT,
CCC004*      VERVERTRZUSTANDDAT             DATE,
CCC004*      VERABSCHLUSSGRUND              VARCHAR(30),
CCC004*      VERDATVERTRAEND                DATE,
CCC004*      VERREMDVERTRGESNAM             VARCHAR(25),
CCC004*      VERGESAMTBEITRAG               DECIMAL(11, 2),
CCC004*      VERGESBEITRWKZ                 CHAR(1) NOT NULL,
CCC004*      VERFREISTAUFTRAG               CHAR(1) NOT NULL,
CCC004*      VERINTERNEVERTRNR              INTEGER,
CCC004*      VERPRUEFZIFFVNR                SMALLINT,
CCC004*      VERMONATDERHAUPTF              SMALLINT,
CCC004*      VERNACHLASSKZ                  CHAR(1) NOT NULL,
CCC004*      VERGEBUENDELTKZ                CHAR(1) NOT NULL,
CCC004*      VERSTORNODATUM                 DATE,
CCC004*      VERAENDERUNGSDATUM             TIMESTAMP,
CCC004*      VERVERTRAGSABLAUF              DATE,
CCC004*      VERVERTRAGSBEGINN              DATE,
CCC004*      VERNOTIZID                     CHAR(13),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      VERABSCHLUSSVBNR               INTEGER,
CCC004*      VEREIGENVERMKZ                 CHAR(1) NOT NULL,
CCC004*      VERVSNRZUSATZ                  CHAR(10),
CCC004*      VERDATPRODLOESCH               DATE,
CCC004*      VEREHELEUTEKZ                  CHAR(1),
CCC004*      VERMANDANTNR                   INTEGER NOT NULL,
CCC004*      VERGESBEITRBKZ                 CHAR(1),
CCC004*      VERSTORNGRND_KTY               SMALLINT,
CCC004*      VERSTORNGRND_KNR               SMALLINT,
CCC004*      VERFIRMENVTGKZ                 CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VERTRAG                     *
      ******************************************************************
       01  VERTRAG-GESAMT.
        05 NI-VERTRAG-TAB.
           10 NI-VERTRAG-ELE PIC S9(04) COMP OCCURS 37 TIMES.
        05 NI-VERTRAG REDEFINES NI-VERTRAG-TAB.
           10 NI-VERVERTRAGSID      PIC S9(4) COMP.
           10 NI-VERFRAPERSONENID   PIC S9(4) COMP.
           10 NI-VERFRALFDNR        PIC S9(4) COMP.
           10 NI-VERPARTGESELLSCHID PIC S9(4) COMP.
           10 NI-VERFREMDVERTRAGKZ  PIC S9(4) COMP.
           10 NI-VERVERTRAGSNUMMER  PIC S9(4) COMP.
           10 NI-VERZAHLUNGSARTID   PIC S9(4) COMP.
           10 NI-VERZAHLUNGSWEISEID PIC S9(4) COMP.
           10 NI-VERVERTRZUSTANDID  PIC S9(4) COMP.
           10 NI-VERVERTRZUSTANDDAT PIC S9(4) COMP.
           10 NI-VERABSCHLUSSGRUND  PIC S9(4) COMP.
           10 NI-VERDATVERTRAEND    PIC S9(4) COMP.
           10 NI-VERREMDVERTRGESNAM PIC S9(4) COMP.
           10 NI-VERGESAMTBEITRAG   PIC S9(4) COMP.
           10 NI-VERGESBEITRWKZ     PIC S9(4) COMP.
           10 NI-VERFREISTAUFTRAG   PIC S9(4) COMP.
           10 NI-VERINTERNEVERTRNR  PIC S9(4) COMP.
           10 NI-VERPRUEFZIFFVNR    PIC S9(4) COMP.
           10 NI-VERMONATDERHAUPTF  PIC S9(4) COMP.
           10 NI-VERNACHLASSKZ      PIC S9(4) COMP.
           10 NI-VERGEBUENDELTKZ    PIC S9(4) COMP.
           10 NI-VERSTORNODATUM     PIC S9(4) COMP.
           10 NI-VERAENDERUNGSDATUM PIC S9(4) COMP.
           10 NI-VERVERTRAGSABLAUF  PIC S9(4) COMP.
           10 NI-VERVERTRAGSBEGINN  PIC S9(4) COMP.
           10 NI-VERNOTIZID         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-VERABSCHLUSSVBNR   PIC S9(4) COMP.
           10 NI-VEREIGENVERMKZ     PIC S9(4) COMP.
           10 NI-VERVSNRZUSATZ      PIC S9(4) COMP.
           10 NI-VERDATPRODLOESCH   PIC S9(4) COMP.
           10 NI-VEREHELEUTEKZ      PIC S9(4) COMP.
           10 NI-VERMANDANTNR       PIC S9(4) COMP.
           10 NI-VERGESBEITRBKZ     PIC S9(4) COMP.
           10 NI-VERSTORNGRND-KTY   PIC S9(4) COMP.
           10 NI-VERSTORNGRND-KNR   PIC S9(4) COMP.
           10 NI-VERFIRMENVTGKZ     PIC S9(4) COMP.
        05 DCL-VERTRAG.
           10 VERVERTRAGSID        PIC X(13).
           10 VERFRAPERSONENID     PIC X(13).
           10 VERFRALFDNR          PIC S9(4) USAGE COMP.
           10 VERPARTGESELLSCHID   PIC S9(4) USAGE COMP.
           10 VERFREMDVERTRAGKZ    PIC X(1).
           10 VERVERTRAGSNUMMER    PIC X(15).
           10 VERZAHLUNGSARTID     PIC S9(4) USAGE COMP.
           10 VERZAHLUNGSWEISEID   PIC S9(4) USAGE COMP.
           10 VERVERTRZUSTANDID    PIC S9(4) USAGE COMP.
           10 VERVERTRZUSTANDDAT   PIC X(10).
           10 VERABSCHLUSSGRUND.
              49 VERABSCHLUSSGRUND-LEN
                 PIC S9(4) USAGE COMP.
              49 VERABSCHLUSSGRUND-TEXT
                 PIC X(30).
           10 VERDATVERTRAEND      PIC X(10).
           10 VERREMDVERTRGESNAM.
              49 VERREMDVERTRGESNAM-LEN
                 PIC S9(4) USAGE COMP.
              49 VERREMDVERTRGESNAM-TEXT
                 PIC X(25).
           10 VERGESAMTBEITRAG     PIC S9(9)V9(2) USAGE COMP-3.
           10 VERGESBEITRWKZ       PIC X(1).
           10 VERFREISTAUFTRAG     PIC X(1).
           10 VERINTERNEVERTRNR    PIC S9(9) USAGE COMP.
           10 VERPRUEFZIFFVNR      PIC S9(4) USAGE COMP.
           10 VERMONATDERHAUPTF    PIC S9(4) USAGE COMP.
           10 VERNACHLASSKZ        PIC X(1).
           10 VERGEBUENDELTKZ      PIC X(1).
           10 VERSTORNODATUM       PIC X(10).
           10 VERAENDERUNGSDATUM   PIC X(26).
           10 VERVERTRAGSABLAUF    PIC X(10).
           10 VERVERTRAGSBEGINN    PIC X(10).
           10 VERNOTIZID           PIC X(13).
           10 LETZTEAENDERUNG      PIC X(26).
           10 VERABSCHLUSSVBNR     PIC S9(9) USAGE COMP.
           10 VEREIGENVERMKZ       PIC X(1).
           10 VERVSNRZUSATZ        PIC X(10).
           10 VERDATPRODLOESCH     PIC X(10).
           10 VEREHELEUTEKZ        PIC X(1).
           10 VERMANDANTNR         PIC S9(9) USAGE COMP.
           10 VERGESBEITRBKZ       PIC X(1).
           10 VERSTORNGRND-KTY     PIC S9(4) USAGE COMP.
           10 VERSTORNGRND-KNR     PIC S9(4) USAGE COMP.
           10 VERFIRMENVTGKZ       PIC X(1).
      ************************** ENDE-DCLGEN *************************
