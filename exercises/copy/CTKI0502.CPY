      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25/03/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0502.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0502.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:13:04   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:48:56   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Mar 25 2015 15:19:41   T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE DVMFIRMA TABLE
CCC004*    ( DKZVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      DKZRECHTSFORM_KTY              SMALLINT NOT NULL,
CCC004*      DKZRECHTSFORM_KNR              SMALLINT NOT NULL,
CCC004*      DKZINHABER                     VARCHAR(100),
CCC004*      DKZPOSTFACH                    CHAR(10),
CCC004*      DKZPOSTFACHPLZ                 INTEGER,
CCC004*      DKZPOSTFACHORT                 CHAR(40),
CCC004*      DKZTELNR                       CHAR(30),
CCC004*      DKZMOBILNR                     CHAR(30),
CCC004*      DKZFAXNR                       CHAR(30),
CCC004*      DKZAPTITEL_KTY                 SMALLINT NOT NULL,
CCC004*      DKZAPTITEL_KNR                 SMALLINT,
CCC004*      DKZAPANREDE_KTY                SMALLINT NOT NULL,
CCC004*      DKZAPANREDE_KNR                SMALLINT,
CCC004*      DKZAPVORNAME                   CHAR(30),
CCC004*      DKZAPNACHNAME                  CHAR(30),
CCC004*      DKZAPGEBDAT                    DATE,
CCC004*      DKZAPEMAIL                     VARCHAR(128),
CCC004*      DKZAPFUNKTION                  VARCHAR(50),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      DKZERSTELLTTST                 TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DVMFIRMA                    *
      ******************************************************************
       01  DVMFIRMA-GESAMT.
        05 NI-DVMFIRMA-TAB.
           10 NI-DVMFIRMA-ELE PIC S9(04) COMP OCCURS 21 TIMES.
        05 NI-DVMFIRMA REDEFINES NI-DVMFIRMA-TAB.
           10 NI-DKZVERTRAGSID      PIC S9(4) COMP.
           10 NI-DKZRECHTSFORM-KTY  PIC S9(4) COMP.
           10 NI-DKZRECHTSFORM-KNR  PIC S9(4) COMP.
           10 NI-DKZINHABER         PIC S9(4) COMP.
           10 NI-DKZPOSTFACH        PIC S9(4) COMP.
           10 NI-DKZPOSTFACHPLZ     PIC S9(4) COMP.
           10 NI-DKZPOSTFACHORT     PIC S9(4) COMP.
           10 NI-DKZTELNR           PIC S9(4) COMP.
           10 NI-DKZMOBILNR         PIC S9(4) COMP.
           10 NI-DKZFAXNR           PIC S9(4) COMP.
           10 NI-DKZAPTITEL-KTY     PIC S9(4) COMP.
           10 NI-DKZAPTITEL-KNR     PIC S9(4) COMP.
           10 NI-DKZAPANREDE-KTY    PIC S9(4) COMP.
           10 NI-DKZAPANREDE-KNR    PIC S9(4) COMP.
           10 NI-DKZAPVORNAME       PIC S9(4) COMP.
           10 NI-DKZAPNACHNAME      PIC S9(4) COMP.
           10 NI-DKZAPGEBDAT        PIC S9(4) COMP.
           10 NI-DKZAPEMAIL         PIC S9(4) COMP.
           10 NI-DKZAPFUNKTION      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-DKZERSTELLTTST     PIC S9(4) COMP.
        05 DCL-DVMFIRMA.
           10 DKZVERTRAGSID        PIC X(13).
           10 DKZRECHTSFORM-KTY    PIC S9(4) USAGE COMP.
           10 DKZRECHTSFORM-KNR    PIC S9(4) USAGE COMP.
           10 DKZINHABER.
              49 DKZINHABER-LEN    PIC S9(4) USAGE COMP.
              49 DKZINHABER-TEXT   PIC X(100).
           10 DKZPOSTFACH          PIC X(10).
           10 DKZPOSTFACHPLZ       PIC S9(9) USAGE COMP.
           10 DKZPOSTFACHORT       PIC X(40).
           10 DKZTELNR             PIC X(30).
           10 DKZMOBILNR           PIC X(30).
           10 DKZFAXNR             PIC X(30).
           10 DKZAPTITEL-KTY       PIC S9(4) USAGE COMP.
           10 DKZAPTITEL-KNR       PIC S9(4) USAGE COMP.
           10 DKZAPANREDE-KTY      PIC S9(4) USAGE COMP.
           10 DKZAPANREDE-KNR      PIC S9(4) USAGE COMP.
           10 DKZAPVORNAME         PIC X(30).
           10 DKZAPNACHNAME        PIC X(30).
           10 DKZAPGEBDAT          PIC X(10).
           10 DKZAPEMAIL.
              49 DKZAPEMAIL-LEN    PIC S9(4) USAGE COMP.
              49 DKZAPEMAIL-TEXT   PIC X(128).
           10 DKZAPFUNKTION.
              49 DKZAPFUNKTION-LEN
                 PIC S9(4) USAGE COMP.
              49 DKZAPFUNKTION-TEXT
                 PIC X(50).
           10 LETZTEAENDERUNG      PIC X(26).
           10 DKZERSTELLTTST       PIC X(26).
      ************************** ENDE-DCLGEN *************************

