      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21/12/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0548.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0548.CPv  $
      *
      *   Rev 2.2   Jan 11 2021 19:01:26   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.1   Jan 06 2021 10:17:54   u130685
      *AKI01821: DVAG Wien Sparplan
      *
      *   Rev 2.0   Dec 21 2020 16:45:48   U130685
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G33_SPARPLAN TABLE
CCC004*    ( G33VNR                         CHAR(15) NOT NULL,
CCC004*      G33LFDNR                       SMALLINT NOT NULL,
CCC004*      G33SPARPLANANF                 CHAR(8) NOT NULL,
CCC004*      G33SPARPLANENDE                CHAR(8) NOT NULL,
CCC004*      G33ABWEICHKTOINH               CHAR(30) NOT NULL,
CCC004*      G33WKN                         CHAR(6) NOT NULL,
CCC004*      G33ISIN                        CHAR(12) NOT NULL,
CCC004*      G33SPARRATE                    DECIMAL(9, 2) NOT NULL,
CCC004*      G33PROZENTDYNAMIS              DECIMAL(4, 2) NOT NULL,
CCC004*      G33ZAHLWEISE                   INTEGER NOT NULL,
CCC004*      G33TAGEINZAHLUNG               SMALLINT NOT NULL,
CCC004*      G33SPARPLANENDE2               CHAR(8) NOT NULL,
CCC004*      G33VERTRAGSTYP                 CHAR(2) NOT NULL,
CCC004*      G33BEGINNRATENAUS              CHAR(8) NOT NULL,
CCC004*      G33ENDERATENAUS                CHAR(8) NOT NULL,
CCC004*      G33KOSTENPROZENT               DECIMAL(3, 2) NOT NULL,
CCC004*      G33URGESAMTEURO                DECIMAL(8, 2) NOT NULL,
CCC004*      G33URSPARRATE                  DECIMAL(6, 2) NOT NULL,
CCC004*      G33AUSGABEURFOND               DECIMAL(4, 2) NOT NULL,
CCC004*      G33AUSGABEAKTFOND              DECIMAL(4, 2) NOT NULL,
CCC004*      G33VERTRAGLFZ                  INTEGER NOT NULL,
CCC004*      G33BEGINNDYN                   CHAR(8) NOT NULL,
CCC004*      G33ZLASTRUECK                  INTEGER NOT NULL,
CCC004*      G33KUENDMERKMAL                CHAR(1) NOT NULL,
CCC004*      G33DATVERTRAGKUEND             INTEGER NOT NULL,
CCC004*      G33BONI                        DECIMAL(7, 4) NOT NULL,
CCC004*      G33IBAN                        CHAR(34) NOT NULL,
CCC004*      G33BIC                         CHAR(11) NOT NULL,
CCC004*      G33MANDAT                      CHAR(35) NOT NULL,
CCC004*      G33ABRKTO                      CHAR(20) NOT NULL,
CCC004*      G33PIN                         INTEGER NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G33_SPARPLAN                *
      ******************************************************************
       01  G33-SPARPLAN-GESAMT.
        05 NI-G33-SPARPLAN-TAB.
           10 NI-G33-SPARPLAN-ELE PIC S9(04) COMP OCCURS 32 TIMES.
        05 NI-G33-SPARPLAN REDEFINES NI-G33-SPARPLAN-TAB.
           10 NI-G33VNR             PIC S9(4) COMP.
           10 NI-G33LFDNR           PIC S9(4) COMP.
           10 NI-G33SPARPLANANF     PIC S9(4) COMP.
           10 NI-G33SPARPLANENDE    PIC S9(4) COMP.
           10 NI-G33ABWEICHKTOINH   PIC S9(4) COMP.
           10 NI-G33WKN             PIC S9(4) COMP.
           10 NI-G33ISIN            PIC S9(4) COMP.
           10 NI-G33SPARRATE        PIC S9(4) COMP.
           10 NI-G33PROZENTDYNAMIS  PIC S9(4) COMP.
           10 NI-G33ZAHLWEISE       PIC S9(4) COMP.
           10 NI-G33TAGEINZAHLUNG   PIC S9(4) COMP.
           10 NI-G33SPARPLANENDE2   PIC S9(4) COMP.
           10 NI-G33VERTRAGSTYP     PIC S9(4) COMP.
           10 NI-G33BEGINNRATENAUS  PIC S9(4) COMP.
           10 NI-G33ENDERATENAUS    PIC S9(4) COMP.
           10 NI-G33KOSTENPROZENT   PIC S9(4) COMP.
           10 NI-G33URGESAMTEURO    PIC S9(4) COMP.
           10 NI-G33URSPARRATE      PIC S9(4) COMP.
           10 NI-G33AUSGABEURFOND   PIC S9(4) COMP.
           10 NI-G33AUSGABEAKTFOND  PIC S9(4) COMP.
           10 NI-G33VERTRAGLFZ      PIC S9(4) COMP.
           10 NI-G33BEGINNDYN       PIC S9(4) COMP.
           10 NI-G33ZLASTRUECK      PIC S9(4) COMP.
           10 NI-G33KUENDMERKMAL    PIC S9(4) COMP.
           10 NI-G33DATVERTRAGKUEND PIC S9(4) COMP.
           10 NI-G33BONI            PIC S9(4) COMP.
           10 NI-G33IBAN            PIC S9(4) COMP.
           10 NI-G33BIC             PIC S9(4) COMP.
           10 NI-G33MANDAT          PIC S9(4) COMP.
           10 NI-G33ABRKTO          PIC S9(4) COMP.
           10 NI-G33PIN             PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-G33-SPARPLAN.
           10 G33VNR               PIC X(15).
           10 G33LFDNR             PIC S9(4) USAGE COMP.
           10 G33SPARPLANANF       PIC X(8).
           10 G33SPARPLANENDE      PIC X(8).
           10 G33ABWEICHKTOINH     PIC X(30).
           10 G33WKN               PIC X(6).
           10 G33ISIN              PIC X(12).
           10 G33SPARRATE          PIC S9(7)V9(2) USAGE COMP-3.
           10 G33PROZENTDYNAMIS    PIC S9(2)V9(2) USAGE COMP-3.
           10 G33ZAHLWEISE         PIC S9(9) USAGE COMP.
           10 G33TAGEINZAHLUNG     PIC S9(4) USAGE COMP.
           10 G33SPARPLANENDE2     PIC X(8).
           10 G33VERTRAGSTYP       PIC X(2).
           10 G33BEGINNRATENAUS    PIC X(8).
           10 G33ENDERATENAUS      PIC X(8).
           10 G33KOSTENPROZENT     PIC S9(1)V9(2) USAGE COMP-3.
           10 G33URGESAMTEURO      PIC S9(6)V9(2) USAGE COMP-3.
           10 G33URSPARRATE        PIC S9(4)V9(2) USAGE COMP-3.
           10 G33AUSGABEURFOND     PIC S9(2)V9(2) USAGE COMP-3.
           10 G33AUSGABEAKTFOND    PIC S9(2)V9(2) USAGE COMP-3.
           10 G33VERTRAGLFZ        PIC S9(9) USAGE COMP.
           10 G33BEGINNDYN         PIC X(8).
           10 G33ZLASTRUECK        PIC S9(9) USAGE COMP.
           10 G33KUENDMERKMAL      PIC X(1).
           10 G33DATVERTRAGKUEND   PIC S9(9) USAGE COMP.
           10 G33BONI              PIC S9(3)V9(4) USAGE COMP-3.
           10 G33IBAN              PIC X(34).
           10 G33BIC               PIC X(11).
           10 G33MANDAT            PIC X(35).
           10 G33ABRKTO            PIC X(20).
           10 G33PIN               PIC S9(9) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

