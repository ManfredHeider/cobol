      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23/02/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0490.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0490.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:39:06   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 23 2018 15:10:23   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE GEN_KUNDE TABLE
CCC004*    ( GENKGESELLSCHID                CHAR(2) NOT NULL,
CCC004*      GENKVNR                        CHAR(15) NOT NULL,
CCC004*      GENKEZPNR                      SMALLINT NOT NULL,
CCC004*      GENKROLLE                      CHAR(2) NOT NULL,
CCC004*      GENKLFDNR                      SMALLINT NOT NULL,
CCC004*      GENKVERARB_KZN                 CHAR(1) NOT NULL,
CCC004*      GENKNACHNAME                   CHAR(40) NOT NULL,
CCC004*      GENKNACHNAME2                  CHAR(40) NOT NULL,
CCC004*      GENKVORNAME                    CHAR(30) NOT NULL,
CCC004*      GENKGEBDAT                     CHAR(8) NOT NULL,
CCC004*      GENKANREDE                     CHAR(2) NOT NULL,
CCC004*      GENKGESCHLECHT                 CHAR(1) NOT NULL,
CCC004*      GENKSTRASSE                    CHAR(45) NOT NULL,
CCC004*      GENKPLZ                        CHAR(15) NOT NULL,
CCC004*      GENKORT                        CHAR(30) NOT NULL,
CCC004*      GENKAUSL_KZN                   CHAR(3) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GEN_KUNDE                   *
      ******************************************************************
       01  GEN-KUNDE-GESAMT.
        05 NI-GEN-KUNDE-TAB.
           10 NI-GEN-KUNDE-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-GEN-KUNDE REDEFINES NI-GEN-KUNDE-TAB.
           10 NI-GENKGESELLSCHID    PIC S9(4) COMP.
           10 NI-GENKVNR            PIC S9(4) COMP.
           10 NI-GENKEZPNR          PIC S9(4) COMP.
           10 NI-GENKROLLE          PIC S9(4) COMP.
           10 NI-GENKLFDNR          PIC S9(4) COMP.
           10 NI-GENKVERARB-KZN     PIC S9(4) COMP.
           10 NI-GENKNACHNAME       PIC S9(4) COMP.
           10 NI-GENKNACHNAME2      PIC S9(4) COMP.
           10 NI-GENKVORNAME        PIC S9(4) COMP.
           10 NI-GENKGEBDAT         PIC S9(4) COMP.
           10 NI-GENKANREDE         PIC S9(4) COMP.
           10 NI-GENKGESCHLECHT     PIC S9(4) COMP.
           10 NI-GENKSTRASSE        PIC S9(4) COMP.
           10 NI-GENKPLZ            PIC S9(4) COMP.
           10 NI-GENKORT            PIC S9(4) COMP.
           10 NI-GENKAUSL-KZN       PIC S9(4) COMP.
        05 DCL-GEN_KUNDE.
           10 GENKGESELLSCHID      PIC X(2).
           10 GENKVNR              PIC X(15).
           10 GENKEZPNR            PIC S9(4) USAGE COMP.
           10 GENKROLLE            PIC X(2).
           10 GENKLFDNR            PIC S9(4) USAGE COMP.
           10 GENKVERARB-KZN       PIC X(1).
           10 GENKNACHNAME         PIC X(40).
           10 GENKNACHNAME2        PIC X(40).
           10 GENKVORNAME          PIC X(30).
           10 GENKGEBDAT           PIC X(8).
           10 GENKANREDE           PIC X(2).
           10 GENKGESCHLECHT       PIC X(1).
           10 GENKSTRASSE          PIC X(45).
           10 GENKPLZ              PIC X(15).
           10 GENKORT              PIC X(30).
           10 GENKAUSL-KZN         PIC X(3).
      ************************** ENDE-DCLGEN *************************

