      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctpa0002.CPv  $
      *
      *   Rev 2.1   20 Sep 2000 13:34:16   T13AE34
      *AAN00264-Umbenennen PEWP-ERHG-BEITRAG
      *in PEWP-BEWERT-BETRAG
      *
      *   Rev 2.0   Feb 27 1999 14:30:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   06 Aug 1998 12:57:42   T01JM2Y
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.2   Mar 06 1997 11:09:00   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:37:12   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:25:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0002                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0002                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(TBPA0002)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0002 TABLE
CCC004*    ( PEWA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PEWP_TARIF                     CHAR(12) NOT NULL,
CCC004*      PEWP_LAUFZEIT                  SMALLINT NOT NULL,
CCC004*      PEWP_ABLAUF                    DATE NOT NULL,
CCC004*      PRKO_KOND_NR_GSP               SMALLINT NOT NULL,
CCC004*      PRKO_KOND_NR_GRP               SMALLINT NOT NULL,
CCC004*      PRKO_KOND_NR_EINH              SMALLINT NOT NULL,
CCC004*      PEWP_BEITRAG                   DECIMAL(11, 2) NOT NULL,
CCC004*      PEWP_SUMME                     DECIMAL(11, 2) NOT NULL,
CCC004*      PEWP_AVERM                     INTEGER NOT NULL,
CCC004*      PEWP_BETR                      DECIMAL(11, 2) NOT NULL,
CCC004*      PEWP_BETR_AVERM                DECIMAL(11, 2) NOT NULL,
CCC004*      PEWP_EINH                      DECIMAL(8, 2) NOT NULL,
CCC004*      PEWP_EINH_AVERM                DECIMAL(8, 2) NOT NULL,
CCC004*      PEWP_RMELD_DAT                 DATE NOT NULL,
CCC004*      PEWP_BEWERT_BETRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      PEWP_BTO_BEITRAG               DECIMAL(11, 2) NOT NULL,
CCC004*      PEWP_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PEWP_VSTAT_UNR                 SMALLINT NOT NULL,
CCC004*      PEWP_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PEWP_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0002                           *
      ******************************************************************
       01  TBPA0002-GESAMT.
         05  DCL-TBPA0002.
           10 PEWA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PEWP-TARIF           PIC X(12).
           10 PEWP-LAUFZEIT        PIC S9(4) USAGE COMP.
           10 PEWP-ABLAUF          PIC X(10).
           10 PRKO-KOND-NR-GSP     PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-GRP     PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-EINH    PIC S9(4) USAGE COMP.
           10 PEWP-BEITRAG         PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-SUMME           PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-AVERM           PIC S9(9) USAGE COMP.
           10 PEWP-BETR            PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-BETR-AVERM      PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-EINH            PIC S9(6)V9(2) USAGE COMP-3.
           10 PEWP-EINH-AVERM      PIC S9(6)V9(2) USAGE COMP-3.
           10 PEWP-RMELD-DAT       PIC X(10).
           10 PEWP-BEWERT-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PEWP-BTO-BEITRAG     PIC S9(9)V9(2) USAGE COMP-3.
           10 PEWP-VSTAT           PIC X(4).
           10 PEWP-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PEWP-LAST-UPDATE     PIC X(26).
           10 PEWP-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************
