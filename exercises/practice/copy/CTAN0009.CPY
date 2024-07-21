      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0009.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTAN0009.CPv  $
      *
      *   Rev 2.4   15 Nov 2006 09:51:46   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   14 Nov 2006 13:52:46   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Sep 26 2006 11:51:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 11 2006 13:57:00   t13ae34
      *FAN00667: CTAN0008 CTAN0009
      *
      *   Rev 2.0   Sep 08 2006 16:13:34   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*

CCC005*    EXEC SQL DECLARE TBAN0009 TABLE
CCC004*    (
CCC004*       PGEB_GES                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       ANTP_NR                        DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       PROD_PROD_SCHL                 SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_LFD_NR                    SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_TARIF                     CHAR (15)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_BETR                      DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_SUMME                     DECIMAL (11, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       KNR_WAEHRUNG                   SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_ZAHLWEISE                 SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_LAUFZEIT                  SMALLINT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_DYNAMIK                   DECIMAL (6, 3)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_ZUSATZ_1                  DECIMAL (12, 3)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_ZUSATZ_2                  DECIMAL (12, 3)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_EINH_AVERM                DECIMAL (8, 2)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_LAST_UPDATE               TIMESTAMP
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTP_KENNUNG                   CHAR (8)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT
CCC004*    )
CCC003*    END-EXEC.


       01  DCL-TBAN0009.
           03 PGEB-GES                       PIC S9(4) COMP.
           03 ANTP-NR                        PIC S9(15)V COMP-3.
           03 PROD-PROD-SCHL                 PIC S9(4) COMP.
           03 ANTP-LFD-NR                    PIC S9(4) COMP.
           03 ANTP-TARIF                     PIC X(15).
           03 ANTP-BETR                      PIC S9(9)V9(2) COMP-3.
           03 ANTP-SUMME                     PIC S9(9)V9(2) COMP-3.
           03 KNR-WAEHRUNG                   PIC S9(4) COMP.
           03 ANTP-ZAHLWEISE                 PIC S9(4) COMP.
           03 ANTP-LAUFZEIT                  PIC S9(4) COMP.
           03 ANTP-DYNAMIK                   PIC S9(3)V9(3) COMP-3.
           03 ANTP-ZUSATZ-1                  PIC S9(9)V9(3) COMP-3.
           03 ANTP-ZUSATZ-2                  PIC S9(9)V9(3) COMP-3.
           03 ANTP-EINH-AVERM                PIC S9(6)V9(2) COMP-3.
           03 ANTP-LAST-UPDATE               PIC X(26).
           03 ANTP-KENNUNG                   PIC X(8).
