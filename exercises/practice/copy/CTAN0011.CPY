      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTAN0011.CPv  $
      *
      *   Rev 2.1   10 Nov 2006 13:08:44   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   10 Nov 2006 08:19:50   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*

CCC005*    EXEC SQL DECLARE TBAN0011 TABLE
CCC004*    (
CCC004*       PGEB_GES                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PROD_PROD_SCHL                 SMALLINT
CCC004*                                      NOT NULL,
CCC004*       ANTQ_PLAUSI                    SMALLINT
CCC004*                                      NOT NULL,
CCC004*       ANTQ_GUELTIG_AB                DATE
CCC004*                                      NOT NULL,
CCC004*       ANTQ_GUELTIG_BIS               DATE
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTQ_WERT_NUM                  DECIMAL (12, 3)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTQ_WERT_ALPHANUM             CHAR (30)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTQ_LAST_UPDATE               TIMESTAMP
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTQ_KENNUNG                   CHAR (8)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT
CCC004*    )
CCC003*    END-EXEC.


       01  DCL-TBAN0011.
           03 PGEB-GES                       PIC S9(4) COMP.
           03 PROD-PROD-SCHL                 PIC S9(4) COMP.
           03 ANTQ-PLAUSI                    PIC S9(4) COMP.
           03 ANTQ-GUELTIG-AB                PIC X(10).
           03 ANTQ-GUELTIG-BIS               PIC X(10).
           03 ANTQ-WERT-NUM                  PIC S9(9)V9(3) COMP-3.
           03 ANTQ-WERT-ALPHANUM             PIC X(30).
           03 ANTQ-LAST-UPDATE               PIC X(26).
           03 ANTQ-KENNUNG                   PIC X(8).
