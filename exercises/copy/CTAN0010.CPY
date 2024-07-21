      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0010.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAN0010.CPv  $
      *
      *   Rev 2.2   01 Dec 2006 16:05:02   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   16 Nov 2006 10:35:02   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   27 Oct 2006 13:32:04   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*

CCC005*    EXEC SQL DECLARE TBAN0010 TABLE
CCC004*    (
CCC004*       PGEB_GES                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       ANTS_PIN                       INT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTS_NR                        DECIMAL (15, 0)
CCC004*                                      NOT NULL,
CCC004*       ANTS_FREI                      CHAR (1)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTS_AVERM                     INT
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTS_LAST_UPDATE               TIMESTAMP
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT,
CCC004*       ANTS_KENNUNG                   CHAR (8)
CCC004*                                      NOT NULL
CCC004*                                      WITH DEFAULT
CCC004*    )
CCC003*    END-EXEC.


       01  DCL-TBAN0010.
           03 PGEB-GES                       PIC S9(4) COMP.
           03 ANTS-PIN                       PIC S9(9) COMP.
           03 ANTS-NR                        PIC S9(15)V COMP-3.
           03 ANTS-FREI                      PIC X.
           03 ANTS-AVERM                     PIC S9(9) COMP.
           03 ANTS-LAST-UPDATE               PIC X(26).
           03 ANTS-KENNUNG                   PIC X(8).
