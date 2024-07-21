      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0026.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTPA0026.CPv  $
      *
      *   Rev 2.2   18 Jul 2006 08:21:40   t13ae46
      *APA00458: SF44 Textanzeige für PGV
      *
      *   Rev 2.1   Jan 18 2000 14:42:52   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:26:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   04 Jul 1997 08:01:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*

CCC005*    EXEC SQL DECLARE TBPA0026 TABLE
CCC004*    (
CCC004*       PGEB_GES                       SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PGVZ_PGV                       CHAR (8)
CCC004*                                      NOT NULL,
CCC004*       PGVZ_VZ_POSITIV                CHAR (1)
CCC004*                                      NOT NULL,
CCC004*       PGVZ_GUELTIG_AB                DATE
CCC004*                                      NOT NULL,
CCC004*       PGVZ_GUELTIG_BIS               DATE
CCC004*                                      NOT NULL,
CCC004*       KNR_PGV                        SMALLINT
CCC004*                                      NOT NULL,
CCC004*       PGVZ_LAST_UPDATE               TIMESTAMP
CCC004*                                      NOT NULL,
CCC004*       PGVZ_KENNUNG                   CHAR (8)
CCC004*                                      NOT NULL,
CCC004*       PGVZ_PGV_TEXT                  CHAR (42)
CCC004*                                      NOT NULL
CCC004*    )
CCC003*    END-EXEC.


       01  TBPA0026-GESAMT.
          05 DCL-TBPA0026.
           10 PGEB-GES                       PIC S9(4) COMP.
           10 PGVZ-PGV                       PIC X(8).
           10 PGVZ-VZ-POSITIV                PIC X.
           10 PGVZ-GUELTIG-AB                PIC X(10).
           10 PGVZ-GUELTIG-BIS               PIC X(10).
           10 KNR-PGV                        PIC S9(4) COMP.
           10 PGVZ-LAST-UPDATE               PIC X(26).
           10 PGVZ-KENNUNG                   PIC X(8).
280406     10 PGVZ-PGV-TEXT                  PIC X(42).

