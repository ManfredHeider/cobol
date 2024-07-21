      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0029.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\CVAT0029.CPv  $
      *
      *   Rev 2.2   Mar 31 2000 18:21:08   T13AE34
      *AAN00249-SF21 Änderung
      *
      *   Rev 2.0   Mar 16 1999 09:17:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0029 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      VERTRIEB_GES                   SMALLINT NOT NULL,
CCC004*      PGVG_GUELTIG_AB                TIMESTAMP NOT NULL,
CCC004*      PGVG_GUELTIG_BIS               DATE NOT NULL,
CCC004*      KTY_PGEB_GES                   SMALLINT NOT NULL,
CCC004*      KTY_VERTRIEB_GES               SMALLINT NOT NULL,
CCC004*      PGVG_KONTO                     CHAR(10) NOT NULL,
CCC004*      PGVG_EDV_GKTO_EIN              DECIMAL(3, 0) NOT NULL,
CCC004*      PGVG_EDV_GKTO_AUS              DECIMAL(3, 0) NOT NULL,
CCC004*      PGVG_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGVG_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13PA.VIAT0029                     *
      ******************************************************************
       01  VIAT0029-GESAMT.
           05  DCL-VIAT0029.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 VERTRIEB-GES         PIC S9(4) USAGE COMP.
           10 PGVG-GUELTIG-AB      PIC X(26).
           10 PGVG-GUELTIG-BIS     PIC X(10).
           10 KTY-PGEB-GES         PIC S9(4) USAGE COMP.
           10 KTY-VERTRIEB-GES     PIC S9(4) USAGE COMP.
           10 PGVG-KONTO           PIC X(10).
           10 PGVG-EDV-GKTO-EIN    PIC S9(3)V USAGE COMP-3.
           10 PGVG-EDV-GKTO-AUS    PIC S9(3)V USAGE COMP-3.
           10 PGVG-LAST-UPDATE     PIC X(26).
           10 PGVG-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
