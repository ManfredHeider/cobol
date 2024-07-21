      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0018.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTAN0018.CPv  $
      *
      *   Rev 2.4   Oct 24 2018 10:06:44   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.3   Oct 23 2018 08:45:28   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.2   Oct 22 2018 16:00:24   t13ae34
      *AAN01393: Auf dem Host nicht verarbeitbaren Anträge sollen a
      *uf eine Fehlerliste geschrieben werden
      *
      *   Rev 2.1   Nov 26 2009 14:34:14   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.0   Nov 06 2009 08:15:46   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*     EXEC SQL DECLARE TBAN0018 TABLE
CCC004*     ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*       ANVH_HVANTNUM                  CHAR(17) NOT NULL,
CCC004*       ANVH_HINWEIS                   CHAR(79) NOT NULL,
CCC004*       ANVH_KENNUNG                   CHAR(8) NOT NULL,
CCC004*       ANVH_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*       ANVH_ANTA_NR                   DECIMAL(15, 0) NOT NULL,
CCC004*       ANVH_EA_DATEINAME              CHAR(50) NOT NULL,
CCC004*       ANVH_ERSTELLZEITPUNKT          TIMESTAMP NOT NULL,
CCC004*       ANVH_GEDRUCKT                  CHAR(1) NOT NULL,
CCC004*       ANVH_ANTA_AVERM                INTEGER NOT NULL
CCC003*     ) END-EXEC.
        01  DCL-TBAN0018.
            10 PGEB-GES             PIC S9(4) USAGE COMP.
            10 ANVH-HVANTNUM        PIC X(17).
            10 ANVH-HINWEIS         PIC X(79).
            10 ANVH-KENNUNG         PIC X(8).
            10 ANVH-LAST-UPDATE     PIC X(26).
            10 ANVH-ANTA-NR         PIC S9(15)V USAGE COMP-3.
            10 ANVH-EA-DATEINAME    PIC X(50).
            10 ANVH-ERSTELLZEITPUNKT
               PIC X(26).
            10 ANVH-GEDRUCKT        PIC X(1).
            10 ANVH-ANTA-AVERM      PIC S9(9) USAGE COMP.

