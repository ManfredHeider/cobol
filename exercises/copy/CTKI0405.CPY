      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0405.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0405.CPv  $
      *
      *   Rev 2.4   Jun 28 2011 09:55:52   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.3   Sep 21 2009 14:33:48   t13ex27
      *AKI00876: DB-Änderungen für Rel. 4.62-FSAEIGENVERTRAEGE
      *
      *   Rev 2.2   Apr 25 2006 12:17:22   t13ex27
      *AKI00555: DB-Änderung OKI 02/2006
      *
      *   Rev 2.1   Feb 07 2006 15:42:26   t13ex27
      *AKI00555: DB-Änderung OKI 02/2006
      *
      *   Rev 2.0   Feb 07 2006 12:47:52   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE FSAEIGENVERTRAEGE
      ******************************************************************
CCC005*    EXEC SQL DECLARE FSAEIGENVERTRAEGE TABLE
CCC004*    ( FSAVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      FSAPERSONENID                  CHAR(13) NOT NULL,
CCC004*      FSAPARTNERGESID                SMALLINT NOT NULL,
CCC004*      FSAGEMFREIBETRAG               DECIMAL(11, 2),
CCC004*      FSAFREIBETRAGVERBR             DECIMAL(11, 2),
CCC004*      FSAWAEHRUNGSKZ                 CHAR(1) NOT NULL,
CCC004*      FSAANZDEPFREISTBET             SMALLINT,
CCC004*      FSAZINSABSCHSTEUER             DECIMAL(11, 2),
CCC004*      FSAFREIBETRAGABL               DATE,
CCC004*      FSASTUECKZINS                  DECIMAL(11, 2),
CCC004*      FSAGUELTIGAB                   DATE,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      FSAKZSTAUSL                    CHAR(1),
CCC004*      FSANVGUELTIGBIS                DATE,
CCC004*      FSAVERBRFREIVORJ               DECIMAL(11, 2),
CCC004*      FSASUMERTRAG                   DECIMAL(11, 2),
CCC004*      FSASUMKEST                     DECIMAL(11, 2),
CCC004*      FSASUMSOLI                     DECIMAL(11, 2),
CCC004*      FSASUMKIST                     DECIMAL(11, 2),
CCC004*      FSAVERLUSTAKTIEN               DECIMAL(11, 2),
CCC004*      FSAVERLUSTSONST                DECIMAL(11, 2),
CCC004*      FSAAUSLQSTEUER                 DECIMAL(11, 2),
CCC004*      FSAKZVERBAKTIEN                CHAR(1),
CCC004*      FSAKZVERBSONST                 CHAR(1),
CCC004*      FSAKONFESSION1                 CHAR(2),
CCC004*      FSAKONFESSION2                 CHAR(2),
CCC004*      FSAKISTSATZ1                   DECIMAL(5, 2),
CCC004*      FSAKISTSATZ2                   DECIMAL(5, 2),
CCC004*      FSAKISTEINBEHALT               DATE,
CCC004*      FSAAUFTKONF1                   DECIMAL(5, 2),
CCC004*      FSAAUFTKONF2                   DECIMAL(5, 2),
CCC004*      FSAFREISTAUFTRNR               CHAR(30)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE FSAEIGENVERTRAEGE           *
      ******************************************************************
       01  FSAEIGENVERTRAEGE-GESAMT.
        05 NI-FSAEIGENVERTRAEGE-TAB.
           10 NI-FSAEIGENVERTRAEGE-ELE PIC S9(04) COMP OCCURS 32 TIMES.
        05 NI-FSAEIGENVERTRAEGE REDEFINES NI-FSAEIGENVERTRAEGE-TAB.
           10 NI-FSAVERTRAGSID      PIC S9(4) COMP.
           10 NI-FSAPERSONENID      PIC S9(4) COMP.
           10 NI-FSAPARTNERGESID    PIC S9(4) COMP.
           10 NI-FSAGEMFREIBETRAG   PIC S9(4) COMP.
           10 NI-FSAFREIBETRAGVERBR PIC S9(4) COMP.
           10 NI-FSAWAEHRUNGSKZ     PIC S9(4) COMP.
           10 NI-FSAANZDEPFREISTBET PIC S9(4) COMP.
           10 NI-FSAZINSABSCHSTEUER PIC S9(4) COMP.
           10 NI-FSAFREIBETRAGABL   PIC S9(4) COMP.
           10 NI-FSASTUECKZINS      PIC S9(4) COMP.
           10 NI-FSAGUELTIGAB       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-FSAKZSTAUSL        PIC S9(4) COMP.
           10 NI-FSANVGUELTIGBIS    PIC S9(4) COMP.
           10 NI-FSAVERBRFREIVORJ   PIC S9(4) COMP.
           10 NI-FSASUMERTRAG       PIC S9(4) COMP.
           10 NI-FSASUMKEST         PIC S9(4) COMP.
           10 NI-FSASUMSOLI         PIC S9(4) COMP.
           10 NI-FSASUMKIST         PIC S9(4) COMP.
           10 NI-FSAVERLUSTAKTIEN   PIC S9(4) COMP.
           10 NI-FSAVERLUSTSONST    PIC S9(4) COMP.
           10 NI-FSAAUSLQSTEUER     PIC S9(4) COMP.
           10 NI-FSAKZVERBAKTIEN    PIC S9(4) COMP.
           10 NI-FSAKZVERBSONST     PIC S9(4) COMP.
           10 NI-FSAKONFESSION1     PIC S9(4) COMP.
           10 NI-FSAKONFESSION2     PIC S9(4) COMP.
           10 NI-FSAKISTSATZ1       PIC S9(4) COMP.
           10 NI-FSAKISTSATZ2       PIC S9(4) COMP.
           10 NI-FSAKISTEINBEHALT   PIC S9(4) COMP.
           10 NI-FSAAUFTKONF1       PIC S9(4) COMP.
           10 NI-FSAAUFTKONF2       PIC S9(4) COMP.
           10 NI-FSAFREISTAUFTRNR   PIC S9(4) COMP.
        05 DCL-FSAEIGENVERTRAEGE.
           10 FSAVERTRAGSID        PIC X(13).
           10 FSAPERSONENID        PIC X(13).
           10 FSAPARTNERGESID      PIC S9(4) USAGE COMP.
           10 FSAGEMFREIBETRAG     PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAFREIBETRAGVERBR   PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAWAEHRUNGSKZ       PIC X(1).
           10 FSAANZDEPFREISTBET   PIC S9(4) USAGE COMP.
           10 FSAZINSABSCHSTEUER   PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAFREIBETRAGABL     PIC X(10).
           10 FSASTUECKZINS        PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAGUELTIGAB         PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
           10 FSAKZSTAUSL          PIC X(1).
           10 FSANVGUELTIGBIS      PIC X(10).
           10 FSAVERBRFREIVORJ     PIC S9(9)V9(2) USAGE COMP-3.
           10 FSASUMERTRAG         PIC S9(9)V9(2) USAGE COMP-3.
           10 FSASUMKEST           PIC S9(9)V9(2) USAGE COMP-3.
           10 FSASUMSOLI           PIC S9(9)V9(2) USAGE COMP-3.
           10 FSASUMKIST           PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAVERLUSTAKTIEN     PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAVERLUSTSONST      PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAAUSLQSTEUER       PIC S9(9)V9(2) USAGE COMP-3.
           10 FSAKZVERBAKTIEN      PIC X(1).
           10 FSAKZVERBSONST       PIC X(1).
           10 FSAKONFESSION1       PIC X(2).
           10 FSAKONFESSION2       PIC X(2).
           10 FSAKISTSATZ1         PIC S9(3)V9(2) USAGE COMP-3.
           10 FSAKISTSATZ2         PIC S9(3)V9(2) USAGE COMP-3.
           10 FSAKISTEINBEHALT     PIC X(10).
           10 FSAAUFTKONF1         PIC S9(3)V9(2) USAGE COMP-3.
           10 FSAAUFTKONF2         PIC S9(3)V9(2) USAGE COMP-3.
           10 FSAFREISTAUFTRNR     PIC X(30).
      ************************** ENDE-DCLGEN *************************
