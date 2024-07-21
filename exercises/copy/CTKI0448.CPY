      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26/06/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0448.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0448.CPv  $
      *
      *   Rev 2.2   Sep 03 2012 10:16:32   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.1   Aug 15 2012 12:51:30   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.0   Jun 26 2012 11:13:51   T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE VBUNGUELTIGAV TABLE
CCC004*    ( VBUVERTRAGSNR                  CHAR(15) NOT NULL,
CCC004*      VBULFDNR                       SMALLINT NOT NULL,
CCC004*      VBUVBNRGELIEF                  INTEGER,
CCC004*      VBUVRBVBNR                     INTEGER,
CCC004*      VBUPARTGESID                   SMALLINT NOT NULL,
CCC004*      VBUMANDANTNR                   INTEGER NOT NULL,
CCC004*      VBUABLEHNKZ                    CHAR(1),
CCC004*      VBUSTRUKTURSTUFE               INTEGER,
CCC004*      VBUAUSTRITTSDAT                DATE,
CCC004*      VBUAKTDAT                      DATE NOT NULL,
CCC004*      VBUAUSWABLEHNKZ                CHAR(1),
CCC004*      VBUAUSWSTRUKTSTUFE             INTEGER,
CCC004*      VBUAUSWAUSTRTTDAT              DATE,
CCC004*      VBUAUSWDAT                     DATE,
CCC004*      VBUSTATUS                      CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VBUNGUELTIGAV               *
      ******************************************************************
       01  VBUNGUELTIGAV-GESAMT.
        05 NI-VBUNGUELTIGAV-TAB.
           10 NI-VBUNGUELTIGAV-ELE PIC S9(04) COMP OCCURS 16 TIMES.
        05 NI-VBUNGUELTIGAV REDEFINES NI-VBUNGUELTIGAV-TAB.
           10 NI-VBUVERTRAGSNR      PIC S9(4) COMP.
           10 NI-VBULFDNR           PIC S9(4) COMP.
           10 NI-VBUVBNRGELIEF      PIC S9(4) COMP.
           10 NI-VBUVRBVBNR         PIC S9(4) COMP.
           10 NI-VBUPARTGESID       PIC S9(4) COMP.
           10 NI-VBUMANDANTNR       PIC S9(4) COMP.
           10 NI-VBUABLEHNKZ        PIC S9(4) COMP.
           10 NI-VBUSTRUKTURSTUFE   PIC S9(4) COMP.
           10 NI-VBUAUSTRITTSDAT    PIC S9(4) COMP.
           10 NI-VBUAKTDAT          PIC S9(4) COMP.
           10 NI-VBUAUSWABLEHNKZ    PIC S9(4) COMP.
           10 NI-VBUAUSWSTRUKTSTUFE PIC S9(4) COMP.
           10 NI-VBUAUSWAUSTRTTDAT  PIC S9(4) COMP.
           10 NI-VBUAUSWDAT         PIC S9(4) COMP.
           10 NI-VBUSTATUS          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-VBUNGUELTIGAV.
           10 VBUVERTRAGSNR        PIC X(15).
           10 VBULFDNR             PIC S9(4) USAGE COMP.
           10 VBUVBNRGELIEF        PIC S9(9) USAGE COMP.
           10 VBUVRBVBNR           PIC S9(9) USAGE COMP.
           10 VBUPARTGESID         PIC S9(4) USAGE COMP.
           10 VBUMANDANTNR         PIC S9(9) USAGE COMP.
           10 VBUABLEHNKZ          PIC X(1).
           10 VBUSTRUKTURSTUFE     PIC S9(9) USAGE COMP.
           10 VBUAUSTRITTSDAT      PIC X(10).
           10 VBUAKTDAT            PIC X(10).
           10 VBUAUSWABLEHNKZ      PIC X(1).
           10 VBUAUSWSTRUKTSTUFE   PIC S9(9) USAGE COMP.
           10 VBUAUSWAUSTRTTDAT    PIC X(10).
           10 VBUAUSWDAT           PIC X(10).
           10 VBUSTATUS            PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************
