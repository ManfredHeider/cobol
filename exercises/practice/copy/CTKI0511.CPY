      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12/09/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0511.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0511.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:43:22   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.2   Oct 27 2018 15:26:22   t13ex78
      *AKI01615: OZPs für G42_SPARPLAN
      *
      *   Rev 2.1   Sep 19 2018 11:48:36   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.0   Sep 12 2018 07:56:44   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G42_FSA TABLE
CCC004*    ( G42FVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G42FGUELTIGAB                  CHAR(8) NOT NULL,
CCC004*      G42FGUELTIGBIS                 CHAR(8) NOT NULL,
CCC004*      G42FGEMFBETRAG                 DECIMAL(7, 2) NOT NULL,
CCC004*      G42FFBETRVERBR                 DECIMAL(7, 2) NOT NULL,
CCC004*      G42FSTUECKZINS                 DECIMAL(12, 2) NOT NULL,
CCC004*      G42FKZSTAUSL                   CHAR(1) NOT NULL,
CCC004*      G42FNVGUELTIGBIS               CHAR(8) NOT NULL,
CCC004*      G42FVERBVORJ                   DECIMAL(7, 2) NOT NULL,
CCC004*      G42FSUMERTRAG                  DECIMAL(9, 2) NOT NULL,
CCC004*      G42FSUMKEST                    DECIMAL(7, 2) NOT NULL,
CCC004*      G42FSUMSOLI                    DECIMAL(7, 2) NOT NULL,
CCC004*      G42FSUMKIST                    DECIMAL(7, 2) NOT NULL,
CCC004*      G42FVERLUSTAKTI                DECIMAL(9, 2) NOT NULL,
CCC004*      G42FVERLUSTSON                 DECIMAL(9, 2) NOT NULL,
CCC004*      G42FAUSLQSTEUER                DECIMAL(9, 2) NOT NULL,
CCC004*      G42FKZVERBAKTIE                CHAR(1) NOT NULL,
CCC004*      G42FKZVERBSONST                CHAR(1) NOT NULL,
CCC004*      G42FKONFESSION1                CHAR(2) NOT NULL,
CCC004*      G42FKONFESSION2                CHAR(2) NOT NULL,
CCC004*      G42FKISTSATZ1                  DECIMAL(4, 2) NOT NULL,
CCC004*      G42FKISTSATZ2                  DECIMAL(4, 2) NOT NULL,
CCC004*      G42FKISTEBEHALT                CHAR(8) NOT NULL,
CCC004*      G42FAUFTKONF1                  DECIMAL(3, 0) NOT NULL,
CCC004*      G42FAUFTKONF2                  DECIMAL(3, 0) NOT NULL,
CCC004*      G42FFSAID                      CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G42_FSA                     *
      ******************************************************************
       01  G42-FSA-GESAMT.
        05 NI-G42-FSA-TAB.
           10 NI-G42-FSA-ELE PIC S9(04) COMP OCCURS 26 TIMES.
        05 NI-G42-FSA REDEFINES NI-G42-FSA-TAB.
           10 NI-G42FVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G42FGUELTIGAB      PIC S9(4) COMP.
           10 NI-G42FGUELTIGBIS     PIC S9(4) COMP.
           10 NI-G42FGEMFBETRAG     PIC S9(4) COMP.
           10 NI-G42FFBETRVERBR     PIC S9(4) COMP.
           10 NI-G42FSTUECKZINS     PIC S9(4) COMP.
           10 NI-G42FKZSTAUSL       PIC S9(4) COMP.
           10 NI-G42FNVGUELTIGBIS   PIC S9(4) COMP.
           10 NI-G42FVERBVORJ       PIC S9(4) COMP.
           10 NI-G42FSUMERTRAG      PIC S9(4) COMP.
           10 NI-G42FSUMKEST        PIC S9(4) COMP.
           10 NI-G42FSUMSOLI        PIC S9(4) COMP.
           10 NI-G42FSUMKIST        PIC S9(4) COMP.
           10 NI-G42FVERLUSTAKTI    PIC S9(4) COMP.
           10 NI-G42FVERLUSTSON     PIC S9(4) COMP.
           10 NI-G42FAUSLQSTEUER    PIC S9(4) COMP.
           10 NI-G42FKZVERBAKTIE    PIC S9(4) COMP.
           10 NI-G42FKZVERBSONST    PIC S9(4) COMP.
           10 NI-G42FKONFESSION1    PIC S9(4) COMP.
           10 NI-G42FKONFESSION2    PIC S9(4) COMP.
           10 NI-G42FKISTSATZ1      PIC S9(4) COMP.
           10 NI-G42FKISTSATZ2      PIC S9(4) COMP.
           10 NI-G42FKISTEBEHALT    PIC S9(4) COMP.
           10 NI-G42FAUFTKONF1      PIC S9(4) COMP.
           10 NI-G42FAUFTKONF2      PIC S9(4) COMP.
           10 NI-G42FFSAID          PIC S9(4) COMP.
        05 DCL-G42_FSA.
           10 G42FVERTRAGSNR       PIC X(15).
           10 G42FGUELTIGAB        PIC X(8).
           10 G42FGUELTIGBIS       PIC X(8).
           10 G42FGEMFBETRAG       PIC S9(5)V9(2) USAGE COMP-3.
           10 G42FFBETRVERBR       PIC S9(5)V9(2) USAGE COMP-3.
           10 G42FSTUECKZINS       PIC S9(10)V9(2) USAGE COMP-3.
           10 G42FKZSTAUSL         PIC X(1).
           10 G42FNVGUELTIGBIS     PIC X(8).
           10 G42FVERBVORJ         PIC S9(5)V9(2) USAGE COMP-3.
           10 G42FSUMERTRAG        PIC S9(7)V9(2) USAGE COMP-3.
           10 G42FSUMKEST          PIC S9(5)V9(2) USAGE COMP-3.
           10 G42FSUMSOLI          PIC S9(5)V9(2) USAGE COMP-3.
           10 G42FSUMKIST          PIC S9(5)V9(2) USAGE COMP-3.
           10 G42FVERLUSTAKTI      PIC S9(7)V9(2) USAGE COMP-3.
           10 G42FVERLUSTSON       PIC S9(7)V9(2) USAGE COMP-3.
           10 G42FAUSLQSTEUER      PIC S9(7)V9(2) USAGE COMP-3.
           10 G42FKZVERBAKTIE      PIC X(1).
           10 G42FKZVERBSONST      PIC X(1).
           10 G42FKONFESSION1      PIC X(2).
           10 G42FKONFESSION2      PIC X(2).
           10 G42FKISTSATZ1        PIC S9(2)V9(2) USAGE COMP-3.
           10 G42FKISTSATZ2        PIC S9(2)V9(2) USAGE COMP-3.
           10 G42FKISTEBEHALT      PIC X(8).
           10 G42FAUFTKONF1        PIC S9(3)V USAGE COMP-3.
           10 G42FAUFTKONF2        PIC S9(3)V USAGE COMP-3.
           10 G42FFSAID            PIC X(8).
      ************************** ENDE-DCLGEN *************************
 
 

