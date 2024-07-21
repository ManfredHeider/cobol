      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02/09/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0547.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0547.CPv  $
      *
      *   Rev 2.1   Nov 23 2020 17:17:22   t13ex39
      *AKI01790: Deutsche Bank - Depots KSDS--->TT
      *B E S T A N D
      *
      *   Rev 2.0   Sep 2 2020 14:47:53   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_DEPOTBESTAND TABLE
CCC004*    ( G48DBVNR                       CHAR(15) NOT NULL,
CCC004*      G48DBUNTERNR                   CHAR(2) NOT NULL,
CCC004*      G48DBWAEHRUNGSKZ               CHAR(3) NOT NULL,
CCC004*      G48DBWPKENNNR                  CHAR(6) NOT NULL,
CCC004*      G48DBISIN                      CHAR(12) NOT NULL,
CCC004*      G48DBART                       CHAR(1) NOT NULL,
CCC004*      G48DBSTUEOWERT                 DECIMAL(16, 4) NOT NULL,
CCC004*      G48DBORGWKZ                    CHAR(3) NOT NULL,
CCC004*      G48DBKURSWERTEUR               DECIMAL(12, 2) NOT NULL,
CCC004*      G48DBKURSWERTORG               DECIMAL(14, 2) NOT NULL,
CCC004*      G48DBKUMEIN                    DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBKUMAUS                    DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBDELKZ                     CHAR(1) NOT NULL,
CCC004*      G48DBKUMAUTOWA                 DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBKAPITALAKT                DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBKAPITALVORJ               DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBKUMUEGES                  DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBKUMUEVOR                  DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBEINSTKURS                 DECIMAL(7, 2) NOT NULL,
CCC004*      G48DBQUALIFLAG                 CHAR(1) NOT NULL,
CCC004*      G48DBREALGEWVERL               DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBKUMBABR                   DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBANTEILEFREI               DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBVLSPERRDAT1               CHAR(8) NOT NULL,
CCC004*      G48DBANGESPERRT1               DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBVLSPERRDAT2               CHAR(8) NOT NULL,
CCC004*      G48DBANGESPERRT2               DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBVLSPERRDAT3               CHAR(8) NOT NULL,
CCC004*      G48DBANGESPERRT3               DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBANTLGEKOIF                DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBKUMAUSHALBJ               DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBRUEBRUGES                 DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBRUEBRUVORJ                DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBKUBAAUBRVJ                DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBANTLVEFOIF                DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBANTGESPOIF                DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBANTVORJ13                 DECIMAL(11, 4) NOT NULL,
CCC004*      G48DBEREIKONHALB               DECIMAL(8, 2) NOT NULL,
CCC004*      G48DBANLIDOIF                  CHAR(14) NOT NULL,
CCC004*      G48DBWPBEZ                     CHAR(50) NOT NULL,
CCC004*      G48DBRUECKPR                   DECIMAL(9, 4) NOT NULL,
CCC004*      G48DBNRGWINKBA                 DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBNRGWINKBAVH               DECIMAL(8, 2) NOT NULL,
CCC004*      G48DBNRGWOBA                   DECIMAL(13, 2) NOT NULL,
CCC004*      G48DBNRGWOBAVH                 DECIMAL(8, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_DEPOTBESTAND            *
      ******************************************************************
       01  G48-DEPOTBESTAND-GESAMT.
        05 NI-G48-DEPOTBESTAND-TAB.
           10 NI-G48-DEPOTBESTAND-ELE PIC S9(04) COMP OCCURS 45 TIMES.
        05 NI-G48-DEPOTBESTAND REDEFINES NI-G48-DEPOTBESTAND-TAB.
           10 NI-G48DBVNR           PIC S9(4) COMP.
           10 NI-G48DBUNTERNR       PIC S9(4) COMP.
           10 NI-G48DBWAEHRUNGSKZ   PIC S9(4) COMP.
           10 NI-G48DBWPKENNNR      PIC S9(4) COMP.
           10 NI-G48DBISIN          PIC S9(4) COMP.
           10 NI-G48DBART           PIC S9(4) COMP.
           10 NI-G48DBSTUEOWERT     PIC S9(4) COMP.
           10 NI-G48DBORGWKZ        PIC S9(4) COMP.
           10 NI-G48DBKURSWERTEUR   PIC S9(4) COMP.
           10 NI-G48DBKURSWERTORG   PIC S9(4) COMP.
           10 NI-G48DBKUMEIN        PIC S9(4) COMP.
           10 NI-G48DBKUMAUS        PIC S9(4) COMP.
           10 NI-G48DBDELKZ         PIC S9(4) COMP.
           10 NI-G48DBKUMAUTOWA     PIC S9(4) COMP.
           10 NI-G48DBKAPITALAKT    PIC S9(4) COMP.
           10 NI-G48DBKAPITALVORJ   PIC S9(4) COMP.
           10 NI-G48DBKUMUEGES      PIC S9(4) COMP.
           10 NI-G48DBKUMUEVOR      PIC S9(4) COMP.
           10 NI-G48DBEINSTKURS     PIC S9(4) COMP.
           10 NI-G48DBQUALIFLAG     PIC S9(4) COMP.
           10 NI-G48DBREALGEWVERL   PIC S9(4) COMP.
           10 NI-G48DBKUMBABR       PIC S9(4) COMP.
           10 NI-G48DBANTEILEFREI   PIC S9(4) COMP.
           10 NI-G48DBVLSPERRDAT1   PIC S9(4) COMP.
           10 NI-G48DBANGESPERRT1   PIC S9(4) COMP.
           10 NI-G48DBVLSPERRDAT2   PIC S9(4) COMP.
           10 NI-G48DBANGESPERRT2   PIC S9(4) COMP.
           10 NI-G48DBVLSPERRDAT3   PIC S9(4) COMP.
           10 NI-G48DBANGESPERRT3   PIC S9(4) COMP.
           10 NI-G48DBANTLGEKOIF    PIC S9(4) COMP.
           10 NI-G48DBKUMAUSHALBJ   PIC S9(4) COMP.
           10 NI-G48DBRUEBRUGES     PIC S9(4) COMP.
           10 NI-G48DBRUEBRUVORJ    PIC S9(4) COMP.
           10 NI-G48DBKUBAAUBRVJ    PIC S9(4) COMP.
           10 NI-G48DBANTLVEFOIF    PIC S9(4) COMP.
           10 NI-G48DBANTGESPOIF    PIC S9(4) COMP.
           10 NI-G48DBANTVORJ13     PIC S9(4) COMP.
           10 NI-G48DBEREIKONHALB   PIC S9(4) COMP.
           10 NI-G48DBANLIDOIF      PIC S9(4) COMP.
           10 NI-G48DBWPBEZ         PIC S9(4) COMP.
           10 NI-G48DBRUECKPR       PIC S9(4) COMP.
           10 NI-G48DBNRGWINKBA     PIC S9(4) COMP.
           10 NI-G48DBNRGWINKBAVH   PIC S9(4) COMP.
           10 NI-G48DBNRGWOBA       PIC S9(4) COMP.
           10 NI-G48DBNRGWOBAVH     PIC S9(4) COMP.
        05 DCL-G48-DEPOTBESTAND.
           10 G48DBVNR             PIC X(15).
           10 G48DBUNTERNR         PIC X(2).
           10 G48DBWAEHRUNGSKZ     PIC X(3).
           10 G48DBWPKENNNR        PIC X(6).
           10 G48DBISIN            PIC X(12).
           10 G48DBART             PIC X(1).
           10 G48DBSTUEOWERT       PIC S9(12)V9(4) USAGE COMP-3.
           10 G48DBORGWKZ          PIC X(3).
           10 G48DBKURSWERTEUR     PIC S9(10)V9(2) USAGE COMP-3.
           10 G48DBKURSWERTORG     PIC S9(12)V9(2) USAGE COMP-3.
           10 G48DBKUMEIN          PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBKUMAUS          PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBDELKZ           PIC X(1).
           10 G48DBKUMAUTOWA       PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBKAPITALAKT      PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBKAPITALVORJ     PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBKUMUEGES        PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBKUMUEVOR        PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBEINSTKURS       PIC S9(5)V9(2) USAGE COMP-3.
           10 G48DBQUALIFLAG       PIC X(1).
           10 G48DBREALGEWVERL     PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBKUMBABR         PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBANTEILEFREI     PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBVLSPERRDAT1     PIC X(8).
           10 G48DBANGESPERRT1     PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBVLSPERRDAT2     PIC X(8).
           10 G48DBANGESPERRT2     PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBVLSPERRDAT3     PIC X(8).
           10 G48DBANGESPERRT3     PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBANTLGEKOIF      PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBKUMAUSHALBJ     PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBRUEBRUGES       PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBRUEBRUVORJ      PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBKUBAAUBRVJ      PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBANTLVEFOIF      PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBANTGESPOIF      PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBANTVORJ13       PIC S9(7)V9(4) USAGE COMP-3.
           10 G48DBEREIKONHALB     PIC S9(6)V9(2) USAGE COMP-3.
           10 G48DBANLIDOIF        PIC X(14).
           10 G48DBWPBEZ           PIC X(50).
           10 G48DBRUECKPR         PIC S9(5)V9(4) USAGE COMP-3.
           10 G48DBNRGWINKBA       PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBNRGWINKBAVH     PIC S9(6)V9(2) USAGE COMP-3.
           10 G48DBNRGWOBA         PIC S9(11)V9(2) USAGE COMP-3.
           10 G48DBNRGWOBAVH       PIC S9(6)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

