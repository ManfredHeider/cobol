      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07/09/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0513.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0513.CPv  $
      *
      *   Rev 2.4   Nov 22 2018 10:09:16   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.3   Oct 27 2018 15:21:30   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.2   Oct 25 2018 14:42:14   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.0   Sep 7 2018 11:16:13   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G42_DEPOTBESTAND TABLE
CCC004*    ( G42BVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G42BWKN                        CHAR(6) NOT NULL,
CCC004*      G42BISIN                       CHAR(12) NOT NULL,
CCC004*      G42BART                        CHAR(1) NOT NULL,
CCC004*      G42BBESTAND                    DECIMAL(16, 4) NOT NULL,
CCC004*      G42BORGWAEH                    CHAR(3) NOT NULL,
CCC004*      G42BKURSWERT                   DECIMAL(12, 2) NOT NULL,
CCC004*      G42BKUMAUTOWA                  DECIMAL(13, 2) NOT NULL,
CCC004*      G42BKAPITALAKT                 DECIMAL(13, 2) NOT NULL,
CCC004*      G42BKUMUAWIEVOR                DECIMAL(13, 2) NOT NULL,
CCC004*      G42BEINSTKURS                  DECIMAL(7, 2) NOT NULL,
CCC004*      G42BFLEINSTKURS                CHAR(1) NOT NULL,
CCC004*      G42BREALGVERL                  DECIMAL(13, 2) NOT NULL,
CCC004*      G42BKUMBARAUSBR                DECIMAL(13, 2) NOT NULL,
CCC004*      G42BANTEILEFREI                DECIMAL(11, 4) NOT NULL,
CCC004*      G42BVLSPERRDAT1                CHAR(8) NOT NULL,
CCC004*      G42BANTGESP1                   DECIMAL(11, 4) NOT NULL,
CCC004*      G42BVLSPERRDAT2                CHAR(8) NOT NULL,
CCC004*      G42BANTGESP2                   DECIMAL(11, 4) NOT NULL,
CCC004*      G42BVLSPERRDAT3                CHAR(8) NOT NULL,
CCC004*      G42BANTGESP3                   DECIMAL(11, 4) NOT NULL,
CCC004*      G42BANTGEKOIF                  DECIMAL(11, 4) NOT NULL,
CCC004*      G42BKUMAUSZHALBJ               DECIMAL(13, 2) NOT NULL,
CCC004*      G42BANTEILVEFOIF               DECIMAL(13, 2) NOT NULL,
CCC004*      G42BANTEILGESPOIF              DECIMAL(13, 2) NOT NULL,
CCC004*      G42BANLEGERIDOIF               CHAR(50) NOT NULL,
CCC004*      G42BRUECKNAHMEPR               DECIMAL(9, 4) NOT NULL,
CCC004*      G42BNRGWINKLBBA                DECIMAL(13, 2) NOT NULL,
CCC004*      G42BNRGWINKLBBAVH              DECIMAL(8, 2) NOT NULL,
CCC004*      G42BNRGWOHNEBBA                DECIMAL(13, 2) NOT NULL,
CCC004*      G42BNRGWOHNEBBAVH              DECIMAL(8, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G42_DEPOTBESTAND            *
      ******************************************************************
       01  G42-DEPOTBESTAND-GESAMT.
        05 NI-G42-DEPOTBESTAND-TAB.
           10 NI-G42-DEPOTBESTAND-ELE PIC S9(04) COMP OCCURS 31 TIMES.
        05 NI-G42-DEPOTBESTAND REDEFINES NI-G42-DEPOTBESTAND-TAB.
           10 NI-G42BVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G42BWKN            PIC S9(4) COMP.
           10 NI-G42BISIN           PIC S9(4) COMP.
           10 NI-G42BART            PIC S9(4) COMP.
           10 NI-G42BBESTAND        PIC S9(4) COMP.
           10 NI-G42BORGWAEH        PIC S9(4) COMP.
           10 NI-G42BKURSWERT       PIC S9(4) COMP.
           10 NI-G42BKUMAUTOWA      PIC S9(4) COMP.
           10 NI-G42BKAPITALAKT     PIC S9(4) COMP.
           10 NI-G42BKUMUAWIEVOR    PIC S9(4) COMP.
           10 NI-G42BEINSTKURS      PIC S9(4) COMP.
           10 NI-G42BFLEINSTKURS    PIC S9(4) COMP.
           10 NI-G42BREALGVERL      PIC S9(4) COMP.
           10 NI-G42BKUMBARAUSBR    PIC S9(4) COMP.
           10 NI-G42BANTEILEFREI    PIC S9(4) COMP.
           10 NI-G42BVLSPERRDAT1    PIC S9(4) COMP.
           10 NI-G42BANTGESP1       PIC S9(4) COMP.
           10 NI-G42BVLSPERRDAT2    PIC S9(4) COMP.
           10 NI-G42BANTGESP2       PIC S9(4) COMP.
           10 NI-G42BVLSPERRDAT3    PIC S9(4) COMP.
           10 NI-G42BANTGESP3       PIC S9(4) COMP.
           10 NI-G42BANTGEKOIF      PIC S9(4) COMP.
           10 NI-G42BKUMAUSZHALBJ   PIC S9(4) COMP.
           10 NI-G42BANTEILVEFOIF   PIC S9(4) COMP.
           10 NI-G42BANTEILGESPOIF  PIC S9(4) COMP.
           10 NI-G42BANLEGERIDOIF   PIC S9(4) COMP.
           10 NI-G42BRUECKNAHMEPR   PIC S9(4) COMP.
           10 NI-G42BNRGWINKLBBA    PIC S9(4) COMP.
           10 NI-G42BNRGWINKLBBAVH  PIC S9(4) COMP.
           10 NI-G42BNRGWOHNEBBA    PIC S9(4) COMP.
           10 NI-G42BNRGWOHNEBBAVH  PIC S9(4) COMP.
        05 DCL-G42_DEPOTBESTAND.
           10 G42BVERTRAGSNR       PIC X(15).
           10 G42BWKN              PIC X(6).
           10 G42BISIN             PIC X(12).
           10 G42BART              PIC X(1).
           10 G42BBESTAND          PIC S9(12)V9(4) USAGE COMP-3.
           10 G42BORGWAEH          PIC X(3).
           10 G42BKURSWERT         PIC S9(10)V9(2) USAGE COMP-3.
           10 G42BKUMAUTOWA        PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BKAPITALAKT       PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BKUMUAWIEVOR      PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BEINSTKURS        PIC S9(5)V9(2) USAGE COMP-3.
           10 G42BFLEINSTKURS      PIC X(1).
           10 G42BREALGVERL        PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BKUMBARAUSBR      PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BANTEILEFREI      PIC S9(7)V9(4) USAGE COMP-3.
           10 G42BVLSPERRDAT1      PIC X(8).
           10 G42BANTGESP1         PIC S9(7)V9(4) USAGE COMP-3.
           10 G42BVLSPERRDAT2      PIC X(8).
           10 G42BANTGESP2         PIC S9(7)V9(4) USAGE COMP-3.
           10 G42BVLSPERRDAT3      PIC X(8).
           10 G42BANTGESP3         PIC S9(7)V9(4) USAGE COMP-3.
           10 G42BANTGEKOIF        PIC S9(7)V9(4) USAGE COMP-3.
           10 G42BKUMAUSZHALBJ     PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BANTEILVEFOIF     PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BANTEILGESPOIF    PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BANLEGERIDOIF     PIC X(50).
           10 G42BRUECKNAHMEPR     PIC S9(5)V9(4) USAGE COMP-3.
           10 G42BNRGWINKLBBA      PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BNRGWINKLBBAVH    PIC S9(6)V9(2) USAGE COMP-3.
           10 G42BNRGWOHNEBBA      PIC S9(11)V9(2) USAGE COMP-3.
           10 G42BNRGWOHNEBBAVH    PIC S9(6)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************
 
 

