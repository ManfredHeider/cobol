      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  07.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3439.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3439.CPv  $
      *
      *   Rev 2.4   Nov 22 2018 10:09:08   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.3   Nov 02 2018 12:05:32   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.2   Oct 27 2018 15:21:20   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.1   Oct 25 2018 14:42:04   t13ex78
      *AKI01615: OZPs für G42_DEPOTBESTAND
      *
      *   Rev 2.0   Sep 07 2018 11:09:48   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3439                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0513 G42_DEPOTBESTAND               *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01615!13.09.18!2.1 ! Neuerstellung             ! tfl      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3439
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3439.
           05 PI-DRKI3439-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3439-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3439-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3439-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3439-KEY.
               10 PI-DRKI3439-G42BMANDANT     PIC S9(009) COMP.
 
      *
       01  PO-DRKI3439.
           05 PO-DRKI3439-RC                   PIC  X(003).
              88 PO-DRKI3439-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3439-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3439-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3439-DATEN.
              10 PO-DRKI3439-KEY.
                 15 PO-DRKI3439-G42BVERTRAGSNR  PIC  X(0015).
                 15 PO-DRKI3439-G42BWKN         PIC  X(0006).
              10 PO-DRKI3439-G42BANLEGERIDOIF   PIC  X(0050).
              10 PO-DRKI3439-G42BANTEILEFREI   PIC S9(7)V9(4) COMP-3.
              10 PO-DRKI3439-G42BANTEILGESPOIF   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BANTEILVEFOIF   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BANTGEKOIF   PIC S9(7)V9(4) COMP-3.
              10 PO-DRKI3439-G42BANTGESP1   PIC S9(7)V9(4) COMP-3.
              10 PO-DRKI3439-G42BANTGESP2   PIC S9(7)V9(4) COMP-3.
              10 PO-DRKI3439-G42BANTGESP3   PIC S9(7)V9(4) COMP-3.
              10 PO-DRKI3439-G42BART   PIC  X(0001).
              10 PO-DRKI3439-G42BBESTAND   PIC S9(12)V9(4) COMP-3.
              10 PO-DRKI3439-G42BEINSTKURS   PIC S9(5)V9(2) COMP-3.
              10 PO-DRKI3439-G42BFLEINSTKURS   PIC  X(0001).
              10 PO-DRKI3439-G42BISIN   PIC  X(0012).
              10 PO-DRKI3439-G42BKAPITALAKT   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BKUMAUSZHALBJ   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BKUMAUTOWA   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BKUMBARAUSBR   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BKUMUAWIEVOR   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BKURSWERT   PIC S9(10)V9(2) COMP-3.
              10 PO-DRKI3439-G42BNRGWINKLBBA   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BNRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.
              10 PO-DRKI3439-G42BNRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BNRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.
              10 PO-DRKI3439-G42BORGWAEH   PIC  X(0003).
              10 PO-DRKI3439-G42BREALGVERL   PIC S9(11)V9(2) COMP-3.
              10 PO-DRKI3439-G42BRUECKNAHMEPR   PIC S9(5)V9(4) COMP-3.
              10 PO-DRKI3439-G42BVLSPERRDAT1   PIC  X(0008).
              10 PO-DRKI3439-G42BVLSPERRDAT2   PIC  X(0008).
              10 PO-DRKI3439-G42BVLSPERRDAT3   PIC  X(0008).
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 

