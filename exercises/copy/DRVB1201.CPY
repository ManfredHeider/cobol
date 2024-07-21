      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  11.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1201.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB1201.CPv  $
      *
      *   Rev 2.3   Sep 11 2019 09:14:30   T13EX18
      *AVB01456: Nachfolgeregelungen Erweiterung lese OPZ(VBL-510)
      *
      *   Rev 2.2   Jul 18 2019 14:39:40   t13ex18
      *AVB01456: Nachfolgeregelungen Erweiterung lese OPZ(VBL-510)
      *
      *   Rev 2.1   Dec 03 2018 11:49:04   t13ex18
      *AVB01456: Nachfolgeregelungen ZXVB für TBVB0126
      *
      *   Rev 2.0   Sep 11 2018 13:26:48   T13EX18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1201                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0126                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. Bruder                                        *
      *    DATUM:    12.09.2018                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01456!12.09.18!2.01! NEUERSTELLUNG             ! G. Bruder*
      * AVB01456!18.07.19!2.02! KNR 278 Nachfolgeregelung ! G. Bruder*
      *         !        !    ! in Ausgabe aufgenommen.   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1201                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1201.
          05 PI-DRVB1201-PROGRAMM           PIC  X(008).
          05 PI-DRVB1201-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB1201.
 
          05 PO-DRVB1201-RC                 PIC  X(003).
             88 PO-DRVB1201-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1201-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1201-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1201-RC-ERR                       VALUE 'ERR'.
 
          05 PO-DRVB1201-DATEN.
             10 PO-DRVB1201-NRB-ABGEB-NR     PIC S9(09) BINARY.
             10 PO-DRVB1201-NRB-AUFNEHM-NR   PIC S9(09) BINARY.
             10 PO-DRVB1201-NRB-DAT-VON      PIC  X(10).
             10 PO-DRVB1201-NRB-DAT-BIS      PIC  X(10).
180719*  KNR 278 Nachfolgeregelungen
180719*  KTY 5, 6 und 7
180719       10 PO-DRVB1201-KTY-NR-TYP       PIC S9(04) BINARY.
180719       10 PO-DRVB1201-KNR-NR-TYP       PIC S9(04) BINARY.
      *
      ***** Ende-Copy-Member *****************************************
 


