      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  10.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1193.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1193.CPv  $
      *
      *   Rev 2.1   Dec 07 2018 14:00:04   t13ex18
      *AVB01456: Nachfolgeregelungen ZXVB für TBVB0131
      *
      *   Rev 2.0   Aug 10 2018 10:49:12   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1193                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0131                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. Bruder                                        *
      *    DATUM:    13.08.2018                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01456!13.08.18!2.01! NEUERSTELLUNG             ! G. Bruder*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1193                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1193.
          05 PI-DRVB1193-PROGRAMM           PIC  X(008).
          05 PI-DRVB1193-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB1193.
 
          05 PO-DRVB1193-RC                 PIC  X(003).
             88 PO-DRVB1193-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1193-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1193-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1193-RC-ERR                       VALUE 'ERR'.
 
          05 PO-DRVB1193-DATEN.
             10 PO-DRVB1193-NRDF-ABGEB-NR    PIC S9(09) BINARY.
             10 PO-DRVB1193-NRDF-AUFNEHM-NR  PIC S9(09) BINARY.
             10 PO-DRVB1193-KTY-NR-TYP       PIC S9(04) BINARY.
             10 PO-DRVB1193-KNR-NR-TYP       PIC S9(04) BINARY.
             10 PO-DRVB1193-NRDF-DAT-VON     PIC  X(10).
             10 PO-DRVB1193-NRDF-LAST-UPDATE PIC  X(26).
             10 PO-DRVB1193-NRDF-UEBG-ZTPKT  PIC  X(10).
      *
      ***** Ende-Copy-Member *****************************************

