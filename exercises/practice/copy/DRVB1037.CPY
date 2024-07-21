      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1037.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRVB1037.CPv  $
      *
      *   Rev 2.6   Feb 01 2012 15:44:48   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.5   Jan 16 2012 14:52:58   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.4   Jan 16 2012 14:39:36   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.3   Jan 16 2012 14:30:52   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.2   Jan 16 2012 14:16:08   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.1   Jan 16 2012 14:06:34   t13ae14
      *ASL00420: Partnerbonus DCLGEN, Lese OZP, Ermittlungsprogramm
      * ...
      *
      *   Rev 2.0   Dec 19 2011 11:20:24   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB1037                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0215                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    19.12.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00420!19.12.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1037 - ERMITTELN SATZ AUS TABELLE PABONERF    *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1037.
          05 PI-DRVB1037-DATEN.
             10 PI-DRVB1037-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1037-PABONERF-UNTST     PIC S9(09)       BINARY.
             10 PI-DRVB1037-STICHTAG           PIC  X(10).

       01 PO-DRVB1037.
         03 PO-DRVB1037-RC                     PIC  X(03).
             88 PO-DRVB1037-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1037-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1037-DATEN.
          05 PO-DRVB1037-NI-VIVB0215.
             10 NI-DRVB1037-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1037-PABONERF-UNTST     PIC S9(04)       BINARY.
             10 NI-DRVB1037-STICHTAG           PIC S9(04)       BINARY.
             10 NI-DRVB1037-PABON-LAST-UPD     PIC S9(04)       BINARY.
             10 NI-DRVB1037-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1037-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1037-PABON-ANSPR-BETR   PIC S9(04)       BINARY.
             10 NI-DRVB1037-KNR-NERF-GRUND     PIC S9(04)       BINARY.
             10 NI-DRVB1037-KTY-NERF-GRUND     PIC S9(04)       BINARY.
          05 PO-DRVB1037-DCL-VIVB0215.
             10 PO-DRVB1037-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1037-PABONERF-UNTST     PIC S9(09)       BINARY.
             10 PO-DRVB1037-STICHTAG           PIC  X(10).
             10 PO-DRVB1037-PABON-LAST-UPD     PIC  X(26).
             10 PO-DRVB1037-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1037-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1037-PABON-ANSPR-BETR   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1037-KNR-NERF-GRUND     PIC S9(04)       BINARY.
             10 PO-DRVB1037-KTY-NERF-GRUND     PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1037   --------------------------*
      *---------------------------------------------------------------*
