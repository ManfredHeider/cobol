      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3254.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3254.CPv  $
      *
      *   Rev 2.1   Jun 07 2018 10:13:28   t13ae31
      *AKI01567: G10 Umstellung KSDS in DB2 OZP Kunde
      *
      *   Rev 2.0   Jan 30 2018 10:37:08   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3254                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0487                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Hübner                                         *
      *    DATUM:    30.01.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01567!30.01.18!1.0 ! NEU                       ! P.Hübner *
      * -------------------------------------------------------------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3254                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
 
       01 PI-DRKI3254.
          05 PI-DRKI3254-PROGRAMM                PIC  X(008).
          05 PI-DRKI3254-UFKT-CODE               PIC  9(002).
          05 PI-DRKI3254-DATEN.
             10 PI-DRKI3254-G10KDSPARTE          PIC  X(0003).
             10 PI-DRKI3254-G10KDVERTRAGSNR      PIC  X(0015).
             10 PI-DRKI3254-G10KDROLLE           PIC S9(4) COMP.
 
       01 PO-DRKI3254.
 
          05 PO-DRKI3254-RC                      PIC  X(003).
             88 PO-DRKI3254-RC-OK                VALUE 'OK '.
             88 PO-DRKI3254-RC-NOT-FOUND         VALUE 'NFD'.
             88 PO-DRKI3254-RC-EOF               VALUE 'EOF'.
 
          05 PO-DRKI3254-DATEN.
           10 PO-DRKI3254-G10KDSPARTE            PIC  X(0003).
           10 PO-DRKI3254-G10KDVERTRAGSNR        PIC  X(0015).
           10 PO-DRKI3254-G10KDROLLE             PIC S9(4) COMP.
           10 PO-DRKI3254-REST.
             15 PO-DRKI3254-G10KDAENDKZ          PIC S9(4) COMP.
             15 PO-DRKI3254-G10KDANREDEKZ        PIC  X(0002).
             15 PO-DRKI3254-G10KDBIC             PIC  X(0011).
             15 PO-DRKI3254-G10KDGEBDAT          PIC  X(0008).
             15 PO-DRKI3254-G10KDGESCHLECHT      PIC  X(0001).
             15 PO-DRKI3254-G10KDIBAN            PIC  X(0034).
             15 PO-DRKI3254-G10KDLKZ             PIC  X(0003).
             15 PO-DRKI3254-G10KDNACHNAME        PIC  X(0040).
             15 PO-DRKI3254-G10KDORT             PIC  X(0040).
             15 PO-DRKI3254-G10KDPLZ             PIC  X(0009).
             15 PO-DRKI3254-G10KDPOSTFACH        PIC  X(0045).
             15 PO-DRKI3254-G10KDSTRASSE         PIC  X(0045).
             15 PO-DRKI3254-G10KDVORNAME         PIC  X(0030).
 
      ***** Ende Copy-Member DRKI3254 *****************************
 

