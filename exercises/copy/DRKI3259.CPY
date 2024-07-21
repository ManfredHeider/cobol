      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3259.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3259.CPv  $
      *
      *   Rev 2.1   Jun 07 2018 10:13:58   t13ae31
      *AKI01567: G10 Umstellung KSDS in DB2 OZP Kunde
      *
      *   Rev 2.0   Feb 06 2018 13:30:34   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3259                                      *
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
      *    FÜR DIE FUNKTION DRKI3259                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
 
       01 PI-DRKI3259.
          05 PI-DRKI3259-PROGRAMM                PIC  X(008).
          05 PI-DRKI3259-UFKT-CODE               PIC  9(002).
          05 PI-DRKI3259-DATEN.
             10 PI-DRKI3259-G10KDMANDANT         PIC S9(7) COMP.
             10 PI-DRKI3259-G10KDSPARTE          PIC  X(0003).
    *********10 PI-DRKI3259-G10KDVERTRAGSNR      PIC  X(0015).
    *********10 PI-DRKI3259-G10KDROLLE           PIC S9(4) COMP.
 
       01 PO-DRKI3259.
 
          05 PO-DRKI3259-RC                      PIC  X(003).
             88 PO-DRKI3259-RC-OK                VALUE 'OK '.
             88 PO-DRKI3259-RC-NOT-FOUND         VALUE 'NFD'.
             88 PO-DRKI3259-RC-EOF               VALUE 'EOF'.
 
          05 PO-DRKI3259-DATEN.
           10 PO-DRKI3259-G10VVERTRAGSNR        PIC  X(0015).
           10 PO-DRKI3259-G10KDSPARTE            PIC  X(0003).
           10 PO-DRKI3259-G10KDVERTRAGSNR        PIC  X(0015).
           10 PO-DRKI3259-G10KDROLLE             PIC S9(4) COMP.
           10 PO-DRKI3259-REST.
             15 PO-DRKI3259-G10KDAENDKZ          PIC S9(4) COMP.
             15 PO-DRKI3259-G10KDANREDEKZ        PIC  X(0002).
             15 PO-DRKI3259-G10KDBIC             PIC  X(0011).
             15 PO-DRKI3259-G10KDGEBDAT          PIC  X(0008).
             15 PO-DRKI3259-G10KDGESCHLECHT      PIC  X(0001).
             15 PO-DRKI3259-G10KDIBAN            PIC  X(0034).
             15 PO-DRKI3259-G10KDLKZ             PIC  X(0003).
             15 PO-DRKI3259-G10KDNACHNAME        PIC  X(0040).
             15 PO-DRKI3259-G10KDORT             PIC  X(0040).
             15 PO-DRKI3259-G10KDPLZ             PIC  X(0009).
             15 PO-DRKI3259-G10KDPOSTFACH        PIC  X(0045).
             15 PO-DRKI3259-G10KDSTRASSE         PIC  X(0045).
             15 PO-DRKI3259-G10KDVORNAME         PIC  X(0030).
 
      ***** Ende Copy-Member DRKI3259 *****************************
 

