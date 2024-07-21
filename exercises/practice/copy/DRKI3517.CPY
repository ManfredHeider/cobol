      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3517.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3517.CPv  $
      *
      *   Rev 2.2   Jan 22 2019 14:37:42   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.1   Dec 19 2018 09:41:00   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.0   Nov 05 2018 08:54:32   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI3517                                      *
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
      *    FÜR DIE FUNKTION DRKI3517                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
 
       01 PI-DRKI3517.
          05 PI-DRKI3517-PROGRAMM                PIC  X(008).
          05 PI-DRKI3517-UFKT-CODE               PIC  9(002).
          05 PI-DRKI3517-DATEN.
 ************10 PI-DRKI3517-G11KDSPARTE          PIC  X(0003).
 ************10 PI-DRKI3517-G11KDVERTRAGSNR      PIC  X(0015).
 ************10 PI-DRKI3517-G11KDROLLE           PIC S9(4) COMP.
             10 PI-DRKI3517-G11KMANDANT          PIC  S9(009) COMP.
 
       01 PO-DRKI3517.
 
          05 PO-DRKI3517-RC                      PIC  X(003).
             88 PO-DRKI3517-RC-OK                VALUE 'OK '.
             88 PO-DRKI3517-RC-NOT-FOUND         VALUE 'NFD'.
             88 PO-DRKI3517-RC-EOF               VALUE 'EOF'.
 
          05 PO-DRKI3517-DATEN.
           10 PO-DRKI3517-G11KDSPARTE            PIC  X(0003).
           10 PO-DRKI3517-G11KDVERTRAGSNR        PIC  X(0015).
           10 PO-DRKI3517-G11KDROLLE             PIC S9(4) COMP.
           10 PO-DRKI3517-REST.
             15 PO-DRKI3517-G11KDAENDKZ          PIC S9(4) COMP.
             15 PO-DRKI3517-G11KDANREDEKZ        PIC  X(0002).
             15 PO-DRKI3517-G11KDBIC             PIC  X(0011).
             15 PO-DRKI3517-G11KDGEBDAT          PIC  X(0008).
             15 PO-DRKI3517-G11KDGESCHLECHT      PIC  X(0001).
             15 PO-DRKI3517-G11KDIBAN            PIC  X(0034).
             15 PO-DRKI3517-G11KDLKZ             PIC  X(0003).
             15 PO-DRKI3517-G11KDNACHNAME        PIC  X(0040).
             15 PO-DRKI3517-G11KDORT             PIC  X(0040).
             15 PO-DRKI3517-G11KDPLZ             PIC  X(0009).
             15 PO-DRKI3517-G11KDPOSTFACH        PIC  X(0045).
             15 PO-DRKI3517-G11KDSTRASSE         PIC  X(0045).
             15 PO-DRKI3517-G11KDVORNAME         PIC  X(0030).
 
      ***** Ende Copy-Member DRKI3517 *****************************
 
 
 

