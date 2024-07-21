      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0726.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0726.CPv  $
      *
      *   Rev 2.2   Sep 10 2019 10:18:36   T13EX18
      *FVD00090: Korrektur OP-10766 und OP-10767
      *
      *   Rev 2.1   Sep 20 2004 10:02:26   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Aug 13 2004 16:23:46   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0726                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS FUNKTIONS-OZP UPVB0128               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    16.08.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00001!16.08.04!2.1 ! NEU                       ! KLEEMEIER*
      * FVD00090!10.09.19!2.9 ! OCCURS 20 -> 40 erweitert ! G.Bruder *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRVB0726.
           05  PI-DRVB0726-DATEN.
               10 PI-DRVB0726-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0726-STICHTAG-ISO     PIC  X(010).
      *
       01  PO-DRVB0726.
           05  PO-DRVB0726-RC                  PIC  X(003).
               88 PO-DRVB0726-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0726-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0726-MS-NR               PIC S9(004) COMP.
           05  PO-DRVB0726-MS-BEZ-KURZ         PIC  X(004).
           05  PO-DRVB0726-MS-BEZ              PIC  X(040).
           05  PO-DRVB0726-MS-NR-VBI           PIC S9(009) COMP.
           05  PO-DRVB0726-MS-BEZ-KURZ-VBI     PIC  X(004).
           05  PO-DRVB0726-MS-BEZ-VBI          PIC  X(040).
           05  PO-DRVB0726-ANZ-MSBZ            PIC S9(004) COMP.
           05  PO-DRVB0726-TAB-MSBZ            OCCURS 40.
               10  PO-DRVB0726-TAB-MA-ST-NR           PIC S9(009) COMP.
               10  PO-DRVB0726-TAB-MS-NR-VBI          PIC S9(009) COMP.
               10  PO-DRVB0726-TAB-MS-NR              PIC S9(004) COMP.
               10  PO-DRVB0726-TAB-BS-NR              PIC S9(004) COMP.
               10  PO-DRVB0726-TAB-MSBZ-DAT-VON       PIC  X(026).
               10  PO-DRVB0726-TAB-MSBZ-LAST-UPD      PIC  X(026).
               10  PO-DRVB0726-TAB-MSBZ-DAT-BIS       PIC  X(010).
      ****
      **** ENDE COPY DRVB0726 ****************************************
      ****

