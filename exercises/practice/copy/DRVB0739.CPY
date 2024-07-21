      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0739.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0739.CPv  $
      *
      *   Rev 2.1   Sep 20 2004 10:04:10   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Sep 16 2004 12:34:00   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0739                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0009                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    16.09.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00001!16.09.04!2.1 ! NEU                       ! KLEEMEIER*
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
       01  PI-DRVB0739.
           05  PI-DRVB0739-DATEN.
               10 PI-DRVB0739-MS-NR            PIC S9(004) COMP.
               10 PI-DRVB0739-MS-NR-VBI        PIC S9(009) COMP.
      *
       01  PO-DRVB0739.
           05  PO-DRVB0739-RC                  PIC  X(003).
               88 PO-DRVB0739-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0739-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0739-MS-BEZ-KURZ         PIC  X(004).
           05  PO-DRVB0739-MS-BEZ              PIC  X(040).
           05  PO-DRVB0739-MS-BEZ-KURZ-VBI     PIC  X(004).
           05  PO-DRVB0739-MS-BEZ-VBI          PIC  X(040).
      ****
      **** ENDE COPY DRVB0739 ****************************************
      ****
