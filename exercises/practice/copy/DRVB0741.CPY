      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0741.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0741.CPv  $
      *
      *   Rev 2.1   Sep 23 2004 11:11:24   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Sep 22 2004 15:26:24   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0741                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0012                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    22.09.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00001!22.09.04!2.1 ! NEU                       ! KLEEMEIER*
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
       01  PI-DRVB0741.
           05  PI-DRVB0741-DATEN.
               10 PI-DRVB0741-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0741-MS-NR            PIC S9(004) COMP.
               10 PI-DRVB0741-BS-NR            PIC S9(004) COMP.
               10 PI-DRVB0741-MSBZ-DAT-VON     PIC  X(026).
      *
       01  PO-DRVB0741.
           05  PO-DRVB0741-RC                  PIC  X(003).
               88 PO-DRVB0741-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0741-RC-NOT-FOUND                 VALUE 'NFD'.
      ****
      **** ENDE COPY DRVB0741 ****************************************
      ****
