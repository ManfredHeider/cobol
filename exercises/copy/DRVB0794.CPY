      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0794.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0794.CPv  $
      *
      *   Rev 2.1   Nov 09 2005 15:19:44   t13ex13
      *AVB00820: PIN-SST Adressdaten auch für ausgeschiedene VB
      *
      *   Rev 2.0   Sep 27 2005 17:29:04   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0794                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0021                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Ralf Gutfleisch                                  *
      *    DATUM:    27.09.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00820!27.09.05!2.1 ! NEUERSTELLUNG             !Gutfleisch*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0794                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *    Fachliche Beschreibung:                                   *
      *    Ermittlung aktuelle und nicht gesperrte Bankverbindung    *
      ****************************************************************
      *
      *
       01 PI-DRVB0794.
          05 PI-DRVB0794-ST-NR              PIC S9(009) COMP.
          05 PI-DRVB0794-BV-TYP             PIC S9(004) COMP.

       01 PO-DRVB0794.

          05 PO-DRVB0794-RC                 PIC  X(003).
             88 PO-DRVB0794-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0794-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0794-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0794-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0794-DATEN.
             10 PO-DRVB0794-BV-KTO-INHABER     PIC  X(040).
             10 PO-DRVB0794-BV-KTO-NR          PIC  X(020).
             10 PO-DRVB0794-BV-BANKNAME        PIC  X(040).
             10 PO-DRVB0794-BV-BLZ             PIC  X(011).

      *
      ***** ENDE COPY-MEMBER DRVB0707 *****************************

