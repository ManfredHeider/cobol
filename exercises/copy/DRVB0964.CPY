      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0964.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0964.CPv  $
      *
      *   Rev 2.1   May 03 2010 08:03:42   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung 
      *
      *   Rev 2.0   Feb 12 2010 09:16:42   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0964                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0117                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    17.02.2010                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01031!17.02.10!2.1 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0964                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0964.
          05 PI-DRVB0964-EDW-DL-ALT         PIC S9(009) COMP.
          05 PI-DRVB0964-EDW-UNT-ST-NR      PIC S9(009) COMP.
          05 PI-DRVB0964-KNR-EDW-STATUS     PIC S9(004) COMP.
      *
       01 PO-DRVB0964.

          05 PO-DRVB0964-RC                 PIC  X(003).
             88 PO-DRVB0964-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0964-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0964-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0964-DATEN.
             10 PO-DRVB0964-NI-VIVB0117     PIC X(020).
             10 PO-DRVB0964-DCL-VIVB0117    PIC X(066).
      *
      ***** ENDE COPY-MEMBER DRVB0964 *****************************
