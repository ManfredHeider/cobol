      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.04.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0979.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0979.CPv  $
      *
      *   Rev 2.1   May 03 2010 08:04:20   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung 
      *
      *   Rev 2.0   Apr 16 2010 10:59:56   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0979                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0117                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    16.04.2010                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01031!16.04.10!2.1 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0979                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0979.
          05 PI-DRVB0979-EDW-DL-NEU         PIC S9(009) COMP.
          05 PI-DRVB0979-KNR-EDW-STATUS     PIC S9(004) COMP.
      *
       01 PO-DRVB0979.

          05 PO-DRVB0979-RC                 PIC  X(003).
             88 PO-DRVB0979-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0979-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0979-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0979-DATEN.
             10 PO-DRVB0979-ANZAHL-WEICHEN  PIC 9(001).
      *
      ***** ENDE COPY-MEMBER DRVB0979 *****************************
