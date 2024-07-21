      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0970.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0970.CPv  $
      *
      *   Rev 2.1   May 03 2010 08:03:30   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung 
      *
      *   Rev 2.0   Mar 08 2010 12:28:44   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0970                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0117                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.03.2010                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01031!08.03.10!2.1 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0970                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0970.
          05 PI-DRVB0970-EDW-UNT-ST-NR      PIC S9(009) COMP.
          05 PI-DRVB0970-KNR-EDW-STATUS     PIC S9(004) COMP.
          05 PI-DRVB0970-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0970.

          05 PO-DRVB0970-RC                 PIC  X(003).
             88 PO-DRVB0970-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0970-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0970-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0970-DATEN.
             10 PO-DRVB0970-ANZAHL-WEICHEN  PIC 9(001).
      *
      ***** ENDE COPY-MEMBER DRVB0970 *****************************
