      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0940.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0940.CPv  $
      *
      *   Rev 2.1   May 27 2009 12:29:30   t13ex57
      *AVB01035: Kündigungsfristen neuer VB-Vertrag
      *
      *   Rev 2.0   May 12 2009 15:53:00   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0940                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOLL                                             *
      *    DATUM:    12.05.2009                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01035!12.05.09! 01 ! NEUERSTELLUNG             ! BOLL     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0940                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0940.
          05 PI-DRVB0940-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0940-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0940.

          05 PO-DRVB0940-RC                 PIC  X(003).
             88 PO-DRVB0940-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0940-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0940-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0940-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0940-DATEN.
             10 PO-DRVB0940-MS-NR            PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0940 *****************************
