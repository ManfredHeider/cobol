      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1068.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1068.CPv  $
      *
      *   Rev 2.2   Mar 22 2013 15:37:22   t13ae30
      *AVB01244: Anpassung Stammdaten-DTA
      *
      *   Rev 2.1   Mar 22 2013 15:03:14   t13ae30
      *AVB01244: Anpassung Stammdaten-DTA
      *
      *   Rev 2.0   Mar 04 2013 15:28:14   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1068                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0021                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THORSTEN WILLIG                                  *
      *    DATUM:    04.03.2013                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01244!04.03.13!2.0 ! NEUERSTELLUNG             !TH.WILLIG *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1068                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *    Fachliche Beschreibung:                                   *
      *    Ermittlung IBAN u. BIC                                    *
      ****************************************************************
      *
      *
       01 PI-DRVB1068.
          05 PI-DRVB1068-ST-NR              PIC S9(009) COMP.
          05 PI-DRVB1068-BV-TYP             PIC S9(004) COMP.

       01 PO-DRVB1068.

          05 PO-DRVB1068-RC                 PIC  X(003).
             88 PO-DRVB1068-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1068-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1068-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1068-DATEN.
             10 PO-DRVB1068-BV-IBAN            PIC  X(034).
             10 PO-DRVB1068-BV-BIC             PIC  X(011).

      *
      ***** ENDE COPY-MEMBER DRVB1068 *****************************
