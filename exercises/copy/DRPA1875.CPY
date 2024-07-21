      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1875.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1875.CPv  $
      *
      *   Rev 2.1   Jun 13 2017 12:39:16   t13ex67
      *APA02396: Lohnwerk: PEG-Copy MSS
      *
      *   Rev 2.0   May 18 2017 14:33:34   t13ex24
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1875 FUNKTION UWPA0051                   *
      *                 Lohnwerk - PM FORMAL PRUEFEN/NORMIEREN       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KÖPRÜCÜ                                         *
      *    DATUM:    18.05.2017                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02396!18.05.17!2.1 ! NEU                       ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1875                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1875.
             10 PI-DRPA1875-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1875-PEG-DATEN-LOHNWERK.
                COPY DRPA1873.
             10 PI-DRPA1875-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1875.
          05 PO-DRPA1875-RC                 PIC  X(003).
             88 PO-DRPA1875-RC-OK                VALUE 'OK '.
             88 PO-DRPA1875-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************



