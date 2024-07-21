      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1876.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1876.CPv  $
      *
      *   Rev 2.2   Nov 30 2017 11:13:06   t13ae37
      *APA02520: Produkteinführung Geiger Goldsparplan
      *
      *   Rev 2.1   Sep 04 2017 16:19:08   t13ae37
      *APA02473: cadooz: PEG - ABU
      *
      *   Rev 2.0   Jul 10 2017 08:49:10   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1876 FUNKTION UWPA0051                   *
      *                 CADOOZ - PM FORMAL PRUEFEN/NORMIEREN         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    20.04.2017                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02473!10.07.17!2.1 ! NEU                       ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1876                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1876.
             10 PI-DRPA1876-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1876-PEG-DATEN-CADOOZ.
                COPY DRPA1877.
             10 PI-DRPA1876-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1876.
          05 PO-DRPA1876-RC                 PIC  X(003).
             88 PO-DRPA1876-RC-OK                VALUE 'OK '.
             88 PO-DRPA1876-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
 

