      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.11.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1879.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1879.CPv  $
      *
      *   Rev 2.1   Nov 16 2017 10:22:58   t13ae46
      *APA02520: Produkteinführung Geiger Goldsparplan - Programme
      *
      *   Rev 2.0   Nov 13 2017 16:38:46   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1879 FUNKTION UWPA0046                   *
      *                 GEIGER-PM FORMAL PRUEFEN UND NORMIEREN       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    14.11.2017                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02520!14.11.17!2.1 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1879                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1879.
             10 PI-DRPA1879-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1879-PEG-DATEN-GEIGER.
                COPY DRPA1880.
             10 PI-DRPA1879-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1879.
          05 PO-DRPA1879-RC                 PIC  X(003).
             88 PO-DRPA1879-RC-OK                VALUE 'OK '.
             88 PO-DRPA1879-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
      ****************************************************************
      *                                                              *

