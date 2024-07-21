      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1870.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1870.CPv  $
      *
      *   Rev 2.1   May 18 2017 15:27:04   t13ae46
      *APA02360: Fingerhaus: PEG-Copy JWA
      *
      *   Rev 2.0   Apr 20 2017 14:29:00   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1870 FUNKTION UWPA0051                   *
      *                 FINGERHAUS - PM FORMAL PRUEFEN/NORMIEREN     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    20.04.2017                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02360!20.04.17!2.1 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1870                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1870.
             10 PI-DRPA1870-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1870-PEG-DATEN-F-HAUS.
                COPY DRPA1869.
             10 PI-DRPA1870-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1870.
          05 PO-DRPA1870-RC                 PIC  X(003).
             88 PO-DRPA1870-RC-OK                VALUE 'OK '.
             88 PO-DRPA1870-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************


