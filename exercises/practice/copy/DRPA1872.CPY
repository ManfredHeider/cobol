      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1872.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1872.CPv  $
      *
      *   Rev 2.1   May 18 2017 15:49:42   t13ae46
      *APA02438: Devolo: PEG-Copy JWA 
      *
      *   Rev 2.0   Apr 28 2017 10:01:18   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1872 FUNKTION UWPA0051                   *
      *                 DEVOLO - PM FORMAL PRUEFEN/NORMIEREN         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    28.04.2017                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02438!28.04.17!2.1 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1872                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1872.
             10 PI-DRPA1872-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1872-PEG-DATEN-DEVOLO.
                COPY DRPA1871.
             10 PI-DRPA1872-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1872.
          05 PO-DRPA1872-RC                 PIC  X(003).
             88 PO-DRPA1872-RC-OK                VALUE 'OK '.
             88 PO-DRPA1872-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************


