      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.02.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1866.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1866.CPv  $
      *
      *   Rev 2.1   Mar 27 2017 16:47:28   t13ae46
      *APA02353: Neuer Produktpartner TeleSon - PEG
      *
      *   Rev 2.0   Feb 16 2017 13:01:36   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1866 FUNKTION UWPA0051                   *
      *                 TELESON - PM FORMAL PRUEFEN/NORMIEREN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    17.03.2015                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02353!16.02.17!2.1 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1866                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1866.
             10 PI-DRPA1866-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1866-PEG-DATEN-TELESON.
                COPY DRPA1867.
             10 PI-DRPA1866-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1866.
          05 PO-DRPA1866-RC                 PIC  X(003).
             88 PO-DRPA1866-RC-OK                VALUE 'OK '.
             88 PO-DRPA1866-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************

