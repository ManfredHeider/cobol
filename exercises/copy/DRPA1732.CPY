      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1732.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1732.CPv  $
      *
      *   Rev 2.1   Dec 17 2013 11:16:48   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG
      *
      *   Rev 2.0   Feb 06 2013 09:37:32   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1732 FUNKTION UWPA0046                   *
      *                 AM LEBEN FORMAL PRUEFEN UND NORMIEREN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                        *
      *    DATUM:    11.03.2004                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!12.02.13!2.1 ! NEU                       ! STORCH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1732                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1732.
             10 PI-DRPA1732-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1732-PEG-DATEN-AML.
                COPY DRPA1731.
             10 PI-DRPA1732-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1732.
          05 PO-DRPA1732-RC                 PIC  X(003).
             88 PO-DRPA1732-RC-OK                VALUE 'OK '.
             88 PO-DRPA1732-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
