      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1727.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1727.CPv  $
      *
      *   Rev 2.1   Jan 05 2012 14:24:48   t13ex25
      *APA01284: Ges. 95- maschinelle DTA- Übernahme
      *
      *   Rev 2.0   Nov 25 2011 14:51:36   t13ex25
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1727 FUNKTION GES. 95-PM FORMAL PRUEFEN  *
      *                 UND NORMIEREN                                *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01284!29.11.11!2.0 ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1727                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1727.
          10 PI-DRPA1727-PZVP-VERSION    PIC S9(004) BINARY.
          10 PI-DRPA1727-PEG-DATEN-GES95.
             COPY DRPA1728.
          10 PI-DRPA1727-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1727.
          05 PO-DRPA1727-RC                 PIC  X(003).
             88 PO-DRPA1727-RC-OK                VALUE 'OK '.
             88 PO-DRPA1727-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ************ ENDE COPY-MEMBER ************************************
