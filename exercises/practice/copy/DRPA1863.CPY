      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1863.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1863.CPv  $
      *
      *   Rev 2.1   Jan 03 2017 08:48:10   t13ae37
      *APA02347: BKK-Linde als eigenständige Gesellschaft 
      *
      *   Rev 2.0   Nov 29 2016 15:06:54   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1863 FUNKTION BKK    -PM FORMAL PRUEFEN  *
      *                 UND NORMIEREN                                *
      ****************************************************************
      *    VERSION   2.1                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02347!29.11.16!2.1 ! NEU                       ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1863                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1863.
             10 PI-DRPA1863-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1863-PEG-DATEN-BKK.
                COPY DRPA1864.
             10 PI-DRPA1863-KNR-MANDANT     PIC S9(004) BINARY.
 
       01 PO-DRPA1863.
          05 PO-DRPA1863-RC                 PIC  X(003).
             88 PO-DRPA1863-RC-OK                VALUE 'OK '.
             88 PO-DRPA1863-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************

