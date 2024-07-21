      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.09.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1878.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1878.CPv  $
      *
      *   Rev 2.2   Oct 18 2017 10:50:20   t13ae37
      *APA02498: DVAG-Wien: Einführung neue Gesellschaft Capital Ba
      *nk - PEG
      *
      *   Rev 2.1   Oct 18 2017 08:31:08   t13ae37
      *APA02498: DVAG-Wien: Einführung neue Gesellschaft Capital Ba
      *nk - PEG
      *
      *   Rev 2.0   22 Sep 2017 08:42:58   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1878 FUNKTION UWPA0046                   *
      *                 GEN.-INVEST FORMAL PRUEFEN UND NORMIEREN     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    11.03.2004                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02498!22.09.17!2.1 ! NEU                       ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1878                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1878.
             10 PI-DRPA1878-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1878-PEG-DATEN-CAPITAL.
                COPY DRPA1572.
             10 PI-DRPA1878-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1878.
          05 PO-DRPA1878-RC                 PIC  X(003).
             88 PO-DRPA1878-RC-OK                VALUE 'OK '.
             88 PO-DRPA1878-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
 

