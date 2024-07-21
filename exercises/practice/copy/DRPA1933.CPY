      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1933.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1933.CPv  $
      *
      *   Rev 2.2   Sep 17 2019 07:33:46   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen 
      *
      *   Rev 2.1   Aug 27 2019 14:26:34   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen
      *
      *   Rev 2.0   Aug 26 2019 16:25:44   u130076
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1933  FUNKTION UPDATE PMPP-PGV           *
      *    --------------------------------------------------------- *
      *    ERSTELLT: KÖPRÜCÜ                                         *
      *    DATUM:    26.08.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02838!26.08.19! 01 ! NEUERSTELLUNG             ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1933                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE PMPP-PGV <<<       TABELLE PMPP TBPA0103      *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1933.
          05 PI-DRPA1933-PMPP-GES             PIC S9(004) BINARY.
          05 PI-DRPA1933-PMPP-ID              PIC S9(015) COMP-3.
          05 PI-DRPA1933-PMPP-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA1933-PMPP-KENNUNG         PIC  X(008).
          05 PI-DRPA1933-PMPP-PGV             PIC S9(004) BINARY.
      *
       01 PO-DRPA1933.
          05 PO-DRPA1933-RC                   PIC  X(003).
             88 PO-DRPA1933-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1933-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1933-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************



