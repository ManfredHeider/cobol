      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1936.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1936.CPv  $
      *
      *   Rev 2.1   Sep 17 2019 07:35:04   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen 
      *
      *   Rev 2.0   Aug 30 2019 12:58:18   u130076
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1936  FUNKTION INSERT DTA-Sätze          *
      *    --------------------------------------------------------- *
      *    ERSTELLT: KÖPRÜCÜ                                         *
      *    DATUM:    30.08.2019                                      *
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02838!30.08.19! 01 ! NEUERSTELLUNG             ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1935                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT PDTA-ID <<<       TABELLE PDIF TBPA0104       *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1936.
          05 PI-DRPA1936-PDIF-GES             PIC S9(004) BINARY.
          05 PI-DRPA1936-PDIF-DTA-ID          PIC S9(015) COMP-3.
      *
       01 PO-DRPA1936.
          05 PO-DRPA1936-RC                   PIC  X(003).
             88 PO-DRPA1936-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1936-RC-DUPLICATE-VALUE            VALUE 'DVA'.
          05 PO-DRPA1936-INS-COUNT            PIC  9(004) COMP-3.
      *
      *
      ****** ENDE COPY-MEMBER ****************************************

