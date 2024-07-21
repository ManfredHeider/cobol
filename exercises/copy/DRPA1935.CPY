      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  27.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1935.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1935.CPv  $
      *
      *   Rev 2.2   Sep 17 2019 07:34:38   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen 
      *
      *   Rev 2.1   Aug 27 2019 14:28:14   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen
      *
      *   Rev 2.0   Aug 27 2019 11:39:02   u130076
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1935  FUNKTION DELETE DTA-Sätze          *
      *    --------------------------------------------------------- *
      *    ERSTELLT: KÖPRÜCÜ                                         *
      *    DATUM:    27.08.2019                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02838!27.08.19! 01 ! NEUERSTELLUNG             ! KÖPRÜCÜ  *
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
      *    >>>  DELETE PDTA-ID <<<       TABELLE PDIF TBPA0104       *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1935.
          05 PI-DRPA1935-PDIF-GES             PIC S9(004) BINARY.
          05 PI-DRPA1935-PDIF-DTA-ID          PIC S9(015) COMP-3.
      *
       01 PO-DRPA1935.
          05 PO-DRPA1935-RC                   PIC  X(003).
             88 PO-DRPA1935-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1935-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1935-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1935-DEL-COUNT            PIC S9(004) COMP.
      *
      ****** ENDE COPY-MEMBER ****************************************
