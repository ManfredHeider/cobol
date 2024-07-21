      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  16.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1940.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1940.CPv  $
      *
      *   Rev 2.1   Oct 23 2019 09:56:34   t13ae37
      *APA02885: Ges. 16 - Einführung 4 neue PGVs 
      *
      *   Rev 2.0   Oct 16 2019 15:51:56   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1940  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BUCH                                            *
      *    DATUM:    12.04.2010                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01011!12.04.10! 2.0! NEUERSTELLUNG             ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1940                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LIST ALL -> TERMIN <<<                               *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1940.
          05 PI-DRPA1940-PROGRAMM           PIC  X(008).
          05 PI-DRPA1940-UFKT               PIC  9(002).
          05 PI-DRPA1940-PRINTE             PIC S9(015) PACKED-DECIMAL.
          05 PI-DRPA1940-ERST-PRODUKTE.
              10 PI-DRPA1940-ERST-PROD
                                       PIC S9(004)      BINARY
                                       OCCURS 10  INDEXED BY IX-EPS.
      *
       01 PO-DRPA1940.
          05 PO-DRPA1940-RC                 PIC  X(003).
             88 PO-DRPA1940-RC-OK           VALUE 'OK '.
             88 PO-DRPA1940-RC-NFD          VALUE 'NFD'.
 
          05 PO-DRPA1940-DATEN.
             10 PO-DRPA1940-PVPT-PPFL-BEGINN
                                           PIC X(10).
 
      *---------------------------------------------------------------*

