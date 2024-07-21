      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1790.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1790.CPv  $
      *
      *   Rev 2.1   Mar 26 2014 10:55:30   t13ae39
      *APA01766: GESAMTABRECHNUNGSPROTOKOLL DAISY: zus. Kontrolliste
      *
      *   Rev 2.0   Mar 13 2014 15:01:04   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRPA1790  FUNKTION INSERT STANDARD-OZP
      *    -------------------------------------------------------
      ******************************************************************
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA01766!13.03.14!2.0 ! NEUERSTELLUNG             ! STRAUSS   *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRPA1790                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *    >>>  SELECT ALL <<<    TABELLE      TBPA0034 KOSU          *
      *                                                               *
      *****************************************************************
       01 PI-DRPA1790.
          05 PI-DRPA1790-PROGRAMM               PIC  X(008).
          05 PI-DRPA1790-UFKT                   PIC  9(002).
          05 PI-DRPA1790-DATEN.
             10 PI-DRPA1790-KNR-MANDANT         PIC S9(004) BINARY.
      *      10 PI-DRPA1790-KNR-VGES            PIC S9(004) BINARY.
             10 PI-DRPA1790-PGEB-GES            PIC S9(004) BINARY.
      *
       01 PO-DRPA1790.
          05 PO-DRPA1790-RC                     PIC  X(003).
             88 PO-DRPA1790-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1790-RC-LEER                  VALUE 'LER'.
             88 PO-DRPA1790-RC-EOF                   VALUE 'EOF'.
          05 PO-DRPA1790-KOSU-DATEN.
             10 PO-DRPA1790-KNR-MANDANT         PIC S9(004) BINARY.
             10 PO-DRPA1790-PGEB-GES            PIC S9(004) BINARY.
             10 PO-DRPA1790-KOSU-PROVISION      PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1790-KOSU-AUSZAHLUNG     PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1790-KOSU-RUECKSTELLUNG  PIC S9(009)V9(2) COMP-3.
             10 PO-DRPA1790-KOSU-LAST-UPDATE    PIC X(026).
             10 PO-DRPA1790-KOSU-KENNUNG        PIC X(008).
      ******* ENDE COPY-MEMBER ****************************************
