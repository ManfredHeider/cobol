      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1808.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1808.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:06:18   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Apr 01 2015 15:43:34   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1808  FUNKTION INSERT STANDARD-OZP
      *    -------------------------------------------------------
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA01932!01.04.15! 2.0! NEUERSTELLUNG             ! STRAUSS   *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRPA1808                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *    >>>  INSERT  <<<       TABELLE      TBPA0138 PABR          *
      *                                                               *
      *****************************************************************
       01 PI-DRPA1808.
          05 PI-DRPA1808-PROGRAMM               PIC  X(008).
          05 PI-DRPA1808-UFKT                   PIC  X(002).
          05 PI-DRPA1808-PABR-DATEN.
             10 PI-DRPA1808-PGEB-GES            PIC S9(004) USAGE COMP.
             10 PI-DRPA1808-PDPT-ID             PIC S9(015) COMP-3.
             10 PI-DRPA1808-PABR-VBNR           PIC S9(009) BINARY.
             10 PI-DRPA1808-PABR-ABRECH-NR      PIC  X(015).
             10 PI-DRPA1808-PDPT-ID-KORR        PIC S9(015) COMP-3.
      *
       01 PO-DRPA1808.
          05 PO-DRPA1808-RC                     PIC  X(003).
             88 PO-DRPA1808-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1808-RC-DUPLICATE-VALUE       VALUE 'DVA'.
      *
      *****************************************************************

