      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1809.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1809.CPv  $
      *
      *   Rev 2.1   Jul 16 2015 15:06:26   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.0   Apr 01 2015 15:43:40   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1809  FUNKTION UPDATE STANDARD-OZP
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
      *    FÜR DIE FUNKTION DRPA1809                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *    >>>  UPDATE  <<<     TABELLE      TBPA0138 PABR            *
      *                                                               *
      *****************************************************************
       01 PI-DRPA1809.
          05 PI-DRPA1809-PROGRAMM               PIC  X(008).
          05 PI-DRPA1809-PABR-KEY.
             10 PI-DRPA1809-PGEB-GES            PIC S9(004) USAGE COMP.
             10 PI-DRPA1809-PDPT-ID             PIC S9(015) COMP-3.
             10 PI-DRPA1809-PABR-VBNR           PIC S9(009) BINARY.
      *
      *   GÜLTIGE FUNKTIONEN: 1=UPD.ABRECH-NR  ODER  2=UPD.PDPT-ID-KORR
          05 PI-DRPA1809-UFKT                   PIC  9(002).
      *
          05 PI-DRPA1809-PABR-DATEN.
             10 PI-DRPA1809-PABR-ABRECH-NR      PIC  X(015).
             10 PI-DRPA1809-PDPT-ID-KORR        PIC S9(015) COMP-3.
      *
       01 PO-DRPA1809.
          05 PO-DRPA1809-RC                     PIC  X(003).
             88 PO-DRPA1809-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1809-RC-NOT-FOUND             VALUE 'NFD'.
      *
      *****************************************************************

