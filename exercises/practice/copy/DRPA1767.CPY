      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1767.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1767.CPv  $
      *
      *   Rev 2.2   Mar 10 2014 16:51:20   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Dec 11 2013 15:36:18   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell -
      *Tabelle TB
      *PA0033 PRKL -
      *
      *   Rev 2.0   Oct 01 2013 10:29:28   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRPA1767  FUNKTION DELETE STANDARD-OZP
      *    -------------------------------------------------------
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * APA01539!01.10.13! 2.1! NEUERSTELLUNG             ! RAHNER
      *         !        !    !                           !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1767
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE <<<        PRKL TBPA0033
      *
      ******************************************************************
      *
       01 PI-DRPA1767.
          05 PI-DRPA1767-PRKL-KEY.
             10 PI-DRPA1767-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1767-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PI-DRPA1767-PRKL-TARSCHL-GRP   PIC S9(009) BINARY.
             10 PI-DRPA1767-PRKL-TARSCHL       PIC S9(009) BINARY.
             10 PI-DRPA1767-KNR-TARIF-KZ       PIC S9(004) BINARY.
             10 PI-DRPA1767-PRKL-LZ-VON        PIC S9(004) BINARY.
             10 PI-DRPA1767-PRKL-GUELTIG-AB    PIC  X(010).
             10 PI-DRPA1767-PRKL-LAST-UPDATE   PIC  X(026).
      *
       01 PO-DRPA1767.
          05 PO-DRPA1767-RC                    PIC  X(003).
             88 PO-DRPA1767-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1767-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRPA1767-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ******************************************

