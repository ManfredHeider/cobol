      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1766.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1766.CPv  $
      *
      *   Rev 2.3   Apr 07 2016 10:14:12   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.2   Mar 10 2014 16:51:12   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Dec 11 2013 15:36:12   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell -
      *Tabelle TB
      *PA0033 PRKL -
      *
      *   Rev 2.0   Oct 01 2013 10:29:22   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1766  FUNKTION SELECT ALL STANDARD-OZP
      *    -------------------------------------------------------
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * APA01539!01.10.13! 2.1! NEUERSTELLUNG             ! RAHNER
      * APA02220!10.03.16! 2.3! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1766
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ALL <<<        PGVZ TBPA0033
      *
      ******************************************************************
      *
       01 PI-DRPA1766.
          05 PI-DRPA1766-PRKL-KEY.
             10 PI-DRPA1766-PGEB-GES          PIC S9(004) BINARY.
             10 PI-DRPA1766-PROD-PROD-SCHL    PIC S9(004) BINARY.
             10 PI-DRPA1766-PRKL-TARSCHL-GRP  PIC S9(009) BINARY.
             10 PI-DRPA1766-PRKL-TARSCHL      PIC S9(009) BINARY.
             10 PI-DRPA1766-KNR-TARIF-KZ      PIC S9(004) BINARY.
             10 PI-DRPA1766-PRKL-LZ-VON       PIC S9(004) BINARY.
             10 PI-DRPA1766-PRKL-GUELTIG-AB   PIC  X(010).
      *
          05 P0-DRPA1766-PRKL-DATEN.
             10 PO-DRPA1766-PRKL-LZ-BIS
                                              PIC S9(004) BINARY.
             10 PO-DRPA1766-PRKL-GUELTIG-BIS
                                              PIC X(010).
             10 PO-DRPA1766-PRKL-KOND-NR
                                              PIC S9(004) BINARY.
             10 PO-DRPA1766-PRKL-GRUND-PROV-MA
100316*                                       PIC S9(003)V9(003) COMP-3.
100316                                        PIC S9(004)V9(004) COMP-3.
             10 PO-DRPA1766-PRKL-PROV-SATZ-VWF
100316*                                       PIC S9(003)V9(003) COMP-3.
100316                                        PIC S9(004)V9(004) COMP-3.
             10 PO-DRPA1766-PRKL-PROV-SATZ-VTF
100316*                                       PIC S9(003)V9(003) COMP-3.
100316                                        PIC S9(004)V9(004) COMP-3.
             10 PO-DRPA1766-PRKL-KUERZUNG-ART
                                              PIC X(001).
             10 PO-DRPA1766-PRKL-KUERZUNG-AUF
                                              PIC S9(004) BINARY.
             10 PO-DRPA1766-KNR-PVT-KZ
                                              PIC S9(004) BINARY.
             10 PO-DRPA1766-PRKL-PROV-MIN
                                              PIC S9(009) BINARY.
             10 PO-DRPA1766-PRKL-PROV-MAX
                                              PIC S9(009) BINARY.
             10 PO-DRPA1766-KNR-KOND-REGEL-GRP
                                              PIC S9(004) BINARY.
             10 PO-DRPA1766-KNR-KOND-REGEL-GSP
                                              PIC S9(004) BINARY.
             10 PO-DRPA1766-PRKL-KENNUNG
                                              PIC  X(008).
             10 PO-DRPA1766-PRKL-LAST-UPDATE
                                              PIC  X(026).
      *
       01 PO-DRPA1766.
          05 PO-DRPA1766-RC                   PIC  X(003).
             88 PO-DRPA1766-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1766-RC-NOT-FOUND           VALUE 'NFD'.


