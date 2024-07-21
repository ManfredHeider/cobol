      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1789.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1789.CPv  $
      *
      *   Rev 2.2   Jul 16 2015 15:06:56   t13ae39
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Source
      *n und Copies ASR
      *
      *   Rev 2.1   Mar 26 2014 10:55:24   t13ae39
      *APA01766: GESAMTABRECHNUNGSPROTOKOLL DAISY: zus. Kontrolliste
      *
      *   Rev 2.0   Mar 13 2014 15:00:56   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1789  FUNKTION INSERT STANDARD-OZP
      *    -------------------------------------------------------
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA01766!13.03.14!2.0 ! NEUERSTELLUNG             ! STRAUSS   *
      * APA01932!24.04.15!2.1 ! LUCY: UST FÜR GES. 90 DV  ! STRAUSS   *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRPA1789                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *    >>>  INSERT  <<<       TABELLE      TBPA0034 KOSU          *
      *                                                               *
      *****************************************************************
       01 PI-DRPA1789.
          05 PI-DRPA1789-UFKT                   PIC  9(002).
          05 PI-DRPA1789-KOSU-DATEN.
             10 PI-DRPA1789-KNR-MANDANT         PIC S9(004) BINARY.
             10 PI-DRPA1789-KNR-VGES            PIC S9(004) BINARY.
             10 PI-DRPA1789-PGEB-GES            PIC S9(004) BINARY.
             10 PI-DRPA1789-KOSU-PROVISION      PIC S9(009)V9(2) COMP-3.
             10 PI-DRPA1789-KOSU-AUSZAHLUNG     PIC S9(009)V9(2) COMP-3.
             10 PI-DRPA1789-KOSU-RUECKSTELLUNG  PIC S9(009)V9(2) COMP-3.
             10 PI-DRPA1789-KOSU-LAST-UPDATE    PIC X(026).
             10 PI-DRPA1789-KOSU-KENNUNG        PIC X(008).
240415       10 PI-DRPA1789-KOSU-UST-GESAMT     PIC S9(009)V9(2) COMP-3.
240415       10 PI-DRPA1789-KOSU-UST-AUSZAHL    PIC S9(009)V9(2) COMP-3.
      *
       01 PO-DRPA1789.
          05 PO-DRPA1789-RC                    PIC  X(003).
             88 PO-DRPA1789-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1789-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      ******* ENDE COPY-MEMBER ****************************************

