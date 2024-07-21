      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1874.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1874.CPv  $
      *
      *   Rev 2.1   May 18 2017 15:49:48   t13ae46
      *APA02438: Devolo: PEG-Copy JWA 
      *
      *   Rev 2.0   May 11 2017 09:05:12   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1874                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0035                        *
      *        FUNKTION: PRODART ABLEITEN AUS PGV, NUR FP-PROVISIONEN *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WANG                                             *
      *    DATUM:    11.05.2017                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02438!11.05.17!2.00! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1874                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1874.
          05 PI-DRPA1874-ZVP-VERSION        PIC S9(004) BINARY.
          05 PI-DRPA1874-PGV                PIC S9(004) BINARY.
      *
       01 PO-DRPA1874.
          05 PO-DRPA1874-RC                 PIC   X(003).
             88 PO-DRPA1874-RC-OK              VALUE  'OK '.
          05 PO-DRPA1874-PRODART            PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


