      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1819.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1819.CPv  $
      *
      *   Rev 2.1   Jul 02 2015 10:40:22   t13ex44
      *APA02093: Einheiten bei Dynamik
      *
      *   Rev 2.0   Jun 05 2015 09:24:50   T13EX44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1819 FUNKTION PRÜFEN PRODUKT AUF NEUE    *
      *                          PGRP_ID                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    05.06.2015                                      *
      ****************************************************************
      *    VERSION   2.1                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02093!05.06.15! 2.1! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1819                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1819.
          05 PI-DRPA1819-DATEN.
             10 PI-DRPA1819-PGEB-GES             PIC S9(004) BINARY.
             10 PI-DRPA1819-PROD-PROD-SCHL       PIC S9(004) BINARY.
             10 PI-DRPA1819-STICHTAG             PIC  X(010).
      *
       01 PO-DRPA1819.
          05 PO-DRPA1819-RC                      PIC  X(003).
             88 PO-DRPA1819-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1819-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRPA1819-DATEN.
             10 PO-DRPA1819-KNR-EINH-BEI-DYN     PIC S9(004) BINARY.
      *
 

