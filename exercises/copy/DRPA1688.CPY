      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1688.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1688.CPv  $
      *
      *   Rev 2.1   Mar 19 2009 08:25:30   t13ex44
      *APA00868: Erweiterung Prov- und Rück-DTA Generali
      *
      *   Rev 2.0   Feb 03 2009 10:57:38   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1688  FUNKTION LESEN VERTRAGSKENNZEICHEN *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    03.02.2009                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00868!03.02.09! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1688                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN VERTRAGSKENNZEICHEN GENERALI  <<<              *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1688.
           05 PI-DRPA1688-PGEB-GES     PIC S9(004)       BINARY.
           05 PI-DRPA1688-VNR          PIC  X(015).
      *
        01 PO-DRPA1688.
           05 PO-DRPA1688-RC           PIC  X(003).
              88 PO-DRPA1688-RC-OK          VALUE 'OK '.
              88 PO-DRPA1688-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1688-DATEN.
              10 PO-DRPA1688-GHKZ      PIC  X(002).
