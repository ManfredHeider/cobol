      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1679.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1679.CPv  $
      *
      *   Rev 2.1   Aug 10 2009 10:35:26   t13ae46
      *FPA01082: Korrektur Umnummerierung / Ges.91
      *
      *   Rev 2.0   Oct 28 2008 10:37:26   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1679 FUNKTION VERTRAGSUMNUMMERIERUNG     *
      *                          GES. 91                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    23.04.2009                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA01082!23.04.09!2.01! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1679                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1679.
          05 PI-DRPA1679-DATEN.
             10 PI-DRPA1679-PGEB-GES             PIC S9(04) BINARY.
             10 PI-DRPA1679-PROD-PROD-SCHL       PIC S9(04) BINARY.
      *
       01 PO-DRPA1679.
          05 PO-DRPA1679-RC                      PIC   X(03).
             88 PO-DRPA1679-RC-OK                VALUE 'OK '.
             88 PO-DRPA1679-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRPA1679-DATEN.
             10 PO-DRPA1679-KNR-PRODUKTGEBER     PIC S9(04) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
