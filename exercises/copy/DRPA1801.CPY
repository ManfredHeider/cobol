      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1801.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1801.CPv  $
      *
      *   Rev 2.1   Apr 02 2015 08:34:16   t13ex67
      *APA01986: EVS: Aufteilung Grund- und Mehrprämie
      *
      *   Rev 2.0   Jan 28 2015 11:54:44   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1801  FUNKTION                           *
      *                           JNP MIT ZAHLUNGSBONUS ERMITTELN    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHAEFER                                 *
      *    DATUM:    28.01.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA01986!28.01.15!2.01 !NEU                       ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1801                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1801.
          05 PI-DRPA1801-DATEN.
             10 PI-DRPA1801-PGEB-GES            PIC S9(004) BINARY.
             10 PI-DRPA1801-STICHTAG            PIC  X(010).
             10 PI-DRPA1801-KNR-ZAHLWEISE       PIC S9(004) BINARY.
             10 PI-DRPA1801-JAHRES-BEITRAG          PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1801.
          05 PO-DRPA1801-RC                     PIC  X(003).
             88 PO-DRPA1801-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1801-RC-NOT-FOUND             VALUE 'NFD'.
          05 PO-DRPA1801-DATEN.
             10 PO-DRPA1801-JAHRES-BEITRAG          PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1801-JNP-MIT-ZAHLBONUS       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      ******** ENDE COPY-MEMBER **************************************

