      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1829.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1829.CPv  $
      *
      *   Rev 2.1   Oct 09 2015 10:50:56   T13EX67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.0   Jul 13 2015 14:46:36   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1829  DELETE-FUNKTION FÜR STANDARD-OZP   *
      *                           PASU TBPA0035                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M. STORCH-SCHÄFER                               *
      *    DATUM:    13.07.2015                                      *
      ****************************************************************
      *    VERSION   2.01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!13.07.15!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1829                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PASU TBPA0035 PASU         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1829.
          05 PI-DRPA1829-UFKT                   PIC  X(002).
          05 PI-DRPA1829-PASU-DATEN.
            10 PI-DRPA1829-PASU-KEY.
               15 PI-DRPA1829-PASU-VBNR         PIC S9(009) COMP.
               15 PI-DRPA1829-PASU-LFDNR        PIC S9(004) COMP.
               15 PI-DRPA1829-KNR-PASU-ZUSTAND  PIC S9(004) COMP.
            10 PI-DRPA1829-PASU-ZUSTAND-TS      PIC  X(026).
      *
       01 PO-DRPA1829.
          05 PO-DRPA1829-RC                     PIC  X(003).
             88 PO-DRPA1829-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1829-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1829-RC-NOT-FOUND                  VALUE 'NFD'.
      *

