      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1827.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1827.CPv  $
      *
      *   Rev 2.1   Oct 09 2015 10:50:32   T13EX67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Datenm
      *odellanpassungen - MSS
      *
      *   Rev 2.0   Jul 13 2015 14:46:18   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1827  UPDATE-FUNKTION FÜR STANDARD-OZP   *
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
      *    FÜR DIE FUNKTION DRPA1827                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PASU TBPA0035 PASU         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1827.
          05 PI-DRPA1827-UFKT                   PIC  X(002).
          05 PI-DRPA1827-PASU-DATEN.
            10 PI-DRPA1827-PASU-KEY.
               15 PI-DRPA1827-PASU-VBNR         PIC S9(009) COMP.
               15 PI-DRPA1827-PASU-LFDNR        PIC S9(004) COMP.
               15 PI-DRPA1827-KNR-PASU-ZUSTAND  PIC S9(004) COMP.
      *
            10 PI-DRPA1827-PASU-ZIEL-STATUS     PIC S9(004) COMP.
            10 PI-DRPA1827-PASU-JAHR            PIC S9(004) COMP.
            10 PI-DRPA1827-PASU-AUFGABE-TS      PIC  X(026).
            10 PI-DRPA1827-PASU-ZUSTAND-TS      PIC  X(026).
      *
       01 PO-DRPA1827.
          05 PO-DRPA1827-RC                     PIC  X(003).
             88 PO-DRPA1827-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1827-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

