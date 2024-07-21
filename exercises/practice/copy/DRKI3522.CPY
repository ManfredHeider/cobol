      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3522.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3522.CPv  $
      *
      *   Rev 2.2   Jan 07 2019 15:29:20   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.1   Nov 28 2018 11:52:42   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.0   Nov 14 2018 16:30:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3522                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3522                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3522
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3522.
           05  PI-DRKI3522-KEY.

             10 PI-DRKI3522-KNDPERSONENID         PIC X(013).
             10 PI-DRKI3522-KNDSUCHVORNAME        PIC X(30).
             10 PI-DRKI3522-KNDSUCHNAME           PIC X(30).
             10 PI-DRKI3522-KNDMANDANTNR          PIC S9(9) COMP.
      *
       01  PO-DRKI3522.
           05  PO-DRKI3522-RC                     PIC  X(003).
               88 PO-DRKI3522-RC-OK               VALUE 'OK '.
               88 PO-DRKI3522-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3522-DATEN.
      *
            10 PO-DRKI3522-KNDANZBETREUER        PIC S9(4) COMP.
            10 PO-DRKI3522-KNDNUMMER             PIC S9(9) COMP.
            10 PO-DRKI3522-KNDSTATUSID           PIC X(01).
            10 PO-DRKI3522-KNDANREDEID           PIC S9(004) COMP.
            10 PO-DRKI3522-KNDANZNAME            PIC X(30).
            10 PO-DRKI3522-KNDANZVORNAME         PIC X(30).

      *
      ********** Ende Copy-Member DRKI3522 ****************************


