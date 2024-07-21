      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  14.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3521.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3521.CPv  $
      *
      *   Rev 2.2   Jan 07 2019 15:29:08   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.1   Nov 28 2018 11:52:28   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.0   Nov 14 2018 16:30:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3521                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3521                    *
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
      *    FÜR DIE FUNKTION DRKI3521
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3521.
           05  PI-DRKI3521-KEY.

             10 PI-DRKI3521-KNDPERSONENID         PIC X(013).
             10 PI-DRKI3521-KNDSUCHVORNAME        PIC X(30).
             10 PI-DRKI3521-KNDSUCHNAME           PIC X(30).
             10 PI-DRKI3521-KNDGEBGRUENDDAT       PIC X(010).
             10 PI-DRKI3521-KNDMANDANTNR          PIC S9(9) COMP.
      *
       01  PO-DRKI3521.
           05  PO-DRKI3521-RC                     PIC  X(003).
               88 PO-DRKI3521-RC-OK               VALUE 'OK '.
               88 PO-DRKI3521-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3521-DATEN.
      *
             10 PO-DRKI3521-KNDANZBETREUER        PIC S9(4) COMP.
             10 PO-DRKI3521-KNDSTATUSID           PIC X(01).
             10 PO-DRKI3521-KNDANREDEID           PIC S9(004) COMP.
             10 PO-DRKI3521-KNDANZNAME            PIC X(30).
             10 PO-DRKI3521-KNDANZVORNAME         PIC X(30).
             10 PO-DRKI3521-KNDSUCHVORNAME        PIC X(30).
             10 PO-DRKI3521-KNDSUCHNAME           PIC X(30).
      *
      ********** Ende Copy-Member DRKI3521 ****************************


