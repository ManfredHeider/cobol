      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2553.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2553.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:59:18   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Dec 13 2011 11:24:42   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2553                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2553                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!14.12.11!2.1 ! neu                       !I. Frenzel*
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2553
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI2553.
          05 PI-DRKI2553-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2553-UFKT-CODE                PIC  9(002).
          05 PI-DRKI2553-DATEN.
            07 PI-DRKI2553-SUCHNAME               PIC X(030).
            07 PI-DRKI2553-SUCHVORNAME            PIC X(030).
            07 PI-DRKI2553-GEBGRUENDDAT           PIC X(010).
      *
       01 PO-DRKI2553.

          05 PO-DRKI2553-RC                       PIC  X(003).
             88 PO-DRKI2553-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2553-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2553-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI2553-DATEN.
            10 PO-DRKI2553-KNDPERSONENID         PIC X(013).
            10 PO-DRKI2553-KNDGEBGRUENDDAT       PIC X(010).

            10 PO-DRKI2553-KNDANZBETREUER        PIC S9(4) COMP.
            10 PO-DRKI2553-KNDNUMMER             PIC S9(9) COMP.
            10 PO-DRKI2553-KNDSTATUSID           PIC X(01).
      *
      ********** Ende Copy-Member DRKI2553 ****************************
