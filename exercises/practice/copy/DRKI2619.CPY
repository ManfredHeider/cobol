      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2619.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2619.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 13:59:56   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Jun 07 2012 14:34:02   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2619                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2619                    *
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
      *    FÜR DIE FUNKTION DRKI2619
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2619.
           05 PI-DRKI2619-PROGRAMM                 PIC  X(008).
           05 PI-DRKI2619-UFKT-CODE                PIC  9(002).
      *
           05  PI-DRKI2619-KEY.
             10 PI-DRKI2619-KNDSUCHNAME            PIC X(030).
             10 PI-DRKI2619-KNDSUCHVORNAME         PIC X(030).
             10 PI-DRKI2619-KNDGEBGRUENDDAT        PIC X(010).
             10 PI-DRKI2619-KNDSTATUSID            PIC X(001).
             10 PI-DRKI2619-KNDMANDANTNR           PIC S9(009) COMP.
      *
       01  PO-DRKI2619.
           05  PO-DRKI2619-RC                      PIC  X(003).
               88 PO-DRKI2619-RC-OK                   VALUE 'OK '.
               88 PO-DRKI2619-RC-LEER                 VALUE 'LER'.
               88 PO-DRKI2619-RC-EOF                  VALUE 'EOF'.
      *
           05  PO-DRKI2619-DATEN.
             10 PO-DRKI2619-QUALITAET.
               49 PO-DRKI2619-QUALITAET-LEN        PIC S9(004) COMP.
               49 PO-DRKI2619-QUALITAET-TEXT       PIC  X(001).
             10 PO-DRKI2619-KNDPERSONENID          PIC  X(013).
             10 PO-DRKI2619-KNDNUMMER              PIC S9(009) COMP.
             10 PO-DRKI2619-KNDGEANDERTPGM         PIC  X(008).
      *
      ******* ENDE COPY-MEMBER ***************************************
