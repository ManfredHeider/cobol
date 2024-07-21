      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3285.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3285.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:29:46   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 21 2018 18:04:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI3285                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3285                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01572!12.02.18!2.3 ! Anpassung für FW/neuer MDW! IFR      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3285
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3285.
           05 PI-DRKI3285-PROGRAMM              PIC  X(008).
           05 PI-DRKI3285-UFKT-CODE             PIC  9(002).
                88 PI-DRKI3285-UFKT-FIRST               VALUE 1.
                88 PI-DRKI3285-UFKT-NEXT                VALUE 2.
                88 PI-DRKI3285-UFKT-QUIT                VALUE 3.
      *
      *    05  PI-DRKI3285-KEY.
      *         10 PI-DRKI3285-G90VMANDANT            PIC S9(9) COMP.
      *
       01  PO-DRKI3285.
           05  PO-DRKI3285-RC                   PIC  X(003).
                88 PO-DRKI3285-RC-OK                    VALUE 'OK '.
                88 PO-DRKI3285-RC-LEER                  VALUE 'LER'.
                88 PO-DRKI3285-RC-EOF                   VALUE 'EOF'.
      *
           05  PO-DRKI3285-DATEN.
                10 PO-DRKI3285-GENVGESELLSCHID    PIC X(02).
                10 PO-DRKI3285-GENVVNR            PIC X(15).
                10 PO-DRKI3285-GENVEZPNR          PIC S9(4) USAGE COMP.

                10 PO-DRKI3285-GENKGESELLSCHID    PIC X(02).
                10 PO-DRKI3285-GENKVNR            PIC X(15).
                10 PO-DRKI3285-GENKEZPNR          PIC S9(4) USAGE COMP.
                10 PO-DRKI3285-GENKROLLE          PIC X(02).
                10 PO-DRKI3285-NI-GENKROLLE       PIC S9(4) USAGE COMP.
                10 PO-DRKI3285-GENKLFDNR          PIC S9(4) USAGE COMP.
                10 PO-DRKI3285-NI-GENKLFDNR       PIC S9(4) USAGE COMP.
                10 PO-DRKI3285-GENKVERARB-KZN     PIC X(01).
                10 PO-DRKI3285-GENKNACHNAME       PIC X(40).
                10 PO-DRKI3285-GENKNACHNAME2      PIC X(40).
                10 PO-DRKI3285-GENKVORNAME        PIC X(30).
                10 PO-DRKI3285-GENKGEBDAT         PIC X(08).
                10 PO-DRKI3285-GENKANREDE         PIC X(02).
                10 PO-DRKI3285-GENKGESCHLECHT     PIC X(01).
                10 PO-DRKI3285-GENKSTRASSE        PIC X(45).
                10 PO-DRKI3285-GENKPLZ            PIC X(15).
                10 PO-DRKI3285-GENKORT            PIC X(30).
                10 PO-DRKI3285-GENKAUSL-KZN       PIC X(03).
      *
      ******* ENDE COPY-MEMBER ***************************************

