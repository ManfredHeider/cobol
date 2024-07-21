      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2970.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2970.CPv  $
      *
      *   Rev 2.3   Mar 13 2018 08:25:24   t13ex27
      *AKI01572: FW-Gesellschaft DV(G90)
      *
      *   Rev 2.2   Sep 02 2015 15:10:58   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:46:58   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Jan 28 2015 11:57:20 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2970                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2970                    *
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
      *    FÜR DIE FUNKTION DRKI2970
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2970.
           05 PI-DRKI2970-PROGRAMM              PIC  X(008).
           05 PI-DRKI2970-UFKT-CODE             PIC  9(002).
                88 PO-DRKI2970-UFKT-FIRST               VALUE 1.
                88 PO-DRKI2970-UFKT-NEXT                VALUE 2.
                88 PO-DRKI2970-UFKT-QUIT                VALUE 3.
      *
           05  PI-DRKI2970-KEY.
                10 PI-DRKI2970-G90VMANDANT            PIC S9(9) COMP.
      *
       01  PO-DRKI2970.
           05  PO-DRKI2970-RC                   PIC  X(003).
                88 PO-DRKI2970-RC-OK                    VALUE 'OK '.
                88 PO-DRKI2970-RC-LEER                  VALUE 'LER'.
                88 PO-DRKI2970-RC-EOF                   VALUE 'EOF'.
      *
           05  PO-DRKI2970-DATEN.
                10 PO-DRKI2970-G90VVERTRAGSNR         PIC  X(0015).
                10 PO-DRKI2970-G90KVERTRAGSNR         PIC  X(0015).
                10 PO-DRKI2970-NI-G90KVERTRAGSNR      PIC S9(4) COMP.
                10 PO-DRKI2970-G90KAENDKZ             PIC  X(0002).
                10 PO-DRKI2970-G90KGESID              PIC S9(4) COMP.
                10 PO-DRKI2970-G90KRECHTSFORM         PIC S9(4) COMP.
                10 PO-DRKI2970-G90KANREDE             PIC S9(4) COMP.
                10 PO-DRKI2970-G90KVORNAME            PIC  X(0030).
                10 PO-DRKI2970-G90KNACHNAME           PIC  X(0030).
                10 PO-DRKI2970-G90KGRUENDDAT          PIC  X(010).
                10 PO-DRKI2970-G90KINHABER            PIC  X(0100).
                10 PO-DRKI2970-G90KSTRASSE            PIC  X(0040).
                10 PO-DRKI2970-G90KPLZ                PIC S9(9) COMP.
                10 PO-DRKI2970-G90KORT                PIC  X(0040).
                10 PO-DRKI2970-G90KPFPOSTFACH         PIC S9(9) COMP.
                10 PO-DRKI2970-G90KPFPLZ              PIC S9(9) COMP.
                10 PO-DRKI2970-G90KPFORT              PIC  X(0040).
                10 PO-DRKI2970-G90KTELNR              PIC  X(0030).
                10 PO-DRKI2970-G90KMOBILNR            PIC  X(0030).
                10 PO-DRKI2970-G90KFAXNR              PIC  X(0030).
                10 PO-DRKI2970-G90KAPTITEL            PIC S9(4) COMP.
                10 PO-DRKI2970-G90KAPANREDE           PIC S9(4) COMP.
                10 PO-DRKI2970-G90KAPVORNAME          PIC  X(0030).
                10 PO-DRKI2970-G90KAPNACHNAME         PIC  X(0030).
                10 PO-DRKI2970-G90KAPGEBDAT           PIC  X(010).
                10 PO-DRKI2970-G90KAPEMAIL            PIC  X(0128).
                10 PO-DRKI2970-G90KAPFUNKTION         PIC  X(0050).
      *
      ******* ENDE COPY-MEMBER ***************************************

