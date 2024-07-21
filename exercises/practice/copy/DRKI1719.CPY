      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1719.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1719.CPv  $
      *
      *   Rev 2.1   Dec 23 2004 09:24:22   t13ex12
      *FKI00874: Eintrag in RDTA-Tabelle unvollständig
      *
      *   Rev 2.0   Sep 22 2004 15:09:58   t13ex12
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1719 FUNKTION:RDTADB24-Tabelle           *
      *                      AUFLISTEN nach VB-ID                    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: von der Goltz                                   *
      *    DATUM:    21.09.04                                        *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00874!21.09.04! 01 !NEU                        !v.d.Goltz *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1719                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: Ermitteln, ob ein VB Einträge in Rücl-DTA-      *
      *              Tabelle für neue DeuBa-VBs hat.                 *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1719.
           05  PI-DRKI1719-PROGRAMM             PIC  X(008).
           05  PI-DRKI1719-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1719-DATEN.
             10 PI-DRKI1719-RDTADB24VBID        PIC  X(013).
       01 PO-DRKI1719.
          05 PO-DRKI1719-RC                     PIC  X(003).
             88 PO-DRKI1719-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1719-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1719-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1719-NI-TAB.
             10 PO-DRKI1719-NI-ELE PIC S9(04) COMP OCCURS  5 TIMES.
          05 PO-DRKI1719-NIS  REDEFINES  PO-DRKI1719-NI-TAB.
             10 PO-DRKI1719-NI-RDTADB24VBID        PIC  S9(004) COMP.
             10 PO-DRKI1719-NI-RDTADB24ERSTELL     PIC  S9(004) COMP.
             10 PO-DRKI1719-NI-RDTADB24HERKUNF     PIC  S9(004) COMP.
             10 PO-DRKI1719-NI-RDTADB24ERL-KZ      PIC  S9(004) COMP.
             10 PO-DRKI1719-NI-RDTADB24ERL-DAT     PIC  S9(004) COMP.
          05 PO-DRKI1719-DATEN.
             10 PO-DRKI1719-RDTADB24VBID           PIC  X(013).
             10 PO-DRKI1719-RDTADB24ERSTELLT       PIC  X(026).
             10 PO-DRKI1719-RDTADB24HERKUNFT       PIC  X(002).
             10 PO-DRKI1719-RDTADB24ERL-KZ         PIC  X(001).
             10 PO-DRKI1719-RDTADB24ERL-DAT        PIC  X(010).
      *
      ******** Ende Copy-Member **************************************

