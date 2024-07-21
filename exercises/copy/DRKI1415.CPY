      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1415.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1415.CPv  $
      *
      *   Rev 2.1   Aug 05 2002 12:44:48   t13ex48
      *AKI00274
      *RUECK-DTA fuer DB24
      *
      *   Rev 2.0   Apr 23 2002 16:01:36   p13sp16
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRKI1415 FUNKTION:RDTADB24-Tabelle           *
      *                                   AUFLISTEN                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Sebbach                                         *
      *    DATUM:    07.05.02                                        *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!07.05.02! 01 !NEU                        !Sebbach   *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1415                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ADRESSAENDERUNGSDATEN ERMITTELN                 *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1415.
           05  PI-DRKI1415-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1415-DATEN.
             10 PI-DRKI1415-ERL-KZ              PIC  X(001).
       01 PO-DRKI1415.
          05 PO-DRKI1415-RC                     PIC  X(003).
             88 PO-DRKI1415-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1415-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1415-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1415-NI-TAB.
             10 PO-DRKI1415-NI-ELE PIC S9(04) COMP OCCURS  5 TIMES.
          05 PO-DRKI1415-NIS  REDEFINES  PO-DRKI1415-NI-TAB.
             10 PO-DRKI1415-NI-RDTADB24VBID        PIC  S9(004) COMP.
             10 PO-DRKI1415-NI-RDTADB24ERSTELL     PIC  S9(004) COMP.
             10 PO-DRKI1415-NI-RDTADB24HERKUNF     PIC  S9(004) COMP.
             10 PO-DRKI1415-NI-RDTADB24ERL-KZ      PIC  S9(004) COMP.
             10 PO-DRKI1415-NI-RDTADB24ERL-DAT     PIC  S9(004) COMP.
          05 PO-DRKI1415-DATEN.
             10 PO-DRKI1415-RDTADB24VBID           PIC  X(013).
             10 PO-DRKI1415-RDTADB24ERSTELLT       PIC  X(026).
             10 PO-DRKI1415-RDTADB24HERKUNFT       PIC  X(002).
             10 PO-DRKI1415-RDTADB24ERL-KZ         PIC  X(001).
             10 PO-DRKI1415-RDTADB24ERL-DAT        PIC  X(010).
      *
      ******** Ende Copy-Member **************************************

