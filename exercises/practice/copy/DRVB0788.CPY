      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0788.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0788.CPv  $
      *
      *   Rev 2.1   Aug 17 2005 09:12:26   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Jul 08 2005 11:32:50   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0788 (ERSTELLT FÜR KAPITÄNS-CLUB)        *
      *    FUNKTION ERMITTELT FÜR EINEN BETREUER ALLE MITARBEITER    *
      *    AB EINER VORGEGEBENEN STUFE, MIT NETTO_GESAMT_UMSATZ      *
      *    DER ÜBER EINEM GELIEFERTEN WERT LIEGT.                    *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    08.07.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAD00009!08.07.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0788                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0788.
          05 PI-DRVB0788-PROGRAMM                PIC  X(008).
          05 PI-DRVB0788-UFKT-CODE               PIC  9(002).
          05 PI-DRVB0788-MA-ST-NR                PIC  S9(009)    COMP.
          05 PI-DRVB0788-ANZ-MONATE              PIC  S9(004)    COMP.
          05 PI-DRVB0788-ANZ-EINHEITEN           PIC  S9(009)V9  COMP-3.
          05 PI-DRVB0788-STUFE                   PIC  S9(004)    COMP.
      *
       01 PO-DRVB0788.
          05 PO-DRVB0788-RC                      PIC  X(003).
             88 PO-DRVB0788-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0788-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0788-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0788-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0788-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0788-DATEN.
             10 PO-DRVB0788-BETR-NR              PIC  S9(009)    COMP.
             10 PO-DRVB0788-UNTST-NR             PIC  S9(009)    COMP.
             10 PO-DRVB0788-STUFE                PIC  S9(004)    COMP.
             10 PO-DRVB0788-EINHEITEN            PIC  S9(009)V9  COMP-3.
             10 PO-DRVB0788-DAT-VON              PIC   X(010).
      *
      ******** Ende Copy-Member DRVB0257 *****************************
