      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0373.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0373.CPv  $
      *
      *   Rev 2.1   Jun 10 2005 09:48:32   t13ex44
      *APA00353: Sortierung der Datensätze im Provisionseingang
      *
      *   Rev 2.0   Feb 27 1999 14:28:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:24:14   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 24 1997 14:30:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0373                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE und PMPP >>                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    18.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.07.97!    ! NEU                       ! D.WILHELM*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0373                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0373.
          05 PI-DRPA0373-PROGRAMM            PIC  X(008).
          05 PI-DRPA0373-UFKT                PIC  9(002).
          05 PI-DRPA0373-SCHALTER            PIC  9.
          05 PI-DRPA0373-DATEN.
             10 PI-DRPA0373-PDTA-ID          PIC  S9(15)V USAGE COMP-3.
             10 PI-DRPA0373-PGEB-GES         PIC  S9(04) COMP.
             10 PI-DRPA0373-PMPP-PGV         PIC  S9(04) COMP.
      *
       01 PO-DRPA0373.
          05 PO-DRPA0373-RC                  PIC  X(003).
             88 PO-DRPA0373-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0373-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0373-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0373-DATEN.
             10 PO-DRPA0373-PMPG-ID           PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA0373-PMPG-VERSION      PIC S9(04) COMP.
