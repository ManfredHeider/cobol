      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1158.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1158.CPv  $
      *
      *   Rev 1.1   Nov 26 2001 16:32:24   t13ex21
      *FPA00293 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:01:54   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1158                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF BEREITSTELLEN UMBUCHUNGEN FÜR KORREKTUR    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    24.10.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00293!24.10.01!    ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1158                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1158.
          05 PI-DRPA1158-PROGRAMM            PIC  X(08).
          05 PI-DRPA1158-UFKT                PIC  9(02).
          05 PI-DRPA1158-SCHALTER            PIC  9.
          05 PI-DRPA1158-AUSWAHLKRITERIEN.
             10 PI-DRPA1158-PGEB-GES         PIC S9(04) USAGE COMP.
             10 PI-DRPA1158-BUCHUNGSART      PIC S9(04) USAGE COMP.
             10 PI-DRPA1158-KENNUNG          PIC X(08).
             10 PI-DRPA1158-ZEITRAUM-VON     PIC X(10).
             10 PI-DRPA1158-ZEITRAUM-BIS     PIC X(10).
      *
       01 PO-DRPA1158.
          05 PO-DRPA1158-RC                  PIC X(03).
             88 PO-DRPA1158-RC-OK                VALUE 'OK '.
             88 PO-DRPA1158-RC-EOF               VALUE 'EOF'.
             88 PO-DRPA1158-RC-LEER              VALUE 'LER'.
          05 PO-DRPA1158-ANZAHL-ERG-ZEILEN   PIC  9(05).

          05 PO-DRPA1158-ERGEBNISZEILE.
               10 PO-DRPA1158-PGEB-GES       PIC S9(04) USAGE COMP.
               10 PO-DRPA1158-PRINTE         PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA1158-PDPT-ID        PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA1158-PMPT-ID        PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA1158-PMPT-VERSION   PIC S9(04) COMP.
               10 PO-DRPA1158-PVER-VNR       PIC X(15).
               10 PO-DRPA1158-PVER-AKT-PIN   PIC 9(07).
      *
      ******* ENDE COPY-MEMBER **************************************
