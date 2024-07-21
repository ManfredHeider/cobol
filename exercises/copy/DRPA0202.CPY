      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0202.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0202.CPv  $
      *
      *   Rev 2.0   Mar 10 1999 12:04:10   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 12:09:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0202  FUNKTION LIST-OZP FÜR TABELLE PEWA *
      *                           FUNKTION PEW-EXPORT                *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPAnnnn!30.01.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !10.02.97!    ! VSTAT Anpassung           ! Giese    *
      *         !23.04.97!    ! VSTAT Anpassung           ! Cecconi  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0202                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0202.
          05 PI-DRPA0202-PROGRAMM           PIC  X(008).
          05 PI-DRPA0202-UFKT-CODE          PIC  9(002).
          05 PI-DRPA0202-AUSWAHL-KRITERIEN.
             10 PI-DRPA0202-PGEB-GES        PIC S9(004) BINARY.
230497*   Nur Sätze mit VSTAT O000 selektieren
100297       10 PI-DRPA0202-PEWA-VSTAT1     PIC  X(004).
230497*   Folgende Zeile auskommentiert, da O101 nicht verwendet wird
100297*      10 PI-DRPA0202-PEWA-VSTAT2     PIC  X(004).
      *
       01 PO-DRPA0202.
          05 PO-DRPA0202-RC                 PIC  X(003).
             88 PO-DRPA0202-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0202-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0202-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0202-DATEN.
             10 PO-DRPA0202-PEWA-ID          PIC S9(15) PACKED-DECIMAL.
             10 PO-DRPA0202-PEWA-PIN         PIC S9(009) BINARY.
             10 PO-DRPA0202-PEWA-VNR         PIC  X(015).
             10 PO-DRPA0202-PEWA-KUNDE-NAME  PIC  X(025).
             10 PO-DRPA0202-PEWA-KUNDE-VNAME PIC  X(020).
             10 PO-DRPA0202-PEWA-AVERM       PIC S9(009) BINARY.
             10 PO-DRPA0202-PEWA-BEGINN      PIC  X(010).
             10 PO-DRPA0202-KNR-ZAHLWEISE    PIC S9(004) BINARY.
100297       10 PO-DRPA0202-PEWA-VSTAT       PIC  X(004).
             10 PO-DRPA0202-PEWA-VSTAT-UNR   PIC S9(004) BINARY.
             10 PO-DRPA0202-PEWA-AN-ARB-DAT  PIC  X(010).
 ###         10 PO-DRPA0202-PEWA-PG-MELD-DAT PIC  X(010).
 ###         10 PO-DRPA0202-PDTA-ID          PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA0202-PEWA-LAST-UPDATE PIC  X(026).
      *
      ***** Ende-Copy-Member *****************************************
