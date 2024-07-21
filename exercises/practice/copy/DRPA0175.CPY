      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0175.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0175.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:03:34   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0175  Funktion "PEW-Struktur erg./prüfen"*
      *    -------------------------------------------------------   *
      *    ERSTELLT: Wolff                                           *
      *    DATUM:    23.01.97                                        *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !23.01.97! 01 ! Neuerstellung             ! Wolff    *
      * PEW-Rel.!        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0175                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0175.
          05 PI-DRPA0175-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA0175-DATEN.
             10 PI-DRPA0175-KNR-VERTRIEB-GES    PIC S9(004) BINARY.
             10 PI-DRPA0175-PGEB-GES            PIC S9(004) BINARY.
             10 PI-DRPA0175-STICHTAG            PIC  X(010).
             10 PI-DRPA0175-PEWA-STRUK-DAT      PIC  X(010).
             10 PI-DRPA0175-STRUK-MAN-AEND-KEZ  PIC  X(001).
      *
       01 PO-DRPA0175.
          05 PO-DRPA0175-RC                     PIC  X(003).
             88 PO-DRPA0175-RC-OK                    VALUE 'OK '.
          05 PO-DRPA0175-DATEN.
             10 PO-DRPA0175-VTAB-MAX            PIC  9(002).
             10 PO-DRPA0175-VSTAT-TABELLE.
                15 PO-DRPA0175-VSTAT-TAB        OCCURS 20
                                       INDEXED BY PO-DRPA0175-VTAB-IND.
                   20 PO-DRPA0175-VTAB-VSTAT    PIC  X(004).
                   20 PO-DRPA0175-VTAB-VSTAT-UNR PIC S9(004) BINARY.
                   20 PO-DRPA0175-VTAB-ORT      PIC  X(030).
                   20 PO-DRPA0175-VTAB-BEREICH  PIC  X(010).
             10 PO-DRPA0175-AVERM               PIC S9(009) BINARY.
             10 PO-DRPA0175-VM                  PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************
