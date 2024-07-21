      * T13EX13   Stand: 20/01/97    16:16:36
      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  Januar 1997
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0218.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0218.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:53:20   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:11:00   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0218  Funktion PE - Struk ermitteln      *
      *                           mit Offset                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Giese                                           *
      *    DATUM:    10.02.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.01.97! 01 ! Neuerstellung             ! Giese    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0218                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0218.
          05 PI-DRPA0218-DATEN.
             10 PI-DRPA0218-VB-OFFSET           PIC S9(09) BINARY.
             10 PI-DRPA0218-POS-IN-STRUK        PIC S9(04) BINARY.
             10 PI-DRPA0218-PGEB-GES            PIC S9(04) BINARY.
             10 PI-DRPA0218-STICHTAG            PIC  X(10).

       01 PO-DRPA0218.
          05 PO-DRPA0218-RC                     PIC  X(003).
             88 PO-DRPA0218-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0218-RC-NOK                   VALUE 'NOK'.

          05 PO-DRPA0218-DATEN.
             10 PO-DRPA0218-PE-STRUKTUR OCCURS 20.
                15  PO-DRPA0218-PE-NAME         PIC X(20).
                15  PO-DRPA0218-EMP-TYP-BEZ     PIC X(10).
             10 PO-DRPA0218-VTAB-MAX            PIC  9(002).
             10 PO-DRPA0218-VSTAT-TABELLE.
                15 PO-DRPA0218-VSTAT-TAB        OCCURS 20
                                       INDEXED BY PO-DRPA0218-VTAB-IND.
                   20 PO-DRPA0218-VTAB-VSTAT    PIC  X(004).
                   20 PO-DRPA0218-VTAB-VSTAT-UNR PIC S9(004) BINARY.
                   20 PO-DRPA0218-VTAB-ORT      PIC  X(030).
                   20 PO-DRPA0218-VTAB-BEREICH  PIC  X(010).
             10 PO-DRPA0218-AVERM               PIC S9(009) BINARY.
             10 PO-DRPA0218-VM                  PIC S9(009) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
