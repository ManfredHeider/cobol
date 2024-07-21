      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0957.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0957.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:41:14   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:18:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0957 FUNKTION LIST PVPT-DATEN            *
      *                          FüR PO24                            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!15.05.98! 01 ! NEU                       ! STORCH   *
      *         !21.12.98!1.02! PO-BEREICH ERWEITERT      !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0957                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0957.
             10 PI-DRPA0957-UFKT               PIC  9(02).
             10 PI-DRPA0957-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
      *
       01 PO-DRPA0957.
          05 PO-DRPA0957-RC                    PIC  X(003).
             88 PO-DRPA0957-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0957-RC-LEER                     VALUE 'LER'.
             88 PO-DRPA0957-RC-EOF                      VALUE 'EOF'.

          05 PO-DRPA0957-DATEN.
             10 PO-DRPA0957-PVPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PO-DRPA0957-PVPT-VERSION       PIC S9(004) COMP.
             10 PO-DRPA0957-PVPT-VT-PRIO       PIC S9(004) COMP.
             10 PO-DRPA0957-PVPT-LAUFZEIT      PIC S9(004) COMP.
             10 PO-DRPA0957-PVPT-PPFL-BEGINN   PIC  X(010).
             10 PO-DRPA0957-KNR-PVPT-ART       PIC S9(004) COMP.
             10 PO-DRPA0957-PVPT-EE-ENTSCHEID  PIC  X(001).

      *
