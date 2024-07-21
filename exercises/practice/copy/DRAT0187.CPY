      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0187.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0187.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:20   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0187  Funktion Update Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.11.97! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0187                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<    NTZ     TBAT0020                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0187.
          05 PI-DRAT0187-LAST-UPDATE         PIC  X(026).
          05 PI-DRAT0187-NTZ-KEY.
             10 PI-DRAT0187-NTZ-ID           PIC S9(015) PACKED-DECIMAL.
          05 PI-DRAT0187-NTZ-DATEN.
             10 PI-DRAT0187-KNR-NOTIZ-ART      PIC S9(004) BINARY.
             10 PI-DRAT0187-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRAT0187-NTZ-PTR-ID       PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0187-NTZ-PTR-ST-NR    PIC S9(009) BINARY.
             10 PI-DRAT0187-NTZ-PTR-PIN      PIC S9(009) BINARY.
             10 PI-DRAT0187-NTZ-PTR-VNR      PIC  X(015).
      *
       01 PO-DRAT0187.
          05 PO-DRAT0187-RC                   PIC  X(003).
             88 PO-DRAT0187-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0187-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRAT0187-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************************************************************
