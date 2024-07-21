      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0192.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0192.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:40   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:22:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0192  Funktion Update Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRAT0192                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<    NTZZ    TBAT0021                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0192.
          05 PI-DRAT0192-LAST-UPDATE         PIC  X(026).
          05 PI-DRAT0192-NTZZ-KEY.
             10 PI-DRAT0192-NTZ-ID           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0192-NTZZ-ZEILEN-NR   PIC S9(004) BINARY.
          05 PI-DRAT0192-NTZZ-DATEN.
             10 PI-DRAT0192-NTZZ-ZEILE       PIC  X(080).
      *
       01 PO-DRAT0192.
          05 PO-DRAT0192-RC                   PIC  X(003).
             88 PO-DRAT0192-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0192-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRAT0192-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************************************************************
