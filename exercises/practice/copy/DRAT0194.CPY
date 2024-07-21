      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0194.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0194.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:48   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:22:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0194  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.11.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.11.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0194                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    TBAT0021  NTZZ                     *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0194.
          05 PI-DRAT0194-NTZZ-KEY.
             10 PI-DRAT0194-NTZ-ID         PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0194-NTZZ-ZEILEN-NR PIC S9(004) BINARY.
      *
       01 PO-DRAT0194.
          05 PO-DRAT0194-RC                PIC  X(003).
             88 PO-DRAT0194-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0194-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRAT0194-NTZZ-DATEN.
             10 PO-DRAT0194-NTZZ-ZEILE     PIC  X(080).
          05 PO-DRAT0194-NTZZ-LAST-UPDATE  PIC  X(026).
          05 PO-DRAT0194-NTZZ-KENNUNG      PIC  X(008).
      *
      ****** Ende Copy-Member *****************************************
