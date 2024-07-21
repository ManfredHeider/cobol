      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0193.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0193.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:44   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:22:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0193  Funktion Delete Standard-OZP       *
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
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0193                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<  NTZZ TBAT0021                           *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0193.
          05 PI-DRAT0193-LAST-UPDATE        PIC  X(026).
          05 PI-DRAT0193-NTZZ-KEY.
             10 PI-DRAT0193-NTZ-ID          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0193-NTZZ-ZEILEN-NR  PIC S9(004) BINARY.
      *
       01 PO-DRAT0193.
          05 PO-DRAT0193-RC                 PIC  X(003).
             88 PO-DRAT0193-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0193-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRAT0193-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
