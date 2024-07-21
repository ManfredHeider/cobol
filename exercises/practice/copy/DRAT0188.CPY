      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0188.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0188.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:24   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0188  Funktion Delete Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRAT0188                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<  NTZ  TBAT0020                           *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0188.
          05 PI-DRAT0188-LAST-UPDATE        PIC  X(026).
          05 PI-DRAT0188-NTZ-KEY.
             10 PI-DRAT0188-NTZ-ID          PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRAT0188.
          05 PO-DRAT0188-RC                 PIC  X(003).
             88 PO-DRAT0188-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0188-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRAT0188-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
