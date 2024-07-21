      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0228.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0228.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 09:07:34   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   03 Dec 1997 15:48:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0228  Funktion COMMIT Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    03.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!03.02.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0228                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  COMMIT  <<<  COMMIT-POINT IN QSAM-DATEI SETZEN       *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0228.
             10 PI-DRAT0228-COMMIT-POINT-NR    PIC S9(009) BINARY.
      *
       01 PO-DRAT0228.
          05 PO-DRAT0228-RC                   PIC  X(003).
             88 PO-DRAT0228-RC-OK                        VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
