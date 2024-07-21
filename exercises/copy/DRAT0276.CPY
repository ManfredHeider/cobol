      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0276.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0276.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   13 Nov 1998 09:07:38   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   11 Nov 1998 09:00:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   13 Jul 1998 09:28:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0276  Funktion COMMIT Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: H. GRÜNER                                       *
      *    DATUM:    20.07.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.07.98! 01  !NEUERSTELLUNG             ! H. GRÜNER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0276                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  COMMIT  <<<  COMMIT-POINT IN QSAM-DATEI SETZEN       *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0276.
             10 PI-DRAT0276-COMMIT-POINT-NR    PIC S9(009) BINARY.
      *
       01 PO-DRAT0276.
          05 PO-DRAT0276-RC                   PIC  X(003).
             88 PO-DRAT0276-RC-OK                        VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
