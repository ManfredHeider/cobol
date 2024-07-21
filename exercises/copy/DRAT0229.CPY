      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0229.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0229.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:00:00   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   03 Dec 1997 15:48:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0229  SATZBESCHREIBUNG DES COMMIT-SATZES *
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
      *         !16.06.98!01.02!DEF. DER COMMIT-POINT-NR  ! BOHLINGER*
      *         !        !     !GEäNDERT                  !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DEN SATZAUFBAU DES                    *
      *                                                              *
      *    >>>  COMMIT-SATZ <<<   IN QSAM-DATEIEN                    *
      *                                                              *
      *   (SATZLäNGE: 80 BYTE)                                       *
      ****************************************************************
       01 DRAT0229-COMMIT-SATZ.
          10 DRAT0229-COMMIT-TEXT-1       PIC  X(017)
                                          VALUE 'COMMIT-POINT-NR: '.
160698    10 DRAT0229-COMMIT-POINT-NR     PIC  9(006).
          10 DRAT0229-COMMIT-TEXT-2       PIC  X(012)
                                          VALUE ' TIMESTAMP: '.
          10 DRAT0229-COMMIT-TIMESTAMP    PIC  X(026).
          10 DRAT0229-COMMIT-TEXT-3       PIC  X(010)
                                          VALUE ' KENNUNG: '.
          10 DRAT0229-COMMIT-KENNUNG      PIC  X(010).
      *
      ***** Ende-Copy-Member *****************************************
