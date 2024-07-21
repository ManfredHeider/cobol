      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0531.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0531.CPv  $
      *
      *   Rev 2.1   Feb 17 2000 16:00:40   T13EX44
      *APA00081
      *PGEB-GES in Standard-Status-Bereich
      *
      *   Rev 2.0   Feb 27 1999 14:29:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:27:48   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Sep 1997 17:00:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0531  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: T. MUND                                         *
      *    DATUM:    22.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA....!22.09.97! 01 ! Neuerstellung             ! T. MUND  *
      *         !        !    !                           !          *
      * APA00081!25.01.00!2.01! FUER PGEB-GES UM ZWEI     ! SCHEIDEL *
      *         !        !    ! STELLEN ERWEITERT         !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0531                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0531.
          05 PI-DRPA0531-UFKT               PIC  9(002).
250100*   05 PI-DRPA0531-DATEN              PIC  X(3962).
250100    05 PI-DRPA0531-DATEN              PIC  X(3964).
      *
       01 PO-DRPA0531.
          05 PO-DRPA0531-RC                 PIC  X(003).
             88 PO-DRPA0531-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
