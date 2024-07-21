      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0769.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0769.CPv  $
      *
      *   Rev 2.1   Feb 17 2000 17:41:40   T13EX44
      *APA00081
      *PGES-GES aus Standard-Status-Bereich
      *
      *   Rev 2.0   May 26 1999 16:08:30   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:46:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0769  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      * apa00012!18.08.98! 02 ! ergänzt um PEBD-Daten (DN)! Parent   *
      *         !        !    ! 242 Byte plus 24 Byte Ver-!          *
      *         !        !    ! längerung der PEST        !          *
      *         !        !    !                           !          *
      * APA00081!01.02.00!2.01! ERWEITERUNG UM 2 STELLEN  ! SCHEIDEL *
      *         !        !    ! WEGEN PGEB-GES            !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0769                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0769.
          05 PI-DRPA0769-UFKT               PIC  9(002).
180898*   05 PI-DRPA0769-DATEN              PIC  X(071).
180898    05 PI-DRPA0769-DATEN              PIC  X(339).
      *
       01 PO-DRPA0769.
          05 PO-DRPA0769-RC                 PIC  X(003).
             88 PO-DRPA0769-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************
