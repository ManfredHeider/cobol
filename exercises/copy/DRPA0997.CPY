      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0997.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0997.CPv  $
      *
      *   Rev 2.2   Feb 17 2000 17:41:42   T13EX44
      *APA00081
      *PGES-GES aus Standard-Status-Bereich
      *
      *   Rev 2.1   Mar 19 1999 15:46:50   T13EX21
      *APA00012 - MFE-Umstellung 
      *
      *   Rev 2.0   Feb 27 1999 14:29:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:21:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0997  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GRÜNER                                          *
      *    DATUM:    02.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.98! 01 ! NEUERSTELLUNG             ! GRÜNER   *
      *         !        !    !                           !          *
      * APA00081!01.02.00!2.02! ERWEITERUNG UM 2 STELLEN  ! SCHEIDEL *
      *         !        !    ! WEGEN PGEB-GES            !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0996                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0997.
          05 PI-DRPA0997-UFKT               PIC  9(002).
          05 PI-DRPA0997-DATEN              PIC  X(051).
      *
       01 PO-DRPA0997.
          05 PO-DRPA0997-RC                 PIC  X(003).
             88 PO-DRPA0997-RC-OK           VALUE 'OK '.
      *

      ***** ENDE-COPY-MEMBER *****************************************
