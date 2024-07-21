      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1217.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1217.CPv  $
      *
      *   Rev 2.2   Feb 17 2000 17:41:46   T13EX44
      *APA00081
      *PGES-GES aus Standard-Status-Bereich
      *
      *   Rev 2.1   Apr 15 1999 17:44:30   T13EX21
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:04   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1217  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.04.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!15.04.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      * APA00081!01.02.00!2.02! ERWEITERUNG UM 2 STELLEN  ! SCHEIDEL *
      *         !        !    ! WEGEN PGEB-GES            !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1217                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1217.
          05 PI-DRPA1217-UFKT               PIC  9(002).
          05 PI-DRPA1217-DATEN              PIC  X(092).
      *
       01 PO-DRPA1217.
          05 PO-DRPA1217-RC                 PIC  X(003).
             88 PO-DRPA1217-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************



