      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1213.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1213.CPv  $
      *
      *   Rev 2.3   Jun 09 2020 11:31:46   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.2   Feb 17 2000 17:41:44   T13EX44
      *APA00081
      *PGES-GES aus Standard-Status-Bereich
      *
      *   Rev 2.1   Apr 15 1999 17:44:22   T13EX21
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
      *    COPY-MEMBER: DRPA1213  INSERT                             *
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
      * APA00081!01.02.00!2.02! ERWEITERUNG UM 2 STELLEN  ! SCHEIDEL *
      *         !        !    ! WEGEN PGEB-GES            !          *
      * APA02908!17.02.20!2.3 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1213                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1213.
          05 PI-DRPA1213-UFKT               PIC  9(002).
170220    05 PI-DRPA1213-DATEN              PIC  X(054).
      *
       01 PO-DRPA1213.
          05 PO-DRPA1213-RC                 PIC  X(003).
             88 PO-DRPA1213-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************


