      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0809.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0809.CPv  $
      *
      *   Rev 2.1   Jun 02 1999 13:51:56   T13EX44
      * 
      *
      *   Rev 2.0   May 26 1999 16:58:10   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   14 Jan 1998 12:43:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0809  Gesch„fts-Kto. bilden              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Pr”mpeler                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !21.01.98! 01 ! NEUERSTELLUNG             ! Parent   *
      *         !        !    ! (kopiert von drpa0122 alt,!          *
      *         !        !    ! um alten Aufruf von       !          *
      *         !        !    ! UWPA0004 weiterhin zu     !          *
      *                         erlauben                                     *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FšR DIE FUNKTION DRPA0809                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0809.
          05 PI-DRPA0809-KNR-VERTRIEB-GES     PIC S9(004) BINARY.

       01 PO-DRPA0809.
          05 PO-DRPA0809-RC                   PIC  X(003).
             88 PO-DRPA0809-RC-OK                  VALUE 'OK '.
      *
      ******** Ende Copy-Member **************************************
