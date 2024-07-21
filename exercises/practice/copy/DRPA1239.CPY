      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1239.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1239.CPv  $
      *
      *   Rev 2.1   May 21 1999 13:06:34   T13EX26
      *Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:10   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1239  FUNKTION LESEN  OB NOCH EINTRÄGE   *
      *                           IN PNAP ZUR VNR MIT ZUSTAND FEHLER *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    19.05.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00012!19.05.99!01.01! NEUERSTELLUNG             ! PARENT  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FŠR DIE FUNKTION DRPA1239                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1239.
          05 PI-DRPA1239-PNAP-VNR          PIC  X(015).
          05 PI-DRPA1239-PNAP-ZUSTAND      PIC S9(004) BINARY.
      *
       01 PO-DRPA1239.
          05 PO-DRPA1239-RC                PIC  X(003).
             88 PO-DRPA1239-RC-OK               VALUE 'OK '.
             88 PO-DRPA1239-RC-NFD              VALUE 'NFD'.
          05 PO-DRPA1239-PVER-PNAP-EINTRAG PIC  X.
      *
      ****** ENDE COPY-MEMBER ****************************************

