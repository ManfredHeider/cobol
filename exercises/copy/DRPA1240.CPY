      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1240.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1240.CPv  $
      *
      *   Rev 2.2   Jun 15 1999 11:50:36   T13EX21
      *APA00012 - Änderung PI-Bereich
      *
      *   Rev 2.1   May 21 1999 13:06:38   T13EX26
      *Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:10   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:30   P13SP20
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRPA1240  FUNKTION SUCHE ALLE ZUKÜNFTIGEN    *
      *                           PNAP-OBJEKTE ZUM FÄLLIG-VON        *
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
      * APA00012!07.06.99!01.02! ÄNDERUNGEN PI-BEREICH     ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1240                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1240.
070699*   05 PI-DRPA1240-FAELLIG-VON      PIC  X(010).
070699    05 PI-DRPA1240-KNR-PNAP-OBJEKT  PIC S9(004) BINARY.
          05 PI-DRPA1240-UFKT-CODE        PIC S9(002).
      *
       01 PO-DRPA1240.
          05 PO-DRPA1240-RC                PIC  X(003).
             88 PO-DRPA1240-RC-OK               VALUE 'OK '.
             88 PO-DRPA1240-RC-EOF              VALUE 'EOF'.
             88 PO-DRPA1240-RC-LEER             VALUE 'LER'.

          05 PO-DRPA1240-PNAP.
                COPY DRPA0407.
      *
      ****** ENDE COPY-MEMBER ****************************************

