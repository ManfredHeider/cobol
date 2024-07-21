      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1247.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1247.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:32:02   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   Jun 02 1999 11:17:56   T13EX22
      *
      *
      *   Rev 2.0   Mar 30 1999 08:44:12   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:32   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1247 Löschen einzelnes PNAP-Objekt        *
      *    -------------------------------------------------------    *
      *    ERSTELLT: T. MUND                                          *
      *    DATUM:    31.05.1999                                       *
      *****************************************************************
      *    VERSION   01                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME      *
      * ------------------------------------------------------------- *
      * APA00012!31.05.99!01.01!NEU                       ! MUND      *
      * APA02908!17.02.20!2.2  ! GES-UEBERGR EVS          ! SCHEIDEL  *
      *         !        !     !                          !           *
      *         !        !     !                          !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRPA1247                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
       01 PI-DRPA1247.
          05 PI-DRPA1247-DATEN.
170220       10 PI-DRPA1247-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1247-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA1247-FAELLIG-VON      PIC  X(010).
      *
       01 PO-DRPA1247.
          05 PO-DRPA1247-RC                  PIC  X(003).
             88 PO-DRPA1247-RC-OK            VALUE 'OK '.
          05 PO-DRPA1247-PVER-PNAP-EINTRAG   PIC  X.
      *
      ******** Ende Copy-Member ***************************************

