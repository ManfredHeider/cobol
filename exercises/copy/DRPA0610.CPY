      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0610.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0610.CPv  $
      *
      *   Rev 2.3   Jun 09 2020 11:26:12   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.2   Mar 10 2014 16:45:50   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Jul 26 2013 11:41:06   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.0   Feb 27 1999 14:29:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 10:32:26   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:54:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0610                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0610                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JäGER-SOKOLUK                                    *
      *    DATUM:    11.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !11.11.97!01.01! NEU                      ! JäGER    *
      * APA00012!27.03.98!     ! ERWEITERUNG PO-BEREICH   ! STORCH   *
      * apa00012!15.07.98!     ! Unterfunktion hinzugefügt! Parent   *
      *         !        !     ! Timestamp aus PO-Daten   !          *
      *         !        !     ! entfernt, da jetzt im WF !          *
      * APA01539!26.07.13!2.1  ! ERW. DRPA0408 KALYPSO    ! RAHNER   *
      * APA02908!17.02.20!2.3  ! GES-UEBERGR EVS          ! SCHEIDEL *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0610: LESEN AKTUELLSTE PVER MIT PRINTE
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0610.
150798     05  PI-DRPA0610-UFKT             PIC  9(002).
           05  PI-DRPA0610-DATEN.
               10 PI-DRPA0610-PGEB-GES      PIC S9(004) BINARY.
               10 PI-DRPA0610-PRINTE        PIC S9(015) PACKED-DECIMAL.
               10 PI-DRPA0610-VERSION       PIC S9(004) BINARY.
      *
       01  PO-DRPA0610.
           05  PO-DRPA0610-RC               PIC  X(003).
               88 PO-DRPA0610-RC-OK              VALUE 'OK '.
               88 PO-DRPA0610-RC-NOT-FOUND       VALUE 'NFD'.
150798*    05  PO-DRPA0610-DATEN-1.
150798*        10 PO-DRPA0610-LAST-UPDATE   PIC  X(026).
150798*        10 PO-DRPA0610-KENNUNG       PIC  X(008).
           05  PO-DRPA0610-DATEN.
               COPY DRPA0408.
      *
      ******* ENDE COPY-MEMBER **************************************

