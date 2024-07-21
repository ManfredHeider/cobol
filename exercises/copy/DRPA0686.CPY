      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0686.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA0686.CPv  $
      *
      *   Rev 2.3   Jun 24 1999 17:58:08   T13EX21
      *APA00012 - PVCS-Kopf korrigiert
      *
      *   Rev 1.0   11 Nov 1997 17:00:10   P13SP99
      *Initial Revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0686 FUNKTION PM O.K. SETZEN IN PNAP     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    20.05.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.12.97!01.01!NEU (!!! UNTER GLEICHEM NA! PARENT   *
      *         !        !     !MEN EXISTIERTE EINE FUNK. !          *
      *         !        !     !ZUM LESEN PNAP MIT ID UND !          *
      *         !        !     !VERSION                   !          *
      * APA00012!07.06.99!01.02! PI-BEREICH GEÄNDERT      ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0686                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0686.
          05 PI-DRPA0686-DATEN.
070699*      10 PI-DRPA0686-FAELLIG-VON      PIC  X(010).
070699       10 PI-DRPA0686-KNR-PNAP-OBJEKT  PIC S9(004) BINARY.
             10 PI-DRPA0686-UFKT-CODE    PIC S9(002).
      *
       01 PO-DRPA0686.
          05 PO-DRPA0686-RC               PIC  X(003).
             88 PO-DRPA0686-RC-OK              VALUE 'OK '.
             88 PO-DRPA0686-RC-EOF             VALUE 'EOF'.
             88 PO-DRPA0686-RC-LEER            VALUE 'LER'.

          05 PO-DRPA0686-PNAP.
                COPY DRPA0407.

      *
      ******** ENDE COPY-MEMBER **************************************
