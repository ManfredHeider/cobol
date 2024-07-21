      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0685.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0685.CPv  $
      *
      *   Rev 2.2   Jun 24 1999 17:58:06   T13EX21
      *APA00012 - PVCS-Kopf korrigiert
      *
      *   Rev 1.0   11 Nov 1997 17:00:06   P13SP99
      *Initial Revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0685 FUNKTION SUCHE ALLE PNAP-OBJEKTE    *
      *                                   ZU PNAP-ZUSTAND            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. JÄGER-SOKOLUK                                *
      *    DATUM:    16.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!16.12.97!01.01!NEU                       ! JÄGER    *
      *         !        !     !                          !          *
      * APA00012!11.05.99! 2.00!UMSTELLUNG AUF SATZWEISE  ! PARENT   *
      *         !        !     !ERMITTLUNG UND FAELLIG-VON!          *
      *         !        !     !HINZU ; KTY-PNAP RAUS     !          *
      *         !        !     !                          !          *
      * APA00012!07.06.99!02.01! PI-BEREICH ERWEITERT     ! STORCH   *
      *         !15.06.99!02.02! PI-BEREICH GEÄNDERT      ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0684                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0685.
          05 PI-DRPA0685-DATEN.
110599*      10 PI-DRPA0685-KTY-PNAP-ZUSTAND PIC S9(004) BINARY.
             10 PI-DRPA0685-KNR-PNAP-ZUSTAND PIC S9(004) BINARY.
110599       10 PI-DRPA0685-UFKT-CODE        PIC  9(002).
      *
       01 PO-DRPA0685.
          05 PO-DRPA0685-RC                  PIC  X(003).
             88 PO-DRPA0685-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0685-RC-LEER               VALUE 'LER'.
             88 PO-DRPA0685-RC-EOF                VALUE 'EOF'.
110599    05 PO-DRPA0685-PNAP.
110599*   05 PO-DRPA0685-PNAP-TABELLE.
110599*      10 PO-DRPA0685-MAX-USED-PNAP        PIC S9(004) BINARY.
110599*      10 FILLER                           OCCURS 50
110599*                                          INDEXED BY IX-DRPA0685.
                COPY DRPA0407.
      *
      ******** ENDE COPY-MEMBER **************************************
