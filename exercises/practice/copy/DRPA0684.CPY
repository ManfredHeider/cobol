      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0684.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0684.CPv  $
      *
      *   Rev 2.2   Jun 15 1999 11:49:56   T13EX21
      *APA00012 - Änderungen PI-Bereich
      *
      *   Rev 2.1   Jun 09 1999 09:16:24   T13EX21
      *APA00012 - Änderungen nach Modifikation
      *der Spezifikation
      *
      *   Rev 2.0   May 21 1999 13:07:32   T13EX26
      *NT Umstellung und PNAP Änderungen
      *
      *   Rev 1.0   11 Nov 1997 17:00:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0684 FUNKTION SUCHE ALLE PNAP-OBJEKTE    *
      *                                   ZU VSTATUS                 *
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
      * APA00012!18.05.99! 2.0 !RÜCKGABE AUF SATZWEISE ;  ! PARENT   *
      *         !        !     !U-FUNKTION AUFGENOMMEN    !          *
      *         !        !     !                          !          *
      * APA00012!07.06.99! 2.1 !PI-DRPA0684-PNAP-OBJEKT-ID! STORCH   *
      *         !        !     !EINGEFÜGT                 !          *
      * APA00012!15.06.99! 2.2 !PI-DRPA0684-PNAP-OBJEKT-ID! STORCH   *
      *         !        !     !ENTFERNT (RESTART ENTFÄLLT!          *
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
       01 PI-DRPA0684.
          05 PI-DRPA0684-DATEN.
             10 PI-DRPA0684-VSTAT            PIC  X(004).
180599       10 PI-DRPA0684-UFKT-CODE        PIC  9(002).
      *
       01 PO-DRPA0684.
          05 PO-DRPA0684-RC                  PIC  X(003).
             88 PO-DRPA0684-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0684-RC-LEER               VALUE 'LER'.
             88 PO-DRPA0684-RC-EOF                VALUE 'EOF'.
180599    05 PO-DRPA0684-PNAP.
180599*   05 PO-DRPA0684-PNAP-TABELLE.
180599*      10 PO-DRPA0684-MAX-USED-PNAP   PIC S9(004) BINARY:
180599*      10 FILLER                           OCCURS 50
180599*                                          INDEXED BY IX-DRPA0684.
                COPY DRPA0407.
      *
      ******** ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
