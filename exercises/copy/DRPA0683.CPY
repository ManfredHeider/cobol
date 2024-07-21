      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0683.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0683.CPv  $
      *
      *   Rev 2.0   May 21 1999 13:07:30   T13EX26
      *NT Umstellung und PNAP Änderungen
      *
      *   Rev 1.0   11 Nov 1997 16:59:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0683 FUNKTION Suche alle PNAP-Objekte    *
      *                                   zu Vertrag                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Jäger-Sokoluk                                *
      *    DATUM:    15.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01!NEU                       ! Jäger    *
      *         !        !     !                          !          *
      * apa00012!18.05.99! 2.0 !Rückgabe auf satzweise ;  ! Parent   *
      *         !        !     !U-Funktion aufgenommen ;  !          *
      *         !        !     !RC-EOF hinzu              !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0683                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0683.
          05 PI-DRPA0683-DATEN.
             10 PI-DRPA0683-PRINTE           PIC S9(015) PACKED-DECIMAL.
180599       10 PI-DRPA0683-UFKT-CODE        PIC  9(001).
      *
       01 PO-DRPA0683.
          05 PO-DRPA0683-RC                  PIC  X(003).
             88 PO-DRPA0683-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0683-RC-LEER               VALUE 'LER'.
             88 PO-DRPA0683-RC-EOF                VALUE 'EOF'.
180599    05 PO-DRPA0683-PNAP.
180599*   05 PO-DRPA0683-PNAP-TABELLE.
180599*      10 PO-DRPA0683-MAX-USED-PNAP   PIC S9(004) BINARY.
180599*      10 FILLER                      OCCURS 50
180599*                                     INDEXED BY IX-DRPA0683.
                COPY DRPA0407.
      *
      ******** Ende Copy-Member **************************************
