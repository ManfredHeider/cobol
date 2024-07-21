      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0611.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0611.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 10:33:04   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   11 Nov 1997 16:54:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0611                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0611                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    11.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * Daisy   !11.11.97!01.01! NEU                      ! Jäger    *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0611: LESEN aktuellste PVER MIT PIN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0611.
           05  PI-DRPA0611-DATEN.
               10 PI-DRPA0611-PGEB-GES      PIC S9(004) BINARY.
               10 PI-DRPA0611-PIN           PIC S9(009) BINARY.
      *
       01  PO-DRPA0611.
           05  PO-DRPA0611-RC               PIC  X(003).
               88 PO-DRPA0611-RC-OK              VALUE 'OK '.
               88 PO-DRPA0611-RC-NOT-FOUND       VALUE 'NFD'.
           05  PO-DRPA0611-DATEN.
               10 PO-DRPA0611-PRINTE        PIC S9(15) PACKED-DECIMAL.
               10 PO-DRPA0611-VERSION       PIC S9(04) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
