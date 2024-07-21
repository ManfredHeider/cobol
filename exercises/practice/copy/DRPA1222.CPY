      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1222.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1222.CPv  $
      *
      *   Rev 2.1   Apr 30 1999 15:36:56   T13EX21
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:06   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1222 FUNKTION GET PEST-ID                *
      *                          FÜR DIALOG PF14 (NOTIZANZEIGE)      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    28.04.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!28.04.99! 01  !NEU                       ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1222                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1222.
             10 PI-DRPA1222-PMPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.

       01 PO-DRPA1222.
          05 PO-DRPA1222-RC                    PIC  X(003).
             88 PO-DRPA1222-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1222-RC-NOT-FOUND                VALUE 'NFD'.

          05 PO-DRPA1222-DATEN.
             10 PO-DRPA1222-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.
