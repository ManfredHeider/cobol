      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0961.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0961.CPv  $
      *
      *   Rev 2.1   May 27 1999 10:08:44   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:18:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0961  Funktion Update Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    04.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.06.98! 01 ! NEUERSTELLUNG             !D.WILHELM *
      *         !        !    !                           !          *
      * apa00012!25.08.98! 2.0! allgemeine Copystrecke für! Parent   *
      *         !        !    ! PDPT-PDPE eingefügt       !          *
      *         !        !    ! ( im Rahmen Denormalisier)!          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0961                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0961.
          05 PI-DRPA0961-PDPT-LAST-UPDATE        PIC X(26).
          05 PI-DRPA0961-PDPT-KEY.
             10 PI-DRPA0961-PDPT-ID              PIC S9(15)V COMP-3.
          05 PI-DRPA0961-PDPT-PDPE.

250898          COPY DRPA0451.
      *
       01 PO-DRPA0961.
          05 PO-DRPA0961-RC                   PIC  X(003).
             88 PO-DRPA0961-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0961-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0961-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
