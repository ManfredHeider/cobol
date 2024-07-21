      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1561.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1561.CPv  $
      *
      *   Rev 2.1   Aug 12 2003 09:37:56   t13ex21
      *APA00245 - Neuerstellung
      *
      *   Rev 2.0   Jul 04 2002 15:30:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1561 FUNKTION SET PVPT-KDBETR-REDUZ      *
      *                          FÜR PRODUKTTEIL                     *
      *                          FÜR PROVISIONSDATENMODUL UGPA0001   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    29.07.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00245!29.07.03! 01  ! NEUERSTELLUNG            ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1561                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1561.
             10 PI-DRPA1561-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1561-PVPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1561-PVPT-VERSION       PIC S9(004)
                                               BINARY.
             10 PI-DRPA1561-PVPT-KD-REDUZ      PIC  X(001).

       01 PO-DRPA1561.
          05 PO-DRPA1561-RC                    PIC  X(003).
             88 PO-DRPA1561-RC-OK                       VALUE 'OK '.
      *
      ******** ENDE COPY-MEMBER **************************************
