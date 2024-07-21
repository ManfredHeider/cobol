      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1166.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1166.CPv  $
      *
      *   Rev 2.0   May 25 1999 14:25:12   T13AE18
      *apa00012
      *
      *   Rev 1.0   21 Oct 1998 14:02:38   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1166 FUNKTION GET VERTRASDATEN           *
      *                          FüR PROVISIONSDATENMODUL UGPA0001   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    15.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!15.02.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1166                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1166.
             10 PI-DRPA1166-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1166-PVER-VERSION       PIC S9(004)   BINARY.

       01 PO-DRPA1166.
          05 PO-DRPA1166-RC                    PIC  X(003).
             88 PO-DRPA1166-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1166-RC-NOT-FOUND                VALUE 'NFD'.

          05 PO-DRPA1166-DATEN.
             10 PO-DRPA1166-PVER-VNR           PIC  X(015).
      *
      ******** Ende Copy-Member **************************************
