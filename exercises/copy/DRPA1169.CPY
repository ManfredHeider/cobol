      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1169.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1169.CPv  $
      *
      *   Rev 2.1   May 25 1999 14:26:18   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:22   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:02:54   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1169 FUNKTION SET URSPRUNGS PE-STRUKTUR  *
      *                 FüR PRODUKTTEIL                              *
      *                 FüR PROVISIONSDATENMODUL UGPA0002            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    11.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!11.03.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1169                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1169.
          05 PI-DRPA1169-PVPT-ID               PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1169-EE-ENTSCHEID          PIC X(001).
          05 PI-DRPA1169-S-EE-ENTSCHEID        PIC X(001).
          05 PI-DRPA1169-URSPRUNGS-PEST-ID     PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1169-KNR-BUCHUNGS-GRUND    PIC S9(004) BINARY.

       01 PO-DRPA1169.
          05 PO-DRPA1169-RC                    PIC  X(003).
             88 PO-DRPA1169-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1169-RC-NOT-FOUND                VALUE 'NFD'.

          05 PO-DRPA1169-MELDUNG.
             10 PO-DRPA1169-MELDUNG-TYP       PIC  X(001).
             10 PO-DRPA1169-MELDUNG-NR        PIC S9(004) BINARY.

      ******** Ende Copy-Member **************************************
