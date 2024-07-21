      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1172.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1172.CPv  $
      *
      *   Rev 2.2   Nov 06 2006 09:17:58   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   May 25 1999 14:27:56   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:26   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:03:10   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1172 FUNKTION SET EE-ENTSCHEIDUNG FüR    *
      *                                   EVS-PRODUKT                *
      *                          FüR VORVERTRAGSDATENMODUL UGPA0003  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    22.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.02.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1172                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1172.
             10 PI-DRPA1172-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1172-PGEB-GES           PIC S9(004)
                                               BINARY.
             10 PI-DRPA1172-MAX-USED-PROD-TAB  PIC S9(004)  BINARY.
             10 PI-DRPA1172-PROD-TAB           OCCURS 09.
011106*         20 PI-DRPA1172-PROD-SCHL       PIC S9(004)  BINARY.
011106          20 PI-DRPA1172-PROD-PROD-SCHL  PIC S9(004)  BINARY.
                20 PI-DRPA1172-EE-ENTSCHEID    PIC  X(001).

       01 PO-DRPA1172.
          05 PO-DRPA1172-RC                    PIC  X(003).
             88 PO-DRPA1172-RC-OK                       VALUE 'OK '.

      ******** Ende Copy-Member **************************************
