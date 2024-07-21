      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0688.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0688.cpv  $
      *
      *   Rev 2.1   Jun 02 1999 11:17:54   T13EX22
      * 
      *
      *   Rev 2.0   May 21 1999 13:07:42   T13EX26
      *NT Umstellung und PNAP Änderungen
      *
      *   Rev 1.0   11 Nov 1997 17:00:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0688 - Löschen einzelnes PNAP-Objekt     *
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
      * APA00012!10.11.98!01.02!Schalter hinzugefügt      ! Jäger    *
      *         !        !     !                          !          *
      * apa00012!18.05.99! 2.0 !Schalter umbenannt & KNR- ! Parent   *
      *         !        !     !PNAP-OBJEKT hinzu         !          *
      * APA00012!31.05.99! 2.1 !FKPAW054 V1.1 UMGESETZT   ! MUND     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0688                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0688.
          05 PI-DRPA0688-DATEN.
             10 PI-DRPA0688-PNAP-ID          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0688-PNAP-VERSION     PIC S9(004) BINARY.
310599*      10 PI-DRPA0688-KNR-PNAP-OBJEKT  PIC S9(004) BINARY.
      *
       01 PO-DRPA0688.
          05 PO-DRPA0688-RC                PIC  X(003).
             88 PO-DRPA0688-RC-OK               VALUE 'OK '.
180599*   05 PO-DRPA0688-NULL-SCHALTER     PIC  X.
310599*   05 PO-DRPA0688-PVER-PNAP-EINTRAG PIC  X.
      *
      ******** Ende Copy-Member **************************************
