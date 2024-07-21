      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0636.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0636.cpv  $
      *
      *   Rev 2.1   Jun 02 1999 11:17:52   T13EX22
      * 
      *
      *   Rev 2.0   May 21 1999 13:07:28   T13EX26
      *NT Umstellung und PNAP Änderungen
      *
      *   Rev 1.0   11 Nov 1997 16:56:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0636  FUNKTION DELETE STANDARD-OZP ZTPA0105
      *    -------------------------------------------------------   *
      *    ERSTELLT: J„GER-SOKOLUK                                   *
      *    DATUM:    20.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!08.12.97!01.01! NEUERSTELLUNG             ! JÄGER   *
      *         !        !     !                           !         *
      * APA00012!19.05.99! 2.0 ! PNAP-OBJEKT HINZU         ! PARENT  *
      * APA00012!31.05.99! 2.1 ! PNAP-OBJEKT ENTFERNT      ! MUND    *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FŠR DIE FUNKTION DRPA0636                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE      TBPA0105 PNAP         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0636.
          05 PI-DRPA0636-PNAP-KEY.
             10 PI-DRPA0636-PNAP-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0636-PNAP-VERSION      PIC S9(004) COMP.
          05 PI-DRPA0636-PNAP-LAST-UPDATE     PIC  X(026).
310599*   05 PI-DRPA0636-PNAP-OBJEKT          PIC S9(004) BINARY.
      *
       01 PO-DRPA0636.
          05 PO-DRPA0636-RC                   PIC  X(003).
             88 PO-DRPA0636-RC-OK                         VALUE 'OK '.
      *
      ****** ENDE COPY-MEMBER ****************************************
