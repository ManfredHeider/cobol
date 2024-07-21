      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0721.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0721.CPv  $
      *
      *   Rev 2.1   Aug 23 1999 11:33:04   T13EX21
      *APA00049 - Unterfunktion eingefügt
      *
      *   Rev 2.0   May 26 1999 15:15:40   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0721  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      * APA00012!06.08.98! 02 ! GUELTIG-AB AUF TIMESTAMP  ! JÄGER    *
      *         !        !    ! UMSTELLEN                 !          *
      * APA00012!14.12.98! 03 ! GUELTIG-BIS/-AB ENTFERNT  ! MUND     *
      * APA00049!02.07.99! 02 ! UFKT. EINGEFÜGT           ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0721                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PMDS TBPA0123              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0721.
020799    05 PI-DRPA0721-UFKT                  PIC  X(002).
          05 PI-DRPA0721-PMDS-KEY.
           10 PI-DRPA0721-PMBE-ID              PIC S9(15)V COMP-3.
           10 PI-DRPA0721-PMBE-VERSION         PIC S9(4) COMP.
           10 PI-DRPA0721-PMDS-PMBE-ID         PIC S9(15)V COMP-3.
           10 PI-DRPA0721-PMDS-PMBE-VERSION    PIC S9(4) COMP.
141298*   05 PI-DRPA0721-PMDS-DATEN.
      *    10 PI-DRPA0721-PMDS-GUELTIG-AB      PIC X(10).
141298*    10 PI-DRPA0721-PMDS-GUELTIG-AB      PIC X(26).
141298*    10 PI-DRPA0721-PMDS-GUELTIG-BIS     PIC X(10).
      *
       01 PO-DRPA0721.
          05 PO-DRPA0721-RC                   PIC  X(003).
             88 PO-DRPA0721-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0721-RC-DUPLICATE-VALUE            VALUE 'DVA'.

