      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0477.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0477.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:49:26   T13EX25
      *APA00042, Erstellung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:41:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0477  FUNKTION                           *
      *                           "Provisionsbetraege verteilen   "  *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    20.08.1997                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      * APA00012!21.07.98!    ! drpa1022 eingebaut        ! Foltz    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0033.
           05 PI-UWPA0033-FUNKTION            PIC X(008).
              88 PI-UWPA0033-DRPA0478             VALUE 'DRPA0478'.
              88 PI-UWPA0033-DRPA1022             VALUE 'DRPA1022'.
