      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1539.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1539.CPv  $
      *
      *   Rev 2.1   Jun 27 2002 08:54:38   t13ex43
      *APA00204 : Migration Wiener Allianz, Generali, S-Bausparkasse
      *
      *   Rev 2.0   04 Jul 2001 11:11:32   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1539  FUNKTIONSAUFRUFE FUER UPPA0132      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.06.2002                                       *
      *****************************************************************
      *    VERSION   2.00                                             *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  ! VER ! AENDERUNG                 ! NAME     *
      * --------------------------------------------------------------*
      * APA00204!10.06.02! 2.01! NEU                       ! KLEEMEIER*
      *         !        !     !                           !          *
      *         !        !     !                           !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
          01 PI-UPPA0132.
           05 PI-UPPA0132-FUNKTION     PIC  X(008).
            88 PI-UPPA0132-DRPA0866-LESEN
                                       VALUE 'DRPA0866'.
      **** ENDE COPY DRPA1539
