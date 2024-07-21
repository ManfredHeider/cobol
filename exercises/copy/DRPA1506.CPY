      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1506.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1506.CPv  $
      *
      *   Rev 2.1   09 Nov 2001 15:39:18   t13ex43
      *APA00168: Migration DIT Österreich und Bank Austria Invest
      *
      *   Rev 2.0   04 Jul 2001 11:10:42   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1506  FUNKTIONSAUFRUFE FÜR ZAPA0015      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    06.11.2001                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00168!06.11.01!2.01! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
          01 PI-ZAPA0015.
           05 PI-ZAPA0015-FUNKTION     PIC  X(008).
            88 PI-ZAPA0015-DRPA1507-LESEN
                                       VALUE 'DRPA1507'.
            88 PI-ZBPA0015-DRPA1507-LESEN
                                       VALUE 'DRPA1507'.
            88 PI-ZDPA0015-DRPA1507-LESEN
                                       VALUE 'DRPA1507'.
