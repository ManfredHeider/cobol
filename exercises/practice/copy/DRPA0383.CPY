      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0383.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0383.CPv  $
      *
      *   Rev 2.2   Sep 21 2018 12:14:48   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.1   Jul 17 2006 12:28:14   t13ex44
      *APA00409: Kundenbetreuungsvereinbarung
      *
      *   Rev 2.0   May 27 1999 12:15:42   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:29:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0383  FUNKTION                           *
      *                           "KD-Betreuer-Struktur anpassen"    *
      *                           "VM PE -STRUKTUR ANPASSEN"         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    23.07.1996                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !23.07.97! 01  ! NEUERSTELLUNG            ! JäGER    *
      * APA00012!27.10.98!01.02! NEUE FUNKTION DRPA1107   !BOHLINGER *
      * APA00409!14.03.06!02.01! NEUE FUNKTION DRPA1656   ! SCHEIDEL *
      * APA02548!26.07.18! 2.2 ! NEUE FUNKTION DRPA1902   ! SCHEIDEL *
      * DSY04437!27.04.22!     ! NEUE FUNKTION DRPA1970   ! KÖPRÜCÜ  *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0021.
           05 PI-UWPA0021-FUNKTION            PIC X(008).
              88 PI-UWPA0021-DRPA0384             VALUE 'DRPA0384'.
              88 PI-UWPA0021-DRPA1107             VALUE 'DRPA1107'.
140306        88 PI-UWPA0021-DRPA1656             VALUE 'DRPA1656'.
260718        88 PI-UWPA0021-DRPA1902             VALUE 'DRPA1902'.
270422        88 PI-UWPA0021-DRPA1970             VALUE 'DRPA1970'.

