      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0931.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0931.CPv  $
      *
      *   Rev 2.3   Aug 30 2016 13:45:26   t13ae37
      *APA02301: PIN-Rückmelde-DTA an Central erweitern
      *
      *   Rev 2.2   Jul 02 2002 14:21:50   t13ex22
      *APA00186: DRPA1536 entfernt
      *
      *   Rev 2.1   Apr 15 2002 16:40:58   T13ex22
      *FPA00346/APA00186: Neue Funktionen
      *
      *   Rev 2.0   May 26 1999 15:37:46   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0931                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    17.04.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!17.04.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00012!23.11.98!1.2 ! NEUE FUNKTION DRPA0929    !BOHLINGER *
      * FPA00346!28.02.02!2.1 ! DRPA1525 U. DRPA1526 NEU  !MUND      *
      * APA00186!15.04.02!2.2 ! DRPA1536 NEU              !WANG      *
      * APA00186!02.07.02!2.3 ! DRPA5136 entfernt         !MUND      *
      * APA02301!26.08.16!2.4 ! DRPA1854 ergänzt          !Buch      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE FUNKTIONEN FÜR DAS LIST-OZP       *
      *    ZXPA0106                                                  *
      *                                                              *
      ****************************************************************
      *
       01  PI-ZXPA0106.
           05 PI-ZXPA0106-FUNKTION            PIC X(008).
              88 PI-ZXPA0106-DRPA0932             VALUE 'DRPA0932'.
              88 PI-ZXPA0106-DRPA0929             VALUE 'DRPA0929'.
280202        88 PI-ZXPA0106-DRPA1525             VALUE 'DRPA1525'.
280202        88 PI-ZXPA0106-DRPA1526             VALUE 'DRPA1526'.
020702*       88 PI-ZXPA0106-DRPA1536             VALUE 'DRPA1536'.
260816        88 PI-ZXPA0106-DRPA1854             VALUE 'DRPA1854'.
      ***** Ende-Copy-Member *****************************************

