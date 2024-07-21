      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0949.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0949.CPv  $
      *
      *   Rev 2.5   Nov 25 2004 07:21:38   T13EX39
      *FKI00885: Korrektur DeuBa Buchungen
      *
      *   Rev 2.4   Dec 10 2003 11:46:42   t13ex39
      *FKI00806 KORREKTUR DOPPELTER BUCHUNGEN
      *
      *   Rev 2.3   Jul 10 2003 13:29:16   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.2   23 Mar 2001 12:00:14   T13EX39
      *FKI00466 DIT-BUCHUNG
      *
      *   Rev 2.1   Jan 18 2000 17:19:04   T13EX39
      *AKI00081 DIT-REPLIAKTION
      *
      *   Rev 2.0   Feb 27 1999 12:53:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0949  FUNKTIONSAUFRUFE FUER              *
      *                           LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO                                             *
      *    DATUM:    23.12.1999                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00069!23.12.99! 01 ! NEUERSTELLUNG             !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00081!13.01.00! 02 ! zus. Funktion             !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00466!23.03.01! 02 ! zus. Funktion             !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00341!03.03.03! 03 ! zus. Funktion DRKI1519    !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00806!05.12.03! 03 ! zus. Funktion DRKI1624    !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00885!10.11.04! 04 ! zus. Funktion DRKI1737    !WHO       *
      *         !        !    !                           !          *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXKI0314.
           05 PI-ZXKI0314-FUNKTION            PIC X(008).
              88 PI-ZXKI0314-DRKI0950-LIST        VALUE 'DRKI0950'.
              88 PI-ZXKI0314-DRKI0958-LIST        VALUE 'DRKI0958'.
              88 PI-ZXKI0314-DRKI1216-LIST        VALUE 'DRKI1216'.
              88 PI-ZXKI0314-DRKI1519-LIST        VALUE 'DRKI1519'.
              88 PI-ZXKI0314-DRKI1624-LIST        VALUE 'DRKI1624'.
              88 PI-ZXKI0314-DRKI1737-LIST        VALUE 'DRKI1737'.
      *
      ******** Ende Copy-Member DRKI0551  ***************************
