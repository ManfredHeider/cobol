      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1096.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI1096.CPv  $
      *
      *   Rev 2.2   10 Aug 2000 16:05:24   T13EX39
      *AKI00136 BFG-KUNDENÜBERTRAGUNG
      *
      *   Rev 2.1   06 Jul 2000 13:27:12   T13EX39
      *AKI00100 BFG-INVEST
      *
      *   Rev 2.0   Feb 27 1999 12:53:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:50:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1096  FUNKTIONSAUFRUFE FUER              *
      *                           LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO                                             *
      *    DATUM:     5.06.2000                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00100!24.03.00! 01 ! NEUERSTELLUNG             !WHO       *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00136!03.08.00!1.0 ! zus. DECLARE CURSOR       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXKI0357.
           05 PI-ZXKI0357-FUNKTION            PIC X(008).
              88 PI-ZXKI0357-DRKI1097-LIST        VALUE 'DRKI1097'.
              88 PI-ZXKI0357-DRKI1106-LIST        VALUE 'DRKI1106'.
              88 PI-ZXKI0357-DRKI1115-LIST        VALUE 'DRKI1115'.
      *
      ******** Ende Copy-Member DRKI0551  ***************************
