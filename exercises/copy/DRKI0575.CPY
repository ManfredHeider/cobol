      * T13EX39   Stand: 19/08/98    12:20:29
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0575.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0575.CPv  $
      *
      *   Rev 2.1   Feb 29 2000 12:08:34   T13EX39
      *AKI00094 REPLIKATIONSZÄHLER
      *
      *   Rev 1.1   08 Feb 1999 17:27:18   T13EX39
      *FUNKTION PRUEFE AUF ALT-KUNDE UPKI0050
      *
      *   Rev 1.0   07 Aug 1998 14:49:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0575   VARIABLEN  FÜR UPRO UPKI0050      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO                                             *
      *    DATUM:    13.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.08.98!1.0 ! NEUERSTELLUNG             ! WHO      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00095!23.02.00!1.0 ! DB-OPERATION ZÄHLEN       ! WHO      *
      *         !        !    ! FÜR REPLIKATIONSZÄHLER    !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRKI0575.
           05  PI-DRKI0575-DATEN.
             10 PI-DRKI0575-VERTID        PIC X(13).
      *
       01  PO-DRKI0575.
           05  PO-DRKI0575-UPDATE-Z       PIC  9(001).
           05  PO-DRKI0575-RC             PIC  X(003).
               88 PO-DRKI0575-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0575-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI0575-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
