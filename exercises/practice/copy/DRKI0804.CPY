      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0804.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0804.CPv  $
      *
      *   Rev 2.1   Feb 29 2000 12:08:36   T13EX39
      *AKI00094 REPLIKATIONSZÄHLER
      *
      *   Rev 1.1   08 Feb 1999 17:31:08   T13EX39
      *FUNKTION SETZE KUNDENSTATUS UPKI0050
      *
      *   Rev 1.0   12 Jan 1999 15:11:18   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0804   VARIABLEN  FÜR UPRO UPKI0050      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO                                             *
      *    DATUM:    13.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!01.02.99!1.0 ! NEUERSTELLUNG             ! WHO      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00095!23.02.00!1.0 ! DB-OPERATION ZÄHLEN       ! WHO      *
      *         !        !    ! FÜR REPLIKATIONSZÄHLER    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRKI0804.
           05  PI-DRKI0804-DATEN.
             10 PI-DRKI0804-VERTID        PIC X(13).
      *
       01  PO-DRKI0804.
           05  PO-DRKI0804-UPDATE-Z       PIC  9(001).
           05  PO-DRKI0804-RC             PIC  X(003).
               88 PO-DRKI0804-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0804-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI0804-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
