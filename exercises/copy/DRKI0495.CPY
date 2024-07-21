      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0495.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0495.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 17:00:48   T13EX39
      *FUNKTION INSERT FUER UPKI0048
      *
      *   Rev 1.0   28 Jul 1998 09:58:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0495   VARIABLEN  FÜR UPRO UPKI0048      *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRKI0495.
           05  PI-DRKI0495-DATEN.
             10 PI-DRKI0495-VERTID               PIC X(13).
             10 PI-DRKI0495-VB-NR                PIC 9(07).
             10 PI-DRKI0495-VRBGUELTIGKEITSDAT   PIC X(10).
      *
       01  PO-DRKI0495.
           05  PO-DRKI0495-RC             PIC  X(003).
               88 PO-DRKI0495-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0495-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRKI0495-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
