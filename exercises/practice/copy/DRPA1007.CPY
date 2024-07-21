      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1007.CPv  $
      *
      *   Rev 2.0   May 25 1999 13:53:16   T13AE18
      *APA00012
      *
      *   Rev 1.0   08 Jul 1998 08:51:40   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1007  FUNKTION LIST PMPT- MELDUNGEN      *
      *                           SORT NACH FAELLIG UND PROD-SCHL    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: bohlinger                                       *
      *    DATUM:    08.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.07.98! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA1007.
          05 PI-DRPA1007-PROGRAMM            PIC  X(008).
          05 PI-DRPA1007-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1007-DATEN.
             10 PI-DRPA1007-PMPR-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1007-PMPR-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1007.
          05 PO-DRPA1007-RC                         PIC  X(003).
             88 PO-DRPA1007-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1007-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1007-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA1007-DATEN.
             10 PO-DRPA1007-PMPT.
                COPY DRPA0413.
                35 PMPT-KENNUNG                     PIC X(008).
                35 PMPT-LAST-UPDATE                 PIC X(026).
      *
      ******* ENDE COPY-MEMBER **************************************
