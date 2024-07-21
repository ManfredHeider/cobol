      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0692.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0692.CPv  $
      *
      *   Rev 2.2   May 27 1999 15:07:46   T13EX26
      *MFE-Umstellung 2
      *
      *   Rev 2.1   May 27 1999 13:18:20   T13EX26
      *MFE-Umstellung
      *
      *
      *   Rev 2.0   May 26 1999 16:58:00   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 17:00:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0692  FUNKTION ALLE PMPT  MELDUNGEN      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    09.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA0692.
          05 PI-DRPA0692-PROGRAMM            PIC  X(008).
          05 PI-DRPA0692-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0692-DATEN.
             10 PI-DRPA0692-PMPR-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0692-PMPR-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA0692.
          05 PO-DRPA0692-RC                         PIC  X(003).
             88 PO-DRPA0692-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0692-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0692-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA0692-DATEN.
             10 PO-DRPA0692-PMPT.
                COPY DRPA0413.
      *
      ******* ENDE COPY-MEMBER **************************************
