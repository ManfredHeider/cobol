      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0662.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0662.CPv  $
      *
      *   Rev 2.1   May 27 1999 16:11:48   T13EX26
      *MFE die 2.
      *
      *   Rev 2.0   May 26 1999 16:57:58   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0622  FUNKTION ALLE DAISY MELDUNGEN      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    03.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA0662.
          05 PI-DRPA0662-PROGRAMM            PIC  X(008).
          05 PI-DRPA0662-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0662-DATEN.
             10 PI-DRPA0662-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0662-PMBE-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA0662.
          05 PO-DRPA0662-RC                         PIC  X(003).
             88 PO-DRPA0662-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0662-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0662-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA0662-DATEN.
             10 PO-DRPA0662-PMDS.
                COPY DRPA0570.
      *
      ******* ENDE COPY-MEMBER **************************************
