      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0231.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0231.CPv  $
      *
      *   Rev 2.1   Nov 25 1999 09:14:36   T13EX21
      *APA00063 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:28:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 12:12:34   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0231                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE 'CHECK DTA-BEARBEITUNGSFUNKTION' FÜR         *
      *       PROGRAMM UPPA0083 (DTA-SACHBEARBEITER)                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    06.10.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00063!06.10.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *
      ****************************************************************
      *
       01 PI-DRPA0231.
          05 PI-DRPA0231-PDTA-ID                 PIC S9(015)V COMP-3.
          05 PI-DRPA0231-KNR-DTA-BFKT-TAB     OCCURS 10.
             10 PI-DRPA0231-KNR-DTA-BEARBFKT     PIC S9(004) COMP.
      *
       01 PO-DRPA0231.
          05 PO-DRPA0231-RC                      PIC  X(003).
             88 PO-DRPA0231-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0231-RC-NOT-FOUND              VALUE 'NFD'.

          05 PO-DRPA0231-KNR-DTA-BFKT-TAB     OCCURS 10.
             10 PO-DRPA0231-KNR-DTA-BEARBFKT     PIC S9(004) COMP.
             10 PO-DRPA0231-DTA-BEARBFKT-ZUL     PIC  X(001).
      ******* ENDE COPY-MEMBER **************************************
