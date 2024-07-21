      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0599.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0599.CPv  $
      *
      *   Rev 2.1   Nov 25 1999 09:16:24   T13EX21
      *APA00063 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:29:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Oct 1997 13:13:52   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0599                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE 'DTA SPERREN' FÜR                            *
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
       01 PI-DRPA0599.
          05 PI-DRPA0599-PDTA-ID                 PIC S9(015)V COMP-3.
      *
       01 PO-DRPA0599.
          05 PO-DRPA0599-RC                      PIC  X(003).
             88 PO-DRPA0599-RC-OK                     VALUE 'OK '.
             88 PO-DRPA0599-RC-NOK                    VALUE 'NOK'.

          05 PO-DRPA0599-MELDUNG-TYP             PIC  X(001).
          05 PO-DRPA0599-MELDUNG-NR              PIC  9(003).
      ******* ENDE COPY-MEMBER **************************************
