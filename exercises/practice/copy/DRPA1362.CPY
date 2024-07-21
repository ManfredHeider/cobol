      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1362.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1362.CPv  $
      *
      *   Rev 2.1   Apr 13 2000 15:51:34   T13EX21
      * 
      *
      *   Rev 2.0   Mar 01 2000 09:58:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1362  FUNKTION PIN FÜR BADENIA PRÜFEN    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    29.03.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00067!29.03.00!2.1  ! NEUERSTELLUNG             ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1362.
          05 PI-DRPA1362-ZVP-VERSION             PIC S9(004) COMP.
          05 PI-DRPA1362-PIN                     PIC S9(009) COMP.
          05 PI-DRPA1362-PGEB-GES                PIC S9(004) COMP.
          05 PI-DRPA1362-PROD-PROD-SCHL          PIC S9(004) COMP.
          05 PI-DRPA1362-VERTR-SPLIT-KZ          PIC  X(001).
      *
       01 PO-DRPA1362.
          05 PO-DRPA1362-RC                     PIC  X(003).
             88 PO-DRPA1362-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1362-RC-VSTAT-GEFUELLT            VALUE 'VSG'.
      *
      ****** ENDE COPY-MEMBER ****************************************
