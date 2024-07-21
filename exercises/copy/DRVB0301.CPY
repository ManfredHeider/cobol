      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0301.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0301.CPv  $
      *
      *   Rev 2.2   10 Jul 2000 15:18:34   T13AE34
      *AVB00487-Hat VB einger. Geschäft im 
      *Jahr JHJJ?
      *
      *   Rev 2.1   16 Jun 2000 11:28:34   T13AE34
      *AVB00483-Dialog VB51 
      *
      *   Rev 2.0   Feb 27 1999 15:58:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   09 Jul 1998 09:56:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB00301 FUNKTIONSAUFRUFE FÜR ZAUU0008      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    27.03.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00483!27.03.00!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AVB00487!05.07.00!2.2 ! BATCHVERSION              ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZAUU0008.
           05  PI-ZAUU0008-FUNKTION            PIC  X(008).
               88 PI-ZAUU0008-DRVB0316-LESEN        VALUE 'DRVB0316'.
               88 PI-ZDUU0008-DRVB0316-LESEN        VALUE 'DRVB0316'.
050700         88 PI-ZBUU0008-DRVB0316-LESEN        VALUE 'DRVB0316'.
