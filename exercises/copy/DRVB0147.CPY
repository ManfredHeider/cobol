      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0147.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0147.CPv  $
      *
      *   Rev 2.1   Jun 23 2009 10:30:34   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - VB29
      *
      *   Rev 2.0   Mar 09 1999 15:41:38   T13AE35
      *AAS00096 neue Fkt
      *
      *   Rev 1.0   Feb 19 1997 09:58:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0147  FUNKTION                           *
      *                           "Lese-OZP zu ED VIVB0003 "         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    08.08.1996                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !08.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      * AAS00096!19.02.99! 02 ! NEUE FUNKTION DRVB0358    ! WALB     *
      * AVB01029!24.03.09!2.1 ! NEUE FUNKTION DRVB0933    ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWVB0003.
           05 PI-ZWVB0003-FUNKTION            PIC X(008).
              88 PI-ZWVB0003-DRVB0148             VALUE 'DRVB0148'.
190299        88 PI-ZWVB0003-DRVB0358             VALUE 'DRVB0358'.
240309        88 PI-ZWVB0003-DRVB0933             VALUE 'DRVB0933'.
