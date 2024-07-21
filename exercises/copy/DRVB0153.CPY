      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0153.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0153.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 15:40:48   T13AE35
      *AAS00096 neue Fkt
      *
      *   Rev 1.1   12 Aug 1998 16:55:10   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   Feb 19 1997 09:58:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0153  FUNKTION                           *
      *                           "Lese-OZP zu JUR VIVB0051"         *
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
      * AKI00001!13.07.98! 02 ! Erwietert für DRVB0263    !          *
      * AAS00096!19.02.99! 03 ! NEUE FUNKTION DRVB0360    ! WALB     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWVB0051.
           05 PI-ZWVB0051-FUNKTION            PIC X(008).
              88 PI-ZWVB0051-DRVB0154             VALUE 'DRVB0154'.
              88 PI-ZWVB0051-DRVB0263             VALUE 'DRVB0263'.
190299        88 PI-ZWVB0051-DRVB0360             VALUE 'DRVB0360'.
