      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0252.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0252.CPv  $
      *
      *   Rev 2.1   Mar 30 2000 11:05:36   T13EX39
      *AKI00098 OZPs
      *
      *   Rev 1.2   29 Oct 1998 17:48:14   T13EX19
      *FKI00010 - Korrektur
      *
      *   Rev 1.1   12 Aug 1998 16:48:54   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   07 Jul 1998 11:54:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0252  FUNKTIONEN STANDARD-OZP            *
      *                           KOMMUNIKATION                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    25.06.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!25.06.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !09.10.98! 1.1! Funktion SEL eingefügt    ! gbe      *
      *         !15.10.98! 1.1! Funktion DEL eingefügt    ! gbe      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00098!13.03.00!1.0 ! SELECT (DRKI1044)         ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTKI0050.
           05 PI-ZTKI0050-FUNKTION             PIC X(008).
              88 PI-ZTKI0050-DRKI0253-INS          VALUE 'DRKI0253'.
              88 PI-ZTKI0050-DRKI0711-SEL          VALUE 'DRKI0711'.
              88 PI-ZTKI0050-DRKI0734-DEL          VALUE 'DRKI0734'.
              88 PI-ZTKI0050-DRKI1044              VALUE 'DRKI1044'.
      ***************************************************************
