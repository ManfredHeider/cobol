      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0149.CPY  $
      *    $Revision:   1.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0149.CPv  $
      *
      *   Rev 1.4   Jun 02 2015 15:01:16   t13ex71
      *AVB01347: Unterprogramm UPVB0189 zum Ermitteln steuerlicher 
      *Status für Daisy
      *
      *   Rev 1.3   Sep 19 2005 12:14:32   t13ae30
      *AVB00811: RFT - Rangfrei Tod
      *
      *   Rev 1.2   15 Sep 1998 17:19:32   T13AE35
      *ASL00022 - Aenderung, neue Fkt. DRVB0310
      *
      *   Rev 1.1   12 Aug 1998 16:54:58   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   Feb 19 1997 09:58:32   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0143  FUNKTION                           *
      *                           "Lese-OZP zu RFT VIVB0005 "        *
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
      * AKI00001!12.07.98! 02 ! DRVB0248 Erweiterung      !Zgur-Vujic*
      * ASl00022!04.08.98! 02 ! NEUE FUNKTION DRVB0310    ! WALB     *
      * AVB00811!05.08.05!1.3 ! NEUE FUNKTION DRVB0789    ! WILLIG   *
      * AVB01347!26.05.15!1.4 ! NEUE FUNKTION DRVB1102    !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWVB0005.
           05 PI-ZWVB0005-FUNKTION            PIC X(008).
              88 PI-ZWVB0005-DRVB0150             VALUE 'DRVB0150'.
              88 PI-ZWVB0005-DRVB0248             VALUE 'DRVB0248'.
040898        88 PI-ZWVB0005-DRVB0310             VALUE 'DRVB0310'.
050805        88 PI-ZWVB0005-DRVB0789             VALUE 'DRVB0789'.
260515        88 PI-ZWVB0005-DRVB1102             VALUE 'DRVB1102'.

