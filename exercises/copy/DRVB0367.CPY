      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0367.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0367.cpv  $
      *
      *   Rev 2.1   Sep 02 1999 12:09:50   T13AE30
      * 
      *
      *   Rev 2.0   May 18 1999 10:42:18   T13AE42
      *NT-Version
      *
      *   Rev 1.1   May 17 1999 16:02:32   T13AE42
      *AAS00117 - Delta-Rueckkonvertierung
      *
      *   Rev 1.0   22 Jan 1999 13:41:06   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0367  FUNKTION                           *
      *                           "LESE-OZP ZU RFT VIVB0045 "         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                     *
      *    DATUM:    22.02.1999                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00117!22.02.99! 01 ! NEUERSTELLUNG             ! SCHMIDT,B*
      * AAS00051!23.08.99! 02 ! NEUE FUNKTION DRVB0439    ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWVB0045.
           05 PI-ZWVB0045-FUNKTION            PIC X(008).
              88 PI-ZWVB0045-DRVB0368             VALUE 'DRVB0368'.
              88 PI-ZWVB0045-DRVB0439             VALUE 'DRVB0439'.
